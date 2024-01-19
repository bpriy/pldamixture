#' @import stats
#' @noRd
constrained_logistic_regression <- function(X, y, bound, cmaxiter){
  #browser()
  X <- as.matrix(X)
  d <- ncol(X)
  #log(mean(mu)/(1-mean(mu)))
  #mean(X %*% beta)
  barx <- colMeans(X)

  # parameter for back-tracking line search
  tau <- 0.1

  nloglik_logistic <- function(mu){

    #mu <- plogis(X %*% beta)
    cutoff <- 1E-10 # 1E-8
    flag0 <- (mu < cutoff)
    flag1 <- (mu > 1 - cutoff)
    flag <- flag0 | flag1

    nloglik_nonflag <- sum(-(y[!flag] * log(mu[!flag]) + (1-y[!flag]) * log(1 - mu[!flag])))

    if(any(flag0)){
      if(max(y[flag0]) > cutoff){
        warning("Fitted probabilites zero with non-matching reponse; evaluation of the logistic likelihood for the mismatch indicator may be inaccurate \n")
      }
    }

    if(any(flag1)){
      if(min(y[flag1]) < 1 - cutoff){
        warning("Fitted probabilites one with non-matching reponse; evaluation of the logistic likelihood for the mismatch indicator may be inaccurate \n")
      }
    }
    return(nloglik_nonflag)
  }
  grad_Hess <- function(mu){

    grad <- -crossprod(X, y - mu)
    w <- sqrt(mu * (1-mu))
    Xw <- sweep(X, MARGIN = 1, STATS = w, FUN = "*")
    Hess <- crossprod(Xw)

    return(list(grad = grad, Hess = Hess))
  }

  #betaglm <- coef(glm(y ~ X - 1, family = binomial))
  #betaglm[1] <- betaglm[1] - .45
  betacur <- c(bound, rep(0, d-1))
  mu <- plogis(X %*% betacur)
  iter <- 1
  maxiter <- cmaxiter
  mu <- plogis(X %*% betacur)
  tol <- 1E-8
  objs <- numeric(maxiter)
  objs[1] <- nloglik_logistic(mu)
  #browser()
  while(iter < maxiter){

    gH <- grad_Hess(mu)
    gr <- gH$grad
    H <- gH$Hess

    rhs <- -gr + (H %*% betacur)
    betanew <- solve(H, rhs)

    eta_tmp <- X%*% betanew

    if(mean(eta_tmp) > bound){
      #    betacur <- betanew
      #    mu <- plogis(X %*% betacur)
      #    iter <- iter+1
      #    objs[iter] <- nloglik_logistic(mu)
      #}
      #else{

      betanew_theta <- solve(rbind(cbind(H,barx), c(barx,0)), c(rhs, bound))
      betanew <- betanew_theta[1:d]
      theta <- betanew_theta[d+1]
    }
    # linesearch

    #foo <- function(gamma){
    #  betagamma <- (1-gamma) * betacur + gamma * betanew
    #  mu <- plogis(X %*% betagamma)
    #  nloglik_logistic(mu)
    #}
    #gammastar <- optimize(foo, interval = c(0,1))$minimum
    m <- 0

    # inexact back-tracking line search via Armijo rule
    upd <- betanew - betacur
    munew <- plogis(X %*% betanew)
    objnew <- nloglik_logistic(munew)

    while( (objs[iter] - objnew) < tau * (1/2)^m * sum(upd * (-gr))){
      m <- m + 1
      if((1/2)^m < tol^2){
        betanew <- betacur
        break
      }

      else{
        betanew <- betacur + upd * (1/2)^m
        munew <- plogis(X %*% betanew)
        objnew <- nloglik_logistic(munew)
      }
    }

    betacur <- betanew

    mu <- munew
    iter <- iter+1
    objs[iter] <- objnew

    #repeat{

    #if(is(try(optimize(foo, interval = c(0,2^-m))$minimum, silent = TRUE),"try-error")){
    #  m <- m + 1
    #} else {
    #  gammastar <- optimize(foo, interval = c(0,2^-m))$minimum
    #  break}
    #if(2^(-m) < tol) {
    #  gammastar <- optimize(foo, interval = c(0,2^-m))$minimum
    #  break}
    #}
    #betacur <- (1-gammastar) * betacur + gammastar * drop(betanew)
    #if(is.matrix(betacur)){
    #    browser()
    #}


    if(iter >= 2){
      if(objs[iter-1] - objs[iter] < tol)
        break
    }

  }

  return(list(beta = betacur, objs = objs))

}

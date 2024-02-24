#' @import survival
#' @noRd
fit_mixture_cox <- function(formula, data, family,
                            mformula, safematches, mrate,
                            initbeta, initgamma, fy, maxiter, tol, cmaxiter){
  # 1. INPUTS
  # ------------------------------------------------------------------------
  # formula, data, and mformula (X, Y, cens, logis_ps, n, p)
  if (!missing(data)){
    mf <- model.frame(formula, data = data)
    cens <- 1 - mf[[1]][,"status"]
    X <- mf[,-1]
    y <- mf[[1]][,"time"]
    if(attr(mf[[1]], "type") != "right"){
      stop("Error: censoring type other than right-censoring is not currently supported")
    }
    if(!is.Surv(mf[[1]])){
      stop(("Error: response should be a survival object"))}
    n <- nrow(X)
    p <- ncol(X)

    if(missing(mformula)){
      logis_ps <- matrix(nrow = n, ncol = 1, data = 1)
      colnames(logis_ps) <- "(Intercept)"
    } else {
      if(!is.null(model.response(model.frame(mformula, data = data)))){
        stop("Error: mformula should be a one-sided formula")}
      logis_ps <- model.matrix(mformula, data=data)}

  } else {
    mf <- model.frame(formula)
    cens <- 1 - mf[[1]][,"status"]
    X <- mf[,-1]
    y <- mf[[1]][,"time"]
    if(attr(mf[[1]], "type") != "right"){
      stop("Error: censoring type other than right-censoring is not currently supported")
    }
    if(!is.Surv(mf[[1]])){
      stop(("Error: response should be a survival object"))}
    n <- nrow(X)
    p <- ncol(X)

    if(missing(mformula)){
      logis_ps <- matrix(nrow = n, ncol = 1, data = 1)
      colnames(logis_ps) <- "(Intercept)"
    } else {
      if(!is.null(model.response(model.frame(mformula)))){
        stop("Error: mformula should be a one-sided formula")}
      logis_ps <- model.matrix(mformula)}
  }

  if(any(is.na(X)) | any(is.na(y))){"Error (formula): Cannot have a missing observations"}
  if(any(is.na(logis_ps))){"Error (mformula): Cannot have a missing observations"}
  if(nrow(logis_ps) != n){"Error (mformula): Number of observations in formula and mformula data should match"}

  # safe matches (is_flagged)
  if(missing(safematches)){
    is_flagged <- rep(FALSE,n)
  } else {
    is_flagged <- safematches
  }

  if(any(is.na(is_flagged))){"Error (safematches): Cannot have a missing observations"}
  if(length(is_flagged) != n){"Error (safematches): Length of safematches should match number of observations"}

  # mrate (logitbound)
  if(!missing(mrate)){
    logitbound <- -log((1 - mrate)/mrate)
  }

  # maxiter and tol controls
  maxiter <- maxiter
  tol <- tol

  # fy
  if (fy != "default"){
    fy <- as.vector(fy)
    if(length(fy) != n){
      warning("Default 'fy' used. 'fy' should be a vector with a value
              for each observation")}
    fy <- "default"
  } else {
    survf0 <- survfit(Surv(y, event = 1 - cens) ~ 1)
    times <- survf0$time
    D <- c(survf0$cumhaz[1], diff(survf0$cumhaz)/diff(times))

    g_lambdahat_0 <- D[match(y,times)]
    g_Lambdahat_0 <- survf0$cumhaz[match(y,times)]

    fy <- g_lambdahat_0^(1-cens) * exp(-g_Lambdahat_0)
  }

  # initbeta
  if (initbeta != "default"){
    betacur <- as.vector(initbeta)
    if(length(betacur) != p){
      warning("Default 'initbeta' used. 'initbeta' should be a vector of length ", p)}
    betacur <- "default"
  } else {
    creg <- coxph(Surv(y, event = 1 - cens) ~ X - 1)
    beta_cur <- creg$coef
  }

  # initgamma
  Delta <- logis_ps
  if (initgamma != "default"){
    gammacur <- as.vector(initgamma)
    if(length(gammacur) != p){
      warning("Default 'initgamma' used. 'initgamma' should be a vector of length ", ncol(Delta))}
    gammacur <- "default"
  } else {
    if(!missing(mrate)){
      if(sum(Delta == 1) == n){
        gammacur <- rep(-logitbound, ncol(Delta))
      } else {
        gammacur <- c(min(logitbound, 0), rep(0, ncol(Delta)-1))
      }
    } else {
      gammacur <- rep(0, ncol(Delta))
    }
  }

    # 2. INITIALIZE EM-ALGORITHM
  # -------------------------------------------------------------------------
  fymu <- function(mu, cens, l0, L0) (exp(mu) * l0)^(1 - cens) *
    exp(-exp(mu) * L0)

  nloglik <- function(mu, cens, hs, l0, L0) sum(-log(hs[!is_flagged] *
                                                       fymu(mu, cens, l0, L0)[!is_flagged] + (1 - hs[!is_flagged])*
                                                       fy[!is_flagged])) - sum(log(fymu(mu, cens, l0, L0)[is_flagged]))

  ## hs
  hgamma <- function(eta){
    pi <- plogis(eta)
    return(list(fun = pi, dfun = pi*(1-pi), d2fun = pi*(1-pi)*(1 - 2*pi)))
  }
  hs <- hgamma(Delta %*% gammacur)$fun

  ## mucur
  mu <- X %*% beta_cur

  ## objs
  iter <- 1
  Breslow_Estimator <- basehaz(creg, centered=FALSE)
  cumhazard <- Breslow_Estimator$hazard
  times <- Breslow_Estimator$time
  D1 <-c(cumhazard[1], diff(cumhazard)/diff(times))
  lambdahat_0 <- D1[match(y,times)]
  Lambdahat_0 <- cumhazard[match(y,times)]
  nloglik_cur <- nloglik(mu, cens, hs, lambdahat_0, Lambdahat_0)
  objs <- numeric(maxiter)
  objs[iter] <- nloglik_cur

  ## pcur
  pcur = rep(0,n)

  ## additional trackers
  track_beta <- matrix(0, nrow = maxiter, ncol = p)
  track_lam <- matrix(0, nrow = maxiter, ncol = n)
  track_Lam <- matrix(0, nrow = maxiter, ncol = n)

  track_beta[1,] <- beta_cur
  track_lam[1,] <- lambdahat_0
  track_Lam[1,] <- Lambdahat_0

  lambdahat_0_ <- lambdahat_0
  Lambdahat_0_ <- Lambdahat_0

  # 3. EM-ALGORITHM
  # -------------------------------------------------------------------------
  while(iter < maxiter){
    num <- hs[!is_flagged] * fymu(mu, cens, lambdahat_0_,
                                  Lambdahat_0_)[!is_flagged]
    denom <- num + (1-hs[!is_flagged]) * fy[!is_flagged]
    pcur[!is_flagged] <- num/denom
    pcur[is_flagged] <- 1

    if(anyNA(pcur)){
      warning("EM algorithm did not converge. NA observation weight(s) occurred in the E-step.")
      return(list(coefficients =  beta_cur, m.coefficients = gammacur,
           match.prob = hs, family = family, objective = objs[1:(iter)],
           Lambdahat_0 = Lambdahat_0_,  g_Lambdahat_0= g_Lambdahat_0))
    }

    if(!missing(mrate)){
      if(sum(Delta == 1) == n){
        glm_h <- glm(pcur[!is_flagged] ~ Delta[!is_flagged,] - 1, family = quasibinomial)
        gammacur <- max(coef(glm_h), -logitbound)
      }
      else {
        glm_h <- constrained_logistic_regression(Delta[!is_flagged,],
                                                 1-pcur[!is_flagged], logitbound,
                                                 cmaxiter)
        gammacur <- -glm_h$beta
      }
    } else {
      glm_h <- glm(pcur[!is_flagged] ~ Delta[!is_flagged,] - 1,
                   family = quasibinomial)
      gammacur <- coef(glm_h)
    }

    hs <- hgamma(Delta %*% gammacur)$fun

    creg <- coxph(Surv(y, event = 1 - cens) ~ X - 1, weights =
                    pmax(drop(pcur),1E-6))
    mu <- X %*% creg$coefficients
    beta_cur <- creg$coefficients
    track_beta[iter+1,] <- beta_cur

    Breslow_Estimator <- basehaz(creg, centered=FALSE)
    cumhazard <- Breslow_Estimator$hazard
    times <- Breslow_Estimator$time
    D1 <- c(cumhazard[1], diff(cumhazard)/diff(times))

    lambdahat_0_ <- D1[match(y,times)]
    Lambdahat_0_ <- cumhazard[match(y,times)]

    track_lam[iter+1,] <- lambdahat_0_
    track_Lam[iter+1,] <- Lambdahat_0_

    iter <- iter + 1
    objs[iter] <- nloglik(mu, cens, hs, lambdahat_0_, Lambdahat_0_)

    if(is.na(objs[iter])){
      warning("EM algorithm did not converge. NA objective value occurred.")
      return(list(coefficients =  beta_cur, m.coefficients = gammacur,
                  match.prob = hs, family = family, objective = objs[1:(iter)],
                  Lambdahat_0 = Lambdahat_0_,  g_Lambdahat_0= g_Lambdahat_0))
    }

    if(objs[iter] + tol > objs[iter-1]){
      break
    }

  }
  names(gammacur) <- colnames(logis_ps)

  # 4. STANDARD ERRORS
  # -------------------------------------------------------------------------
  Xorig <- X # design matrix
  yorig <- y # response
  censoring <- cens # censoring indicator (1 if yes, 0 if no)
  muorig <- mu # Xorig %*% betacur
  Z <- Delta # m-model covariates

  ## PART 1: Run k Monte-Carlo Samples (i.e., of the {m_i}_{i=1}^n's)
  k <- 1000

  for (sample in 1:k){
    # 1. Sample {m_i^[k]}_{i=1}^n given data, \hat{\beta}, \hat{\gamma}
    m_i <- rbinom(n, size = 1, prob = (1-pcur)) # 1-pcur is conditional probability of mismatch

    # 2. Subset Data w/ m_i = 0
    X <- Xorig[m_i == 0,,drop = FALSE]
    y <- yorig[m_i == 0,drop = FALSE]
    cens <- censoring[m_i == 0,drop = FALSE]
    mu <- muorig[m_i == 0,,drop = FALSE]

    zgam <- Z %*% gammacur

    pb <- length(beta_cur)
    pg  <- length(gammacur)
    pt <- pb + pg

    delta <- 1-cens # event indicator (1 if yes, 0 if no)
    n_event <- sum(delta)
    risk_set <- apply(as.matrix(y[delta == 1]), 1, function(yi) which(y >= yi)) # find R(y_i)

    # 3. Evaluate Gradient w.r.t \hat{\beta} & \hat{\gamma}
    #   derivative of -ve partial log-likelihood
    s_bg <- numeric(pt)
    val2 <- numeric(n_event)
    for (d1 in 1:pt){
      if (d1 <= pb){
        for(i in 1:n_event){
          val2[i] <- sum(exp(mu[risk_set[[i]]])*X[risk_set[[i]],d1])/sum(exp(mu[risk_set[[i]]]))
        }
        s_bg[d1] <- -sum(X[(delta ==1),d1]) + sum(val2)
      } else {
        indz <- d1 - pb
        s_bg[d1] <- sum((exp(zgam)*Z[,indz])/(1+exp(zgam)) - (1-m_i)*Z[,indz])
      }
    }
    if (sample == 1){
      gradient <- s_bg
    } else{
      gradient <- rbind(gradient, s_bg)
    }

    # 4. Evaluate Hessian w.r.t \hat{\beta} & \hat{\gamma}
    #    second derivative of -ve partial log-likelihood
    h_bg <- matrix(NA, nrow = pt, ncol = pt)
    low <- numeric(n_event)
    high <- numeric(n_event)
    dhigh <- numeric(n_event)
    dlow <- numeric(n_event)
    val <- numeric(n_event)
    for (d1 in 1:pt){
      for (d2 in 1:pt){
        if (d1 <= pb & d2 <= pb){
          for(i in 1:n_event){
            low[i] <- sum(exp(mu[risk_set[[i]]]))
            dlow[i] <- sum(exp(mu[risk_set[[i]]])*X[risk_set[[i]],d2])
            high[i] <- sum(exp(mu[risk_set[[i]]])*X[risk_set[[i]],d1])
            dhigh[i] <- sum(exp(mu[risk_set[[i]]])*X[risk_set[[i]],d1]*X[risk_set[[i]],d2])
            val[i] <- (low[i]*dhigh[i] - high[i]*dlow[i])/(low[i])^2
          }
          h_bg[d1,d2] <- sum(val)
        }

        if (d1 <= pb & d2 > pb | d1 > pb & d2 <= pb){
          h_bg[d1,d2] <- 0
        }

        if (d1 > pb & d2 > pb){
          indz1 <- d1 - pb
          indz2 <- d2 - pb
          low <- (1+exp(zgam))
          dlow <- (exp(zgam)*Z[,indz2])
          high <- (exp(zgam)*Z[,indz1])
          dhigh <- (exp(zgam)*Z[,indz1]*Z[,indz2])
          h_bg[d1,d2] <- sum((low *dhigh  - high *dlow)/(low)^2)
        }
      }
    }

    if (sample == 1){
      hessian <- h_bg
    } else{
      hessian <- array(c(hessian, h_bg), dim = c(pt, pt, sample))
    }
  }
  row.names(gradient) <- NULL
  hmcg <- apply(hessian,c(1,2),mean) - cov(gradient)
  se <- sqrt(diag(solve(t(hmcg))))

  # 5. OUTPUTS
  # -------------------------------------------------------------------------
  list(coefficients =  beta_cur, m.coefficients = gammacur,
       match.prob = hs, family = family, objective = objs[1:(iter)],
       Lambdahat_0 = Lambdahat_0_,  g_Lambdahat_0= g_Lambdahat_0,
       wfit = creg, standard.errors = se)
}

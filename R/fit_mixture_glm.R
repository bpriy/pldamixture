#' @noRd
fit_mixture_glm <- function(formula, data, family,
                            mformula, safematches, mrate,
                            initbeta, initgamma, fy, maxiter, tol, cmaxiter){
  # 1. INPUTS
  # ------------------------------------------------------------------------
  # formula, data, and mformula (X, Y, logis_ps, n, p)
  if (!missing(data)){
    X <- model.matrix(formula, data=data)
    y <- model.response(model.frame(formula, data = data))
    if(family == "binomial"){
      y <- as.numeric(as.vector(y))
      if((sum(y > 1) != 0)) stop("Error: y values must be 0 or 1")
    } else {
      y <- as.numeric(y)
    }
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
    X <- model.matrix(formula)
    y <- as.numeric(model.response(model.frame(formula)))
    if(family == "binomial" & (sum(y > 1) != 0)){
      stop("Error: currently only binary models are supported (# number of trials should be 1)")
    }
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
    if (family != "binomial"){
      kde_obj <- density(y)
      g <- approxfun(x = kde_obj$x, y = kde_obj$y, method = "linear")
    } else {
      g <- function(y) (mean(y)^y) * (1-mean(y))^(1-y)
    }
    fy <- g(y)
  }

  # initbeta
  if (initbeta != "default"){
    betacur <- as.vector(initbeta)
    if(length(betacur) != p){
      warning("Default 'initbeta' used. 'initbeta' should be a vector of length ", p)}
    betacur <- "default"
  } else {
    if (family != "gamma"){
      betacur <- coef(glm(y~X-1, family = family))
    } else {
      pcur = rep(0,n)
      shape_update <- function(betacur, mme, pcur){
        k <- -3
        delta <- 2^k
        lower = (1-delta)*mme
        upper = (1+delta)*mme

        f <- function(shape) sum((1-pcur)*(shape*(y*exp(-X %*% betacur) + (X %*% betacur)) -
                                             (shape-1)*log(y) - shape*(log(shape)) + log(gamma(shape))))
        optimize(f = f, lower = lower, upper = upper)$minimum
      }
      glm0 <- glm(y ~ X - 1, family = Gamma(link = "log"))
      betacur <- coef(glm0)
      shape <- shape_update(betacur, mme = (summary(glm0)$dispersion)^-1, pcur)
    }
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
m <- 1
  fymu <- function(mu, sub, ...){
  if(family == "poisson"){return(dpois(y[sub], mu[sub]))}
  if(family == "binomial"){return(dbinom(y[sub], m, mu[sub]))}
  if(family == "gamma"){return(dgamma(y[sub], shape, shape/mu[sub]))}
}

  fymu_all_GLM <- function(mu, sub, family, shape){
    if(family == "poisson"){
      shape <- NA
      fun <- dpois(y[sub], mu[sub])
      d_fun <- fun * (y - mu)[sub]
      d2_fun <- fun * ((y - mu)[sub]^2 - mu[sub])
      return(list(fun = fun, dfun = d_fun, d2fun = d2_fun))
    }

    if(family == "binomial"){
      shape <- NA
      fun <- dbinom(y[sub], m, mu[sub])
      d_fun <- fun * (y/mu + 1/(1 - mu))[sub]
      d2_fun <- d_fun * (y/mu + 1/(1 - mu))[sub] + (y^2/(mu^2) - 1/((1 - mu)^2))*fun
      return(list(fun = fun, dfun = d_fun, d2fun = d2_fun))
    }

    if(family == "gamma"){
      fun <- dgamma(y[sub], shape, shape/mu[sub])
      d_fun <- fun * (y/mu^2 - 1/mu)[sub] * shape
      d2_fun <- d_fun * (y/mu^2 - 1/mu)[sub] * shape + (1/mu^2 - y/mu^3) * shape * fun
      return(list(fun = fun, dfun = d_fun, d2fun = d2_fun))
    }
  }

  nloglik <- function(mu, hs, ...){
    if(family != "gamma"){sum(-log(hs[!is_flagged] * fymu(mu, !is_flagged) +
                                     (1 - hs[!is_flagged])* fy[!is_flagged])) -
        sum(log(fymu(mu, is_flagged)))} else {
          sum(-log(hs[!is_flagged] * fymu(mu, !is_flagged, shape) +
                     (1 - hs[!is_flagged])* fy[!is_flagged])) -
            sum(log(fymu(mu, is_flagged, shape)))}
  }

  ## hs
  hgamma <- function(eta){
    pi <- plogis(eta)
    return(list(fun = pi, dfun = pi*(1-pi), d2fun = pi*(1-pi)*(1 - 2*pi)))
  }
  hs <- hgamma(Delta %*% gammacur)$fun

  ## mucur
  if(family == "poisson"){
    mucur <- exp(X %*% betacur)
  }
  if(family == "binomial"){
    mucur <- plogis(X %*% betacur)
  }
  if(family == "gamma"){
    mucur <- exp(X %*% betacur)
  }

  ## objs
  iter <- 1
  if (family != "gamma"){
    nloglik_cur <- nloglik(mucur, hs)
  } else {
    nloglik_cur <- nloglik(mucur, hs, shape)
  }
  objs <- numeric(maxiter)
  objs[iter] <- nloglik_cur

  ## pcur
  pcur = rep(0,n)

  # 3. EM-ALGORITHM
  # -------------------------------------------------------------------------
  while(iter < maxiter){
    if (family != "gamma"){
      num <-  hs[!is_flagged] * fymu(mucur,!is_flagged)
    } else {
      num <-  hs[!is_flagged] * fymu(mucur,!is_flagged, shape)
    }
    denom <- num + (1-hs[!is_flagged]) * fy[!is_flagged]
    pcur[!is_flagged] <- num/denom
    pcur[is_flagged] <- 1

    if(anyNA(pcur)){
      warning("EM algorithm did not converge. NA observation weight(s) occurred in the E-step.")
      return(list(coefficients = betacur, match.prob = hs,
                  objective = objs[1:(iter)], family = family,
                  m.coefficients = gammacur))
    }

    if(!missing(mrate)){
      if(sum(Delta == 1) == n){
        glm_h <- glm(pcur[!is_flagged] ~ Delta[!is_flagged,] - 1,
                     family = quasibinomial)
        gammacur <- max(coef(glm_h), -logitbound)
      }
      else {
        glm_h <- constrained_logistic_regression(Delta[!is_flagged,],
                                                 1-pcur[!is_flagged], logitbound, cmaxiter)
        gammacur <- -glm_h$beta
      }
    } else {
      glm_h <- glm(pcur[!is_flagged] ~ Delta[!is_flagged,] - 1,
                   family = quasibinomial)
      gammacur <- coef(glm_h)
    }

    hs <- hgamma(Delta %*% gammacur)$fun

    if (family != "gamma"){
      wglmfit <- glm(y ~ X - 1, family = family, weights = pcur)
      betacur <- coef(wglmfit)
    } else {
      wglmfit <- glm(y ~ X - 1, family = Gamma(link = "log"), weights = pcur)
      betacur <- coef(wglmfit)
      shape <- shape_update(betacur, mme = (summary(wglmfit)$dispersion)^-1, pcur)
    }

    if(family == "poisson"){
      mucur <- exp(X %*% betacur)
    }

    if(family == "binomial"){
      mucur <- plogis(X %*% betacur)
    }

    if(sum(family == "gamma") == 1){
      mucur <- exp(X %*% betacur)
    }

    iter <- iter + 1
    if (family != "gamma"){
      objs[iter] <- nloglik(mucur, hs)
    } else {
      objs[iter] <- nloglik(mucur, hs, shape)
    }

    if(is.na(objs[iter])){
      warning("EM algorithm did not converge. NA objective value occurred.")
      return(list(coefficients = betacur, match.prob = hs,
                  objective = objs[1:(iter)], family = family,
                  m.coefficients = gammacur))
    }

    if(objs[iter] + tol > objs[iter-1]){
      break
    }
  }

  # 4. STANDARD ERRORS
  # -------------------------------------------------------------------------
  fymu_all_eval <- fymu_all_GLM(mucur, !is_flagged, family, shape)
  hgamma_eval <- hgamma(Delta[!is_flagged,] %*% as.matrix(gammacur))
  # beta, score, numerator
  mixprob <- fy[!is_flagged] * (1 - hgamma_eval$fun) + hgamma_eval$fun * fymu_all_eval$fun

  w_beta_score_num <- (-1) * fymu_all_eval$dfun*hgamma_eval$fun
  w_beta_score_denom <- mixprob
  w_beta_score <- w_beta_score_num/w_beta_score_denom

  w_gamma_score_num <- (-1) * (fymu_all_eval$fun - fy[!is_flagged])*hgamma_eval$dfun
  w_gamma_score_denom <- mixprob
  w_gamma_score <- w_gamma_score_num/w_gamma_score_denom

  w1 <- w_beta_score^2
  w3 <- w_gamma_score^2

  Xw1 <- sweep(X[!is_flagged,], MARGIN = 1, STATS = w_beta_score, FUN = "*")
  Deltaw3 <- sweep(as.matrix(Delta[!is_flagged,]), MARGIN = 1, STATS = w_gamma_score, FUN = "*")

  meat <- crossprod(cbind(Xw1, Deltaw3))

  w_beta2_hess <- (-(hgamma_eval$fun * fymu_all_eval$d2fun)/mixprob) + (w_beta_score)^2
  w_gamma2_hess <- ((-(fymu_all_eval$fun - fy[!is_flagged])*hgamma_eval$d2fun)/mixprob) + (w_gamma_score)^2
  w_beta_gamma_hess <- (-(fymu_all_eval$dfun * hgamma_eval$dfun)/mixprob) + ((fymu_all_eval$fun - fy[!is_flagged])*(hgamma_eval$fun)*hgamma_eval$dfun*fymu_all_eval$dfun)/(mixprob^2)

  Xw4 <- sweep(X[!is_flagged,], MARGIN = 1, STATS = w_beta2_hess, FUN = "*")
  Deltaw6 <- sweep(as.matrix(Delta[!is_flagged,]), MARGIN = 1, STATS = w_gamma2_hess, FUN = "*")
  Xw5 <-  sweep(X[!is_flagged,], MARGIN = 1, STATS = w_beta_gamma_hess, FUN = "*")
  d <- ncol(X)
  Hess <- matrix(nrow = d + ncol(Delta), ncol =  d + ncol(Delta))
  one_vector <- matrix(nrow = sum(!is_flagged), ncol = 1, data = 1)
  Hess[1:d, 1:d] <- crossprod(X[!is_flagged,], Xw4)
  Hess[(d+1):(d+ncol(Delta)), (d+1):(d+ncol(Delta))] <- crossprod(Delta[!is_flagged,], Deltaw6)
  Hess[1:d, (d+1):(d+ncol(Delta))] <- crossprod(Xw5, Delta[!is_flagged,])
  Hess[(d+1):(d+ncol(Delta)), 1:d] <- t(Hess[1:d, (d+1):(d+ncol(Delta))])

  cov_1_hat  <- solve(Hess, meat)
  covhat <- t(solve(Hess, t(cov_1_hat)))
  ses <- sqrt(diag(covhat))

  nam = NULL
  for (i in 1:(d+ncol(Delta))){
    if(i <= d){nam[i] = paste0("beta", i-1)}
    if(i>=d+1){nam[i] = paste0("gamma", i-(d+1))}
  }
  names(ses) <- nam
  names(gammacur) <- colnames(logis_ps)

  # 5. OUTPUTS
  # -------------------------------------------------------------------------
  output <- list(coefficients = betacur, match.prob = hs,
                 objective = objs[1:(iter)], family = family, standard.errors = ses,
                 m.coefficients = gammacur, wfit = wglmfit)


  if (family == "gamma"){
    output <- append(output, 1/shape)
    names(output)[[length(output)]] <- "dispersion"
  }

  if (family == "binomial" | family == "poisson"){
    output <- append(output, 1)
    names(output)[[length(output)]] <- "dispersion"
  }

  output
}

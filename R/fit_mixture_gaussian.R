fit_mixture_gaussian <- function(formula, data, family,
                                 mformula, safematches, mrate,
                                 initbeta, initgamma, fy, maxiter, tol, cmaxiter){
# 1. INPUTS
# ------------------------------------------------------------------------
# formula, data, and mformula (X, Y, logis_ps, n, p)
  if (!missing(data)){
  X <- model.matrix(formula, data=data)
  y <- as.numeric(model.response(model.frame(formula, data = data)))
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
  g <- function(y){dnorm(y, mean = mean(y), sd = sd(y))} # Gaussian density
  fy <- g(y)
}

# initbeta
if (initbeta != "default"){
  betacur <- as.vector(initbeta)
  if(length(betacur) != p){
    warning("Default 'initbeta' used. 'initbeta' should be a vector of length ", p)}
  betacur <- "default"
} else {
  betacur <- solve(crossprod(X), crossprod(X, y))
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
fymu <- function(mu, sigma, sub) dnorm((y - mu)[sub], sd = sigma)

fymu_all_normal <- function(mu, std, sub){
  fun <- dnorm((y - mu)[sub], sd = std)
  d_fun_beta <- fun * (y - mu)[sub]/(std^2)
  d_fun_sigma <- fun*((y - mu)[sub]^2/std^3 - 1/std)
  d2_fun_beta <- d_fun_beta*(y - mu)[sub]/(std^2) - fun/(std^2)
  d2_fun_sigma <- d_fun_sigma*((y - mu)[sub]^2/std^3 - 1/std) +
    fun*(1/std^2 - 3*(y - mu)[sub]^2/std^4)
  d2_fun_beta_sigma <- d_fun_sigma*(y - mu)[sub]/(std^2) -
    2*fun*(y - mu)[sub]/(std^3)
  return(list(fun = fun, dfun_beta = d_fun_beta, dfun_sigma = d_fun_sigma,
              d2fun_beta = d2_fun_beta, d2fun_sigma = d2_fun_sigma,
              d2fun_beta_sigma = d2_fun_beta_sigma))
}

nloglik <- function(mu, sigma, hs){
  sum(-log(hs[!is_flagged] * fymu(mu, sigma, !is_flagged) +
             (1 - hs[!is_flagged])* fy[!is_flagged])) -
    sum(log(fymu(mu, sigma, is_flagged)))
}

## stdcur
stdcur <- sqrt(sum((y - X %*% betacur)^2)/(n-p))

## hs
hgamma <- function(eta){
  pi <- plogis(eta)
  return(list(fun = pi, dfun = pi*(1-pi), d2fun = pi*(1-pi)*(1 - 2*pi)))
}
hs <- hgamma(Delta %*% gammacur)$fun

## mucur
mucur = X%*%betacur

## objs
iter <- 1
nloglik_cur <- nloglik(mucur, stdcur, hs)
objs <- numeric(maxiter)
objs[iter] <- nloglik_cur

## pcur
pcur = rep(0,n)

# 3. EM-ALGORITHM
# -------------------------------------------------------------------------
while(iter < maxiter){
  num <-  hs[!is_flagged] * fymu(mucur, stdcur,!is_flagged)
  denom <- num + (1-hs[!is_flagged]) * fy[!is_flagged]
  pcur[!is_flagged] <- num/denom
  pcur[is_flagged] <- 1

  if(anyNA(pcur)){
    warning("EM algorithm did not converge. NA observation weight(s) occurred in the E-step.")
    return(list(coefficients = betacur, dispersion = stdcur^2, match.prob = hs,
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

  wglmfit <- glm(y ~ X - 1, family = gaussian, weights = pcur)
  betacur <- coef(wglmfit)
  mucur <- X %*% betacur

  stdcur <- sqrt(weighted.mean((y - mucur)^2, w = pcur))

  iter <- iter + 1
  objs[iter] <- nloglik(mucur, stdcur, hs)

  if(is.na(objs[iter])){
    warning("EM algorithm did not converge. NA objective value occurred.")
    return(list(coefficients = betacur, dispersion = stdcur^2, match.prob = hs,
                objective = objs[1:(iter)], family = family,
                m.coefficients = gammacur))
  }

  if(objs[iter] + tol > objs[iter-1]){
    break
  }
}

# 4. STANDARD ERRORS
# -------------------------------------------------------------------------
fymu_all_eval <- fymu_all_normal(mucur, stdcur, !is_flagged)

hgamma_eval <- hgamma(Delta[!is_flagged,] %*% as.matrix(gammacur))

mixprob <- fy[!is_flagged] * (1 - hgamma_eval$fun) +
  hgamma_eval$fun * fymu_all_eval$fun

w_beta_score_num <- (-1) * fymu_all_eval$dfun_beta*hgamma_eval$fun
w_beta_score_denom <- mixprob
w_beta_score <- w_beta_score_num/w_beta_score_denom

w_sigma_score_num <- (-1) * fymu_all_eval$dfun_sigma*hgamma_eval$fun
w_sigma_score_denom <- mixprob
w_sigma_score <- w_sigma_score_num/w_sigma_score_denom

w_gamma_score_num <- (-1) * (fymu_all_eval$fun -
                               fy[!is_flagged])*hgamma_eval$dfun
w_gamma_score_denom <- mixprob
w_gamma_score <- w_gamma_score_num/w_gamma_score_denom

w1 <- w_beta_score^2
w2 <- w_sigma_score^2
w3 <- w_gamma_score^2

Xw1 <- sweep(X[!is_flagged,], MARGIN = 1, STATS = w_beta_score, FUN = "*")
Xw2 <- sweep(matrix(nrow = sum(!is_flagged), ncol = 1, data = 1),
             MARGIN = 1, STATS = w_sigma_score, FUN = "*")
Deltaw3 <- sweep(as.matrix(Delta[!is_flagged,]), MARGIN = 1,
                 STATS = w_gamma_score, FUN = "*")

meat <- crossprod(cbind(Xw1, Xw2, Deltaw3))

w_beta2_hess <- (-(hgamma_eval$fun * fymu_all_eval$d2fun_beta)/mixprob) + (w_beta_score)^2
w_sigma2_hess <- (-(hgamma_eval$fun * fymu_all_eval$d2fun_sigma)/mixprob) + (w_sigma_score)^2
w_gamma2_hess <- ((-(fymu_all_eval$fun - fy[!is_flagged])*hgamma_eval$d2fun)/mixprob) + (w_gamma_score)^2
w_beta_gamma_hess <- (-(fymu_all_eval$dfun_beta * hgamma_eval$dfun)/mixprob) + ((fymu_all_eval$fun - fy[!is_flagged])*(hgamma_eval$fun)*hgamma_eval$dfun*fymu_all_eval$dfun_beta)/(mixprob^2)
w_sigma_gamma_hess <- (-(fymu_all_eval$dfun_sigma * hgamma_eval$dfun)/mixprob) + ((fymu_all_eval$fun - fy[!is_flagged])*(hgamma_eval$fun)*hgamma_eval$dfun*fymu_all_eval$dfun_sigma)/(mixprob^2)
w_beta_sigma_hess <- (-(fymu_all_eval$d2fun_beta_sigma * hgamma_eval$dfun)/mixprob) + w_beta_score * w_sigma_score

Xw4 <- sweep(X[!is_flagged,], MARGIN = 1, STATS = w_beta2_hess, FUN = "*")
Deltaw6 <- sweep(as.matrix(Delta[!is_flagged,]), MARGIN = 1, STATS = w_gamma2_hess, FUN = "*")
Xw5 <-  sweep(X[!is_flagged,], MARGIN = 1, STATS = w_beta_gamma_hess, FUN = "*")
d <- ncol(X)
Hess <- matrix(nrow = d + 1 + ncol(Delta), ncol =  d + 1 + ncol(Delta))
one_vector <- matrix(nrow = sum(!is_flagged), ncol = 1, data = 1)
Hess[1:d, 1:d] <- crossprod(X[!is_flagged,], Xw4)
Hess[d+1, d+1] <- crossprod(one_vector, sweep(one_vector, MARGIN = 1, STATS = w_sigma2_hess, FUN = "*"))
Hess[(d+2):(d+ncol(Delta)+1), (d+2):(d+ncol(Delta)+1)] <- crossprod(Delta[!is_flagged,], Deltaw6)
Hess[1:(d+1), (d+2):(d+ncol(Delta)+1)] <- rbind(crossprod(Xw5, Delta[!is_flagged,]), crossprod(sweep(one_vector, MARGIN = 1, STATS = w_sigma_gamma_hess, FUN = "*"), Delta[!is_flagged,]))
Hess[(d+2):(d+ncol(Delta)+1), 1:(d+1)] <- t(Hess[1:(d+1), (d+2):(d+ncol(Delta)+1)])
Hess[1:d, d+1] <- crossprod(X[!is_flagged,], sweep(one_vector, MARGIN = 1, STATS = w_beta_sigma_hess, FUN = "*"))
Hess[d+1, 1:d] <- t(Hess[1:d, d+1])

cov_1_hat  <- solve(Hess, meat)
covhat <- t(solve(Hess, t(cov_1_hat)))

ses <- sqrt(diag(covhat))
nam = NULL
for (i in 1:(d+ncol(Delta)+1)){
  if(i <= d){nam[i] = paste0("beta", i-1)}
  if(i == d+1){nam[i] = paste0("sigma")}
  if(i>=d+2){nam[i] = paste0("gamma", i-(d+2))}
}
names(ses) <- nam
names(gammacur) <- colnames(logis_ps)

ses[d+1] <- ses[d+1]*(2*stdcur) # delta method for SE of sigma^2

# 5. OUTPUTS
# -------------------------------------------------------------------------
list(coefficients = betacur, dispersion = stdcur^2, match.prob = hs,
     objective = objs[1:(iter)], family = family, standard.errors = ses,
     m.coefficients = gammacur, wfit = wglmfit)
}

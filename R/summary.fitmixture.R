#' Summarize a "fitmixture" Object
#' @description Summarize results from a `fit_mixture()` object
#'
#' @param object the result of a call to `fit_mixture()`
#' @param ... for additional summary arguments
#'
#' @returns a list of results from the function called depending on the "family" specified.
#' \item{call}{the matched call}
#' \item{family}{the assumed type of (outcome) regression model}
#' \item{coefficients}{a matrix with the outcome model's coefficient estimates, standard errors, t or z values, and p-values}
#' \item{m.coefficients}{a matrix with the correct match model's coefficient estimates and standard errors}
#' \item{avgcmr}{the average correct match rate among all records}
#' \item{match.prob}{the correct match probabilities for all observations}
#' \item{dispersion}{the dispersion parameter estimate when the family is a Generalized Linear Model}
#'
#' @examples
#' ## commonness score of first and last names used for linkage
#' mformula <- ~commf + comml
#' ## hand-linked records are considered "safe" matches
#' safematches <- ifelse(lifem$hndlnk =="Hand-Linked At Some Level", TRUE, FALSE)
#' ## overall mismatch rate in the data set is assumed to be ~ 0.05
#' mrate <- 0.05
#' fit <- fit_mixture(age_at_death ~ poly(unit_yob, 3, raw = TRUE), data = lifem,
#'                    family = "gaussian", mformula, safematches, mrate)
#'
#' summary(fit)
#'
#' @export
summary.fitmixture <- function(object,...){
  l <- length(object$coefficients)
  l2 <- l + 1
  if (object$family == "gaussian" | object$family == "gamma"){
    tval <- object$coefficients/object$standard.errors[1:l]
    df.residual <- df.residual(object$wfit)
    pval <- 2*pt(abs(tval),df=df.residual, lower.tail = FALSE)

    e <- length(object$standard.errors)
    TAB <- cbind(object$coefficients, object$standard.errors[1:l],
                 tval, pval)
    colnames(TAB) <- c("Estimate","Std. Error", "t value", "Pr(>|t|)")
    rownames(TAB) <- substring(rownames(TAB), first=2)
    if (object$family == "gamma"){
      TAB2 <- cbind(object$m.coefficients, object$standard.errors[l2:e])
    } else {
      TAB2 <- cbind(object$m.coefficients, object$standard.errors[(l2+1):e])
    }
    colnames(TAB2) <- c("Estimate","Std. Error")
  }

  if (object$family == "poisson" | object$family == "binomial"){
    zval <- object$coefficients/object$standard.errors[1:l]
    pval <- 2 * (1 - pnorm(abs(zval)))

    e <- length(object$standard.errors)
    TAB <- cbind(object$coefficients, object$standard.errors[1:l],
                 zval, pval)
    colnames(TAB) <- c("Estimate","Std. Error", "z value", "Pr(>|z|)")
    rownames(TAB) <- substring(rownames(TAB), first=2)
    TAB2 <- cbind(object$m.coefficients, object$standard.errors[l2:e])
    colnames(TAB2) <- c("Estimate","Std. Error")
  }

  if (object$family == "cox"){
    zval <- object$coefficients/object$standard.errors[1:l]
    pval <- 2 * (1 - pnorm(abs(zval)))

    e <- length(object$standard.errors)
    TAB <- cbind(object$coefficients, exp(object$coefficients),
                 object$standard.errors[1:l],
                 zval, pval)
    colnames(TAB) <- c("coef", "exp(coef)", "se(coef)", "z value", "Pr(>|z|)")
    rownames(TAB) <- substring(rownames(TAB), first=2)
    TAB2 <- cbind(object$m.coefficients, object$standard.errors[l2:e])
    colnames(TAB2) <- c("Estimate","Std. Error")
  }

  if (object$family == "gaussian"){
    TAB1 <- cbind(object$dispersion, object$standard.errors[l+1])
    colnames(TAB1) <- c("Estimate","Std. Error")
    rownames(TAB1) <- ""
  }

  object <- list(call = object$call, family = object$family,
                         coefficients = TAB, m.coefficients = TAB2,
                         avgcmr = mean(object$match.prob), match.prob = object$hs)

  if (object$family == "gamma"){
    object <- append(object, object$dispersion)
    names(object)[[length(object)]] <- "dispersion"
  }

  if (object$family == "gaussian"){
    object <- append(object, list(TAB1))
    names(object)[[length(object)]] <- "dispersion"
  }

  class(object)    <- "summary.fitmixture"
  object
}

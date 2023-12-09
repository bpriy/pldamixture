#' Summarize `fit_mixture()` object
#' @description Summarize results from the adjustment approach
#'
#' @param output The result of a call to `fit_mixture()`
#'
#' @returns A list of results from the function called depending on the "family" specified.
#' \item{call}{the matched call}
#' \item{family}{assumed type of (outcome) regression model}
#' \item{coefficients}{A matrix with the outcome model's coefficient estimates. For all families
#' except "cox", the matrix also includes standard errors, t or z values, and p-values}
#' \item{m.coefficients}{A matrix with the correct match model's coefficient estimates. For all families
#' except "cox", the matrix also includes standard errors}
#' \item{avgcmr}{Average correct match rate among all records}
#' \item{match.prob}{correct match probabilities for all observations}
#' \item{dispersion}{the dispersion parameter estimate when the family is a Generalized
#' Linear Model}
#'
#' @examples
#' ## commonness score of first and last names used for linkage
#' mformula <- ~commf + comml
#' ## hand-linked records are considered "safe" matches
#' safematches <- ifelse(lifem$hndlnk =="Hand-Linked At Some Level", TRUE, FALSE)
#' ## overall mismatch rate in the data set is assumed to be ~ 0.05
#' mrate <- 0.05
#' fit <- fit_mixture(age_at_death ~ poly(unit_yob, 3, raw = TRUE), data = lifem,
#'                    family = "gaussian", mformula, safematches, mrate, cmaxiter = 3)
#'
#' summary(fit)
#'
#' @export
# s3 summary - https://stackoverflow.com/questions/18684229/how-to-get-summary-to-work-with-custom-class-in-r?noredirect=1&lq=1
summary.fitmixture <- function(output){
  l <- length(output$coefficients)
  l2 <- l + 1
  if (output$family == "gaussian" | output$family == "gamma"){
    tval <- output$coefficients/output$standard.errors[1:l]
    df.residual <- df.residual(output$wfit)
    pval <- 2*pt(abs(tval),df=df.residual, lower = FALSE)

    e <- length(output$standard.errors)
    TAB <- cbind(output$coefficients, output$standard.errors[1:l],
                 tval, pval)
    colnames(TAB) <- c("Estimate","Std. Error", "t value", "Pr(>|t|)")
    rownames(TAB) <- substring(rownames(TAB), first=2)
    if (output$family == "gamma"){
      TAB2 <- cbind(output$m.coefficients, output$standard.errors[l2:e])
    } else {
      TAB2 <- cbind(output$m.coefficients, output$standard.errors[(l2+1):e])
    }
    colnames(TAB2) <- c("Estimate","Std. Error")
  }

  if (output$family == "poisson" | output$family == "binomial"){
    zval <- output$coefficients/output$standard.errors[1:l]
    pval <- 2 * (1 - pnorm(abs(zval)))

    e <- length(output$standard.errors)
    TAB <- cbind(output$coefficients, output$standard.errors[1:l],
                 zval, pval)
    colnames(TAB) <- c("Estimate","Std. Error", "z value", "Pr(>|z|)")
    rownames(TAB) <- substring(rownames(TAB), first=2)
    TAB2 <- cbind(output$m.coefficients, output$standard.errors[l2:e])
    colnames(TAB2) <- c("Estimate","Std. Error")
  }

  if (output$family == "cox"){
    TAB <- cbind(output$coefficients, exp(output$coefficients))
    colnames(TAB) <- c("coef","exp(coef)")
    rownames(TAB) <- substring(rownames(TAB), first=2)
    TAB2 <- cbind(output$m.coefficients)
    colnames(TAB2) <- ""
  }

  if (output$family == "gaussian"){
    TAB1 <- cbind(output$dispersion, output$standard.errors[l+1])
    colnames(TAB1) <- c("Estimate","Std. Error")
    rownames(TAB1) <- ""
  }

  summary.object <- list(call = output$call, family = output$family,
                         coefficients = TAB, m.coefficients = TAB2,
                         avgcmr = mean(output$match.prob), match.prob = output$hs)

  if (output$family == "gamma"){
    summary.object <- append(summary.object, output$dispersion)
    names(summary.object)[[length(summary.object)]] <- "dispersion"
  }

  if (output$family == "gaussian"){
    summary.object <- append(summary.object, list(TAB1))
    names(summary.object)[[length(summary.object)]] <- "dispersion"
  }

  class(summary.object)    <- "summary.fitmixture"
  summary.object
}

#' Print a "fitmixture" Object
#' @description Print call and outcome model coefficients from a `fit_mixture()` object
#'
#' @param x the result of a call to `fit_mixture()`
#' @param digits the number of significant digits to print
#' @param ... for additional print arguments
#'
#' @returns invisibly returns the `fit_mixture()` object that is provided as an argument
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
#' print(fit)
#'
#' @export
print.fitmixture <- function(x, digits = max(3L, getOption("digits") - 3L),...){
  cat("Call:\n")
  print(x$call, quote = F, digits = digits)
  cat("\n")

  if (x$family == "cox"){
    printCoefmat(cbind(coef = x$coefficients, "exp(coef)" = exp(x$coefficients)),
                 quote=F, digits = digits, has.Pvalue = FALSE)
    cat("\n")
  } else {
    cat("Coefficients:", sep="\n")
    print(format(signif(x$coefficients, digits)), print.gap = 2, quote = F)
    cat("\n")
  }

  invisible(x)
}

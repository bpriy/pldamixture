#' Print function for `fit_mixture()` output
#' @description Print call and outcome model coefficients from `fit_mixture()` output
#'
#' @param output The result of a call to `fit_mixture()`
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
#' print(fit)
#'
#' @export
# https://stackoverflow.com/questions/11653127/what-does-the-function-invisible-do
print.fitmixture <- function(output, digits = max(3L, getOption("digits") - 3L)){
  cat("Call:\n")
  print(output$call, quote = F, digits = digits)
  cat("\n")

  if (output$family == "cox"){
    printCoefmat(cbind(coef = output$coefficients, "exp(coef)" = exp(output$coefficients)),
                 quote=F, digits = digits, has.Pvalue = FALSE)
    cat("\n")
  } else {
    cat("Coefficients:", sep="\n")
    print(format(signif(output$coefficients, digits)), print.gap = 2, quote = F)
    cat("\n")
  }

  invisible(output)
}

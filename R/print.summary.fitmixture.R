#' @noRd
#' @export
print.summary.fitmixture <- function(summary.object, digits = max(3L, getOption("digits") - 3L),
                                     signif.stars = getOption("show.signif.stars")){
  cat("Call:", sep="\n")
  print(summary.object$call,quote=F)
  cat(" ", sep="\n")
  cat("Family:", summary.object$family, " ", sep="\n")

  cat("Outcome Model Coefficients:", sep="\n")
  printCoefmat(summary.object$coefficients,quote=F, digits = digits,
               signif.stars = signif.stars)
  cat(" ", sep="\n")

  if (summary.object$family == "gamma"){
    cat("Dispersion: ", format(signif(as.numeric(summary.object$dispersion), digits)))
    cat("\n")
    cat("\n")
  }

  if(summary.object$family == "poisson" | summary.object$family == "binomial"){
    cat("Dispersion: ", 1)
    cat("\n")
    cat("\n")
  }

  if (summary.object$family == "gaussian"){
    cat("Dispersion:", sep="\n")
    printCoefmat(summary.object$dispersion,
                 quote=F, digits = digits, has.Pvalue = FALSE)
    cat(" ", sep="\n")
  }

  cat("Correct Match Model Coefficients:", sep="\n")
  printCoefmat(summary.object$m.coefficients,quote=F, digits = digits,
               signif.stars = signif.stars)
  cat(" ", sep="\n")

  cat("Average Correct Match Rate: ", format(signif(summary.object$avgcmr, digits)))
  cat(" ", sep="\n")

  invisible(summary.object)
}

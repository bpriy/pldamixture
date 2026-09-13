#' Predictions From a "fitmixture" Object
#' @description Obtain predictions from a `fit_mixture()` object. If `newdata` is omitted, predictions are obtained using the underlying model's predict method (`predict.coxph()`, `predict.glm()`, or `predict.lm()`). If `newdata` is provided, predictions are computed directly from the reconstructed model matrix and estimated coefficients.
#'
#' @param object the result of a call to `fit_mixture()`
#' @param newdata optional new data to obtain predictions for. The original data is used by default.
#' @param type the type of prediction. When `newdata` is provided: for the "cox" family, the choices are the linear predictor ("lp") and the risk score exp(lp) ("risk"). For the "gaussian" family and all other glm families ("poisson", "binomial", "gamma"), the choices are predictions on the scale of the linear predictors ("link") or the response ("response"). When `newdata` is omitted, additional types such as "terms" or "expected" may be supported by the underlying predict methods.
#' @param terms the terms when type = "terms". By default, all terms are included. Note: This is only supported when `newdata` is omitted.
#' @param na.action a function for what to do with missing values in `newdata`. The default is to predict "NA".
#' @param ... for future predict arguments
#'
#' @returns a vector or matrix of predictions based on arguments specified.
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
#' predict(fit)
#'
#' @export
predict.fitmixture <- function(object, newdata, type, terms = NULL, na.action = na.pass,...){

  if (missing(newdata)) {
    if (missing(type)) {
      return(predict(object$wfit, na.action = na.action))
    } else {
      return(predict(object$wfit, type = type, terms = terms, na.action = na.action))
    }
  }

  form <- object$call$formula
  if(is.null(form)) stop("Formula missing from model call.")

  mf <- model.frame(form, newdata, na.action = na.action)
  X_new <- model.matrix(form, mf)
  beta <- object$coefficients
  lp <- as.vector(X_new %*% beta)

  if (missing(type)) type <- "link"

  if (object$family == "cox") {
    if (type == "lp") return(lp)
    if (type == "risk") return(exp(lp))
    stop("Type is unsupported for Cox models with newdata in pldamixture.")
  } else {
    if (type == "link") return(lp)
    if (type == "response") {
      if (object$family == "binomial") return(plogis(lp))
      if (object$family %in% c("poisson", "gamma")) return(exp(lp))
      return(lp)
    }
  }
}


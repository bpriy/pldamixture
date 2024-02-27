#' Predictions from a fitmixture object
#' @description Obtain predictions from a `fit_mixture()` object using `predict.coxph()`, `predict.glm()`, or `predict.lm()`.
#'
#' @param object the result of a call to `fit_mixture()`
#' @param newdata optional new data to obtain predictions for. The original data is used by default.
#' @param type the type of prediction. For the "cox" family, the choices are the linear predictor ("lp"), the risk score exp(lp) ("risk"),
#' the expected number of events given the covariates and follow-up time ("expected"),
#' and the terms of the linear predictor ("terms"). The survival probability for a subject is
#' equal to exp(-expected). For the "gaussian" family, the choices are response ("response") or model term ("terms").
#' For the other glm families ("poisson", "binomial", "gamma"), the choices are predictions on the scale of the linear predictors ("link"),
#' response ("response"), or model term ("terms").
#' @param terms the terms when type = "terms". By default, all terms are included.
#' @param na.action a function for what to do with missing values in `newdata`. The default is to predict "NA".
#' @param reference when family = "cox", reference for centering predictions. Available options are c("strata" - default,
#' "sample", "zero"). The default is "strata".
#' @param ... for future predict arguments
#'
#' @returns Predictions based on arguments specified.
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
predict.fitmixture <- function(object, newdata, type, terms = NULL, na.action = na.pass, reference = "strata",...){

  if (object$family == "cox"){
    object <- object$wfit
    terms <- ifelse(is.null(terms), names(object$assign), terms)
    if (missing(newdata)) {
      if (missing(type)) {
        predict(object = object, na.action = na.action,
                reference = reference)
      } else {
        predict(object = object, type = type, terms = terms,
                na.action = na.action, reference = reference)
      }
    } else {
      if (missing(type)) {
        predict(object = object, newdata = newdata,
                na.action = na.action, reference = reference)
      } else {
        predict(object = object, newdata = newdata, type = type,
                terms = terms, na.action = na.action,
                reference = reference)
      }
    }
  } else {
    object <- object$wfit
    if (missing(newdata)) {
      if (missing(type)) {
        predict(object = object, na.action = na.action)
      } else {
        predict(object = object, type = type, terms = terms,
                na.action = na.action)
      }
    } else {
      if (missing(type)) {
        predict(object = object, newdata = newdata,
                na.action = na.action)
      } else {
        predict(object = object, newdata = newdata, type = type,
                terms = terms, na.action = na.action)
      }
    }
  }
}


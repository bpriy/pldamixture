library(survival)

#' Adjustment Method
#' @description
#' Perform regression adjusted for mismatched data. The function currently supports
#' CoxPH Regression (right-censored data only) and Generalized Linear Regression
#' Models (Gaussian, Gamma, Poisson, and Logistic (binary models only)). Information
#' about the underlying record linkage process can be incorporated into the method
#' if available (e.g., assumed overall mismatch rate,
#' safe matches, or predictors of match status).
#'
#' @param formula a formula object for outcome model, with the covariates on
#' the right of "~" and the response on the left. In the Coxph setting, the
#' response should be provided using the `Surv` function.
#' @param data a data.frame with linked data used in "formula" and "formula.m" (optional)
#' @param family type of regression model ("gaussian" - default, "poisson",
#' "binomial", "gamma", "cox")
#' @param mformula one-sided formula object for the mismatch indicator model, with the
#' covariates on the right of "~". The default is an intercept-only model corresponding
#' to a constant mismatch rate)
#' @param safematches indicator variable for safe matches (TRUE : record can be treated as a
#' correct match and FALSE : record may be mismatched). The default is FALSE for all matches.
#' @param mrate assumed overall mismatch rate (a proportion between 0 and 1). If
#' not provided, no overall mismatch rate is assumed.
#' @param control optional list variable to customize the initial parameter estimates
#' ("initbeta" for the outcome model and "initgamma" for the mismatch indicator model),
#' estimated marginal density of the response ("fy"), maximum iterations for the
#' EM algorithm ("maxiter"), maximum iterations for the subroutine in the constrained
#' logistic regression function ("cmaxiter"), and convergence tolerance for
#' the termination of the EM algorithm ("tol").
#'
#' @returns A list of results from the function called depending on the "family" specified.
#' \item{coefficients}{outcome model coefficient estimates}
#' \item{match.prob}{correct match probabilities for all observations}
#' \item{objective}{variable that tracks the negative log pseudo-likelihood for all iterations of the EM algorithm.}
#' \item{family}{type of (outcome) regression model}
#' \item{standard.errors}{estimated standard errors (when family is not "cox")}
#' \item{m.coefficients}{correct match model coefficient estimates}
#' \item{call}{the matched call}
#' \item{wfit}{object for internal use to obtain predictions from predict function}
#' \item{dispersion}{the dispersion parameter estimate when the family is a Generalized
#' Linear Model}
#' \item{Lambdahat_0}{baseline cumulative hazard (using weighted Breslow estimator)}
#' \item{g_Lambdahat_0}{baseline cumulative hazard for the marginal density of the response variable
#' (using Nelson-Aalen estimator)}
#'
#' @examples
#' ## commonness score of first and last names used for linkage
#' mformula <- ~commf + comml
#' ## hand-linked records are considered "safe" matches
#' safematches <- ifelse(lifem$hndlnk =="Hand-Linked At Some Level", TRUE, FALSE)
#' ## overall mismatch rate in the data set is assumed to be ~ 0.05
#' mrate <- 0.05
#'
#' fit <- fit_mixture(age_at_death ~ poly(unit_yob, 3, raw = TRUE), data = lifem,
#'                    family = "gaussian", mformula, safematches, mrate, cmaxiter = 3)

#' @references Slawski, M., West, B. T., Bukke, P., Diao, G., Wang, Z., & Ben-David, E. (2023).
#' A General Framework for Regression with Mismatched Data Based on Mixture Modeling.
#' \url{https://arxiv.org/pdf/2306.00909.pdf}\cr
#' Bukke, P., Ben-David, E., Diao, G., Slawski, M., & West, B. T. (2023).
#' Cox Proportional Hazards Regression Using Linked Data: An Approach Based on Mixture Modelling.
#' \cr
#' Slawski, M., Diao, G., Ben-David, E. (2021). A pseudo-likelihood approach to linear
#' regression with partially shuffled data. Journal of Computational and Graphical
#' Statistics. 30(4), 991-1003
#' \cr
#'
#' @export
fit_mixture <- function(formula, data, family = "gaussian",
                        mformula, safematches, mrate,
                        control = list(initbeta = "default",
                                       initgamma = "default",
                                       fy = "default",
                                       maxiter = 1000,
                                       tol = 1E-4, cmaxiter = 1000),...){
# ------------------------------------------------------------------------------
### CONTROL ARGUMENTS ###
# ------------------------------------------------------------------------------
  dcontrols <- list(...)
  if ("initbeta" %in% names(dcontrols)){
    initbeta <- dcontrols$initbeta
  } else {
    initbeta <- ifelse("initbeta" %in% names(control), control$initbeta, "default")
  }

  if ("initgamma" %in% names(dcontrols)){
    initgamma <- dcontrols$initgamma
  } else {
    initgamma <- ifelse("initgamma" %in% names(control), control$initgamma, "default")
  }

  if ("fy" %in% names(dcontrols)){
    fy <- dcontrols$fy
  } else {
    fy <- ifelse("fy" %in% names(control), control$fy, "default")
  }

  if ("maxiter" %in% names(dcontrols)){
    maxiter <- dcontrols$maxiter
  } else {
    maxiter <- ifelse("maxiter" %in% names(control), control$maxiter, 1000)
  }

  if(maxiter < 2 | (floor(maxiter) != maxiter)){
    warning("Default maxiter used. maxiter should be a non-integer greater than 2")
    maxiter <- 1000}

  if ("cmaxiter" %in% names(dcontrols)){
    cmaxiter <- dcontrols$cmaxiter
  } else {
    cmaxiter <- ifelse("cmaxiter" %in% names(control), control$cmaxiter, 1000)
  }

  if(cmaxiter < 0 | (floor(cmaxiter) != cmaxiter)){
    warning("Default cmaxiter used. cmaxiter should be a non-integer")
    cmaxiter <- 1000}

  if ("tol" %in% names(dcontrols)){
    tol <- dcontrols$tol
  } else {
    tol <- ifelse("tol" %in% names(control), control$tol, 1E-4)
  }
# ------------------------------------------------------------------------------
### MAIN ARGUMENTS ###
# ------------------------------------------------------------------------------
  if(!missing(mrate) && (mrate <= 0 | mrate >= 1)){
    stop("Error: assumed mismatch rate should be a proportion between 0 and 1")}

if(missing(formula)){ stop("Error: a formula for the outcome model is required")}
if(!inherits(formula, "formula")){ stop("Error: formula should be a formula object")}

if(!missing(data) && !is.data.frame(data)){ stop("Error: data should be a data.frame")}

if(!(family %in% c("gaussian", "poisson", "binomial", "gamma", "cox"))){
  stop("Error: the family should be gaussian, poisson, binomial, gamma, or cox")}

if(!missing(mformula) && !inherits(mformula, "formula")) stop("Error: mformula should be a formula object")

if(!missing(safematches) && !is.logical(safematches)){
    stop("Error: safematches should be a logical object")}

# ------------------------------------------------------------------------------
if (family == "gaussian"){
    outputs <- fit_mixture_gaussian(formula, data, family,
                                    mformula, safematches, mrate,
                                    initbeta, initgamma, fy, maxiter, tol, cmaxiter)
  }

if (family == "cox"){
  outputs <- fit_mixture_cox(formula, data, family,
                             mformula, safematches, mrate,
                             initbeta, initgamma, fy, maxiter, tol, cmaxiter)
  }

if (family != "gaussian" & family != "cox"){
    outputs <- fit_mixture_glm(formula, data, family,
                               mformula, safematches, mrate,
                               initbeta, initgamma, fy, maxiter, tol, cmaxiter)
  }
# ------------------------------------------------------------------------------
  outputs <- append(outputs, match.call())
  names(outputs)[[length(outputs)]] <- "call"

  class(outputs) <- "fitmixture"
  outputs
}

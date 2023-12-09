#' Post-Linkage Data Analysis Based on Mixture Modelling
#'
#' \code{pldamixture} implements the "General Framework for Regression with
#' Mismatched Data" developed by Slawski et al., 2023. The framework uses a
#' mixture model for pairs of linked records whose two components reflect
#' distributions conditional on match status, i.e., correct match or mismatch.
#' Inference is based on composite likelihood and the EM algorithm.\cr\cr
#' The package contains 4 functions for usage:\cr \code{fit_mixture}\cr
#' \code{print.fitmixture}\cr \code{summary.fitmixture}\cr \code{predict.fitmixture}\cr
#'
#' @name fastLink-package
#' @docType package
#'
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
#' @examples
#' # optional inputs for linear regression adjusted for potential mismatch error
#' ## use commonness of names as predictors of match status
#' ## first and last names were used for linkage
#' mformula <- ~commf + comml
#' ## hand-linked records are considered "safe" matches
#' safematches <- ifelse(lifem$hndlnk =="Hand-Linked At Some Level", TRUE, FALSE)
#' ## overall mismatch rate in the data set is assumed to be ~ 0.05
#' mrate <- 0.05
#'
#' fit <- fit_mixture(age_at_death ~ poly(unit_yob, 3, raw = TRUE), data = lifem,
#'                    family = "gaussian", mformula, safematches, mrate, cmaxiter = 3)
#' print(fit)
#' summary(fit)
#' predict(fit)
NULL

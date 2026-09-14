# Updates on GitHub

### General Notes
* Upcoming Package Transition: Please note that `pldamixture` will soon be superseded by the upcoming `postlink` package. 
* Acknowledgements: Special thanks to @tbrown122387 for reporting three issues (first three items below) that were addressed in this update!

### Bug Fixes & Improvements
* `summary.fitmixture`: Fixed a bug so the function correctly returns `match.prob` instead of `hs`, and improved standard error indexing to use regex (`grepl("gamma", ...)`) rather than hardcoded indices.
* Error Handling: Wrapped missing-data conditional strings with `stop()` so they throw errors, and fixed minor grammar issues in the error messages.
* `fit_mixture_cox`: Fixed a condition length warning by correcting the initialization check (`initbeta[1] != "default"`) and resolved a variable naming inconsistency (`betacur` to `beta_cur`).
* `predict.fitmixture`: Added support for the `newdata` argument. Predictions are now computed from the reconstructed model matrix and estimated coefficients when `newdata` is provided.
* Standard Error Calculations (`fit_mixture_gaussian`, `fit_mixture_glm`): Corrected derivations to evaluate across all observations (rather than just mismatched ones). Safe matches are now assigned probabilities of 1 with derivatives of 0.
* Derivative Calculations (`fit_mixture_glm`): Fixed analytical first and second derivatives for binomial and gamma families within the internal `fymu_all_GLM` helper function.
* Observation Weights: `match.prob` returns the calculated observation weights (`pcur`) instead of the linear predictor probabilities (`hs`) across all fitting functions.
* Metadata: Updated corresponding author and maintainer email addresses across the `DESCRIPTION`, `README.md`, and compiled `.Rd` files.
* References: Updated the references to papers. 
* `m.prob`: documentation to "posterior correct match probabilities for observations given parameter estimates"
* `fit_mixture_gaussian`, `fit_mixture_cox`, and `fit_mixture_glm`: replace sum(Delta == 1) == n, which can be true when using a non-default (not intercept-only) mformula.
* `fit_mixture`: data can be a data.frame or list.
* `fit_mixture`: update ifelse() statements used to define fy, initbeta, and initgamma when the control arguments are specified. 

# pldamixture 0.1.1

* CRAN Release: 06-07-2024

* DESCRIPTION: arXiv link re-formatted in the DESCRIPTION file 

* `fit_mixture_gaussian`: reformatted g function so function input vs. y is clearer.

* `fit_mixture_cox`: replaced match(y,times) to improve handling of survival time ties. 

# pldamixture 0.1.0

* CRAN Release: 03-05-2024

* Initial version.




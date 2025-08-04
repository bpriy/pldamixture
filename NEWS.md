### Updates On GitHub:
  
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




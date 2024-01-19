# Post-Linkage Data Analysis Based on Mixture Modelling

The "pldamixture" package can be used for inference in the secondary analysis setting with linked data potentially containing mismatch errors. Only the linked data file may be accessible and information about the record linkage process may be limited or unavailable. Implements the 'General Framework for Regression with Mismatched Data' developed by Slawski et al., 2023. The framework uses a mixture model for pairs of linked records whose two components reflect distributions conditional on match status, i.e., correct match or mismatch. Inference is based on composite likelihood and the EM algorithm. The package currently supports Cox Proportional Hazards Regression (right-censored data only) and Generalized Linear Regression Models (Gaussian, Gamma, Poisson, and Logistic (binary models only)). Information about the underlying record linkage process can be incorporated into the method if available (e.g., assumed overall mismatch rate, safe matches, predictors of match status, or predicted probabilities of correct matches).
  
## References
  
Slawski, M., West, B. T., Bukke, P., Diao, G., Wang, Z., & Ben-David, E. (2023). A General Framework for Regression with Mismatched Data Based on Mixture Modeling. Journal of the Royal Statistical Society (Series A). Submitted. <https://arxiv.org/pdf/2306.00909.pdf>

Bukke, P., Ben-David, E., Diao, G., Slawski, M., & West, B. T. (2023). Cox Proportional Hazards Regression Using Linked Data: An Approach Based on Mixture Modelling. IISA Series on Statistics and Data Science. Submitted.

Slawski, M., Diao, G., Ben-David, E. (2021). A pseudo-likelihood approach to linear regression with partially shuffled data. Journal of Computational and Graphical Statistics. 30(4), 991-1003 <https://doi.org/10.1080/10618600.2020.1870482>

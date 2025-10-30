---
title: "QlearningPlus"
output: github_document
---

# QlearningPlus

`QlearningPlus`is an R package for estimating optimal dynamic treatment regimes (DTRs)
using Q-learning and its extensions. The package integrates standard approaches and
introduces regularized, Bayesian, and deep learning–based methods (via Conditional 
Variational Autoencoders, CVAE), making it suitable for high-dimensional or multi-treatment settings.

## Features

-	Q-learning with Lasso regularization
-	Q-learning with Elastic Net regularization
-	Bayesian Weighted Q-learning for small-sample settings
-	CVAE Q-learning for high-dimensional treatment spaces
-	Multi-stage DTR estimation via backward induction (multi_dtr())

## Installation

``` r
# Install dependencies 

install.packages(c("glmnet",  "rstan", "keras", "tensorflow", "caret"))
keras::install_keras()

# Install the development version from GitHub

devtools::install_github("xz76/QlearningPlus")
```

# Example Usage

The package includes a built-in simulated dataset `tmp` and an example regression formula list `formula_list`.

``` r
# Load built-in dataset and formula
data(tmp)
data(formula_list)
```


## Function Overview

- The `dtr()` function is a unified interface for estimating optimal dynamic treatment regimes (DTRs)
using various extensions of Q-learning. It supports standard regression-based Q-learning, 
Lasso and Elastic Net regularization, Bayesian-weighted Q-learning, and can accommodate 
user-defined priors on treatment effects.

- The `multi_dtr()` function supports multiple stage DTRs by calling `dtr()` based on backward induction.


### Key Features 

- Standard Q-learning via linear regression. 
- Lasso-penalized Q-learning via glmnet. 
- Elastic Net Q-learning via caret and glmnet. 
- Bayesian Q-learning using conjugate normal priors and precision weighting.
- Conditional VAE Q-learning via encoder and decoder process.
- Compatible with multi-treatment and high-dimensional settings


## Example Usage

- The example data `tmp` has two decision points with two variables.
- `formula_list` is the list of formula corresponding to each stages.

``` r
library(QlearningPlus)
data(tmp)
data(formula_list)

# Standard Q-learning
fit_q <- multi_dtr(
    data = tmp,
    stages = 2,
    method = "Qlearning",
    treatment_prefix = "A",
    outcome_list = c("Y1", "Y2"),
    formula_list = formula_list
)

# Lasso-penalized Q-learning
fit_lasso <- multi_dtr(
    data = tmp,
    stages = 2,
    method = "lasso",
    treatment_prefix = "A",
    outcome_list = c("Y1", "Y2"),
    formula_list = formula_list
)

# Elastic Net Q-learning
fit_elnet <- multi_dtr(
    data = tmp,
    stages = 2,
    method = "ElasticNet",
    treatment_prefix = "A",
    outcome_list = c("Y1", "Y2"),
    formula_list = formula_list
)

# Bayesian Q-learning with default non-informative priors
fit_bayes <- multi_dtr(
    data = tmp,
    stages = 2,
    method = "BaysianQ",
    treatment_prefix = "A",
    outcome_list = c("Y1", "Y2"),
    formula_list = formula_list,
    prior_mean = NULL, 
    prior_sd = NULL, 
    sample_sd = NULL
)

# CVAE Q-learning
fit_CVAE <- multi_dtr(
    data = tmp,
    stages = 2,
    method = "CVAE",
    treatment_prefix = "A",
    outcome_list = c("Y1", "Y2"),
    formula_list = formula_list, 
    epoch = 10,
    latent_dim = 2, 
    intermediate_dim = 32
)
```

## Result

The output provides the estimated value function and 
the corresponding recommended treatment, along with method-specific outcomes such as 
posterior distributions in the Bayesian framework.

``` r
## Check stage 2 result
summary(fit_CVAE[[2]])

## Recommendation
fit_CVAE[[2]]$recommend

```

## `plot_dtr_forest` Plot Function

### BayesianQ Method

— Posterior Distribution Visualization

The function `plot_dtr_forest()` provides a clear visualization of the 
posterior distributions of treatment effects obtained from Bayesian Q-learning.
This plot helps illustrate both the uncertainty and the distributional shape of the
posterior samples for each treatment.

``` r
# Assuming fit_bayes is the output from dtr(..., method = "BayesianQ")
plot_dtr_forest(fit_bayes[[1]], method = "BayesianQ")
```

### Lasso / ElasticNet Methods

- Coefficient Visualization

The `plot_dtr_forest()` function can also be used to visualize the 
non-zero coefficients selected by Lasso and Elastic Net models in the QlearningPlus
package. This provides an intuitive, forest-plot-style display of the estimated 
effects of covariates after regularization.

``` r
# fit_lasso is the output from multi_dtr(..., method = "Lasso")
plot_dtr_forest(fit_lasso[[1]], method = "Lasso")

#fit_elnet is the output from  multi_dtr(..., method = "ElasticNet")
plot_dtr_forest(fit_elnet[[1]], method = "ElasticNet")
```

## `qlearning_gof()` Goodness-of-Fit Metrics

The `qlearning_gof()` function computes standard goodness-of-fit (GOF) metrics to 
evaluate how well the model predictions align with the observed outcomes. 
This is particularly useful for comparing models across different Q-learning 
methods (standard, Lasso, ElasticNet).The key metrics provided: RMSE, MAE, R$^2$

``` r
# Check the GOF of the first stage result, assuming fit_lasso is the output from multi_dtr(..., method = "Lasso")
dtr_gof(fit_lasso[[1]], outcome = "Y1")

# Check the GOF of the second stage result, assuming fit_elnet is the output from multi_dtr(..., method = "ElasticNet")
dtr_gof(fit_elnet[[2]], outcome = "Y2")

# Check the GOF plot of the first stage result, assuming fit_q is the output from multi_dtr(..., method = "Qlearning")
qlearning_gof_plot(fit_q[[1]], outcome = "Y1")
```

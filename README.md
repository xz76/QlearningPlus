---
title: "QlearningPlus"
output: github_document
---

# QlearningPlus

`QlearningPlus` is an R package that implements and extends Q-learning methods for estimating optimal dynamic treatment regimes (DTRs). This package integrates standard Q-learning approaches and introduces regularized and Bayesian extensions, including a deep learning-based method using Conditional Variational Autoencoders (CVAE), suitable for high-dimensional or multi-treatment settings.

## Features

-   Standard Q-learning via the `DynTxRegime` package
-   Q-learning with Lasso regularization
-   Q-learning with Elastic Net regularization
-   Bayesian Weighted Q-learning for small-sample settings
-   CVAE Q-learning for high-dimensional treatment spaces

## Installation

``` r
# Install dependencies 

install.packages(c("glmnet",  "rstan", "keras", "tensorflow", "caret")) keras::install_keras()

# Install the development version from GitHub

devtools::install_github("xz76/QlearningPlus")
```

# Example Usage

The package includes a built-in simulated dataset named sample_data and example formula.

``` r
# Load built-in dataset and formula
data(sample_data)
data(f1)

# View first few rows of the data
head(sample_data)
f1
```

Define the regression formula as `f1`

## `dtr()` Function Overview

The dtr() function is a unified interface for estimating optimal dynamic treatment regimes (DTRs) using various extensions of Q-learning. It supports standard regression-based Q-learning, Lasso and Elastic Net regularization, Bayesian-weighted Q-learning, and can accommodate user-defined priors on treatment effects.

Key Features • Standard Q-learning via linear regression. • Lasso-penalized Q-learning via glmnet. • Elastic Net Q-learning via caret and glmnet. • Bayesian Q-learning using conjugate normal priors and precision weighting. • Flexible for multi-treatment settings.

## Example Usage

``` r
library(QlearningPlus)
data(sample_data)
data(f1)

# Standard Q-learning
fit_q <- dtr(data = sample_data, formula = f1, method = "Qlearning")

# Lasso-penalized Q-learning
fit_lasso <- dtr(data = sample_data, formula = f1, method = "Lasso")

# Elastic Net Q-learning
fit_elnet <- dtr(data = sample_data, formula = f1, method = "ElasticNet")

# Bayesian Q-learning with default non-informative priors
fit_bayes <- dtr(data = sample_data, formula = f1, method = "BayesianQ")
```

## `plot_dtr_forest` Plot Function

### BayesianQ Method

— Posterior Distribution Visualization

The function `plot_dtr_forest()` provides a clear visualization of the posterior distributions of treatment effects obtained from Bayesian Q-learning. This plot helps illustrate both the uncertainty and the distributional shape of the posterior samples for each treatment.

``` r
# Assuming fit_bayes is the output from dtr(..., method = "BayesianQ")
plot_dtr_forest(fit_bayes, method = "BayesianQ")
```

### Lasso / ElasticNet Methods

— Coefficient Visualization

The `plot_dtr_forest()` function can also be used to visualize the non-zero coefficients selected by Lasso and Elastic Net models in the QlearningPlus package. This provides an intuitive, forest-plot-style display of the estimated effects of covariates after regularization.

``` r
# Assuming fit_lasso is the output from dtr(..., method = "Lasso")
plot_dtr_forest(fit_lasso, method = "Lasso")

# Assuming fit_elnet is the output from dtr(..., method = "ElasticNet")
plot_dtr_forest(fit_elnet, method = "ElasticNet")
```

## `qlearning_gof()` Goodness-of-Fit Metrics

The `qlearning_gof()` function computes standard goodness-of-fit (GOF) metrics
to evaluate how well the model predictions align with the observed outcomes. 
This is particularly useful for comparing models across different Q-learning 
methods (standard, Lasso, ElasticNet).The key metrics provided: RMSE, MAE, R$^2$

```r
# Assuming fit_lasso is the output from dtr(..., method = "Lasso")
qlearning_gof(fit_lasso)

# Assuming fit_elnet is the output from dtr(..., method = "ElasticNet")
qlearning_gof(fit_elnet)

# Assuming fit_q is the output from dtr(..., method = "Qlearning")
qlearning_gof(fit_q)
```




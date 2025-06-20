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

\`\`\`r \# Install dependencies install.packages(c("glmnet", "DynTxRegime", "rstan", "keras", "tensorflow")) keras::install_keras()

# Install the development version from GitHub

devtools::install_github("xz76/QlearningPlus")

# Example Usage

The package includes a built-in simulated dataset named sample_data.

``` r
library(QlearningPlus)
data(sample_data)
```
Define the regression formula as `f1`

## Q-learning with Lasso

```r
qlearning_lasso(sample_data, formula = f1)
```

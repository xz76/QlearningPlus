---
title: "QlearningPlus"
output: github_document
---

# QlearningPlus

`QlearningPlus` is an R package that implements and extends Q-learning methods for 
estimating optimal dynamic treatment regimes (DTRs). This package integrates standard Q-learning approaches and introduces regularized and Bayesian extensions, including a deep learning-based method using Conditional Variational Autoencoders (CVAE), suitable for high-dimensional or multi-treatment settings.

## Features

-   Standard Q-learning via the `DynTxRegime` package
-   Q-learning with Lasso regularization
-   Q-learning with Elastic Net regularization
-   Bayesian Weighted Q-learning for small-sample settings
-   CVAE Q-learning for high-dimensional treatment spaces

## Installation

```r
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

## Q-learning 

```r
qlearning_dyn(sample_data, formula = f1)
```

## Q-learning with Lasso

```r
qlearning_lasso(sample_data, formula = f1)
```

## Q-learning with Elastic Net

```r
qlearning_elasticnet(sample_data, formula = f1)
```

## Q-learning with Bayesian

```r
qlearning_baysian(sample_data, formula = f1)
`

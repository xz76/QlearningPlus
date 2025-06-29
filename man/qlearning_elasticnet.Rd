% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qlearning_elasticnet.R
\name{qlearning_elasticnet}
\alias{qlearning_elasticnet}
\title{Q-learning with Elastic Net Regularization}
\usage{
qlearning_elasticnet(data, formula = f1)
}
\arguments{
\item{X}{A matrix of covariates.}

\item{A}{A binary treatment vector.}

\item{Y}{Outcome vector.}

\item{alpha}{Elastic net mixing parameter (0 = ridge, 1 = lasso).}

\item{lambda}{Optional regularization strength.}
}
\value{
A fitted glmnet object.
}
\description{
Q-learning with Elastic Net Regularization
}

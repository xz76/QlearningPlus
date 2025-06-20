#' Q-learning with Lasso Regularization
#'
#' Fits a Q-learning model using a lasso penalty via the `glmnet` package.
#'
#' @param X A matrix of covariates.
#' @param A A binary treatment vector (e.g., -1, 1).
#' @param Y Outcome vector.
#' @param lambda Optional lambda value; if NULL, cross-validation will be used.
#' @return A fitted `cv.glmnet` object.
#' @export
qlearning_lasso <- function(X, A, Y, lambda = NULL) {
  X_tilde <- X * A  # interaction
  if (is.null(lambda)) {
    fit <- glmnet::cv.glmnet(X_tilde, Y, alpha = 1)
  } else {
    fit <- glmnet::glmnet(X_tilde, Y, alpha = 1, lambda = lambda)
  }
  return(fit)
}

#' Q-learning with Elastic Net Regularization
#'
#' @param X A matrix of covariates.
#' @param A A binary treatment vector.
#' @param Y Outcome vector.
#' @param alpha Elastic net mixing parameter (0 = ridge, 1 = lasso).
#' @param lambda Optional regularization strength.
#' @return A fitted glmnet object.
#' @export
qlearning_elasticnet <- function(X, A, Y, alpha = 0.5, lambda = NULL) {
  X_tilde <- X * A
  if (is.null(lambda)) {
    fit <- glmnet::cv.glmnet(X_tilde, Y, alpha = alpha)
  } else {
    fit <- glmnet::glmnet(X_tilde, Y, alpha = alpha, lambda = lambda)
  }
  return(fit)
}

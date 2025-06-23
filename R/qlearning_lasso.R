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
qlearning_lasso <- function(data, formula = f1){
  m1 <- glmnetUtils::cv.glmnet(formula, data = data, nfolds = 10)
  coef_mat <- as.matrix(coef(m1, s = "lambda.min"))
  coef_nm <- names(coef_mat[abs(coef_mat[, 1]) != 0, ])
  get_opt <- function(data, model){
    res <- matrix(NA, nrow = nrow(data), ncol = length(unique(data$a)))
    for (i in seq(length(unique(data$a)))){
      data$a <- i
      data$a <- factor(data$a, levels = c("1", "2", "3", "4"))
      res[, i] <- predict(m1, data)
    }
    res
  }
  recommend <- apply(get_opt(data, m1), 1, which.max)
  idx <- cbind(seq_len(nrow(data)), recommend)
  trtvalue <- data[, c("t1","t2","t3", "t4")]
  methodvalue <- mean(trtvalue[idx])
  rate <- mean(recommend == data$opt)
  newdat <- data
  newdat$a <- factor(data$opt, levels = c("1", "2", "3", "4"))
  yhat <- predict(m1, newdata = newdat)
  yopt <- data$intercept + data$value
  mse <- yopt - yhat
  c(agreement = rate, methodvalue = methodvalue,
    optvalue = mean(data$value),
    alpha = 1,  MSE = mean(mse^2))
}

#' Q-learning with Elastic Net Regularization
#'
#' @param X A matrix of covariates.
#' @param A A binary treatment vector.
#' @param Y Outcome vector.
#' @param alpha Elastic net mixing parameter (0 = ridge, 1 = lasso).
#' @param lambda Optional regularization strength.
#' @return A fitted glmnet object.
#' @export
qlearning_elasticnet <- function(data, formula = f1){
  get_best_result = function(caret_fit) {
    best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
    best_result = caret_fit$results[best, ]
    rownames(best_result) = NULL
    best_result
  }
  cv_10 = caret::trainControl(method = "cv", number = 10)

  hit_elnet_int = caret::train(
    formula, data = data,
    method = "glmnet",
    trControl = cv_10,
    tuneLength = 20
  )
  alphaselect <-  get_best_result(hit_elnet_int)$alpha
  m1 <- glmnetUtils::cv.glmnet(formula, data = data, nfolds = 10,
                               alpha = alphaselect)
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
    alpha = 1, MSE = mean(mse^2))
}

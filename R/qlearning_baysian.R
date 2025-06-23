#' Bayesian Weighted Q-learning (Skeleton)
#'
#' @param X Covariates matrix.
#' @param A Treatment vector.
#' @param Y Outcome vector.
#' @param prior_weight Prior weight parameter.
#' @return Placeholder for Bayesian model output.
#' @export
qlearning_bayes <- function(data, formula = f1){
  # mod <- glmnetUtils::cv.glmnet(formula, data = data, nfolds = 10,
  #                               alpha = 1)
  newdat <- data
  newdat$a <- 1
  newdat$a <- factor(newdat$a, levels = c(1:4))
  # e0 <- mean(predict(mod, newdat, s = "lambda.min"))

  e0 <- mean(data$y[data$a == 1])
  meanp <- c(0, -28, 2, 10) + e0
  sd <- c(80, 80, 70, 70)
  sd2 <- c(1, 48, 30, 34)
  miuhat <- do.call(c, lapply(c(1:4), function(b){
    vec <- data$y[data$a == b]
    ybar <- mean(vec)
    miu <- (sd2[b]/(sd[b]/length(vec) + sd2[b]) )*ybar +
      (sd[b]/(sd[b]/length(vec) + sd2[b]) ) * meanp[b]
    miu
  }))
  residual <- do.call(c, lapply(c(1:4), function(b){
    data$y[data$a == b] - miuhat[b]
  }))
  weights <- 1/(abs(scale(residual) + 1))
  m1 <- glmnetUtils::cv.glmnet(formula, data = data, weights = weights,
                               nfolds = 10, alpha = 1)

  coef_mat <- as.matrix(coef(m1, s = "lambda.min"))
  coef_nm <- names(coef_mat[abs(coef_mat[, 1]) != 0, ])
  get_opt <- function(data, model){
    res <- matrix(NA, nrow = nrow(data),
                  ncol = length(unique(data$a)))
    for (i in seq(length(unique(data$a)))){
      data$a <- i
      data$a <- factor(data$a, levels = c("1", "2", "3", "4"))
      res[, i] <- predict(m1, data, s = "lambda.min")
    }
    res
  }
  recommend <- apply(get_opt(data = data, m1), 1, which.max)
  idx <- cbind(seq_len(nrow(data)), recommend)
  trtvalue <- data[, c("t1","t2","t3", "t4")]
  methodvalue <- mean(trtvalue[idx])
  rate <- mean(recommend == data$opt)
  newdat <- data
  newdat$a <- factor(data$opt, levels = c("1", "2", "3", "4"))
  yhat <- predict(m1, newdata = newdat, s = 'lambda.min')
  yopt <- data$intercept + data$value
  mse <- yopt - yhat
  c(agreement = rate, methodvalue = methodvalue,
    optvalue = mean(data$value),
    alpha = 1, MSE = mean(mse^2))
}

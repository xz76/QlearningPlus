#' Wrapper for DynTxRegime::qLearn
#'
#' @param data A data frame.
#' @param ... Additional arguments to `DynTxRegime::qLearn()`.
#' @return Output from `DynTxRegime::qLearn`.
#' @export
qlearning_classQ <-  function(data, formula = f1){
  m1 <- lm(formula, data = data)
  get_opt <- function(data, model){
    res <- matrix(NA, nrow = nrow(data),
                  ncol = length(unique(data$a)))
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

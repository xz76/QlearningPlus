#' Goodness-of-Fit for dtr() Output
#'
#' Computes goodness-of-fit metrics: RMSE, MAE, and R-squared if applicable.
#'
#' @param dtr_result Output from `dtr()`.
#' @param outcome Name of the outcome variable (default = "y").
#' @return A list of goodness-of-fit metrics.
#' @export
dtr_gof <- function(dtr_result, outcome = "y") {

  actual <- dtr_result$data[[outcome]]

  # Predicted outcomes
  if ("model" %in% names(dtr_result)) {
    if (inherits(dtr_result$model, "lm")) {
      predicted <- fitted(dtr_result$model)
    } else if (inherits(dtr_result$model, "cv.glmnet")) {
      predicted <- predict(dtr_result$model, newdata = dtr_result$data, s = "lambda.min")
      predicted <- as.numeric(predicted)
    } else {
      stop("Model type not supported.")
    }
  } else {
    stop("No model found in result.")
  }

  # Metrics
  rss <- sum((actual - predicted)^2)
  tss <- sum((actual - mean(actual))^2)
  r_squared <- 1 - rss / tss

  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))

  metrics <- list(
    RMSE = rmse,
    MAE = mae,
    R_squared = r_squared
  )

  return(metrics)
}


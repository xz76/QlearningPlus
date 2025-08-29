#' Goodness-of-Fit Plot for Q-learning Models
#'
#' This function generates a scatter plot comparing observed vs. predicted outcomes
#' from a fitted Q-learning model object (either `lm` or `cv.glmnet`) as returned by `dtr()`.
#'
#' @param dtr_result A result list returned from the `dtr()` function.
#' @param outcome A character string indicating the name of the outcome variable. Default is `"y"`.
#'
#' @return A ggplot object showing observed vs. predicted values, including a 45-degree reference line.
#' @export
#'
#' @examples
#' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), a = factor(sample(1:2, 100, TRUE)))
#' data$y <- with(data, 2 * as.numeric(a) + 1.5 * x1 - 0.5 * x2 + rnorm(100))
#' fit <- dtr(data = data, formula = y ~ x1 + x2 + a, method = "Qlearning", treatment = "a", outcome = "y")
#' p <- qlearning_gof_plot(fit)
#' print(p)
qlearning_gof_plot <- function(dtr_result, outcome = "y") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  actual <- dtr_result$data[[outcome]]
    predicted <- dtr_result$yhat


  df <- data.frame(
    Observed = actual,
    Predicted = predicted
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = df[, 1], y = Predicted)) +
    ggplot2::geom_point(alpha = 0.6, color = "#0072B2") +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(
      title = "Observed vs. Predicted Outcomes",
      x = "Observed Outcome",
      y = "Predicted Outcome"
    )

  return(p)
}

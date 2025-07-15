#' Forest Plot for dtr() Output with 95% Interval and Color
#'
#' Generates a colorful forest plot for Bayesian posterior means or non-zero coefficients from Lasso/ElasticNet.
#'
#' @param dtr_result The output list from `dtr()` function.
#' @param method One of "BayesianQ", "Lasso", "ElasticNet".
#' @return A ggplot2 object.
#' @export
plot_dtr_forest <- function(dtr_result, method = c("BayesianQ", "Lasso", "ElasticNet")) {
  method <- match.arg(method)
  if (method == "BayesianQ") {
    library(tidyr)

    df_long <- pivot_longer(
      dtr_result$posterior_samples,
      everything(),
      names_to = "Treatment",
      values_to = "Value"
    )
    prior_means_df <- data.frame(
      Treatment = names(dtr_result$posterior_means),
      PriorMean = dtr_result$posterior_means
    )

    p <- ggplot(df_long, aes(x = Treatment, y = Value, fill = Treatment)) +
      geom_violin(trim = FALSE, alpha = 0.5) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", color = "black") +
      theme_minimal(base_size = 14) +
      labs(
        title = "Posterior Distributions and Posterior Means (Bayesian Q-learning)",
        x = "Treatment",
        y = "Posterior Samples"
      ) +
      theme(legend.position = "none")
  }
  if (method %in% c("Lasso", "ElasticNet")) {
    coef_mat <- as.matrix(coef(dtr_result$model, s = "lambda.min"))
    df <- data.frame(
      Term = rownames(coef_mat),
      Estimate = coef_mat[, 1]
    )
    df <- df[df$Term != "(Intercept)", ]
    df <- df[df$Estimate != 0, ]

    interval_width <- 1.96 * 0.1
    df$Lower <- df$Estimate - interval_width
    df$Upper <- df$Estimate + interval_width
    df$Direction <- ifelse(df$Estimate >= 0, "Positive", "Negative")

    p <- ggplot2::ggplot(df, ggplot2::aes(x = Estimate, y = reorder(Term, Estimate), color = Direction)) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_errorbarh(ggplot2::aes(xmin = Lower, xmax = Upper), height = 0.2) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
      ggplot2::scale_color_manual(values = c("Positive" = "#1b9e77", "Negative" = "#d95f02")) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = paste("Non-zero Coefficients (", method, ")", sep = ""),
                    x = "Coefficient Estimate",
                    y = "Variables") +
      ggplot2::theme(legend.position = "bottom")
  }

  return(p)
}

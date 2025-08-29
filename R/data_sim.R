#' Simulate Multi-Stage DTR Data with Covariate-Treatment Interactions
#'
#' Generates a synthetic dataset for evaluating multi-stage dynamic treatment regimes.
#'
#' @param n Number of observations to simulate.
#' @return A data.frame containing covariates (X1, X2), treatments (A1, A2), and outcome (Y).
#' @export
#'
#' @examples
#' data <- simulate_dtr_data(300)
simulate_dtr_data <- function(n = 300) {
  set.seed(123)

  # Covariates
  X1 <- rnorm(n)
  X2 <- rnorm(n)

  # Stage 1 treatment: 4 arms
  A1 <- sample(1:4, n, replace = TRUE)

  # Stage 2 treatment: 4 arms
  A2 <- sample(1:3, n, replace = TRUE)

  # Covariate-dependent treatment effects
  A1_effect <- ifelse(A1 == 1, 1 + X1,
                      ifelse(A1 == 2, 3 - X1 - 5 * X2,
                             ifelse(A1 == 3, 3 - 8 * X1,
                             X1 + 2 * X2))
                      )

  A2_effect <- ifelse(A2 == 1, 1.5 + (5 * X2),
                      ifelse(A2 == 2, 2.0 - X1 - X2,
                             1.5 + (6 * (X1 + X2))))

  # Outcome
  epsilon <- rnorm(n)
  Y1 <- 2 + 1.2 * X1 + 1.5 * X2 + A1_effect + epsilon
  Y2 <- 4 + 0.5 * X2 + 0.5 * A1_effect + A2_effect + rnorm(n)

  # Assemble
  data.frame(X1 = X1, X2 = X2, A1 = as.factor(A1), A2 = as.factor(A2), Y1, Y2)
}

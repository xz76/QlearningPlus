#' Dynamic Treatment Regime Estimation via Q-learning and Extensions
#'
#' Supports standard Q-learning, Lasso, Elastic Net, and Bayesian Weighted Q-learning.
#'
#' @param data A data.frame including covariates, treatment, and outcome.
#' @param formula A formula specifying the model.
#' @param method Method used: "Qlearning", "Lasso", "ElasticNet", or "BayesianQ".
#' @param treatment Treatment column name (default "a").
#' @param outcome Outcome column name (default "y").
#' @param prior_mean Prior means (BayesianQ only).
#' @param prior_sd Prior variances (BayesianQ only).
#' @param sample_sd Sample variances (optional).
#' @param default_var Default variance for weak priors.
#' @return A list containing model, methodvalue, recommended treatments, and more.
#' @export
#'
dtr <-  function(data, formula = f1, method = "Qlearning", treatment = "a", outcome = "y",
                 prior_mean = NULL, prior_sd = NULL, sample_sd = NULL, default_var = 1e6){
  levels <- levels(data[[treatment]])
  if(method == "Qlearning"){
    m1 <- lm(formula, data = data)
    get_opt <- function(data, model){
      res <- matrix(NA, nrow = nrow(data),
                    ncol = length(unique(data[[treatment]])))
      for (i in seq(length(unique(data[[treatment]])))){
        data[[treatment]] <- i
        data[[treatment]] <- factor(data[[treatment]], levels = levels)
        res[, i] <- predict(m1, data)
      }
      res
    }
    recommend <- as.integer(apply(get_opt(data, m1), 1, which.max))
    methodvalue <- mean(apply(get_opt(data, m1), 1, max))
    list(model = summary(m1),
      methodvalue = methodvalue,
      recommend = recommend,
      data = data)
  }
  if (method == "Lasso") {

    m1 <- glmnetUtils::cv.glmnet(formula, data = data, nfolds = 10)

    # Extract non-zero coefficients
    coef_mat <- as.matrix(coef(m1, s = "lambda.min"))
    nonzero_coef <- coef_mat[abs(coef_mat[, 1]) != 0, , drop = FALSE]
    coef_names <- rownames(nonzero_coef)

    # Function to compute predicted outcomes across treatment levels
    get_opt <- function(data, model) {
      treatment_levels <- unique(data[[treatment]])
      res <- matrix(NA, nrow = nrow(data), ncol = length(treatment_levels))

      for (i in seq_along(treatment_levels)) {
        data[[treatment]] <- treatment_levels[i]
        data[[treatment]] <- factor(data[[treatment]], levels = levels)
        res[, i] <- as.numeric(predict(model, newdata = data, s = "lambda.min"))
      }

      return(res)
    }

    # Compute optimal treatment recommendations
    opt_mat <- get_opt(data, m1)
    recommend <- as.integer(apply(opt_mat, 1, which.max))
    methodvalue <- mean(apply(opt_mat, 1, max))

    # Output
    result <- list(
      model = m1,  # Return the glmnet model directly, not summary
      methodvalue = methodvalue,
      recommend = recommend,
      alpha = m1$lambda.min,
      selected_variables = coef_names,
      data = data
    )

    return(result)
  }
  if(method == "ElasticNet"){
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
    nonzero_coef <- coef_mat[abs(coef_mat[, 1]) != 0, , drop = FALSE]
    coef_names <- rownames(nonzero_coef)
    get_opt <- function(data, model){
      res <- matrix(NA, nrow = nrow(data), ncol = length(unique(data[[treatment]])))
      for (i in seq(length(unique(data[[treatment]])))){
        data[[treatment]] <- i
        data[[treatment]] <- factor(data[[treatment]], levels = levels)
        res[, i] <- predict(m1, data)
      }
      res
    }
    # Compute optimal treatment recommendations
    opt_mat <- get_opt(data, m1)
    recommend <- as.integer(apply(opt_mat, 1, which.max))
    methodvalue <- mean(apply(opt_mat, 1, max))

    # Output
    result <- list(
      model = m1,  # Return the glmnet model directly, not summary
      methodvalue = methodvalue,
      recommend = recommend,
      alpha = m1$lambda.min,
      selected_variables = coef_names,
      data = data
    )
    return(result)
  }
  if (method == "BayesianQ") {
    if (is.null(prior_mean)) prior_mean <- rep(0, length(levels))
    if (is.null(prior_sd)) prior_sd <- rep(default_var, length(levels))  # large prior variance -> non-informative
    if (is.null(sample_sd)) {
      sample_sd <- sapply(levels, function(b) var(data[[outcome]][data[[treatment]] == b], na.rm = TRUE))
    }

    n_samples <- 1000  # Posterior samples per treatment

    # Compute posterior means and posterior samples
    posterior_list <- lapply(seq_along(levels), function(i) {
      vec <- data[[outcome]][data[[treatment]] == levels[i]]
      ybar <- mean(vec, na.rm = TRUE)
      n <- length(vec)

      weight_data <- sample_sd[i] / n
      post_var <- 1 / (1 / prior_sd[i] + n / sample_sd[i])  # Posterior variance (conjugate normal)
      post_mean <- (prior_sd[i] / (weight_data + prior_sd[i])) * ybar +
        (weight_data / (weight_data + prior_sd[i])) * prior_mean[i]

      # Sample from posterior
      rnorm(n_samples, mean = post_mean, sd = sqrt(post_var))
    })

    posterior_samples_df <- as.data.frame(do.call(cbind, posterior_list))
    colnames(posterior_samples_df) <- paste0("Treatment_", levels)

    miuhat <- colMeans(posterior_samples_df)
    names(miuhat) <- paste0("treatment_", levels)

    residual <- do.call(c, lapply(seq_along(levels), function(b) {
      data[[outcome]][data[[treatment]] == levels[b]] - miuhat[b]
    }))

    weights <- 1 / (abs(scale(residual)) + 1)

    m1 <- glmnetUtils::cv.glmnet(formula, data = data, weights = weights,
                                 nfolds = 10, alpha = 1)
    coef_mat <- as.matrix(coef(m1, s = "lambda.min"))
    nonzero_coef <- coef_mat[abs(coef_mat[, 1]) != 0, , drop = FALSE]
    coef_names <- rownames(nonzero_coef)

    get_opt <- function(data, model) {
      res <- matrix(NA, nrow = nrow(data), ncol = length(unique(data$a)))
      for (i in seq_along(unique(data$a))) {
        data$a <- i
        data$a <- factor(data[[treatment]], levels = levels)
        res[, i] <- predict(m1, data, s = "lambda.min")
      }
      res
    }

    opt_mat <- get_opt(data, m1)
    recommend <- as.integer(apply(opt_mat, 1, which.max))
    methodvalue <- mean(apply(opt_mat, 1, max))

    result <- list(
      model = m1,
      methodvalue = methodvalue,
      recommend = recommend,
      alpha = m1$lambda.min,
      selected_variables = coef_names,
      posterior_means = miuhat,
      posterior_samples = posterior_samples_df,
      data = data
    )
    return(result)
  }
  if (method == "CVAE") {
    treatment_matrix <- data[[treatment]]  # or your treatment column names
    treatment_factor <- factor(treatment_matrix, levels = 1:length(unique(treatment_matrix)))
    binary_matrix <- model.matrix(~ treatment_factor - 1)

    cvae_result <- fit_cvae_encoder(as.matrix(binary_matrix), latent_dim = 3, epochs = 50)

    encoder_output <- cvae_result$encoder_output

    data_with_latent <- cbind(data, encoder_output)

    result <- list(
      encoder_output = encoder_output,
      encoder_model = cvae_result$encoder_model,
      decoder_model = cvae_result$decoder_model,
      cvae_model = cvae_result$cvae_model,
      data = data_with_latent
    )
  }
}

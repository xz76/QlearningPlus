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
    newdat <- data
    newdat[[treatment]] <- as.factor(recommend)
    psudo_outcome <- predict(m1, newdat)
    return(list(model = summary(m1),
                methodvalue = methodvalue,
                recommend = recommend,
                data = data,
                psudo_outcome = psudo_outcome))
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
    newdat <- data
    newdat[[treatment]] <- as.factor(recommend)
    psudo_outcome <- predict(m1, newdat)
    # Output
    result <- list(
      model = m1,  # Return the glmnet model directly, not summary
      methodvalue = methodvalue,
      recommend = recommend,
      alpha = m1$lambda.min,
      selected_variables = coef_names,
      data = data,
      psudo_outcome = psudo_outcome
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
    newdat <- data
    newdat[[treatment]] <- as.factor(recommend)
    psudo_outcome <- predict(m1, newdat)
    # Output
    result <- list(
      model = m1,  # Return the glmnet model directly, not summary
      methodvalue = methodvalue,
      recommend = recommend,
      alpha = m1$lambda.min,
      selected_variables = coef_names,
      data = data,
      psudo_outcome = psudo_outcome
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
    newdat <- data
    newdat[[treatment]] <- as.factor(recommend)
    psudo_outcome <- predict(m1, newdat)
    result <- list(
      model = m1,
      methodvalue = methodvalue,
      recommend = recommend,
      alpha = m1$lambda.min,
      selected_variables = coef_names,
      posterior_means = miuhat,
      posterior_samples = posterior_samples_df,
      data = data,
      psudo_outcome = psudo_outcome
    )
    return(result)
  }
  if (method == "CVAE") {
    # One-hot encode treatment
    treatment_vector <- as.integer(data[[treatment]])
    treatment_levels <- sort(unique(treatment_vector))
    treatment_factor <- factor(treatment_vector, levels = treatment_levels)
    treatment_matrix <- model.matrix(~ treatment_factor - 1)

    # Fit CVAE
    cvae_result <- fit_cvae_encoder(
      treatment_matrix = treatment_matrix,
      latent_dim = 3,
      intermediate_dim = 16,
      epochs = 50,
      batch_size = 16
    )

    encoder_output <- cvae_result$encoder_output
    z_names <- paste0("z", seq_len(ncol(encoder_output)))
    colnames(encoder_output) <- z_names
    data_with_z <- cbind(data, encoder_output)

    # Build regression formula: Y ~ X1 + X2 + A1 + z1 + z2 + z3
    covars <- setdiff(names(data), c(treatment, outcome))
    formula_z <- as.formula(
      paste(outcome, "~", paste(c(covars, z_names), collapse = " + "))
    )

    # Fit outcome model
    outcome_model <- lm(formula_z, data = data_with_z)

    # Optimize z to maximize predicted Y per subject
    n <- nrow(data)
    latent_dim <- ncol(encoder_output)
    z_grid <- matrix(runif(100 * latent_dim, -2, 2), ncol = latent_dim)
    colnames(z_grid) <- z_names

    predict_for_z <- function(i) {
      subject_row <- data[i, covars, drop = FALSE]
      test_df <- do.call("rbind", replicate(nrow(z_grid), subject_row, simplify = FALSE))
      test_df <- cbind(test_df, z_grid)
      preds <- predict(outcome_model, newdata = test_df)
      best_idx <- which.max(preds)
      best_z <- z_grid[best_idx, , drop = FALSE]
      best_pred <- preds[best_idx]
      list(z = best_z, y = best_pred)
    }

    opt_z_list <- lapply(seq_len(n), predict_for_z)
    opt_z_matrix <- do.call(rbind, lapply(opt_z_list, `[[`, "z"))
    opt_y <- sapply(opt_z_list, `[[`, "y")

    # Decode optimal latent z to treatment recommendation
    decoder_model <- cvae_result$decoder_model
    decoded_matrix <- decoder_model$predict(opt_z_matrix)
    recommend <- apply(decoded_matrix, 1, which.max)

    return(list(
      encoder_output = encoder_output,
      encoder_model = cvae_result$encoder_model,
      decoder_model = decoder_model,
      outcome_model = outcome_model,
      recommend = recommend,
      psudo_outcome = opt_y,
      data = data
    ))
  }
}

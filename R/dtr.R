#' Q-learning with Lasso Regularization, Elastic Net Regularization, Bayesian weights, CVAE
#'
#' @param X A matrix of covariates.
#' @param A A binary treatment vector.
#' @param Y Outcome vector.
#' @param alpha Elastic net mixing parameter (0 = ridge, 1 = lasso).
#' @param lambda Optional regularization strength.
#' @return Method value function, recommend treatment and model summary.
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
 if(method == "BayesianQ"){
   # If no prior means or prior variances are provided, default to non-informative
   if (is.null(prior_mean)) prior_mean <- rep(0, length(levels))
   if (is.null(prior_sd)) prior_sd <- rep(default_var, length(levels))  # large prior variance -> non-informative
   if (is.null(sample_sd)) {
     # Estimate sample variance per group if not supplied
     sample_sd <- sapply(levels, function(b) var(data[[outcome]][data[[treatment]] == b], na.rm = TRUE))
   }
   # Compute posterior means
   miuhat <- sapply( seq_along(levels), function(i) {
     vec <- data[[outcome]][data[[treatment]] == levels[i]]
     ybar <- mean(vec, na.rm = TRUE)
     n <- length(vec)

     weight_data <- sample_sd[i] / n
     post_mean <- (prior_sd[i] / (weight_data + prior_sd[i])) * ybar +
       (weight_data / (weight_data + prior_sd[i])) * prior_mean[i]
     post_mean
   })

   names(miuhat) <- paste0("treatment_", levels)
  residual <- do.call(c, lapply(c(1:length(levels)), function(b){
    data[[outcome]][data[[treatment]] == b] - miuhat[b]
  }))
   weights <- 1/(abs(scale(residual) + 1))

   m1 <- glmnetUtils::cv.glmnet(formula, data = data, weights = weights,
                                nfolds = 10, alpha = 1)
   coef_mat <- as.matrix(coef(m1, s = "lambda.min"))
   nonzero_coef <- coef_mat[abs(coef_mat[, 1]) != 0, , drop = FALSE]
   coef_names <- rownames(nonzero_coef)
   get_opt <- function(data, model){
     res <- matrix(NA, nrow = nrow(data),
                   ncol = length(unique(data$a)))
     for (i in seq(length(unique(data$a)))){
       data$a <- i
       data$a <- factor(data[[treatment]], levels = levels)
       res[, i] <- predict(m1, data, s = "lambda.min")
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
}

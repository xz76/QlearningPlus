#' CVAE Q-learning Method
#'
#' @param X Covariate matrix
#' @param A Treatment matrix or categorical/one-hot
#' @param Y Outcome
#' @param latent_dim Dimension of latent variable Z
#' @param epochs Number of training epochs
#' @return A list containing encoder, decoder, Q model, and treatment decoder
#' @export
cvae_qlearning <- function(X, A, Y, latent_dim = 3, epochs = 100) {
  library(keras)

  input_dim <- ncol(A)
  cond_dim <- ncol(X)

  # Define encoder
  x_input <- layer_input(shape = c(input_dim))
  cond_input <- layer_input(shape = c(cond_dim))
  concat <- layer_concatenate(list(x_input, cond_input))

  h <- concat %>%
    layer_dense(units = 64, activation = "relu")

  z_mean <- h %>% layer_dense(units = latent_dim)
  z_log_var <- h %>% layer_dense(units = latent_dim)

  sampling <- function(args) {
    z_mean <- args[[1]]
    z_log_var <- args[[2]]
    epsilon <- k_random_normal(shape = k_shape(z_mean))
    z_mean + k_exp(0.5 * z_log_var) * epsilon
  }

  z <- list(z_mean, z_log_var) %>%
    layer_lambda(f = sampling)

  # Define decoder
  decoder_input <- layer_input(shape = c(latent_dim))
  decoder_cond <- layer_input(shape = c(cond_dim))
  decoder_concat <- layer_concatenate(list(decoder_input, decoder_cond))
  decoder_output <- decoder_concat %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = input_dim, activation = "softmax")  # For one-hot A

  encoder <- keras_model(list(x_input, cond_input), list(z_mean, z_log_var, z))
  decoder <- keras_model(list(decoder_input, decoder_cond), decoder_output)

  # Placeholder for training loop
  # Fit encoder + decoder using reconstruction loss + KL divergence
  # Then fit Q(X, Z) using another model

  return(list(encoder = encoder, decoder = decoder))
}

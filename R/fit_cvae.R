#' Fit Conditional Variational Autoencoder (CVAE) Encoder
#'
#' @param treatment_matrix Binary matrix of treatment indicators (one-hot encoded)
#' @param latent_dim Dimensionality of latent space
#' @param intermediate_dim Number of units in hidden layer
#' @param epochs Number of training epochs
#' @param batch_size Mini-batch size
#'
#' @return A list with encoder/decoder models and encoder outputs
#' @export
fit_cvae_encoder <- function(treatment_matrix,
                             latent_dim = 3,
                             intermediate_dim = 16,
                             epochs = 50,
                             batch_size = 16) {
  library(keras)
  library(tensorflow)

  original_dim <- ncol(treatment_matrix)

  # Encoder
  encoder_input <- layer_input(shape = original_dim, name = "encoder_input")
  h <- encoder_input %>%
    layer_dense(units = intermediate_dim, activation = "relu")
  z_mean <- h %>%
    layer_dense(units = latent_dim, name = "z_mean")
  z_log_var <- h %>%
    layer_dense(units = latent_dim, name = "z_log_var")

  sampling <- function(args) {
    z_mean <- args[[1]]
    z_log_var <- args[[2]]
    epsilon <- k_random_normal(shape = k_shape(z_mean))
    z_mean + k_exp(0.5 * z_log_var) * epsilon
  }

  z <- list(z_mean, z_log_var) %>%
    layer_lambda(f = sampling, name = "z")

  # Decoder
  decoder_input <- layer_input(shape = latent_dim, name = "decoder_input")
  decoder_output <- decoder_input %>%
    layer_dense(units = intermediate_dim, activation = "relu") %>%
    layer_dense(units = original_dim, activation = "softmax")

  decoder <- keras_model(decoder_input, decoder_output, name = "decoder")
  output <- decoder(z)

  # VAE model
  cvae <- keras_model(encoder_input, output, name = "cvae")

  # Loss function
  vae_loss <- function(x, x_decoded) {
    recon <- loss_categorical_crossentropy(x, x_decoded)
    recon <- k_mean(recon)
    kl <- -0.5 * k_mean(1 + z_log_var - k_square(z_mean) - k_exp(z_log_var))
    recon + kl
  }

  cvae %>% compile(optimizer = "adam", loss = vae_loss)

  # Fit model
  cvae %>% fit(
    x = treatment_matrix,
    y = treatment_matrix,
    epochs = epochs,
    batch_size = batch_size,
    verbose = 1
  )

  # Encoder model (return z_mean only)
  encoder <- keras_model(encoder_input, z_mean)
  encoder_output <- encoder %>% predict(treatment_matrix)

  return(list(
    encoder_output = encoder_output,
    encoder_model = encoder,
    decoder_model = decoder,
    cvae_model = cvae
  ))
}

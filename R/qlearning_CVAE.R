#' Fit Conditional Variational Autoencoder (CVAE) for Treatment Encoding
#'
#' Trains a CVAE on binary treatment assignments, conditioned on the sum of treatments,
#' and returns a 3‐dimensional latent embedding plus the trained encoder/decoder models.
#'
#' @param treatment_matrix A binary matrix (n × p) of treatment indicators (0/1).
#' @param latent_dim Integer; dimensionality of the latent space (default = 3).
#' @param intermediate_dim Integer; units in the hidden dense layer (default = 16).
#' @param epochs Integer; number of training epochs (default = 50).
#' @param batch_size Integer; batch size for training (default = 32).
#' @param verbose Integer; verbosity level passed to `fit()` (default = 1).
#'
#' @return A list with components:
#' \describe{
#'   \item{encoder_output}{A data.frame (n × latent_dim) of latent embeddings.}
#'   \item{encoder_model}{The trained Keras encoder model.}
#'   \item{decoder_model}{The trained Keras decoder model.}
#'   \item{cvae_model}{The full trained CVAE model.}
#'   \item{treatment_matrix}{The original input matrix.}
#'   \item{condition}{Conditioning vector (sum of each row of treatment_matrix).}
#' }
#' @export
library(keras)
library(tensorflow)

fit_cvae_encoder1 <- function(
    treatment_matrix,
    latent_dim       = 3,
    intermediate_dim = 16,
    epochs           = 50,
    batch_size       = 32,
    verbose          = 1
) {
  # dims & conditioning
  input_dim <- ncol(treatment_matrix)
  condition <- matrix(rowSums(treatment_matrix), ncol = 1)

  # Encoder inputs
  t_in <- layer_input(shape = input_dim, name = "treatment")
  c_in <- layer_input(shape = 1,         name = "condition")

  x <- layer_concatenate(list(t_in, c_in)) |>
    layer_dense(units = intermediate_dim, activation = "relu")

  z_mean    <- layer_dense(x, units = latent_dim, name = "z_mean")
  z_log_var <- layer_dense(x, units = latent_dim, name = "z_log_var")

  # <-- TF-native sampling: no R floats, only TF ops -->
  sampling <- function(args) {
    zm <- args[[1]]; zv <- args[[2]]
    dims  <- tf$shape(zm)         # [batch, latent]
    batch <- tf$cast(dims[0], tf$int32)
    lat   <- tf$cast(dims[1], tf$int32)
    shape_tensor <- tf$stack(list(batch, lat))
    eps <- tf$random$normal(shape_tensor, mean = 0.0, stddev = 1.0)
    zm + tf$exp(0.5 * zv) * eps
  }
  z <- layer_lambda(
    f            = sampling,
    output_shape = c(latent_dim),   # just the length of the latent vector
    name         = "z"
  )(list(z_mean, z_log_var))

  # Decoder
  dec <- layer_concatenate(list(z, c_in)) |>
    layer_dense(units = intermediate_dim, activation = "relu") |>
    layer_dense(units = input_dim, activation = "sigmoid")
  cvae <- keras_model(inputs = list(t_in, c_in), outputs = dec)

  # Loss
  vae_loss <- function(y_true, y_pred) {
    rec <- loss_binary_crossentropy(y_true, y_pred)
    kl  <- -0.5 * k_mean(1 + z_log_var - k_square(z_mean) - k_exp(z_log_var), axis = -1)
    rec + kl
  }
  cvae$compile(optimizer = "adam", loss = vae_loss)

  # ensure 0/1 are doubles, not ints
  treatment_matrix <- matrix(
    as.numeric(treatment_matrix),
    nrow = nrow(treatment_matrix),
    ncol = ncol(treatment_matrix)
  )

  condition <- matrix(
    as.numeric(condition),
    nrow = nrow(condition),
    ncol = ncol(condition)
  )

  # Train
  cvae$fit(
    x          = list(treatment = treatment_matrix, condition = condition),
    y          = treatment_matrix,
    epochs     = epochs,
    batch_size = batch_size,
    verbose    = verbose
  )

  # Extract encoder model and latent
  encoder <- keras_model(inputs = list(t_in, c_in), outputs = z_mean)
  latent_z <- encoder$predict(list(treatment_matrix, condition))
  colnames(latent_z) <- paste0("Z", seq_len(latent_dim))

  list(
    encoder_output   = as.data.frame(latent_z),
    encoder_model    = encoder,
    decoder_model    = NULL,        # you can add if needed
    cvae_model       = cvae,
    treatment_matrix = treatment_matrix,
    condition        = condition
  )
}

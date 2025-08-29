#' Fit Conditional Variational Autoencoder (CVAE) for Multi-Treatment Encoding
#'
#' Trains a CVAE using a custom training loop compatible with Keras 3 in R.
#'
#' @param treatment_matrix One-hot encoded treatment matrix (n × k).
#' @param latent_dim Number of latent dimensions (default: 3).
#' @param intermediate_dim Number of hidden units (default: 16).
#' @param epochs Number of training epochs (default: 50).
#' @param batch_size Mini-batch size (default: 16).
#'
#' @return A list containing encoder_output, encoder_model, decoder_model.
#' @export
fit_cvae_encoder <- function(treatment_matrix,
                             latent_dim = 3,
                             intermediate_dim = 16,
                             epochs = 50,
                             batch_size = 16) {
  library(keras)
  library(tensorflow)

  original_dim <- ncol(treatment_matrix)
  n_samples <- nrow(treatment_matrix)

  # Build encoder layers
  encoder_input <- layer_input(shape = original_dim)
  encoder_hidden <- layer_dense(units = intermediate_dim, activation = "relu")
  encoder_z_mean <- layer_dense(units = latent_dim)
  encoder_z_log_var <- layer_dense(units = latent_dim)

  # Build decoder layers
  decoder_hidden <- layer_dense(units = intermediate_dim, activation = "relu")
  decoder_output_layer <- layer_dense(units = original_dim, activation = "softmax")

  # Optimizer
  optimizer <- optimizer_adam()

  # Training loop
  for (epoch in 1:epochs) {
    cat("Epoch", epoch, "\n")
    indices <- sample(1:n_samples)
    for (i in seq(1, n_samples, by = batch_size)) {
      batch_idx <- indices[i:min(i + batch_size - 1, n_samples)]
      x_batch <- treatment_matrix[batch_idx, , drop = FALSE]

      with(tf$GradientTape(persistent = FALSE) %as% tape, {
        # Encode
        h <- encoder_hidden(x_batch)
        z_mean <- encoder_z_mean(h)
        z_log_var <- encoder_z_log_var(h)

        # Reparameterize
        epsilon <- k_random_normal(shape = k_shape(z_mean))
        z <- z_mean + k_exp(0.5 * z_log_var) * epsilon

        # Decode
        h_dec <- decoder_hidden(z)
        x_decoded <- decoder_output_layer(h_dec)

        # Compute losses
        recon_loss <- loss_categorical_crossentropy(x_batch, x_decoded)
        recon_loss <- tf$reduce_mean(recon_loss)
        kl_loss <- -0.5 * tf$reduce_mean(1 + z_log_var - tf$square(z_mean) - tf$exp(z_log_var))
        loss <- recon_loss + kl_loss
      })

      # Collect trainable variables
      trainable_vars <- c(
        encoder_hidden$trainable_variables,
        encoder_z_mean$trainable_variables,
        encoder_z_log_var$trainable_variables,
        decoder_hidden$trainable_variables,
        decoder_output_layer$trainable_variables
      )

      gradients <- tape$gradient(loss, trainable_vars)
      optimizer$apply_gradients(purrr::transpose(list(gradients, trainable_vars)))
    }
  }

  # Define encoder model
  encoder_model <- keras_model(
    inputs = encoder_input,
    outputs = encoder_z_mean(encoder_hidden(encoder_input))
  )

  # Define decoder model
  decoder_input <- layer_input(shape = latent_dim)
  decoder_model <- keras_model(
    inputs = decoder_input,
    outputs = decoder_output_layer(decoder_hidden(decoder_input))
  )

  # Get latent encodings
  encoder_output <- encoder_model$predict(treatment_matrix)

  return(list(
    encoder_output = encoder_output,
    encoder_model = encoder_model,
    decoder_model = decoder_model,
    cvae_model = NULL  # model trained via custom loop
  ))
}

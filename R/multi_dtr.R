#' Multi-Stage Dynamic Treatment Regime Estimation via Backward Induction
#'
#' Performs backward induction for multi-stage DTR estimation using Q-learning and its extensions.
#'
#' @param data A data.frame with covariates, stage-wise treatment columns (e.g., A1, A2, ...), and outcome.
#' @param stages Number of decision stages (e.g., 2 for A1 and A2).
#' @param method Q-learning method: "Qlearning", "Lasso", "ElasticNet", "BayesianQ", or "CVAE".
#' @param treatment_prefix Prefix of treatment variables (e.g., "A" for A1, A2, ...).
#' @param outcome Name of the outcome variable (default: "Y").
#' @param formula_list A named list of formulas (e.g., list("1" = Y ~ X1 + X2 + A1, "2" = Y ~ X1 + X2 + A1 + A2)).
#'                     Alternatively, a function formula_fn(stage) can be used.
#' @param use_individual_pseudoY If TRUE, use per-individual max predicted values as pseudo-outcomes (default: TRUE).
#' @param ... Additional arguments passed to the `dtr()` function.
#'
#' @return A list of results for each stage, from last to first.
#' @export
multi_dtr <- function(data,
                      stages,
                      method = "Qlearning",
                      treatment_prefix = "A",
                      outcome_list = NULL,
                      formula_list = NULL,
                      formula_fn = NULL,
                      use_individual_pseudoY = TRUE,
                      ...) {

  if (is.null(outcome_list) || length(outcome_list) != stages) {
    stop("Please provide outcome_list with one outcome name per stage (e.g., c('Y1', 'Y2')).")
  }

  results <- vector("list", stages)
  pseudoY <- data[[outcome_list[stages]]]  # true Y for the final stage

  for (stage in stages:1) {
    trt_col <- paste0(treatment_prefix, stage)
    outcome <- outcome_list[stage]
    temp_data <- data
    temp_data[[outcome]] <- pseudoY
    temp_data[[trt_col]] <- factor(temp_data[[trt_col]])

    # Get formula for this stage
    stage_formula <- if (!is.null(formula_list)) {
      if (as.character(stage) %in% names(formula_list)) {
        formula_list[[as.character(stage)]]
      } else {
        stop(paste0("No formula provided for stage ", stage))
      }
    } else if (!is.null(formula_fn)) {
      formula_fn(stage)
    } else {
      stop("Must provide either `formula_list` or `formula_fn`.")
    }

    # Replace LHS of formula with correct outcome
    rhs <- paste(all.vars(stage_formula)[-1], collapse = " + ")
    stage_formula <- as.formula(paste0(outcome, " ~ ", rhs))

    # Run dtr
    dtr_result <- dtr(
      data = temp_data,
      formula = stage_formula,
      method = method,
      treatment = trt_col,
      outcome = outcome,
      ...
    )

    results[[stage]] <- dtr_result

    # Prepare pseudoY for earlier stage
    if (stage > 1) {
      if (use_individual_pseudoY) {
        if (!is.null(dtr_result$psudo_outcome)) {
          pseudoY <- data[[outcome_list[stage - 1]]] + dtr_result$psudo_outcome
        } else {
          stop("`psudo_outcome` not found in dtr_result.")
        }
      } else {
        pseudoY <- data[[outcome_list[stage - 1]]] + rep(dtr_result$methodvalue, nrow(data))
      }
    }
  }

  return(results)
}

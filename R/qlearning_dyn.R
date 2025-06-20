#' Wrapper for DynTxRegime::qLearn
#'
#' @param data A data frame.
#' @param ... Additional arguments to `DynTxRegime::qLearn()`.
#' @return Output from `DynTxRegime::qLearn`.
#' @export
qlearning_dyn <- function(data, ...) {
  DynTxRegime::qLearn(data = data, ...)
}

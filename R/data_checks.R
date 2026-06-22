#' Check that required session column names are set and present in the data
#'
#' This function checks that the required column names have been set (e.g. using set_colnames)
#' and that these columns are present in the provided sessions data frame.
#' @param sessions A sessions data frame to check for required columns.
#' @param required_cols A character vector of required column identifiers.
#' @returns No return value, called for its side effects.
#' @keywords internal
#' @export
#' @examples
#' check_session_colnames(example_sessions, required_cols = c("time_at_sleep_onset", "time_at_wakeup"))
check_session_colnames <- function(sessions, required_cols, call = rlang::caller_env()) {
  col <- get_session_colnames(sessions)
  unset_cols <- required_cols[sapply(col[required_cols], is.null)]
  if (length(unset_cols) > 0) {
    cli::cli_abort(c(
      "x" = "Some required column names have not been set",
      "i" = "Unset column names: {paste(unset_cols, collapse = ', ')}",
      "i" = "Use {.fn set_colnames} to set column names."
    ), call = call)
  }
  missing_cols <- setdiff(col[required_cols], names(sessions))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "x" = "Some column names were set but are not present in the session data",
      "i" = "Missing columns in data: {paste(missing_cols, collapse = ', ')}",
      "i" = "Check the data or use {.fn set_colnames} to set correct column names."
    ), call = call)
  }
}

#' Check that required epoch column names are set and present in the data
#'
#' This function checks that the required column names have been set (e.g. using set_colnames)
#' and that these columns are present in the provided epochs data frame.
#' @param epochs An epochs data frame to check for required columns.
#' @param required_cols A character vector of required column identifiers.
#' @returns No return value, called for its side effects.
#' @keywords internal
#' @export
#' @examples
#' check_epoch_colnames(example_epochs, required_cols = c("timestamp"))
check_epoch_colnames <- function(epochs, required_cols, call = rlang::caller_env()) {
  col <- get_epoch_colnames(epochs)
  unset_cols <- required_cols[sapply(col[required_cols], is.null)]
  if (length(unset_cols) > 0) {
    cli::cli_abort(c(
      "x" = "Some required epoch column names have not been set",
      "i" = "Unset column names: {paste(unset_cols, collapse = ', ')}",
      "i" = "Use {.fn set_colnames} to set column names."
    ), call = call)
  }
  missing_cols <- setdiff(col[required_cols], names(epochs))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "x" = "Some column names were set but are not present in the epoch data",
      "i" = "Missing columns in data: {paste(missing_cols, collapse = ', ')}",
      "i" = "Check the data or use {.fn set_colnames} to set correct column names."
    ), call = call)
  }
}

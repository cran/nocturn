#' Get a list of standard column names for sessions data
#'
#' This function retrieves the standard column names for sessions data.
#' If the column names are not set as an attribute, it attempts to infer them
#' from a list of common column names.
#' @param sessions A data frame containing sessions data
#' @returns A named list of column names
#' @family internal
#' @export
#' @examples
#' get_session_colnames(example_sessions)
get_session_colnames <- function(sessions) {
  col_names <- attr(sessions, "col")
  if (is.null(col_names)) {
    col_names <- .sessions_col_none
  }

  for (col in names(col_names)) {
    if (is.null(col_names[[col]])) {
      for (name in .sessions_col_presets[[col]]) {
        if (name %in% colnames(sessions)) {
          col_names[[col]] <- name
          break
        }
      }
    }
  }
  col_names
}

#' Get a list of standard column names for epochs data
#'
#' This function retrieves the standard column names for epochs data.
#' If the column names are not set as an attribute, it attempts to infer them
#' from a list of common column names.
#' @param epochs A data frame containing epochs data
#' @returns A named list of column names
#' @family internal
#' @export
#' @examples
#' get_epoch_colnames(example_epochs)
get_epoch_colnames <- function(epochs) {
  col_names <- attr(epochs, "col")
  if (is.null(col_names)) {
    col_names <- .epochs_col_none
  }

  for (col in names(col_names)) {
    if (is.null(col_names[[col]])) {
      for (name in .epochs_col_presets[[col]]) {
        if (name %in% colnames(epochs)) {
          col_names[[col]] <- name
          break
        }
      }
    }
  }
  col_names
}

#' Get standard column names for a data frame (sessions or epochs)
#'
#' Dispatch to the appropriate function based on the dataframe type attribute (expecting "sessions" or "epochs").
#' @param df A data frame containing either sessions or epochs data
#' @returns A named list of column names
#' @family internal
#' @export
#' @examples
#' get_colnames(example_sessions)
get_colnames <- function(df) {
  type <- attr(df, "type")
  if (is.null(type)) {
    NULL
  } else if (type == "sessions") {
    get_session_colnames(df)
  } else if (type == "epochs") {
    get_epoch_colnames(df)
  } else {
    cli::cli_abort(c("x" = "Unknown data type: {.val {type}}.",
                     "i" = "Data type must be \"sessions\" or \"epochs\".",
                     "i" = "Use {.fn set_data_type} to set the data type attribute."))
  }
}

#' Set column names for a data frame (sessions or epochs)
#'
#' This function updates the "col" attribute for a dataframe, overwriting existing column names with the ones provided.
#' @param df A data frame
#' @param col A named list of column names
#' @returns The data frame with updated "col" attribute
#' @family internal
#' @export
#' @examples
#' sessions <- set_colnames(
#'         example_sessions,
#'         list(night = "calendar_date", sleep_period = "duration_sleep")
#' )
set_colnames <- function(df, col) {
  existing_col <- attr(df, "col")
  merged_col <- existing_col
  for (nm in names(col)) {
    merged_col[[nm]] <- col[[nm]]
  }
  attr(df, "col") <- merged_col
  df
}

#' Rename sessions columns to canonical names
#'
#' @param df The input dataframe (sessions or epochs) to rename columns on
#' @returns The updated dataframe with missing canonical columns created
#' @keywords internal
colnames_to_canonical <- function(df) {
  col <- get_colnames(df)
  for (canon in names(col)) {
    src <- col[[canon]]

    if (is.null(src)) next
    if (!src %in% names(df)) next

    df[[canon]] <- df[[src]]
  }
  set_colnames(df, col)
}

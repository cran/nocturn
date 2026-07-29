#' Make a summary of session information
#'
#' Summarise session information, including the number of sessions, mean session length,
#' mean time at sleep onset and wakeup, subject and device ID.
#' @param sessions The sessions dataframe.
#' @details This function uses columns:
#' - `time_at_sleep_onset`
#' - `time_at_wakeup`
#' - `time_in_bed`
#' - `sleep_period`
#' @returns A single-row dataframe summarising session information.
#' @importFrom rlang .data
#' @export
#' @family data tables
#' @examples
#' get_sessions_summary(example_sessions)
#' @seealso [get_epochs_summary()] to summarise epoch information.
get_sessions_summary <- function(sessions) {
  if (nrow(sessions) == 0) {
    return(data.frame(
      total_sessions = 0,
      mean_sleep_onset = NA,
      mean_wakeup_time = NA,
      mean_time_in_bed = NA,
      sleep_efficiency = NA
    ))
  }

  has_time_in_bed <- "time_in_bed" %in% names(sessions)
  has_sleep_period <- "sleep_period" %in% names(sessions)

  summary <- sessions |>
    dplyr::summarise(
      total_sessions = dplyr::n_distinct(.data$id, na.rm = TRUE),
      mean_sleep_onset = mean_time(.data$time_at_sleep_onset),
      mean_wakeup_time = mean_time(.data$time_at_wakeup),
      mean_time_in_bed = if (has_time_in_bed) mean(.data$time_in_bed) / 3600 else NA,
      sleep_efficiency = {
        if (has_time_in_bed && has_sleep_period) {
          round(mean(.data$sleep_period, na.rm = TRUE) / mean(.data$time_in_bed, na.rm = TRUE), 2)
        } else {
          NA
        }
      }
    )

  if ("annotation" %in% names(sessions)) {
    annot <- sessions$annotation[sessions$annotation != ""]
    summary$annotations <- dplyr::n_distinct(annot)
  }
  summary
}

#' Summarise epoch information
#'
#' Display the number of sessions in the epoch data, as well as the start and end dates of the epoch data
#' @param epochs The epochs dataframe
#' @details This function uses columns:
#' - `timestamp`
#' - `session_id`
#' @returns A single-row dataframe summarising epoch information
#' @importFrom rlang .data
#' @export
#' @family data tables
#' @examples
#' get_epochs_summary(example_epochs)
#' @seealso [get_sessions_summary()] to summarise session information.
get_epochs_summary <- function(epochs) {
  if (nrow(epochs) == 0) {
    return(data.frame(total_sessions = 0, start_date = NA, end_date = NA))
  }

  epochs |>
    dplyr::summarise(
      total_sessions = dplyr::n_distinct(.data$session_id),
      start_date = if ("timestamp" %in% names(epochs)) format(min(parse_time(.data$timestamp), na.rm = TRUE), "%Y-%m-%d") else NA,
      end_date = if ("timestamp" %in% names(epochs)) format(max(parse_time(.data$timestamp), na.rm = TRUE), "%Y-%m-%d") else NA
    )
}

#' Get a column from a dataframe safely
#'
#' Retrieve a column from a dataframe.
#' If the column does not exist or is NULL, it returns a vector of NULLs of the same length as the number of rows in the dataframe.
#' @param df A dataframe
#' @param col The name of the column to retrieve
#' @returns A vector containing the values of the specified column, or a vector of NULLs if the column does not exist
#' @family internal
#' @export
#' @examples
#' get_col(example_sessions, "session_start")
#' get_col(example_sessions, "this_col_does_not_exist")
get_col <- function(df, col) {
  if (is.null(col) || !col %in% names(df)) {
    rep(list(NULL), nrow(df))
  } else {
    df[[col]]
  }
}

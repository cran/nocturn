#' Clean session data
#'
#' @param sessions A dataframe containing the session data
#' @returns The cleaned-up sessions dataframe
#' @examples
#' sessions <- clean_sessions(nocturn::example_sessions)
#' @importFrom rlang .data
#' @export
#' @examples
#' clean_sessions(example_sessions)
clean_sessions <- function(sessions) {
  col <- get_session_colnames(sessions)

  # Create session IDs if they do not exist
  if (is.null(col$id)) {
    sessions <- sessions |>
      dplyr::mutate(session_id = dplyr::row_number())
    col$id <- "session_id"
  }
  # For GGIR data: parse start_end_window to session_start and session_end
  if (is.null(col$session_start) && is.null(col$session_end) && "start_end_window" %in% colnames(sessions)) {
    sessions <- sessions |>
      dplyr::mutate(
        session_start = sub("-.*$", "", .data$start_end_window) |>
          parse_time() |>
          update_date(.data$calendar_date),
        session_end = sub("^[^-]*-", "", .data$start_end_window) |>
          parse_time() |>
          update_date(.data$calendar_date) + lubridate::days(1),
      )
    col$session_start <- "session_start"
    col$session_end   <- "session_end"
  }
  # Parse session_start time
  if (!is.null(col$session_start)) {
    sessions[[col$session_start]] <- parse_time(sessions[[col$session_start]])
  }
  # Parse session_end time
  if (!is.null(col$session_end)) {
    sessions[[col$session_end]] <- parse_time(sessions[[col$session_end]])
  }
  #Convert night to date if it exists
  if (!is.null(col$night)) {
    sessions[[col$night]] <- parse_date(sessions[[col$night]])
  }
  # Create night column if it doesn't exist
  if (is.null(col$night) && !is.null(col$session_start)) {
    sessions <- group_sessions_by_night(sessions)
    col$night <- "night"
  }
  # Set Birth Year to numerical
  if (!is.null(col$birth_year)) {
    sessions[[col$birth_year]] <- as.numeric(sessions[[col$birth_year]])
  }

  # Parse time at sleep onset time or create it if possible
  if (!is.null(col$time_at_sleep_onset)) {
    sessions[[col$time_at_sleep_onset]] <- parse_time(sessions[[col$time_at_sleep_onset]])
  } else if (!is.null(col$session_start) && !is.null(col$sleep_onset_latency)) {
    sessions$time_at_sleep_onset <- sessions[[col$session_start]] + as.numeric(sessions[[col$sleep_onset_latency]])
    col$time_at_sleep_onset <- "time_at_sleep_onset"
  }

  # Parse time at wakeup time
  if (!is.null(col$time_at_wakeup)) {
    sessions[[col$time_at_wakeup]] <- parse_time(sessions[[col$time_at_wakeup]])
  }
  # Set time_in_bed to numerical, or create it if possible
  if (!is.null(col$time_in_bed)) {
    sessions[[col$time_in_bed]] <- as.numeric(sessions[[col$time_in_bed]])
  } else if (!is.null(col$session_start) && !is.null(col$session_end)) {
    sessions$time_in_bed <- time_diff(sessions[[col$session_start]], sessions[[col$session_end]], unit = "second")
    col$time_in_bed <- "time_in_bed"
  }
  # Set sleep_period as numerical and create if possible
  if (!is.null(col$sleep_period)) {
    sessions[[col$sleep_period]] <- as.numeric(sessions[[col$sleep_period]])
  } else if (!is.null(col$time_at_sleep_onset) && !is.null(col$time_at_wakeup)) {
    sessions$sleep_period <- time_diff(sessions[[col$time_at_sleep_onset]], sessions[[col$time_at_wakeup]], unit = "second")
    col$sleep_period <- "sleep_period"
  }
  # For GGIR data: convert dur_spt_sleep_min (part 5) or SleepDurationInSpt (part 4) to seconds
  if (identical(col$sleep_period, "dur_spt_sleep_min")) {
    sessions$dur_spt_sleep_sec <- sessions$dur_spt_sleep_min * 60
    col$sleep_period <- "dur_spt_sleep_sec"
  } else if (identical(col$sleep_period, "SleepDurationInSpt")) {
    sessions$SleepDurationInSpt_sec <- sessions$SleepDurationInSpt * 60 * 60
    col$sleep_period <- "SleepDurationInSpt_sec"
  }
  # Parse time at midsleep time, or create it if possible
  if (!is.null(col$time_at_midsleep)) {
    sessions[[col$time_at_midsleep]] <- parse_time(sessions[[col$time_at_midsleep]])
  } else if (!is.null(col$time_at_sleep_onset) && !is.null(col$sleep_period)) {
    sessions$time_at_midsleep <- sessions[[col$time_at_sleep_onset]] + (sessions[[col$sleep_period]] / 2)
    col$time_at_midsleep <- "time_at_midsleep"
  }
  # Create is_workday if possible
  if (identical(col$is_workday, "daytype") && !is.logical(sessions[[col$is_workday]])) {
    sessions[[col$is_workday]] <- ifelse(sessions$daytype == "WD", TRUE, FALSE) # Parse GGIR format
  } else if (is.null(col$is_workday) && !is.null(col$night)) {
    sessions$is_workday <- !(weekdays(sessions[[col$night]]) %in% c("Saturday", "Sunday"))
    col$is_workday <- "is_workday"
  }
  # Calculate sleep onset latency if possible
  if (is.null(col$sleep_onset_latency) && !is.null(col$session_start) && !is.null(col$time_at_sleep_onset)) {
    sessions$sleep_onset_latency <- time_diff(sessions[[col$session_start]], sessions[[col$time_at_sleep_onset]], unit = "second")
    col$sleep_onset_latency <- "sleep_onset_latency"
  }
  set_colnames(sessions, col)
}

#' Clean epoch data
#'
#' @param epochs A dataframe containing the epoch data
#' @returns The cleaned-up epochs dataframe
#' @examples
#' epochs <- clean_epochs(nocturn::example_epochs)
#' @importFrom rlang .data
#' @export
#' @examples
#' clean_epochs(example_epochs)
clean_epochs <- function(epochs) {
  col <- get_epoch_colnames(epochs)

  # Somnofy_v1: if session IDs are missing, create them from filenames
  if (is.null(col$session_id)) {
    epochs$session_id <- stringr::str_extract(epochs$filename[1], "^[^.]+")
    col$session_id <- "session_id"
  }
  # GGIR: parse timenum to POSIXct
  if (!is.null(col$timestamp) && identical(col$timestamp, "timenum")) {
    epochs[[col$timestamp]] <- as.POSIXct(epochs[[col$timestamp]], origin = "1970-01-01", tz = "Europe/London")
  }
  # Set is_asleep column
  if (!is.null(col$sleep_stage) && identical(col$sleep_stage, "class_id")) {  # GGIR format
    epochs$is_asleep <- ifelse(epochs[[col$sleep_stage]] == 0, 1, 0) # is_asleep: 0 = awake, 1 = asleep
    col$is_asleep <- "is_asleep"
  } else if (!is.null(col$sleep_stage) && col$sleep_stage == "sleep_stage") {  # Somnofy format
    epochs$is_asleep <- ifelse(epochs[[col$sleep_stage]] %in% c(4, 5), 0, 1)
    col$is_asleep <- "is_asleep"
  }
  #Convert night to date if it exists
  if (!is.null(col$night)) {
    epochs[[col$night]] <- parse_date(epochs[[col$night]])
  }
  # Create night column if it doesn't exist
  if (is.null(col$night) && !is.null(col$timestamp)) {
    epochs <- group_epochs_by_night(epochs)
    col$night <- "night"
  }
  set_colnames(epochs, col)
}

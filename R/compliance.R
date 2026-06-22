#' Set minimum time in bed
#'
#' @param sessions The sessions dataframe
#' @param min_time_in_bed The minimum time in bed in hours
#' @param return_mask If TRUE, return a logical vector indicating which sessions meet the minimum time in bed requirement
#' @details This function uses columns:
#' - `time_in_bed`
#' @returns The sessions dataframe with only the sessions that meet the minimum time in bed requirement, or a logical vector if `return_mask` is TRUE
#' @export
#' @family filtering
#' @examples
#' filtered_sessions <- set_min_time_in_bed(example_sessions, 2)
set_min_time_in_bed <- function(sessions, min_time_in_bed, return_mask = FALSE) {
  check_session_colnames(sessions, "time_in_bed")
  col <- get_session_colnames(sessions)

  mask <- sessions[[col$time_in_bed]] >= min_time_in_bed * 60 * 60

  mask[is.na(mask)] <- FALSE

  if (return_mask) {
    mask
  } else {
    sessions[mask, ]
  }
}

#' Set minimum sleep period
#'
#' @param sessions The sessions dataframe
#' @param min_sleep_period The minimum sleep period in hours
#' @param return_mask If TRUE, return a logical vector indicating which sessions meet the minimum sleep period requirement
#' @details This function uses columns:
#' - `sleep_period`
#' @returns The sessions dataframe with only the sessions that meet the minimum sleep period requirement, or a logical vector if `return_mask` is TRUE
#' @export
#' @family filtering
#' @examples
#' filtered_sessions <- set_min_sleep_period(example_sessions, 2)
set_min_sleep_period <- function(sessions, min_sleep_period, return_mask = FALSE) {
  check_session_colnames(sessions, "sleep_period")
  col <- get_session_colnames(sessions)

  mask <- sessions[[col$sleep_period]] >= min_sleep_period * 60 * 60

  mask[is.na(mask)] <- FALSE

  if (return_mask) {
    mask
  } else {
    sessions[mask, ]
  }
}

#' Set session start time range
#'
#' @param sessions The sessions dataframe
#' @param from_time Include sessions that started after this time (in format HH:MM)
#' @param to_time Include sessions that started before this time (in format HH:MM)
#' @param return_mask If TRUE, returns a logical vector indicating which sessions meet the time range requirement
#' @details This function uses columns:
#' - `session_start`
#' @returns The sessions dataframe with only the sessions that started within the specified time range, or a logical vector if `return_mask` is TRUE
#' @export
#' @family filtering
#' @seealso [set_session_sleep_onset_range()] to filter sessions based on sleep onset time.
#' @examples
#' filtered_sessions <- set_session_start_time_range(example_sessions, "22:00", "06:00")
set_session_start_time_range <- function(sessions, from_time, to_time, return_mask = FALSE) {
  check_session_colnames(sessions, "session_start")
  col <- get_session_colnames(sessions)

  session_times <- update_date(sessions[[col$session_start]], "0000-01-01")
  from_time <- if (is.null(from_time)) min(session_times) else parse_time(from_time)
  to_time <- if (is.null(to_time)) max(session_times) else parse_time(to_time)

  if (from_time <= to_time) {
    mask <- session_times >= from_time & session_times <= to_time
  } else {
    mask <- session_times >= from_time | session_times <= to_time
  }

  mask[is.na(mask)] <- FALSE

  if (return_mask) {
    mask
  } else {
    sessions[mask, ]
  }
}

#' Set sleep onset time range
#'
#' @param sessions The sessions dataframe
#' @param from_time Include sessions where sleep started after this time (in format HH:MM)
#' @param to_time Include sessions where sleep started before this time (in format HH:MM)
#' @param return_mask If TRUE, returns a logical vector indicating which sessions meet the sleep onset time range requirement
#' @details This function uses columns:
#' - `time_at_sleep_onset`
#' @returns The sessions dataframe with only the sessions where sleep started within the specified time range,
#' or a logical vector if `return_mask` is TRUE
#' @export
#' @family filtering
#' @seealso [set_session_start_time_range()] to filter sessions based on start time.
#' @examples
#' filtered_sessions <- set_session_sleep_onset_range(example_sessions, "22:00", "06:00")
set_session_sleep_onset_range <- function(sessions, from_time, to_time, return_mask = FALSE) {
  check_session_colnames(sessions, "time_at_sleep_onset")
  col <- get_session_colnames(sessions)

  session_times <- update_date(sessions[[col$time_at_sleep_onset]], "0000-01-01")
  from_time <- if (is.null(from_time)) min(session_times, na.rm = TRUE) else parse_time(from_time)
  to_time <- if (is.null(to_time)) max(session_times, na.rm = TRUE) else parse_time(to_time)

  if (from_time <= to_time) {
    mask <- session_times >= from_time & session_times <= to_time
  } else {
    mask <- session_times >= from_time | session_times <= to_time
  }

  mask[is.na(mask)] <- FALSE

  if (return_mask) {
    mask
  } else {
    sessions[mask, ]
  }
}

#' Remove sessions with no sleep
#'
#' @param sessions The sessions dataframe
#' @param return_mask If TRUE, returns a logical vector indicating which sessions have a sleep period greater than 0
#' @details This function uses columns:
#' - `sleep_period`
#' @returns The sessions dataframe with only the sessions that have a sleep period greater than 0, or a logical vector if `return_mask` is TRUE
#' @export
#' @family filtering
#' @examples
#' filtered_sessions <- remove_sessions_no_sleep(example_sessions)
remove_sessions_no_sleep <- function(sessions, return_mask = FALSE) {
  check_session_colnames(sessions, "sleep_period")
  col <- get_session_colnames(sessions)

  mask <- sessions[[col$sleep_period]] > 0

  mask[is.na(mask)] <- FALSE

  if (return_mask) {
    mask
  } else {
    sessions[mask, ]
  }
}

#' Get non-complying sessions (i.e. where there is more than one session on the same day)
#'
#' @param sessions The sessions dataframe
#' @details This function uses columns:
#' - `night`
#' @returns The sessions dataframe with only the sessions that are non-complying
#' @export
#' @family data tables
#' @examples
#' duplicate_sessions <- get_non_complying_sessions(example_sessions)
get_non_complying_sessions <- function(sessions) {
  check_session_colnames(sessions, "night")
  col <- get_session_colnames(sessions)
  dup_mask <- duplicated(sessions[, c(col$night, col$subject_id)]) |
    duplicated(sessions[, c(col$night, col$subject_id)], fromLast = TRUE)
  sessions[dup_mask, ]
}

#' Get a table of sessions that were removed during filtering
#'
#' @param sessions The original sessions dataframe
#' @param filtered_sessions The filtered sessions dataframe
#' @details This function uses columns:
#' - `id`
#' - `sleep_period`
#' @returns The sessions dataframe with only the sessions that were removed during filtering
#' @export
#' @family data tables
#' @examples
#' filtered_sessions <- set_session_start_time_range(example_sessions, "22:00", "06:00")
#' removed_sessions <- get_removed_sessions(example_sessions, filtered_sessions)
get_removed_sessions <- function(sessions, filtered_sessions) {
  check_session_colnames(sessions, c("id", "sleep_period"))
  check_session_colnames(filtered_sessions, c("id", "sleep_period"))
  col <- get_session_colnames(sessions)

  if (nrow(filtered_sessions) > nrow(sessions)) {
    cli::cli_abort(c(
      "!" = "There are more rows in filtered sessions than in sessions.",
      "i" = "Have you accidentally swapped the function arguments?",
      "i" = "get_removed_sessions(sessions, filtered_sessions)"
    ))
  }
  sessions |>
    dplyr::anti_join(filtered_sessions, by = col$id) |>
    remove_sessions_no_sleep()
}

#' Filter epochs based on session IDs
#'
#' @param epochs The epochs dataframe
#' @param sessions The sessions dataframe
#' @param return_mask If TRUE, returns a logical vector indicating which epochs belong to the specified sessions
#' @details This function uses sessions columns:
#' - `id`
#' And epoch columns:
#' - `session_id`
#' @returns The epochs dataframe with only the epochs that belong to the specified sessions, or a logical vector if `return_mask` is TRUE
#' @export
#' @examples
#' # Apply filtering to sessions to keep specific nights, and filter epochs accordingly
#' filtered_sessions <- filter_by_night_range(example_sessions, "2025-04-07", "2025-04-10")
#' filtered_epochs <- filter_epochs_from_sessions(example_epochs, filtered_sessions)
#'
#' @seealso [filter_by_night_range()] to filter sessions by night range.
#' @family filtering
filter_epochs_from_sessions <- function(epochs, sessions, return_mask = FALSE) {
  check_session_colnames(sessions, "id")
  check_epoch_colnames(epochs, "session_id")
  scol <- get_session_colnames(sessions)
  ecol <- get_epoch_colnames(epochs)

  if (is.null(ecol$session_id) || is.null(scol$id)) {
    mask <- rep(FALSE, nrow(epochs))
    if (return_mask) {
      return(mask)
    } else {
      return(epochs[mask, ])
    }
  }

  if (sum(epochs[[ecol$session_id]] %in% unique(sessions[[scol$id]])) == 0) {
    cli::cli_warn(c("!" = "None of the epochs match the selected sessions.",
                    "i" = "Returning an empty epoch table."))
  }

  mask <- epochs[[ecol$session_id]] %in% sessions[[scol$id]]

  # If there are unmatched session_ids, set mask to FALSE
  mask[is.na(mask)] <- FALSE

  if (return_mask) {
    mask
  } else {
    epochs[mask, ]
  }
}

#' Filter sessions for nights within a night range
#'
#' @param sessions The sessions dataframe
#' @param from_night The start night of the range (inclusive) in YYYY-MM-DD format
#' @param to_night The end night of the range (inclusive) in YYYY-MM-DD format
#' @param return_mask If TRUE, returns a logical vector indicating which sessions meet the night range requirement
#' @details This function uses columns:
#' - `night`
#' @returns The sessions dataframe with only the sessions that fall within the specified night range, or a logical vector if `return_mask` is TRUE
#' @importFrom rlang .data
#' @export
#' @family filtering
#' @examples
#' filtered_sessions <- filter_by_night_range(example_sessions, "2025-04-07", "2025-04-10")
filter_by_night_range <- function(sessions, from_night, to_night, return_mask = FALSE) {
  check_session_colnames(sessions, "night")
  col <- get_session_colnames(sessions)

  from_night <- if (is.null(from_night)) min(sessions[[col$night]]) else from_night
  to_night <- if (is.null(to_night)) max(sessions[[col$night]]) else to_night

  if (from_night > to_night) {
    cli::cli_abort(c("!" = "from_night must be before to_night."))
  }

  mask <- (sessions[[col$night]] >= lubridate::as_date(from_night) &
             sessions[[col$night]] <= lubridate::as_date(to_night))

  mask[is.na(mask)] <- FALSE

  if (return_mask) {
    mask
  } else {
    sessions[mask, ]
  }
}

#' Filter sessions by age range
#'
#' @param sessions The sessions dataframe
#' @param min_age The minimum age of the subjects (inclusive)
#' @param max_age The maximum age of the subjects (inclusive)
#' @param return_mask If TRUE, returns a logical vector indicating which sessions belong to subjects within the specified age range
#' @details This function uses columns:
#' - `birth_year`
#' - `night`
#' @returns The sessions dataframe with only the sessions that belong to subjects within the specified age range,
#' or a logical vector if `return_mask` is TRUE
#' @importFrom rlang .data
#' @export
#' @family filtering
#' @examples
#' filtered_sessions <- filter_by_age_range(example_sessions_v1, min_age = 11, max_age = 18)
filter_by_age_range <- function(sessions, min_age = NULL, max_age = NULL, return_mask = FALSE) {
  check_session_colnames(sessions, c("birth_year", "night"))
  col <- get_session_colnames(sessions)

  min_age <- if (is.null(min_age)) min(lubridate::year(sessions[[col$night]]) - sessions[[col$birth_year]]) else min_age
  max_age <- if (is.null(max_age)) max(lubridate::year(sessions[[col$night]]) - sessions[[col$birth_year]]) else max_age

  if (min_age > max_age) {
    cli::cli_abort(c("!" = "min_age must be before max_age."))
  }

  mask <- (sessions[[col$birth_year]] >= (lubridate::year(sessions[[col$night]]) - max_age) &
             sessions[[col$birth_year]] <= (lubridate::year(sessions[[col$night]]) - min_age))

  mask[is.na(mask)] <- FALSE

  if (return_mask) {
    mask
  } else {
    sessions[mask, ]
  }
}

#' Filter by sex
#'
#' @param sessions The sessions dataframe
#' @param sex The sex to filter for (M, F, or NULL for both)
#' @param return_mask If TRUE, returns a logical vector indicating which sessions belong to the specified sex
#' @details This function uses columns:
#' - `sex`
#' @returns The sessions dataframe with only the sessions that belong to the specified sex, or a logical vector if `return_mask` is TRUE
#' @importFrom rlang .data
#' @export
#' @family filtering
#' @examples
#' filtered_sessions <- filter_by_sex(example_sessions_v1, "M")
filter_by_sex <- function(sessions, sex, return_mask = FALSE) {
  check_session_colnames(sessions, "sex")
  col <- get_session_colnames(sessions)

  mask <- sessions[[col$sex]] %in% sex

  mask[is.na(mask)] <- FALSE

  if (return_mask) {
    mask
  } else {
    sessions[mask, ]
  }
}

#' Select subjects by ID
#'
#' @param sessions The sessions dataframe
#' @param subject_ids The subject IDs to select
#' @details This function uses columns:
#' @param return_mask If TRUE, returns a logical vector indicating which sessions belong to the specified subjects
#' - `subject_id`
#' @returns The sessions dataframe with only the sessions that belong to the specified subjects, or a logical vector if `return_mask` is TRUE
#' @export
#' @family filtering
#' @seealso [select_devices()] to select sessions by device ID.
#' @examples
#' filtered_sessions <- select_subjects(example_sessions, c("sub_01JNDH3Z5NP0PSV82NFBGPV31X"))
select_subjects <- function(sessions, subject_ids, return_mask = FALSE) {
  check_session_colnames(sessions, "subject_id")
  col <- get_session_colnames(sessions)

  if (sum(sessions[[col$subject_id]] %in% subject_ids) == 0) {
    cli::cli_warn(c("!" = "None of the subject IDs were found in the sessions table.",
                    "i" = "Available subject IDs: {unique(sessions[[col$subject_id]])}",
                    "i" = "Returning an empty sessions table."))
  }

  mask <- sessions[[col$subject_id]] %in% subject_ids

  mask[is.na(mask)] <- FALSE

  if (return_mask) {
    mask
  } else {
    sessions[mask, ]
  }
}

#' Select devices by ID
#'
#' @param sessions The sessions dataframe
#' @param device_ids The device IDs to select
#' @param return_mask If TRUE, returns a logical vector indicating which sessions were recorded by the specified devices
#' @details This function uses columns:
#' - `device_id`
#' @returns The sessions dataframe with only the sessions recorded by the specified devices, or a logical vector if `return_mask` is TRUE
#' @export
#' @family filtering
#' @seealso [select_subjects()] to select sessions by subject ID.
#' @examples
#' filtered_sessions <- select_devices(example_sessions, c("VTGVSRTHCA"))
select_devices <- function(sessions, device_ids, return_mask = FALSE) {
  check_session_colnames(sessions, "device_id")
  col <- get_session_colnames(sessions)

  if (sum(sessions[[col$device_id]] %in% device_ids) == 0) {
    cli::cli_warn(c("!" = "None of the device IDs were found in the sessions table.",
                    "i" = "Available device IDs: {unique(sessions[[col$device_id]])}",
                    "i" = "Returning an empty sessions table."))
  }

  mask <- sessions[[col$device_id]] %in% device_ids

  mask[is.na(mask)] <- FALSE

  if (return_mask) {
    mask
  } else {
    sessions[mask, ]
  }
}

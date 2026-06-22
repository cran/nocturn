#' Calculate the mean time from a vector of time strings
#'
#' This function calculates the mean time from a vector of time strings in the format "YYYY-MM-DD HH:MM:SS".
#' @param time_vector A vector of time strings in format "YYYY-MM-DD HH:MM:SS", "HH:MM:SS" or "HH:MM".
#' @param unit The unit of time for the result. Can be "HH:MM" (default), "hour", "minute" or "second".
#' @returns A string representing the mean time in the format "HH:MM".
#' @export
#' @family time processing
#' @examples
#' # Use on a vector of time strings representing full dates
#' time_vector <- c("2025-04-08 23:00:00", "2025-04-09 01:00:00")
#' mean_time(time_vector)
#'
#' # Use on time-only strings
#' time_vector <- c("22:56", "01:32")
#' mean_time(time_vector)
#'
#' # Use on a dataframe column
#' mean_time(example_sessions$time_at_sleep_onset)
mean_time <- function(time_vector, unit = "HH:MM") {
  if (length(time_vector) == 0) {
    return(NA_character_)
  }

  time_vector <- time_vector |>
    parse_time() |>
    stats::na.omit() |>
    time_to_hours() |>
    circular::circular(units = "hours", modulo = "asis") |>
    circular::mean.circular() |>
    as.numeric() |>
    prod(3600) |>
    round() |>
    as.POSIXct(origin = "1970-01-01", tz = "UTC")

  switch(unit,
    "HH:MM" = time_vector |> format("%H:%M"),
    "hour" = time_to_hours(time_vector),
    "minute" = time_to_hours(time_vector) * 60,
    "second" = time_to_hours(time_vector) * 3600,
    cli::cli_abort(c("!" = "unit must be one of 'HH:MM', 'hour', 'minute', or 'second'.",
                     "x" = "You supplied {unit}."))
  )
}

#' Calculate the circular standard deviation of a vector of times
#'
#' This function calculates the standard deviation of a vector of time strings,
#' accounting for the circular nature of time (e.g., 23:59 is close to 00:00).
#' @param time_vector A vector of time strings in format "YYYY-MM-DD HH:MM:SS", "HH:MM:SS" or "HH:MM".
#' @param unit The unit of time for the result. Can be "second", "minute", or "hour". Default is "hour".
#' @returns A numeric value representing the standard deviation in the specified unit.
#' @export
#' @family time processing
#' @examples
#' sd_time(c("23:59", "00:01"))
sd_time <- function(time_vector, unit = "hour") {
  if (length(time_vector) == 0) {
    return(NA_real_)
  }
  times_in_hours <- time_vector |>
    parse_time() |>
    stats::na.omit() |>
    time_to_hours()
  circ <- circular::circular(times_in_hours, units = "hours", modulo = "asis")
  circ_sd <- circular::sd.circular(circ)
  switch(unit,
    hour = circ_sd,
    minute = circ_sd * 60,
    second = circ_sd * 3600,
    cli::cli_abort(c("!" = "unit must be one of 'second', 'minute', or 'hour'.",
                     "x" = "You supplied {unit}."))
  )
}

#' Calculate the minimum time from 12pm to 12pm
#'
#' This function calculates the minimum time from a vector of time strings in the format "YYYY-MM-DD HH:MM:SS".
#' It considers a time window from 12pm to 12pm the next day, so 11:00 is considered later than 13:00.
#' @param time_vector A vector of time strings in the format "YYYY-MM-DD HH:MM:SS".
#' @returns A string representing the minimum time in the format "HH:MM".
#' @export
#' @family time processing
#' @seealso [max_time()] to calculate the maximum time in the same format.
#' @examples
#' min_time(c("2025-04-08 23:00:00", "2025-04-09 01:00:00", "2025-04-09 02:30:00"))
min_time <- function(time_vector) {
  time_vector |>
    shift_times_by_12h() |>
    time_to_hours() |>
    min(na.rm = TRUE) |>
    (\(x) (x + 12) * 3600)() |>
    as.POSIXct(tz = "UTC") |>
    format("%H:%M")
}

#' Calculate the maximum time from 12pm to 12pm
#'
#' This function calculates the maximum time from a vector of time strings in the format "YYYY-MM-DD HH:MM:SS".
#' It considers a time window from 12pm to 12pm the next day, so 11:00 is considered later than 13:00.
#' @param time_vector A vector of time strings in the format "YYYY-MM-DD HH:MM:SS".
#' @returns A string representing the maximum time in the format "HH:MM".
#' @export
#' @family time processing
#' @seealso [min_time()] to calculate the minimum time in the same format.
#' @examples
#' max_time(c("2025-04-08 23:00:00", "2025-04-09 01:00:00", "2025-04-09 02:30:00"))
max_time <- function(time_vector) {
  time_vector |>
    shift_times_by_12h() |>
    time_to_hours() |>
    max(na.rm = TRUE) |>
    (\(x) (x + 12) * 3600)() |>
    as.POSIXct(tz = "UTC") |>
    format("%H:%M")
}

#' Compute the forward time difference from t1 to t2 (wrapping at 24)
#'
#' This function returns the time from t1 to t2, always moving forward on the clock.
#' For example, from 07:00 to 22:00 is 15 hours, from 22:00 to 07:00 is 9 hours.
#' @param t1 First time (character, POSIXct, or numeric hour)
#' @param t2 Second time (character, POSIXct, or numeric hour)
#' @param unit The unit of time. Can be "second", "minute", or "hour". Default is "hour".
#' @returns The forward difference in the specified unit (numeric, always positive, 0 <= x < 24)
#' @export
#' @family time processing
#' @examples
#' time_diff("07:00", "22:00") # 15
#' time_diff("22:00", "07:00") # 9
#' time_diff("07:00", "22:00", unit = "minute") # 540
time_diff <- function(t1, t2, unit = "hour") {
  h1 <- time_to_hours(t1)
  h2 <- time_to_hours(t2)
  switch(unit,
    second = (h2 - h1) %% 24 * 3600,
    minute = (h2 - h1) %% 24 * 60,
    hour = (h2 - h1) %% 24,
    cli::cli_abort(c("!" = "unit must be one of 'second', 'minute', or 'hour'.",
                     "x" = "You supplied {unit}."))
  )
}

#' Shift times to break at 12 pm
#'
#' This function shifts times so that the day starts at 12 PM.
#' This is useful for plotting night data
#' @param times A vector of times in POSIXct format, character convertible to POSIXct, or numerical (in hours).
#' @returns A vector of times in POSIXct format (or numerical if numerical provided as input) shifted to start at 12 PM
#' @export
#' @family time processing
#' @examples
#' # Shift a vector of times in HH:MM format
#' shift_times_by_12h(c("02:30", "16:00"))
#' #> "14:30" "04:00"
#'
#' # Shift times in YYYY-MM-DD HH:MM:SS format
#' shift_times_by_12h(c("2025-04-08 23:00:00", "2025-04-09 01:00:00"))
#' #> "2025-04-08 11:00" "2025-04-09 13:00"
#'
#' # Shift sessions start times to start at 12 PM
#' shifted_times <- shift_times_by_12h(example_sessions$session_start)
#'
#' # Use dplyr::mutate to dicrectly add the shifted times to a dataframe
#' epochs <- example_epochs |>
#'   dplyr::mutate(shifted_time = shift_times_by_12h(timestamp))
shift_times_by_12h <- function(times) {
  if (inherits(times, "numeric")) {
    return(dplyr::if_else(times < 12, times + 12, times - 12))
  }

  times <- parse_time(times)
  dplyr::if_else(lubridate::hour(times) < 12, times + lubridate::hours(12), times - lubridate::hours(12))
}

#' Create a grouping by night for epoch data
#'
#' The function creates a new column `night` that groups the epochs by night.
#' Timepoints before 12 PM are considered part of the previous night.
#'
#' @param epochs The epochs dataframe
#' @details This function uses columns:
#' - `timestamp`
#' @returns The epochs dataframe with the `night` column added
#' @importFrom rlang .data
#' @export
#' @seealso [group_sessions_by_night()] to group session data by night.
#' @family time processing
#' @examples
#' epochs <- group_epochs_by_night(example_epochs)
group_epochs_by_night <- function(epochs) {
  check_epoch_colnames(epochs, c("timestamp"))
  col <- get_epoch_colnames(epochs)
  epochs |>
    tidyr::drop_na(dplyr::all_of(col$timestamp)) |>
    dplyr::mutate(
      time_stamp = parse_time(.data[[col$timestamp]]),
      date = lubridate::as_date(.data$time_stamp),
      hour = time_to_hours(.data$time_stamp),
      night = lubridate::as_date(ifelse(.data$hour < 12, .data$date - 1, .data$date))
    ) |>
    dplyr::select(-"time_stamp", -"date", -"hour")
}

#' Create a grouping by night for session data
#'
#' The function creates a new column `night` that groups the sessions by night depending on their start time.
#' Sessions that start before 12 PM are considered part of the previous night.
#'
#' @param sessions The sessions dataframe
#' @details This function uses columns:
#' - `session_start`
#' @returns The sessions dataframe with the `night` column added
#' @export
#' @family time processing
#' @seealso [group_epochs_by_night()] to group epoch data by night.
#' @examples
#' sessions <- group_sessions_by_night(example_sessions)
group_sessions_by_night <- function(sessions) {
  check_session_colnames(sessions, c("session_start"))
  col <- get_session_colnames(sessions)
  sessions |>
    tidyr::drop_na(dplyr::all_of(col$session_start)) |>
    dplyr::mutate(
      start_time = parse_time(.data[[col$session_start]]),
      date = lubridate::as_date(.data$start_time),
      start_hour = time_to_hours(.data$start_time),
      night = lubridate::as_date(ifelse(.data$start_hour < 12, date - 1, date))
    ) |>
    dplyr::select(-"start_time", -"date", -"start_hour")
}

#' Calculate the number of time units in a day
#'
#' This function returns the number of specified time units (seconds, minutes or hours) in a day.
#' @param unit The unit of time. Can be "second", "minute", or "hour". Default is "second".
#' @returns The number of time units in a day (numeric)
#' @family internal
#' @export
#' @examples
#' get_time_per_day("hour")
get_time_per_day <- function(unit = "second") {
  switch(unit,
    second = 86400,
    minute = 1440,
    hour = 24,
    cli::cli_abort(c("!" = "unit must be one of 'second', 'minute', or 'hour'.",
                     "x" = "You supplied {unit}."))
  )
}

#' Check if a column contains ISO 8601 datetime strings
#'
#' This function checks if all non-missing values in a column are valid ISO 8601 datetime strings.
#' @param x A vector of character strings
#' @returns TRUE if all non-missing values are valid ISO 8601 datetime strings, FALSE otherwise
#' @family internal
#' @export
#' @examples
#' is_iso8601_datetime(c("2026-01-01T14:10"))
is_iso8601_datetime <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  } else {
    x <- stats::na.omit(x)
  }

  # If x is a POSIXct, return TRUE
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(TRUE)
  }

  # Date patterns
  date_extended <- "\\d{4}-\\d{2}-\\d{2}"  # YYYY-MM-DD
  date_basic <- "\\d{4}\\d{2}\\d{2}"       # YYYYMMDD

  # Time patterns
  time_extended <- "\\d{2}:\\d{2}:\\d{2}(?:\\.\\d+)?"  # HH:MM:SS(.sss)
  time_basic <- "\\d{2}\\d{2}\\d{2}(?:\\.\\d+)?"       # HHMMSS(.sss)

  # Timezone pattern
  timezone <- "(?:Z|[+-]\\d{2}(?::\\d{2}|\\d{2}))?"

  # Time separator
  separator <- "[ T]"

  # Combined patterns
  # Extended format: YYYY-MM-DDThh:mm:ss(.sss)(Z|±hh:mm)
  datetime_extended <- paste0(
    "^", date_extended, separator, time_extended, timezone, "$"
  )

  # Basic format: YYYYMMDDThhmmss(.sss)(Z|±hhmm)
  datetime_basic <- paste0(
    "^", date_basic, separator, time_basic, timezone, "$"
  )

  # Mix of extended date with basic time: YYYY-MM-DDThhmmss(.sss)(Z|±hhmm)
  datetime_mixed1 <- paste0(
    "^", date_extended, separator, time_basic, timezone, "$"
  )

  # Mix of basic date with extended time: YYYYMMDDThh:mm:ss(.sss)(Z|±hh:mm)
  datetime_mixed2 <- paste0(
    "^", date_basic, separator, time_extended, timezone, "$"
  )

  # For each element in the input vector
  result <- rep(NA, length(x))
  not_na <- !is.na(x)
  result[not_na] <-
    grepl(datetime_extended, x[not_na]) |
    grepl(datetime_basic, x[not_na]) |
    grepl(datetime_mixed1, x[not_na]) |
    grepl(datetime_mixed2, x[not_na])

  all(as.logical(result))
}

#' Convert time vector to numeric hours
#'
#' This function converts a vector of time strings or POSIXct objects to numeric hours.
#' @details See [parse_time()] for supported time formats.
#' @param time_vector A vector of time strings
#' @returns A numeric vector representing the time in hours
#' @family time processing
#' @export
#' @examples
#' time_to_hours(c("2026-10-12T14:20:09"))
time_to_hours <- function(time_vector) {
  if (inherits(time_vector, "numeric")) {
    return(time_vector)
  }
  time_vector <- parse_time(time_vector)
  lubridate::hour(time_vector) + lubridate::minute(time_vector) / 60
}

#' Parse a vector of time strings into POSIXct objects
#'
#' This function parses a vector of time strings into POSIXct objects.
#' Supported formats include "YYYY-MM-DD HH:MM:SS", "YYYY-MM-DD HH:MM", "HH:MM:SS", and "HH:MM".
#' Timezone information is ignored.
#' @param time_vector A vector of time strings
#' @returns A vector of POSIXct objects
#' @family time processing
#' @export
#' @examples
#' parse_time("2026-01-01T14:34:09")
parse_time <- function(time_vector) {
  if ((inherits(time_vector, "POSIXct") || inherits(time_vector, "POSIXt") || inherits(time_vector, "numeric"))) {
    return(time_vector)
  }
  time_vector <- gsub("(\\+|-)[0-9]{2}:[0-9]{2}$|Z$", "", time_vector) # Remove timezone information
  time_formats <- c("ymd_HMS", "ymd_HM", "ymd_HMSz", "HMS", "HM")
  lubridate::parse_date_time(time_vector, orders = time_formats, quiet = TRUE)
}

#' Parse a vector of date strings into Date objects
#'
#' This function parses a vector of date strings into Date objects.
#' All formats containing year, month, and day information are supported.
#' @param date_vector A vector of date strings
#' @returns A vector of Date objects
#' @family time processing
#' @export
#' @examples
#' parse_date("2026-01-02T14:33:09")
parse_date <- function(date_vector) {
  if (inherits(date_vector, "Date")) {
    return(date_vector)
  }
  parsed_date <- suppressWarnings(lubridate::as_date(date_vector))
  if (!all(is.na(parsed_date))) {
    return(parsed_date)
  }

  date_formats <- c("ymd", "Ymd", "dmy", "Y-m-d", "d/m/Y")
  lubridate::parse_date_time(date_vector, orders = date_formats, tz = NULL, quiet = TRUE) |>
    lubridate::as_date()
}

#' Update the date component of a POSIXct time object
#'
#' This function updates the date component of a POSIXct time object while preserving the time component.
#' @param time A POSIXct time object or a character string convertible to POSIXct
#' @param date A Date object or a character string convertible to Date
#' @returns A POSIXct time object with the updated date
#' @family time processing
#' @export
#' @examples
#' update_date("2026-01-01T14:09:09", "0000-01-01")
update_date <- function(time, date) {
  if (!inherits(time, "POSIXct")) {
    time <- parse_time(time)
  }
  if (!inherits(date, "Date")) {
    date <- lubridate::as_date(date)
  }

  if (length(date) == 1 && length(time) > 1) {
    date <- rep(date, length(time))
  }

  stats::update(
    time,
    year = lubridate::year(date),
    month = lubridate::month(date),
    day = lubridate::day(date)
  )
}

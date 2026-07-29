#' Calculate Interdaily Stability (IS)
#'
#' This function calculates the Interdaily Stability (IS) metric from a binary awake/asleep variable
#' @param epochs The epochs data frame
#' @details This function uses columns:
#' - `timestamp`
#' - `is_asleep`
#' @returns The Interdaily Stability (IS) value
#' @export
#' @family sleep metrics
#' @examples
#' interdaily_stability(example_epochs)
#' @importFrom rlang .data
interdaily_stability <- function(epochs) {
  check_epoch_colnames(epochs, c("timestamp", "is_asleep"))

  epochs <- epochs |>
    dplyr::mutate(tod = floor(time_to_hours(.data$timestamp)),
                  day = parse_date(.data$timestamp))

  mean_tod <- epochs |>
    dplyr::group_by(.data$tod) |>
    dplyr::summarise(mean_val = mean(.data$is_asleep, na.rm = TRUE), .groups = "drop")

  overall_mean <- mean(epochs$is_asleep, na.rm = TRUE)

  n_tod <- table(epochs$tod)
  numerator <- sum((mean_tod$mean_val - overall_mean)^2 * as.numeric(n_tod))
  denominator <- sum((epochs$is_asleep - overall_mean)^2, na.rm = TRUE)

  # IS
  numerator / denominator
}

#' Calculate Social Jet Lag
#'
#' This function calculates the Social Jet Lag (SJL) metric as the difference in mid-sleep times
#' between workdays and free days.
#' @param sessions The sessions data frame
#' @details This function uses columns:
#' - `time_at_midsleep`
#' - `is_workday`
#' @returns The Social Jet Lag (SJL) value in hours
#' @export
#' @family sleep metrics
#' @examples
#' social_jet_lag(example_sessions)
#' @importFrom rlang .data
social_jet_lag <- function(sessions) {
  check_session_colnames(sessions, c("time_at_midsleep", "is_workday"))

  sessions <- sessions |>
    dplyr::group_by(.data$is_workday) |>
    dplyr::summarise(
      mean_midsleep = mean_time(.data$time_at_midsleep, unit = "hour"), .groups = "drop"
    )

  sessions$mean_midsleep[!sessions$is_workday] -
    sessions$mean_midsleep[sessions$is_workday]
}

#' Calculate the Chronotype
#'
#' This function calculates the Chronotype metric based on the mid-sleep time
#' If sleep duration on free days is greater than on workdays, it applies a correction
#' as described in Roenneberg et al. (2019).
#' @param sessions The sessions data frame
#' @details This function uses columns:
#' - `time_at_midsleep`
#' - `sleep_period`
#' - `is_workday`
#' @returns The Chronotype value in hours
#' @export
#' @family sleep metrics
#' @examples
#' chronotype(example_sessions)
#' @importFrom rlang .data
chronotype <- function(sessions) {
  check_session_colnames(sessions, c("time_at_midsleep", "sleep_period", "is_workday"))

  data <- sessions |>
    remove_sessions_no_sleep() |>
    dplyr::group_by(.data$is_workday) |>
    dplyr::summarise(
      chronotype = mean_time(.data$time_at_midsleep, unit = "hour"),
      sleep_period = mean(.data$sleep_period / 3600, na.rm = TRUE),
      .groups = "drop"
    )

  if (nrow(data[!data$is_workday, ]) == 0 || nrow(data[data$is_workday, ]) == 0) {
    cli::cli_warn(c(
      "!" = "No free day sessions found or no workday sessions found",
      "i" = "Exiting."
    ))
    return(NA)
  }

  chronotype <- data[!data$is_workday, ]$chronotype
  sleep_period_free <- data[!data$is_workday, ]$sleep_period
  sleep_period_work <- data[data$is_workday, ]$sleep_period

  # If the sleep duration on free days is greater than on workdays, we apply a correction
  # (see Roenneberg et al., 2019)
  if (sleep_period_free <= sleep_period_work) {
    chronotype
  } else {
    sleep_duration_all <- sessions |>
      remove_sessions_no_sleep() |>
      dplyr::summarise(sleep_duration = mean(.data$sleep_period / 3600, na.rm = TRUE)) |>
      dplyr::pull(.data$sleep_duration)
    chronotype - (sleep_period_free - sleep_duration_all) / 2
  }
}

#' Calculate Composite Phase Deviation (CPD)
#'
#' This function calculates the Composite Phase Deviation (CPD) metric, used to measure
#' the regularity of the sleep patterns.
#' @param sessions The sessions data frame
#' @details This function uses columns:
#' - `time_at_midsleep`
#' - `is_workday`
#' - `night`
#' @returns The Composite Phase Deviation (CPD) value
#' @export
#' @family sleep metrics
#' @examples
#' composite_phase_deviation(example_sessions)
#' @importFrom rlang .data
composite_phase_deviation <- function(sessions) {
  check_session_colnames(sessions, c("time_at_midsleep", "is_workday", "night"))

  chronotype <- chronotype(sessions)

  sessions |>
    remove_sessions_no_sleep() |>
    dplyr::arrange(.data$night) |>
    dplyr::mutate(midsleep_h = time_to_hours(.data$time_at_midsleep),
                  mistiming = chronotype - .data$midsleep_h,
                  irregularity = .data$midsleep_h - dplyr::lag(.data$midsleep_h)) |>
    dplyr::summarise(cpd = mean(sqrt(.data$mistiming^2 + .data$irregularity^2), na.rm = TRUE)) |>
    dplyr::pull(.data$cpd)
}

#' Calculate the Sleep Regularity Index (SRI)
#'
#' The Sleep Regularity Index (SRI) is a measure of the regularity of sleep patterns.
#' It is calculated as the percentage of epochs where the sleep state remains the same after 24 hours.
#' @param epochs The epochs data frame
#' @details This function uses columns:
#' - `timestamp`
#' - `is_asleep`
#' @returns The Sleep Regularity Index (SRI) value
#' @export
#' @family sleep metrics
#' @examples
#' sleep_regularity_index(example_epochs)
#' @importFrom rlang .data
sleep_regularity_index <- function(epochs) {
  check_epoch_colnames(epochs, c("timestamp", "is_asleep"))

  epochs <- epochs |>
    dplyr::mutate(
      timestamp = floor(as.numeric(.data$timestamp))
    )

  p_same <- data.frame(timestamp = seq(min(epochs$timestamp), max(epochs$timestamp), by = 30)) |>
    dplyr::left_join(epochs, by = "timestamp") |>
    dplyr::mutate(is_asleep = ifelse(is.na(.data$is_asleep), 0, .data$is_asleep),
                  is_asleep_nextday = dplyr::lead(.data$is_asleep, n = 24 * 60 * 2)) |> # 24h = 2880 epochs of 30s
    dplyr::mutate(same_state = .data$is_asleep == .data$is_asleep_nextday) |>
    dplyr::summarise(p_same = mean(.data$same_state, na.rm = TRUE)) |>
    dplyr::pull(p_same)

  100 * (2 * p_same - 1)
}

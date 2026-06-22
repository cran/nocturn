#' Plot epoch time series data for a given variable
#'
#' @param epochs The epochs dataframe
#' @param variable The variable to plot (e.g., "temperature_ambient_mean")
#' @param exclude_zero Logical, whether to exclude zero values from the plot (default: FALSE)
#' @param color_by The variable to color the points by. Can be "default" or any other column name in the epochs dataframe.
#' @details This function uses columns:
#' - `timestamp`
#' - `night`
#' @returns A ggplot object
#' @importFrom rlang .data
#' @export
#' @family plot epochs
#' @seealso [plot_timeseries_sessions()] to plot session data.
#' @examples
#' plot_timeseries(example_epochs, variable="signal_quality_mean")
plot_timeseries <- function(epochs, variable, color_by = "default", exclude_zero = FALSE) {
  check_epoch_colnames(epochs, c("timestamp", "night"))
  col <- get_epoch_colnames(epochs)

  color_by <- if (color_by %in% colnames(epochs)) color_by else "night"

  if (exclude_zero) {
    epochs <- epochs |>
      dplyr::filter(.data[[variable]] != 0)
  }

  if (is_iso8601_datetime(epochs[[variable]])) {
    epochs$variable <- parse_time(epochs[[variable]]) |> update_date(date = "1970-01-01")
    y_is_time <- TRUE
  } else {
    epochs$variable <- epochs[[variable]]
    y_is_time <- FALSE
  }

  if (color_by != "default" && color_by %in% names(epochs)) {
    color_var <- epochs[[color_by]]
    if (is_iso8601_datetime(color_var)) {
      color_var <- parse_time(color_var) |> update_date(date = "1970-01-01")
      color_aes <- ggplot2::aes(
        x = time_to_hours(shift_times_by_12h(.data[[col$timestamp]])),
        y = .data$variable,
        color = color_var,
        group = .data[[col$night]]
      )
    } else if (is.numeric(color_var)) {
      color_aes <- ggplot2::aes(
        x = time_to_hours(shift_times_by_12h(.data[[col$timestamp]])),
        y = .data$variable,
        color = color_var,
        group = .data[[col$night]]
      )
    } else {
      epochs$color_group <- as.factor(color_var)
      color_aes <- ggplot2::aes(
        x = time_to_hours(shift_times_by_12h(.data[[col$timestamp]])),
        y = .data$variable,
        color = .data$color_group,
        group = .data[[col$night]]
      )
    }
    color_scale <- get_color_scale(color_var)
  } else {
    color_aes <- ggplot2::aes(
      x = time_to_hours(shift_times_by_12h(.data[[col$timestamp]])),
      y = .data$variable,
      group = .data[[col$night]]
    )
    color_scale <- ggplot2::scale_color_manual(values = "black", guide = "none")
  }

  p <- ggplot2::ggplot(epochs, color_aes) +
    ggplot2::geom_point() +
    color_scale +
    ggplot2::labs(
      x = "Time",
      y = variable,
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 24),
      breaks = seq(0, 24, by = 2),
      labels = function(x) sprintf("%02d:00", (x + 12) %% 24)
    )

  if (y_is_time) {
    p <- p + ggplot2::scale_y_datetime(
      labels = function(x) format(x, "%H:%M")
    )
  }

  p
}

#' Plot session time series data for a given variable
#'
#' @param sessions The sessions dataframe
#' @param variable The variable to plot (e.g., "time_at_sleep_onset")
#' @param exclude_zero Logical, whether to exclude zero values from the plot (default: FALSE)
#' @details This function uses columns:
#' - `night`
#' @param color_by The variable to color the points by. Can be "default" or any other column name in the sessions dataframe.
#' @returns A ggplot object
#' @export
#' @family plot sessions
#' @seealso [plot_timeseries()] to plot epoch data.
#' @examples
#' plot_timeseries_sessions(example_sessions, variable="time_at_midsleep")
plot_timeseries_sessions <- function(sessions, variable, color_by = "default", exclude_zero = FALSE) {
  check_session_colnames(sessions, c("night"))
  col <- get_session_colnames(sessions)

  if (exclude_zero) {
    sessions <- sessions |>
      dplyr::filter(.data[[variable]] != 0)
  }

  if (is_iso8601_datetime(sessions[[variable]])) {
    sessions$variable <- parse_time(sessions[[variable]]) |> update_date(date = "1970-01-01")
    y_is_time <- TRUE
  } else {
    sessions$variable <- sessions[[variable]]
    y_is_time <- FALSE
  }

  if (color_by != "default" && color_by %in% names(sessions)) {
    color_var <- sessions[[color_by]]
    if (is_iso8601_datetime(color_var)) {
      color_var <- parse_time(color_var) |> update_date(date = "1970-01-01")
      color_aes <- ggplot2::aes(x = .data[[col$night]], y = .data$variable, color = color_var)
    } else if (is.numeric(color_var)) {
      color_aes <- ggplot2::aes(x = .data[[col$night]], y = .data$variable, color = color_var)
    } else {
      sessions$color_group <- as.factor(color_var)
      color_aes <- ggplot2::aes(x = .data[[col$night]], y = .data$variable, color = .data$color_group)
    }
    color_scale <- get_color_scale(color_var)
  } else {
    color_aes <- ggplot2::aes(x = .data[[col$night]], y = .data$variable)
    color_scale <- ggplot2::scale_color_manual(values = "black", guide = "none")
  }

  p <- ggplot2::ggplot(sessions, color_aes) +
    ggplot2::geom_point(size = 5) +
    color_scale +
    ggplot2::labs(
      x = NULL,
      y = variable,
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  if (y_is_time) {
    p <- p + ggplot2::scale_y_datetime(
      labels = function(x) format(x, "%H:%M")
    )
  }
  p
}

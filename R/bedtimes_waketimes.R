#' Plot bedtimes and waketimes
#'
#' @param sessions The sessions dataframe
#' @param groupby The grouping variable for the plot. Can be "night", "workday", or "weekday".
#' @param color_by The variable to color the bars by. Can be "default" or any other column name in the sessions dataframe.
#' Note that if color_by is anything else than "default", groupby will be set to "night".
#' @details This function uses columns:
#' - `night`
#' - `time_at_sleep_onset`
#' - `time_at_wakeup`
#' - `is_workday`
#' @returns A ggplot graph showing the bedtimes and waketimes
#' @importFrom rlang .data
#' @export
#' @examples
#' plot_bedtimes_waketimes(example_sessions)
plot_bedtimes_waketimes <- function(sessions, groupby = "night", color_by = "default") {
  check_session_colnames(sessions, c("night", "time_at_sleep_onset", "time_at_wakeup", "is_workday"))
  if (color_by != "default") {
    groupby <- "night"
  }

  col <- get_session_colnames(sessions)

  expansion_factor <- switch(
    groupby,
    workday = 1,
    0.05
  )

  Sys.setlocale("LC_TIME", "C") # Ensure weekday names are in English

  plot_data <- sessions |>
    dplyr::filter(!is.na(.data[[col$time_at_sleep_onset]]) & !is.na(.data[[col$time_at_wakeup]])) |>
    dplyr::mutate(
      time_at_sleep_onset = parse_time(.data[[col$time_at_sleep_onset]]),
      time_at_wakeup = parse_time(.data[[col$time_at_wakeup]])
    ) |>
    dplyr::mutate(
      group = switch(
        groupby,
        night = .data[[col$night]],
        workday = factor(
          ifelse(.data[[col$is_workday]], "Weekday", "Weekend"),
          levels = c("Weekend", "Weekday")
        ),
        weekday = factor(
          lubridate::as_date(.data[[col$night]]) |> weekdays(),
          levels = c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday")
        ),
        .data[[col$night]]
      )
    ) |>
    dplyr::group_by(.data$group) |>
    dplyr::filter(!all(is.na(.data$time_at_sleep_onset)) & !all(is.na(.data$time_at_wakeup))) |>
    dplyr::summarise(
      sleep_start_labels = mean_time(.data$time_at_sleep_onset),
      sleep_end_labels = mean_time(.data$time_at_wakeup),
      color_group = if (color_by != "default" && color_by %in% names(sessions)) unique(.data[[color_by]]) else NA_character_
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      sleep_start = time_to_hours(shift_times_by_12h(.data$sleep_start_labels)),
      sleep_end = time_to_hours(shift_times_by_12h(.data$sleep_end_labels)),
      sleep_start_labels = parse_time(.data$sleep_start_labels),
      sleep_end_labels = parse_time(.data$sleep_end_labels),
      group_numeric = as.numeric(factor(.data$group))
    )

  if (color_by != "default" && color_by %in% names(sessions)) {
    color_var <- plot_data$color_group
    if (is_iso8601_datetime(color_var)) {
      color_var <- parse_time(color_var) |> update_date(date = "1970-01-01")
      plot_data$color_group <- color_var
    } else if (is.numeric(color_var)) {
      plot_data$color_group <- color_var
    } else {
      plot_data$color_group <- as.factor(color_var)
    }
    color_scale <- get_fill_scale(color_var)
    legend_show <- TRUE
  } else {
    plot_data$color_group <- "blue"
    color_scale <- ggplot2::scale_fill_manual(values = c("blue"), guide = "none")
    legend_show <- FALSE
  }

  ggplot2::ggplot(plot_data) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = .data$sleep_start,
        xmax = .data$sleep_end,
        ymin = .data$group_numeric - 0.4,
        ymax = .data$group_numeric + 0.4,
        fill = .data$color_group
      ),
      alpha = 0.6,
      show.legend = legend_show
    ) +
    color_scale +
    ggplot2::geom_text(
      ggplot2::aes(
        x = .data$sleep_start,
        y = .data$group_numeric,
        label = format(.data$sleep_start_labels, "%H:%M")
      ),
      hjust = 1.1,
      size = 6,
      color = "black"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = .data$sleep_end,
        y = .data$group_numeric,
        label = format(.data$sleep_end_labels, "%H:%M")
      ),
      hjust = -0.1,
      size = 6,
      color = "black"
    ) +
    ggplot2::scale_y_continuous(
      breaks = unique(plot_data$group_numeric),
      labels = unique(plot_data$group),
      expand = ggplot2::expansion(mult = c(expansion_factor, 0.05))
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.15, 0.15))
    ) +
    ggplot2::labs(
      title = "Average Sleep onset and Wakeup times",
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 24),
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 16),
      legend.text = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 16),
    )
}

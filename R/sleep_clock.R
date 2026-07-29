#' Plot Sleep Clock
#'
#' @param sessions The sessions dataframe
#' @param color_by The variable to color the segments by. Can be "default" or any other column name in the sessions dataframe.
#' @details This function uses columns:
#' - `time_at_sleep_onset`
#' - `time_at_wakeup`
#' - `night`
#' @returns A ggplot object showing the sleep clock
#' @importFrom rlang .data
#' @export
#' @family plot sessions
#' @examples
#' plot_sleep_clock(example_sessions)
plot_sleep_clock <- function(sessions, color_by = "default") {
  check_session_colnames(sessions, c("time_at_sleep_onset", "time_at_wakeup", "night"))

  sessions$session_idx <- seq_len(nrow(sessions))

  sessions <- sessions |>
    dplyr::filter(!is.na(.data$time_at_sleep_onset) & !is.na(.data$time_at_wakeup)) |>
    dplyr::mutate(
      sleep_onset_hour = time_to_hours(.data$time_at_sleep_onset),
      wakeup_hour = time_to_hours(.data$time_at_wakeup),
      night = as.factor(.data$night)
    )

  sleep_onset_data <- sessions |>
    dplyr::select("sleep_onset_hour", "night", "session_idx") |>
    dplyr::mutate(type = "Sleep Onset") |>
    dplyr::rename(hour = "sleep_onset_hour")

  wakeup_data <- sessions |>
    dplyr::select("wakeup_hour", "night", "session_idx") |>
    dplyr::mutate(type = "Wakeup") |>
    dplyr::rename(hour = "wakeup_hour")

  plot_data <- dplyr::bind_rows(
    sleep_onset_data,
    wakeup_data
  ) |>
    dplyr::mutate(
      radial_distance = as.numeric(.data$night) / max(as.numeric(.data$night))
    )

  curve_data <- sessions |>
    dplyr::mutate(
      sleep_onset_radial = as.numeric(as.factor(.data$night)) / max(as.numeric(as.factor(.data$night))),
      wakeup_radial = as.numeric(as.factor(.data$night)) / max(as.numeric(as.factor(.data$night)))
    ) |>
    dplyr::select("sleep_onset_hour", "wakeup_hour", "sleep_onset_radial", "wakeup_radial", "night", "session_idx") |>
    dplyr::rowwise() |>
    dplyr::do({
      if (.data$wakeup_hour < .data$sleep_onset_hour) {
        data.frame(
          x = c(.data$sleep_onset_hour, 0),
          y = c(.data$sleep_onset_radial, .data$wakeup_radial),
          xend = c(24, .data$wakeup_hour),
          yend = c(.data$sleep_onset_radial, .data$wakeup_radial),
          night = .data$night,
          session_idx = .data$session_idx
        )
      } else {
        data.frame(
          x = .data$sleep_onset_hour,
          y = .data$sleep_onset_radial,
          xend = .data$wakeup_hour,
          yend = .data$wakeup_radial,
          night = .data$night,
          session_idx = .data$session_idx
        )
      }
    })

  circle_outline <- data.frame(
    hour = seq(0, 24, length.out = 100),
    y = 1
  )

  if (color_by != "default" && color_by %in% names(sessions)) {
    color_var <- sessions[[color_by]]
    if (is_iso8601_datetime(color_var)) {
      color_var <- parse_time(color_var) |> update_date(date = "1970-01-01")
      plot_data$color_group <- color_var[plot_data$session_idx]
      curve_data$color_group <- color_var[curve_data$session_idx]
    } else if (is.numeric(color_var)) {
      plot_data$color_group <- color_var[plot_data$session_idx]
      curve_data$color_group <- color_var[curve_data$session_idx]
    } else {
      plot_data$color_group <- as.factor(color_var)[plot_data$session_idx]
      curve_data$color_group <- as.factor(color_var)[curve_data$session_idx]
    }
    color_scale <- get_color_scale(color_var)

    # Custom color_by: color both curves and points by color_group
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$hour, y = .data$radial_distance)) +
      ggplot2::geom_path(
        data = circle_outline,
        ggplot2::aes(x = .data$hour, y = .data$y),
        inherit.aes = FALSE,
        color = "grey",
        linewidth = 0.5
      ) +
      ggplot2::geom_segment(
        data = curve_data,
        ggplot2::aes(
          x = .data$x,
          y = .data$y,
          xend = .data$xend,
          yend = .data$yend,
          color = .data$color_group
        ),
        linewidth = 0.8,
        show.legend = TRUE
      ) +
      ggplot2::geom_point(
        data = plot_data,
        ggplot2::aes(color = .data$color_group),
        size = 3,
        alpha = 0.8
      ) +
      color_scale
  } else {
    # Default: curved lines by night (viridis), points by type (purple/orange)
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$hour, y = .data$radial_distance)) +
      ggplot2::geom_path(
        data = circle_outline,
        ggplot2::aes(x = .data$hour, y = .data$y),
        inherit.aes = FALSE,
        color = "grey",
        linewidth = 0.5
      ) +
      ggplot2::geom_segment(
        data = curve_data,
        ggplot2::aes(
          x = .data$x,
          y = .data$y,
          xend = .data$xend,
          yend = .data$yend,
          color = .data$night
        ),
        linewidth = 0.8,
        show.legend = FALSE
      ) +
      ggplot2::scale_color_manual(
        values = stats::setNames(
          grDevices::hcl.colors(length(unique(curve_data$night)), "viridis"),
          unique(curve_data$night)
        )
      ) +
      ggnewscale::new_scale_color() +
      ggplot2::geom_point(
        data = plot_data |> dplyr::filter(.data$type == "Sleep Onset"),
        ggplot2::aes(color = .data$type),
        size = 3,
        alpha = 0.8
      ) +
      ggplot2::geom_point(
        data = plot_data |> dplyr::filter(.data$type == "Wakeup"),
        ggplot2::aes(color = .data$type),
        size = 3,
        alpha = 0.8
      ) +
      ggplot2::scale_color_manual(
        values = c("Sleep Onset" = "#8e44ad", "Wakeup" = "#e67e22")
      )
  }

  # Common plot elements to default and custom color_by
  p <- p +
    ggplot2::scale_x_continuous(
      limits = c(0, 24),
      breaks = seq(0, 23, by = 1),
      labels = (seq(0, 23, by = 1)) %% 24
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::coord_polar(theta = "x", start = 0) +
    ggplot2::labs(
      title = NULL,
      x = NULL,
      y = NULL,
      color = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 16),
      legend.position = "right"
    )
  p
}

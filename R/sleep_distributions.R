utils::globalVariables(".")

#' Plot boxplots for sleep onset, midsleep, and wakeup times
#'
#' @param sessions The sessions dataframe
#' @param circular Whether to output a circular plot (default FALSE)
#' @details This function uses columns:
#' - `time_at_sleep_onset`
#' - `time_at_wakeup`
#' - `time_at_midsleep`
#' @returns A ggplot object with three horizontal boxplots (onset, midsleep, wakeup)
#' @export
#' @importFrom rlang .data
#' @examples
#' sleeptimes_boxplot(example_sessions)
sleeptimes_boxplot <- function(sessions, circular = FALSE) {
  check_session_colnames(sessions, c("time_at_sleep_onset", "time_at_wakeup", "time_at_midsleep"))
  plot_data <- prepare_sleeptimes_data(sessions)
  box_colors <- c("Sleep Onset" = "purple", "Midsleep" = "cornflowerblue", "Wakeup" = "orange")

  stats <- plot_data |>
    dplyr::group_by(.data$variable) |>
    dplyr::summarise(
      circ_stats = list(compute_circular_stats(.data$hour)),
      .groups = "drop"
    ) |>
    tidyr::unnest_wider("circ_stats")

  # Split IQR if it crosses midnight
  stats_split <- stats |>
    dplyr::mutate(
      box_split = .data$q1 > .data$q3
    ) |>
    tidyr::unnest_longer(c("q1", "q3")) |>
    dplyr::group_by(.data$variable) |>
    dplyr::do({
      row <- .
      if (row$q1 > row$q3) {
        # Two boxes: q1 to 24, 0 to q3
        dplyr::bind_rows(
          dplyr::mutate(row, xlower = row$q1, xupper = 24),
          dplyr::mutate(row, xlower = 0, xupper = row$q3)
        )
      } else {
        dplyr::mutate(row, xlower = row$q1, xupper = row$q3)
      }
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      y = as.numeric(factor(.data$variable, levels = c("Wakeup", "Midsleep", "Sleep Onset")))
    )

  p <- ggplot2::ggplot(stats_split) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = .data$xlower, xmax = .data$xupper,
        ymin = .data$y - 0.3, ymax = .data$y + 0.3,
        fill = .data$variable
      ),
      color = NA, alpha = 0.4
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$median, xend = .data$median,
        y = .data$y - 0.3, yend = .data$y + 0.3,
        color = .data$variable
      ),
      linewidth = 1.2
    ) +
    ggplot2::scale_fill_manual(values = box_colors, guide = "none") +
    ggplot2::scale_color_manual(values = box_colors, guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 24, by = 2),
      labels = function(x) sprintf("%02d:00", (x + 12) %% 24)
    ) +
    ggplot2::scale_y_continuous(
      breaks = 1:3,
      labels = c("Wakeup", "Midsleep", "Sleep Onset")
    ) +
    ggplot2::labs(
      title = NULL,
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 18),
      axis.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 16),
      aspect.ratio = 1 / 4
    )

  if (circular) {
    p +
      ggplot2::coord_polar(theta = "x", start = pi) +
      ggplot2::theme(aspect.ratio = 1) +
      ggplot2::scale_y_continuous(
        breaks = 1:3,
        labels = NULL
      ) +
      ggplot2::scale_x_continuous(
        limits = c(0, 24),
        breaks = seq(0, 23, by = 1),
        labels = (seq(0, 23, by = 1) + 12) %% 24
      )
  } else {
    p
  }
}

#' Plot histograms for sleep onset, midsleep, and wakeup times
#'
#' @param sessions The sessions dataframe
#' @param binwidth The width of the bins for the histogram (default 0.25)
#' @param circular Whether to output a circular plot (default FALSE)
#' @details This function uses columns:
#' - `time_at_sleep_onset`
#' - `time_at_wakeup`
#' - `time_at_midsleep`
#' @returns A ggplot object with three overlaid histograms (sleep onset, midsleep, wakeup)
#' @export
#' @importFrom rlang .data
#' @examples
#' sleeptimes_histogram(example_sessions)
sleeptimes_histogram <- function(sessions, binwidth = 0.25, circular = FALSE) {
  check_session_colnames(sessions, c("time_at_sleep_onset", "time_at_wakeup", "time_at_midsleep"))
  plot_data <- prepare_sleeptimes_data(sessions)
  box_colors <- c("Sleep Onset" = "purple", "Midsleep" = "cornflowerblue", "Wakeup" = "orange")

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$hour, color = .data$variable)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(.data$count), fill = .data$variable),
      binwidth = binwidth,
      position = "identity",
      alpha = 0.3,
      linewidth = 0.5
    ) +
    ggplot2::scale_color_manual(values = box_colors, limits = names(box_colors), guide = "none") +
    ggplot2::scale_fill_manual(values = box_colors, limits = names(box_colors)) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 24, by = 2),
      labels = function(x) sprintf("%02d:00", (x + 12) %% 24),
      expand = ggplot2::expansion(mult = c(0.08, 0.08))
    ) +
    ggplot2::labs(
      title = NULL,
      x = NULL,
      y = "Count",
      fill = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 18),
      axis.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 16),
      legend.text = ggplot2::element_text(size = 14),
      legend.position = "right",
      aspect.ratio = 1 / 4
    )

  if (circular) {
    p +
      ggplot2::coord_polar(theta = "x", start = pi) +
      ggplot2::theme(aspect.ratio = 1) +
      ggplot2::scale_y_continuous(
        labels = NULL
      ) +
      ggplot2::scale_x_continuous(
        limits = c(0, 24),
        breaks = seq(0, 23, by = 1),
        labels = (seq(0, 23, by = 1) + 12) %% 24
      ) +
      ggplot2::labs(
        y = NULL
      )
  } else {
    p
  }
}

#' Plot density curves for sleep onset, midsleep, and wakeup times with a dashed line showing the median
#'
#' @param sessions The sessions dataframe
#' @param adjust The bandwidth adjustment for the density estimate (default 1)
#' @param circular Whether to output a circular plot (default FALSE)
#' @details This function uses columns:
#' - `time_at_sleep_onset`
#' - `time_at_wakeup`
#' - `time_at_midsleep`
#' @returns A ggplot object with three overlaid density curves (sleep onset, midsleep, wakeup)
#' @export
#' @importFrom rlang .data
#' @examples
#' sleeptimes_density(example_sessions)
sleeptimes_density <- function(sessions, adjust = 1, circular = FALSE) {
  check_session_colnames(sessions, c("time_at_sleep_onset", "time_at_wakeup", "time_at_midsleep"))
  plot_data <- prepare_sleeptimes_data(sessions)
  box_colors <- c("Sleep Onset" = "purple", "Midsleep" = "cornflowerblue", "Wakeup" = "orange")

  medians <- plot_data |>
    dplyr::group_by(.data$variable) |>
    dplyr::summarise(
      median_hour = compute_circular_stats(.data$hour)["median"],
      .groups = "drop"
    )

  density_data <- plot_data |>
    dplyr::group_by(.data$variable) |>
    dplyr::group_modify(~{
      radians <- .x$hour / 24 * 2 * pi
      circ <- circular::circular(radians, units = "radians", modulo = "2pi")
      dens <- circular::density.circular(
        circ,
        bw = 24,
        adjust = adjust,
        kernel = "vonmises",
        n = 512,
        control.circular = list(units = "radians")
      )
      tibble::tibble(
        hour = as.numeric(dens$x) * 24 / (2 * pi),
        density = dens$y
      )
    }) |>
    dplyr::ungroup()

  median_lines <- density_data |>
    dplyr::left_join(medians, by = "variable") |>
    dplyr::group_by(.data$variable) |>
    dplyr::filter(abs(.data$hour - .data$median_hour) == min(abs(.data$hour - .data$median_hour))) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select("variable", "median_hour", density_at_median = "density")

  p <- ggplot2::ggplot(density_data, ggplot2::aes(x = .data$hour, y = .data$density, color = .data$variable, fill = .data$variable)) +
    ggplot2::geom_area(alpha = 0.3, position = "identity") +
    ggplot2::geom_segment(
      data = median_lines,
      ggplot2::aes(
        x = .data$median_hour, xend = .data$median_hour,
        y = 0, yend = .data$density_at_median,
        color = .data$variable
      ),
      linetype = "dashed",
      linewidth = 0.7,
      alpha = 0.7,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_color_manual(values = box_colors, limits = names(box_colors), guide = "none") +
    ggplot2::scale_fill_manual(values = box_colors, limits = names(box_colors)) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 24, by = 2),
      labels = function(x) sprintf("%02d:00", (x + 12) %% 24),
      expand = ggplot2::expansion(mult = c(0.08, 0.08))
    ) +
    ggplot2::labs(
      title = NULL,
      x = NULL,
      y = "Density",
      fill = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 18),
      axis.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 16),
      legend.text = ggplot2::element_text(size = 14),
      legend.position = "right",
      aspect.ratio = 1 / 4
    )

  if (circular) {
    p +
      ggplot2::coord_polar(theta = "x", start = pi) +
      ggplot2::theme(aspect.ratio = 1) +
      ggplot2::scale_y_continuous(
        labels = NULL
      ) +
      ggplot2::scale_x_continuous(
        limits = c(0, 24),
        breaks = seq(0, 23, by = 1),
        labels = (seq(0, 23, by = 1) + 12) %% 24
      ) +
      ggplot2::labs(
        y = NULL
      )
  } else {
    p
  }
}

#' @importFrom rlang .data
prepare_sleeptimes_data <- function(sessions) {
  sessions <- remove_sessions_no_sleep(sessions)
  plot_data <- tidyr::pivot_longer(
    sessions,
    cols = dplyr::all_of(c("time_at_sleep_onset", "time_at_midsleep", "time_at_wakeup")),
    names_to = "variable",
    values_to = "time"
  ) |>
    dplyr::mutate(
      time = parse_time(.data$time),
      variable = dplyr::case_when(
        .data$variable == "time_at_sleep_onset" ~ "Sleep Onset",
        .data$variable == "time_at_midsleep" ~ "Midsleep",
        .data$variable == "time_at_wakeup" ~ "Wakeup"
      ),
      hour = time_to_hours(shift_times_by_12h(.data$time))
    )
  plot_data$variable <- factor(plot_data$variable, levels = c("Wakeup", "Midsleep", "Sleep Onset"))
  plot_data
}

compute_circular_stats <- function(hours) {
  radians <- hours / 24 * 2 * pi
  circ <- circular::circular(radians, units = "radians", modulo = "2pi")
  median_rad <- circular::median.circular(circ)
  q1_rad <- circular::quantile.circular(circ, probs = 0.25)
  q3_rad <- circular::quantile.circular(circ, probs = 0.75)
  # Convert stats back to hours
  c(
    median = as.numeric(median_rad) * 24 / (2 * pi),
    q1 = as.numeric(q1_rad) * 24 / (2 * pi),
    q3 = as.numeric(q3_rad) * 24 / (2 * pi)
  )
}

circular_distance <- function(a, b) abs(atan2(sin(a - b), cos(a - b)))

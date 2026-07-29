#' Plot Hypnogram
#'
#' @param epochs The epochs dataframe
#' @details This function uses columns:
#' - `timestamp`
#' - `sleep_stage`
#' @returns A ggplot object showing the hypnogram as bars
#' @importFrom rlang .data
#' @export
#' @family plot epochs
#' @examples
#' plot_hypnogram(example_epochs)
plot_hypnogram <- function(epochs) {
  check_epoch_colnames(epochs, c("timestamp", "sleep_stage"))

  hypnogram_data <- epochs |>
    dplyr::mutate(
      sleep_stage_numeric = unclass(factor(.data$sleep_stage)) + 1,
      sleep_stage = factor(
        .data$sleep_stage,
        levels = sort(unique(.data$sleep_stage))
      )
    ) |>
    dplyr::group_by(timestamp = lubridate::floor_date(.data$timestamp, unit = "minute")) |>
    dplyr::summarize(
      sleep_stage_numeric = dplyr::first(.data$sleep_stage_numeric),
      sleep_stage = dplyr::first(.data$sleep_stage),
      .groups = "drop"
    )

  custom_colors <- c(
    "#8dd3c7",  # Light teal (Set3)
    "#ffffb3",  # Light yellow (Set3)
    "#bebada",  # Light purple (Set3)
    "#fb8072",  # Light coral (Set3)
    "#80b1d3",  # Light blue (Set3)
    "#fdb462",  # Light orange (Set3)
    "#b3de69",  # Light green (Set3)
    "#fccde5",  # Light pink (Set3)
    "#d9d9d9",  # Light gray (Set3)
    "#bc80bd",  # Medium purple (Set3)
    "#1f77b4",  # Blue (tab20)
    "#ff7f0e",  # Orange (tab20)
    "#2ca02c",  # Green (tab20)
    "#d62728",  # Red (tab20)
    "#9467bd",  # Purple (tab20)
    "#8c564b",  # Brown (tab20)
    "#e377c2",  # Pink (tab20)
    "#7f7f7f",  # Gray (tab20)
    "#bcbd22",  # Yellow-green (tab20)
    "#17becf"   # Cyan (tab20)
  )

  ggplot2::ggplot(hypnogram_data) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = .data$timestamp,
        xmax = .data$timestamp + lubridate::minutes(1),
        ymin = 0,
        ymax = .data$sleep_stage_numeric,
        fill = .data$sleep_stage
      )
    ) +
    ggplot2::scale_x_datetime(
      date_labels = "%Y-%m-%d\n%H:%M",
    ) +
    # ggplot2::scale_y_continuous(
    #   breaks = sleep_stage_numeric,
    #   labels = names(sleep_stage_numeric),
    #   limits = c(0, max(sleep_stage_numeric) + 2)
    # ) +
    ggplot2::scale_fill_manual(values = custom_colors) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = NULL,
      title = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 14),
      legend.position = "bottom"
    )
}

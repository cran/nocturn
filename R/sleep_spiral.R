#' Plot Sleep Spiral
#'
#' @param epochs The epochs dataframe
#' @param color_by The variable to color the spiral by. Can be "default" or any other column name in the epochs dataframe.
#' @details This function uses columns:
#' - `timestamp`
#' - `is_asleep`
#' @returns A ggplot object showing the sleep spiral
#' @importFrom rlang .data
#' @export
#' @family plot epochs
#' @examples
#' plot_sleep_spiral(example_epochs)
plot_sleep_spiral <- function(epochs, color_by = "default") {
  check_epoch_colnames(epochs, c("timestamp", "is_asleep"))
  col <- get_epoch_colnames(epochs)

  epochs <- epochs[seq(1, nrow(epochs), by = 5), ] # Downsampling to 5 min intervals to speed up plotting

  reference_time <- epochs[[col$timestamp]] |>
    parse_time() |>
    min() |>
    lubridate::floor_date(unit = "day")

  epochs <- epochs |>
    dplyr::mutate(
      timestamp = parse_time(.data[[col$timestamp]]),
      is_asleep = as.character(.data[[col$is_asleep]])
    ) |>
    tidyr::complete(
      timestamp = seq(min(.data$timestamp), max(.data$timestamp), by = "2 min"),
      fill = list(is_asleep = "0")
    ) |>
    dplyr::mutate(
      time_in_days = as.numeric(difftime(.data$timestamp, reference_time, units = "days")),
      time_in_min = as.numeric(difftime(.data$timestamp, reference_time, units = "mins")),
      angle = (.data$time_in_days %% 1) * 2 * pi,
      radius = .data$time_in_min
    )

  # Set up color mappings
  if (color_by != "default" && color_by %in% names(epochs)) {
    color_var <- epochs[[color_by]]
    if (is_iso8601_datetime(color_var)) {
      color_var <- parse_time(color_var)
      color_aes <- ggplot2::aes(color = color_var)
      color_scale <- ggplot2::scale_color_gradientn(
        colours = grDevices::hcl.colors(100, "viridis"),
        labels = function(x) format(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"), "%H:%M")
      )
    } else if (is.numeric(color_var)) {
      color_aes <- ggplot2::aes(color = color_var)
      color_scale <- ggplot2::scale_color_gradientn(
        colours = grDevices::hcl.colors(100, "viridis")
      )
    } else {
      epochs$color_group <- as.factor(color_var)
      color_levels <- levels(epochs$color_group)
      color_map <- stats::setNames(scales::hue_pal()(length(color_levels)), color_levels)
      # Orange for awake, colormap for others
      epochs$plot_color <- ifelse(
        epochs$is_asleep == "0",
        "Awake",
        as.character(epochs$color_group)
      )
      color_values <- c(stats::setNames("lightgrey", "Awake"), color_map)
      color_aes <- ggplot2::aes(color = .data$plot_color)
      color_scale <- ggplot2::scale_color_manual(
        values = color_values,
        breaks = names(color_map),
        labels = names(color_map),
        na.value = "grey"
      )
    }
  } else {
    # Default: original colors
    epochs$plot_color <- epochs$is_asleep
    color_values <- c(
      "0" = "orange", # Awake
      "1" = "purple" # Asleep
    )
    color_aes <- ggplot2::aes(color = .data$plot_color)
    color_scale <- ggplot2::scale_color_manual(
      values = color_values,
      breaks = c("0", "1"),
      labels = c("Awake", "Asleep"),
      na.value = "grey"
    )
  }

  ggplot2::ggplot(epochs, ggplot2::aes(x = .data$angle, y = .data$radius)) +
    ggplot2::geom_point(
      mapping = color_aes,
      size = 1,
      shape = 16
    ) +
    color_scale +
    ggplot2::coord_polar(theta = "x") +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 2 * pi, length.out = 25),
      labels = 0:24
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      color = NULL,
      title = NULL
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(size = 4))
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
}

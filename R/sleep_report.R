#' Generate a patient sleep report in PDF format
#'
#' This function generates a sleep report in PDF format using an SVG template.
#' It is designed to work with Somnofy data. Other data types may be supported in the future.
#' @param sessions The sessions dataframe
#' @param title The title of the report. Default is an empty string.
#' @param output_file Path for the output PDF. Default is "Sleep_report.pdf"
#' @returns No return value. Called for side-effects
#' @details This function uses columns:
#' - `night`
#' - `time_at_sleep_onset`
#' - `time_at_wakeup`
#' - `time_at_midsleep`
#' - `sleep_onset_latency`
#' - `sleep_period`
#' - `time_in_bed`
#' @export
#' @examples
#' \donttest{sleep_report(example_sessions)}
sleep_report <- function(sessions, title = "", output_file = "Sleep_report.pdf") {
  nocturn_version <- as.character(utils::packageVersion("nocturn"))

  check_session_colnames(sessions, c("night", "time_at_sleep_onset", "time_at_wakeup", "time_at_midsleep",
                                     "sleep_onset_latency", "sleep_period", "time_in_bed"))

  dates <- format(c(min(sessions$night), max(sessions$night)), "%d/%m/%Y")

  sessions <- remove_sessions_no_sleep(sessions)

  # Stats: Time to fall asleep, sleep efficiency, chronotype, Sleep Regularity (based on midsleep standard deviation)
  stats <- list()
  stats$time_to_fall_asleep <- round(mean(sessions$sleep_onset_latency, na.rm = TRUE) / 60)
  stats$sleep_efficiency <- round(mean(sessions$sleep_period, na.rm = TRUE) / mean(sessions$time_in_bed, na.rm = TRUE) * 100)
  stats$chronotype <- ifelse(chronotype(sessions = sessions) < 4.25, "Morning Lark", "Evening Owl")
  stats$chronotype_image <- ifelse(stats$chronotype == "Morning Lark",
    "Morning_Lark.jpg",
    "Evening_Owl.JPG"
  )
  stats$chronotype_credit <- ifelse(stats$chronotype == "Morning Lark",
    "Artemy Voikhansky, CC BY-SA 4.0",
    "Charles J. Sharp, CC BY-SA 4.0"
  )
  stats$sleep_regularity <- round(100 * (1 - sd_time(sessions$time_at_midsleep) / 2))
  stats$social_jet_lag <- round(social_jet_lag(sessions = sessions), 2)

  clock_plot <- plot_sleep_clock(sessions) +
    ggplot2::scale_color_manual(
      values = c("Sleep Onset" = "#8e44ad", "Wakeup" = "#e67e22"),
      labels = c("Sleep Onset" = "Sleep Time", "Wakeup" = "Wakeup Time")
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(color = "white"),
      axis.text.x = ggplot2::element_text(color = "white"),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.background = ggplot2::element_rect(fill = "transparent", color = NA)
    )

  sleep_times <- plot_bedtimes_waketimes(sessions, groupby = "weekday") +
    ggplot2::labs(title = NULL) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
                   plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
                   legend.background = ggplot2::element_rect(fill = "transparent", color = NA))

  sleep_duration_plot <- sleep_duration_distribution(sessions) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
                   plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
                   legend.background = ggplot2::element_rect(fill = "transparent", color = NA))

  template_path <- system.file("resources", package = "nocturn")
  filled_svg <- tempfile(fileext = ".svg")

  svgedit::draw(
    input_svg = paste0(template_path, "/Sleep_report_template.svg"),
    output_svg = filled_svg,
    plots = list(
      clock_plot = clock_plot,
      sleep_duration_plot = sleep_duration_plot,
      sleep_times = sleep_times
    ),
    plot_scale = list(
      clock_plot = 0.66,
      sleep_duration_plot = 0.66,
      sleep_times = 0.5
    ),
    text = list(
      title = title,
      dates = c(dates[1], dates[2]),
      time_to_fall_asleep = stats$time_to_fall_asleep,
      sleep_efficiency = stats$sleep_efficiency,
      chronotype = stats$chronotype,
      sleep_regularity = stats$sleep_regularity,
      social_jet_lag = stats$social_jet_lag,
      credits = c(nocturn_version, stats$chronotype_credit)
    ),
    images = list(
      chronotype_image = paste0(template_path, "/", stats$chronotype_image)
    )
  )

  filled_pdf <- tempfile(fileext = ".pdf")
  glossary_pdf <- tempfile(fileext = ".pdf")

  rsvg::rsvg_pdf(filled_svg, filled_pdf, width = 842, height = 595)
  rsvg::rsvg_pdf(paste0(template_path, "/Sleep_report_glossary.svg"), glossary_pdf, width = 842, height = 595)

  qpdf::pdf_combine(c(filled_pdf, glossary_pdf), output = output_file)
}

#' @importFrom rlang .data
sleep_duration_distribution <- function(sessions, adjust = 1) {

  plot_data <- sessions |>
    remove_sessions_no_sleep() |>
    dplyr::filter(!is.na(.data$sleep_period)) |>
    dplyr::mutate(sleep_period = as.numeric(.data$sleep_period) / 3600)

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$sleep_period)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(.data$count)),
      color = "blue",
      fill = "blue",
      binwidth = 0.25,
      position = "identity",
      alpha = 0.3,
      linewidth = 0.5
    ) +
    ggplot2::labs(
      title = NULL,
      x = "Sleep Duration (hours)",
      y = NULL
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.08, 0.08))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 18),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 20),
      panel.grid = ggplot2::element_blank()
    )
}

#' Create a Bland-Altman plot
#'
#' @param sessions1 The first sessions dataframe to be compared
#' @param sessions2 The second sessions dataframe to be compared
#' @param variable The name of the variable to compare. Can be a single value for both dataframes, or an array of two values, one for each dataframe
#' @details This function uses columns:
#' - `night`
#' - `sleep_period`
#' @returns a ggplot object showing the Bland-Altman plot
#' @importFrom rlang .data
#' @export
#' @family comparison plot
#' @examples
#'   sessions1 <- data.frame(
#'    time_at_wakeup = c("2025-03-03T11:24:11.524000+00:00",
#'                       "2025-03-03T14:44:11.524000+00:00",
#'                       "2025-03-03T18:24:11.524000+00:00",
#'                       "2025-03-03T00:24:11.524000+00:00",
#'                       "2025-03-03T23:24:11.524000+00:00"),
#'    night = c("2025-03-02", "2025-03-03", "2025-03-03", "2025-03-03", "2025-03-03"),
#'    sleep_period = c(1000, 1000, 10, 2000, 1500)
#'  )
#'
#' sessions2 <- data.frame(
#'    time_at_wakeup = c("2025-03-03T11:14:11.524000+00:00",
#'                       "2025-03-03T14:24:11.524000+00:00",
#'                       "2025-03-03T18:00:11.524000+00:00",
#'                       "2025-03-03T00:22:11.524000+00:00",
#'                       "2025-03-03T23:12:11.524000+00:00"),
#'    night = c("2025-03-02", "2025-03-03", "2025-03-03", "2025-03-03", "2025-03-03"),
#'    sleep_period = c(1000, 1000, 10000, 200, 1500)
#'  )
#' plot_bland_altman(sessions1, sessions2, "time_at_wakeup")
plot_bland_altman <- function(sessions1, sessions2, variable) {
  check_session_colnames(sessions1, c("night", "sleep_period"))
  check_session_colnames(sessions2, c("night", "sleep_period"))

  vars <- rlang::syms(variable)

  if (length(variable) == 1) {
    vars <- rep(vars, 2)
  } else if (length(variable) != 2) {
    cli::cli_abort(c(
      "x" = "{.var variable} must contain either one or two column names",
      "i" = "you provided {len(variable)} values"
    ))
  }

  var1 <- vars[[1]]
  var2 <- vars[[2]]

  if (is_iso8601_datetime(sessions1[[var1]]) && is_iso8601_datetime(sessions2[[var2]])) {
    var_type = "time"
  } else if (is.numeric(sessions1[[var1]][1]) && is.numeric(sessions2[[var2]][1])) {
    var_type = "numeric"
  } else {
    cli::cli_abort(c("x" = "{.var variable} data type must be numeric or datetime for both dataframes",
                     "i" = "sessions1: {variable} is {class(sessions1[[var1]][1])}",
                     "i" = "sessions2: {variable} is {class(sessions2[[var2]][1])}"))
  }

  standardise <- function(x, source_label, variable) {
    x <- keep_longest(x) |>
      dplyr::mutate(
        source = source_label,
        value  = !!variable
      ) |>
      dplyr::select("night", "source", "value") |>
      dplyr::filter(!is.na(.data$value))

    if (var_type == "time") {
      x$value <- update_date(x$value, "0000-01-01")
    }
    x
  }

  s1 <- standardise(sessions1, "sessions1", var1)
  s2 <- standardise(sessions2, "sessions2", var2)

  df <- dplyr::bind_rows(s1, s2) |>
    dplyr::semi_join(dplyr::inner_join(s1, s2, by = "night"), by = "night") |>
    dplyr::arrange(.data$night, .data$source) |>
    dplyr::group_by(.data$night) |>
    dplyr::summarise(
      average = {
        if (var_type == "numeric") {
          mean(.data$value, na.rm = TRUE)
        } else if (var_type == "time") {
          mean_time(.data$value) |>
            shift_times_by_12h()
        }
      },
      diff = {
        v <- .data$value
        if (length(v) < 2) {
          cli::cli_abort(c(
            "x" = "Dataframes have no night in common",
            "i" = "Make sure the two session dataframes have at least one night in common"
          ))
        }
        if (var_type == "time") {
          circ_time_diff(v[2], v[1], unit = "hour")
        } else {
          as.numeric(v[2] - v[1])
        }
      },
      .groups = "drop"
    )

  # --- Bland–Altman reference lines ---
  md  <- mean(df$diff, na.rm = TRUE)
  sdd <- stats::sd(df$diff, na.rm = TRUE)

  loa_upper <- md + 1.96 * sdd
  loa_lower <- md - 1.96 * sdd

  x_lab <- max(df$average, na.rm = TRUE)

  # numeric label formatting helper
  fmt <- function(x) format(round(x, 2), nsmall = 2, trim = TRUE)

  ann <- data.frame(
    x = x_lab,
    y = c(loa_upper, md, loa_lower),
    lab = c(
      paste0("+1.96 SD\n", fmt(loa_upper)),
      paste0("Mean\n", fmt(md)),
      paste0("-1.96 SD\n", fmt(loa_lower))
    )
  )

  p <- ggplot2::ggplot(df) +
    ggplot2::aes(x = .data$average, y = .data$diff) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_hline(yintercept = md, colour = "steelblue", linewidth = 0.9) +
    ggplot2::geom_hline(yintercept = loa_upper, linetype = "dashed", colour = "grey30", linewidth = 0.8) +
    ggplot2::geom_hline(yintercept = loa_lower, linetype = "dashed", colour = "grey30", linewidth = 0.8) +
    ggplot2::geom_text(
      data = ann,
      mapping = ggplot2::aes(x = .data$x, y = .data$y, label = .data$lab),
      inherit.aes = FALSE,
      hjust = -0.05,
      size = 5
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      x = "Average of two measures",
      y = ifelse(var_type == "time", "Difference between two measures (h)", "Difference between two measures"),
      title = "Bland-Altman Plot"
    ) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(plot.margin = ggplot2::margin(5.5, 50, 5.5, 5.5))

  if (var_type == "time") {
    p <- p +
      ggplot2::scale_x_datetime(
        limits = range(df$average, na.rm = TRUE),
        date_breaks = "1 hour",
        labels = \(x) shift_times_by_12h(x) |> format("%H:%M")
      )
  }
  p
}

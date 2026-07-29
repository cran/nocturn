#' Plot a timeseries of variables from different session data
#'
#' @param sessions_list A named list of sessions dataframes from which to plot
#' @param variable The variable to plot - either a single string, or a list of strings (one per dataframe in sessions_list)
#' @param common_nights_only Whether to only plot night for which all datasets have data
#' @returns A ggplot object
#' @details This function uses columns:
#' - `night`
#' @export
#' @family plot sessions
#' @seealso [plot_bland_altman()] to compare a variable between two session dataframes
#' @examples
#' sessions_list <- list(dataset1 = example_sessions, dataset2 = example_sessions_v1)
#' plot_timeseries_comparison(sessions_list, variable = "time_at_sleep_onset")
plot_timeseries_comparison <- function(sessions_list, variable, common_nights_only = FALSE) {

  data <- data.frame()
  nights_list <- vector("list", length(sessions_list))
  is_time <- is_iso8601_datetime(sessions_list[[1]][[variable[1]]])

  for (i in seq_along(sessions_list)) {
    s <- sessions_list[[i]]
    check_session_colnames(s, c("night"))

    if (length(variable) == 1) {
      var <- variable
    } else if (length(variable) == length(sessions_list)) {
      var <- variable[[i]]
    } else {
      cli::cli_abort(c(
        "x" = "{.var variable} must contain either one value, or as many as sessions in {.var sessions_list}",
        "i" = "You provided {len(variable)} values",
        "i" = "{.var sessions_list} contains {len(sessions_list)} values"
      ))
    }

    var_type <- is_iso8601_datetime(s[[var]])
    if (!is.numeric(s[[var]]) && !var_type) {
      cli::cli_abort(c(
        "x" = "{.var variable} column must be either numeric or datetime",
        "i" = "{class(s[[var]])} detected}"
      ))
    }
    if (var_type != is_time) {
      cli::cli_abort(c(
        "x" = "{.var variable} data type must be either numeric or datetime for all dataframes",
        "i" = "Sessions {names(sessions_list)[[i]]} have an inconsistent type ({var_type})"
      ))
    }

    s <- s |>
      dplyr::mutate(
        night = .data$night,
        value = {
          if (is_time) {
            parse_time(.data[[var]]) |>
              shift_times_by_12h() |>
              time_to_hours()
          } else {
            .data[[var]]
          }
        },
        label = names(sessions_list)[[i]]
      ) |>
      dplyr::filter(!is.na(.data$value))

    nights_list[[i]] <- s |>
      dplyr::distinct(.data$night) |>
      dplyr::pull(.data$night)

    data <- dplyr::bind_rows(data, s)
  }

  if (common_nights_only) {
    common_nights <- Reduce(intersect, nights_list)
    data <- data |>
      dplyr::filter(.data$night %in% common_nights)
  }

  p <- ggplot2::ggplot(data, ggplot2::aes(
    x = .data$night,
    y = .data$value,
    colour = .data$label
  )) +
    ggplot2::geom_point(size = 3) +
    ggplot2::labs(
      x = NULL,
      y = variable[1],
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 16)

  if (is_time) {
    p <- p +
      ggplot2::scale_y_continuous(
        breaks = seq(0, 24, by = 2),
        labels = function(x) sprintf("%02d:00", (x + 12) %% 24),
        expand = ggplot2::expansion(mult = c(0.08, 0.08))
      )
  }

  p
}

#' Get appropriate color scale based on the type of the color variable
#'
#' @description For datetime variables, returns a viridis gradient with labels formatted as HH:MM.
#' For numeric variables, returns a viridis gradient. For categorical variables, returns a discrete
#' color scale using default ggplot2 colors.
#' @param color_var The variable to color by (dataframe column)
#' @returns A ggplot2 color scale
#' @keywords internal
#' @seealso [get_fill_scale()] for fill scales
get_color_scale <- function(color_var) {
  if (is_iso8601_datetime(color_var)) {
    ggplot2::scale_color_gradientn(
      colours = grDevices::hcl.colors(100, "viridis"),
      labels = function(x) format(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"), "%H:%M")
    )
  } else if (is.numeric(color_var)) {
    ggplot2::scale_color_gradientn(
      colours = grDevices::hcl.colors(100, "viridis")
    )
  } else {
    color_levels <- levels(as.factor(color_var))
    color_map <- stats::setNames(
      scales::hue_pal()(length(color_levels)),
      color_levels
    )
    ggplot2::scale_color_manual(
      values = color_map
    )
  }
}

#' Get appropriate fill scale based on the type of the color variable
#'
#' @description For datetime variables, returns a viridis gradient with labels formatted as HH:MM.
#' For numeric variables, returns a viridis gradient. For categorical variables, returns a discrete
#' fill scale using default ggplot2 colors.
#' @param color_var The variable to color by (dataframe column)
#' @returns A ggplot2 color scale
#' @keywords internal
#' @seealso [get_color_scale()] for color scales
get_fill_scale <- function(color_var) {
  if (is_iso8601_datetime(color_var)) {
    ggplot2::scale_fill_gradientn(
      colours = grDevices::hcl.colors(100, "viridis"),
      labels = function(x) format(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"), "%H:%M")
    )
  } else if (is.numeric(color_var)) {
    ggplot2::scale_fill_gradientn(
      colours = grDevices::hcl.colors(100, "viridis")
    )
  } else {
    color_levels <- levels(as.factor(color_var))
    color_map <- stats::setNames(
      scales::hue_pal()(length(color_levels)),
      color_levels
    )
    ggplot2::scale_fill_manual(
      values = color_map
    )
  }
}

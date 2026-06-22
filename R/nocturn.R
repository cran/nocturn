#' nocturn app
#'
#' This function launches the nocturn app, a Shiny application for visualizing and analyzing sleep data.
#' @import shiny
#' @returns No return value, called for side-effects
#' @export
#' @examples
#' if(interactive()){nocturn()}
nocturn <- function() {
  app_path <- system.file("shiny", package = "nocturn")
  shiny::runApp(app_path)
}

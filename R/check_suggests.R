#' @title Check whether suggested packages are installed
#' @description Checks whether all the packages in Suggests are installed and prompts for install if not.
#' @returns No return value, called for side effects
#' @family internal
#' @export
check_suggests <- function() {
  desc <- read.dcf(system.file("DESCRIPTION", package = "nocturn"))
  suggests <- trimws(
    gsub(
      "\\(.*\\)", "",
      gsub("\\n", "", unlist(strsplit(desc[, "Suggests"], ",")))
    )
  )

  missing <- suggests[!sapply(suggests, requireNamespace, quietly = TRUE)]

  if (length(missing) > 0) {
    msg <- paste0(
      "The following suggested packages are missing:\n  ",
      paste(missing, collapse = ", "),
      "\n\nWould you like to install them now? [y/n]: "
    )
    response <- readline(msg)
    if (tolower(response) == "y") {
      utils::install.packages(missing)
    } else {
      cli::cli_abort("Cannot proceed without suggested packages.")
    }
  }
}

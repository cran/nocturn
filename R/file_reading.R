#' Read EDF Sessions
#'
#' @param file The path to the EDF file
#' @returns A dataframe containing the session data extracted from the EDF file header
#' @details The function reads the header of the EDF file to extract session information such as
#' start time, duration, and calculates end time and midsleep time.
#' @export
#' @family data loading
#' @examples
#' edf <- system.file("extdata", "mini.edf", package = "nocturn")
#' read_edf_sessions(edf)
read_edf_sessions <- function(file) {
  if (!file.exists(file)) {
    cli::cli_abort(c(
      "!" = "EDF file not found: {.file {file}}",
      "i" = "Please check the file path."
    ))
  }

  hdr <- edfReader::readEdfHeader(file)

  data.frame(
    subject_id = hdr$patient,
    startTime = as.POSIXct(hdr$startTime),
    sleep_period = as.numeric(hdr$recordedPeriod),  # seconds
    endTime = as.POSIXct(hdr$startTime) + as.numeric(hdr$recordedPeriod),
    midsleep = as.POSIXct(hdr$startTime) + as.numeric(hdr$recordedPeriod) / 2,
    is_workday = weekdays(hdr$startTime) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
    stringsAsFactors = FALSE
  )
}

#' Read EDF Epochs
#'
#' @param file The path to the EDF file
#' @returns A dataframe containing the epoch data extracted from the EDF file signals
#' @details The function reads the signals of the EDF file to extract epoch information. It must contain
#' a column for timestamps and a column for sleep stage annotations.
#' @export
#' @family data loading
#' @examples
#' edf <- system.file("extdata", "mini.edf", package = "nocturn")
#' read_edf_epochs(edf)
read_edf_epochs <- function(file) {
  if (!file.exists(file)) {
    cli::cli_abort(c(
      "!" = "EDF file not found: {.file {file}}",
      "i" = "Please check the file path."
    ))
  }

  hdr <- edfReader::readEdfHeader(file)
  sig <- edfReader::readEdfSignals(hdr)

  if (!"annotations" %in% names(sig)) {
    cli::cli_warn(c(
      "!" = "No annotations signal found in EDF file: {.file {file}}",
      "i" = "Returning NULL."
    ))
    return(NULL)
  }

  as.data.frame(sig$annotations, stringsAsFactors = FALSE)
}

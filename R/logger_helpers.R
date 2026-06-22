#' @title write_log
#' @description For internal use. Add text to a logger
#' @param logger The logger to write the text to. Can be `NULL` or a function
#' @param ... Messages to write to the logger
#' @param type One of `default`, `info`, `error`, `warning`, `starting`, `complete`
#' @returns No return value, called for side effects
#' @keywords internal
#' @export
write_log <- function(logger, ..., type = "default") {
  if (is.null(logger)) {
    if (type == "error") {
      stop(paste0(..., collapse = ""), call. = FALSE)
    } else if (type == "warning") {
      warning(paste0(..., collapse = ""), call. = FALSE)
    } else {
      message(paste0(..., collapse = ""))
    }
  } else if (is.function(logger)) {
    if (type == "default") {
      pre <- "> "
    } else if (type == "starting") {
      pre <- paste0(shiny::icon("clock", class = "log_start"), " ")
    } else if (type == "complete") {
      pre <- paste0(shiny::icon("check", class = "log_end"), " ")
    } else if (type == "info") {
      pre <- paste0(shiny::icon("info", class = "log_info"), " ")
    } else if (type == "error") {
      if (nchar(...) < 80) {
        shinyalert::shinyalert(...,
                               type = "error")
      } else {
        shinyalert::shinyalert("Please check log window for more information ",
                               type = "error")
      }
      pre <- paste0(shiny::icon("xmark", class = "log_error"), " ")
    } else if (type == "warning") {
      pre <- paste0(shiny::icon("triangle-exclamation", class = "log_warn"), " ")
    }
    new_entries <- paste0("<br>", pre, ..., collapse = "")
    logger(paste0(logger(), new_entries))
  } else {
    warning("Invalid logger type")
  }
  invisible()
}

#' Show a log message only once based on a condition
#'
#' This function logs a message only once when a specified condition is true.
#' It uses a reactive value to track whether the message has already been logged.
#' @param logger The logger to write the text to
#' @param condition A logical condition to determine whether to log the message
#' @param log_shown A reactive value indicating whether the log message has been shown
#' @param log_msg The message to log
#' @param log_type One of `default`, `info`, `error`, `warning`, `starting`, `complete`
#' @returns No return value, called for side effects
#' @keywords internal
#' @export
show_log_once <- function(logger, condition, log_shown, log_msg, log_type) {
  if (condition) {
    if (!log_shown()) {
      logger |> write_log(log_msg, type = log_type)
      log_shown(TRUE)
    }
  } else {
    log_shown(FALSE)
  }
}

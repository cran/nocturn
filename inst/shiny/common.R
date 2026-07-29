common_class <- R6::R6Class(
  classname = "common",
  public = list(
    logger = NULL,
    sessions = NULL,
    sessions_raw = NULL,
    epochs = NULL,
    epochs_raw = NULL,
    annotations = NULL,
    filter_values = NULL,
    session_filters = NULL,
    epoch_filters = NULL,
    secondary_sessions = NULL,
    initialize = function() {
      self$sessions <- shiny::reactiveVal()
      self$sessions_raw <- shiny::reactiveVal()
      self$epochs <- shiny::reactiveVal()
      self$epochs_raw <- shiny::reactiveVal()
      self$annotations <- shiny::reactiveVal()
      self$filter_values <- shiny::reactiveVal()
      self$session_filters <- shiny::reactiveVal()
      self$epoch_filters <- shiny::reactiveVal()
      self$secondary_sessions <- shiny::reactiveVal()
    }
  )
)

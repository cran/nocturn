common_class <- R6::R6Class(
  classname = "common",
  public = list(
    logger = NULL,
    sessions = NULL,
    epochs = NULL,
    annotations = NULL,
    session_filters = NULL,
    epoch_filters = NULL,
    initialize = function() {
      self$sessions <- shiny::reactiveVal()
      self$epochs <- shiny::reactiveVal()
      self$annotations <- shiny::reactiveVal()
      self$session_filters <- shiny::reactiveVal()
      self$epoch_filters <- shiny::reactiveVal()
    }
  )
)

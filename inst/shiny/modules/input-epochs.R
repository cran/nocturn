input_epochs_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      shiny::h4("Epochs"),
      shiny::fileInput(
        inputId = ns("epochs_file"),
        label = NULL,
        multiple = TRUE,
        placeholder = "(multiple files accepted)",
        accept = c(".csv", ".xls", ".xlsx", ".edf", ".rec")
      ),
      shiny::fluidRow(
        shiny::column(
          width = 8,
          shiny::actionButton(ns("open_epoch_col_names"), "Set Column Names", width = "100%", icon = shiny::icon("columns"))
        ),
        shiny::column(
          width = 4,
          shiny::actionButton(ns("clear_epochs"), "Clear", width = "100%", icon = shiny::icon("trash"), class = "delete-btn")
        )
      ),
      class = "sidebar_card"
    )
  )
}

input_epochs_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Clear epochs ----
    shiny::observeEvent(input$clear_epochs, {
      clear_epochs(common)
      common$logger |> write_log("Cleared epoch data", type = "complete")
    })

    # File upload ----
    shiny::observeEvent(input$epochs_file, {
      shiny::req(input$epochs_file)
      if (nrow(input$epochs_file) == 1) {
        data <- load_epochs(input$epochs_file$datapath)
        common$logger |> write_log(paste0("Loaded session file: ", input$epochs_file$name), type = "complete")
      } else if (nrow(input$epochs_file) > 1) {
        data <- load_batch(file_list = input$epochs_file$datapath, file_names = input$epochs_file$name, type = "epochs")
        common$logger |> write_log(paste0("Loaded ", nrow(input$epochs_file), " epoch files"), type = "complete")
      } else {
        return()
      }
      init_epochs(data, common)
    })

    # Column names modal ----
    shiny::observeEvent(input$open_epoch_col_names, {
      shiny::req(common$epochs())
      show_colnames_modal(
        ns = ns,
        colnames_list = colnames(common$epochs()),
        current_map = get_colnames(common$epochs()),
        type = "Epochs",
        save_id = "save_epoch_col_names",
        reset_id = "reset_epoch_col_names"
      )
    })

    shiny::observeEvent(input$reset_epoch_col_names, {
      epochs <- common$epochs()
      attr(epochs, "col") <- NULL
      col <- get_colnames(epochs)
      common$epochs(set_colnames(epochs, col))
      common$logger |> write_log("Reset epoch column names to default", type = "complete")
      shiny::removeModal()
    })

    shiny::observeEvent(input$save_epoch_col_names, {
      keys <- names(.epochs_long)
      vals <- lapply(keys, function(key) {
        val <- input[[paste0("col_", key)]]
        if (identical(val, "-")) NULL else val
      })
      common$epochs(set_colnames(common$epochs(), stats::setNames(vals, keys)))
      common$epochs(clean_epochs(common$epochs()))
      common$logger |> write_log("Epoch column names saved", type = "complete")
      shiny::removeModal()
    })

  })
}

init_epochs <- function(epochs, common) {
  epochs$annotation <- ""
  common$epochs(epochs)
  common$epoch_filters(data.frame(from_sessions = rep(TRUE, nrow(epochs))))
}

clear_epochs <- function(common) {
  common$epochs(NULL)
  common$epoch_filters(NULL)
}

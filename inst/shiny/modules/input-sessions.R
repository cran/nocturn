input_sessions_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      shiny::h4("Sessions"),
      shiny::fileInput(
        inputId = ns("sessions_file"),
        label = NULL,
        multiple = TRUE,
        placeholder = "(multiple files accepted)",
        accept = c(".csv", ".xls", ".xlsx", ".edf", ".rec")
      ),
      shiny::fluidRow(
        shiny::column(
          width = 8,
          shiny::actionButton(ns("open_session_col_names"), "Set Column Names", width = "100%", icon = shiny::icon("columns"))
        ),
        shiny::column(
          width = 4,
          shiny::actionButton(ns("clear_sessions"), "Clear", width = "100%", icon = shiny::icon("trash"), class = "delete-btn")
        )
      ),
      class = "sidebar_card"
    )
  )
}

input_sessions_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Clear sessions ----
    shiny::observeEvent(input$clear_sessions, {
      clear_sessions(common)
      common$logger |> write_log("Cleared session data", type = "complete")
    })

    # File upload ----
    shiny::observeEvent(input$sessions_file, {
      shiny::req(input$sessions_file)
      if (nrow(input$sessions_file) == 1) {
        data <- load_sessions(input$sessions_file$datapath)
        common$logger |> write_log(paste0("Loaded session file: ", input$sessions_file$name), type = "complete")
      } else if (nrow(input$sessions_file) > 1) {
        data <- load_batch(file_list = input$sessions_file$datapath, type = "sessions")
        common$logger |> write_log(paste0("Loaded ", nrow(input$sessions_file), " session files"), type = "complete")
      } else {
        return()
      }
      init_sessions(data, common)
    })

    # Column names modal ----
    shiny::observeEvent(input$open_session_col_names, {
      shiny::req(common$sessions())
      show_colnames_modal(
        ns = ns,
        colnames_list = colnames(common$sessions()),
        current_map = get_colnames(common$sessions()),
        type = "Sessions",
        save_id = "save_session_col_names",
        reset_id = "reset_session_col_names"
      )
    })

    shiny::observeEvent(input$reset_session_col_names, {
      sessions <- common$sessions()
      attr(sessions, "col") <- NULL
      col <- get_colnames(sessions)
      common$sessions(set_colnames(sessions, col))
      common$sessions(clean_sessions(common$sessions()))
      common$logger |> write_log("Reset session column names to default", type = "complete")
      shiny::removeModal()
    })

    shiny::observeEvent(input$save_session_col_names, {
      keys <- names(.sessions_long)
      vals <- lapply(keys, function(key) {
        val <- input[[paste0("col_", key)]]
        if (identical(val, "-")) NULL else val
      })
      common$sessions(set_colnames(common$sessions(), stats::setNames(vals, keys)))
      common$sessions(clean_sessions(common$sessions()))
      common$logger |> write_log("Session column names saved", type = "complete")
      shiny::removeModal()
    })

  })
}

init_sessions <- function(sessions, common) {
  col <- get_colnames(sessions)
  sessions$annotation <- ""
  common$sessions(sessions)
  common$session_filters(data.frame(no_sleep = rep(TRUE, nrow(sessions))))
  common$annotations(data.frame(
    id = sessions[[col$id]],
    annotation = "",
    stringsAsFactors = FALSE
  ))
}

clear_sessions <- function(common) {
  common$sessions(NULL)
  common$session_filters(NULL)
  common$annotations(NULL)
}

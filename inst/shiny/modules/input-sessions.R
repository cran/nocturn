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

    register_colnames_modal(
      input = input, session = session, ns = ns, common = common,
      type = "sessions",
      open_event = "open_session_col_names",
      get_df = \() common$sessions(),
      get_raw = \() common$sessions_raw(),
      set_df = \(df) common$sessions(df)
    )
  })
}

init_sessions <- function(sessions, common) {
  sessions$annotation <- ""
  common$sessions(sessions)
  common$sessions_raw(sessions)
  common$session_filters(data.frame(no_sleep = rep(TRUE, nrow(sessions))))
  common$filter_values(list())
  common$annotations(data.frame(
    id = sessions$id,
    annotation = "",
    stringsAsFactors = FALSE
  ))
}

clear_sessions <- function(common) {
  common$sessions(NULL)
  common$sessions_raw(NULL)
  common$session_filters(NULL)
  common$annotations(NULL)
}

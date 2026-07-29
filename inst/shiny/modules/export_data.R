export_data_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Raw Data"),
    shiny::downloadButton(
      outputId = ns("download_sessions"),
      label = "Sessions"
    ),
    shiny::downloadButton(
      outputId = ns("download_epochs"),
      label = "Epochs"
    ),
    shiny::br(),
    shiny::br(),
    shiny::h4("Subject Report"),
    shiny::p("(Somnofy data only)"),
    shiny::textInput(
      inputId = ns("title"),
      label = "Report Title",
      value = ""
    ),
    shiny::downloadButton(
      outputId = ns("download_report"),
      label = "Subject Report"
    )
  )
}

export_data_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {

    # Sessions download ----
    shiny::observe({
      shiny::req(common$sessions(), common$session_filters())
      sessions <- apply_filters(common$sessions(), common$session_filters()) |>
        annotate(common$annotations())
      output$download_sessions <- get_table_download_handler(
        session = session,
        common = common,
        output_table = sessions,
        output_name = "sessions"
      )
    })

    # Epochs download ----
    shiny::observe({
      shiny::req(common$epochs(), common$epoch_filters())
      epochs <- apply_filters(common$epochs(), common$epoch_filters())
      output$download_epochs <- get_table_download_handler(
        session = session,
        common = common,
        output_table = epochs,
        output_name = "epochs"
      )
    })

    # Report download ----
    shiny::observe({
      shiny::req(common$sessions(), common$session_filters())
      sessions <- apply_filters(common$sessions(), common$session_filters()) |>
        annotate(common$annotations())
      output$download_report <- get_report_download_handler(
        session = session,
        logger = common$logger,
        sessions = sessions,
        title = shiny::reactive(input$title)
      )
    })
  })
}

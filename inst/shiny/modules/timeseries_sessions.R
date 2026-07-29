timeseries_sessions_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    help_modal_ui(ns),
    shiny::fluidRow(
      shiny::column(4,
        shiny::selectInput(
          inputId = ns("variable"),
          label = "Select Variable:",
          choices = NULL
        )
      ),
      shiny::column(4,
        shiny::selectInput(
          inputId = ns("colorby"),
          label = "Colour by:",
          choices = NULL
        )
      )
    ),
    shiny::checkboxInput(
      inputId = ns("exclude_zero"),
      label = "Exclude Zero Values",
      value = FALSE
    ),
    shiny::plotOutput(ns("timeseries_sessions_plot")),
    shiny::downloadButton(
      outputId = ns("download_plot"),
      label = NULL,
      class = "small-btn"
    ),
    shiny::radioButtons(
      inputId = ns("download_format"),
      label = NULL,
      choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
      inline = TRUE
    )
  )
}

timeseries_sessions_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {

    plot_options <- shiny::reactiveValues(variable = NULL, colorby = NULL)
    update_variable_dropdown(common$sessions, plot_options, input, session)
    update_colorby_dropdown(common$sessions, plot_options, input, session)

    timeseries_sessions_plot <- shiny::reactive({
      shiny::req(input$variable, common$sessions(), common$session_filters())
      sessions <- apply_filters(common$sessions(), common$session_filters()) |>
        annotate(common$annotations())
      validate_columns(sessions, "night")
      plot_timeseries_sessions(
        sessions = sessions,
        variable = input$variable,
        color_by = input$colorby,
        exclude_zero = input$exclude_zero
      )
    })

    output$timeseries_sessions_plot <- shiny::renderPlot({
      shiny::req(timeseries_sessions_plot())
      timeseries_sessions_plot()
    })

    output$download_plot <- get_plot_download_handler(
      session = session,
      common = common,
      output_plot = timeseries_sessions_plot,
      format = shiny::reactive(input$download_format),
      width = 12,
      height = 6
    )

    shiny::observeEvent(input$help, {
      show_help_modal("Session_timeseries")
    })
  })
}

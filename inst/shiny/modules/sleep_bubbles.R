sleep_bubbles_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    help_modal_ui(ns),
    shiny::selectInput(
      inputId = ns("colorby"),
      label = "Colour by:",
      choices = NULL
    ),
    shiny::plotOutput(ns("sleep_bubbles_plot")),
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

sleep_bubbles_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {

    plot_options <- shiny::reactiveValues(colorby = NULL)
    update_colorby_dropdown(common$sessions, plot_options, input, session)

    sleep_bubbles_plot <- shiny::reactive({
      shiny::req(common$sessions(), common$session_filters())
      sessions <- apply_filters(common$sessions(), common$session_filters()) |>
        annotate(common$annotations())
      if (nrow(sessions) == 0) {
        return(NULL)
      }
      validate_columns(sessions, c("night", "sleep_period"))
      plot_sleep_bubbles(
        sessions = sessions,
        color_by = input$colorby
      )
    })

    output$sleep_bubbles_plot <- shiny::renderPlot({
      shiny::req(sleep_bubbles_plot())
      sleep_bubbles_plot()
    })

    output$download_plot <- get_plot_download_handler(
      session = session,
      common = common,
      output_plot = sleep_bubbles_plot,
      format = shiny::reactive(input$download_format),
      width = 10,
      height = 6
    )

    shiny::observeEvent(input$help, {
      show_help_modal("Sleep_bubbles")
    })
  })
}

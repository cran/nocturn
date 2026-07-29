hypnogram_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    help_modal_ui(ns),
    shiny::plotOutput(ns("hypnogram_plot")),
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

hypnogram_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {

    hypnogram_plot <- shiny::reactive({
      shiny::req(common$epochs(), common$epoch_filters())
      epochs <- apply_filters(common$epochs(), common$epoch_filters())
      if (nrow(epochs) == 0) {
        return(NULL)
      }
      validate_columns(epochs, c("timestamp", "sleep_stage"))
      plot_hypnogram(epochs = epochs)
    })

    output$hypnogram_plot <- shiny::renderPlot({
      shiny::req(hypnogram_plot())
      hypnogram_plot()
    })

    output$download_plot <- get_plot_download_handler(
      session = session,
      common = common,
      output_plot = hypnogram_plot,
      format = shiny::reactive(input$download_format),
      width = 12,
      height = 6
    )

    shiny::observeEvent(input$help, {
      show_help_modal("Hypnogram")
    })
  })
}

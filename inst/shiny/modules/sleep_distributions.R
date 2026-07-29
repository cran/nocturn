sleep_distributions_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    help_modal_ui(ns),
    shiny::fluidRow(
      shiny::column(4,
        shiny::selectInput(
          inputId = ns("plot_type"),
          label = "Select Plot Type:",
          choices = c("Boxplot", "Histogram", "Density")
        )
      ),
      shiny::column(4,
        shiny::uiOutput(ns("binwidth_slider")),
        shiny::uiOutput(ns("adjust_slider"))
      )
    ),
    shiny::checkboxInput(
      inputId = ns("circular"),
      label = "Circular",
      value = FALSE
    ),
    shiny::plotOutput(ns("sleep_distribution_plot")),
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

sleep_distributions_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {

    output$binwidth_slider <- shiny::renderUI({
      if (input$plot_type == "Histogram") {
        shiny::sliderInput(
          session$ns("binwidth"),
          "Binwidth:",
          min = 0.1,
          max = 2,
          value = 0.25,
          step = 0.05,
          ticks = FALSE
        )
      }
    })

    output$adjust_slider <- shiny::renderUI({
      if (input$plot_type == "Density") {
        shiny::sliderInput(
          session$ns("adjust"),
          "Adjust:",
          min = 0.2,
          max = 3,
          value = 1,
          step = 0.2,
          ticks = FALSE
        )
      }
    })

    sleep_distribution_plot <- shiny::reactive({
      shiny::req(input$plot_type, common$sessions(), common$session_filters())
      col <- get_colnames(common$sessions())
      sessions <- apply_filters(common$sessions(), common$session_filters()) |>
        annotate(common$annotations())
      validate_columns(sessions, c("time_at_sleep_onset", "time_at_midsleep", "time_at_wakeup", "sleep_period"))
      shiny::validate(
        shiny::need(nrow(sessions) > 0, message = FALSE)
      )
      switch(input$plot_type,
        "Boxplot" = {
          sleeptimes_boxplot(sessions, circular = input$circular)
        },
        "Histogram" = {
          shiny::req(input$binwidth)
          sleeptimes_histogram(sessions, binwidth = input$binwidth, circular = input$circular)
        },
        "Density" = {
          shiny::req(input$adjust)
          sleeptimes_density(sessions, adjust = input$adjust, circular = input$circular)
        }
      )
    })

    output$sleep_distribution_plot <- shiny::renderPlot({
      shiny::req(sleep_distribution_plot())
      sleep_distribution_plot()
    })

    output$download_plot <- get_plot_download_handler(
      session = session,
      common = common,
      output_plot = sleep_distribution_plot,
      format = shiny::reactive(input$download_format),
      width = 12,
      height = 6
    )

    shiny::observeEvent(input$help, {
      show_help_modal("Sleep_times_distributions")
    })
  })
}

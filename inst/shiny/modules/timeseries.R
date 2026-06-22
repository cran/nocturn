timeseries_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::HTML("<b>Requires Epoch data</b>"),
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
    shiny::plotOutput(ns("timeseries_plot")),
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

timeseries_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {

    plot_options <- shiny::reactiveValues(variable = NULL, colorby = NULL)
    update_variable_dropdown(common$epochs, plot_options, input, session)
    update_colorby_dropdown(common$epochs, plot_options, input, session)

    timeseries_plot <- shiny::reactive({
      shiny::req(input$variable, common$epochs(), common$epoch_filters())
      epochs <- apply_filters(common$epochs(), common$epoch_filters())
      col <- get_colnames(common$epochs())
      shiny::validate(
        shiny::need(!is.null(col$timestamp), "'timestamp' column was not specified."),
        shiny::need(!is.null(col$night), "'night' column was not specified.")
      )
      plot_timeseries(
        epochs = epochs,
        variable = input$variable,
        color_by = input$colorby,
        exclude_zero = input$exclude_zero
      )
    })

    output$timeseries_plot <- shiny::renderPlot({
      shiny::req(timeseries_plot())
      timeseries_plot()
    })

    output$download_plot <- get_plot_download_handler(
      session = session,
      common = common,
      output_plot = timeseries_plot,
      format = shiny::reactive(input$download_format),
      width = 12,
      height = 6
    )

  })
}

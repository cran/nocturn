bland_altman_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    help_modal_ui(ns),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::selectInput(
          inputId = ns("sessions1"),
          label = "Sessions 1",
          choices = NULL
        )
      ),
      shiny::column(
        width = 4,
        shiny::selectInput(
          inputId = ns("sessions2"),
          label = "Sessions 2",
          choices = NULL
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::selectInput(
          inputId = ns("variable1"),
          label = "Variable 1",
          choices = NULL
        )
      ),
      shiny::column(
        width = 4,
        shiny::selectInput(
          inputId = ns("variable2"),
          label = "Variable 2",
          choices = NULL
        )
      )
    ),
    shiny::plotOutput(ns("bland_altman_plot")),
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

bland_altman_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {

    ss <- common$secondary_sessions

    plot_options <- shiny::reactiveValues(sessions1 = NULL, sessions2 = NULL, variable1 = NULL, variable2 = NULL)

    s1_data <- shiny::reactive({
      shiny::req(input$sessions1)
      ss()[[input$sessions1]]$data
    })

    s2_data <- shiny::reactive({
      shiny::req(input$sessions2)
      ss()[[input$sessions2]]$data
    })

    update_session_dropdown(ss, plot_options, input, session, input_id = "sessions1")
    update_session_dropdown(ss, plot_options, input, session, input_id = "sessions2")

    update_variable_dropdown(s1_data, plot_options, input, session, input_id = "variable1")
    update_variable_dropdown(s2_data, plot_options, input, session, input_id = "variable2")

    bland_altman_plot <- shiny::reactive({
      shiny::req(ss, s1_data(), s2_data(), input$variable1, input$variable2)
      validate_column_types(s1_data(), s2_data(), input$variable1, input$variable2)
      s1 <- apply_filters(s1_data(), ss()[[input$sessions1]]$filters)
      s2 <- apply_filters(s2_data(), ss()[[input$sessions2]]$filters)
      validate_columns(s1, c("night", "sleep_period"))
      validate_columns(s2, c("night", "sleep_period"))

      shiny::validate(
        shiny::need(
          length(intersect(s1$night, s2$night)) > 0,
          "Datasets have no night in common"
        )
      )

      plot_bland_altman(
        s1,
        s2,
        variable = c(input$variable1, input$variable2)
      )
    })

    output$bland_altman_plot <- shiny::renderPlot({
      bland_altman_plot()
    })

    output$download_plot <- get_plot_download_handler(
      session = session,
      common = common,
      output_plot = bland_altman_plot,
      format = shiny::reactive(input$download_format),
      width = 12,
      height = 6
    )

    shiny::observeEvent(input$help, {
      show_help_modal("Bland-Altman")
    })
  })
}

validate_column_types <- function(s1, s2, col1, col2) {
  shiny::req(col1, col2)

  v1 <- s1[[col1]]
  v2 <- s2[[col2]]

  is_time1 <- is_iso8601_datetime(v1)
  is_time2 <- is_iso8601_datetime(v2)

  is_num1  <- is.numeric(v1)
  is_num2  <- is.numeric(v2)

  is_num_or_date <- (is_num1 || is_time1) && (is_num2 || is_time2)
  is_same_type   <- (is_num1 && is_num2) || (is_time1 && is_time2)

  shiny::validate(
    shiny::need(is_num_or_date, "Variable 1 and Variable2 must both be either numerical or time type"),
    shiny::need(is_same_type, "Variable 1 and Variable 2 must have the same type (numerical or time)")
  )
}

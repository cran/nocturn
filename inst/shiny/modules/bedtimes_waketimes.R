bedtimes_waketimes_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    help_modal_ui(ns),
    shiny::fluidRow(
      shiny::column(4,
        shiny::selectInput(
          inputId = ns("groupby"),
          label = "Group by:",
          choices = c("night", "weekday", "workday")
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
    shiny::plotOutput(ns("bedtimes_waketimes_plot")),
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

bedtimes_waketimes_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {

    plot_options <- shiny::reactiveValues(colorby = NULL)
    update_colorby_dropdown(common$sessions, plot_options, input, session)

    log_shown <- shiny::reactiveVal(FALSE)
    shiny::observe({
      if (!is.null(input$colorby) && input$colorby != "default" && input$groupby != "night") {
        if (!log_shown()) {
          common$logger |> write_log("Resetting 'Group by' to 'night' when 'Colour by' is set to a custom column",
                                     type = "warning")
          log_shown(TRUE)
        }
        shiny::updateSelectInput(session, inputId = "groupby", selected = "night")
      } else {
        log_shown(FALSE)
      }
    })

    bedtimes_waketimes_plot <- shiny::reactive({
      shiny::req(common$sessions(), common$session_filters())
      sessions <- apply_filters(common$sessions(), common$session_filters()) |>
        annotate(common$annotations())
      if (nrow(sessions) == 0) {
        return(NULL)
      }
      validate_columns(sessions, c("time_at_sleep_onset", "time_at_wakeup", "night", "is_workday"))

      plot_bedtimes_waketimes(
        sessions = sessions,
        groupby = input$groupby,
        color_by = input$colorby
      )
    })

    output$bedtimes_waketimes_plot <- shiny::renderPlot({
      shiny::req(bedtimes_waketimes_plot())
      bedtimes_waketimes_plot()
    })

    output$download_plot <- get_plot_download_handler(
      session = session,
      common = common,
      output_plot = bedtimes_waketimes_plot,
      format = shiny::reactive(input$download_format),
      width = 8,
      height = 6
    )

    shiny::observeEvent(input$help, {
      show_help_modal("Sleep_onset_and_wakeup")
    })

  })
}

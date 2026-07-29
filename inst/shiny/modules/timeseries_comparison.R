timeseries_comparison_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    help_modal_ui(ns),
    shiny::uiOutput(ns("variable_choices")),
    shiny::checkboxInput(ns("common_nights_only"), label = "Keep only nights in common", value = FALSE),
    shiny::plotOutput(ns("timeseries_comparison_plot")),
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

variable_picker_ui <- function(id, title = NULL) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(2, shiny::HTML(title %||% "")),
    shiny::column(
      width = 3,
      shiny::selectInput(
        inputId = ns("variable"),
        label = NULL,
        choices = character(0)
      )
    ),
    shiny::column(
      width = 2,
      shiny::checkboxInput(ns("display"), label = "Display", value = TRUE)
    )
  )
}

timeseries_comparison_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    secondary_sessions <- common$secondary_sessions
    started <- shiny::reactiveVal(character())
    variable_list <- shiny::reactiveVal(list())

    # Dynamic variable picker ----
    output$variable_choices <- shiny::renderUI({
      ss <- secondary_sessions()
      keys <- names(ss)
      if (!length(keys)) return(shiny::div("No sessions loaded yet."))

      shiny::tagList(lapply(keys, function(key) {
        variable_picker_ui(id = ns(paste0("row__", key)), title = ss[[key]]$title)
      }))
    })

    outputOptions(output, "variable_choices", suspendWhenHidden = FALSE)

    shiny::observe({
      shiny::req(secondary_sessions())
      ss <- secondary_sessions()
      keys = names(ss)
      new_keys <- setdiff(keys, started())

      for (key in new_keys) {
        df_r <- shiny::reactive({
          ss2 <- secondary_sessions()
          req(key %in% names(ss2))
          ss2[[key]]$data
        })

        variable_picker_server(
          id = paste0("row__", key),
          key = key,
          df = df_r,
          variable_list = variable_list
        )
      }
      started(c(started(), new_keys))
    })

    # Timeseries plot ----
    timeseries_comparison_plot <- shiny::reactive({
      shiny::req(secondary_sessions(), variable_list())
      ss <- secondary_sessions()
      var_list <- variable_list()
      shiny::req(length(ss) > 0)
      plot_df <- list()
      plot_var <- character(0)
      var_types <- list()
      for (id in names(ss)) {
        cfg <- var_list[[id]]
        if (is.null(cfg)) next
        if (!isTRUE(cfg$display)) next
        if (is.null(cfg$variable) || !nzchar(cfg$variable)) next

        df <- apply_filters(ss[[id]]$data, ss[[id]]$filters)

        validate_columns(df, "night")
        if (is.numeric(df[[cfg$variable]])) {
          var_types[[id]] <- "numeric"
        } else if (is_iso8601_datetime(df[[cfg$variable]])) {
          var_types[[id]] <- "time"
        } else {
          next
        }

        title <- ss[[id]]$title
        plot_df[[title]] <- df
        plot_var[title] <- var_list[[id]]$variable
      }

      shiny::validate(
        shiny::need(length(plot_df) > 0, "Variables must be numerical or time"),
        shiny::need(length(unique((var_types))) == 1, "Variables must have the same type (numerical or time)")
      )
      plot_timeseries_comparison(plot_df, plot_var, input$common_nights_only)
    })

    output$timeseries_comparison_plot <- shiny::renderPlot({
      shiny::req(timeseries_comparison_plot())
      timeseries_comparison_plot()
    })

    output$download_plot <- get_plot_download_handler(
      session = session,
      common = common,
      output_plot = timeseries_comparison_plot,
      format = shiny::reactive(input$download_format),
      width = 12,
      height = 6
    )

    shiny::observeEvent(input$help, {
      show_help_modal("Comparison_timeseries")
    })
  })
}

variable_picker_server <- function(id, key, df, variable_list) {
  shiny::moduleServer(id, function(input, output, session) {

    plot_options <- shiny::reactiveValues(variable = NULL)

    update_variable_dropdown(
      df = df,
      plot_options = plot_options,
      input = input,
      session = session,
      input_id = "variable"
    )

    shiny::observe({
      shiny::req(plot_options$variable)

      var_list <- variable_list()
      if (is.null(var_list[[key]])) {
        var_list[[key]] <- list(variable = plot_options$variable, display = TRUE)
      } else {
        var_list[[key]]$variable <- plot_options$variable
        if (is.null(var_list[[key]]$display)) var_list[[key]]$display <- TRUE
      }
      variable_list(var_list)
    })

    shiny::observe({
      cfg <- variable_list()[[key]]
      req(!is.null(cfg), !is.null(cfg$display))

      shiny::updateCheckboxInput(
        session,
        "display",
        value = isTRUE(cfg$display)
      )
    })

    # Update display status
    shiny::observeEvent(input$display, {
      var_list <- variable_list()
      if (is.null(var_list[[key]])) var_list[[key]] <- list(variable = plot_options$variable, display = TRUE)
      var_list[[key]]$display <- input$display
      variable_list(var_list)
    }, ignoreInit = TRUE)
  })
}

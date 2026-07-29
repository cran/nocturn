sleep_regularity_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::HTML("Click on metrics names for more information.<br>"),
    shiny::h5("Sessions-based Sleep Regularity Metrics"),
    shiny::tableOutput(ns("sleep_sessions_regularity_table")),
    shiny::h5("Epochs-based Sleep Regularity Metrics"),
    shiny::tableOutput(ns("sleep_epochs_regularity_table")),
    shiny::HTML("<span>Metrics based on <a href='https://doi.org/10.1093/sleep/zsab103' target='_blank'>Fischer et al. (2021)</a></span>")
  )
}

sleep_regularity_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sessions <- shiny::reactive({
      shiny::req(common$sessions(), common$session_filters())
      apply_filters(common$sessions(), common$session_filters())
    })

    epochs <- shiny::reactive({
      shiny::req(common$epochs(), common$epoch_filters())
      apply_filters(common$epochs(), common$epoch_filters())
    })

    metric_names_sessions <- c(
      "Mid-sleep Standard Deviation",
      "Social Jet Lag",
      "Composite Phase Deviation",
      "Chronotype"
    )
    metric_ids_sessions <- c(
      "msd", "sjl", "cpd", "chronotype"
    )

    metric_names_epochs <- c(
      "Interdaily Stability",
      "Sleep Regularity Index"
    )
    metric_ids_epochs <- c(
      "is", "sri"
    )

    metric_values_sessions <- shiny::reactive({
      s <- sessions()
      validate_columns(s, c("time_at_midsleep", "is_workday", "night", "sleep_period"))
      if (is.null(s) || nrow(s) == 0) return(rep(NA, length(metric_names_sessions)))
      c(
        sd_time(s$time_at_midsleep, unit = "hour"),
        social_jet_lag(s),
        composite_phase_deviation(s),
        chronotype(s)
      )
    })

    metric_values_epochs <- shiny::reactive({
      e <- epochs()
      validate_columns(e, c("timestamp", "is_asleep"))
      if (is.null(e) || nrow(e) == 0) return(rep(NA, length(metric_names_epochs)))
      c(
        interdaily_stability(e),
        sleep_regularity_index(e)
      )
    })

    output$sleep_sessions_regularity_table <- shiny::renderUI({
      shiny::tags$table(
        class = "table",
        style = "width: 300px;",
        shiny::tags$thead(
          shiny::tags$tr(
            shiny::tags$th("Metric"),
            shiny::tags$th("Value")
          )
        ),
        shiny::tags$tbody(
          c(
            lapply(seq_along(metric_names_sessions), function(i) {
              shiny::tags$tr(
                shiny::tags$td(
                  shiny::actionLink(ns(paste0("metric_", metric_ids_sessions[i])), metric_names_sessions[i])
                ),
                shiny::tags$td(
                  round(metric_values_sessions()[i], 2)
                )
              )
            })
          )
        )
      )
    })

    output$sleep_epochs_regularity_table <- shiny::renderUI({
      shiny::tags$table(
        class = "table",
        style = "width: 300px;",
        shiny::tags$thead(
          shiny::tags$tr(
            shiny::tags$th("Metric"),
            shiny::tags$th("Value")
          )
        ),
        shiny::tags$tbody(
          c(
            lapply(seq_along(metric_names_epochs), function(i) {
              shiny::tags$tr(
                shiny::tags$td(
                  shiny::actionLink(ns(paste0("metric_", metric_ids_epochs[i])), metric_names_epochs[i])
                ),
                shiny::tags$td(
                  round(metric_values_epochs()[i], 2)
                )
              )
            })
          )
        )
      )
    })

    shiny::observeEvent(input$metric_msd, {
      show_metric_modal("Mid-sleep_Standard_Deviation")
    })
    shiny::observeEvent(input$metric_is, {
      show_metric_modal("Interdaily_Stability")
    })
    shiny::observeEvent(input$metric_sjl, {
      show_metric_modal("Social_Jet_Lag")
    })
    shiny::observeEvent(input$metric_cpd, {
      show_metric_modal("Composite_Phase_Deviation")
    })
    shiny::observeEvent(input$metric_sri, {
      show_metric_modal("Sleep_Regularity_Index")
    })
    shiny::observeEvent(input$metric_chronotype, {
      show_metric_modal("Chronotype")
    })
  })
}

compliance_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("compliance_text")),
    shiny::tableOutput(ns("compliance_table")),
    shiny::downloadButton(
      outputId = ns("download_compliance"),
      label = NULL,
      class = "small-btn"
    )
  )
}

compliance_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {
    compliance_table <- shiny::reactive({
      shiny::req(common$sessions(), common$session_filters())
      validate_columns(common$sessions(), c("night"))
      get_compliance_table(common)
    })

    output$compliance_table <- shiny::renderTable({
      shiny::req(compliance_table())
      shiny::validate(
        shiny::need(nrow(compliance_table()) > 0, "There are no duplicate sessions on the same night.")
      )
      compliance_table()
    })

    output$compliance_text <- shiny::renderUI({
      shiny::req(compliance_table())
      if (nrow(compliance_table()) > 0) {
        shiny::HTML("<p>The compliance table below lists nights where multiple sessions have been recorded.</p>")
      }
    })

    shiny::observe({
      shiny::req(compliance_table())
      if (nrow(compliance_table()) > 0) {
        shinyjs::runjs("$('#main_tabs_tables li a[data-value=\"compliance_tab\"]').css('color', 'red');")
      } else {
        shinyjs::runjs("$('#main_tabs_tables li a[data-value=\"compliance_tab\"]').css('color', '');")
      }
    })

    log_shown <- shiny::reactiveVal(FALSE)
    shiny::observe({
      show_log_once(
        condition = nrow(compliance_table()) > 0,
        log_shown = log_shown,
        log_msg = "There are nights with multiple sessions (see compliance table).",
        log_type = "warning",
        logger = common$logger
      )
    })

    shiny::observe({
      shiny::req(common$sessions(), common$session_filters())
      sessions <- common$sessions()
      validate_columns(sessions, "night", message = FALSE)
      output_table <- sessions |>
        apply_filters(common$session_filters()) |>
        annotate(common$annotations()) |>
        get_non_complying_sessions()
      output$download_compliance <- get_table_download_handler(
        session = session,
        common = common,
        output_table = output_table,
        output_name = "compliance"
      )
    })
  })
}

#' @importFrom rlang .data
get_compliance_table <- function(common) {
  common$sessions() |>
    apply_filters(common$session_filters()) |>
    annotate(common$annotations()) |>
    get_non_complying_sessions() |>
    make_sessions_display_table()
}

make_sessions_display_table <- function(sessions) {
  sessions <- sessions |>
    dplyr::mutate(
      start = get_col(sessions, "session_start") |> format("%H:%M"),
      sleep_onset = get_col(sessions, "time_at_sleep_onset") |> format("%H:%M"),
      wakeup = get_col(sessions, "time_at_wakeup") |> format("%H:%M"),
      end = get_col(sessions, "session_end") |> format("%H:%M"),
      time_in_bed_h = if ("time_in_bed" %in% names(sessions)) sessions$time_in_bed / 60 / 60 else NA,
      night = get_col(sessions, "night") |> format("%Y-%m-%d"),
      time_asleep_h = if ("sleep_period" %in% names(sessions)) sessions$sleep_period / 60 / 60 else NA
    ) |>
    dplyr::select(
      dplyr::any_of(c(
        "id",
        "subject_id",
        "night",
        "start",
        "sleep_onset",
        "wakeup",
        "end",
        "time_in_bed_h",
        "time_asleep_h",
        "annotation"
      ))
    )
}

filtering_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::HTML("Nights that have more than one session will be displayed in the
    <b>Compliance</b> tab (tab will be red if it contains any sessions)."),
    shiny::br(), shiny::br(),
    shiny::HTML("Sessions that have been filtered out will be displayed in the <b>Filtering</b> tab."),
    shiny::hr(),
    bslib::accordion(
      bslib::accordion_panel(
        "Date",
        shiny::uiOutput(ns("date_range_slider"))
      ),
      bslib::accordion_panel(
        "Subject",
        shiny::uiOutput(ns("subject_select")),
        shiny::uiOutput(ns("sex_select")),
        shiny::uiOutput(ns("age_range_slider"))
      ),
      bslib::accordion_panel(
        "Sleep",
        shiny::uiOutput(ns("sleep_onset_range")),
        shiny::uiOutput(ns("time_in_bed_slider")),
        shiny::uiOutput(ns("sleep_period_slider"))
      ),
      open = NULL
    )
  )
}

filtering_tab <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("removed_sessions_text")),
    shinyWidgets::pickerInput(
      inputId = ns("filters_columns"),
      label = "Filters to display:",
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `live-search` = FALSE
      )
    ),
    shiny::tableOutput(ns("removed_sessions")),
    shiny::downloadButton(
      outputId = ns("download_removed_sessions"),
      label = NULL,
      class = "small-btn"
    )
  )
}

filtering_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observe({
      shiny::req(common$session_filters())
      filters <- common$session_filters()
      filter_names <- setdiff(colnames(filters), c("no_sleep"))
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "filters_columns",
        choices = filter_names,
        selected = filter_names
      )
    })

    output$date_range_slider <- shiny::renderUI({
      shiny::req(common$sessions())
      col <- get_colnames(common$sessions())
      if (!is.null(col$night) && nrow(common$sessions()) > 0) {
        min_date <- min(common$sessions()[[col$night]], na.rm = TRUE)
        max_date <- max(common$sessions()[[col$night]], na.rm = TRUE)
        shiny::sliderInput(
          inputId = session$ns("date_range"),
          label = "Date Range:",
          min = min_date,
          max = max_date,
          value = c(min_date, max_date),
          timeFormat = "%Y-%m-%d",
          ticks = FALSE
        )
      }
    })

    output$subject_select <- shiny::renderUI({
      shiny::req(common$sessions())
      col <- get_colnames(common$sessions())
      if (!is.null(col$subject_id)) {
        subject_choices <- unique(common$sessions()[[col$subject_id]])
        shinyWidgets::pickerInput(
          inputId = session$ns("subject_filter"),
          label = "Subjects:",
          choices = subject_choices,
          selected = subject_choices,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = FALSE
          )
        )
      }
    })

    output$age_range_slider <- shiny::renderUI({
      shiny::req(common$sessions())
      col <- get_colnames(common$sessions())
      if (!is.null(col$birth_year)) {
        birth_years <- common$sessions()[[col$birth_year]]
        birth_years <- birth_years[!is.na(birth_years)]
        if (length(birth_years) > 0) {
          min_age <- min(lubridate::year(common$sessions()[[col$night]]) - birth_years)
          max_age <- max(lubridate::year(common$sessions()[[col$night]]) - birth_years)
          shiny::sliderInput(
            session$ns("age_range"),
            "Age Range:",
            min = min_age,
            max = max_age,
            value = c(min_age, max_age),
            step = 1
          )
        }
      }
    })

    output$sex_select <- shiny::renderUI({
      shiny::req(common$sessions())
      col <- get_colnames(common$sessions())
      if (!is.null(col$sex)) {
        sex_choices <- unique(common$sessions()[[col$sex]])
        shinyWidgets::pickerInput(
          inputId = session$ns("sex_filter"),
          label = "Sex:",
          choices = sex_choices,
          selected = sex_choices,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = FALSE
          )
        )
      }
    })

    output$time_in_bed_slider <- shiny::renderUI({
      shiny::req(common$sessions())
      col <- get_colnames(common$sessions())
      if (!is.null(col$time_in_bed)) {
        shiny::sliderInput(
          inputId = session$ns("time_in_bed"),
          label = "Minimum Time in Bed:",
          min = 0,
          max = 12,
          value = 0,
          step = 0.5,
          post = "h",
          ticks = FALSE
        )
      }
    })

    output$sleep_period_slider <- shiny::renderUI({
      shiny::req(common$sessions())
      col <- get_colnames(common$sessions())
      if (!is.null(col$sleep_period)) {
        shiny::sliderInput(
          inputId = session$ns("sleep_period"),
          label = "Minimum Time Asleep:",
          min = 0,
          max = 12,
          value = 0,
          step = 0.5,
          post = "h",
          ticks = FALSE
        )
      }
    })

    output$sleep_onset_range <- shiny::renderUI({
      shiny::req(common$sessions())
      col <- get_colnames(common$sessions())
      if (!is.null(col$time_at_sleep_onset)) {
        shinyWidgets::sliderTextInput(
          inputId = session$ns("time_range"),
          label = "Sleep Onset Time:",
          choices = sprintf("%02d", c(13:23, 0:12)),
          selected = c("13", "12"),
          grid = TRUE
        )
      }
    })

    shiny::observe({
      shiny::req(common$sessions(), common$session_filters())
      col <- get_colnames(common$sessions())
      filters <- common$session_filters()

      from_time <- if (!is.null(input$time_range[1])) paste0(input$time_range[1], ":00") else NULL
      to_time <- if (!is.null(input$time_range[2])) paste0(input$time_range[2], ":00") else NULL

      if (!is.null(col$sleep_period)) {
        filters$no_sleep <- common$sessions() |>
          remove_sessions_no_sleep(return_mask = TRUE)
      }
      if (!is.null(col$sleep_period) && !is.null(input$sleep_period)) {
        filters$time_asleep <- common$sessions() |>
          set_min_sleep_period(input$sleep_period, return_mask = TRUE)
      }
      if (!is.null(col$night) && !is.null(input$date_range)) {
        filters$night <- common$sessions() |>
          filter_by_night_range(input$date_range[1], input$date_range[2], return_mask = TRUE)
      }
      if (!is.null(col$time_at_sleep_onset)) {
        filters$sleep_onset <- common$sessions() |>
          set_session_sleep_onset_range(from_time, to_time, return_mask = TRUE)
      }
      if (!is.null(col$time_in_bed) && !is.null(input$time_in_bed)) {
        filters$time_in_bed <- common$sessions() |>
          set_min_time_in_bed(input$time_in_bed, return_mask = TRUE)
      }
      if (!is.null(col$birth_year) && !is.null(col$night) && !is.null(input$age_range)) {
        filters$age <- common$sessions() |>
          filter_by_age_range(input$age_range[1], input$age_range[2], return_mask = TRUE)
      }
      if (!is.null(col$sex) && !is.null(input$sex_filter)) {
        filters$sex <- common$sessions() |>
          filter_by_sex(input$sex_filter, return_mask = TRUE)
      }
      if (!is.null(col$subject_id) && !is.null(input$subject_filter)) {
        filters$subject_id <- common$sessions() |>
          select_subjects(input$subject_filter, return_mask = TRUE)
      }
      common$session_filters(filters)
    })

    log_shown <- shiny::reactiveVal(FALSE)
    shiny::observe({
      shiny::req(common$sessions())
      show_log_once(
        condition = nrow(apply_filters(common$sessions(), common$session_filters())) == 0,
        log_shown = log_shown,
        log_msg = "Sessions table is empty after filtering.",
        log_type = "warning",
        logger = common$logger
      )
    })

    removed_sessions <- shiny::reactive({
      shiny::req(common$sessions())
      col <- get_colnames(common$sessions())
      get_removed_sessions_table(common, input$filters_columns)
    })

    output$removed_sessions <- shiny::renderTable({
      shiny::req(removed_sessions())
      shiny::validate(
        shiny::need(nrow(removed_sessions()) > 0,
                    "No sessions have been removed by the displayed filters.")
      )
      removed_sessions()
    })

    output$removed_sessions_text <- shiny::renderUI({
      shiny::req(removed_sessions())
      if (nrow(removed_sessions()) > 0) {
        shiny::HTML(paste0(
          "<p>The filtering table below shows sessions that were removed by filtering.</p>",
          "<p>", nrow(removed_sessions()), " sessions have been removed.</p>"
        ))
      }
    })

    shiny::observe({
      shiny::req(common$sessions(), common$session_filters())
      output$download_removed_sessions <- get_table_download_handler(
        session = session,
        output_table = shiny::reactive(get_removed_rows(common$sessions(), common$session_filters())),
        output_name = "removed_sessions"
      )
    })

    shiny::observe({
      shiny::req(common$epochs(), common$sessions(), common$session_filters(), common$epoch_filters())
      filters <- common$epoch_filters()
      filters$from_sessions <- filter_epochs_from_sessions(
        epochs = common$epochs(),
        sessions = apply_filters(common$sessions(), common$session_filters()),
        return_mask = TRUE
      )
      common$epoch_filters(filters)
    })

  })
}

get_removed_sessions_table <- function(common, filter_list) {
  sessions <- common$sessions() |>
    annotate(common$annotations())
  filters <- common$session_filters()
  sessions <- sessions[filters$no_sleep == TRUE, , drop = FALSE]
  filters <- filters[filters$no_sleep == TRUE, , drop = FALSE] |>
    dplyr::select(dplyr::all_of(filter_list))
  display_table <- sessions |>
    get_removed_rows(filters) |>
    make_sessions_display_table()

  removed_idx <- which(!apply(filters, 1, all))
  filter_names <- colnames(filters)
  filters_applied <- apply(filters[removed_idx, , drop = FALSE], 1, function(row) {
    paste(filter_names[which(!row)], collapse = ", ")
  })

  display_table$filters <- filters_applied
  display_table
}

apply_filters <- function(df_in, filters) {
  df_in[apply(filters, 1, all), ]
}

get_removed_rows <- function(df_in, filters) {
  df_in[!apply(filters, 1, all), ]
}

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
    ),
    shiny::br(),
    shiny::div(
      class = "filter-sets-btn",
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::div(
            class = "filters-upload",
            shiny::fileInput(
              inputId = ns("upload_filters"),
              label = NULL,
              placeholder = NULL,
              buttonLabel = tagList(shiny::icon("upload"), "Import Filters"),
              accept = ".yaml"
            )
          )
        ),
        shiny::column(
          width = 6,
          class = "text-end",
          shiny::downloadButton(
            outputId = ns("download_filters"),
            label = "Export filters"
          )
        )
      )
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

    output$date_range_slider <- shiny::renderUI({
      shiny::req(common$sessions())
      sessions <- common$sessions()
      if ("night" %in% names(sessions) && nrow(sessions) > 0) {
        min_date <- min(sessions$night, na.rm = TRUE)
        max_date <- max(sessions$night, na.rm = TRUE)
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
    shiny::outputOptions(output, "date_range_slider", suspendWhenHidden = FALSE)

    output$subject_select <- shiny::renderUI({
      shiny::req(common$sessions())
      sessions <- common$sessions()
      if ("subject_id" %in% names(sessions)) {
        subject_choices <- unique(sessions$subject_id)
        shinyWidgets::pickerInput(
          inputId = session$ns("subject"),
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
    shiny::outputOptions(output, "subject_select", suspendWhenHidden = FALSE)

    output$age_range_slider <- shiny::renderUI({
      shiny::req(common$sessions())
      sessions <- common$sessions()
      if ("birth_year" %in% names(sessions)) {
        birth_years <- sessions$birth_year[!is.na(sessions$birth_years)]
        if (length(birth_years) > 0) {
          min_age <- min(lubridate::year(sessions$night) - birth_years)
          max_age <- max(lubridate::year(sessions$night) - birth_years)
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
    shiny::outputOptions(output, "age_range_slider",    suspendWhenHidden = FALSE)

    output$sex_select <- shiny::renderUI({
      shiny::req(common$sessions())
      sessions <- common$sessions()
      if ("sex" %in% names(sessions)) {
        sex_choices <- unique(sessions$sex)
        shinyWidgets::pickerInput(
          inputId = session$ns("sex"),
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
    shiny::outputOptions(output, "sex_select", suspendWhenHidden = FALSE)

    output$time_in_bed_slider <- shiny::renderUI({
      shiny::req(common$sessions())
      sessions <- common$sessions()
      if ("time_in_bed" %in% names(sessions)) {
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
    shiny::outputOptions(output, "time_in_bed_slider",  suspendWhenHidden = FALSE)

    output$sleep_period_slider <- shiny::renderUI({
      shiny::req(common$sessions())
      sessions <- common$sessions()
      if ("sleep_period" %in% names(sessions)) {
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
    shiny::outputOptions(output, "sleep_period_slider", suspendWhenHidden = FALSE)

    output$sleep_onset_range <- shiny::renderUI({
      shiny::req(common$sessions())
      sessions <- common$sessions()
      if ("time_at_sleep_onset" %in% names(sessions)) {
        shiny::sliderInput(
          inputId = session$ns("sleep_onset_range"),
          label = "Sleep Onset Time:",
          min   = as.POSIXct("2000-01-01 12:00:00"),
          max   = as.POSIXct("2000-01-02 12:00:00"),
          value = c(
            as.POSIXct("2000-01-01 12:00:00"),
            as.POSIXct("2000-01-02 12:00:00")
          ),
          step       = 3600,
          timeFormat = "%H:%M",
          ticks      = FALSE
        )
      }
    })
    shiny::outputOptions(output, "sleep_onset_range",   suspendWhenHidden = FALSE)

    shiny::observe({
      filter_values <- list(
        sleep_period = input$sleep_period,
        date_range = input$date_range,
        sleep_onset_range = input$sleep_onset_range |> format("%H:%M"),
        time_in_bed = input$time_in_bed,
        age_range = input$age_range,
        sex = input$sex,
        subject = input$subject
      )
      common$filter_values(filter_values)
    })

    shiny::observeEvent(common$filter_values(), {
      shiny::req(common$sessions(), common$session_filters())
      filters <- update_masks(common$sessions(), common$session_filters(), common$filter_values())
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

    shiny::observe({
      shiny::req(common$session_filters())
      filters <- common$session_filters()
      filter_names <- setdiff(names(filters), c("no_sleep"))
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "filters_columns",
        choices = filter_names,
        selected = filter_names
      )
    })

    removed_sessions <- shiny::reactive({
      shiny::req(common$sessions())
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

    # Download the current filter set ----
    shiny::observe({
      filter_values = common$filter_values()
      filter_values$date_range <- as.character(filter_values$date_range)
      filter_values$sleep_onset_range <- as.character(filter_values$sleep_onset_range)
      output$download_filters <- get_yaml_download_handler(
        export_list = filter_values,
        logger = common$logger,
        filename = "nocturn_filters",
        message = "Filter set exported"
      )
    })

    # Upload and apply a filter set from a yaml file ----
    shiny::observeEvent(input$upload_filters, {
      filter_values <- yaml::read_yaml(input$upload_filters$datapath)
      common$filter_values(filter_values)
      update_inputs(session, filter_values)
      common$logger |> write_log("Filter set imported", type = "complete")
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
  filter_names <- names(filters)
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

update_masks <- function(df, filters, filter_values) {
  if (is.null(filters)) filters <- data.frame(no_sleep = rep(TRUE, nrow(df)))

  if ("sleep_period" %in% names(df)) {
    filters$no_sleep <- df |>
      remove_sessions_no_sleep(return_mask = TRUE)
  }
  if ("sleep_period" %in% names(df) && !is.null(filter_values$sleep_period)) {
    filters$time_asleep <- df |>
      set_min_sleep_period(filter_values$sleep_period, return_mask = TRUE)
  }
  if ("night" %in% names(df) && !is.null(filter_values$date_range)) {
    filters$night <- df |>
      filter_by_night_range(filter_values$date_range[1], filter_values$date_range[2], return_mask = TRUE)
  }
  if ("time_at_sleep_onset" %in% names(df) && length(filter_values$sleep_onset_range) == 2) {
    filters$sleep_onset <- df |>
      set_session_sleep_onset_range(filter_values$sleep_onset_range[1], filter_values$sleep_onset_range[2], return_mask = TRUE)
  }
  if ("time_in_bed" %in% names(df) && !is.null(filter_values$time_in_bed)) {
    filters$time_in_bed <- df |>
      set_min_time_in_bed(filter_values$time_in_bed, return_mask = TRUE)
  }
  if (all(c("birth_year", "night") %in% names(df)) && !is.null(filter_values$age_range)) {
    filters$age <- df |>
      filter_by_age_range(filter_values$age_range[1], filter_values$age_range[2], return_mask = TRUE)
  }
  if ("sex" %in% names(df) && !is.null(filter_values$sex)) {
    filters$sex <- df |>
      filter_by_sex(filter_values$sex, return_mask = TRUE)
  }
  if ("subject_id" %in% names(df) && !is.null(filter_values$subject)) {
    filters$subject_id <- df |>
      select_subjects(filter_values$subject, return_mask = TRUE)
  }
  filters
}

update_inputs <- function(session, filter_values) {
  shiny::updateSliderInput(
    session = session,
    inputId = "sleep_period",
    value = filter_values$sleep_period
  )
  shiny::updateSliderInput(
    session = session,
    inputId = "time_in_bed",
    value = filter_values$time_in_bed
  )
  shiny::updateSliderInput(
    session = session,
    inputId = "age_range",
    value = filter_values$age_range
  )
  shinyWidgets::updatePickerInput(
    session = session,
    inputId = "subject",
    selected = filter_values$subject
  )
  shinyWidgets::updatePickerInput(
    session = session,
    inputId = "sex",
    selected = filter_values$sex
  )
  shiny::updateSliderInput(
    session = session,
    inputId = "date_range",
    value = parse_date(filter_values$date_range)
  )
  shiny::updateSliderInput(
    session = session,
    inputId = "sleep_onset_range",
    value = c(
      parse_time(filter_values$sleep_onset_range[1]) |> update_date("2000-01-01"),
      parse_time(filter_values$sleep_onset_range[2]) |> update_date("2000-01-02")
    ),
    timeFormat = "%H:%M"
  )
}

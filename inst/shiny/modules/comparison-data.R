comparison_data_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      shiny::h4("Add Sessions"),
      shiny::fileInput(
        inputId = ns("sessions_file"),
        label = NULL,
        multiple = TRUE,
        placeholder = "(multiple files accepted)",
        accept = c(".csv", ".xls", ".xlsx", ".edf", ".rec")
      ),
      class = "sidebar_card"
    ),
    bslib::card(
      shiny::h4("Loaded Sessions"),
      help_modal_ui(ns),
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::HTML("<b>Name</b>")
        ),
        shiny::column(
          width = 4,
          shiny::HTML("<b>Col names</b>")
        ),
        shiny::column(
          width = 4,
          shiny::HTML("<b>Clear</b>")
        ),
      ),
      shiny::uiOutput(ns("loaded_sessions")),
      class = "sidebar_card"
    )
  )
}

dataset_row_ui <- function(id, value = "") {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(4, shiny::textInput(ns("title"), label = NULL, value = value, updateOn = "blur")),
    shiny::column(4, shiny::actionButton(ns("set_cols"), "Set", width = "100%", icon = shiny::icon("columns"))),
    shiny::column(4, shiny::actionButton(ns("clear"), "Clear", width = "100%", icon = shiny::icon("trash"), class = "delete-btn"))
  )
}

comparison_data_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {

    secondary_sessions <- common$secondary_sessions
    started <- shiny::reactiveVal(character())

    # Add/clear main dataset if available
    shiny::observeEvent(common$sessions(), {
      df <- common$sessions()
      x <- shiny::isolate(secondary_sessions())

      # Clear "main" if common$sessions was cleared
      if (is.null(df)) {
        if (!is.null(x[["main"]])) {
          x[["main"]] <- NULL
          secondary_sessions(x)
          common$session_filters(list())
          common$filter_values(list())
        }
        return()
      }

      x[["main"]] <- list(data = df, title = "main", filters = common$session_filters())

      secondary_sessions(x)
    }, ignoreInit = TRUE)

    # New file upload ----
    shiny::observeEvent(input$sessions_file, {
      shiny::req(input$sessions_file)
      if (nrow(input$sessions_file) == 1) {
        data <- load_sessions(input$sessions_file$datapath)
        common$logger |> write_log(paste0("Loaded secondary session file: ", input$sessions_file$name), type = "complete")
      } else if (nrow(input$sessions_file) > 1) {
        data <- load_batch(file_list = input$sessions_file$datapath, type = "sessions")
        common$logger |> write_log(paste0("Loaded ", nrow(input$sessions_file), " session files"), type = "complete")
      } else {
        return()
      }
      id <- id_gen()
      x <- secondary_sessions()
      filter_values <- common$filter_values()
      filter_values$subject <- NULL
      filter_values$date_range <- NULL

      x[[id]] <- list(
        data = data,
        raw_data = data,
        title = input$sessions_file$name,
        filters = update_masks(
          df = data,
          filters = NULL,
          filter_values = filter_values
        )
      )
      secondary_sessions(x)
    })

    # Keep filters up-to-date ----
    shiny::observeEvent(common$filter_values(), {
      x <- secondary_sessions()
      ids <- names(x)
      filter_values <- common$filter_values()
      filter_values$subject <- NULL
      filter_values$date_range <- NULL

      for (id in ids) {
        x[[id]]$filters <- update_masks(
          df = x[[id]]$data,
          filters = x[[id]]$filters,
          filter_values = filter_values
        )
      }
      secondary_sessions(x)
    }, ignoreInit = TRUE)

    # Dynamic rows UI ----
    output$loaded_sessions <- shiny::renderUI({
      x <- secondary_sessions()
      keys <- names(x)
      if (!length(keys)) return(shiny::div("No sessions loaded yet."))

      shiny::tagList(lapply(keys, function(key) {
        dataset_row_ui(session$ns(paste0("row__", key)), value = x[[key]]$title)
      }))
    })

    # Register observers for each row ----
    shiny::observe({
      x <- secondary_sessions()
      keys <- names(x)
      new_keys <- setdiff(keys, started())

      for (key in new_keys) {
        row_id <- paste0("row__", key)
        dataset_row_server(
          id = row_id,
          key = key,
          secondary_sessions = secondary_sessions,
          common = common
        )
      }

      started(c(started(), new_keys))
    })

    shiny::observeEvent(input$help, {
      show_help_modal("Secondary_datasets")
    })
  })
}

dataset_row_server <- function(id, key, secondary_sessions, common) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update title
    shiny::observeEvent(input$title, {
      x <- secondary_sessions()
      if (is.null(x[[key]])) return()
      x[[key]]$title <- input$title
      secondary_sessions(x)
    }, ignoreInit = TRUE)

    # Set colnames
    register_colnames_modal(
      input = input, session = session, ns = ns, common = common,
      open_event = "set_cols",
      get_df = \() secondary_sessions()[[key]]$data,
      get_raw = \() secondary_sessions()[[key]]$raw_data,
      set_df = \(df) {
        x <- secondary_sessions()
        x[[key]]$data <- df
        secondary_sessions(x)
      }
    )

    # Clear
    shiny::observeEvent(input$clear, {
      x <- secondary_sessions()
      title <- x[[key]]$title
      x[[key]] <- NULL
      secondary_sessions(x)
      common$logger |> write_log(paste0("Removed sessions ", title), type = "complete")
    })
  })
}

id_gen <- function() paste0(sample(letters, 6, replace = TRUE), collapse = "")

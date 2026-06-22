input_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    input_sessions_ui(ns("sessions_input_panel")),
    input_epochs_ui(ns("epochs_input_panel")),
    shiny::actionButton(ns("load_example_data"), "Load Example Data", icon = shiny::icon("upload")),
    bslib::tooltip(
      shiny::tags$span(
        shiny::icon("circle-info"),
        class = "colnames-help"
      ),
      "After loading the data, click through the tabs on the right to see the different plots and tables!",
      placement = "right",
      options = list(delay = list(show = 0, hide = 100))
    )
  )
}

input_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {

    # Example data----
    shiny::observeEvent(input$load_example_data, {
      sessions <- nocturn::example_sessions |>
        set_data_type("sessions") |>
        dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., ""))) |>
        clean_sessions()
      epochs <- nocturn::example_epochs |>
        set_data_type("epochs") |>
        dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., ""))) |>
        clean_epochs()
      init_sessions(sessions, common)
      init_epochs(epochs, common)
      common$logger |> write_log("Loaded example session and epoch data", type = "complete")
    })

    # Sessions ----
    input_sessions_server("sessions_input_panel", common)


    # Epochs ----
    input_epochs_server("epochs_input_panel", common)

  })
}

show_colnames_modal <- function(
  ns,
  colnames_list,
  current_map,
  type = "Sessions",
  save_id = "save_col_names",
  reset_id = "reset_col_names"
) {
  if (type == "Sessions") {
    title <- "Set Session Column Names"
    long_names <- .sessions_long
    help_tips <- .sessions_help
  } else if (type == "Epochs") {
    title <- "Set Epoch Column Names"
    long_names <- .epochs_long
    help_tips <- .epochs_help
  }
  inputs <- lapply(names(long_names), function(key) {
    current_value <- as.character(current_map[[key]])
    if (is.null(current_map[[key]]) || is.na(current_map[[key]]) || current_map[[key]] == "-") current_value <- "-"
    choices <- c("-", colnames_list)
    label_text <- long_names[[key]] %||% key
    help_text <- help_tips[[key]] %||% NULL
    label <- shiny::tagList(
      label_text,
      if (!is.null(help_text)) bslib::tooltip(
        shiny::tags$span(
          shiny::icon("circle-info"),
          class = "colnames-help"
        ),
        help_text,
        placement = "right",
        options = list(delay = list(show = 0, hide = 100))
      )
    )
    shiny::selectInput(
      inputId = ns(paste0("col_", key)),
      label = label,
      choices = choices,
      selected = current_value
    )
  })
  shiny::showModal(
    shiny::modalDialog(
      title = title,
      size = "l",
      easyClose = TRUE,
      footer = shiny::tagList(
        shiny::actionButton(ns(reset_id), "Reset", class = "delete-btn"),
        shiny::modalButton("Cancel"),
        shiny::actionButton(ns(save_id), "Save")
      ),
      shiny::p("Hint: type in the boxes to search for column names."),
      do.call(shiny::tagList, inputs)
    )
  )
}

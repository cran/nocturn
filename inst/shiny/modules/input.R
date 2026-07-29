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

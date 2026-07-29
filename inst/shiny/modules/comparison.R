comparison_side_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    comparison_data_ui(ns("comparison_data"))
  )
}

comparison_main_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    comparison_tables_ui(ns("comparison_tables")),
    bslib::navset_card_tab(
      id = "comparison_tabs_plots",
      bslib::nav_panel("Timeseries", timeseries_comparison_ui(ns("timeseries_comparison"))),
      bslib::nav_panel("Bland-Altman", bland_altman_ui(ns("bland_altman")))
    )
  )
}

comparison_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {

    # Side panel: data loading
    comparison_data_server("comparison_data", common)

    # Top main panel: summary data tables
    comparison_tables_server("comparison_tables", common)

    # Bottom main panel: plots
    timeseries_comparison_server("timeseries_comparison", common)
    bland_altman_server("bland_altman", common)
  })
}

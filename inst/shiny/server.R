function(input, output, session) {

  # Init log window
  init_log_msg <- function() {
    intro <- paste0("Welcome to nocturn ", utils::packageVersion("nocturn"))
    brk <- paste(rep("------", 5), collapse = "")
    expl <- "Please find messages for the user in this log window."
    log_init <- gsub(".{4}$", "", paste(intro, brk, expl, brk, "", sep = "<br>"))
    log_init
  }
  common$logger <- reactiveVal(init_log_msg())

  # Write out logs to the log Window
  observeEvent(common$logger(), {
    shinyjs::html(id = "logHeader", html = common$logger(), add = FALSE)
    shinyjs::js$scrollLogger()
  })

  # Footer text
  output$footer_text <- renderText({
    paste0(
      "nocturn version ", utils::packageVersion("nocturn"), ". ",
      "Developed at the University of Edinburgh as part of the Ambient-BD project."
    )
  })

  observeEvent(input$sidebar_toggle_btn, {
    bslib::sidebar_toggle("sidebar")
  })

  # Data loading module
  input_server("input", common)

  # Filtering module
  filtering_server("filtering", common)

  # Annotation module
  annotation_server("annotation", common)

  # Compliance module
  compliance_server("compliance", common)

  # Summary table module
  summary_server("summary", common)

  # Sleep regularity module
  sleep_regularity_server("sleep_regularity", common)

  # Export data module
  export_data_server("export_data", common)

  # Plotting modules
  sleep_clock_server("sleep_clock", common)
  sleep_spiral_server("sleep_spiral", common)
  bedtimes_waketimes_server("bedtimes_waketimes", common)
  sleep_distributions_server("sleep_distributions", common)
  sleep_bubbles_server("sleep_bubbles", common)
  hypnogram_server("hypnogram", common)
  timeseries_sessions_server("timeseries_sessions", common)
  timeseries_server("timeseries", common)

}

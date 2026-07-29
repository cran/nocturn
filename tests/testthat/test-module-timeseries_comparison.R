test_that("timeseries_comparison module works", {
  shiny::testServer(
    timeseries_comparison_server,
    args = list(common = make_common()),
    {
      session$flushReact()

      session$setInputs(
        download_format        = "png",
        common_nights_only     = FALSE,
        `row__abcdef-variable` = "time_at_sleep_onset",
        `row__abcdef-display`  = TRUE,
        `row__ghijkl-variable` = "time_at_sleep_onset",
        `row__ghijkl-display`  = TRUE
      )

      expect_no_error(output$timeseries_comparison_plot)
    }
  )
})

test_that("timeseries_sessions module works", {
  shiny::testServer(
    timeseries_sessions_server,
    args = list(common = make_common()),
    {
      session$setInputs(download_format = "png",
                        variable = "session_start",
                        exclude_zero = TRUE,
                        colorby = "default")

      expect_no_error(output$timeseries_sessions_plot)
    }
  )
})

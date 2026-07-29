test_that("timeseries module works", {
  common <- make_common()

  shiny::testServer(
    timeseries_server,
    args = list(common = common),
    {
      session$setInputs(download_format = "png",
                        variable = "light_ambient_mean",
                        exclude_zero = TRUE,
                        colorby = "default")

      expect_no_error(output$timeseries_plot)
    }
  )
})

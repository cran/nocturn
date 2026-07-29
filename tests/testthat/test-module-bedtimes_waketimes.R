test_that("bedtimes_waketimes module works", {
  shiny::testServer(
    bedtimes_waketimes_server,
    args = list(common = make_common()),
    {
      session$setInputs(download_format = "png",
                        groupby = "weekday",
                        colorby = "default")

      expect_no_error(output$bedtimes_waketimes_plot)
    }
  )
})

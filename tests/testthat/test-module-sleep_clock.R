test_that("sleep_clock module works", {
  shiny::testServer(
    sleep_clock_server,
    args = list(common = make_common()),
    {
      session$setInputs(download_format = "png",
                        colorby = "default")

      expect_no_error(output$sleep_clock_plot)
    }
  )
})

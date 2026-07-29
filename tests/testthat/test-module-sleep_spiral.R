test_that("sleep_spiral module works", {
  shiny::testServer(
    sleep_spiral_server,
    args = list(common = make_common()),
    {
      session$setInputs(download_format = "png",
                        colorby = "default")

      expect_no_error(output$sleep_spiral_plot)
    }
  )
})

test_that("sleep_bubbles module works", {
  shiny::testServer(
    sleep_bubbles_server,
    args = list(common = make_common()),
    {
      session$setInputs(download_format = "png",
                        colorby = "default")

      expect_no_error(output$sleep_bubbles_plot)
    }
  )
})

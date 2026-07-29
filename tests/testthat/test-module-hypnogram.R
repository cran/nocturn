test_that("hypnogram module works", {
  shiny::testServer(
    hypnogram_server,
    args = list(common = make_common()),
    {
      session$setInputs(download_format = "png")

      expect_no_error(output$hypnogram_plot)
    }
  )
})

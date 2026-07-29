test_that("bland-altman module works", {
  shiny::testServer(
    bland_altman_server,
    args = list(common = make_common()),
    {
      session$setInputs(download_format = "png",
                        sessions1 = "abcdef",
                        sessions2 = "ghijkl",
                        variable1 = "time_at_sleep_onset",
                        variable2 = "time_at_sleep_onset")

      expect_no_error(output$bland_altman_plot)
    }
  )
})

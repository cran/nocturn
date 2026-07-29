test_that("sleep distribution module works", {
  shiny::testServer(
    sleep_distributions_server,
    args = list(common = make_common()),
    {
      session$setInputs(download_format = "png",
                        plot_type = "Boxplot",
                        circular = TRUE)

      expect_no_error(output$sleep_distribution_plot)
    }
  )
})

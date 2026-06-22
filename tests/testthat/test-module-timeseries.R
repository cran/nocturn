common <- list(
  epochs = shiny::reactiveVal(example_epochs),
  epoch_filters = shiny::reactiveVal(data.frame(from_sessions = rep(TRUE, nrow(example_epochs))))
)

test_that("timeseries module works", {
  shiny::testServer(
    timeseries_server,
    args = list(common = common),
    {
      plot <- session$getReturned()
      session$setInputs(download_format = "png",
                        variable = "light_ambient_mean",
                        exclude_zero = TRUE)

      expect_s3_class(plot, "shiny.render.function")
    }
  )
})

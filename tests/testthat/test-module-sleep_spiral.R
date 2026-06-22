common <- list(
  epochs = shiny::reactiveVal(example_epochs),
  epoch_filters = shiny::reactiveVal(data.frame(from_sessions = rep(TRUE, nrow(example_epochs))))
)

test_that("sleep_spiral module works", {
  shiny::testServer(
    sleep_spiral_server,
    args = list(common = common),
    {
      plot <- session$getReturned()
      session$setInputs(download_format = "png")

      expect_s3_class(plot, "shiny.render.function")
    }
  )
})

common <- list(
  sessions = shiny::reactiveVal(example_sessions),
  session_filters = shiny::reactiveVal(data.frame(no_sleep = rep(TRUE, nrow(example_sessions))))
)

test_that("sleep_clock module works", {
  shiny::testServer(
    sleep_clock_server,
    args = list(common = common),
    {
      plot <- session$getReturned()
      session$setInputs(download_format = "png")

      expect_s3_class(plot, "shiny.render.function")
    }
  )
})

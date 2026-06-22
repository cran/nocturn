common <- list(
  sessions = shiny::reactiveVal(example_sessions),
  session_filters = shiny::reactiveVal(data.frame(no_sleep = rep(TRUE, nrow(example_sessions))))
)

test_that("timeseries_sessions module works", {
  shiny::testServer(
    timeseries_sessions_server,
    args = list(common = common),
    {
      plot <- session$getReturned()
      session$setInputs(download_format = "png",
                        variable = "session_start",
                        exclude_zero = TRUE)

      expect_s3_class(plot, "shiny.render.function")
    }
  )
})

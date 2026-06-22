common <- list(
  sessions = shiny::reactiveVal(example_sessions),
  session_filters = shiny::reactiveVal(data.frame(no_sleep = rep(TRUE, nrow(example_sessions))))
)

test_that("bedtimes_waketimes module works", {
  shiny::testServer(
    bedtimes_waketimes_server,
    args = list(common = common),
    {
      plot <- session$getReturned()
      session$setInputs(download_format = "png")
      session$setInputs(groupby = "weekday")

      expect_s3_class(plot, "shiny.render.function")
    }
  )
})

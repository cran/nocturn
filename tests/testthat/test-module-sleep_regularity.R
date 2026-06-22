common <- list(
  sessions = shiny::reactiveVal(example_sessions),
  epochs = shiny::reactiveVal(example_epochs),
  session_filters = shiny::reactiveVal(data.frame(no_sleep = rep(TRUE, nrow(example_sessions)))),
  epoch_filters = shiny::reactiveVal(data.frame(from_sessions = rep(TRUE, nrow(example_epochs))))
)


test_that("sleep_regularity module works", {
  expect_error(
    shiny::testServer(
      sleep_regularity_server,
      args = list(common = common),
      {
        session$flushReact()
      }
    ),
    NA
  )
})

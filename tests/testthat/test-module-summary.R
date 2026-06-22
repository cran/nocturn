common <- list(
  sessions = shiny::reactiveVal(example_sessions),
  epochs = shiny::reactiveVal(example_epochs),
  session_filters = shiny::reactiveVal(data.frame(no_sleep = rep(TRUE, nrow(example_sessions)))),
  epoch_filters = shiny::reactiveVal(data.frame(from_sessions = rep(TRUE, nrow(example_epochs)))),
  annotations = shiny::reactiveVal(data.frame(id = example_sessions$id, annotation = "", stringsAsFactors = FALSE))
)

test_that("summary module works", {
  shiny::testServer(
    summary_server,
    args = list(common = common),
    {
      session$flushReact()
      expect_equal(
        sessions_summary_table(),
        get_sessions_summary(common$sessions() |> annotate(common$annotations()))
      )
      expect_equal(
        epochs_summary_table(),
        get_epochs_summary(common$epochs())
      )
    }
  )
})

common <- make_common()

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

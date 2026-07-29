common <- make_common()

test_that("summary table gets created", {
  shiny::testServer(
    comparison_tables_server,
    args = list(common = common),
    {
      session$flushReact()
      expect_equal(
        sessions_summary_table(),
        get_comparison_summary_table(common$secondary_sessions())
      )
    }
  )
})

test_that("sleep table gets created", {
  shiny::testServer(
    comparison_tables_server,
    args = list(common = common),
    {
      session$flushReact()
      expect_equal(
        sessions_sleep_table(),
        get_comparison_sleep_table(common$secondary_sessions())
      )
    }
  )
})

test_that("get_comparison_summary_table output is correct", {
  res <- shiny::isolate(get_comparison_summary_table(common$secondary_sessions()))

  expect_true("data.frame" %in% class(res))
  expect_equal(nrow(res), 2)
  expect_equal(res$earliest_night, c("2025-04-03", "2025-04-03"))
  expect_equal(res$latest_night, c("2025-04-17", "2025-04-17"))
})

test_that("get_comparison_sleep_table output is correct", {
  res <- shiny::isolate(get_comparison_sleep_table(common$secondary_sessions()))

  expect_true("data.frame" %in% class(res))
  expect_equal(res$mean_sleep_onset, c("22:35", "22:35"))
  expect_equal(res$mean_wakeup_time, c("07:29", "07:29"))
})

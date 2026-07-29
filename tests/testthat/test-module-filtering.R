common <- make_common()

test_that("filtering module works", {
  shiny::testServer(
    filtering_server,
    args = list(common = common),
    {
      session$setInputs(
        date_range = c("2025-04-03", "2025-04-17"),
        sleep_onset_range = c(as.POSIXct("2000-01-01 20:00:00"), as.POSIXct("2000-01-02 06:00:00")),
        time_in_bed = 2
      )
      session$flushReact()
      shiny::isolate({

        expected_values <- list(
          sleep_period = NULL,
          date_range = c("2025-04-03", "2025-04-17"),
          sleep_onset_range = c("20:00", "06:00"),
          time_in_bed = 2,
          age_range = NULL,
          sex = NULL,
          subject = NULL
        )
        expect_equal(common$filter_values(), expected_values)

        expected_filters <- data.frame(no_sleep = rep(TRUE, nrow(example_sessions)))
        expected_filters$no_sleep <- remove_sessions_no_sleep(common$sessions(), return_mask = TRUE)
        expected_filters$night <- filter_by_night_range(common$sessions(), "2025-04-03", "2025-04-17", return_mask = TRUE)
        expected_filters$sleep_onset <- set_session_sleep_onset_range(common$sessions(), "20:00", "06:00", return_mask = TRUE)
        expected_filters$time_in_bed <- set_min_time_in_bed(common$sessions(), 2, return_mask = TRUE)
        expect_equal(common$session_filters(), expected_filters)
      })
    }
  )
})

test_that("get_removed_sessions_table output is correct", {
  shiny::isolate({
    filters <- data.frame(no_sleep = rep(TRUE, nrow(example_sessions)))
    filters$no_sleep <- remove_sessions_no_sleep(common$sessions(), return_mask = TRUE)
    filters$night <- filter_by_night_range(common$sessions(), "2025-04-03", "2025-04-17", return_mask = TRUE)
    filters$sleep_onset <- set_session_sleep_onset_range(common$sessions(), "20:00", "06:00", return_mask = TRUE)
    filters$time_in_bed <- set_min_time_in_bed(common$sessions(), 2, return_mask = TRUE)
    common$session_filters(filters)

    result <- get_removed_sessions_table(common, filter_list = c("no_sleep", "night", "time_in_bed", "sleep_onset"))
  })

  expect_equal(class(result), "data.frame")
  expect_equal(nrow(result), 3)
})

test_that("apply_filters returns only rows where all filters are TRUE", {
  df <- data.frame(
    id = 1:5,
    value = c(10, 20, 30, 40, 50)
  )
  filters <- data.frame(
    filter1 = c(TRUE, TRUE, FALSE, TRUE, FALSE),
    filter2 = c(TRUE, FALSE, FALSE, TRUE, TRUE)
  )

  filtered <- apply_filters(df, filters)
  # Only row 1 and 4 have all TRUE
  expect_equal(filtered$id, c(1, 4))
  expect_equal(nrow(filtered), 2)
})

test_that("get_removed_rows returns only rows where any filter is FALSE", {
  df <- data.frame(
    id = 1:5,
    value = c(10, 20, 30, 40, 50)
  )
  filters <- data.frame(
    filter1 = c(TRUE, TRUE, FALSE, TRUE, FALSE),
    filter2 = c(TRUE, FALSE, FALSE, TRUE, TRUE)
  )

  removed <- get_removed_rows(df, filters)
  # Rows 2, 3, and 5 have at least one FALSE
  expect_equal(removed$id, c(2, 3, 5))
  expect_equal(nrow(removed), 3)
})

sessions <- data.frame(
  calendar_date = as.Date("2023-01-01"),
  start_end_window = "22:00-07:00",
  time_at_sleep_onset = "22:30",
  time_at_wakeup = "06:45",
  birth_year = "1990",
  stringsAsFactors = FALSE
)

epochs <- data.frame(
  timenum = 1672531200, # 2023-01-01 00:00:00
  class_id = c(0, 1, 0, 1),
  stringsAsFactors = FALSE
) |>
  set_colnames(list(timestamp = "timenum", "sleep_stage" = "class_id"))

test_that("clean_sessions creates and parses columns correctly", {
  cleaned <- clean_sessions(sessions)

  # Check id created
  expect_true("id" %in% names(cleaned))
  # Check session_start and session_end parsed
  expect_true("session_start" %in% names(cleaned))
  expect_true("session_end" %in% names(cleaned))
  expect_true(inherits(cleaned$session_start, "POSIXct"))
  expect_true(inherits(cleaned$session_end, "POSIXct"))
  # Check time_at_sleep_onset and time_at_wakeup parsed
  expect_true(inherits(cleaned$time_at_sleep_onset, "POSIXct"))
  expect_true(inherits(cleaned$time_at_wakeup, "POSIXct"))
  # Check birth_year is numeric
  expect_true(is.numeric(cleaned$birth_year))
  # Check time_in_bed and sleep_period created
  expect_true("time_in_bed" %in% names(cleaned))
  expect_true("sleep_period" %in% names(cleaned))
  expect_true(is.numeric(cleaned$time_in_bed))
  expect_true(is.numeric(cleaned$sleep_period))
  # Check time_at_midsleep created
  expect_true("time_at_midsleep" %in% names(cleaned))
  expect_true(inherits(cleaned$time_at_midsleep, "POSIXct"))
})

test_that("Double-cleaning sessions is idempotent", {
  cleaned_once <- clean_sessions(sessions)
  cleaned_twice <- clean_sessions(cleaned_once)

  expect_equal(cleaned_once, cleaned_twice)
})

test_that("clean_epochs creates and parses columns correctly", {
  cleaned <- clean_epochs(epochs)

  # Check timestamp parsed to POSIXct
  expect_true("timestamp" %in% names(cleaned))
  expect_true(inherits(cleaned$timestamp, "POSIXct"))
  # Check is_asleep column created
  expect_true("is_asleep" %in% names(cleaned))
  expect_equal(cleaned$is_asleep, c(TRUE, FALSE, TRUE, FALSE))
  # Check night column created
  expect_true("night" %in% names(cleaned))
  expect_true(inherits(cleaned$night, "Date"))
})

test_that("Double-cleaning epochs is idempotent", {
  cleaned_once <- clean_epochs(epochs)
  cleaned_twice <- clean_epochs(cleaned_once)

  expect_equal(cleaned_once, cleaned_twice)
})

testthat::test_that("apply_rules applies rules once, adds produced columns, and updates col mapping", {
  df <- sessions |>
    set_colnames(list(night = "calendar_date"))
  attr(df, "type") <- "sessions"

  rules <- list(
    list(
      requires = NULL,
      produces = "night2",
      apply = function(df) {
        df$night2 <- df$calendar_date
        df
      }
    ),
    list(
      requires = NULL,
      produces = "time_in_bed",
      apply = function(df) {
        df$time_in_bed <- 9 * 60 * 60
        df
      }
    )
  )

  out <- apply_rules(df, rules)

  expect_true(all(c("night2", "time_in_bed") %in% names(out)))

  # Values are as expected
  expect_identical(out$night, sessions$calendar_date)
  expect_identical(out$time_in_bed, rep(9 * 60 * 60, nrow(out)))

  # Mapping attribute updated for produced columns
  out_col <- get_colnames(out)
  expect_identical(out_col$night, "calendar_date")
  expect_identical(out_col$time_in_bed, "time_in_bed")

  # Running again should be idempotent
  out2 <- apply_rules(out, rules)
  testthat::expect_identical(out2, out)
})


testthat::test_that("apply_rules does not run rule if requires missing", {

  df <- data.frame(calendar_date = as.Date("2023-01-01"))

  rules <- list(
    list(
      requires = c("this_column_does_not_exist"),
      produces = "derived",
      apply = function(d) {
        d$derived <- 1
        d
      }
    )
  )

  out <- apply_rules(df, rules)

  testthat::expect_false("derived" %in% names(out))
  testthat::expect_true(is.null(attr(out, "col")$derived))
})

testthat::test_that("standardise_types works as expected", {
  df <- data.frame(
    date_col = c("2026-04-04", "2026-10-28"),
    num_col = c(0, 36),
    str_col = c("CT", "LB")
  )
  parsers = list(
    date_col = parse_date,
    num_col = as.numeric
  )

  res <- standardise_types(df, parsers)
  expect_true(lubridate::is.Date(res$date_col))
  expect_true(is.numeric(res$num_col))
  expect_true(is.character(res$str_col))
})

sessions <- data.frame(
  calendar_date = as.Date("2023-01-01"),
  start_end_window = "22:00-07:00",
  time_at_sleep_onset = "22:30",
  time_at_wakeup = "06:45",
  birth_year = "1990",
  stringsAsFactors = FALSE
)

epochs <- data.frame(
  filename = "sessionA.edf",
  timenum = 1672531200, # 2023-01-01 00:00:00
  class_id = c(0, 1, 0, 1),
  stringsAsFactors = FALSE
)

test_that("clean_sessions creates and parses columns correctly", {
  cleaned <- clean_sessions(sessions)

  # Check session_id created
  expect_true("session_id" %in% colnames(cleaned))
  # Check session_start and session_end parsed
  expect_true("session_start" %in% colnames(cleaned))
  expect_true("session_end" %in% colnames(cleaned))
  expect_true(inherits(cleaned$session_start, "POSIXct"))
  expect_true(inherits(cleaned$session_end, "POSIXct"))
  # Check time_at_sleep_onset and time_at_wakeup parsed
  expect_true(inherits(cleaned$time_at_sleep_onset, "POSIXct"))
  expect_true(inherits(cleaned$time_at_wakeup, "POSIXct"))
  # Check birth_year is numeric
  expect_true(is.numeric(cleaned$birth_year))
  # Check time_in_bed and sleep_period created
  expect_true("time_in_bed" %in% colnames(cleaned))
  expect_true("sleep_period" %in% colnames(cleaned))
  expect_true(is.numeric(cleaned$time_in_bed))
  expect_true(is.numeric(cleaned$sleep_period))
  # Check time_at_midsleep created
  expect_true("time_at_midsleep" %in% colnames(cleaned))
  expect_true(inherits(cleaned$time_at_midsleep, "POSIXct"))
})

test_that("Double-cleaning sessions is idempotent", {
  cleaned_once <- clean_sessions(sessions)
  cleaned_twice <- clean_sessions(cleaned_once)

  expect_equal(cleaned_once, cleaned_twice)
})

test_that("clean_epochs creates and parses columns correctly", {
  cleaned <- clean_epochs(epochs)

  # Check session_id created from filename
  expect_true("session_id" %in% colnames(cleaned))
  expect_equal(cleaned$session_id[1], "sessionA")
  # Check timestamp parsed to POSIXct
  expect_true("timenum" %in% colnames(cleaned))
  expect_true(inherits(cleaned$timenum, "POSIXct"))
  # Check is_asleep column created
  expect_true("is_asleep" %in% colnames(cleaned))
  expect_equal(cleaned$is_asleep, c(1, 0, 1, 0))
  # Check night column created
  expect_true("night" %in% colnames(cleaned))
  expect_true(inherits(cleaned$night, "Date"))
})

test_that("Double-cleaning epochs is idempotent", {
  cleaned_once <- clean_epochs(epochs)
  cleaned_twice <- clean_epochs(cleaned_once)

  expect_equal(cleaned_once, cleaned_twice)
})

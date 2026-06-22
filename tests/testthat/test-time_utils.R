test_that("mean_time calculates the correct mean for valid time strings", {
  time_vector <- c("2025-04-08 23:00:00", "2025-04-09 01:00:00")
  result <- mean_time(time_vector)
  expect_equal(result, "00:00")
})

test_that("mean_time handles POSIXct input correctly", {
  time_vector <- as.POSIXct(c("2025-04-08 23:00:00", "2025-04-09 01:00:00"), tz = "UTC")
  result <- mean_time(time_vector)
  expect_equal(result, "00:00")
})

test_that("mean_time handles NA values gracefully", {
  time_vector <- c("2025-04-08 23:00:00", NA, "2025-04-09 01:00:00")
  result <- mean_time(time_vector)
  expect_equal(result, "00:00")
})

test_that("mean_time handles empty input", {
  time_vector <- c()
  result <- mean_time(time_vector)
  expect_equal(result, NA_character_)
})

test_that("mean_time handles single time input", {
  time_vector <- c("2025-04-08 12:00:00")
  result <- mean_time(time_vector)
  expect_equal(result, "12:00")
})

test_that("mean time outputs in correct format", {
  time_vector <- c("2025-04-08 20:00:00", "2025-04-09 22:00:00")
  result <- mean_time(time_vector, unit = "hour")
  expect_equal(result, 21)
  result <- mean_time(time_vector, unit = "minute")
  expect_equal(result, 1260)
  result <- mean_time(time_vector, unit = "second")
  expect_equal(result, 75600)
})

test_that("sd_time calculates the correct deviation for valid time strings", {
  time_vector <- c("2025-04-08 23:00:00", "2025-04-09 01:00:00")
  result <- sd_time(time_vector)
  expect_equal(round(result, 2), 0.26)
})

test_that("sd_time handles NA values gracefully", {
  time_vector <- c("2025-04-08 23:00:00", NA, "2025-04-09 01:00:00")
  result <- sd_time(time_vector)
  expect_equal(round(result, 2), 0.26)
})

test_that("sd_time handles empty input", {
  time_vector <- c()
  result <- sd_time(time_vector)
  expect_equal(result, NA_real_)
})

test_that("sd_time handles single time input", {
  time_vector <- c("2025-04-08 12:00:00")
  result <- sd_time(time_vector)
  expect_equal(result, 0)
})

test_that("min_time calculates the correct minimum time", {
  time_vector <- c("2025-04-08 11:00:00", "2025-04-05 13:00:00")
  result <- min_time(time_vector)
  expect_equal(result, "13:00")
})

test_that("max_time calculates the correct maximum time", {
  time_vector <- c("2025-04-08 11:00:00", "2025-04-05 13:00:00")
  result <- max_time(time_vector)
  expect_equal(result, "11:00")
})

test_that("time_diff calculates the correct time difference", {
  result <- time_diff("2025-04-08 21:00:00", "2025-04-05 06:00:00", unit = "hour")
  expect_equal(result, 9)
  result <- time_diff("2025-04-08 21:00:00", "2025-04-05 06:00:00", unit = "minute")
  expect_equal(result, 9 * 60)
})

test_that("shift_times_by_12h shifts times correctly", {
  times <- c("2025-04-08 00:00:00", "2025-04-08 12:30:00", "2025-04-08 15:30:00")
  shifted_times <- shift_times_by_12h(times)
  expected_times <- as.POSIXct(c("2025-04-08 12:00:00", "2025-04-08 00:30:00", "2025-04-08 03:30:00"), tz = "UTC")
  expect_equal(shifted_times, expected_times)
})

test_that("shift_times_by_12h works with POSIXct data", {
  times <- as.POSIXct(c("2025-04-08 00:00:00", "2025-04-08 12:30:00", "2025-04-08 15:30:00"), tz = "UTC")
  shifted_times <- shift_times_by_12h(times)
  expected_times <- as.POSIXct(c("2025-04-08 12:00:00", "2025-04-08 00:30:00", "2025-04-08 03:30:00"), tz = "UTC")
  expect_equal(shifted_times, expected_times)
})

test_that("get_time_per_day works correctly", {
  expect_equal(get_time_per_day("second"), 86400)
  expect_equal(get_time_per_day("minute"), 1440)
  expect_equal(get_time_per_day("hour"), 24)

  expect_error(get_time_per_day("blarghs"),
               "unit must be one of 'second', 'minute', or 'hour'")
})

test_that("is_iso8601_datetime correctly identifies ISO 8601 datetime strings", {
  valid_datetimes <- c("2025-04-08T09:49:52.888000+01:00", "2025-04-08T10:00:00Z")
  expect_true(is_iso8601_datetime(valid_datetimes))

  mixed_values <- c("2025-04-08T09:49:52.888000+01:00", NA, "2025-04-08T10:00:00Z")
  expect_true(is_iso8601_datetime(mixed_values))

  empty_and_na <- c("", NA, "")
  expect_false(is_iso8601_datetime(empty_and_na))

  invalid_datetimes <- c("not-a-date", "2025-04-08 10:00:00", "08-04-2025T10:00:00")
  expect_false(is_iso8601_datetime(invalid_datetimes))

  mixed_invalid <- c("2025-04-08T09:49:52.888000+01:00", "not-a-date", NA)
  expect_false(is_iso8601_datetime(mixed_invalid))

  empty_vector <- character(0)
  expect_true(is.na(is_iso8601_datetime(empty_vector)))
})

mock_sessions <- data.frame(
  session_start = c("2025-03-03T09:00:00", "2025-03-03T20:00:00", "2025-03-04T20:00:00"),
  .data_type = "somnofy_v2"
)

mock_epochs <- data.frame(
  timestamp = c("2025-03-03T11:00:00", "2025-03-03T23:00:00", "2025-03-04T01:00:00", "2025-03-04T11:00:00"),
  .data_type = "somnofy_v2"
)

test_that("group_sessions_by_night correctly groups sessions by night", {
  grouped_sessions <- group_sessions_by_night(mock_sessions)
  expect_equal(grouped_sessions$night, lubridate::as_date(c("2025-03-02", "2025-03-03", "2025-03-04")))
})

test_that("group_epochs_by_night correctly groups epochs by night", {
  grouped_epochs <- group_epochs_by_night(mock_epochs)
  expect_equal(grouped_epochs$night, lubridate::as_date(c("2025-03-02", "2025-03-03", "2025-03-03", "2025-03-03")))
})

test_that("time_to_hours correctly converts POSIXct to hours", {
  time_vector <- as.POSIXct(c("2025-03-03 09:30:00", "2025-03-03 20:00:00"), tz = "UTC")
  hours_vector <- time_to_hours(time_vector)
  expect_equal(hours_vector, c(9.5, 20))
})

test_that("parse_time handles character input", {
  time_vector <- c("2025-03-03 09:30:00", "2025-03-03T20:00:00")
  parsed_time <- parse_time(time_vector)
  expected_time <- as.POSIXct(c("2025-03-03 09:30:00", "2025-03-03 20:00:00"), tz = "UTC")
  expect_equal(parsed_time, expected_time)
})

test_that("parse_time handles POSIXct input", {
  time_vector <- as.POSIXct(c("2025-03-03 09:30:00", "2025-03-03T20:00:00"), tz = "UTC")
  parsed_time <- parse_time(time_vector)
  expect_equal(parsed_time, time_vector)
})

test_that("parse_time handles NA values", {
  time_vector <- c("2025-03-03 09:30:00", NA, "2025-03-03T20:00:00")
  parsed_time <- parse_time(time_vector)
  expected_time <- as.POSIXct(c("2025-03-03 09:30:00", NA, "2025-03-03 20:00:00"), tz = "UTC")
  expect_equal(parsed_time, expected_time)
})

test_that("parse_time handles empty input", {
  time_vector <- c()
  parsed_time <- parse_time(time_vector)
  expect_equal(parsed_time, as.POSIXct(c(), tz = "UTC"))
})

test_that("parse_time handles recursive calls", {
  time_vector <- c("2025-03-03 09:30:00", "2025-03-03T20:00:00")
  parsed_time <- parse_time(parse_time(time_vector))
  expect_equal(parsed_time, as.POSIXct(c("2025-03-03 09:30:00", "2025-03-03 20:00:00"), tz = "UTC"))
})

test_that("parse_time returns numeric input unchanged", {
  time_vector <- c(0, 43200, 86400)
  parsed_time <- parse_time(time_vector)
  expect_equal(parsed_time, c(0, 43200, 86400))
})

test_that("parse_time ignores timezone successfully", {
  time_vector <- c("2025-03-03 09:30:00+01:00", "2025-03-03T20:00:00+00:00")
  parsed_time <- parse_time(time_vector)
  expected_time <- as.POSIXct(c("2025-03-03 09:30:00", "2025-03-03 20:00:00"), tz = "UTC")
  expect_equal(parsed_time, expected_time)
})

test_that("update_date works correctly", {
  times <- parse_time(c("2025-03-03 09:30:00", "2025-03-03T20:00:00"))
  updated_date <- update_date(times, "2025-04-08")
  expected_date <- parse_time(c("2025-04-08 09:30:00", "2025-04-08 20:00:00"))
  expect_equal(updated_date, expected_date)
})

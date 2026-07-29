sessions <- data.frame(
  id = 1:5,
  session_start = c(
    "2025-03-10T19:30:00+00:00",
    "2025-03-11T23:45:00+00:00",
    "2025-03-12T01:15:00+00:00",
    "2025-03-12T03:00:00+00:00",
    "2025-03-12T18:00:00+00:00"
  ),
  session_end = c(
    "2025-03-11T06:30:00+00:00",
    "2025-03-12T06:30:00+00:00",
    "2025-03-12T07:30:00+00:00",
    "2025-03-12T07:30:00+00:00",
    "2025-03-13T06:30:00+00:00"
  ),
  time_at_sleep_onset = c(
    "2025-03-10T22:30:00+00:00",
    "",
    "",
    "2025-03-12T05:00:00+00:00",
    ""
  ),
  time_at_wakeup = c(
    "2025-03-11T06:30:00+00:00",
    "",
    "",
    "2025-03-12T07:00:00+00:00",
    ""
  ),
  night = c(
    "2025-03-10",
    "2025-03-11",
    "2025-03-11",
    "2025-03-11",
    "2025-03-12"
  ),
  time_in_bed = c(8 * 60 * 60, 6 * 60 * 60, 7 * 60 * 60, 0.5 * 60 * 60, 1 * 60 * 60),
  sleep_period = c(1736, 0, 0, 26364, 0),
  subject_id = c("ABD000", "ABD000", "ABD000", "ABD000", "ABD000")
)

test_that("set_time_in_bed works", {
  filtered_sessions <- set_min_time_in_bed(sessions, 2)
  expect_equal(nrow(filtered_sessions), 3)
})

test_that("set_time_in_bed masking works", {
  mask <- set_min_time_in_bed(sessions, 2, return_mask = TRUE)
  expect_equal(sum(mask), 3)
  expect_equal(length(mask), nrow(sessions))
})

test_that("set_session_start_time_range masking works", {
  mask <- set_session_start_time_range(sessions, "19:00", "23:00", return_mask = TRUE)
  expect_equal(sum(mask), 1)
  expect_equal(length(mask), nrow(sessions))
})

test_that("set_session_start_time_range works for times before midnight", {
  filtered_sessions <- set_session_start_time_range(sessions, "19:00", "23:00")
  expect_equal(nrow(filtered_sessions), 1)
})

test_that("set_session_start_time_range works for midnight spanning range", {
  filtered_sessions <- set_session_start_time_range(sessions, "19:00", "02:00")
  expect_equal(nrow(filtered_sessions), 3)
})

test_that("set_session_sleep_onset_range masking works", {
  mask <- set_session_sleep_onset_range(sessions, "19:00", "23:00", return_mask = TRUE)
  expect_equal(sum(mask), 1)
  expect_equal(length(mask), nrow(sessions))
})

test_that("set_session_sleep_onset_range works for times before midnight", {
  filtered_sessions <- set_session_sleep_onset_range(sessions, "19:00", "23:00")
  expect_equal(nrow(filtered_sessions), 1)
})

test_that("set_session_sleep_onset_range works for midnight spanning range", {
  filtered_sessions <- set_session_sleep_onset_range(sessions, "19:00", "02:00")
  expect_equal(nrow(filtered_sessions), 1)
})

test_that("remove_sessions_no_sleep works", {
  filtered_sessions <- remove_sessions_no_sleep(sessions)
  expect_equal(nrow(filtered_sessions), 2)
})

test_that("get_non_complying_sessions works", {
  non_complying_sessions <- get_non_complying_sessions(sessions)
  expect_equal(nrow(non_complying_sessions), 3)
})

test_that("get_removed_sessions_table works", {
  filtered_sessions <- set_session_sleep_onset_range(sessions, "19:00", "02:00")
  removed_sessions <- get_removed_sessions(sessions, filtered_sessions)
  expect_equal(nrow(removed_sessions), 1)
})

test_that("get_removed_sessions handles reverse input", {
  filtered_sessions <- set_session_sleep_onset_range(sessions, "19:00", "02:00")
  expect_error(get_removed_sessions(filtered_sessions, sessions), "There are more rows in filtered sessions than in sessions")
})

test_that("keep_longest works as expected", {
  filtered_sessions <- keep_longest(sessions)
  expected_result <- sessions[c(1, 4, 5), ]
  rownames(expected_result) <- NULL
  expect_equal(filtered_sessions, expected_result)
})

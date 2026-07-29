sessions <- data.frame(
  subject_id = c(1, 2, 3),
  session_start = as.POSIXct(c("2024-01-01 22:00", "2024-01-02 23:00", "2024-01-03 21:30")),
  end_of_session = as.POSIXct(c("2024-01-02 06:00", "2024-01-03 07:00", "2024-01-04 05:30"))
)

epochs <- data.frame(
  session_id = c(1, 1, 2, 2, 3, 3),
  timestamp = as.POSIXct(c("2024-01-01 22:00", "2024-01-02 00:00", "2024-01-02 23:00", "2024-01-03 01:00", "2024-01-03 21:30", "2024-01-04 00:30")),
  sleeping = c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)
)

# Sessions ----

test_that("check_session_colnames passes when it should", {
  expect_no_error(check_session_colnames(
    sessions,
    required_cols = c("subject_id", "session_start")
  ))
})

test_that("check_session_colnames fails when columns are missing", {
  expect_error(
    check_session_colnames(
      sessions,
      required_cols = c("subject_id", "session_end")
    ),
    regexp = "Some columns are not present in the session data"
  )
})

# Epochs ----

test_that("check_epoch_colnames passes when it should", {
  expect_no_error(check_epoch_colnames(
    epochs,
    required_cols = c("session_id", "timestamp")
  ))
})

test_that("check_epoch_colnames fails when columns are missing", {
  expect_error(
    check_epoch_colnames(
      epochs,
      required_cols = c("session_id", "is_asleep")
    ),
    regexp = "Some columns are not present in the epoch data"
  )
})

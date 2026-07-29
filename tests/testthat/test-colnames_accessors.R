sessions <- data.frame(
  id = 1:5,
  custom_id = 11:15
)

epochs <- data.frame(
  session_id = 1:5,
  custom_session_id = 11:15
)

test_that("get_session_colnames returns pre-set values", {
  result <- get_session_colnames(sessions)
  expect_equal(result$id, "id")
})

test_that("get_session_colnames doesn't override non-NULL values", {
  col_names <- list(id = "custom_id")
  attr(sessions, "col") <- col_names
  result <- get_session_colnames(sessions)
  expect_equal(result$id, "custom_id")
})

test_that("get_epochs_colnames returns pre-set values", {
  result <- get_epoch_colnames(epochs)
  expect_equal(result$session_id, "session_id")
})

test_that("get_epoch_colnames doesn't override non-NULL values", {
  col_names <- list(session_id = "custom_id")
  attr(epochs, "col") <- col_names
  result <- get_epoch_colnames(epochs)
  expect_equal(result$session_id, "custom_id")
})

test_that("get_colnames dispatches correctly", {
  attr(sessions, "type") <- "sessions"
  result_sessions <- get_colnames(sessions)
  expect_equal(result_sessions$id, "id")

  attr(epochs, "type") <- "epochs"
  result_epochs <- get_colnames(epochs)
  expect_equal(result_epochs$session_id, "session_id")
})

test_that("set_colnames sets the col attribute", {
  col_names <- list(id = "custom_id")
  updated_sessions <- set_colnames(sessions, col_names)
  result <- attr(updated_sessions, "col")
  expect_equal(result$id, "custom_id")
})

test_that("colnames to canonical creates columns", {
  attr(sessions, "type") <- "sessions"
  sessions <- set_colnames(sessions, list(sleep_period = "custom_id"))
  sessions <- colnames_to_canonical(sessions)

  expect_true("sleep_period" %in% names(sessions))
})

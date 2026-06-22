sessions <- data.frame(
  session_id = 1:6,
  is_workday = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
  time_at_midsleep = c("2023-01-01 03:00:00", "2023-01-02 02:30:00",
                       "2023-01-03 02:45:00", "2023-01-04 04:00:00",
                       "2023-01-05 04:15:00", "2023-01-06 04:30:00"),
  tz = "UTC",
  sleep_period = c(7 * 3600, 6.5 * 3600, 7 * 3600, 8 * 3600, 8.5 * 3600, 9 * 3600),
  night = c("2023-01-02", "2023-01-03", "2023-01-04",
            "2023-01-05", "2023-01-06", "2023-01-07")
)

test_that("interdaily_stability works", {
  result <- interdaily_stability(example_epochs)
  expect_equal(round(result, 2), 0.52)
})

test_that("social_jet_lag works", {
  result <- social_jet_lag(example_sessions)
  expect_equal(round(result, 2), -0.28)
})

test_that("chronotype works", {
  result <- chronotype(sessions)
  expect_equal(round(result, 2), 3.83)
})

test_that("composite_phase_deviation works", {
  result <- composite_phase_deviation(sessions)
  expect_equal(round(result, 1), 1)
})

test_that("sleep_regularity_index works", {
  result <- sleep_regularity_index(example_epochs)
  expect_equal(round(result), 96)
})

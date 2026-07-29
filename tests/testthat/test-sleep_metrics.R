sessions <- data.frame(
  id = 1:6,
  is_workday = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
  time_at_midsleep = c("2023-01-01 03:00:00", "2023-01-02 02:30:00",
                       "2023-01-03 02:45:00", "2023-01-04 04:00:00",
                       "2023-01-05 04:15:00", "2023-01-06 04:30:00"),
  sleep_period = c(7 * 3600, 6.5 * 3600, 7 * 3600, 8 * 3600, 8.5 * 3600, 9 * 3600),
  night = c("2023-01-02", "2023-01-03", "2023-01-04",
            "2023-01-05", "2023-01-06", "2023-01-07")
)

epochs <- data.frame(
  timestamp = parse_time(c("2025-04-11 10:01:04", "2025-04-11 08:01:34",
                           "2025-04-12 10:02:04", "2025-04-12 08:02:34",
                           "2025-04-12 12:03:04")),
  is_asleep = c(0, 0, 1, 0, 1)
)

test_that("interdaily_stability works", {
  result <- interdaily_stability(epochs)
  expect_equal(round(result, 2), 0.58)
})

test_that("social_jet_lag works", {
  result <- social_jet_lag(sessions)
  expect_equal(round(result, 2), 1.5)
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
  result <- sleep_regularity_index(epochs)
  expect_equal(round(result), 99)
})

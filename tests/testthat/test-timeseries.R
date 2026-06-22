mock_epochs <- data.frame(
  timestamp = c(
    "2025-03-03T11:00:00", "2025-03-03T23:00:00",
    "2025-03-04T01:00:00", "2025-03-04T11:00:00",
    "2025-03-04T13:00:00", "2025-03-04T23:00:00"
  ),
  temperature_ambient_mean = c(10, 20, 15, 25, 30, 35),
  night = lubridate::as_date(c(
    "2025-03-02", "2025-03-03",
    "2025-03-03", "2025-03-03",
    "2025-03-04", "2025-03-04"
  ))
)

mock_sessions <- data.frame(
  id = 1:3,
  night = c("2025-03-03", "2025-03-03", "2025-03-04"),
  temperature_mean = c(23, 15, 18)
)

test_that("plot_timeseries returns a ggplot object", {
  # Call the function
  plot <- plot_timeseries(mock_epochs, "temperature_ambient_mean")

  # Check that the result is a ggplot object
  expect_s3_class(plot, "ggplot")
})

test_that("plot_timeseries uses the correct x and y mappings", {
  plot <- plot_timeseries(mock_epochs, variable = "temperature_ambient_mean")

  plot_data <- ggplot2::ggplot_build(plot)$data[[1]]

  expect_equal(plot_data$x, time_to_hours(shift_times_by_12h(mock_epochs$timestamp)))
  expect_equal(plot_data$y, mock_epochs$temperature_ambient_mean)
})

test_that("plot_timeseries_sessions returns a ggplot object", {
  plot <- plot_timeseries_sessions(mock_sessions, "temperature_mean")

  expect_s3_class(plot, "ggplot")
})
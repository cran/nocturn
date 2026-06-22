test_that("plot_hypnogram handles valid input correctly", {
  epochs <- data.frame(
    night = c("2025-03-01", "2025-03-01", "2025-03-02", "2025-03-01", "2025-03-01"),
    timestamp = lubridate::as_date(c("2025-03-01 14:00:00",
                                     "2025-03-01 20:00:00",
                                     "2025-03-02 23:00:00",
                                     "2025-03-02 11:30:00",
                                     "2025-03-02 09:25:12")),
    sleep_stage = c(1, 2, 3, 1, 4)
  )

  plot <- plot_hypnogram(epochs)

  expect_s3_class(plot, "ggplot")
})

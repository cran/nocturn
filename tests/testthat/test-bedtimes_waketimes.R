test_that("plot_bedtimes_waketimes handles valid input correctly", {
  sessions <- data.frame(
    time_at_sleep_onset = c("2025-03-03T11:24:11.524000+00:00",
                            "2025-03-03T14:44:11.524000+00:00",
                            "2025-03-03T18:24:11.524000+00:00",
                            "2025-03-03T00:24:11.524000+00:00",
                            "2025-03-03T23:24:11.524000+00:00"),
    time_at_wakeup = c("2025-03-03T11:24:11.524000+00:00",
                       "2025-03-03T14:44:11.524000+00:00",
                       "2025-03-03T18:24:11.524000+00:00",
                       "2025-03-03T00:24:11.524000+00:00",
                       "2025-03-03T23:24:11.524000+00:00"),
    night = c("2025-03-02", "2025-03-03", "2025-03-03", "2025-03-03", "2025-03-03"),
    is_workday = c(TRUE, FALSE, TRUE, FALSE, TRUE)
  )

  plot <- plot_bedtimes_waketimes(sessions, groupby = "night")
  expect_s3_class(plot, "ggplot")

  plot <- plot_bedtimes_waketimes(sessions, groupby = "weekday")
  expect_s3_class(plot, "ggplot")

  plot <- plot_bedtimes_waketimes(sessions, groupby = "workday")
  expect_s3_class(plot, "ggplot")
})

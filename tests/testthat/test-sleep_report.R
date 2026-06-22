test_that("sleep_report generates a PDF file", {

  sessions <- data.frame(
    night = as.Date("2025-06-01") + 0:2,
    time_at_sleep_onset = c(23, 22.5, 0),
    time_at_wakeup = c(7, 6.5, 8),
    time_at_midsleep = c(3, 2.5, 4),
    sleep_onset_latency = c(15, 10, 20),
    sleep_period = c(420, 400, 430),
    time_in_bed = c(480, 470, 490),
    is_workday = c(TRUE, TRUE, FALSE)
  )

  tmp_pdf <- tempfile(fileext = ".pdf")

  suppressWarnings(sleep_report(sessions, output_file = tmp_pdf))

  expect_true(file.exists(tmp_pdf))
  expect_gt(file.info(tmp_pdf)$size, 0)

  con <- file(tmp_pdf, "rb")
  header <- readBin(con, "raw", 5)
  close(con)
  expect_equal(rawToChar(header), "%PDF-")

  unlink(tmp_pdf)
})

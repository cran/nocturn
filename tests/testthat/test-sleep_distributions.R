test_that("sleeptimes_boxplot handles valid input correctly", {
  plot <- sleeptimes_boxplot(example_sessions)
  expect_s3_class(plot, "ggplot")
})

test_that("sleeptimes_histogram handles valid input correctly", {
  plot <- sleeptimes_histogram(example_sessions)
  expect_s3_class(plot, "ggplot")
})

test_that("sleeptimes_density handles valid input correctly", {
  plot <- sleeptimes_density(example_sessions)
  expect_s3_class(plot, "ggplot")
})

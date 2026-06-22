sessions <- data.frame(
  id = c("A", "B", "C", "D", "E"),
  annotation = c("Annot1", "Annot1", "", "", "Annot2")
) |>
  set_data_type("sessions")

epochs <- data.frame(
  session_id = c("A", "A", "A", "I", "I", "C", "D", "E", "F", "G", "H", "B", "J"),
  epoch_data = c(1, 10, 20, 2, 3, 4, 5, 6, 7, 8, 9, 10, 5)
) |>
  set_data_type("epochs")

test_that("annotate epochs from sessions works", {
  result <- annotate_epochs_from_sessions(sessions, epochs)

  expect_equal(class(result), "data.frame")
  expect_equal(nrow(result), nrow(epochs))
  expect_equal(result$annotation[1:3], c("Annot1", "Annot1", "Annot1"))
  expect_equal(result$annotation[4:7], c("", "", "", ""))
})

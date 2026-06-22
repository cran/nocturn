test_folder <- tempdir()
write.csv(example_epochs, file.path(test_folder, "epochs_reports.csv"), row.names = FALSE)

common <- list(
  epochs = shiny::reactiveVal(NULL),
  epoch_filters = shiny::reactiveVal(NULL)
)

test_that("input_epochs module returns correct data", {
  shiny::testServer(
    input_epochs_server,
    args = list(common = common),
    {
      session$setInputs(epochs_file = data.frame(name = "epochs_reports.csv", datapath = file.path(test_folder, "epochs_reports.csv")))
      session$flushReact()

      expect_equal(class(common$epochs()), "data.frame")
      expect_equal(ncol(common$epochs()), 19)
      expect_equal(nrow(common$epochs()), 18755)
    }
  )
})

test_that("init_epochs initialises common reactives correctly", {
  epochs <- data.frame(
    session_id = 1:3,
    timestamp = as.Date("2023-01-01") + 0:2
  ) |>
    set_data_type("epochs")

  init_epochs(epochs, common)

  shiny::isolate({
    # Check epochs reactive
    expect_true(is.data.frame(common$epochs()))
    expect_equal(nrow(common$epochs()), 3)

    # Check epoch_filters reactive
    filters <- common$epoch_filters()
    expect_true(is.data.frame(filters))
    expect_equal(nrow(filters), 3)
    expect_true("from_sessions" %in% colnames(filters))
    expect_true(all(filters$no_sleep))
  })
})

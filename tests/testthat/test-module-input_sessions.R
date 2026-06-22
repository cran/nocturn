test_folder <- tempdir()
write.csv(example_sessions, file.path(test_folder, "sessions_reports.csv"), row.names = FALSE)

common <- list(
  sessions = shiny::reactiveVal(NULL),
  session_filters = shiny::reactiveVal(NULL),
  annotations = shiny::reactiveVal(NULL)
)

test_that("input_sessions module returns correct data", {
  shiny::testServer(
    input_sessions_server,
    args = list(common = common),
    {
      session$setInputs(sessions_file = data.frame(name = "sessions_reports.csv", datapath = file.path(test_folder, "sessions_reports.csv")))
      session$flushReact()

      expect_equal(class(common$sessions()), "data.frame")
      expect_equal(ncol(common$sessions()), 69)
      expect_equal(nrow(common$sessions()), 124)
    }
  )
})

test_that("init_sessions initialises common reactives correctly", {
  sessions <- data.frame(
    id = 1:3,
    calendar_date = as.Date("2023-01-01") + 0:2
  ) |>
    set_data_type("sessions")

  init_sessions(sessions, common)

  shiny::isolate({
    # Check sessions reactive
    expect_true(is.data.frame(common$sessions()))
    expect_equal(nrow(common$sessions()), 3)
    expect_true("annotation" %in% colnames(common$sessions()))

    # Check session_filters reactive
    filters <- common$session_filters()
    expect_true(is.data.frame(filters))
    expect_equal(nrow(filters), 3)
    expect_true("no_sleep" %in% colnames(filters))
    expect_true(all(filters$no_sleep))

    # Check annotations reactive
    ann <- common$annotations()
    expect_true(is.data.frame(ann))
    expect_equal(nrow(ann), 3)
    expect_true("id" %in% colnames(ann))
    expect_true("annotation" %in% colnames(ann))
    expect_true(all(ann$annotation == ""))
  })
})

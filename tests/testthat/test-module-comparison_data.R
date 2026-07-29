test_folder <- tempdir()
write.csv(example_sessions, file.path(test_folder, "somnofy.csv"), row.names = FALSE)
write.csv(example_sessions, file.path(test_folder, "axivity.csv"), row.names = FALSE)

common <- list(
  sessions = shiny::reactiveVal(NULL),
  sessions_raw = shiny::reactiveVal(NULL),
  secondary_sessions = shiny::reactiveVal(NULL),
  session_filters = shiny::reactiveVal(NULL),
  filter_values = shiny::reactiveVal(NULL),
  annotations = shiny::reactiveVal(NULL)
)

test_that("comparison_data module loads datasets to secondary_sessions", {
  shiny::testServer(
    comparison_data_server,
    args = list(common = common),
    {
      session$setInputs(sessions_file = data.frame(name = "somnofy.csv", datapath = file.path(test_folder, "somnofy.csv")))
      session$flushReact()

      session$setInputs(sessions_file = data.frame(name = "axivity.csv", datapath = file.path(test_folder, "somnofy.csv")))
      session$flushReact()

      ss <- common$secondary_sessions()

      id_1 <- names(ss)[1]
      id_2 <- names(ss)[2]

      expect_equal(ss[[id_1]]$title, "somnofy.csv")
      expect_equal(class(ss[[id_1]]$data), "data.frame")
      expect_equal(nrow(ss[[id_1]]$data), 124)

      expect_equal(ss[[id_2]]$title, "axivity.csv")
      expect_equal(class(ss[[id_2]]$data), "data.frame")
      expect_equal(nrow(ss[[id_2]]$data), 124)
    }
  )
})

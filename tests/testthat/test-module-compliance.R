common <- list(
  sessions = shiny::reactiveVal(example_sessions |> remove_sessions_no_sleep()),
  session_filters = shiny::reactiveVal(data.frame(no_sleep = rep(TRUE, nrow(example_sessions |> remove_sessions_no_sleep())))),
  annotations = shiny::reactiveVal(data.frame(id = example_sessions$id, annotation = "", stringsAsFactors = FALSE))
)

test_that("compliance module works", {
  shiny::testServer(
    compliance_server,
    args = list(common = common),
    {
      expect_warning(session$flushReact())  # Expecting app warning "there are nights with multiple sessions"
      expect_equal(
        compliance_table(),
        get_compliance_table(common)
      )
    }
  )
})

test_that("get_compliance_table output is correct", {
  result <- shiny::isolate(get_compliance_table(common))

  expect_equal(class(result), "data.frame")
  expect_equal(nrow(result), 4)
  expect_length(unique(result$night), 1)
})

test_that("make_sessions_display_table output is correct", {
  result <- shiny::isolate(make_sessions_display_table(common$sessions()))

  expect_equal(class(result), "data.frame")
  expect_equal(nrow(result), 17)
  expect_length(unique(result$night), 14)
})

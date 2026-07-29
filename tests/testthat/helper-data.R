sessions <- example_sessions |> dplyr::mutate(is_workday = as.logical(.data$is_workday))
epochs <- example_epochs

make_common <- function() {
  list(
    sessions = shiny::reactiveVal(sessions),
    session_filters = shiny::reactiveVal(data.frame(no_sleep = rep(TRUE, nrow(sessions)))),
    filter_values = shiny::reactiveVal(NULL),
    annotations = shiny::reactiveVal(
      data.frame(
        id = example_sessions$id,
        annotation = "",
        stringsAsFactors = FALSE
      )
    ),

    epochs = shiny::reactiveVal(epochs),
    epoch_filters = shiny::reactiveVal(data.frame(from_sessions = rep(TRUE, nrow(epochs)))),

    secondary_sessions = shiny::reactiveVal(
      list(
        abcdef = list(
          title = "somnofy",
          filters = data.frame(no_sleep = rep(TRUE, nrow(sessions))),
          data = example_sessions
        ),
        ghijkl = list(
          title = "axivity",
          filters = data.frame(no_sleep = rep(TRUE, nrow(sessions))),
          data = sessions
        )
      )
    )
  )
}

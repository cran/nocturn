show_metric_modal <- function(metric_name) {
  rmd_path <- system.file("shiny", package = "nocturn")
  shiny::showModal(
    shiny::modalDialog(
      title = gsub("_", " ", metric_name),
      size = "l",
      shiny::includeMarkdown(paste0(rmd_path, "/Rmd/", metric_name, ".Rmd")),
      easyClose = TRUE,
      footer = shiny::modalButton("Close")
    )
  )
}

help_modal_ui <- function(ns, id = "help") {
  shiny::tagList(
    shiny::actionLink(
      inputId = ns(id),
      label = shiny::icon("circle-info", class = "help-btn"),
    )
  )
}

show_help_modal <- function(help_name) {
  rmd_path <- system.file("shiny", package = "nocturn")
  shiny::showModal(
    shiny::modalDialog(
      title = gsub("_", " ", help_name),
      size = "l",
      shiny::includeMarkdown(paste0(rmd_path, "/Rmd/", help_name, ".Rmd")),
      easyClose = TRUE,
      footer = shiny::modalButton("Close")
    )
  )
}

show_colnames_modal <- function(
  ns,
  colnames_list,
  current_map,
  type = "sessions",
  save_id = "save_col_names",
  reset_id = "reset_col_names"
) {
  if (type == "sessions") {
    title <- "Set Session Column Names"
    long_names <- .sessions_long
    help_tips <- .sessions_help
  } else if (type == "epochs") {
    title <- "Set Epoch Column Names"
    long_names <- .epochs_long
    help_tips <- .epochs_help
  }
  inputs <- lapply(names(long_names), function(key) {
    current_value <- as.character(current_map[[key]])
    if (is.null(current_map[[key]]) || is.na(current_map[[key]]) || current_map[[key]] == "-") current_value <- "-"
    choices <- c("-", colnames_list)
    label_text <- long_names[[key]] %||% key
    help_text <- help_tips[[key]] %||% NULL
    label <- shiny::tagList(
      label_text,
      if (!is.null(help_text)) bslib::tooltip(
        shiny::tags$span(
          shiny::icon("circle-info"),
          class = "colnames-help"
        ),
        help_text,
        placement = "right",
        options = list(delay = list(show = 0, hide = 100))
      )
    )
    shiny::selectInput(
      inputId = ns(paste0("col_", key)),
      label = label,
      choices = choices,
      selected = current_value
    )
  })
  shiny::showModal(
    shiny::modalDialog(
      title = title,
      size = "l",
      easyClose = TRUE,
      footer = shiny::tagList(
        shiny::actionButton(ns(reset_id), "Reset", class = "delete-btn"),
        shiny::modalButton("Cancel"),
        shiny::actionButton(ns(save_id), "Save")
      ),
      shiny::p("Hint: type in the boxes to search for column names."),
      do.call(shiny::tagList, inputs)
    )
  )
}

register_colnames_modal <- function(
  input, session, ns, common,
  type = "sessions",
  open_event,
  get_df,
  get_raw,
  set_df
) {
  save_id <- "session_save_colnames"
  reset_id <- "session_reset_colnames"

  # Open modal
  shiny::observeEvent(input[[open_event]], {
    df <- get_df()
    shiny::req(df)

    show_colnames_modal(
      ns = ns,
      colnames_list = names(df),
      current_map = get_colnames(df),
      type = type,
      save_id = save_id,
      reset_id = reset_id
    )
  }, ignoreInit = TRUE)

  # Reset colnames
  shiny::observeEvent(input[[reset_id]], {

    if (type == "sessions") {
      df <- clean_sessions(get_raw())
    } else {
      df <- clean_epochs(get_raw())
    }

    set_df(df)
    common$logger |> write_log("Reset session column names to default", type = "complete")
    shiny::removeModal()
  }, ignoreInit = TRUE)

  # Save colnames
  shiny::observeEvent(input[[save_id]], {
    if (type == "sessions") {
      df <- get_raw()
      keys <- names(.sessions_long)
    } else {
      df <- get_raw()
      keys <- names(.epochs_long)
    }
    shiny::req(df)
    vals <- lapply(keys, function(k) {
      val <- input[[paste0("col_", k)]]
      if (identical(val, "-")) NULL else val
    })

    df <- df |>
      set_colnames(stats::setNames(vals, keys))

    df <- if (type == "sessions") clean_sessions(df) else clean_epochs(df)

    set_df(df)
    common$logger |> write_log("Session column names saved", type = "complete")
    shiny::removeModal()
  }, ignoreInit = TRUE)
}

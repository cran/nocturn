annotation_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("annotations_text")),
    shiny::textInput(ns("annotation_text"), NULL, placeholder = "Annotation"),
    shiny::div(
      style = "display: flex; gap: 0.5em;",
      shiny::actionButton(ns("apply_annotation"), "Apply"),
      shiny::actionButton(ns("reset_annotations"), "Reset", class = "delete-btn")
    ),
    DT::DTOutput(ns("annotation_table"))
  )
}

annotation_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {

    output$annotations_text <- shiny::renderUI({
      shiny::HTML(paste0(
        "<p>Select sessions to annotate by clicking on the table below.</p>",
        "<p>Use the search bar to control which sessions are shown in the table.</p>",
        "<p>Type your annotation in the text box and click 'Apply' to save it.</p>"
      ))
    })

    output$annotation_table <- DT::renderDT({
      shiny::req(common$sessions(), common$session_filters(), common$annotations())
      DT::datatable(
        make_annotation_table(common),
        rownames = FALSE,
        selection = "multiple",
        options = list(
          dom = "lftip",              # Show length menu, filter, table, info, and pagination
          pageLength = 50,            # Default page length
          lengthMenu = c(25, 50, 100, 200, 500), # Choices for page length
          paging = TRUE               # Enable pagination
        )
      )
    })

    # Record new annotations ----
    shiny::observeEvent(input$apply_annotation, {
      col <- get_colnames(common$sessions())
      ann <- common$annotations()
      sessions <- apply_filters(common$sessions(), common$session_filters())
      selected <- input$annotation_table_rows_selected
      if (length(selected) > 0) {
        selected_ids <- sessions[[col$id]][selected]
        ann$annotation[match(selected_ids, ann$id)] <- input$annotation_text
        common$logger |> write_log(paste0("Added annotation ", input$annotation_text), type = "info")
        common$annotations(ann)
      }
    })

    # Reset all annotations ----
    shiny::observeEvent(input$reset_annotations, {
      ann <- common$annotations()
      ann$annotation <- ""
      common$logger |> write_log("Reset all annotations", type = "complete")
      common$annotations(ann)
    })

    # Apply annotations to epochs table ----
    shiny::observe({
      shiny::req(common$sessions(), common$epochs())
      sessions <- common$sessions() |>
        annotate(common$annotations())
      common$epochs(annotate_epochs_from_sessions(
        sessions = sessions,
        epochs = common$epochs()
      ))
    })

  })
}

make_annotation_table <- function(common) {
  col <- get_colnames(common$sessions())
  sessions <- common$sessions() |>
    apply_filters(common$session_filters()) |>
    annotate(common$annotations())
  sessions |>
    dplyr::mutate(
      annotation = common$annotations()$annotation[match(.data[[col$id]], common$annotations()$id)],
      start = parse_time(get_col(sessions, col$session_start)) |> format("%Y-%m-%d %H:%M"),
      sleep_onset = parse_time(get_col(sessions, col$time_at_sleep_onset)) |> format("%H:%M"),
      wakeup = parse_time(get_col(sessions, col$time_at_wakeup)) |> format("%H:%M"),
      end = parse_time(get_col(sessions, col$session_end)) |> format("%Y-%m-%d %H:%M"),
      night = format(get_col(sessions, col$night), "%Y-%m-%d"),
      time_in_bed_h = if (!is.null(col$time_in_bed)) round(get_col(sessions, col$time_in_bed) / 60 / 60, 2) else NA
    ) |>
    dplyr::select(
      dplyr::any_of(c(
        "annotation",
        col$subject_id,
        "start",
        "sleep_onset",
        "wakeup",
        "end",
        "time_in_bed_h"
      ))
    )
}

annotate_epochs_from_sessions <- function(sessions, epochs) {
  if (nrow(epochs) == 0) {
    return(epochs)
  }
  scol <- get_colnames(sessions)
  ecol <- get_colnames(epochs)

  annotation_map <- stats::setNames(sessions$annotation, sessions[[scol$id]])

  if (is.null(ecol$session_id)) {
    epochs$annotation <- ""
  } else {
    epochs$annotation <- annotation_map[as.character(epochs[[ecol$session_id]])]
  }
  epochs$annotation[is.na(epochs$annotation)] <- ""

  epochs
}

annotate <- function(sessions, annotations) {
  col <- get_colnames(sessions)
  sessions$annotation <- annotations$annotation[match(sessions[[col$id]], annotations$id)]
  sessions
}

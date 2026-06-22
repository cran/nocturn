get_plot_download_handler <- function(session, common, output_plot, format, width = 8, height = 6) {
  shiny::downloadHandler(
    filename = function() {
      paste0("plot_", Sys.Date(), ".", format())
    },
    content = function(file) {
      shiny::req(output_plot())
      plot <- output_plot()

      common$logger |> write_log(paste0("Exporting plot in ", format(), " format."), type = "complete")

      ggplot2::ggsave(filename = file, plot = plot, device = format(), bg = "white", width = width, height = height)
    }
  )
}

get_table_download_handler <- function(session, common, output_table, output_name = "") {
  shiny::downloadHandler(
    filename = function() {
      paste0(output_name, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(output_table, file)
      common$logger |> write_log(paste0("Exporting table: ", output_name, " (", nrow(output_table), " rows)"), type = "complete")
    }
  )
}

get_report_download_handler <- function(session, logger, sessions, title) {
  shiny::downloadHandler(
    filename = function() {
      paste0("Sleep_report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      col <- get_colnames(sessions)
      required_cols <- c(
        "time_at_sleep_onset", "time_at_wakeup", "time_at_midsleep",
        "sleep_onset_latency", "sleep_period", "time_in_bed", "night"
      )
      missing_cols <- required_cols[sapply(required_cols, function(nm) is.null(col[[nm]]))]
      if (length(missing_cols) > 0) {
        logger |> write_log(paste0("Sleep report missing columns: ", paste(missing_cols, collapse = ", ")), type = "error")
        logger |> write_log("Could not generate sleep report", type = "info")
        return(invisible(NULL))
      }
      tmpfile <- tempfile(fileext = ".pdf")
      sleep_report(sessions = sessions, title = title(), output_file = tmpfile)
      file.copy(tmpfile, file)
      logger |> write_log(paste0("Sleep report generated"), type = "complete")
    },
    contentType = "application/pdf"
  )
}

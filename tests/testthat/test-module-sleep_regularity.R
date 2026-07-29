test_that("sleep_regularity module produces sessions table", {
  shiny::testServer(
    sleep_regularity_server,
    args = list(common = make_common()),
    {
      session$flushReact()

      doc <- xml2::read_html(as.character(output$sleep_sessions_regularity_table$html))

      th <- rvest::html_elements(doc, "th") |> rvest::html_text(trim = TRUE)
      expect_equal(th, c("Metric", "Value"))

      rows <- rvest::html_elements(doc, "tbody tr")
      expect_length(rows, 4)
    }
  )
})

test_that("sleep_regularity module produces epochs table", {
  shiny::testServer(
    sleep_regularity_server,
    args = list(common = make_common()),
    {
      session$flushReact()

      doc <- xml2::read_html(as.character(output$sleep_epochs_regularity_table$html))

      rows <- rvest::html_elements(doc, "tbody tr")
      expect_length(rows, 2)

      metrics <- rvest::html_elements(doc, "tbody tr td:first-child") |> rvest::html_text(trim = TRUE)
      expect_equal(metrics, c("Interdaily Stability", "Sleep Regularity Index"))
    }
  )
})

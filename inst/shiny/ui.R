resource_path <- system.file("shiny", "www", package = "nocturn")
addResourcePath("resources", resource_path)

tagList(
  bslib::page_navbar(
    lang = "en",
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "yeti"
    ),
    id = "tabs",
    header = tagList(
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(
        script = file.path("shinyjs-funcs.js"),
        functions = c("scrollLogger")
      ),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      ),
    ),
    title = img(src = "logo.png", height = "50", width = "50"),
    window_title = "nocturn",
    bslib::nav_panel("Intro", value = "intro"),
    bslib::nav_panel("Import Data", value = "import_data"),
    bslib::nav_panel("Filtering", value = "filtering"),
    bslib::nav_panel("Export Data", value = "export_data"),
    bslib::nav_menu("Support",
                    HTML('<a href="https://github.com/chronopsychiatry/AMBIENT-BD-nocturn/wiki" target="_blank">Wiki</a>'),
                    HTML('<a href="https://github.com/chronopsychiatry/AMBIENT-BD-nocturn/blob/main/NEWS.md" target="_blank">Changelog</a>'),
                    HTML('<a href="https://github.com/chronopsychiatry/AMBIENT-BD-nocturn/issues" target="_blank">GitHub Issues</a>'),
                    HTML('<a href="mailto: daniel.thedie@ed.ac.uk" target="_blank">Send Email</a>')),
  ),

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 400,
      tags$button(
        id = "sidebar_toggle_btn",
        class = "sidebar-toggle-btn",
        HTML("&#x25C0;") # Unicode for left-pointing triangle/arrow
      ),
      div(class = "sidebar_container",
        conditionalPanel(
          condition = "input.tabs == 'intro'",
          includeMarkdown("Rmd/text_intro_tab.Rmd"),
        ),
        conditionalPanel(
          condition = "input.tabs == 'import_data'",
          input_ui("input"),
        ),
        conditionalPanel(
          condition = "input.tabs == 'filtering'",
          filtering_ui("filtering"),
        ),
        conditionalPanel(
          condition = "input.tabs == 'export_data'",
          export_data_ui("export_data"),
        ),
        hr(),
        conditionalPanel(
          condition = "input.tabs != 'intro'",
          div(
            strong("Log window"),
            div(style = "margin-top: 5px"),
            div(
              id = "messageLog",
              div(id = "logHeader", div(id = "logContent"))
            ),
            br(),
            textOutput("logger")
          )
        )
      )
    ),

    # Main panel ----
    div(
      class = "main_panel",

      # Tabset for tables ----
      bslib::navset_card_tab(
        id = "main_tabs_tables",
        bslib::nav_panel("Summary", summary_ui("summary")),
        bslib::nav_panel("Compliance", compliance_ui("compliance"), value = "compliance_tab"),
        bslib::nav_panel("Filtering", filtering_tab("filtering"), value = "filtering_tab"),
        bslib::nav_panel("Annotation", annotation_ui("annotation"), value = "annotation_tab"),
        bslib::nav_panel("Sleep Regularity", sleep_regularity_ui("sleep_regularity"), value = "sleep_regularity_tab"),
      ),

      # Tabset for plots ----
      bslib::navset_card_tab(
        id = "main_tabs_plots",
        bslib::nav_panel("Sleep Clock", sleep_clock_ui("sleep_clock")),
        bslib::nav_panel("Sleep Spiral", sleep_spiral_ui("sleep_spiral")),
        bslib::nav_panel("Sleep Onset & Wakeup", bedtimes_waketimes_ui("bedtimes_waketimes")),
        bslib::nav_panel("Sleep Times Distributions", sleep_distributions_ui("sleep_distributions")),
        bslib::nav_panel("Sleep Bubbles", sleep_bubbles_ui("sleep_bubbles")),
        bslib::nav_panel("Hypnogram", hypnogram_ui("hypnogram")),
        bslib::nav_panel("Session Timeseries", timeseries_sessions_ui("timeseries_sessions")),
        bslib::nav_panel("Epoch Timeseries", timeseries_ui("timeseries"))
      )
    ),

    # Footer ----
    div(
      style = "text-align: left; margin-top: 5px; font-size: 12px; color: #555;",
      textOutput("footer_text")
    )
  )
)

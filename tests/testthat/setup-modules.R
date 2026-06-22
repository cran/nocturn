files <- list.files(
  system.file("shiny", "modules", package = "nocturn"),
  pattern = "\\.R$", full.names = TRUE
)
for (f in files) source(f)

source(system.file("shiny", "global.R", package = "nocturn"))

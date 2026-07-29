library(nocturn)

nocturn::check_suggests()

mb <- 1024^2
upload_size_mb <- 5000
options(shiny.maxRequestSize = upload_size_mb * mb)

# Source helper functions
source(system.file("shiny", "plot_helpers.R", package = "nocturn"))
source(system.file("shiny", "download_handlers.R", package = "nocturn"))
source(system.file("shiny", "modal_dialogs.R", package = "nocturn"))

# Source all app modules
modules <- list.files("modules", pattern = "\\.R$", full.names = TRUE)
for (m in modules) source(m)

# Initialise common
source(system.file("shiny", "common.R", package = "nocturn"))
common <- common_class$new()

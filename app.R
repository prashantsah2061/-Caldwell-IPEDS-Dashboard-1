library(shiny)
library(tidyverse)
library(ggplot2)
library(scales)
library(DT)
library(janitor)

app_dir <- {
  sourced_files <- vapply(
    sys.frames(),
    function(frame) {
      if (is.null(frame$ofile)) {
        return(NA_character_)
      }

      frame$ofile
    },
    character(1)
  )
  sourced_files <- sourced_files[!is.na(sourced_files)]

  if (length(sourced_files) > 0) {
    dirname(normalizePath(sourced_files[[length(sourced_files)]]))
  } else {
    normalizePath(".")
  }
}

app_path <- function(...) {
  file.path(app_dir, ...)
}

addResourcePath(prefix = "assets", directoryPath = app_path("www"))

# Reusable helper functions and data preparation utilities.
source(app_path("functions.R"), local = TRUE)

# Dashboard user interface.
source(app_path("ui.R"), local = TRUE)

# Load prepared data once per R process. The helper uses data/.cache/*.rds
# when the source CSV files have not changed.
dashboard_data <- load_dashboard_data()

# Dashboard server logic.
source(app_path("mod_ipedsr.R"), local = TRUE)

shinyApp(ui = ui, server = server)

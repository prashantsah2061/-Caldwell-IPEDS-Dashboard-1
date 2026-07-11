library(shiny)
library(dplyr)
library(ggplot2)

options(shiny.legacy.datatable = TRUE)

addResourcePath(prefix = "assets", directoryPath = "www")

# Reusable helper functions.
source("functions.R")

# Dashboard user interface.
source("ui.R")

# Dashboard server logic.
source("mod_ipedsr.R")

shinyApp(ui = ui, server = server)

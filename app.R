# -----------------------------------------------------------------------
# app.R
# Owner: Fatemeh
# -----------------------------------------------------------------------

library(shiny)
library(ggplot2)
library(sf)

source_dir <- function(path) {
  files <- list.files(path, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  for (f in files) source(f)
}

# --- fatemeh code -------------------------------------------------
source("R/utilities/constants.R")
source_dir("R/inputs")
source_dir("R/pipeline")
source_dir("R/plotting")
source("R/app_ui.R")
source("R/app_server.R")

# --- Stella code (patterns/, geometry/) -------------------------------
if (dir.exists("R/patterns"))  source_dir("R/patterns")
if (dir.exists("R/geometry"))  source_dir("R/geometry")

# --- Liba code (layouts/) -------------------------------------------
if (dir.exists("R/layouts"))   source_dir("R/layouts")

shinyApp(ui = app_ui(), server = app_server)

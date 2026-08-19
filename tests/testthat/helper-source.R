# -----------------------------------------------------------------------
# helper-source.R
#
# testthat automatically sources every tests/testthat/helper-*.R file
# before running tests (via testthat::test_dir("tests/testthat")), so
# this loads the app's R/ files WITHOUT sourcing app.R itself (which
# would try to launch shinyApp()). Mirrors app.R's own sourcing order.
#
# Run tests with your working directory set to the repo root, e.g.:
#   testthat::test_dir("tests/testthat")
# -----------------------------------------------------------------------

library(sf)
library(ggplot2)

source_dir <- function(path) {
  files <- list.files(path, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  for (f in files) source(f)
}

source("R/utilities/constants.R")
source_dir("R/inputs")
source_dir("R/pipeline")
source_dir("R/plotting")

if (dir.exists("R/patterns")) source_dir("R/patterns")
if (dir.exists("R/geometry"))  source_dir("R/geometry")
if (dir.exists("R/layouts"))   source_dir("R/layouts")

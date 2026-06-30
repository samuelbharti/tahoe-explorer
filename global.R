# Load libraries and source files
library(shiny)
library(bslib)

# Optionally theme base/ggplot/lattice output to match the app theme. Activates
# only if the {thematic} package is installed, so it adds no hard dependency.
if (requireNamespace("thematic", quietly = TRUE)) {
  thematic::thematic_shiny(font = "auto")
}

source("R/load_components.R")

# Load data/connections
# Example: app_data <- readRDS("data/app_data.rds")

# Preprocess small data

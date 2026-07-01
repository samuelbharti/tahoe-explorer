# Load libraries and source files.
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
library(DT)
library(reactable)
library(plotly)
library(DBI)
library(duckdb)

# Optionally theme base/ggplot/lattice output to match the app theme. Activates
# only if the {thematic} package is installed, so it adds no hard dependency.
if (requireNamespace("thematic", quietly = TRUE)) {
  thematic::thematic_shiny(font = "auto")
}

# Source utilities (incl. the data layer and page registry), modules, then the
# page-level UI files, which self-register into the navbar.
source("R/load_components.R")

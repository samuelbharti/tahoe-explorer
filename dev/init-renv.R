# dev/init-renv.R
# Generate renv.lock for Tahoe Explorer from its ACTUAL dependency set.
#
# renv.lock cannot be hand-authored (it records exact versions + content hashes),
# so run this once in your own R environment (RStudio or Rscript) from the app
# root:
#
#   Rscript dev/init-renv.R
#
# It initializes renv, installs the packages the app + tooling actually use, and
# writes renv.lock. Review it, then commit renv.lock, renv/activate.R, and
# .Rprofile so a fresh clone and the Docker image install a reproducible library.

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}

# Runtime dependencies: global.R's library() calls plus pkg:: usage across R/,
# modules/, and userInterface/. Base packages (utils/stats/grDevices) are
# omitted. DiagrammeR renders the About architecture diagram.
runtime <- c(
  "shiny",
  "bslib",
  "ggplot2",
  "dplyr",
  "stringr",
  "scales",
  "janitor",
  "reactable",
  "plotly",
  "DBI",
  "duckdb",
  "DiagrammeR"
)

# Tooling used by dev scripts and the test suite (captured so a restored library
# can also rebuild data and run the tests).
tooling <- c("jsonlite", "testthat", "shinytest2", "chromote", "withr")

if (!file.exists("renv.lock")) {
  message("Initializing renv and installing dependencies ...")
  renv::init(bare = TRUE)
  renv::install(c(runtime, tooling))
  renv::snapshot(prompt = FALSE)
  message(
    "renv.lock written. Review it, then commit renv.lock, renv/activate.R, ",
    "and .Rprofile."
  )
} else {
  message(
    "renv.lock already exists. Run renv::restore() to install from it, ",
    "or delete it and re-run this script to regenerate."
  )
}

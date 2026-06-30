# dev/init-renv.R
# Helper script to initialize renv for a new project based on this template.

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}

# Initialize renv if not already
if (!file.exists("renv.lock")) {
  message("Initializing renv and taking initial snapshot...")
  renv::init(bare = TRUE)
  # Install a few recommended packages used by the template
  renv::install(c(
    "shiny",
    "bslib",
    "brand.yml",
    "dplyr",
    "ggplot2",
    "DT",
    "plotly"
  ))
  renv::snapshot()
  message(
    "renv initialized and renv.lock written. Review renv.lock before committing."
  )
} else {
  message("renv.lock already exists. Use renv::restore() to restore packages.")
}

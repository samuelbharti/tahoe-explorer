# Project Title

<!-- template:strip:start -->
> **Using this template:** click **Use this template** on GitHub (or
> `npx degit SamuelBharti/RShiny_template my-app`), then run
> `Rscript dev/use_template.R --project_name="My App" --author="Your Name"`
> to set your project name, author, and version. This block, the citation
> files, and the template machinery are removed automatically.

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.19968600.svg)](https://doi.org/10.5281/zenodo.19968600)
<!-- template:strip:end -->

Add a short description of your application.

Current app version: v2.2

## Requirements

- R (>= 4.3)
- RStudio (optional)
- Packages used by the app (managed with `renv` recommended)

## Installation

### Option 1: Using renv (recommended)

```r
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

renv::restore()
```

### Option 2: Manual package installation

```r
install.packages(c(
  "shiny",
  "bslib",
  "brand.yml",
  "dplyr",
  "ggplot2",
  "DT",
  "plotly"
))
```

## How To Run

```r
shiny::runApp()
```

Or open the project in RStudio and click Run App.

## Build And Run With Docker

```bash
docker build -t my-shiny-app .
docker run --rm -p 3838:3838 my-shiny-app
```

Then open [http://localhost:3838](http://localhost:3838).

This template expects a project-level `renv.lock` file and `renv/` metadata to be present in the app you create from it.

## Project Structure

```txt
.
├── _brand.yml              # Brand colors, fonts, logo (theming)
├── global.R                # Libraries and global objects
├── ui.R                    # App UI definition
├── server.R                # App server logic
├── R/                      # Utility functions
├── modules/                # Reusable Shiny modules
├── userInterface/          # Page-level UI components
├── data/                   # App data files
├── dev/                    # Local development scripts
├── www/                    # Static assets (css/js/img)
└── docs/                   # Project documentation
```

## Deployment

Recommended deployment paths:

- Posit Publisher or Posit Connect for direct app publishing
- Docker image deployment when you want a containerized release

CI/CD-driven publishing is not included here by default unless you explicitly add and maintain it for a given app.

## Theming

Branding lives in [`_brand.yml`](_brand.yml) — colors, fonts, and logo in one
place. It is applied automatically by bslib via `bs_theme(brand = TRUE)` in
[ui.R](ui.R). Edit `_brand.yml` to restyle the whole app; no other changes are
needed. To also theme plots and tables, install
[`thematic`](https://rstudio.github.io/thematic/) — `global.R` picks it up
automatically when present. See [docs/theming.md](docs/theming.md).

## Contributing

See CONTRIBUTING.md for contribution guidelines.

<!-- template:strip:start -->
---

Template developed by [samuelbharti](https://github.com/SamuelBharti).

## How to cite

If you use this template or a derived project, please cite it. Machine-readable citation metadata is available in `CITATION.cff`, and a human-readable example is provided in `CITATION.md`.
<!-- template:strip:end -->

# Tahoe Explorer

A lightweight Shiny app to explore [Tahoe-100M](https://huggingface.co/datasets/vevotx/Tahoe-100M)
metadata: filter and summarize drug, cell-line, sample, and cell-level metadata
in quick charts, and pull a subset (filtered file + analysis recipe) to plan
downstream analysis.

The full dataset is ~100M cells; this app works with the small curated metadata
tables and queries the large cell-level `obs` table lazily with duckdb — no need
to load 2.29 GB into memory.

## Requirements

- R (>= 4.3)
- Packages: `shiny`, `bslib`, `brand.yml`, `duckdb`, `dplyr`, `stringr`,
  `ggplot2`, `scales`, `DT`, `reactable`, `plotly`, `DiagrammeR` (managed with
  `renv` recommended)

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
  "shiny", "bslib", "brand.yml", "duckdb", "dplyr", "stringr",
  "ggplot2", "scales", "DT", "reactable", "plotly", "DiagrammeR"
))
```

## Data

The app ships with small **synthetic fixtures** under `data/fixtures/` so it
runs out of the box with no network and no real data.

To explore the real Tahoe-100M metadata, download it (into the gitignored
`data/` directory):

```bash
# Small curated tables (~1.5 MB): drug, cell line, sample, gene
Rscript dev/download_metadata.R

# Also fetch the 2.29 GB cell-level obs table (optional)
Rscript dev/download_metadata.R --obs
```

Configuration via environment variables (see `.Renviron.example`):

- `TAHOE_METADATA_DIR` — directory holding downloaded metadata (default `data`).
- `TAHOE_OBS_REMOTE` — set to `1` to query the cell-level `obs` table directly
  from HuggingFace (slower) instead of downloading it.
- `HF_TOKEN` — optional HuggingFace token for better remote access (higher rate
  limits, gated datasets). See below.

Real data is never committed; only the synthetic fixtures are.

### HuggingFace token (optional)

The Tahoe-100M dataset is public, so no token is needed — but a token gives
higher rate limits and more reliable remote reads. To add one:

1. Create a token with **read** scope at
   <https://huggingface.co/settings/tokens>.
2. Copy `.Renviron.example` to `.Renviron` (git-ignored) and set:

   ```sh
   HF_TOKEN=hf_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
   ```

3. Restart R. The app registers it as a duckdb HuggingFace secret for remote
   `obs` queries, and `dev/download_metadata.R` sends it when downloading.

`HUGGING_FACE_HUB_TOKEN` and `HUGGINGFACE_TOKEN` are also accepted.

## How To Run

```r
shiny::runApp()
```

Or open the project in RStudio and click Run App.

## Build And Run With Docker

```bash
docker build -t tahoe-explorer .
docker run --rm -p 3838:3838 tahoe-explorer
```

Then open [http://localhost:3838](http://localhost:3838).

## Project Structure

```txt
.
├── _brand.yml              # Brand colors, fonts (theming)
├── global.R                # Libraries and global objects
├── ui.R                    # App UI (assembles registered tabs)
├── server.R                # App server (mounts registered tabs)
├── R/                      # Utilities, data layer, page registry
├── modules/                # Reusable Shiny modules (one per feature tab)
├── userInterface/          # Page definitions that self-register tabs
├── data/                   # Downloaded metadata (gitignored) + fixtures/
├── dev/                    # Download + fixture-generation scripts
├── www/                    # Static assets (css/js/img)
└── docs/                   # Project documentation
```

## Theming

Branding lives in [`_brand.yml`](_brand.yml) — colors and fonts in one place,
applied automatically by bslib via `bs_theme(brand = TRUE)` in [ui.R](ui.R).
See [docs/theming.md](docs/theming.md).

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md). Pull request titles follow
[Conventional Commits](https://www.conventionalcommits.org/) (enforced in CI).

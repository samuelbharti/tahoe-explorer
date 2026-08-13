# Tahoe Explorer

A lightweight Shiny app to explore [Tahoe-100M](https://huggingface.co/datasets/tahoebio/Tahoe-100M)
metadata: filter and summarize drug, cell-line, sample, and cell-level metadata
in quick charts, and pull a subset (filtered file + analysis recipe) to plan
downstream analysis.

The full dataset is ~100M cells; this app works with the small curated metadata
tables and queries the large cell-level `obs` table lazily with duckdb -- no need
to load 2.29 GB into memory.

## Requirements

- R (>= 4.3)
- Packages: `shiny`, `bslib`, `duckdb`, `dplyr`, `stringr`, `echarts4r`
  (charts), `plotly`, `ggplot2`, `scales`, `janitor`, `reactable`, `cicerone`
  (guided tours), `DiagrammeR` (About-tab diagrams), and `ltc` (chart palettes,
  from GitHub). `renv` is recommended -- `renv::restore()` installs the exact
  set, including the GitHub `ltc` package.

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
  "shiny", "bslib", "duckdb", "dplyr", "stringr", "ggplot2",
  "echarts4r", "plotly", "scales", "janitor", "reactable",
  "cicerone", "DiagrammeR"
))

# `ltc` (chart palettes) is on GitHub, not CRAN:
# install.packages("remotes")
# remotes::install_github("loukesio/ltc-color-palettes")
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

- `TAHOE_METADATA_DIR` -- directory holding downloaded metadata (default `data`).
- `TAHOE_OBS_REMOTE` -- set to `1` to query the cell-level `obs` table directly
  from HuggingFace (slower) instead of downloading it.
- `HF_TOKEN` -- optional HuggingFace token for better remote access (higher rate
  limits, gated datasets). See below.

Real data is never committed; only the synthetic fixtures are.

### Data sources

This app does not redistribute any of the datasets below; it downloads them on
request and ships only synthetic fixtures. Each source carries its own license
and citation terms, which apply to your use of the data independently of this
app's MIT license.

- **Tahoe-100M** ([tahoebio/Tahoe-100M](https://huggingface.co/datasets/tahoebio/Tahoe-100M))
  is the dataset this app explores. Released under CC0 1.0, but the authors ask
  that you cite it: Zhang, J., Ubas, A. A., de Borja, R., Svensson, V., Thomas,
  N., Thakar, N., Lai, I., Winters, A., Khan, U., Jones, M. G., et al. (2025).
  *Tahoe-100M: A Giga-Scale Single-Cell Perturbation Atlas for Context-Dependent
  Gene Function and Cellular Modeling*. bioRxiv.
- **DepMap 24Q4 Public** (`OmicsSomaticMutations.csv`), used by
  `dev/download_variants.R` for somatic-variant annotation. CC BY 4.0.
  DOI: [10.25452/figshare.plus.27993248.v1](https://doi.org/10.25452/figshare.plus.27993248.v1).
  Cite: DepMap, Broad (2024). *DepMap 24Q4 Public*. Figshare+.
- **Cellosaurus** (<https://www.cellosaurus.org>), the fallback source of curated
  driver variants for cell lines DepMap does not cover. CC BY 4.0.

### HuggingFace token (optional)

The Tahoe-100M dataset is public, so no token is needed -- but a token gives
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

## AI assistant

An optional **Tahoe assistant** -- a collapsible left sidebar available on every
page, toggled by the **Assistant** button in the navbar -- adds a Gemini-backed
assistant (via `ellmer` and `shinychat` on Google Vertex AI) that can explain the
Tahoe-100M dataset and this app, and -- most usefully -- help you plan a subset:
describe your research question and it recommends which filters and columns to
pick, then returns the same reproducible R + Python pull recipe as the **Subset
builder** tab. It is also **page-aware**: it knows which tab you're on and can
apply filters / selections for you on the interactive pages (Drugs, Cell lines,
Subset builder, Coverage, Samples & cells) -- e.g. "select the breast-cancer
drugs". It answers only from a set of hand-written tools over the app's metadata,
so it does not fabricate numbers; no tool can read files, environment variables,
or secrets.

The assistant is **off by default and degrades gracefully**: until it is
configured (and `ellmer` + `shinychat` are installed), the sidebar shows a short
setup panel and the rest of the app is unaffected. Nothing about the assistant is
loaded unless it is enabled.

It can be powered two ways, chosen live from a **Model source** selector in the
sidebar's collapsible **Model & key** section: a **shared** assistant the
operator configures on Google Vertex (below), and/or **bring your own key** --
each user pastes their own Gemini, OpenAI, or Anthropic key, held only in their
browser session and never stored or logged. Either path alone is enough to enable
the assistant. With your own key you can pick a model from a short curated list or
click **List models for this key** to load the provider's current, key-scoped
models (so the picker never offers a model your account can't use) -- or just type
any model id and press Enter.

To configure the shared assistant:

1. Install the packages (both are in `renv.lock`): `renv::restore()`, or
   `install.packages(c("ellmer", "shinychat"))`.
2. Authenticate with Google Cloud once -- **no API key or token is stored in the
   repo**:

   ```sh
   gcloud auth application-default login
   ```

3. Set your project in `.Renviron` (git-ignored; see `.Renviron.example`) and
   restart R.

Configuration via environment variables:

- `TAHOE_VERTEX_PROJECT` -- GCP project with Vertex AI enabled (required to enable
  the shared assistant). The aliases `VERTEX_PROJECT_ID`, `GOOGLE_CLOUD_PROJECT`,
  and `GCLOUD_PROJECT` are also accepted.
- `TAHOE_VERTEX_LOCATION` -- Vertex region (default `us-central1`); the alias
  `VERTEX_LOCATION` is also accepted.
- `TAHOE_VERTEX_MODEL` -- Gemini model id (default `gemini-2.5-flash`).
- `TAHOE_AGENT_TEMPERATURE` -- sampling temperature (default `0.2`, for factual
  answers).
- `TAHOE_AGENT_DISABLE` -- set to `1` to force the assistant off even when
  configured (the test suite sets this).
- `TAHOE_AGENT_BYOK` -- set to `0` to remove the bring-your-own-key option (on by
  default whenever the packages are installed).
- `TAHOE_AGENT_BYOK_PROVIDERS` -- comma list of BYOK providers to offer (default
  `gemini,openai,anthropic`; unknown names are ignored).

The assistant sticks to the Tahoe-100M dataset, this app, and subset planning; it
declines clinical or medical advice and off-topic requests. In Docker there is no
`gcloud` login, so the shared assistant degrades to the setup panel unless you
provide credentials (for example, a mounted service-account key or workload
identity) -- but users can still bring their own key. Because a bring-your-own key
travels from the browser to the server, **serve the app over HTTPS** for any
public deployment.

## Build And Run With Docker

```bash
docker build -t tahoe-explorer .
docker run --rm -p 3838:3838 tahoe-explorer
```

Then open [http://localhost:3838](http://localhost:3838).

## Project Structure

```txt
.
├-- _brand.yml              # Brand colors, fonts (theming)
├-- global.R                # Libraries and global objects
├-- ui.R                    # App UI (assembles registered tabs)
├-- server.R                # App server (mounts registered tabs)
├-- R/                      # Utilities, data layer, page registry
├-- modules/                # Reusable Shiny modules (one per feature tab)
├-- userInterface/          # Page definitions that self-register tabs
├-- data/                   # Downloaded metadata (gitignored) + fixtures/
├-- dev/                    # Download + fixture-generation scripts
├-- www/                    # Static assets (css/js/img)
└-- docs/                   # Project documentation
```

## Theming

Branding lives in [`_brand.yml`](_brand.yml) -- colors and fonts in one place,
applied automatically by bslib via `bs_theme(brand = TRUE)` in [ui.R](ui.R).
See [docs/theming.md](docs/theming.md).

## Citation

If you use this app, please cite it using the metadata in
[CITATION.cff](CITATION.cff). Note that the datasets it reads carry their own
citation requirements: see [Data sources](#data-sources).

## License

This app's source is released under the [MIT License](LICENSE). The datasets it
reads are licensed separately by their respective providers.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md). Pull request titles follow
[Conventional Commits](https://www.conventionalcommits.org/) (enforced in CI).

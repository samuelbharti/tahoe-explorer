# Tahoe Explorer

[![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.21926312-1682D4)](https://doi.org/10.5281/zenodo.21926312)

By [Samuel Bharti](https://www.samuelbharti.com)

A Shiny app that explores the metadata of
[Tahoe-100M](https://huggingface.co/datasets/tahoebio/Tahoe-100M). You can filter
and summarize the drug, cell-line, sample, and cell-level metadata in charts. You
can also pull a subset, which is a filtered file with an analysis recipe.

The full dataset holds approximately 100 million cells. This app reads the small
curated tables instead. It queries the large cell-level `obs` table with duckdb,
and it reads only the rows that a page needs. The app therefore does not load
2.29 GB into memory.

## Requirements

- R 4.3 or later.
- These packages: `shiny`, `bslib`, `duckdb`, `dplyr`, `stringr`, `echarts4r`,
  `plotly`, `ggplot2`, `scales`, `janitor`, `reactable`, `cicerone`,
  `DiagrammeR`, and `ltc`.

`echarts4r` and `plotly` draw the charts. `cicerone` gives the guided tours.
`DiagrammeR` draws the diagrams on the About tab. `ltc` gives the chart palettes,
and it comes from GitHub and not from CRAN.

Use `renv` to get the exact versions. `renv::restore()` installs every package,
including `ltc`.

## Installation

### Option 1: renv

This option gives the exact package versions from `renv.lock`. Use it if you can.

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

The repository holds the **six small curated tables** (approximately 3.3 MB under
`data/`). A clone and a deployment therefore show the real Tahoe-100M numbers
immediately: 100,648,790 cells, 50 assayed cell lines, 379 drugs, 1,344 samples,
and 62,710 genes. Tahoe-100M is CC0 1.0. [Data sources](#data-sources) credits
the sources of the variant table.

The repository does **not** hold the 2.29 GB cell-level `obs` table. The
cell-level tab therefore uses a synthetic fixture. To make that tab real, either
download the table or set `TAHOE_OBS_REMOTE=1`, which is described below.

The synthetic fixtures are in `data/fixtures/`. The test suite and a checkout
without real data use these fixtures, so both work without a network.

To download the real data, run these commands. The data goes into the part of
`data/` that git ignores:

```bash
# Refresh the small curated tables: drug, cell line, sample, gene
Rscript dev/download_metadata.R

# Also fetch the 2.29 GB cell-level obs table (optional)
Rscript dev/download_metadata.R --obs
```

These environment variables control the data. `.Renviron.example` shows each one:

- `TAHOE_METADATA_DIR` is the directory that holds the metadata. The default is
  `data`.
- `TAHOE_OBS_REMOTE` set to `1` queries the cell-level `obs` table from
  HuggingFace. The app then does not need a local copy, but the queries are slow.
- `HF_TOKEN` is an optional HuggingFace token. A token gives higher rate limits
  and access to gated datasets. The next section describes it.

The 2.29 GB `obs` table is never committed, and neither is anything else under
`data/` beyond the six small tables and the fixtures.

### Deploying to Posit Connect Cloud

`manifest.json` drives the deployment. To write that file again, run
`Rscript dev/write_manifest.R`.

The repository holds the small curated tables. The deployed app therefore shows
real numbers, and not the synthetic demo.

To make the cell-level tab real, set these environment variables in the
deployment:

- `TAHOE_OBS_REMOTE=1` makes duckdb read the `obs` table from HuggingFace through
  `hf://`.
- `HF_TOKEN` gives higher rate limits.
- `DUCKDB_EXTENSION_DIRECTORY` keeps the `httpfs` extension. Without this
  variable, each container start downloads the extension again.

### Data sources

The repository holds the six small curated tables. It does not hold the 2.29 GB
cell-level `obs` table, and the app reads that table only on request.

Each source below has its own license and citation terms. These terms apply to
your use of the data. The MIT license of this app does not change them.

- **Tahoe-100M**
  ([tahoebio/Tahoe-100M](https://huggingface.co/datasets/tahoebio/Tahoe-100M)) is
  the dataset that this app explores. The license is CC0 1.0. The authors ask you
  to cite this reference: Zhang, J., Ubas, A. A., de Borja, R., Svensson, V.,
  Thomas, N., Thakar, N., Lai, I., Winters, A., Khan, U., Jones, M. G., et al.
  (2025). *Tahoe-100M: A Giga-Scale Single-Cell Perturbation Atlas for
  Context-Dependent Gene Function and Cellular Modeling*. bioRxiv.
- **DepMap 24Q4 Public** (`OmicsSomaticMutations.csv`) gives the somatic-variant
  annotation, and `dev/download_variants.R` reads it. The license is CC BY 4.0.
  The DOI is
  [10.25452/figshare.plus.27993248.v1](https://doi.org/10.25452/figshare.plus.27993248.v1).
  Cite: DepMap, Broad (2024). *DepMap 24Q4 Public*. Figshare+.
- **Cellosaurus** (<https://www.cellosaurus.org>) gives curated driver variants
  for the cell lines that DepMap does not cover. The license is CC BY 4.0.

### HuggingFace token (optional)

The Tahoe-100M dataset is public, so a token is not necessary. A token gives
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

The app has an optional **Tahoe assistant**. The assistant is a collapsible
sidebar on the left of every page. The **Assistant** button in the navbar opens
and closes it. `ellmer` and `shinychat` connect it to Gemini on Google Vertex AI.

The assistant does three things:

- It explains the Tahoe-100M dataset and this app.
- It helps you plan a subset. Describe your research question, and the assistant
  recommends the filters and the columns. It then returns the same R and Python
  pull recipe as the **Subset builder** tab.
- It applies filters and selections for you. The assistant knows the active tab,
  and it can act on the Drugs, Cell lines, Subset builder, Coverage, and
  Samples and cells tabs. For example, you can write "select the breast-cancer
  drugs".

The assistant answers only from a set of tools over the metadata of the app. It
therefore does not invent numbers. No tool can read files, environment variables,
or secrets.

The assistant is off by default. Until you configure it, and until `ellmer` and
`shinychat` are installed, the sidebar shows a short setup panel. The rest of the
app works as usual, because the app loads nothing for the assistant until you
enable it.

Two paths can power the assistant. The **Model source** selector in the
**Model & key** section of the sidebar selects the path:

- A **shared** assistant. The operator configures it on Google Vertex AI, as the
  next section describes.
- **Bring your own key**. Each user adds a personal Gemini, OpenAI, or Anthropic
  key. The key stays in the browser session of that user. The app does not store
  the key and does not write it to a log.

One path alone can enable the assistant. Both paths together are also correct.

With your own key, you can select a model in three ways:

- Select a model from the short list.
- Click **List models for this key**. The app then loads the current models of
  that provider for that key, so the list shows only models that your account can
  use.
- Type any model id and press Enter.

To configure the shared assistant:

1. Install the packages with `renv::restore()`, or with
   `install.packages(c("ellmer", "shinychat"))`. `renv.lock` holds both packages.
2. Authenticate with Google Cloud one time. The repository stores no API key and
   no token:

   ```sh
   gcloud auth application-default login
   ```

3. Set your project in `.Renviron`, which git ignores. `.Renviron.example` shows
   the format.
4. Restart R.

These environment variables control the assistant:

- `TAHOE_VERTEX_PROJECT` is the Google Cloud project that has Vertex AI enabled.
  The shared assistant needs this variable. The app also accepts the names
  `VERTEX_PROJECT_ID`, `GOOGLE_CLOUD_PROJECT`, and `GCLOUD_PROJECT`.
- `TAHOE_VERTEX_LOCATION` is the Vertex region. The default is `us-central1`. The
  app also accepts the name `VERTEX_LOCATION`.
- `TAHOE_VERTEX_MODEL` is the Gemini model id. The default is `gemini-2.5-flash`.
- `TAHOE_AGENT_TEMPERATURE` is the sampling temperature. The default is `0.2`,
  which gives factual answers.
- `TAHOE_AGENT_DISABLE` set to `1` keeps the assistant off, even after you
  configure it. The test suite sets this variable.
- `TAHOE_AGENT_BYOK` set to `0` removes the bring-your-own-key option. The option
  is on while the packages are installed.
- `TAHOE_AGENT_BYOK_PROVIDERS` is a list of providers, divided by commas. The
  default is `gemini,openai,anthropic`. The app ignores an unknown name.

The assistant answers only about the Tahoe-100M dataset, this app, and subset
planning. It refuses clinical advice, medical advice, and off-topic requests.

Docker has no `gcloud` login. The shared assistant therefore shows the setup
panel, unless you supply credentials. A mounted service-account key and a workload
identity are two examples of such credentials. Each user can still bring a
personal key.

**CAUTION:** Serve the app over HTTPS in a public deployment. A
bring-your-own key travels from the browser to the server.

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

[`_brand.yml`](_brand.yml) holds the branding. It keeps the colors and the fonts
in one place. bslib applies the file through `bs_theme(brand = TRUE)` in
[ui.R](ui.R). For more information, read [docs/theming.md](docs/theming.md).

## Author

Samuel Bharti

- Email: <samuelbharti.io@gmail.com>
- Web: [samuelbharti.com](https://www.samuelbharti.com)
- ORCID: [0000-0003-4190-7058](https://orcid.org/0000-0003-4190-7058)
- GitHub: [@samuelbharti](https://github.com/samuelbharti)

## Citation

Zenodo archives each release. The DOI at the top of this file resolves to the
most recent version. To cite one specific version, use the DOI of that version
from the [Zenodo record](https://doi.org/10.5281/zenodo.21926312). The DOI of
v0.1.0 is [10.5281/zenodo.21926313](https://doi.org/10.5281/zenodo.21926313).

[CITATION.cff](CITATION.cff) holds the full metadata, which includes the author
and the version. [CITATION.md](CITATION.md) gives a ready-made text and BibTeX
entry.

Note: the datasets that this app reads have their own citation requirements. Read
[Data sources](#data-sources) for these requirements.

## License

The [MIT License](LICENSE) covers the source of this app. Copyright (c) 2026
Samuel Bharti. The providers of each dataset license their data separately.

## Contributing

Issues and pull requests are welcome. Read [CONTRIBUTING.md](CONTRIBUTING.md)
first, and please follow the [Code of Conduct](CODE_OF_CONDUCT.md). Each pull request title must obey
[Conventional Commits](https://www.conventionalcommits.org/), because a
workflow checks it.

For a security problem, do not open a public issue: [SECURITY.md](SECURITY.md)
explains how to report it privately.

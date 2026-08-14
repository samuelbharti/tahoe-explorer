# Changelog

All notable changes to this project should be documented in this file.

## [0.1.1] - 2026-08-14

### Added

- `manifest.json` for Posit Connect Cloud, plus `dev/write_manifest.R` to write
  it again. The generator takes its file list from `git ls-files`, so the 2.29 GB
  `obs` table cannot enter the deployment bundle.
- The six small curated metadata tables (approximately 3.3 MB) are now committed,
  so a clone and a deployment show the real Tahoe-100M numbers instead of the
  synthetic demo. The 2.29 GB `obs` table is still not committed.
- `Secret scan` workflow that runs gitleaks over the full history, and a gitleaks
  pre-commit hook. The hooks also gained `detect-private-key`.
- A `DMSO_TF` vehicle-control arm in the synthetic fixtures, on every plate and
  every cell line.
- Zenodo DOI badge in the README, and the concept DOI and version DOI in
  `CITATION.cff`.

### Changed

- The `app` CI job installs `ellmer` and `shinychat`. Without them the assistant
  tests skipped, so `R/agent*.R` and `modules/chat_mod.R` had no coverage. CI now
  reports 402 passing tests and no skips.
- README, CONTRIBUTING, and `docs/*.md` are rewritten in ASD-STE100 Simplified
  Technical English.
- README credits the data sources and states the license of each one.

### Fixed

- The QC control-bar assertion ran no checks, because the fixtures had no vehicle
  control. The test now asserts instead of skipping (#45).
- `docs/development.md` told contributors to branch from `dev`, which does not
  exist, and said that no CI workflows are assumed.
- `docs/installation.md` and `docs/theming.md` described this repository as a
  project template.
- `docs/project_structure.md` omitted `inst/`, `tests/`, and `manifest.json`.
- README said that the app redistributes none of the datasets and ships only
  synthetic fixtures. Both statements became untrue when the curated tables were
  committed.

## [0.1.0] - 2026-08-12

### Added

- Initial Tahoe Explorer app scaffolded from the Shiny template.
- Data access layer (`R/data.R`) backed by duckdb, reading Tahoe-100M metadata
  with a synthetic-fixture fallback so the app runs offline.
- Script to download metadata from HuggingFace (`dev/download_metadata.R`) and
  to regenerate synthetic fixtures (`dev/make_fixtures.R`).
- Page registry (`R/app_pages.R`) so feature tabs self-register.
- Overview tab with dataset summary value boxes and quick charts.
- Reusable export module (filtered CSV + parquet download).
- CI: Conventional Commits PR-title lint and path-based PR auto-labeler.
- Drugs tab: explore drugs and mechanisms of action, with MOA-based filters.
- Cell lines tab: browse assayed lines, filtered by organ and driver mutation.
- Samples & cells tab: sample/plate summaries plus a lazily queried,
  guarded view of the cell-level `obs` table.
- Subset builder tab: assemble a filtered subset and emit a reproducible
  R + Python pull recipe alongside the filtered download.
- Coverage and QC tabs: per-condition coverage, underpowered-condition
  thresholds, control and plate breakdowns.
- Genes tab: search the measured features.
- About tab: explains the Tahoe-100M dataset, with DiagrammeR diagrams.
- Optional AI assistant: a collapsible, page-aware sidebar on every page
  (`R/agent.R`, `R/agent_tools.R`, `R/agent_bridge.R`, `modules/chat_mod.R`).
  Backed by Gemini on Vertex AI via `ellmer`/`shinychat`, or bring-your-own
  Gemini/OpenAI/Anthropic key held only in the browser session. Answers from
  hand-written tools over the app's metadata, and can drive filters and
  selections on the interactive pages. Off by default and degrades to a setup
  panel when unconfigured.
- Guided demo tours for every page, connected across pages (`R/tour.R`,
  cicerone).
- Data-provenance badge on every tab, showing whether real or fixture data is
  loaded (`R/provenance.R`).
- Somatic-variant enrichment vendored from DepMap 24Q4 with a Cellosaurus
  fallback (`dev/download_variants.R`), joined on `cell_name`.
- Themed loading indicators and async helpers (`R/async.R`).
- End-to-end shinytest2 smoke test that launches the app in a headless browser.

### Changed

- Alpine Lake theme via `_brand.yml`, applied through `bs_theme(brand = TRUE)`.
- Charts moved to echarts4r (from plotly/ggplot2) across the app.
- Overview is interactive and preselects a cell line.
- Subset recipe keeps its controls, pushes predicates down into duckdb, offers
  copy-to-clipboard, and defaults the notebook download to Quarto.
- Genes tab ordered before About in the navbar.
- Assistant example prompts render as bullets with follow-ups, and the model
  and key settings sit above the assistant heading.
- Cell-level `obs` reads avoid a full scan; metadata reads prefer local files
  for speed.
- Docker image and git exclude real Tahoe data; only synthetic fixtures ship.

### Fixed

- Data layer is robust and source-consistent across real and fixture data.
- Hardcoded numbers no longer contradict the loaded data.
- Corrected the mito-fraction label.
- Use the canonical `tahoebio` dataset slug; added a Code of Conduct contact.
- Filter sidebars no longer blank out after using the assistant.
- The Overview cell-line selection survives re-renders.
- Cell-cycle legend overlap and plate axis-title clipping.
- The shinytest2 smoke test skips when no Chrome/Chromium is available.

# Changelog

All notable changes to this project should be documented in this file.

## [Unreleased]

### Added

- Data access layer (`R/data.R`) backed by duckdb, reading Tahoe-100M metadata
  with a synthetic-fixture fallback so the app runs offline.
- Script to download metadata from HuggingFace (`dev/download_metadata.R`) and
  to regenerate synthetic fixtures (`dev/make_fixtures.R`).
- Page registry (`R/app_pages.R`) so feature tabs self-register.
- Overview tab with dataset summary value boxes and quick charts.
- Reusable export module (filtered CSV + parquet download).
- CI: Conventional Commits PR-title lint and path-based PR auto-labeler.

## [0.1.0]

- Initial Tahoe Explorer app scaffolded from the Shiny template.

# Contributing Guidelines

## Branching

- Create feature branches from `dev`.
- Open pull requests into `dev` unless instructed otherwise.

## Local Setup

1. Restore dependencies with `renv::restore()`.
2. Run the app locally with `shiny::runApp()`.
3. (Recommended) Install the git pre-commit hooks:

   ```bash
   pip install pre-commit
   pre-commit install
   ```

## Code Style

- Keep page UI definitions in `userInterface/`.
- Keep reusable UI/server logic in `modules/`.
- Keep utility functions in `R/` (auto-sourced by `R/load_components.R`).
- Format R code with [air](https://posit-dev.github.io/air/): `air format .`
- Lint with `lintr::lint_dir(".")` (config in `.lintr`).

## Testing

- Tests live in `tests/testthat/`. Run them with:

  ```r
  shiny::runTests(".")
  ```

- Add unit tests for `R/` helpers, `shiny::testServer()` tests for module
  reactivity, and `shinytest2` tests for end-to-end behavior.

## Template Mechanism

This repo is a project template. Two files drive turning it into a real project:

- `template.yml` -- the manifest: variables, find/replace targets, files to
  strip, and files to reset. This is the single source of truth.
- `dev/use_template.R` -- a generic engine that applies the manifest. It is
  called by a human (`Rscript dev/use_template.R --project_name="..."`) or
  programmatically by an initializer package (`use_template(values = ...)`).

Keep the manifest, not the engine, as the place to encode template decisions.
Template-only identity (citation files, the Zenodo badge, the "How to cite"
section) lives inside `<!-- template:strip:start --> ... :end -->` markers so it
is removed on init. If you mirror this template into a package's `inst/`, treat
that copy as a generated mirror of a tagged release -- never hand-edit it.

## Continuous Integration

Every push and pull request runs the `CI` workflow (`.github/workflows/ci.yaml`):
lint, formatting check, the test suite, and Markdown linting. Make sure these
pass locally before opening a PR.

## Pull Request Checklist

- [ ] App runs locally (`shiny::runApp()`).
- [ ] Code is formatted (`air format .`) and lints clean (`lintr::lint_dir(".")`).
- [ ] Tests pass (`shiny::runTests(".")`).
- [ ] New/changed code follows the project structure.
- [ ] README/docs updated if behavior changed.

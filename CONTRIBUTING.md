# Contributing Guidelines

## Branching

- Create feature branches from `main`.
- Open pull requests into `main`.
- Pull request titles follow [Conventional Commits](https://www.conventionalcommits.org/)
  (enforced by the `PR Title Lint` workflow).

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

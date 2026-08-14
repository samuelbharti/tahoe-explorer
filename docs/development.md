# Development Guide

## Structure

- Keep the global setup in `global.R`.
- Keep the page layouts in `userInterface/`.
- Keep the reusable module pairs in `modules/`.
- Keep the utility functions in `R/`.

`R/load_components.R` sources the files in `R/`, `modules/`, and
`userInterface/`. A new file in one of these directories needs no manual
`source()` call.

## Workflow

1. Create a branch from `main`.
2. Add the UI or the server changes to the correct directory.
3. Run the app with `shiny::runApp()`.
4. If the package versions change, restore the dependencies with `renv::restore()`.
5. Run the tests with `shiny::runTests(".")`.
6. Format the code with `air format .`.
7. Make sure that the lint is clean with `lintr::lint_dir(".")`.
8. Open a pull request into `main`.

## Tests

The tests use the synthetic fixtures only. `tests/testthat/setup.R` points
`TAHOE_METADATA_DIR` at an empty directory, so each table resolves to its
fixture. As a result, the tests need no network and no real data.

To run the tests against the real data instead, set `TAHOE_TEST_USE_REAL=1`.

There are three test layers:

- Unit tests for the functions in `R/`.
- `shiny::testServer()` tests for the reactive logic of each module.
- One `shinytest2` test that starts the app in a headless browser.

## Continuous integration

Four workflows run on GitHub Actions:

- `CI` does the lint, the format check, the tests, and the Markdown lint.
- `Secret scan` runs gitleaks over the full history of the repository.
- `PR Title Lint` makes sure that a pull request title obeys Conventional Commits.
- `Labeler` adds labels from the paths that the pull request changes.

Note: the `app` job installs `ellmer` and `shinychat`. Without these two
packages, the assistant tests skip and the code in `R/agent*.R` gets no
coverage.

## Deployment

There are two deployment paths:

- Posit Connect Cloud reads `manifest.json`. To write this file again, run
  `Rscript dev/write_manifest.R`.
- Docker builds an image from the `Dockerfile`.

For the environment variables of each path, read the README.

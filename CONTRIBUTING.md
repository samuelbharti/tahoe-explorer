# Contributing Guidelines

## Branching

- Create feature branches from `main`.
- Open pull requests into `main`.
- Give each pull request a title that obeys
  [Conventional Commits](https://www.conventionalcommits.org/). The
  `PR Title Lint` workflow enforces this rule.

## Local Setup

1. Restore the dependencies with `renv::restore()`.
2. Run the app with `shiny::runApp()`.
3. Install the git pre-commit hooks. This step is recommended:

   ```bash
   pip install pre-commit
   pre-commit install
   ```

The hooks format the R code, lint it, and scan for secrets before each commit.

## Code Style

- Keep the page UI definitions in `userInterface/`.
- Keep the reusable UI and server logic in `modules/`.
- Keep the utility functions in `R/`. `R/load_components.R` sources them.
- Format the R code with [air](https://posit-dev.github.io/air/): `air format .`
- Lint the R code with `lintr::lint_dir(".")`. The configuration is in `.lintr`.

## Testing

Run the tests with:

```r
shiny::runTests(".")
```

Add tests at the correct layer:

- Unit tests for the functions in `R/`.
- `shiny::testServer()` tests for the reactive logic of a module.
- `shinytest2` tests for behavior from end to end.

The tests use the synthetic fixtures, so they need no network and no real data.

## Continuous Integration

Every push and every pull request runs these workflows:

- `CI` does the lint, the format check, the tests, and the Markdown lint.
- `Secret scan` runs gitleaks over the full history of the repository.
- `PR Title Lint` makes sure that the pull request title obeys Conventional
  Commits.
- `Labeler` adds labels from the changed paths.

Make sure that these checks pass on your machine before you open a pull request.

## Pull Request Checklist

- [ ] The app runs locally (`shiny::runApp()`).
- [ ] The code is formatted (`air format .`) and the lint is clean
      (`lintr::lint_dir(".")`).
- [ ] The tests pass (`shiny::runTests(".")`).
- [ ] The new code obeys the project structure.
- [ ] If the behavior changed, the README and the documents are current.

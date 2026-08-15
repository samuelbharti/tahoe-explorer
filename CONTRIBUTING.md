# Contributing

Thanks for looking. This is a one-person project, so for anything large please
open an issue first: that way you get an early yes or no instead of sinking time
into work that may not land. Small fixes are welcome as a pull request straight
away.

Please also read the [Code of Conduct](CODE_OF_CONDUCT.md).

## Setup

```r
renv::restore()   # install the pinned dependencies, including ltc from GitHub
shiny::runApp()   # run the app
```

You need no data download and no network. The repository carries the six small
curated tables, and the tests use the synthetic fixtures in `data/fixtures/`.

The git hooks are optional but recommended. They format, lint, and scan for
secrets before each commit:

```bash
pip install pre-commit
pre-commit install
```

## Where code goes

- `R/` for the utilities, the data layer, and the page registry.
  `R/load_components.R` sources them for you.
- `modules/` for Shiny modules, one per feature tab.
- `userInterface/` for the page definitions, which register their own tabs.
- `dev/` for the download and fixture-generation scripts.
- `tests/testthat/` for the tests.

## Before you open a pull request

Branch from `main` and open the pull request against `main`. Give the pull
request a title in [Conventional Commits](https://www.conventionalcommits.org/)
form, such as `feat: add a coverage tab`, because a workflow checks it. Then
check that all of this passes:

```bash
air format .              # format
```

```r
lintr::lint_dir(".")      # lint, configured in .lintr
shiny::runTests(".")      # tests
shiny::runApp()           # the app still starts
```

Add a test for what you changed: a unit test for an `R/` function, a
`shiny::testServer()` test for module reactivity, or a `shinytest2` test for
end-to-end behavior. If behavior changed, update the README too.

On every push and pull request, `CI` runs the lint, the format check, the
tests, and the Markdown lint; `Secret scan` runs gitleaks over the full
history; `PR Title Lint` checks the title; and `Labeler` labels the pull
request from the paths you changed.

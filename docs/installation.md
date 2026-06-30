# Installation

## Recommended: renv

1. Install `renv` if needed.
2. Run `renv::restore()` in the project root.
3. Start the app with `shiny::runApp()`.

If you are using Docker, keep `renv.lock` and the `renv/` directory in the project root so the image can restore the project library from the lockfile.

### Quick-start helper

This template includes a helper script to initialize `renv` for a new project. Run:

```sh
Rscript dev/init-renv.R
```

This will create `renv.lock` after installing a small set of recommended packages. Review the lockfile before committing.

## Manual setup

Install required packages listed in README and run `shiny::runApp()`.

## Docker

Build:

```bash
docker build -t my-shiny-app .
```

Run:

```bash
docker run --rm -p 3838:3838 my-shiny-app
```

The Dockerfile is intended to restore from `renv.lock` rather than install packages ad hoc.

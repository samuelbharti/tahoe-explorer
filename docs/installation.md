# Installation

## Recommended: renv

1. If `renv` is not installed, install it with `install.packages("renv")`.
2. In the project root, run `renv::restore()`.
3. Start the app with `shiny::runApp()`.

`renv::restore()` installs the exact package versions from `renv.lock`. These
versions include `ltc`, which comes from GitHub and not from CRAN.

Note: keep `renv.lock` and the `renv/` directory in the project root. The Docker
image restores the project library from the lockfile.

## Manual setup

1. Install the packages that the README lists.
2. Start the app with `shiny::runApp()`.

This path does not give the exact versions from the lockfile. Use `renv` if you
need a reproducible library.

## Docker

To build the image, run:

```bash
docker build -t tahoe-explorer .
```

To start the container, run:

```bash
docker run --rm -p 3838:3838 tahoe-explorer
```

Then open [http://localhost:3838](http://localhost:3838).

The `Dockerfile` restores the library from `renv.lock`. It does not install
packages one at a time.

Note: the image carries the small curated tables and the synthetic fixtures. It
does not carry the 2.29 GB cell-level `obs` table.

## Renv helper script

`dev/init-renv.R` initializes `renv` for a new project. To run it:

```sh
Rscript dev/init-renv.R
```

The script writes `renv.lock` after it installs a small set of packages. Read
the lockfile before you commit it.

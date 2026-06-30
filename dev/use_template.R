# Turn this template into a real project.
#
# Usage
# -----
# Interactive (prompts for each value, in an R console):
#   source("dev/use_template.R"); use_template()
#
# Command line (pass values as flags; uses defaults for anything omitted):
#   Rscript dev/use_template.R --project_name="My App" --author="Jane Doe"
#
# Programmatic (e.g. from an initializer package):
#   source("dev/use_template.R")
#   use_template(
#     values = list(project_name = "My App", author = "Jane Doe"),
#     interactive = FALSE
#   )
#
# Reads `template.yml` for the list of variables, substitutions, files to strip,
# and files to reset, then applies them in `root`. After a successful run the
# template machinery (template.yml + this script) is removed, unless
# `keep_template_files = TRUE` (or `--keep` on the command line).

# Standard null-coalescing operator.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x # nolint: object_name_linter.

# Read and lightly validate the manifest.
read_manifest <- function(path) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    message("Installing 'yaml' (needed to read the template manifest)...")
    install.packages("yaml", repos = "https://cloud.r-project.org")
  }
  if (!file.exists(path)) {
    stop("Template manifest not found: ", path, call. = FALSE)
  }
  yaml::read_yaml(path)
}

# Resolve each declared variable from `values`, else prompt (when interactive),
# else fall back to the manifest default.
resolve_values <- function(manifest, values, interactive) {
  vars <- manifest$variables %||% list()
  out <- list()
  for (name in names(vars)) {
    default <- vars[[name]]$default %||% ""
    if (!is.null(values[[name]])) {
      out[[name]] <- values[[name]]
    } else if (isTRUE(interactive)) {
      prompt <- vars[[name]]$prompt %||% name
      answer <- readline(sprintf("%s [%s]: ", prompt, default))
      out[[name]] <- if (nzchar(answer)) answer else default
    } else {
      out[[name]] <- default
    }
  }
  out
}

# Expand {{var}} placeholders in a string using resolved values.
expand_vars <- function(text, values) {
  for (name in names(values)) {
    text <- gsub(paste0("{{", name, "}}"), values[[name]], text, fixed = TRUE)
  }
  text
}

apply_substitutions <- function(manifest, values, root) {
  for (sub in manifest$substitutions %||% list()) {
    path <- file.path(root, sub$file)
    if (!file.exists(path)) {
      message("  skip (missing): ", sub$file)
      next
    }
    contents <- readLines(path, warn = FALSE)
    target <- expand_vars(sub$to, values)
    if (!any(grepl(sub$from, contents, fixed = TRUE))) {
      message("  skip (no match in ", sub$file, "): ", sub$from)
      next
    }
    contents <- gsub(sub$from, target, contents, fixed = TRUE)
    writeLines(contents, path)
    message("  set ", sub$file, ": ", sub$from, " -> ", target)
  }
}

strip_files <- function(manifest, root) {
  for (file in manifest$strip_files %||% character()) {
    path <- file.path(root, file)
    if (file.exists(path)) {
      unlink(path)
      message("  removed ", file)
    }
  }
}

# Drop everything between (and including) the strip markers.
strip_blocks <- function(manifest, root) {
  start <- "<!-- template:strip:start -->"
  end <- "<!-- template:strip:end -->"
  for (file in manifest$strip_blocks %||% character()) {
    path <- file.path(root, file)
    if (!file.exists(path)) {
      next
    }
    lines <- readLines(path, warn = FALSE)
    keep <- rep(TRUE, length(lines))
    inside <- FALSE
    for (i in seq_along(lines)) {
      trimmed <- trimws(lines[i])
      if (identical(trimmed, start)) {
        inside <- TRUE
      }
      if (inside) {
        keep[i] <- FALSE
      }
      if (identical(trimmed, end)) {
        inside <- FALSE
      }
    }
    if (any(!keep)) {
      # Collapse runs of blank lines left behind by removed blocks.
      result <- lines[keep]
      blank <- !nzchar(trimws(result))
      drop <- blank & c(FALSE, head(blank, -1))
      writeLines(result[!drop], path)
      message("  stripped template blocks from ", file)
    }
  }
}

reset_changelog <- function(path) {
  fresh <- c(
    "# Changelog",
    "",
    "All notable changes to this project should be documented in this file.",
    "",
    "## [Unreleased]",
    "",
    "- Initial project setup."
  )
  writeLines(fresh, path)
  message("  reset ", basename(path))
}

apply_resets <- function(manifest, root) {
  changelog <- manifest$reset$changelog %||% NULL
  if (!is.null(changelog)) {
    path <- file.path(root, changelog)
    if (file.exists(path)) {
      reset_changelog(path)
    }
  }
}

self_remove <- function(manifest, root) {
  for (file in manifest$self_remove %||% character()) {
    path <- file.path(root, file)
    if (file.exists(path)) {
      unlink(path)
      message("  removed template machinery: ", file)
    }
  }
}

#' Convert the template in `root` into a real project.
#'
#' @param values Named list of variable values (overrides prompts/defaults).
#' @param manifest Path to the manifest, relative to `root`.
#' @param root Project root directory.
#' @param interactive Whether to prompt for missing values.
#' @param keep_template_files Keep `template.yml` and this script afterwards.
use_template <- function(
  values = list(),
  manifest = "template.yml",
  root = ".",
  interactive = base::interactive(),
  keep_template_files = FALSE
) {
  manifest_data <- read_manifest(file.path(root, manifest))
  values <- resolve_values(manifest_data, values, interactive)

  message("Applying template with:")
  for (name in names(values)) {
    message("  - ", name, ": ", values[[name]] %||% "<empty>")
  }

  apply_substitutions(manifest_data, values, root)
  strip_blocks(manifest_data, root)
  strip_files(manifest_data, root)
  apply_resets(manifest_data, root)
  if (!isTRUE(keep_template_files)) {
    self_remove(manifest_data, root)
  }

  message("Done. Review the changes, then commit your new project.")
  invisible(values)
}

# Command-line entry point: parse --key=value flags and --keep.
parse_cli_args <- function(args) {
  keep <- "--keep" %in% args
  args <- args[args != "--keep"]
  values <- list()
  for (arg in args) {
    if (grepl("^--[^=]+=", arg)) {
      key <- sub("^--([^=]+)=.*$", "\\1", arg)
      val <- sub("^--[^=]+=", "", arg)
      values[[key]] <- val
    }
  }
  list(values = values, keep = keep)
}

if (sys.nframe() == 0L) {
  cli <- parse_cli_args(commandArgs(trailingOnly = TRUE))
  use_template(
    values = cli$values,
    interactive = FALSE,
    keep_template_files = cli$keep
  )
}

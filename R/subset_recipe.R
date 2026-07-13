# Shared subset-recipe logic.
#
# The Subset Builder tab and the Chat assistant's `build_subset_recipe` tool must
# produce the SAME reproducible pull recipe for the same selection, so the pure
# logic lives here and both call it. Extracted verbatim from the subset builder's
# recipe() reactive; the string output is unchanged (guarded by
# tests/testthat/test-subset-builder.R).

# Public obs parquet on HuggingFace — the source the generated code reads from,
# so the snippets run without pre-downloading anything. Pinned to the dataset
# revision (defined in R/data.R, sourced before this file).
.subset_obs_hf <- sprintf(
  "hf://datasets/%s@%s/metadata/obs_metadata.parquet",
  .tahoe_dataset_repo,
  .tahoe_dataset_revision
)

# Approximate compressed bytes per cell in the obs parquet (~2.29 GB / 100.6M
# cells), used for the subset size estimate.
.subset_obs_bytes_per_cell <- 23

# Format an integer for display, with an em dash for NA / unknown.
.subset_fmt <- function(x) {
  if (length(x) != 1 || is.na(x)) "—" else format(x, big.mark = ",")
}

# Render a character vector as a single-quoted SQL IN list, e.g. ('a', 'b').
.subset_sql_vec <- function(x) {
  vals <- paste(
    vapply(
      x,
      function(v) paste0("'", gsub("'", "''", v, fixed = TRUE), "'"),
      character(1)
    ),
    collapse = ", "
  )
  paste0("(", vals, ")")
}

# Render a character vector as a Python list literal, e.g. ["a", "b"].
.subset_py_list <- function(x) {
  esc <- gsub('"', '\\\\"', x, perl = TRUE)
  paste0("[", paste(sprintf('"%s"', esc), collapse = ", "), "]")
}

# Render a numeric vector as a Python list literal, e.g. [0.05, 5].
.subset_py_num <- function(x) {
  paste0("[", paste(format(x, trim = TRUE), collapse = ", "), "]")
}

# Coerce a loose selection list to clean, typed vectors. The module passes
# already-clean reactive values (a no-op here); the agent tool may pass NULLs or
# stray types, so normalise defensively without reordering.
.subset_normalize_sel <- function(sel) {
  chr <- function(x) {
    x <- as.character(x %||% character())
    x[!is.na(x) & nzchar(x)]
  }
  num <- function(x) {
    x <- suppressWarnings(as.numeric(x %||% numeric()))
    x[!is.na(x)]
  }
  list(
    organs = chr(sel$organs),
    drivers = chr(sel$drivers),
    cell_lines = chr(sel$cell_lines),
    drugs = chr(sel$drugs),
    doses = num(sel$doses),
    plates = chr(sel$plates)
  )
}

#' Assayed cell lines implied by the organ + driver + explicit-cell-line
#' selection, intersected with the lines actually present in `grid`. Pure; used
#' by both the Subset Builder's matched_cell_names() reactive and
#' tahoe_subset_recipe(). `sel` is a list(organs, drivers, cell_lines, ...),
#' `grid` the cell grid (needs cell_name, organ), `lines_tbl` the driver-level
#' cell-line table (needs cell_name, Driver_Gene_Symbol).
tahoe_subset_matched_lines <- function(sel, grid, lines_tbl) {
  out <- unique(grid$cell_name)
  if (length(sel$organs) > 0 && "organ" %in% names(grid)) {
    out <- intersect(out, unique(grid$cell_name[grid$organ %in% sel$organs]))
  }
  if (
    length(sel$drivers) > 0 &&
      all(c("cell_name", "Driver_Gene_Symbol") %in% names(lines_tbl))
  ) {
    hit <- unique(
      lines_tbl$cell_name[lines_tbl$Driver_Gene_Symbol %in% sel$drivers]
    )
    out <- intersect(out, hit)
  }
  if (length(sel$cell_lines) > 0) {
    out <- intersect(out, sel$cell_lines)
  }
  out
}

# Cells in the grid after the matched-line + drug/dose/plate filters (mirrors the
# module's grid_filtered() reactive).
.subset_filtered_cells <- function(sel, grid, lines) {
  if (nrow(grid) == 0) {
    return(0L)
  }
  g <- grid[grid$cell_name %in% lines, , drop = FALSE]
  if (length(sel$drugs) > 0) {
    g <- g[g$drug %in% sel$drugs, , drop = FALSE]
  }
  if (length(sel$doses) > 0) {
    g <- g[!is.na(g$conc) & g$conc %in% sel$doses, , drop = FALSE]
  }
  if (length(sel$plates) > 0) {
    g <- g[g$plate %in% sel$plates, , drop = FALSE]
  }
  as.integer(sum(g$n_cells, na.rm = TRUE))
}

# Samples matching the drug/dose/plate dimensions (cell lines are pooled across
# all samples), as a count. Mirrors the module's matched_samples() reactive.
.subset_matched_sample_count <- function(sel, samples) {
  df <- samples
  keep <- rep(TRUE, nrow(df))
  if (length(sel$drugs) > 0 && "drug" %in% names(df)) {
    keep <- keep & df$drug %in% sel$drugs
  }
  if (length(sel$plates) > 0 && "plate" %in% names(df)) {
    keep <- keep & df$plate %in% sel$plates
  }
  if (length(sel$doses) > 0 && "drugname_drugconc" %in% names(df)) {
    conc <- tahoe_parse_dose(df$drugname_drugconc)$conc
    keep <- keep & !is.na(conc) & conc %in% sel$doses
  }
  sum(keep)
}

#' Build the reproducible subset recipe (R duckdb + Python scanpy) and size
#' estimate for a selection across the six subset dimensions. Returns
#' list(recipe, cells, samples, obs_mb). `sel` is a list(organs, drivers,
#' cell_lines, drugs, doses, plates); the data tables default to the cached data
#' layer so the agent tool can call it with just a selection, while the Subset
#' Builder passes its (possibly async) reactive grid + cell-line table.
tahoe_subset_recipe <- function(
  sel,
  grid = tahoe_cell_grid(),
  lines_tbl = tahoe_cell_line(),
  samples = tahoe_sample()
) {
  sel <- .subset_normalize_sel(sel)
  drugs <- sel$drugs
  doses <- sel$doses
  plates <- sel$plates
  # Only constrain cell lines when the resolved set is a strict subset.
  all_lines <- unique(grid$cell_name)
  lines <- tahoe_subset_matched_lines(sel, grid, lines_tbl)
  constrain_lines <- length(lines) > 0 &&
    length(lines) < length(all_lines)

  cells <- .subset_filtered_cells(sel, grid, lines)
  n_samp <- .subset_matched_sample_count(sel, samples)
  obs_mb <- round(cells * .subset_obs_bytes_per_cell / 1e6)
  est <- list(cells = cells, samples = n_samp, obs_mb = obs_mb)

  if (
    length(drugs) == 0 &&
      length(doses) == 0 &&
      length(plates) == 0 &&
      !constrain_lines
  ) {
    return(list(
      recipe = paste(
        "No filters selected: this recipe would return the full dataset.",
        "Pick a tissue, driver, cell line, drug, dose, or plate to build a",
        "reproducible subset predicate."
      ),
      cells = cells,
      samples = n_samp,
      obs_mb = obs_mb
    ))
  }

  r_where <- character()
  py_where <- character()
  if (length(drugs) > 0) {
    r_where <- c(r_where, sprintf("drug IN %s", .subset_sql_vec(drugs)))
    py_where <- c(
      py_where,
      sprintf('df["drug"].isin(%s)', .subset_py_list(drugs))
    )
  }
  if (constrain_lines) {
    r_where <- c(
      r_where,
      sprintf("cell_name IN %s", .subset_sql_vec(lines))
    )
    py_where <- c(
      py_where,
      sprintf('df["cell_name"].isin(%s)', .subset_py_list(lines))
    )
  }
  if (length(plates) > 0) {
    r_where <- c(r_where, sprintf("plate IN %s", .subset_sql_vec(plates)))
    py_where <- c(
      py_where,
      sprintf('df["plate"].isin(%s)', .subset_py_list(plates))
    )
  }
  if (length(doses) > 0) {
    r_where <- c(
      r_where,
      sprintf(
        paste0(
          "TRY_CAST(regexp_extract(drugname_drugconc, ",
          "',\\s*([0-9.eE+-]+)\\s*,', 1) AS DOUBLE) IN (%s)"
        ),
        paste(format(doses, trim = TRUE), collapse = ", ")
      )
    )
    py_where <- c(
      py_where,
      sprintf(
        paste0(
          'df["drugname_drugconc"].str.extract(',
          'r",\\s*([0-9.eE+-]+)\\s*,")[0].astype(float).isin(%s)'
        ),
        .subset_py_num(doses)
      )
    )
  }

  r_predicate <- paste(r_where, collapse = "\n    AND ")
  py_predicate <- paste(py_where, collapse = "\n    & ")

  header <- paste(
    "# ── Estimated subset ─────────────────────────────────────────",
    sprintf(
      "# ~%s cells across %s samples · ~%s MB of obs metadata to scan.",
      .subset_fmt(est$cells),
      .subset_fmt(est$samples),
      .subset_fmt(est$obs_mb)
    ),
    "# The expression matrix is downloaded separately and is larger.",
    sprintf("# Source: %s", .subset_obs_hf),
    sep = "\n"
  )
  r_snippet <- paste(
    "## R (duckdb) — pull the subset's cell-level metadata ---------",
    "library(duckdb); library(DBI)",
    "con <- dbConnect(duckdb())",
    'dbExecute(con, "INSTALL httpfs; LOAD httpfs;")',
    "obs <- dbGetQuery(con, \"",
    "  SELECT *",
    sprintf("  FROM read_parquet('%s')", .subset_obs_hf),
    sprintf("  WHERE %s", r_predicate),
    "\")",
    "dbDisconnect(con, shutdown = TRUE)",
    sep = "\n"
  )
  py_snippet <- paste(
    "## Python (scanpy / AnnData) — subset cells for analysis ------",
    "import pandas as pd, scanpy as sc",
    sprintf('obs = pd.read_parquet(\n    "%s"\n)', .subset_obs_hf),
    sprintf("mask = (\n    %s\n)", gsub("df\\[", "obs[", py_predicate)),
    'cells = obs.loc[mask, "BARCODE_SUB_LIB_ID"]',
    "",
    "# Point `adata` at the Tahoe-100M expression AnnData, then keep",
    "# only these cells (obs_names are the BARCODE_SUB_LIB_ID values):",
    '# adata = sc.read_h5ad("<tahoe_expression.h5ad>", backed="r")',
    "# adata = adata[adata.obs_names.isin(cells)].to_memory()",
    sep = "\n"
  )
  list(
    recipe = paste(header, "", r_snippet, "", py_snippet, sep = "\n"),
    cells = cells,
    samples = n_samp,
    obs_mb = obs_mb
  )
}

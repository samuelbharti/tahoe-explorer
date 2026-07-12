# Build the per (drug x cell line x plate x dose) cell-count grid from a locally
# downloaded obs_metadata.parquet. This is the small (~66k-row) aggregate the
# app reads for live cell counts / coverage / QC, so it never re-scans the
# 2.29 GB obs at runtime.
#
# Run this after dev/download_metadata.R --obs (or any run that placed the real
# obs_metadata.parquet in the data dir):
#
#   Rscript dev/build_grid.R
#
# Override the data dir with TAHOE_METADATA_DIR (defaults to "data"). Requires
# the duckdb package. The query matches the one in dev/download_metadata.R so
# the grid is identical however it is produced.

dest_dir <- Sys.getenv("TAHOE_METADATA_DIR", unset = "data")
obs_file <- file.path(dest_dir, "obs_metadata.parquet")
grid_dest <- file.path(dest_dir, "obs_cell_grid.parquet")

if (!file.exists(obs_file)) {
  stop(
    "No obs_metadata.parquet in '",
    dest_dir,
    "'. ",
    "Download it first: Rscript dev/download_metadata.R --obs",
    call. = FALSE
  )
}
if (!requireNamespace("duckdb", quietly = TRUE)) {
  stop("The duckdb package is required to build the grid.", call. = FALSE)
}

fmt_size <- function(bytes) {
  units <- c("B", "KB", "MB", "GB")
  i <- if (bytes <= 0) 1 else min(length(units), floor(log(bytes, 1024)) + 1)
  sprintf("%.1f %s", bytes / 1024^(i - 1), units[i])
}

cat(sprintf(
  "Building cell-count grid from %s (%s) ...\n",
  obs_file,
  fmt_size(file.info(obs_file)$size)
))

con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

quote_sql <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")

# Extended grid: cell counts plus the QC / cell-cycle / QC-metric aggregates the
# app needs so it never has to re-scan the 2.29 GB obs. Kept identical to the
# fallback query in R/data.R so the grid is the same however it is built.
DBI::dbExecute(
  con,
  sprintf(
    paste0(
      "COPY (SELECT drug, cell_name, plate, TRY_CAST(regexp_extract(",
      "drugname_drugconc, ',\\s*([0-9.eE+-]+)\\s*,', 1) AS DOUBLE) AS conc, ",
      "count(*) AS n_cells, ",
      "count(*) FILTER (WHERE pass_filter = 'full') AS n_full, ",
      "count(*) FILTER (WHERE phase = 'G1') AS n_g1, ",
      "count(*) FILTER (WHERE phase = 'S') AS n_s, ",
      "count(*) FILTER (WHERE phase = 'G2M') AS n_g2m, ",
      "sum(pcnt_mito) AS sum_pcnt_mito, ",
      "sum(gene_count) AS sum_gene_count, ",
      "sum(tscp_count) AS sum_tscp_count ",
      "FROM read_parquet(%s) GROUP BY 1, 2, 3, 4) TO %s (FORMAT PARQUET)"
    ),
    quote_sql(obs_file),
    quote_sql(grid_dest)
  )
)

# Report what was built so the numbers can be sanity-checked against the app.
summ <- DBI::dbGetQuery(
  con,
  sprintf(
    paste0(
      "SELECT count(*) AS grid_rows, sum(n_cells) AS total_cells, ",
      "sum(n_full) AS full_cells, ",
      "sum(n_g1) AS g1, sum(n_s) AS s, sum(n_g2m) AS g2m, ",
      "count(DISTINCT cell_name) AS cell_lines, ",
      "count(DISTINCT drug) AS drugs, count(DISTINCT plate) AS plates ",
      "FROM read_parquet(%s)"
    ),
    quote_sql(grid_dest)
  )
)

pct <- function(a, b) if (b > 0) sprintf("%.1f%%", 100 * a / b) else "—"
cat(sprintf(
  paste0(
    "  done (%s)\n",
    "  grid rows: %s | total cells: %s | cell lines: %s | drugs: %s | plates: %s\n",
    "  QC-passing (full): %s (%s) | phase G1/S/G2M: %s / %s / %s\n"
  ),
  fmt_size(file.info(grid_dest)$size),
  format(summ$grid_rows, big.mark = ","),
  format(summ$total_cells, big.mark = ","),
  summ$cell_lines,
  summ$drugs,
  summ$plates,
  format(summ$full_cells, big.mark = ","),
  pct(summ$full_cells, summ$total_cells),
  format(summ$g1, big.mark = ","),
  format(summ$s, big.mark = ","),
  format(summ$g2m, big.mark = ",")
))

# Download external somatic-variant calls for the Tahoe-100M cell lines and
# vendor them as a small local table the app joins on cell_name.
#
# Primary source: Broad Institute DepMap, 24Q4 Public, OmicsSomaticMutations.csv
#   (full somatic profiles; one row per variant per model, keyed by DepMap id).
#   Portal: https://depmap.org/portal   DOI: 10.25452/figshare.plus.27993248.v1
#   License: CC BY 4.0. Cite: DepMap, Broad (2024). DepMap 24Q4 Public. Figshare+.
# Fallback: Cellosaurus (https://www.cellosaurus.org), CC BY 4.0 -- curated driver
#   variants (by CVCL_ accession) for the lines DepMap does not cover, so every
#   assayed line that has known variants is represented.
#
# We download the full DepMap mutations CSV (~339 MB) to a temp dir, filter it to
# the cell lines Tahoe assayed (via their DepMap id -> cell_name), add the curated
# Cellosaurus variants for the remaining lines, and write a focused
# data/cell_line_variants.parquet (a couple of MB). The big CSV is deleted; only
# the small table is kept. The data dir is gitignored, so real data is never
# committed (only the synthetic fixture under data/fixtures/ is).
#
# Usage (from the app root; needs duckdb, and jsonlite for the Cellosaurus step):
#   Rscript dev/download_variants.R
#
# Override the data dir with TAHOE_METADATA_DIR (defaults to "data").

# Pinned release for reproducibility. figshare hosting stopped after 24Q4; newer
# DepMap releases are portal/AWS-only (signed URLs), so pin by release + md5.
depmap_release <- "24Q4"
mutations_url <- "https://ndownloader.figshare.com/files/51065732"
mutations_md5 <- "7bdba347a1602fe96d5654a74d6e52f1"
cellosaurus_api <- "https://api.cellosaurus.org/cell-line/"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

if (!requireNamespace("duckdb", quietly = TRUE)) {
  stop(
    "The duckdb package is required to build the variant table.",
    call. = FALSE
  )
}

dest_dir <- Sys.getenv("TAHOE_METADATA_DIR", unset = "data")
dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(dest_dir, "cell_line_variants.parquet")

# Cell-line table that carries the DepMap / Cellosaurus ids and cell_name: real
# if downloaded, else the synthetic fixture (so a demo run still works).
cell_line_file <- file.path(dest_dir, "cell_line_metadata.parquet")
if (!file.exists(cell_line_file)) {
  cell_line_file <- file.path(
    dest_dir,
    "fixtures",
    "cell_line_metadata.parquet"
  )
}
if (!file.exists(cell_line_file)) {
  cell_line_file <- file.path("data", "fixtures", "cell_line_metadata.parquet")
}
if (!file.exists(cell_line_file)) {
  stop(
    "No cell_line_metadata.parquet found to source cell ids from.",
    call. = FALSE
  )
}

fmt_size <- function(bytes) {
  units <- c("B", "KB", "MB", "GB")
  i <- if (bytes <= 0) 1 else min(length(units), floor(log(bytes, 1024)) + 1)
  sprintf("%.1f %s", bytes / 1024^(i - 1), units[i])
}

quote_sql <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")

# The mutations CSV is large; give the whole transfer up to an hour, and stage
# it (and the intermediate DepMap parquet) in a temp dir so nothing big lands in
# the (possibly synced) data dir.
options(timeout = max(3600, getOption("timeout")))
csv_path <- file.path(tempdir(), "OmicsSomaticMutations.csv")
dm_path <- file.path(tempdir(), "depmap_variants.parquet")

cat(sprintf(
  "Downloading DepMap %s OmicsSomaticMutations.csv (~339 MB) ...\n",
  depmap_release
))
ok <- tryCatch(
  {
    utils::download.file(mutations_url, csv_path, mode = "wb", quiet = TRUE)
    TRUE
  },
  error = function(e) {
    cat(sprintf("  FAILED: %s\n", conditionMessage(e)))
    FALSE
  }
)
if (!ok && file.exists(csv_path)) {
  unlink(csv_path)
}
if (!ok || !file.exists(csv_path)) {
  stop("Download failed; see message above.", call. = FALSE)
}
on.exit(unlink(c(csv_path, dm_path)), add = TRUE)
cat(sprintf("  done (%s)\n", fmt_size(file.info(csv_path)$size)))

got_md5 <- unname(tools::md5sum(csv_path))
if (!identical(got_md5, mutations_md5)) {
  msg <- sprintf(
    "md5 %s does not match the pinned %s for DepMap %s.",
    got_md5,
    mutations_md5,
    depmap_release
  )
  if (nzchar(Sys.getenv("TAHOE_ALLOW_MD5_MISMATCH"))) {
    cat(sprintf(
      "  WARNING: %s Continuing (TAHOE_ALLOW_MD5_MISMATCH set).\n",
      msg
    ))
  } else {
    stop(
      sprintf(
        paste(
          "%s Refusing to build an unpinned table. Set",
          "TAHOE_ALLOW_MD5_MISMATCH=1 to override (e.g. to use a newer release)."
        ),
        msg
      ),
      call. = FALSE
    )
  }
} else {
  cat("  md5 verified against the pinned release.\n")
}

con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

# DepMap: read the CSV as all-varchar (so True/False parse predictably), join to
# the cell-line table to attach cell_name, keep a focused, renamed set. Boolean
# flags derive from the "True" string; blank annotations become NULL.
cat("Filtering DepMap to Tahoe cell lines ...\n")
DBI::dbExecute(
  con,
  sprintf(
    paste0(
      "COPY (SELECT c.cell_name, 'DepMap' AS source, m.HugoSymbol AS gene, ",
      "nullif(m.ProteinChange, '') AS protein_change, ",
      "nullif(m.DNAChange, '') AS dna_change, ",
      "m.VariantType AS variant_type, ",
      "nullif(m.VariantInfo, '') AS consequence, ",
      "(m.Hotspot = 'True') AS hotspot, ",
      "(m.LikelyLoF = 'True') AS likely_lof, ",
      "nullif(m.DbsnpRsID, '') AS dbsnp, ",
      "CAST(NULL AS VARCHAR) AS zygosity ",
      "FROM read_csv_auto(%s, all_varchar = true) m ",
      "JOIN (SELECT DISTINCT cell_name, Cell_ID_DepMap FROM read_parquet(%s) ",
      "WHERE Cell_ID_DepMap LIKE 'ACH-%%') c ON m.ModelID = c.Cell_ID_DepMap) ",
      "TO %s (FORMAT PARQUET)"
    ),
    quote_sql(csv_path),
    quote_sql(cell_line_file),
    quote_sql(dm_path)
  )
)
covered <- DBI::dbGetQuery(
  con,
  sprintf("SELECT DISTINCT cell_name FROM read_parquet(%s)", quote_sql(dm_path))
)$cell_name
cat(sprintf("  DepMap covers %d cell lines.\n", length(covered)))

# Cellosaurus fallback: curated driver variants for the lines DepMap missed.
# Best-effort -- skipped (with a note) if jsonlite is unavailable or a fetch fails,
# so the DepMap table is still produced.
cello_df <- NULL
lines <- DBI::dbGetQuery(
  con,
  sprintf(
    paste0(
      "SELECT DISTINCT cell_name, Cell_ID_Cellosaur FROM read_parquet(%s) ",
      "WHERE Cell_ID_Cellosaur LIKE 'CVCL_%%'"
    ),
    quote_sql(cell_line_file)
  )
)
uncovered <- lines[!lines$cell_name %in% covered, , drop = FALSE]

parse_cellosaurus <- function(cell_name, rec) {
  cl <- rec[["Cellosaurus"]][["cell-line-list"]]
  if (length(cl) == 0) {
    return(NULL)
  }
  svs <- cl[[1]][["sequence-variation-list"]]
  if (length(svs) == 0) {
    return(NULL)
  }
  rows <- lapply(svs, function(sv) {
    desc <- sv[["mutation-description"]] %||% ""
    mtype <- sv[["mutation-type"]] %||% ""
    # Skip lines reported as wild-type / no variant.
    if (identical(mtype, "None_reported") || desc %in% c("", "-")) {
      return(NULL)
    }
    gene <- NA_character_
    for (xr in sv[["xref-list"]] %||% list()) {
      if (identical(xr[["database"]], "HGNC")) {
        gene <- xr[["label"]] %||% NA_character_
        break
      }
    }
    pm <- regmatches(desc, regexpr("p\\.[A-Za-z0-9*]+", desc))
    cm <- regmatches(desc, regexpr("c\\.[^) ]+", desc))
    zt <- sv[["zygosity-type"]]
    data.frame(
      cell_name = cell_name,
      source = "Cellosaurus",
      gene = gene,
      protein_change = if (length(pm)) pm else NA_character_,
      dna_change = if (length(cm)) cm else NA_character_,
      variant_type = sv[["variation-type"]] %||% NA_character_,
      consequence = if (nzchar(mtype)) mtype else NA_character_,
      hotspot = FALSE,
      likely_lof = FALSE,
      dbsnp = NA_character_,
      zygosity = if (!is.null(zt) && !identical(zt, "-")) zt else NA_character_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

if (nrow(uncovered) > 0 && requireNamespace("jsonlite", quietly = TRUE)) {
  cat(sprintf(
    "Fetching Cellosaurus variants for %d line(s) DepMap missed ...\n",
    nrow(uncovered)
  ))
  cello_df <- tryCatch(
    {
      parts <- lapply(seq_len(nrow(uncovered)), function(i) {
        cvcl <- uncovered$Cell_ID_Cellosaur[i]
        url <- sprintf(
          "%s%s?format=json&fields=ac,id,var,dr",
          cellosaurus_api,
          cvcl
        )
        rec <- tryCatch(
          jsonlite::fromJSON(url, simplifyVector = FALSE),
          error = function(e) NULL
        )
        if (is.null(rec)) {
          cat(sprintf("  %s: fetch failed\n", cvcl))
          return(NULL)
        }
        parse_cellosaurus(uncovered$cell_name[i], rec)
      })
      do.call(rbind, parts)
    },
    error = function(e) {
      cat(sprintf("  Cellosaurus step skipped: %s\n", conditionMessage(e)))
      NULL
    }
  )
  if (!is.null(cello_df)) {
    cat(sprintf("  added %d curated Cellosaurus variant(s).\n", nrow(cello_df)))
  }
} else if (nrow(uncovered) > 0) {
  cat("Cellosaurus fallback skipped (jsonlite not installed).\n")
}

# Combine DepMap + Cellosaurus into the final table (UNION ALL BY NAME aligns the
# columns regardless of order); DepMap-only if the fallback produced nothing.
cat("Writing", out_path, "...\n")
if (!is.null(cello_df) && nrow(cello_df) > 0) {
  duckdb::duckdb_register(con, "cello_tmp", cello_df)
  DBI::dbExecute(
    con,
    sprintf(
      paste0(
        "COPY (SELECT * FROM read_parquet(%s) UNION ALL BY NAME ",
        "SELECT * FROM cello_tmp) TO %s (FORMAT PARQUET)"
      ),
      quote_sql(dm_path),
      quote_sql(out_path)
    )
  )
  duckdb::duckdb_unregister(con, "cello_tmp")
} else {
  DBI::dbExecute(
    con,
    sprintf(
      "COPY (SELECT * FROM read_parquet(%s)) TO %s (FORMAT PARQUET)",
      quote_sql(dm_path),
      quote_sql(out_path)
    )
  )
}

summ <- DBI::dbGetQuery(
  con,
  sprintf(
    paste0(
      "SELECT count(*) AS variants, count(DISTINCT cell_name) AS lines, ",
      "count(*) FILTER (WHERE source = 'DepMap') AS depmap, ",
      "count(*) FILTER (WHERE source = 'Cellosaurus') AS cellosaurus ",
      "FROM read_parquet(%s)"
    ),
    quote_sql(out_path)
  )
)
cat(sprintf(
  "  done (%s)\n  %s variants across %s cell lines (DepMap %s + Cellosaurus %s)\n",
  fmt_size(file.info(out_path)$size),
  format(summ$variants, big.mark = ","),
  summ$lines,
  format(summ$depmap, big.mark = ","),
  format(summ$cellosaurus, big.mark = ",")
))
cat("\nSources: DepMap", depmap_release, "+ Cellosaurus (both CC BY 4.0)\n")

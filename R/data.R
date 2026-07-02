# Data access layer for Tahoe-100M metadata.
#
# Every module reads through these helpers rather than touching files directly.
# Resolution order for each table is: real file in the data dir -> bundled
# synthetic fixture. The app therefore always runs, even with no network and no
# downloaded data. See docs/plans/00-architecture.md.

# Small in-process cache so repeated reactive reads are cheap.
.tahoe_cache <- new.env(parent = emptyenv())

# App root, captured when this file is sourced. global.R is always loaded with
# the working directory at the app root (by runApp, and by the tests via
# source(chdir = TRUE)), so anchoring data paths here keeps them correct even
# when tests later run from tests/testthat/.
.tahoe_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

# Columns we allow callers to group/filter on in the cell-level obs queries.
# Whitelisting keeps the generated SQL safe and predictable.
.tahoe_obs_group_cols <- c(
  "drug",
  "cell_line",
  "cell_name",
  "plate",
  "sample",
  "phase",
  "pass_filter",
  "sublibrary"
)

# Supported obs aggregation metrics mapped to their SQL expressions.
.tahoe_obs_metrics <- c(
  n_cells = "count(*)",
  mean_pcnt_mito = "avg(pcnt_mito)",
  mean_tscp_count = "avg(tscp_count)",
  mean_gene_count = "avg(gene_count)",
  mean_mread_count = "avg(mread_count)"
)

# Remote location of the cell-level obs file (used only when opted in).
.tahoe_obs_remote_url <- paste0(
  "https://huggingface.co/datasets/vevotx/Tahoe-100M/",
  "resolve/main/metadata/obs_metadata.parquet"
)

#' Directory holding downloaded metadata (TAHOE_METADATA_DIR or "data").
tahoe_data_dir <- function() {
  env <- Sys.getenv("TAHOE_METADATA_DIR", unset = "")
  if (nzchar(env) && dir.exists(env)) {
    return(env)
  }
  file.path(.tahoe_root, "data")
}

#' Directory holding the committed synthetic fixtures.
tahoe_fixture_dir <- function() {
  file.path(.tahoe_root, "data", "fixtures")
}

#' Resolve a small table to a real file if present, else its fixture.
#' Returns a list(path, source) where source is "real" or "fixture".
tahoe_table_path <- function(name) {
  real <- file.path(tahoe_data_dir(), paste0(name, ".parquet"))
  if (file.exists(real)) {
    return(list(path = real, source = "real"))
  }
  list(
    path = file.path(tahoe_fixture_dir(), paste0(name, ".parquet")),
    source = "fixture"
  )
}

#' Cached duckdb connection for the app's lifetime.
tahoe_con <- function() {
  if (is.null(.tahoe_cache$con) || !DBI::dbIsValid(.tahoe_cache$con)) {
    .tahoe_cache$con <- DBI::dbConnect(duckdb::duckdb())
    # A fresh connection has no extensions loaded; clear the httpfs flag so a
    # later remote query re-loads it instead of assuming the old connection's
    # state.
    .tahoe_cache$httpfs <- FALSE
  }
  .tahoe_cache$con
}

# Single-quote a string for safe inlining into duckdb SQL.
.tahoe_quote <- function(x) {
  paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}

#' Read a parquet (or csv) file into a tibble via duckdb.
tahoe_read_file <- function(path) {
  reader <- if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    "read_csv_auto"
  } else {
    "read_parquet"
  }
  sql <- sprintf("SELECT * FROM %s(%s)", reader, .tahoe_quote(path))
  dplyr::as_tibble(DBI::dbGetQuery(tahoe_con(), sql))
}

#' Load a small metadata table by name, cached. Returns a tibble plus a
#' "tahoe_source" attribute ("real" or "fixture").
tahoe_table <- function(name) {
  key <- paste0("table_", name)
  if (is.null(.tahoe_cache[[key]])) {
    resolved <- tahoe_table_path(name)
    df <- tahoe_read_file(resolved$path)
    attr(df, "tahoe_source") <- resolved$source
    .tahoe_cache[[key]] <- df
  }
  .tahoe_cache[[key]]
}

#' Typed wrappers for the four small tables.
tahoe_drug <- function() tahoe_table("drug_metadata")
tahoe_cell_line <- function() tahoe_table("cell_line_metadata")
tahoe_sample <- function() tahoe_table("sample_metadata")
tahoe_gene <- function() tahoe_table("gene_metadata")

#' Parse the `drugname_drugconc` string, e.g. "[('Drug', 5.0, 'uM')]", into a
#' tibble of (conc, unit). Returns NA for values it cannot parse.
tahoe_parse_dose <- function(x) {
  x <- as.character(x)
  conc <- suppressWarnings(as.numeric(
    stringr::str_match(x, ",\\s*([0-9.eE+-]+)\\s*,")[, 2]
  ))
  unit <- stringr::str_match(x, ",\\s*'([^']*)'\\s*\\)")[, 2]
  dplyr::tibble(conc = conc, unit = unit)
}

#' Where the cell-level obs data would come from given the environment.
#' Returns list(type = "local"|"remote"|"fixture", src = path-or-url).
#' Default (nothing downloaded, no env) is the synthetic fixture so the app is
#' fast, offline, and CI-safe. Set TAHOE_OBS_REMOTE=1 to query HuggingFace.
tahoe_obs_source <- function() {
  local <- file.path(tahoe_data_dir(), "obs_metadata.parquet")
  if (file.exists(local)) {
    return(list(type = "local", src = local))
  }
  remote <- Sys.getenv("TAHOE_OBS_REMOTE", unset = "")
  if (nzchar(remote) && !identical(tolower(remote), "false") && remote != "0") {
    return(list(type = "remote", src = .tahoe_obs_remote_url))
  }
  list(
    type = "fixture",
    src = file.path(tahoe_fixture_dir(), "obs_metadata.parquet")
  )
}

# Ensure httpfs is available before querying a remote parquet.
.tahoe_load_httpfs <- function() {
  if (isTRUE(.tahoe_cache$httpfs)) {
    return(invisible(TRUE))
  }
  DBI::dbExecute(tahoe_con(), "INSTALL httpfs; LOAD httpfs;")
  .tahoe_cache$httpfs <- TRUE
  invisible(TRUE)
}

# Build the WHERE clause from a whitelisted, value-escaped filter list.
.tahoe_obs_where <- function(filters) {
  filters <- filters[
    names(filters) %in%
      .tahoe_obs_group_cols &
      vapply(filters, function(v) length(v) > 0, logical(1))
  ]
  if (length(filters) == 0) {
    return("")
  }
  clauses <- vapply(
    names(filters),
    function(col) {
      vals <- paste(
        vapply(filters[[col]], .tahoe_quote, character(1)),
        collapse = ", "
      )
      sprintf("\"%s\" IN (%s)", col, vals)
    },
    character(1)
  )
  paste0(" WHERE ", paste(clauses, collapse = " AND "))
}

#' Lazily aggregate the cell-level obs data with duckdb and return a small
#' tibble. Never pulls raw cells into R. `group_by` and `metric` are
#' whitelisted; `filters` is a named list (column -> allowed values).
#'
#' Returns a tibble with columns `<group_by>` and `value`, carrying a
#' "tahoe_source" attribute. On any failure it returns a 0-row tibble with a
#' "tahoe_error" attribute rather than throwing, so the UI can degrade.
tahoe_obs_summary <- function(
  group_by,
  filters = list(),
  metric = "n_cells",
  limit = 100
) {
  stopifnot(length(group_by) == 1, group_by %in% .tahoe_obs_group_cols)
  if (!metric %in% names(.tahoe_obs_metrics)) {
    metric <- "n_cells"
  }
  source <- tahoe_obs_source()
  agg <- .tahoe_obs_metrics[[metric]]
  where <- .tahoe_obs_where(filters)
  limit_sql <- if (is.null(limit)) {
    ""
  } else {
    sprintf(" LIMIT %d", as.integer(limit))
  }
  sql <- sprintf(
    paste0(
      'SELECT "%s" AS "%s", %s AS value ',
      "FROM read_parquet(%s)%s GROUP BY 1 ORDER BY value DESC%s"
    ),
    group_by,
    group_by,
    agg,
    .tahoe_quote(source$src),
    where,
    limit_sql
  )
  out <- tryCatch(
    {
      if (identical(source$type, "remote")) {
        .tahoe_load_httpfs()
      }
      dplyr::as_tibble(DBI::dbGetQuery(tahoe_con(), sql))
    },
    error = function(e) {
      res <- dplyr::tibble(!!group_by := character(), value = numeric())
      attr(res, "tahoe_error") <- conditionMessage(e)
      res
    }
  )
  attr(out, "tahoe_source") <- source$type
  out
}

# Distinct values of a column, or nrow() if the column is missing. Counts of
# distinct entities, not raw rows: the real cell_line_metadata table is
# driver-level (many rows per cell line), so nrow() would overcount.
.tahoe_n_distinct <- function(df, column) {
  if (column %in% names(df)) {
    dplyr::n_distinct(df[[column]])
  } else {
    nrow(df)
  }
}

#' Cell-line metadata collapsed to one row per cell line. The source table has
#' one row per driver mutation, so this keeps the first row per `cell_name` and
#' summarizes the distinct driver genes into a `drivers` column (with
#' `n_drivers`). Returns the table unchanged if `cell_name` is absent.
tahoe_cell_line_unique <- function() {
  df <- tahoe_cell_line()
  if (!"cell_name" %in% names(df)) {
    return(df)
  }
  drivers <- if ("Driver_Gene_Symbol" %in% names(df)) {
    df |>
      dplyr::group_by(.data$cell_name) |>
      dplyr::summarise(
        drivers = paste(
          sort(unique(stats::na.omit(.data$Driver_Gene_Symbol))),
          collapse = ", "
        ),
        n_drivers = dplyr::n_distinct(
          stats::na.omit(.data$Driver_Gene_Symbol)
        ),
        .groups = "drop"
      )
  } else {
    NULL
  }
  out <- df[!duplicated(df$cell_name), , drop = FALSE]
  if (!is.null(drivers)) {
    out <- dplyr::left_join(out, drivers, by = "cell_name")
  }
  dplyr::as_tibble(out)
}

#' Headline dataset counts for the Overview tab. Counts distinct entities
#' (distinct cell lines, drugs, ...), not raw metadata rows.
tahoe_summary_counts <- function() {
  drug <- tahoe_drug()
  cell_line <- tahoe_cell_line()
  sample <- tahoe_sample()
  gene <- tahoe_gene()
  list(
    drugs = .tahoe_n_distinct(drug, "drug"),
    cell_lines = .tahoe_n_distinct(cell_line, "cell_name"),
    samples = .tahoe_n_distinct(sample, "sample"),
    plates = .tahoe_n_distinct(sample, "plate"),
    genes = .tahoe_n_distinct(gene, "gene_symbol"),
    obs_source = tahoe_obs_source()$type,
    data_source = attr(drug, "tahoe_source")
  )
}

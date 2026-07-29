# Unit tests for the data access layer, running against the bundled synthetic
# fixtures (no network, no real data).

test_that("small metadata tables load from fixtures", {
  drug <- tahoe_drug()
  expect_s3_class(drug, "tbl_df")
  expect_true(all(c("drug", "moa-broad", "human-approved") %in% names(drug)))
  expect_gt(nrow(drug), 0)
  expect_identical(attr(drug, "tahoe_source"), "fixture")

  cell <- tahoe_cell_line()
  expect_true(all(c("cell_name", "Organ") %in% names(cell)))
})

test_that("tahoe_parse_dose extracts concentration and unit", {
  d <- tahoe_parse_dose(c("[('X', 5.0, 'uM')]", "[('Y', 0.05, 'uM')]", "junk"))
  expect_equal(d$conc, c(5.0, 0.05, NA_real_))
  expect_equal(d$unit, c("uM", "uM", NA_character_))
})

test_that("tahoe_obs_summary aggregates and whitelists the group column", {
  res <- tahoe_obs_summary("drug", metric = "n_cells", limit = 5)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("drug", "value") %in% names(res)))
  expect_lte(nrow(res), 5)
  expect_true(all(res$value > 0))
  expect_identical(attr(res, "tahoe_source"), "fixture")

  expect_error(tahoe_obs_summary("not_a_real_column"))
})

test_that("tahoe_obs_summary applies whitelisted filters", {
  unfiltered <- tahoe_obs_summary("phase", metric = "n_cells")
  filtered <- tahoe_obs_summary(
    "phase",
    filters = list(pass_filter = "full"),
    metric = "n_cells"
  )
  expect_true(sum(filtered$value) <= sum(unfiltered$value))
})

test_that("tahoe_summary_counts returns headline fields", {
  cc <- tahoe_summary_counts()
  expect_true(all(
    c(
      "drugs",
      "cell_lines",
      "samples",
      "plates",
      "genes",
      "cells",
      "obs_source"
    ) %in%
      names(cc)
  ))
  expect_gt(cc$drugs, 0)
  # "fixture" offline/CI; "grid"/"local"/"remote" when real metadata is present.
  expect_true(cc$obs_source %in% c("fixture", "grid", "local", "remote"))
})

test_that("cell line and cell counts come from the obs data, not tables", {
  cc <- tahoe_summary_counts()

  # Assayed cell lines = distinct cell lines present in obs; total cells = sum
  # of the per-cell-line counts. Reconcile against whichever obs-level source
  # drives the counts (never the larger, driver-level cell_line_metadata table).
  if (identical(cc$obs_source, "grid")) {
    grid <- tahoe_cell_grid()
    expect_equal(cc$cell_lines, dplyr::n_distinct(grid$cell_name))
    expect_equal(cc$cells, sum(grid$n_cells))
  } else {
    by_line <- tahoe_obs_summary("cell_line", metric = "n_cells", limit = NULL)
    expect_equal(cc$cell_lines, nrow(by_line))
    expect_equal(cc$cells, sum(by_line$value))
  }
  expect_lt(cc$cell_lines, nrow(tahoe_cell_line()))
})

test_that("tahoe_cell_grid aggregates obs and reconciles with the counts", {
  grid <- tahoe_cell_grid()
  expect_true(all(
    c("drug", "cell_name", "plate", "conc", "n_cells") %in% names(grid)
  ))
  expect_gt(nrow(grid), 0)
  expect_true(all(grid$n_cells > 0))

  cc <- tahoe_summary_counts()
  # The grid is a full partition of the obs cells, so totals must reconcile.
  expect_equal(sum(grid$n_cells), cc$cells)
  expect_equal(dplyr::n_distinct(grid$cell_name), cc$cell_lines)
})

test_that("the extended grid exposes QC-tier, phase, and QC-metric columns", {
  grid <- tahoe_cell_grid()
  ext <- c(
    "n_full",
    "n_g1",
    "n_s",
    "n_g2m",
    "sum_pcnt_mito",
    "sum_gene_count",
    "sum_tscp_count"
  )
  skip_if(!all(ext %in% names(grid)), "counts-only grid (no extended columns)")

  # QC-passing cells never exceed all cells; phase counts are non-negative and
  # never exceed the total cells in a condition.
  expect_true(all(grid$n_full <= grid$n_cells))
  expect_true(all(grid$n_g1 >= 0 & grid$n_s >= 0 & grid$n_g2m >= 0))
  expect_true(all(grid$n_g1 + grid$n_s + grid$n_g2m <= grid$n_cells))

  # The coverage / conditions helpers propagate the extended aggregates and a
  # cell-weighted mean QC metric, still bounded by the total cell counts.
  cov <- tahoe_coverage()
  expect_true(all(
    c("n_full", "n_g1", "n_s", "n_g2m", "mean_pcnt_mito") %in% names(cov)
  ))
  expect_true(all(cov$n_full <= cov$n_cells))

  cond <- tahoe_conditions()
  expect_true(all(c("n_full", "mean_pcnt_mito") %in% names(cond)))
  expect_true(all(cond$n_full <= cond$n_cells))
})

test_that("tahoe_cell_line_unique collapses to one row per cell line", {
  u <- tahoe_cell_line_unique()
  expect_equal(nrow(u), dplyr::n_distinct(tahoe_cell_line()$cell_name))
  expect_false(any(duplicated(u$cell_name)))
  expect_true(all(c("drivers", "n_drivers") %in% names(u)))
  expect_true(all(u$n_drivers >= 1))
})

test_that("tahoe_cell_variants loads and every variant joins to a cell line", {
  v <- tahoe_cell_variants()
  expect_s3_class(v, "tbl_df")
  expect_true(all(c("cell_name", "source", "gene") %in% names(v)))
  expect_gt(nrow(v), 0)
  expect_identical(attr(v, "tahoe_source"), "fixture")
  # Referential integrity: every variant's cell_name is a real cell line, so the
  # join in the Cell Lines tab never drops or invents rows.
  expect_true(all(v$cell_name %in% tahoe_cell_line()$cell_name))
})

test_that("drug-target x mutation cross-reference restricts to assayed lines", {
  # Empty / unknown inputs are safe (no error, zero rows).
  expect_equal(nrow(tahoe_target_mutations(character(0))), 0)
  expect_equal(nrow(tahoe_target_mutations(NA_character_)), 0)
  expect_equal(tahoe_drug_targets("___no_such_drug___"), character(0))

  # tahoe_drug_targets parses the comma-separated targets column into atoms.
  d <- tahoe_drug()
  skip_if(!"targets" %in% names(d))
  with_t <- d$drug[!is.na(d$targets) & nzchar(as.character(d$targets))]
  skip_if(length(with_t) == 0)
  expect_gte(length(tahoe_drug_targets(with_t[[1]])), 1)

  # A hit set only ever contains assayed lines mutated in the queried gene.
  genes <- tahoe_cell_variants()$gene
  skip_if(length(genes) == 0)
  gene <- names(sort(table(genes), decreasing = TRUE))[[1]]
  hits <- tahoe_target_mutations(gene)
  if (nrow(hits) > 0) {
    expect_true(all(hits$gene == gene))
    expect_true(all(hits$cell_name %in% unique(tahoe_cell_grid()$cell_name)))
  }
})

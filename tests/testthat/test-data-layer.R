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
    filters = list(pass_filter = "True"),
    metric = "n_cells"
  )
  expect_true(sum(filtered$value) <= sum(unfiltered$value))
})

test_that("tahoe_summary_counts returns headline fields", {
  cc <- tahoe_summary_counts()
  expect_true(all(
    c("drugs", "cell_lines", "samples", "plates", "genes", "obs_source") %in%
      names(cc)
  ))
  expect_gt(cc$drugs, 0)
  expect_identical(cc$obs_source, "fixture")
})

test_that("counts are of distinct entities, not raw metadata rows", {
  cell <- tahoe_cell_line()
  cc <- tahoe_summary_counts()

  # The cell_line table is driver-level (many rows per cell line), so the
  # headline count must be distinct cell lines, strictly fewer than nrow().
  expect_gt(nrow(cell), dplyr::n_distinct(cell$cell_name))
  expect_equal(cc$cell_lines, dplyr::n_distinct(cell$cell_name))
  expect_lt(cc$cell_lines, nrow(cell))
})

test_that("tahoe_cell_line_unique collapses to one row per cell line", {
  u <- tahoe_cell_line_unique()
  expect_equal(nrow(u), dplyr::n_distinct(tahoe_cell_line()$cell_name))
  expect_false(any(duplicated(u$cell_name)))
  expect_true(all(c("drivers", "n_drivers") %in% names(u)))
  expect_true(all(u$n_drivers >= 1))
})

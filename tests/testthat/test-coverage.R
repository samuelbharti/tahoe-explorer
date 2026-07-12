# Tests for the Coverage tab: the coverage summary, the heatmap and dose-bar
# builders, and the server's filtering + click handling.

test_that("tahoe_coverage summarizes the grid to one row per drug x line", {
  cov <- tahoe_coverage()
  expect_true(all(
    c(
      "drug",
      "cell_name",
      "organ",
      "n_cells",
      "n_doses",
      "doses",
      "n_plates"
    ) %in%
      names(cov)
  ))
  if (nrow(cov) > 0) {
    # One row per (drug, cell line) pair.
    expect_equal(nrow(cov), dplyr::n_distinct(paste(cov$drug, cov$cell_name)))
    expect_true(all(cov$n_cells > 0))
    expect_true(all(cov$n_doses >= 0 & cov$n_doses <= 3))
  }
})

test_that("heatmap builds and orderings cover every drug and cell line", {
  cov <- tahoe_coverage()
  p <- .coverage_heatmap(cov)
  expect_s3_class(p, "ggplot")
  expect_true("fill" %in% names(p$mapping))

  ord <- .coverage_orders(cov)
  expect_setequal(ord$drug, unique(cov$drug))
  expect_setequal(as.character(ord$line), unique(cov$cell_name))
})

test_that("dose bar builds from grid rows for one combination", {
  g <- tahoe_cell_grid()
  skip_if(nrow(g) == 0)
  pair <- g[g$drug == g$drug[[1]] & g$cell_name == g$cell_name[[1]], ]
  expect_s3_class(.coverage_dose_bar(pair), "ggplot")
})

test_that("server filters coverage by drug and organ", {
  testServer(coverage_server, {
    session$setInputs(drugs = character(0), organs = character(0))
    all_rows <- filtered()
    expect_gt(nrow(all_rows), 0)

    one_organ <- all_rows$organ[[1]]
    session$setInputs(organs = one_organ)
    expect_true(all(filtered()$organ == one_organ))

    one_drug <- all_rows$drug[[1]]
    session$setInputs(drugs = one_drug, organs = character(0))
    expect_true(all(filtered()$drug == one_drug))
  })
})

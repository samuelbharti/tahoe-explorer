# Tests for the QC tab: the conditions helper, the plot builders, and the
# server's threshold-driven underpowered / incomplete / control reactives.

test_that("tahoe_conditions is one row per drug x cell line x dose", {
  cond <- tahoe_conditions()
  expect_true(all(
    c("drug", "cell_name", "organ", "conc", "n_cells", "n_plates") %in%
      names(cond)
  ))
  if (nrow(cond) > 0) {
    expect_equal(
      nrow(cond),
      dplyr::n_distinct(paste(cond$drug, cond$cell_name, cond$conc))
    )
    expect_true(all(cond$n_cells > 0))
  }
})

test_that("control and plate bar builders build echarts", {
  cond <- tahoe_conditions()
  ctrl <- dplyr::summarise(
    dplyr::group_by(cond[cond$drug == "DMSO_TF", ], cell_name),
    n_cells = sum(n_cells),
    .groups = "drop"
  )
  # The fixture carries a DMSO_TF vehicle-control arm on every plate and cell
  # line (see dev/make_fixtures.R), so this must not be empty.
  expect_gt(nrow(ctrl), 0)
  expect_s3_class(.qc_control_bar(ctrl), "echarts4r")

  ppd <- dplyr::tibble(drug = c("a", "b", "c"), n_plates = c(1L, 1L, 14L))
  expect_s3_class(.qc_plate_bar(ppd), "echarts4r")
})

test_that("threshold drives the underpowered set; incomplete excludes control", {
  testServer(qc_server, {
    session$setInputs(min_cells = 0)
    expect_equal(nrow(underpowered()), 0) # nothing is < 0 cells

    session$setInputs(min_cells = 1e9)
    expect_equal(nrow(underpowered()), nrow(treatments())) # all treatments

    # Incomplete dose series must never include the vehicle control.
    expect_false("DMSO_TF" %in% incomplete()$drug)
    # Control coverage is one row per cell line.
    expect_equal(
      nrow(control_by_line()),
      dplyr::n_distinct(control_by_line()$cell_name)
    )
  })
})

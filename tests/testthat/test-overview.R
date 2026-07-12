# Tests for the Overview tab: the organ chart builds and carries click keys,
# organ colors are stable, selecting an organ drills the table, and the driver
# / variant plots build from a cell line's driver rows.

test_that("organ bar builds with a key aesthetic and dims non-selected", {
  colors <- .overview_organ_colors(tahoe_cell_line_unique()$Organ)
  p <- .overview_organ_bar(tahoe_cell_line_unique(), colors)
  expect_s3_class(p, "ggplot")
  expect_true(all(c("key", "alpha", "fill") %in% names(p$mapping)))
})

test_that("organ colors are a stable, complete named map", {
  organs <- tahoe_cell_line_unique()$Organ
  colors <- .overview_organ_colors(organs)
  expect_true(all(unique(organs) %in% names(colors)))
  expect_identical(colors, .overview_organ_colors(organs)) # deterministic
})

test_that("driver-gene and variant plots build from a cell line's rows", {
  cl <- tahoe_cell_line()
  cn <- cl$cell_name[[1]]
  rows <- cl[cl$cell_name == cn, , drop = FALSE]
  expect_s3_class(.overview_driver_gene_bar(rows), "ggplot")
  vc <- stats::setNames(
    tahoe_pal(length(unique(rows$Driver_VarType))),
    sort(unique(rows$Driver_VarType))
  )
  expect_s3_class(.overview_variant_bar(rows, vc), "ggplot")
})

test_that("selecting an organ filters the linked cell-line table", {
  testServer(overview_server, {
    all_rows <- filtered_lines()
    expect_gt(nrow(all_rows), 0)
    expect_true(all(
      c("cell_name", "Organ", "Driver_Gene_Symbol") %in% names(all_rows)
    ))

    organ <- filtered_lines()$Organ[[1]]
    selected_organ(organ)
    drilled <- filtered_lines()
    expect_true(all(drilled$Organ == organ))
    expect_lte(nrow(drilled), nrow(all_rows))

    selected_organ(NULL)
    expect_equal(nrow(filtered_lines()), nrow(all_rows))
  })
})

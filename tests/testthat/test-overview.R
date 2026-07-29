# Tests for the Overview tab: the organ chart builds and carries click keys,
# organ colors are stable, selecting an organ drills the table, and the driver
# / variant plots build from a cell line's driver rows.

test_that("organ bar builds (and a selection dims the others)", {
  colors <- .overview_organ_colors(tahoe_cell_line_unique()$Organ)
  counts <- .overview_organ_counts(tahoe_cell_line_unique())
  p <- .overview_organ_bar(counts, colors)
  expect_s3_class(p, "echarts4r")
  p_sel <- .overview_organ_bar(counts, colors, selected = counts$label[[1]])
  expect_s3_class(p_sel, "echarts4r")
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
  expect_s3_class(.overview_driver_gene_bar(rows), "echarts4r")
  vc <- stats::setNames(
    tahoe_pal(length(unique(rows$Driver_VarType))),
    sort(unique(rows$Driver_VarType))
  )
  expect_s3_class(.overview_variant_bar(rows, vc), "echarts4r")
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

test_that("clicking an organ bar selects that organ", {
  testServer(overview_server, {
    d <- organ_counts()
    expect_gt(nrow(d), 1)
    # echarts4r reports the clicked bar's 1-based row in the plotted frame.
    session$setInputs(organ_plot_clicked_row = 2)
    expect_equal(selected_organ(), as.character(d$label[[2]]))
    expect_true(all(filtered_lines()$Organ == as.character(d$label[[2]])))
  })
})

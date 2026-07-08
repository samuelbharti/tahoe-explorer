# Tests for the Overview tab: the organ chart builds, and selecting an organ
# drills the linked cell-line table down to that organ.

test_that("organ bar builds with a key aesthetic for click events", {
  p <- .overview_organ_bar(tahoe_cell_line_unique())
  expect_s3_class(p, "ggplot")
  expect_true("key" %in% names(p$mapping))
})

test_that("selecting an organ filters the linked cell-line table", {
  testServer(overview_server, {
    all_rows <- filtered_lines()
    expect_gt(nrow(all_rows), 0)
    expect_true(all(
      c("cell_name", "Organ", "Driver_Gene_Symbol") %in% names(all_rows)
    ))

    # Drill down to a real organ present in the data.
    organ <- filtered_lines()$Organ[[1]]
    selected_organ(organ)
    drilled <- filtered_lines()
    expect_true(all(drilled$Organ == organ))
    expect_lte(nrow(drilled), nrow(all_rows))

    # Clearing restores the full set.
    selected_organ(NULL)
    expect_equal(nrow(filtered_lines()), nrow(all_rows))
  })
})

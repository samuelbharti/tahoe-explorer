# Tests for the cell-line explorer module server.

test_that("no filters returns every cell line", {
  testServer(cell_line_explorer_server, {
    session$setInputs(
      organ = character(),
      gene = character(),
      var_type = character(),
      cell_name = ""
    )
    expect_equal(nrow(filtered()), nrow(tahoe_cell_line()))
  })
})

test_that("Organ filter narrows to only that organ", {
  testServer(cell_line_explorer_server, {
    full <- tahoe_cell_line()
    skip_if_not("Organ" %in% names(full))
    target <- full$Organ[[1]]

    session$setInputs(
      organ = target,
      gene = character(),
      var_type = character(),
      cell_name = ""
    )
    out <- filtered()

    expect_true(all(out$Organ == target))
    expect_equal(nrow(out), sum(full$Organ == target))
    expect_lte(nrow(out), nrow(full))
  })
})

test_that("cell_name search narrows to matching rows only", {
  testServer(cell_line_explorer_server, {
    full <- tahoe_cell_line()
    skip_if_not("cell_name" %in% names(full))
    # A prefix of the first cell name is guaranteed to match at least it.
    term <- substr(full$cell_name[[1]], 1, 4)
    skip_if(!nzchar(term))

    session$setInputs(
      organ = character(),
      gene = character(),
      var_type = character(),
      cell_name = term
    )
    out <- filtered()

    expect_gt(nrow(out), 0)
    expect_lte(nrow(out), nrow(full))
    expect_true(all(stringr::str_detect(
      out$cell_name,
      stringr::fixed(term, ignore_case = TRUE)
    )))
  })
})

test_that("combined filters yield a subset of each single filter", {
  testServer(cell_line_explorer_server, {
    full <- tahoe_cell_line()
    skip_if_not(all(c("Organ", "cell_name") %in% names(full)))
    target <- full$Organ[[1]]

    session$setInputs(
      organ = target,
      gene = character(),
      var_type = character(),
      cell_name = ""
    )
    organ_only <- nrow(filtered())

    session$setInputs(cell_name = substr(full$cell_name[[1]], 1, 3))
    combined <- filtered()

    expect_lte(nrow(combined), organ_only)
    expect_true(all(combined$Organ == target))
  })
})

test_that("no rows match an impossible filter", {
  testServer(cell_line_explorer_server, {
    session$setInputs(
      organ = character(),
      gene = character(),
      var_type = character(),
      cell_name = "___no_such_cell_line___"
    )
    expect_equal(nrow(filtered()), 0)
  })
})

test_that("somatic variants join to the filtered cell lines by DepMap id", {
  testServer(cell_line_explorer_server, {
    session$setInputs(
      organ = character(),
      gene = character(),
      var_type = character(),
      cell_name = ""
    )
    v <- variants()
    skip_if(nrow(v) == 0, "no variant fixture present")

    # Every variant row belongs to a currently matching cell line.
    expect_true(all(c("cell_name", "gene", "source") %in% names(v)))
    expect_true(all(v$cell_name %in% filtered_lines()$cell_name))

    # An organ filter narrows the variant set to that organ's lines.
    full <- tahoe_cell_line()
    skip_if_not("Organ" %in% names(full))
    session$setInputs(organ = full$Organ[[1]])
    v2 <- variants()
    expect_true(all(v2$cell_name %in% filtered_lines()$cell_name))
    expect_lte(nrow(v2), nrow(v))
  })
})

test_that("the table/export view has one row per distinct cell line", {
  testServer(cell_line_explorer_server, {
    session$setInputs(
      organ = character(),
      gene = character(),
      var_type = character(),
      cell_name = ""
    )
    lines <- filtered_lines()
    # Source table is driver-level (more rows than distinct cell lines); the
    # collapsed view must have exactly one row per distinct cell line.
    expect_false(any(duplicated(lines$cell_name)))
    expect_equal(nrow(lines), dplyr::n_distinct(tahoe_cell_line()$cell_name))
    expect_lt(nrow(lines), nrow(filtered()))
  })
})

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

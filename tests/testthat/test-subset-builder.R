# Server-logic tests for the subset builder module, running against whatever
# metadata is present (bundled fixtures in CI). Values are discovered at
# runtime so the assertions stay robust to the specific data.
#
# Note: inside testServer() the module's internal reactive names (grid,
# grid_filtered, ...) are in scope, so test-local variables use distinct names.

# All six selectors, defaulting to "no restriction".
.sb_inputs <- function(...) {
  base <- list(
    organs = character(),
    drivers = character(),
    cell_lines = character(),
    drugs = character(),
    doses = character(),
    plates = character()
  )
  utils::modifyList(base, list(...))
}

test_that("no selection matches every sample row", {
  testServer(subset_builder_server, {
    do.call(session$setInputs, .sb_inputs())
    expect_equal(nrow(matched_samples()), nrow(tahoe_sample()))
  })
})

test_that("cells equal the full grid total when nothing is filtered", {
  testServer(subset_builder_server, {
    do.call(session$setInputs, .sb_inputs())
    expect_equal(
      sum(grid_filtered()$n_cells),
      sum(tahoe_cell_grid()$n_cells)
    )
  })
})

test_that("selecting a plate narrows matched_samples to that plate", {
  samples <- tahoe_sample()
  skip_if_not("plate" %in% names(samples))
  target <- samples$plate[[1]]

  testServer(subset_builder_server, {
    do.call(session$setInputs, .sb_inputs(plates = target))
    got <- matched_samples()
    expect_true(all(got$plate == target))
    expect_equal(nrow(got), sum(samples$plate == target))
  })
})

test_that("an organ filter narrows cells to that tissue's cell lines", {
  full_grid <- tahoe_cell_grid()
  skip_if(!"organ" %in% names(full_grid) || all(is.na(full_grid$organ)))
  target_organ <- full_grid$organ[!is.na(full_grid$organ)][[1]]

  testServer(subset_builder_server, {
    do.call(session$setInputs, .sb_inputs(organs = target_organ))
    gf <- grid_filtered()
    expect_true(all(gf$organ == target_organ))
    expect_gt(sum(gf$n_cells), 0)
    expect_lt(sum(gf$n_cells), sum(full_grid$n_cells))
  })
})

test_that("a driver-gene filter restricts to lines with that driver", {
  full_grid <- tahoe_cell_grid()
  line_tbl <- tahoe_cell_line()
  skip_if(!all(c("cell_name", "Driver_Gene_Symbol") %in% names(line_tbl)))
  assayed <- unique(full_grid$cell_name)
  drv <- line_tbl$Driver_Gene_Symbol[line_tbl$cell_name %in% assayed]
  drv <- drv[!is.na(drv)]
  skip_if(length(drv) == 0)
  gene <- names(sort(table(drv), decreasing = TRUE))[[1]]
  expected_lines <- unique(line_tbl$cell_name[
    line_tbl$Driver_Gene_Symbol == gene
  ])

  testServer(subset_builder_server, {
    do.call(session$setInputs, .sb_inputs(drivers = gene))
    gf <- grid_filtered()
    expect_true(all(gf$cell_name %in% expected_lines))
    expect_gt(sum(gf$n_cells), 0)
  })
})

test_that("the recipe embeds the selected drug and plate", {
  samples <- tahoe_sample()
  target_drug <- samples$drug[[1]]
  target_plate <- samples$plate[[1]]

  testServer(subset_builder_server, {
    do.call(
      session$setInputs,
      .sb_inputs(drugs = target_drug, plates = target_plate)
    )
    txt <- recipe()
    expect_true(nzchar(txt))
    expect_match(txt, target_drug, fixed = TRUE)
    expect_match(txt, target_plate, fixed = TRUE)
    expect_match(txt, "read_parquet('obs_metadata.parquet')", fixed = TRUE)
    expect_match(txt, "pd.read_parquet", fixed = TRUE)
  })
})

test_that("an empty selection yields a full-dataset recipe note", {
  testServer(subset_builder_server, {
    do.call(session$setInputs, .sb_inputs())
    expect_match(recipe(), "No filters selected", fixed = TRUE)
  })
})

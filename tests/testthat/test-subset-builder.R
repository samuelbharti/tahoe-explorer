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
    txt <- recipe_parts()$recipe
    expect_true(nzchar(txt))
    expect_match(txt, target_drug, fixed = TRUE)
    expect_match(txt, target_plate, fixed = TRUE)
    # Reads straight from the pinned HuggingFace obs parquet revision, via duckdb
    # (predicate pushdown) in both the R and Python snippets.
    expect_match(txt, .subset_obs_hf, fixed = TRUE)
    expect_match(txt, "duckdb", fixed = TRUE)
    expect_match(txt, "scanpy", fixed = TRUE)
    expect_match(txt, "BARCODE_SUB_LIB_ID", fixed = TRUE)
    # The DMSO_TF vehicle control is auto-included for a valid DE contrast.
    expect_match(txt, "DMSO_TF", fixed = TRUE)
    # Includes the cell / size estimate.
    expect_match(txt, "Estimated subset", fixed = TRUE)
  })
})

test_that("an empty selection yields a full-dataset recipe note", {
  testServer(subset_builder_server, {
    do.call(session$setInputs, .sb_inputs())
    expect_match(recipe_parts()$recipe, "No filters selected", fixed = TRUE)
  })
})

test_that("tahoe_subset_document renders single-language docs per format", {
  sel <- list(drugs = tahoe_drug()$drug[[1]], doses = 0.5)
  parts <- tahoe_subset_recipe(sel)
  expect_false(is.null(parts$r_code))
  expect_false(is.null(parts$py_code))

  # R Markdown, R language: an R chunk only, with setup + HF-token guidance.
  rmd_r <- tahoe_subset_document(parts, "rmd", "r")
  expect_match(rmd_r, "output: html_document", fixed = TRUE)
  expect_match(rmd_r, "```{r, eval=FALSE}", fixed = TRUE)
  expect_false(grepl("```{python", rmd_r, fixed = TRUE))
  expect_match(rmd_r, "renv::init()", fixed = TRUE)
  expect_match(rmd_r, "HF_TOKEN", fixed = TRUE)

  # Quarto, Python language: a python chunk only, with pip/venv guidance.
  qmd_py <- tahoe_subset_document(parts, "qmd", "python")
  expect_match(qmd_py, "format: html", fixed = TRUE)
  expect_match(qmd_py, "```{python}", fixed = TRUE)
  expect_match(qmd_py, "#| eval: false", fixed = TRUE)
  expect_false(grepl("```{r", qmd_py, fixed = TRUE))
  expect_match(qmd_py, "pip install", fixed = TRUE)

  # Jupyter uses the matching kernel: IRkernel for R, python3 for Python.
  nb_r <- jsonlite::fromJSON(
    tahoe_subset_document(parts, "ipynb", "r"),
    simplifyVector = FALSE
  )
  expect_equal(nb_r$nbformat, 4L)
  expect_equal(nb_r$metadata$kernelspec$name, "ir")

  nb_py <- jsonlite::fromJSON(
    tahoe_subset_document(parts, "ipynb", "python"),
    simplifyVector = FALSE
  )
  expect_equal(nb_py$metadata$kernelspec$name, "python3")
  code <- Filter(function(cell) identical(cell$cell_type, "code"), nb_py$cells)
  expect_true(length(code) >= 1)
  expect_match(
    paste(unlist(code[[1]]$source), collapse = ""),
    "read_parquet",
    fixed = TRUE
  )
})

test_that("tahoe_subset_document handles an empty selection", {
  parts <- tahoe_subset_recipe(list())
  expect_null(parts$r_code)
  expect_match(
    tahoe_subset_document(parts, "rmd"),
    "No filters selected",
    fixed = TRUE
  )
  expect_no_error(jsonlite::fromJSON(tahoe_subset_document(parts, "ipynb")))
})

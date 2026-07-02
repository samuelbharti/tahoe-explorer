# Server-logic tests for the subset builder module, running against the bundled
# synthetic fixtures. Values are discovered from the fixtures at runtime so the
# assertions stay robust to fixture specifics.

test_that("no selection matches every sample row", {
  testServer(subset_builder_server, {
    session$setInputs(
      drugs = character(),
      cell_lines = character(),
      doses = character(),
      plates = character()
    )
    expect_equal(nrow(matched_samples()), nrow(tahoe_sample()))
  })
})

test_that("selecting a plate narrows matched_samples to that plate", {
  samples <- tahoe_sample()
  skip_if_not("plate" %in% names(samples))
  target <- samples$plate[[1]]

  testServer(subset_builder_server, {
    session$setInputs(
      drugs = character(),
      cell_lines = character(),
      doses = character(),
      plates = target
    )
    got <- matched_samples()
    expect_true(all(got$plate == target))
    expect_equal(nrow(got), sum(samples$plate == target))
    expect_lte(nrow(got), nrow(samples))
  })
})

test_that("selecting a drug narrows matched_samples to that drug", {
  samples <- tahoe_sample()
  skip_if_not("drug" %in% names(samples))
  target <- samples$drug[[1]]

  testServer(subset_builder_server, {
    session$setInputs(
      drugs = target,
      cell_lines = character(),
      doses = character(),
      plates = character()
    )
    got <- matched_samples()
    expect_true(all(got$drug == target))
    expect_gt(nrow(got), 0)
  })
})

test_that("the recipe embeds the selected values", {
  samples <- tahoe_sample()
  drug <- samples$drug[[1]]
  plate <- samples$plate[[1]]

  testServer(subset_builder_server, {
    session$setInputs(
      drugs = drug,
      cell_lines = character(),
      doses = character(),
      plates = plate
    )
    txt <- recipe()
    expect_type(txt, "character")
    expect_true(nzchar(txt))
    expect_match(txt, drug, fixed = TRUE)
    expect_match(txt, plate, fixed = TRUE)
    # Both language snippets are present.
    expect_match(txt, "read_parquet", fixed = TRUE)
    expect_match(txt, "read_parquet('obs_metadata.parquet')", fixed = TRUE)
    expect_match(txt, "pd.read_parquet", fixed = TRUE)
  })
})

test_that("an empty selection yields a full-dataset recipe note", {
  testServer(subset_builder_server, {
    session$setInputs(
      drugs = character(),
      cell_lines = character(),
      doses = character(),
      plates = character()
    )
    txt <- recipe()
    expect_true(nzchar(txt))
    expect_match(txt, "No filters selected", fixed = TRUE)
  })
})

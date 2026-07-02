# Server tests for the samples & cell-level obs explorer module. Runs against
# the bundled synthetic fixtures (obs source type is "fixture"), so the obs
# summary computes reactively without the remote "Run query" gate.

test_that("sample filters narrow the filtered-samples reactive", {
  testServer(obs_explorer_server, {
    all_df <- samples_filtered()
    expect_s3_class(all_df, "data.frame")
    expect_gt(nrow(all_df), 0)
    # Parsed dose columns are attached by the module.
    expect_true(all(c("conc", "unit") %in% names(all_df)))

    plates <- sort(unique(all_df$plate))
    expect_gt(length(plates), 1)

    session$setInputs(sample_plate = plates[[1]], sample_drug = character())
    narrowed <- samples_filtered()
    expect_true(all(narrowed$plate == plates[[1]]))
    expect_lt(nrow(narrowed), nrow(all_df))
  })
})

test_that("a drug filter further narrows the samples", {
  testServer(obs_explorer_server, {
    all_df <- samples_filtered()
    drug1 <- all_df$drug[[1]]
    session$setInputs(sample_plate = character(), sample_drug = drug1)
    narrowed <- samples_filtered()
    expect_true(all(narrowed$drug == drug1))
    expect_lte(nrow(narrowed), nrow(all_df))
  })
})

test_that("obs summary returns a value column with rows for the fixture", {
  testServer(obs_explorer_server, {
    session$setInputs(
      obs_group = "drug",
      obs_metric = "n_cells",
      obs_drug = character()
    )
    res <- obs_result()
    expect_s3_class(res, "data.frame")
    expect_true("value" %in% names(res))
    expect_gt(nrow(res), 0)
    expect_true(all(res$value > 0))
    # The group-by column is present alongside `value`.
    expect_true("drug" %in% names(res))
  })
})

test_that("switching the obs group-by column reshapes the result", {
  testServer(obs_explorer_server, {
    session$setInputs(
      obs_group = "phase",
      obs_metric = "n_cells",
      obs_drug = character()
    )
    res <- obs_result()
    expect_true("phase" %in% names(res))
    expect_true("value" %in% names(res))
    expect_gt(nrow(res), 0)
  })
})

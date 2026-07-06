# Tests for the About tab: the dimensions table renders and the Mermaid
# diagram specs are well-formed.

test_that("about server renders the dimensions UI without error", {
  testServer(about_server, {
    expect_false(is.null(output$dims))
  })
})

test_that("mermaid diagram specs are well-formed", {
  expect_type(.about_design_spec, "character")
  expect_match(.about_design_spec, "digraph", fixed = TRUE)
  expect_match(.about_design_spec, "cell lines", fixed = TRUE)
  expect_match(.about_model_spec, "obs_metadata", fixed = TRUE)
  expect_match(.about_model_spec, "gene_metadata", fixed = TRUE)
})

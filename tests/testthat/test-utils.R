test_that("app_version() returns the current version string", {
  expect_type(app_version(), "character")
  expect_match(app_version(), "^[0-9]+[.][0-9]+[.][0-9]+$")
})

test_that("safe_read_rds() returns the default for a missing file", {
  expect_null(safe_read_rds(tempfile()))
  expect_identical(safe_read_rds(tempfile(), default = "fallback"), "fallback")
})

test_that("safe_read_rds() reads an existing file", {
  path <- tempfile(fileext = ".rds")
  saveRDS(mtcars, path)
  on.exit(unlink(path), add = TRUE)

  expect_equal(safe_read_rds(path), mtcars)
})

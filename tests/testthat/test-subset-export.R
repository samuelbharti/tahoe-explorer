# Tests for the reusable export module.

test_that("subset_export_server reports the selected row count", {
  testServer(
    subset_export_server,
    args = list(data_reactive = reactive(utils::head(iris, 7))),
    {
      expect_match(output$n_rows, "^7 rows")
    }
  )
})

test_that("subset_export_server renders a recipe when one is supplied", {
  testServer(
    subset_export_server,
    args = list(
      data_reactive = reactive(iris),
      recipe = reactive("SELECT * FROM read_parquet('obs_metadata.parquet')")
    ),
    {
      expect_match(output$recipe, "SELECT")
    }
  )
})

test_that("duckdb round-trips a data frame to parquet (export mechanism)", {
  con <- tahoe_con()
  df <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  path <- tempfile(fileext = ".parquet")
  on.exit(unlink(path), add = TRUE)

  duckdb::duckdb_register(con, "rt_tmp", df)
  DBI::dbExecute(
    con,
    sprintf(
      "COPY rt_tmp TO '%s' (FORMAT PARQUET)",
      path
    )
  )
  duckdb::duckdb_unregister(con, "rt_tmp")

  back <- tahoe_read_file(path)
  expect_equal(nrow(back), 3)
  expect_equal(back$b, c("a", "b", "c"))
})

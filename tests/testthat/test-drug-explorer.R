# Server tests for the drug & MOA explorer module. These exercise the
# `filtered` reactive without a browser, against the bundled fixtures.

test_that("with no filters, filtered returns all drug rows", {
  testServer(drug_explorer_server, {
    session$setInputs(
      moa_broad = character(0),
      moa_fine = character(0),
      approval = character(0),
      trials = character(0),
      target_search = "",
      name_search = ""
    )
    expect_equal(nrow(session$returned()), nrow(tahoe_drug()))
  })
})

test_that("a MOA-broad filter narrows rows to only that MOA", {
  testServer(drug_explorer_server, {
    session$setInputs(
      moa_broad = character(0),
      moa_fine = character(0),
      approval = character(0),
      trials = character(0),
      target_search = "",
      name_search = ""
    )
    before <- session$returned()
    skip_if_not("moa-broad" %in% names(before))

    pick <- before[["moa-broad"]][1]
    session$setInputs(moa_broad = pick)
    after <- session$returned()

    expect_true(nrow(after) <= nrow(before))
    expect_true(nrow(after) > 0)
    expect_true(all(after[["moa-broad"]] == pick))
  })
})

test_that(".drug_field reads present columns and NA-guards the rest", {
  row <- tahoe_drug()[1, , drop = FALSE]
  expect_equal(.drug_field(row, "drug"), as.character(tahoe_drug()$drug[[1]]))
  expect_true(is.na(.drug_field(row, "no_such_column")))
  expect_true(is.na(.drug_field(NULL, "drug")))
})

test_that(".drug_detail_ui renders the drug's identity and fields", {
  row <- tahoe_drug()[1, , drop = FALSE]
  ui <- .drug_detail_ui(row)
  expect_s3_class(ui, "shiny.tag")
  html <- as.character(ui)
  expect_true(grepl(as.character(row$drug[[1]]), html, fixed = TRUE))
})

test_that("count and target bars accept a highlight and stay ggplots", {
  d <- tahoe_drug()
  skip_if_not(nrow(d) > 0 && "moa-broad" %in% names(d))
  p1 <- .drug_count_bar(
    d,
    "moa-broad",
    tahoe_colors$primary,
    highlight = as.character(d[["moa-broad"]][[1]])
  )
  expect_s3_class(p1, "ggplot")
  p2 <- .drug_target_bar(d, tahoe_colors$sand, highlight = "EGFR")
  expect_s3_class(p2, "ggplot")
})

test_that("selecting a focus drug renders its detail card", {
  drug <- as.character(tahoe_drug()$drug[[1]])
  testServer(drug_explorer_server, {
    session$setInputs(
      moa_broad = character(0),
      moa_fine = character(0),
      approval = character(0),
      trials = character(0),
      target_search = "",
      name_search = "",
      focus_drug = drug
    )
    # renderUI's test value is a list(html=, deps=); assert against the html.
    expect_true(grepl(drug, output$drug_detail$html, fixed = TRUE))
  })
})

test_that("a drug-name search narrows appropriately", {
  testServer(drug_explorer_server, {
    session$setInputs(
      moa_broad = character(0),
      moa_fine = character(0),
      approval = character(0),
      trials = character(0),
      target_search = "",
      name_search = ""
    )
    before <- session$returned()
    skip_if_not("drug" %in% names(before))
    skip_if(nrow(before) == 0)

    # Search for a substring of a real drug name so at least one row matches.
    target_name <- as.character(before[["drug"]][1])
    needle <- substr(target_name, 1, max(1, nchar(target_name) - 1))
    session$setInputs(name_search = needle)
    after <- session$returned()

    expect_true(nrow(after) <= nrow(before))
    expect_true(nrow(after) >= 1)
    expect_true(all(grepl(needle, after[["drug"]], fixed = TRUE)))
  })
})

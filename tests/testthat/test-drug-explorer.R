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

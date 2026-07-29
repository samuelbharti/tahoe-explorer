# Server-logic tests for the gene explorer module, running against whatever
# gene metadata is present (bundled fixtures in CI).

test_that("no query shows the full gene table", {
  testServer(gene_explorer_server, {
    session$setInputs(query = "")
    expect_equal(nrow(shown()), nrow(tahoe_gene()))
  })
})

test_that("a query filters to the matching measured gene", {
  g <- tahoe_gene()
  skip_if(!"gene_symbol" %in% names(g) || nrow(g) == 0)
  target <- g$gene_symbol[[1]]

  testServer(gene_explorer_server, {
    session$setInputs(query = target)
    got <- shown()
    expect_gte(nrow(got), 1)
    expect_true(all(toupper(got$gene_symbol) == toupper(target)))
  })
})

test_that("query parsing is case-insensitive and splits on commas/spaces", {
  g <- tahoe_gene()
  skip_if(!"gene_symbol" %in% names(g) || nrow(g) < 2)
  two <- g$gene_symbol[1:2]

  testServer(gene_explorer_server, {
    session$setInputs(query = paste(tolower(two), collapse = ", "))
    res <- lookup()
    expect_equal(length(res$found), 2)
    expect_equal(length(res$missing), 0)
  })
})

test_that("an unknown gene yields no rows and is reported missing", {
  testServer(gene_explorer_server, {
    session$setInputs(query = "NOT_A_REAL_GENE_ZZZ")
    expect_equal(nrow(shown()), 0)
    expect_equal(lookup()$missing, "NOT_A_REAL_GENE_ZZZ")
  })
})

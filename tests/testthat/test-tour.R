# The guided-demo tour is pure UI wiring; verify it builds a cicerone guide
# whose steps target the drug module's tour anchors (the anchors themselves are
# asserted present by the drug-explorer UI test / the shinytest2 smoke test).

test_that("drug_tour() builds a cicerone guide over the drug anchors", {
  guide <- drug_tour()
  expect_s3_class(guide, "Cicerone")

  steps <- guide$.__enclos_env__$private$steps
  els <- vapply(steps, function(s) s$element, character(1))
  expect_setequal(
    els,
    paste0(
      "#drugs-",
      c(
        "tour_filters",
        "tour_picker",
        "tour_table",
        "tour_detail",
        "tour_mut",
        "tour_charts",
        "tour_export"
      )
    )
  )
})

test_that("the drug UI carries every tour anchor id", {
  html <- as.character(drug_explorer_ui("drugs"))
  for (anchor in c(
    "tour_filters",
    "tour_picker",
    "tour_table",
    "tour_detail",
    "tour_mut",
    "tour_charts",
    "tour_export"
  )) {
    expect_match(html, paste0('id="drugs-', anchor, '"'), fixed = TRUE)
  }
})

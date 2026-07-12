# End-to-end smoke test: launch the real app in a headless browser and confirm
# the Overview tab renders against the fixtures.
# Skipped automatically when no Chrome/Chromium is available.

test_that("app launches and renders the Overview tab", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("chromote")
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  testthat::skip_if(
    is.null(chrome) || !nzchar(chrome),
    "No Chrome/Chromium available"
  )

  app <- shinytest2::AppDriver$new(
    app_dir = test_path("..", ".."),
    name = "app-overview",
    height = 900,
    width = 1200
  )
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 30000)

  # The Overview value boxes are rendered server-side from the data layer.
  boxes <- app$get_html("#overview-summary_boxes")
  expect_match(boxes, "Drugs")
})

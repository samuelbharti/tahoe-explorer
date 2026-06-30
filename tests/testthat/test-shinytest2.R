# End-to-end smoke test: launch the real app in a headless browser and confirm
# it starts and the example module reacts to input.
# Skipped automatically when no Chrome/Chromium is available.

test_that("app launches and the counter responds to clicks", {
  testthat::skip_if_not_installed("shinytest2")

  app <- shinytest2::AppDriver$new(
    app_dir = test_path("..", ".."),
    name = "app-smoke",
    height = 800,
    width = 1000
  )
  withr::defer(app$stop())

  # The counter module is mounted on the Home tab as "home_counter".
  expect_equal(app$get_value(output = "home_counter-value"), "Current value: 0")

  app$click("home_counter-increment")
  app$wait_for_idle()
  expect_equal(app$get_value(output = "home_counter-value"), "Current value: 1")
})

# Reactive logic test for the example counter module, using shiny::testServer().
# No browser is launched, so this is fast and runs anywhere.

test_that("counter increments on each button click", {
  testServer(example_counter_server, {
    # Starts at zero.
    expect_match(output$value, "Current value: 0")

    session$setInputs(increment = 1)
    expect_match(output$value, "Current value: 1")

    session$setInputs(increment = 2)
    expect_match(output$value, "Current value: 2")
  })
})

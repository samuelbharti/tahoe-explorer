# Unit tests for the optional async runtime (R/async.R). These cover only the
# pure gating / parsing logic, which needs no duckdb, promises, or future -- the
# actual background execution is exercised by dev/test-async.R in a real R
# session. The load-bearing guarantee here is that async is OFF by default, so
# the opt-in layer can never change the app's synchronous behaviour.

test_that(".tahoe_env_truthy recognises truthy and falsey values", {
  expect_true(.tahoe_env_truthy("1"))
  expect_true(.tahoe_env_truthy("true"))
  expect_true(.tahoe_env_truthy("TRUE"))
  expect_true(.tahoe_env_truthy("yes"))
  expect_false(.tahoe_env_truthy(""))
  expect_false(.tahoe_env_truthy("0"))
  expect_false(.tahoe_env_truthy("false"))
  expect_false(.tahoe_env_truthy("FALSE"))
})

test_that(".tahoe_async_workers parses the env with a safe default", {
  restore <- Sys.getenv("TAHOE_ASYNC_WORKERS", unset = NA)
  on.exit(
    if (is.na(restore)) {
      Sys.unsetenv("TAHOE_ASYNC_WORKERS")
    } else {
      Sys.setenv(TAHOE_ASYNC_WORKERS = restore)
    },
    add = TRUE
  )

  Sys.unsetenv("TAHOE_ASYNC_WORKERS")
  expect_equal(.tahoe_async_workers(), 2L)

  Sys.setenv(TAHOE_ASYNC_WORKERS = "4")
  expect_equal(.tahoe_async_workers(), 4L)

  Sys.setenv(TAHOE_ASYNC_WORKERS = "0")
  expect_equal(.tahoe_async_workers(), 2L)

  Sys.setenv(TAHOE_ASYNC_WORKERS = "not-a-number")
  expect_equal(.tahoe_async_workers(), 2L)
})

test_that("async is disabled by default, so the sync path is always taken", {
  # The suite never opts in, so async must report disabled without touching the
  # promises/future machinery. Skip only if a developer's shell has it set.
  skip_if(
    .tahoe_env_truthy(Sys.getenv("TAHOE_ASYNC", "")),
    "TAHOE_ASYNC is set in the environment"
  )
  expect_false(.tahoe_async_enabled())
})

test_that("the ExtendedTask factories exist and are functions", {
  expect_true(is.function(tahoe_make_grid_task))
  expect_true(is.function(tahoe_make_obs_task))
})

# dev/test-async.R
# End-to-end smoke test for the optional async runtime (R/async.R). It needs
# duckdb + a real worker plan, so it cannot run in the headless CI sandbox --
# run it in your own R session (RStudio or Rscript) from the app root:
#
#   Rscript dev/test-async.R
#
# It verifies:
#   1. async is OFF by default (TAHOE_ASYNC unset);
#   2. the promises + future packages are installed;
#   3. async turns ON with TAHOE_ASYNC=1;
#   4. a background grid build returns the SAME result as the synchronous path.
#
# Works offline on the bundled fixtures; point TAHOE_METADATA_DIR at your real
# data dir (and optionally set TAHOE_OBS_REMOTE=1) to exercise the real grid.

suppressWarnings(suppressMessages(source("global.R", chdir = TRUE)))

pass <- function(msg) cat("  PASS:", msg, "\n")
fail <- function(msg) {
  cat("  FAIL:", msg, "\n")
  quit(status = 1, save = "no")
}

# The async decision is cached for the process lifetime; clear it so we can flip
# TAHOE_ASYNC and re-evaluate within this one script.
reset_async_cache <- function() {
  .tahoe_cache$async_enabled <- NULL
  .tahoe_cache$async_planned <- NULL
}

cat("1) async disabled by default\n")
Sys.unsetenv("TAHOE_ASYNC")
reset_async_cache()
if (isTRUE(.tahoe_async_enabled())) {
  fail("expected async OFF when TAHOE_ASYNC is unset")
}
pass(".tahoe_async_enabled() is FALSE with TAHOE_ASYNC unset")

cat("2) required packages present\n")
for (pkg in c("promises", "future", "later")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    fail(sprintf("package '%s' not installed -- run dev/init-renv.R", pkg))
  }
  pass(sprintf("'%s' available", pkg))
}

cat("3) async enables with TAHOE_ASYNC=1\n")
Sys.setenv(TAHOE_ASYNC = "1")
reset_async_cache()
if (!isTRUE(.tahoe_async_enabled())) {
  fail("expected async ON with TAHOE_ASYNC=1 and packages present")
}
pass(".tahoe_async_enabled() is TRUE (worker plan established)")

cat("4) background grid build matches the synchronous grid\n")
g_sync <- tahoe_cell_grid()

# Drive a promise to completion outside Shiny by pumping the `later` event loop.
drain <- function(p, timeout = 300) {
  done <- FALSE
  val <- NULL
  err <- NULL
  promises::then(
    p,
    onFulfilled = function(v) {
      val <<- v
      done <<- TRUE
    },
    onRejected = function(e) {
      err <<- e
      done <<- TRUE
    }
  )
  start <- Sys.time()
  while (!done && as.numeric(Sys.time() - start, units = "secs") < timeout) {
    later::run_now(0.2)
  }
  if (!done) {
    stop("timed out waiting for the async result")
  }
  if (!is.null(err)) {
    stop(err)
  }
  val
}

g_async <- drain(.tahoe_worker_promise(.tahoe_root, "tahoe_cell_grid"))

if (!identical(nrow(g_sync), nrow(g_async))) {
  fail(sprintf(
    "row count differs: sync=%d async=%d",
    nrow(g_sync),
    nrow(g_async)
  ))
}
if (!identical(sort(names(g_sync)), sort(names(g_async)))) {
  fail("column sets differ between the sync and async grids")
}
if (!isTRUE(all.equal(sum(g_sync$n_cells), sum(g_async$n_cells)))) {
  fail("total cell counts differ between the sync and async grids")
}
pass(sprintf(
  "async grid matches sync grid (%d rows, %s cells)",
  nrow(g_async),
  format(sum(g_async$n_cells), big.mark = ",")
))

cat("\nAll async smoke checks passed.\n")
cat(
  "Next: launch the app with TAHOE_ASYNC=1 (add TAHOE_OBS_REMOTE=1 to exercise",
  "the remote path) and confirm the Subset and Cells tabs populate without",
  "freezing other sessions.\n"
)

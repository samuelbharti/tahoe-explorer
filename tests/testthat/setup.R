# Tests run against the bundled synthetic fixtures (hermetic: no network, no
# real data), so they are deterministic and match CI regardless of any real
# metadata a developer has downloaded into data/ locally. Point the metadata dir
# at an empty directory so every table resolves to its committed fixture. Set
# TAHOE_TEST_USE_REAL=1 to instead exercise whatever is in the real data dir.
if (!nzchar(Sys.getenv("TAHOE_TEST_USE_REAL"))) {
  .tahoe_fixtures_only <- tempfile("tahoe-fixtures-only-")
  dir.create(.tahoe_fixtures_only)
  Sys.setenv(TAHOE_METADATA_DIR = .tahoe_fixtures_only)
}

# Load the app's helper code (utilities, modules, page UI) so that unit and
# server tests can reference it directly. `chdir = TRUE` runs global.R from the
# app root so its relative source() paths resolve.
source(test_path("..", "..", "global.R"), chdir = TRUE)

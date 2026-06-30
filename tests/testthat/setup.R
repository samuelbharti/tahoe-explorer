# Load the app's helper code (utilities, modules, page UI) so that unit and
# server tests can reference it directly. `chdir = TRUE` runs global.R from the
# app root so its relative source() paths resolve.
source(test_path("..", "..", "global.R"), chdir = TRUE)

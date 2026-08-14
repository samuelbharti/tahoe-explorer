# Regenerate manifest.json for Posit Connect Cloud.
#
# Run from the app root:  Rscript dev/write_manifest.R
# Re-run whenever runtime dependencies or the set of app files change, and
# commit the result. Connect Cloud reads manifest.json to decide which R version
# and packages to install.
#
# Why an explicit file list rather than letting rsconnect scan the directory:
#
#   1. `data/` holds real downloaded metadata when present, including a ~2.1 GB
#      obs_metadata.parquet. It is gitignored, but rsconnect scans the working
#      directory, not the git index, so a plain writeManifest() would bake those
#      files (and their checksums) into the manifest. Only the small synthetic
#      fixtures under data/fixtures/ belong in a deployment.
#   2. `.Rprofile` sources renv/activate.R. Connect installs packages itself
#      from the manifest, so shipping the renv bootstrap invites it to fight
#      Connect's library. Both are excluded.
#   3. Excluding tests/ and dev/ keeps test-only and deploy-only packages
#      (testthat, shinytest2, lintr, rsconnect) out of the dependency scan.
#
# The list is derived from tracked files, so new files under R/, modules/,
# userInterface/, and so on are picked up automatically.

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop(
    "The rsconnect package is required to write the manifest.",
    call. = FALSE
  )
}

tracked <- system2("git", c("ls-files"), stdout = TRUE)
if (length(tracked) == 0) {
  stop(
    "Could not list tracked files; run this from the app root.",
    call. = FALSE
  )
}

# Files the running app actually needs.
keep_exact <- c("global.R", "ui.R", "server.R", "_brand.yml")
keep_prefix <- c(
  "R/",
  "modules/",
  "userInterface/",
  "www/",
  "inst/",
  "data/fixtures/"
)

app_files <- tracked[
  tracked %in%
    keep_exact |
    Reduce(`|`, lapply(keep_prefix, function(p) startsWith(tracked, p)))
]

missing <- app_files[!file.exists(app_files)]
if (length(missing) > 0) {
  stop(
    "Tracked but not on disk: ",
    paste(missing, collapse = ", "),
    call. = FALSE
  )
}

cat(sprintf("Writing manifest for %d files ...\n", length(app_files)))
rsconnect::writeManifest(appDir = ".", appFiles = app_files)

manifest <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
cat(sprintf(
  "  appMode:  %s\n  R:        %s\n  packages: %d\n  files:    %d\n",
  manifest$metadata$appmode,
  manifest$platform,
  length(manifest$packages),
  length(manifest$files)
))

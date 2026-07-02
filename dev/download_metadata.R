# Download Tahoe-100M metadata from HuggingFace into the local data dir.
#
# The small tables (drug, cell line, sample, gene) are a few MB total. The
# cell-level obs file is ~2.29 GB and is only fetched when you pass --obs.
# The data dir is gitignored: real data is never committed.
#
# Usage:
#   Rscript dev/download_metadata.R           # small tables only
#   Rscript dev/download_metadata.R --obs     # also the 2.29 GB obs file
#
# Override the destination with TAHOE_METADATA_DIR (defaults to "data").

base_url <- paste0(
  "https://huggingface.co/datasets/vevotx/Tahoe-100M/",
  "resolve/main/metadata/"
)

small_files <- c(
  "drug_metadata.parquet",
  "cell_line_metadata.parquet",
  "sample_metadata.parquet",
  "gene_metadata.parquet"
)
obs_file <- "obs_metadata.parquet"

args <- commandArgs(trailingOnly = TRUE)
want_obs <- "--obs" %in% args

dest_dir <- Sys.getenv("TAHOE_METADATA_DIR", unset = "data")
dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

# Optional HuggingFace token for authenticated (higher rate limit / gated)
# downloads. Read from the environment; never hard-code it.
hf_token <- ""
for (var in c("HF_TOKEN", "HUGGING_FACE_HUB_TOKEN", "HUGGINGFACE_TOKEN")) {
  if (nzchar(Sys.getenv(var, unset = ""))) {
    hf_token <- Sys.getenv(var)
    break
  }
}
dl_headers <- if (nzchar(hf_token)) {
  c(Authorization = paste("Bearer", hf_token))
} else {
  character()
}

files <- if (want_obs) c(small_files, obs_file) else small_files

fmt_size <- function(bytes) {
  units <- c("B", "KB", "MB", "GB")
  i <- if (bytes <= 0) 1 else min(length(units), floor(log(bytes, 1024)) + 1)
  sprintf("%.1f %s", bytes / 1024^(i - 1), units[i])
}

for (f in files) {
  dest <- file.path(dest_dir, f)
  url <- paste0(base_url, f)
  cat(sprintf("Downloading %s -> %s\n", f, dest))
  ok <- tryCatch(
    {
      utils::download.file(
        url,
        dest,
        mode = "wb",
        quiet = TRUE,
        headers = dl_headers
      )
      TRUE
    },
    error = function(e) {
      cat(sprintf("  FAILED: %s\n", conditionMessage(e)))
      FALSE
    }
  )
  if (ok && file.exists(dest)) {
    cat(sprintf("  done (%s)\n", fmt_size(file.info(dest)$size)))
  }
}

cat(sprintf("\nMetadata downloaded to '%s' (gitignored).\n", dest_dir))
if (!want_obs) {
  cat("Re-run with --obs to also fetch the 2.29 GB cell-level obs file.\n")
}

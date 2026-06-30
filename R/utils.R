safe_read_rds <- function(path, default = NULL) {
  if (!file.exists(path)) {
    return(default)
  }

  readRDS(path)
}

app_version <- function() {
  "2.2.0"
}

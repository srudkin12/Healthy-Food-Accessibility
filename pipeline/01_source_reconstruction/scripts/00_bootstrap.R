required <- c(
  "curl", "digest", "dplyr", "httr2", "jsonlite", "purrr",
  "readr", "stringr", "tibble", "tidyr"
)
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing missing CRAN packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}
failed <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(failed)) stop("Required packages unavailable: ", paste(failed, collapse = ", "))
cat("PACKAGE_PREFLIGHT_OK\n")
cat("R_VERSION=", R.version.string, "\n", sep = "")

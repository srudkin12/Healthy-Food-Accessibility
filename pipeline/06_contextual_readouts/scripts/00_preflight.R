source("R/functions.R")

ROOT <- root_dir()
local_lib <- file.path(ROOT, ".Rlib")
dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(unique(c(local_lib, .libPaths())))

need <- c("jsonlite", "RANN")
missing_before <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_before)) {
  cat("MISSING_R_PACKAGES=", paste(missing_before, collapse = ","), "\n", sep = "")
  cat("LOCAL_R_LIBRARY=", local_lib, "\n", sep = "")
  cat("INSTALLING_MISSING_R_PACKAGES_FROM_CRAN\n")
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  install.packages(missing_before, lib = local_lib,
                   dependencies = c("Depends","Imports","LinkingTo"))
}

missing_after <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_after)) {
  stop("Required package(s) unavailable after installation: ",
       paste(missing_after, collapse = ", "), call. = FALSE)
}

info <- do.call(rbind, lapply(need, function(pkg) {
  data.frame(
    package = pkg,
    version = as.character(utils::packageVersion(pkg)),
    library_path = dirname(find.package(pkg)),
    stringsAsFactors = FALSE
  )
}))
ensure_dir(file.path(ROOT, "results", "f5"))
write.csv(info, file.path(ROOT, "results", "f5", "r_package_environment.csv"),
          row.names = FALSE)



required_helpers <- c(
  "build_url",
  "arcgis_get_json",
  "arcgis_attributes_to_df",
  "arcgis_download_attribute_table",
  "try_geods_ckan_resource"
)

missing_helpers <- required_helpers[
  !vapply(required_helpers, exists, logical(1), mode = "function")
]

if (length(missing_helpers)) {
  stop(
    "F5 helper preflight failed; missing function(s): ",
    paste(missing_helpers, collapse = ", "),
    call. = FALSE
  )
}

# Functional smoke test for URL construction, because a parsed-but-missing helper
# caused the v1.0.1 ArcGIS fallback failure.
smoke_url <- build_url(
  "https://example.invalid/query",
  list(
    where = "LSOA11_CD LIKE 'E%'",
    returnCountOnly = "true",
    f = "json"
  )
)

if (!grepl("^https://example\\.invalid/query\\?", smoke_url) ||
    !grepl("returnCountOnly=true", smoke_url, fixed = TRUE)) {
  stop("F5 URL-builder smoke test failed.", call. = FALSE)
}

cat("HELPER_PREFLIGHT_OK functions=", length(required_helpers), "\n", sep = "")

cat("PACKAGE_PREFLIGHT_OK\n")
cat("R_VERSION=", R.version.string, "\n", sep = "")
cat("LOCAL_R_LIBRARY=", local_lib, "\n", sep = "")

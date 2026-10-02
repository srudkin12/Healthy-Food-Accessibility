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

pkg_info <- do.call(rbind, lapply(need, function(pkg) {
  data.frame(
    package = pkg,
    version = as.character(utils::packageVersion(pkg)),
    library_path = dirname(find.package(pkg)),
    installed_in_project_local_library =
      normalizePath(dirname(find.package(pkg)), winslash = "/", mustWork = TRUE) ==
      normalizePath(local_lib, winslash = "/", mustWork = TRUE),
    stringsAsFactors = FALSE
  )
}))

ensure_dir(file.path(ROOT, "results", "f4"))
write.csv(pkg_info, file.path(ROOT, "results", "f4", "r_package_environment.csv"),
          row.names = FALSE)

cat("PACKAGE_PREFLIGHT_OK\n")
cat("R_VERSION=", R.version.string, "\n", sep = "")
cat("LOCAL_R_LIBRARY=", local_lib, "\n", sep = "")

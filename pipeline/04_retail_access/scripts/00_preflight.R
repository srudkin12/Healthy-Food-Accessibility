source("R/functions.R")

ROOT <- root_dir()
local_lib <- file.path(ROOT, ".Rlib")
dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)

# Ensure the project-local library is first for the whole R process.
.libPaths(unique(c(local_lib, .libPaths())))

need <- c("jsonlite", "sf", "RANN")

is_available <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

missing_before <- need[!vapply(need, is_available, logical(1))]

if (length(missing_before)) {
  cat("MISSING_R_PACKAGES=", paste(missing_before, collapse = ","), "\n", sep = "")
  cat("LOCAL_R_LIBRARY=", local_lib, "\n", sep = "")
  cat("INSTALLING_MISSING_R_PACKAGES_FROM_CRAN\n")

  options(repos = c(CRAN = "https://cloud.r-project.org"))

  tryCatch(
    install.packages(
      missing_before,
      lib = local_lib,
      dependencies = c("Depends", "Imports", "LinkingTo"),
      quiet = FALSE
    ),
    error = function(e) {
      stop(
        "Automatic installation into the project-local R library failed: ",
        conditionMessage(e),
        "\nLocal library: ", local_lib,
        call. = FALSE
      )
    }
  )
}

missing_after <- need[!vapply(need, is_available, logical(1))]
if (length(missing_after)) {
  stop(
    "Required package(s) remain unavailable after automatic installation: ",
    paste(missing_after, collapse = ", "),
    "\nProject-local library: ", local_lib,
    call. = FALSE
  )
}

pkg_info <- do.call(
  rbind,
  lapply(need, function(pkg) {
    desc <- utils::packageDescription(pkg)
    data.frame(
      package = pkg,
      version = as.character(utils::packageVersion(pkg)),
      library_path = dirname(find.package(pkg)),
      installed_in_project_local_library =
        normalizePath(dirname(find.package(pkg)), winslash = "/", mustWork = TRUE) ==
        normalizePath(local_lib, winslash = "/", mustWork = TRUE),
      stringsAsFactors = FALSE
    )
  })
)

dir.create(file.path(ROOT, "results", "f3"), recursive = TRUE, showWarnings = FALSE)
write.csv(
  pkg_info,
  file.path(ROOT, "results", "f3", "r_package_environment.csv"),
  row.names = FALSE
)

cat("PACKAGE_PREFLIGHT_OK\n")
cat("R_VERSION=", R.version.string, "\n", sep = "")
cat("LOCAL_R_LIBRARY=", local_lib, "\n", sep = "")
cat("R_LIB_PATHS=", paste(.libPaths(), collapse = " | "), "\n", sep = "")
for (i in seq_len(nrow(pkg_info))) {
  cat(
    "PACKAGE=", pkg_info$package[i],
    " VERSION=", pkg_info$version[i],
    " LIB=", pkg_info$library_path[i],
    " LOCAL=", pkg_info$installed_in_project_local_library[i],
    "\n",
    sep = ""
  )
}

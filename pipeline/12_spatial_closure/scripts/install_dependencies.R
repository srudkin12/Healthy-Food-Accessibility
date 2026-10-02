
options(repos = c(CRAN = "https://cloud.r-project.org"))
lib <- Sys.getenv("R_LIBS_USER", unset = "")
if (!nzchar(lib)) stop("R_LIBS_USER is not set.", call. = FALSE)
dir.create(lib, recursive = TRUE, showWarnings = FALSE)

need <- c("sf", "RANN", "igraph", "Matrix")
for (p in need) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, lib = lib, dependencies = TRUE)
  }
}
bad <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(bad)) {
  stop(
    paste0(
      "Missing R packages after installation attempt: ",
      paste(bad, collapse = ", "),
      ". If sf failed, install the system GDAL/GEOS/PROJ/udunits dependencies first."
    ),
    call. = FALSE
  )
}
cat("SPATIAL_CLOSURE_DEPENDENCIES_OK ", paste(need, collapse = ","), "\n", sep = "")

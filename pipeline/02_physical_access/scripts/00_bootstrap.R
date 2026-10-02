root <- normalizePath(file.path(dirname(sub("^--file=", "", commandArgs(trailingOnly=FALSE)[grep("^--file=", commandArgs(trailingOnly=FALSE))][1])), ".."), mustWork=TRUE)
source(file.path(root,"R","functions.R"))
pkgs <- c("readr","dplyr","tidyr","stringr","jsonlite","digest","readODS")
missing <- pkgs[!vapply(pkgs, requireNamespace, quietly=TRUE, FUN.VALUE=logical(1))]
if (length(missing)) {
  msg("Installing missing R packages: ", paste(missing, collapse=", "))
  install.packages(missing, repos="https://cloud.r-project.org")
}
missing2 <- pkgs[!vapply(pkgs, requireNamespace, quietly=TRUE, FUN.VALUE=logical(1))]
assert(!length(missing2), "Missing required R packages after install: %s", paste(missing2, collapse=", "))
cat("PACKAGE_PREFLIGHT_OK\n")
cat("R_VERSION=", R.version.string, "\n", sep="")

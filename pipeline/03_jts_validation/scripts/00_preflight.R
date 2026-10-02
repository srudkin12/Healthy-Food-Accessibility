need <- c("readODS")
miss <- need[!vapply(need, requireNamespace, logical(1), quietly=TRUE)]
if(length(miss)) stop("Missing required R package(s): ", paste(miss, collapse=", "), call.=FALSE)
cat("PACKAGE_PREFLIGHT_OK\n")
cat("R_VERSION=", R.version.string, "\n", sep="")

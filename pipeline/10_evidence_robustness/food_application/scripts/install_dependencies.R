
repos <- c(CRAN="https://cloud.r-project.org")
lib <- Sys.getenv("R_LIBS_USER", unset="")
if (!nzchar(lib)) stop("R_LIBS_USER is not set.", call.=FALSE)
dir.create(lib,recursive=TRUE,showWarnings=FALSE)
need <- c("Rcpp","igraph","matrixStats")
for (p in need) {
  if (!requireNamespace(p,quietly=TRUE)) {
    install.packages(p,repos=repos,lib=lib,dependencies=TRUE)
  }
}
bad <- need[!vapply(need,requireNamespace,logical(1),quietly=TRUE)]
if(length(bad)) stop("Missing packages after install: ",paste(bad,collapse=", "),call.=FALSE)
cat("P1_4_D_DEPENDENCIES_OK ",paste(need,collapse=","),"\n",sep="")

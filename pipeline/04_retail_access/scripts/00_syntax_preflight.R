rfiles <- c(Sys.glob("R/*.R"), Sys.glob("scripts/*.R"))
for (f in rfiles) {
  tryCatch(
    parse(file = f),
    error = function(e) {
      stop("R syntax preflight failed in ", f, ": ", conditionMessage(e), call. = FALSE)
    }
  )
}
cat("R_SYNTAX_PREFLIGHT_OK files=", length(rfiles), "\n", sep = "")

root <- Sys.getenv("HFA_ROOT")
if (!nzchar(root)) {
  stop("HFA_ROOT is not set", call. = FALSE)
}

files <- c(
  Sys.glob(file.path(root, "R", "*.R")),
  Sys.glob(file.path(root, "scripts", "*.R"))
)
files <- sort(unique(files))

if (length(files) == 0L) {
  stop("No R files found for syntax preflight", call. = FALSE)
}

for (f in files) {
  tryCatch(
    parse(file = f),
    error = function(e) {
      stop(
        "R_SYNTAX_PREFLIGHT_FAILED file=", f,
        " message=", conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

cat(sprintf("R_SYNTAX_PREFLIGHT_OK files=%d\n", length(files)))

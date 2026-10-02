# Application-specific adapter skeleton.
# Keep substantive variable definitions and labels here, outside framework/.

read_application_contract <- function(path) {
  x <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!all(c("field", "value", "status") %in% names(x))) {
    stop("Malformed application contract.", call. = FALSE)
  }
  x
}

# Add deterministic source reconstruction and application checks only after the
# application contract has been recovered from authoritative evidence.

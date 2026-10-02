# Food HFA application adapter for TDABM Portable Framework 1.1.2.
# Application-specific logic only. Generic framework files are not modified.

food_normalise_name <- function(x) {
  y <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  gsub("^_+|_+$", "", y)
}

food_find_column <- function(nms, canonical) {
  nn <- food_normalise_name(nms)
  hit <- which(nn == canonical)
  if (!length(hit)) return(NA_integer_)
  hit[[1L]]
}

food_label_index <- function(nms) {
  nn <- food_normalise_name(nms)
  candidates <- c(
    "lsoa21nm",
    "lsoa21_name",
    "lsoa_name",
    "lsoa21nm_eng"
  )
  for (cand in candidates) {
    hit <- which(nn == cand)
    if (length(hit)) return(hit[[1L]])
  }
  NA_integer_
}

food_required_source_names <- c(
  "lsoa21cd",
  "geolytix_large_nearest_km",
  "car_none_pct",
  "income_deprivation_score_2025"
)

food_read_candidate <- function(path, expected_rows = 33755L) {
  h <- tryCatch(
    names(utils::read.csv(path, nrows = 1L, stringsAsFactors = FALSE, check.names = FALSE)),
    error = function(e) character()
  )
  if (!length(h)) {
    return(list(ok = FALSE, reason = "UNREADABLE_HEADER"))
  }

  hn <- food_normalise_name(h)
  idx <- match(food_required_source_names, hn)
  matched <- sum(!is.na(idx))

  if (matched < length(food_required_source_names)) {
    return(list(
      ok = FALSE,
      reason = paste0("MISSING_REQUIRED_COLUMNS_", matched, "_OF_4"),
      matched = matched
    ))
  }

  d <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
  if (is.null(d)) {
    return(list(ok = FALSE, reason = "UNREADABLE_DATA", matched = matched))
  }

  nms <- names(d)
  nn <- food_normalise_name(nms)
  idx <- match(food_required_source_names, nn)
  names(idx) <- food_required_source_names

  label_idx <- food_label_index(nms)

  x <- data.frame(
    lsoa21cd = as.character(d[[idx[["lsoa21cd"]]]]),
    geolytix_large_nearest_km = suppressWarnings(as.numeric(d[[idx[["geolytix_large_nearest_km"]]]])),
    car_none_pct = suppressWarnings(as.numeric(d[[idx[["car_none_pct"]]]])),
    income_deprivation_score_2025 = suppressWarnings(as.numeric(d[[idx[["income_deprivation_score_2025"]]]])),
    stringsAsFactors = FALSE
  )

  if (!is.na(label_idx)) {
    x$lsoa21nm <- as.character(d[[label_idx]])
  }

  reasons <- character()
  if (nrow(x) != expected_rows) {
    reasons <- c(reasons, paste0("ROW_COUNT_", nrow(x), "_EXPECTED_", expected_rows))
  }
  if (any(is.na(x$lsoa21cd) | !nzchar(trimws(x$lsoa21cd)))) {
    reasons <- c(reasons, "MISSING_OR_BLANK_ID")
  }
  if (anyDuplicated(x$lsoa21cd)) {
    reasons <- c(reasons, "DUPLICATE_ID")
  }

  topo_source <- c(
    "geolytix_large_nearest_km",
    "car_none_pct",
    "income_deprivation_score_2025"
  )
  for (nm in topo_source) {
    if (anyNA(x[[nm]]) || any(!is.finite(x[[nm]]))) {
      reasons <- c(reasons, paste0("NONFINITE_", nm))
    }
    if (length(unique(x[[nm]])) <= 1L) {
      reasons <- c(reasons, paste0("CONSTANT_", nm))
    }
  }

  list(
    ok = !length(reasons),
    reason = if (length(reasons)) paste(reasons, collapse = ";") else "VALID",
    matched = matched,
    data = x,
    label_present = "lsoa21nm" %in% names(x)
  )
}

food_candidate_roots <- function(project_root) {
  preferred <- c(
    file.path(
      project_root,
      "pipeline/06_contextual_readouts"
    ),
    file.path(
      project_root,
      "pipeline/06_contextual_readouts"
    )
  )
  preferred[dir.exists(preferred)]
}

food_canonical_subset <- function(x) {
  cols <- c(
    "lsoa21cd",
    "geolytix_large_nearest_km",
    "car_none_pct",
    "income_deprivation_score_2025"
  )
  if ("lsoa21nm" %in% names(x)) cols <- c(cols, "lsoa21nm")
  y <- x[, cols, drop = FALSE]
  y <- y[order(enc2utf8(y$lsoa21cd), method = "radix"), , drop = FALSE]
  row.names(y) <- NULL
  y
}

food_semantic_sha256 <- function(x, sha_fun) {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  utils::write.csv(
    food_canonical_subset(x),
    tmp,
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
  sha_fun(tmp)
}

food_build_p1_4a_input <- function(x) {
  out <- data.frame(
    lsoa21cd = as.character(x$lsoa21cd),
    stringsAsFactors = FALSE
  )

  if ("lsoa21nm" %in% names(x)) {
    out$lsoa21nm <- as.character(x$lsoa21nm)
  }

  out$geolytix_large_nearest_km <- as.numeric(x$geolytix_large_nearest_km)
  out$car_none_pct <- as.numeric(x$car_none_pct)
  out$income_deprivation_score_2025 <- as.numeric(x$income_deprivation_score_2025)

  out$physical_friction_log_nearest_large_store_km <-
    log1p(out$geolytix_large_nearest_km)

  out$transport_constraint_no_car_pct <-
    out$car_none_pct

  out$material_constraint_income_deprivation_2025 <-
    out$income_deprivation_score_2025

  out
}

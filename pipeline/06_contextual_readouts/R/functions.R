options(stringsAsFactors = FALSE)

root_dir <- function() normalizePath(".", winslash = "/", mustWork = TRUE)

msg <- function(...) {
  cat(sprintf("[%s] %s\n",
              format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              paste0(..., collapse = "")))
}

assert_true <- function(x, message) {
  if (!isTRUE(x)) stop(message, call. = FALSE)
}

ensure_dir <- function(x) {
  if (!dir.exists(x)) dir.create(x, recursive = TRUE, showWarnings = FALSE)
  invisible(x)
}

download_with_retry <- function(url, dest, tries = 3L, quiet = TRUE) {
  ensure_dir(dirname(dest))
  last_err <- NULL
  for (i in seq_len(tries)) {
    msg("Downloading [", i, "/", tries, "]: ", url)
    ok <- tryCatch({
      utils::download.file(url, destfile = dest, mode = "wb",
                           quiet = quiet, method = "libcurl")
      file.exists(dest) && file.info(dest)$size > 0
    }, error = function(e) {
      last_err <<- conditionMessage(e)
      FALSE
    })
    if (isTRUE(ok)) return(TRUE)
    Sys.sleep(i)
  }
  msg("Download failed: ", url,
      if (!is.null(last_err)) paste0(" | ", last_err) else "")
  FALSE
}

sha256_file <- function(path) {
  x <- system2("shasum", c("-a", "256", shQuote(path)), stdout = TRUE, stderr = TRUE)
  assert_true(length(x) > 0L, paste0("shasum failed: ", path))
  sub("[[:space:]].*$", "", x[1])
}

normalise_name <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}

zscore <- function(x) {
  s <- stats::sd(x, na.rm = TRUE)
  assert_true(is.finite(s) && s > 0, "Cannot z-score constant/invalid field")
  (x - mean(x, na.rm = TRUE)) / s
}

jaccard_rows <- function(a, b) {
  stopifnot(nrow(a) == nrow(b))
  out <- numeric(nrow(a))
  for (i in seq_len(nrow(a))) {
    aa <- a[i, ]
    bb <- b[i, ]
    out[i] <- length(intersect(aa, bb)) / length(union(aa, bb))
  }
  out
}

cramers_v <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  x <- as.factor(x[ok])
  y <- as.factor(y[ok])
  if (length(x) < 2L || nlevels(x) < 2L || nlevels(y) < 2L) return(NA_real_)
  tt <- table(x, y)
  suppressWarnings(ch <- stats::chisq.test(tt, correct = FALSE))
  n <- sum(tt)
  k <- min(nrow(tt) - 1L, ncol(tt) - 1L)
  if (k <= 0 || n <= 0) return(NA_real_)
  sqrt(as.numeric(ch$statistic) / (n * k))
}

entropy_normalised <- function(x) {
  tt <- table(x)
  p <- as.numeric(tt) / sum(tt)
  if (length(p) <= 1L) return(0)
  -sum(p * log(p)) / log(length(p))
}

read_csv_guess <- function(path) {
  read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

find_col <- function(nms, patterns, require_unique = TRUE) {
  nn <- normalise_name(nms)
  idx <- which(Reduce(`|`, lapply(patterns, function(p) grepl(p, nn, perl = TRUE))))
  if (require_unique && length(idx) != 1L) {
    stop("Column match expected 1 but found ", length(idx), ": ",
         paste(nms[idx], collapse = " | "), call. = FALSE)
  }
  idx
}




build_url <- function(base, params) {
  if (!length(params)) return(base)

  keep <- !vapply(params, is.null, logical(1))
  params <- params[keep]

  vals <- vapply(
    params,
    function(z) utils::URLencode(as.character(z), reserved = TRUE),
    character(1)
  )

  paste0(
    base,
    "?",
    paste0(names(vals), "=", vals, collapse = "&")
  )
}

arcgis_get_json <- function(url, dest) {
  ok <- download_with_retry(url, dest, tries = 3L, quiet = TRUE)
  assert_true(ok, paste0("ArcGIS request failed: ", url))
  z <- jsonlite::fromJSON(dest, simplifyVector = FALSE)
  if (!is.null(z$error)) {
    stop("ArcGIS error: ", z$error$message, call. = FALSE)
  }
  z
}

arcgis_attributes_to_df <- function(features, fields) {
  if (!length(features)) return(data.frame())
  rows <- lapply(features, function(f) {
    a <- f$attributes
    vals <- lapply(fields, function(nm) {
      v <- a[[nm]]
      if (is.null(v)) NA else v
    })
    names(vals) <- fields
    as.data.frame(vals, stringsAsFactors = FALSE, check.names = FALSE)
  })
  do.call(rbind, rows)
}

arcgis_download_attribute_table <- function(layer_url, fields, order_field,
                                            dest_dir, where = "1=1",
                                            requested_page_size = 2000L) {
  ensure_dir(dest_dir)

  meta_url <- paste0(layer_url, "?f=json")
  meta <- arcgis_get_json(meta_url, file.path(dest_dir, "layer_metadata.json"))
  max_record_count <- suppressWarnings(as.integer(meta$maxRecordCount))
  if (!is.finite(max_record_count) || max_record_count < 1L) {
    max_record_count <- 1000L
  }
  page_size <- min(as.integer(requested_page_size), max_record_count)

  count_url <- build_url(
    paste0(layer_url, "/query"),
    list(where = where, returnCountOnly = "true", f = "json")
  )
  count_obj <- arcgis_get_json(count_url, file.path(dest_dir, "count.json"))
  n <- suppressWarnings(as.integer(count_obj$count))
  assert_true(is.finite(n) && n > 0L, "Invalid ArcGIS count")

  msg("ArcGIS IUC pagination: count=", n,
      " service_maxRecordCount=", max_record_count,
      " page_size=", page_size)

  offsets <- seq.int(0L, max(0L, n - 1L), by = page_size)
  chunks <- vector("list", length(offsets))
  received <- integer(length(offsets))

  for (i in seq_along(offsets)) {
    off <- offsets[i]
    dest <- file.path(dest_dir, sprintf("chunk_%05d.json", off))

    u <- build_url(
      paste0(layer_url, "/query"),
      list(
        where = where,
        outFields = paste(fields, collapse = ","),
        returnGeometry = "false",
        resultOffset = off,
        resultRecordCount = page_size,
        orderByFields = order_field,
        f = "json"
      )
    )

    z <- arcgis_get_json(u, dest)
    chunks[[i]] <- arcgis_attributes_to_df(z$features, fields)
    received[i] <- nrow(chunks[[i]])

    expected <- min(page_size, n - off)
    assert_true(
      received[i] == expected,
      paste0(
        "ArcGIS IUC page-size mismatch at offset ", off,
        ": expected ", expected, " but received ", received[i]
      )
    )

    msg("ArcGIS IUC offset ", off, " / ", n,
        " received=", received[i])
  }

  out <- do.call(rbind, chunks)
  rownames(out) <- NULL

  assert_true(sum(received) == n,
              paste0("ArcGIS IUC received ", sum(received), " rows vs ", n))
  assert_true(nrow(out) == n,
              paste0("ArcGIS IUC table has ", nrow(out), " rows vs ", n))

  out
}

try_geods_ckan_resource <- function(resource_name_regex, dest) {
  api <- "https://data.geods.ac.uk/api/3/action/package_show?id=internet-user-classification"
  tf <- tempfile(fileext = ".json")
  ok <- download_with_retry(api, tf, tries = 2L)
  if (!ok) return(list(ok = FALSE, reason = "package_api_unavailable"))

  z <- tryCatch(jsonlite::fromJSON(tf, simplifyVector = TRUE),
                error = function(e) NULL)
  if (is.null(z) || !isTRUE(z$success)) {
    return(list(ok = FALSE, reason = "package_api_invalid"))
  }

  r <- z$result$resources
  if (is.null(r) || !nrow(r)) return(list(ok = FALSE, reason = "no_resources"))

  nm <- paste(r$name, r$description, r$format)
  idx <- which(grepl(resource_name_regex, nm, ignore.case = TRUE, perl = TRUE))
  if (length(idx) != 1L) {
    return(list(ok = FALSE,
                reason = paste0("resource_match_count_", length(idx)),
                inventory = r))
  }

  u <- r$url[idx]
  ok2 <- download_with_retry(u, dest, tries = 2L, quiet = FALSE)
  list(ok = ok2,
       reason = if (ok2) "downloaded_from_geods_ckan" else "resource_download_failed",
       resource = r[idx, , drop = FALSE],
       inventory = r)
}

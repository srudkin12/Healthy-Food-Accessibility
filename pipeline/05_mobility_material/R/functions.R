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
    if (isTRUE(ok)) return(invisible(dest))
    Sys.sleep(i)
  }
  stop("Download failed after ", tries, " tries: ", url,
       if (!is.null(last_err)) paste0(" | ", last_err) else "",
       call. = FALSE)
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

find_one <- function(nms, include, exclude = character()) {
  nn <- normalise_name(nms)
  ok <- rep(TRUE, length(nn))
  for (p in include) ok <- ok & grepl(p, nn, perl = TRUE)
  for (p in exclude) ok <- ok & !grepl(p, nn, perl = TRUE)
  idx <- which(ok)
  if (length(idx) != 1L) {
    stop("Expected one field but found ", length(idx), ": ",
         paste(nms[idx], collapse = " | "), call. = FALSE)
  }
  idx
}

zscore <- function(x) {
  x <- as.numeric(x)
  s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(NA_real_, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

quartile_flag <- function(x, side = c("low","high")) {
  side <- match.arg(side)
  q <- stats::quantile(x, probs = if (side == "low") .25 else .75,
                       na.rm = TRUE, type = 7)
  if (side == "low") x <= q else x >= q
}

build_url <- function(base, params) {
  vals <- vapply(params, function(z) utils::URLencode(as.character(z), reserved = TRUE),
                 character(1))
  paste0(base, "?", paste0(names(vals), "=", vals, collapse = "&"))
}

arcgis_get_json <- function(url, dest) {
  download_with_retry(url, dest)
  z <- jsonlite::fromJSON(dest, simplifyVector = TRUE)
  if (!is.null(z$error)) stop("ArcGIS error: ", z$error$message, call. = FALSE)
  z
}

arcgis_table_all <- function(layer_url, out_fields, code_field,
                             where, order_field, dest_dir,
                             requested_page_size = 2000L) {
  ensure_dir(dest_dir)

  # Read service metadata first. ArcGIS Feature Services can silently cap
  # resultRecordCount to maxRecordCount. Offsetting by the requested size
  # rather than the actual service limit skips records, so pagination must
  # honour the advertised cap.
  meta_url <- paste0(layer_url, "?f=json")
  meta_file <- file.path(dest_dir, "layer_metadata_for_pagination.json")
  lm <- arcgis_get_json(meta_url, meta_file)

  max_record_count <- suppressWarnings(as.integer(lm$maxRecordCount))
  if (!is.finite(max_record_count) || max_record_count < 1L) {
    max_record_count <- 1000L
  }

  page_size <- min(as.integer(requested_page_size), max_record_count)
  assert_true(page_size >= 1L, "Invalid ArcGIS page size")

  count_url <- build_url(
    paste0(layer_url, "/query"),
    list(where = where, returnCountOnly = "true", f = "json")
  )
  cz <- arcgis_get_json(count_url, file.path(dest_dir, "count.json"))
  n <- as.integer(cz$count)
  assert_true(is.finite(n) && n > 0L, "Invalid ArcGIS row count")

  msg("ArcGIS pagination: count=", n,
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
        outFields = paste(out_fields, collapse = ","),
        returnGeometry = "false",
        resultOffset = off,
        resultRecordCount = page_size,
        orderByFields = order_field,
        f = "json"
      )
    )
    z <- arcgis_get_json(u, dest)

    feats <- z$features
    if (is.data.frame(feats)) {
      a <- feats$attributes
      if (is.data.frame(a)) chunks[[i]] <- a
      else chunks[[i]] <- as.data.frame(a, stringsAsFactors = FALSE)
    } else {
      rows <- lapply(feats, function(f) as.data.frame(f$attributes, stringsAsFactors = FALSE))
      chunks[[i]] <- if (length(rows)) do.call(rbind, rows) else data.frame()
    }

    received[i] <- nrow(chunks[[i]])

    # Every non-final page should return the requested page size. If not,
    # stop rather than silently allowing a gap in the pagination.
    expected_this_page <- min(page_size, n - off)
    assert_true(
      received[i] == expected_this_page,
      paste0(
        "ArcGIS pagination page-size mismatch at offset ", off,
        ": expected ", expected_this_page,
        " but received ", received[i],
        ". Service pagination contract changed; refusing to continue."
      )
    )

    msg("ArcGIS RUC offset ", off, " / ", n,
        " received=", received[i])
  }

  ans <- do.call(rbind, chunks)
  rownames(ans) <- NULL

  # Strong completeness checks.
  assert_true(
    sum(received) == n,
    paste0("ArcGIS pagination total mismatch: received ", sum(received), " vs ", n)
  )
  assert_true(
    nrow(ans) == n,
    paste0("ArcGIS RUC count mismatch: ", nrow(ans), " vs ", n)
  )

  # Validate code uniqueness if present.
  if (code_field %in% names(ans)) {
    assert_true(
      length(unique(ans[[code_field]])) == n,
      paste0(
        "ArcGIS RUC code uniqueness failure: ",
        length(unique(ans[[code_field]])), " unique codes for ", n, " rows"
      )
    )
  }

  ans
}

jaccard_rows <- function(a, b) {
  stopifnot(nrow(a) == nrow(b))
  out <- numeric(nrow(a))
  for (i in seq_len(nrow(a))) {
    aa <- a[i, ]
    bb <- b[i, ]
    inter <- length(intersect(aa, bb))
    uni <- length(union(aa, bb))
    out[i] <- if (uni == 0L) NA_real_ else inter / uni
  }
  out
}

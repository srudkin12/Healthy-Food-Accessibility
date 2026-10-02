options(stringsAsFactors = FALSE)

root_dir <- function() {
  normalizePath(".", winslash = "/", mustWork = TRUE)
}

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

build_url <- function(base, params) {
  vals <- vapply(params, function(z) {
    utils::URLencode(as.character(z), reserved = TRUE)
  }, character(1))
  paste0(base, "?", paste0(names(vals), "=", vals, collapse = "&"))
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
  assert_true(file.exists(path), paste0("Missing file for SHA256: ", path))
  x <- system2("shasum", c("-a", "256", shQuote(path)), stdout = TRUE, stderr = TRUE)
  if (!length(x)) stop("shasum returned no output for: ", path, call. = FALSE)
  sub("[[:space:]].*$", "", x[1])
}

arcgis_count <- function(layer_url, where = "1=1", dest = NULL) {
  u <- build_url(paste0(layer_url, "/query"),
                 list(where = where, returnCountOnly = "true", f = "json"))
  if (is.null(dest)) dest <- tempfile(fileext = ".json")
  download_with_retry(u, dest)
  z <- jsonlite::fromJSON(dest, simplifyVector = TRUE)
  if (!is.null(z$error)) stop("ArcGIS count query failed: ", z$error$message, call. = FALSE)
  assert_true(!is.null(z$count), paste0("ArcGIS count missing for ", layer_url))
  as.integer(z$count)
}

attributes_to_df <- function(features, fields, include_geometry = FALSE) {
  if (!length(features)) return(data.frame())
  rows <- lapply(features, function(f) {
    a <- f$attributes
    out <- lapply(fields, function(nm) {
      val <- a[[nm]]
      if (is.null(val)) NA else val
    })
    names(out) <- fields
    if (isTRUE(include_geometry)) {
      g <- f$geometry
      out[["arcgis_x"]] <- if (is.null(g$x)) NA_real_ else as.numeric(g$x)
      out[["arcgis_y"]] <- if (is.null(g$y)) NA_real_ else as.numeric(g$y)
    }
    as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  })
  do.call(rbind, rows)
}

arcgis_download_table <- function(layer_url, fields, dest_dir,
                                  where = "1=1", order_field,
                                  include_geometry = FALSE,
                                  out_sr = 27700L,
                                  page_size = 2000L) {
  ensure_dir(dest_dir)
  count_file <- file.path(dest_dir, "count.json")
  n <- arcgis_count(layer_url, where = where, dest = count_file)
  msg("ArcGIS layer count: ", n)

  offsets <- seq.int(0L, max(0L, n - 1L), by = page_size)
  chunks <- vector("list", length(offsets))

  for (i in seq_along(offsets)) {
    off <- offsets[i]
    raw_file <- file.path(dest_dir, sprintf("chunk_%05d.json", off))
    params <- list(
      where = where,
      outFields = paste(fields, collapse = ","),
      returnGeometry = if (include_geometry) "true" else "false",
      outSR = if (include_geometry) as.character(out_sr) else "",
      resultOffset = as.character(off),
      resultRecordCount = as.character(page_size),
      orderByFields = order_field,
      f = "json"
    )
    if (!include_geometry) params$outSR <- NULL
    u <- build_url(paste0(layer_url, "/query"), params)
    download_with_retry(u, raw_file)

    z <- jsonlite::fromJSON(raw_file, simplifyVector = FALSE)
    if (!is.null(z$error)) stop("ArcGIS data query failed: ", z$error$message, call. = FALSE)
    chunks[[i]] <- attributes_to_df(z$features, fields, include_geometry)
    msg("ArcGIS records offset ", off, " / ", n)
  }

  ans <- do.call(rbind, chunks)
  rownames(ans) <- NULL
  assert_true(nrow(ans) == n,
              paste0("ArcGIS row-count mismatch: expected ", n, " got ", nrow(ans)))
  ans
}

arcgis_download_geojson_sf <- function(layer_url, fields, dest_dir,
                                       where = "1=1", order_field,
                                       out_sr = 27700L,
                                       page_size = 2000L) {
  ensure_dir(dest_dir)
  count_file <- file.path(dest_dir, "count.json")
  n <- arcgis_count(layer_url, where = where, dest = count_file)
  msg("ArcGIS polygon count: ", n)

  offsets <- seq.int(0L, max(0L, n - 1L), by = page_size)
  parts <- vector("list", length(offsets))

  for (i in seq_along(offsets)) {
    off <- offsets[i]
    raw_file <- file.path(dest_dir, sprintf("chunk_%05d.geojson", off))
    u <- build_url(
      paste0(layer_url, "/query"),
      list(
        where = where,
        outFields = paste(fields, collapse = ","),
        returnGeometry = "true",
        outSR = as.character(out_sr),
        resultOffset = as.character(off),
        resultRecordCount = as.character(page_size),
        orderByFields = order_field,
        f = "geojson"
      )
    )
    download_with_retry(u, raw_file)
    parts[[i]] <- sf::st_read(raw_file, quiet = TRUE, stringsAsFactors = FALSE)
    msg("ArcGIS polygon records offset ", off, " / ", n)
  }

  ans <- do.call(rbind, parts)
  rownames(ans) <- NULL
  assert_true(nrow(ans) == n,
              paste0("ArcGIS polygon row-count mismatch: expected ", n, " got ", nrow(ans)))
  sf::st_transform(ans, out_sr)
}

clean_size_band <- function(x) {
  x <- as.character(x)
  x <- trimws(gsub("[[:space:]]+", " ", x))
  x[x %in% c("", "NA", "NULL")] <- NA_character_
  x
}

classify_size_band <- function(x) {
  s <- clean_size_band(x)
  z <- gsub(",", "", s, fixed = TRUE)

  cls <- rep(NA_character_, length(z))
  cls[!is.na(z) & grepl("^\\s*<\\s*3013", z)] <- "A_under_280m2"
  cls[!is.na(z) & grepl("3013", z) & grepl("15069", z) &
        is.na(cls)] <- "B_280_to_1400m2"
  cls[!is.na(z) & grepl("15069", z) & grepl("30138", z) &
        is.na(cls)] <- "C_1400_to_2800m2"
  cls[!is.na(z) & grepl("30138", z) &
        is.na(cls)] <- "D_2800m2_plus"
  cls
}

compute_nn_access <- function(query_df, store_df, prefix,
                              thresholds_m = c(500, 1000, 2000, 5000, 10000),
                              decay_scales_m = c(2000, 5000),
                              chunk_size = 2000L,
                              start_k = 128L,
                              max_radius_m = 10000) {

  qxy <- as.matrix(query_df[, c("pwc_e", "pwc_n")])
  dxy <- as.matrix(store_df[, c("bng_e", "bng_n")])

  assert_true(nrow(dxy) >= 5L, paste0("Too few stores for ", prefix))
  assert_true(all(is.finite(qxy)), paste0("Non-finite PWC coordinates in ", prefix))
  assert_true(all(is.finite(dxy)), paste0("Non-finite store coordinates in ", prefix))

  n <- nrow(qxy)
  out <- data.frame(row_id = seq_len(n))
  out[[paste0(prefix, "_nearest_m")]] <- NA_real_
  out[[paste0(prefix, "_third_nearest_m")]] <- NA_real_
  out[[paste0(prefix, "_fifth_nearest_m")]] <- NA_real_
  out[[paste0(prefix, "_nearest_store_row")]] <- NA_integer_

  for (r in thresholds_m) {
    out[[paste0(prefix, "_count_", r, "m")]] <- NA_integer_
  }
  for (h in decay_scales_m) {
    out[[paste0(prefix, "_decay_", h, "m_within_", max_radius_m, "m")]] <- NA_real_
  }

  qa <- list()
  starts <- seq.int(1L, n, by = chunk_size)

  for (ii in seq_along(starts)) {
    lo <- starts[ii]
    hi <- min(n, lo + chunk_size - 1L)
    q <- qxy[lo:hi, , drop = FALSE]

    k <- min(as.integer(start_k), nrow(dxy))
    repeat {
      nn <- RANN::nn2(data = dxy, query = q, k = k,
                      treetype = "kd", searchtype = "standard")
      d <- as.matrix(nn$nn.dists)
      idx <- as.matrix(nn$nn.idx)

      kth <- d[, ncol(d)]
      unresolved <- sum(kth <= max_radius_m, na.rm = TRUE)
      if (unresolved == 0L || k >= nrow(dxy)) break
      k <- min(k * 2L, nrow(dxy))
    }

    if (sum(kth <= max_radius_m, na.rm = TRUE) > 0L && k < nrow(dxy)) {
      stop("Nearest-neighbour radius completeness not achieved for ", prefix,
           " chunk ", lo, "-", hi, call. = FALSE)
    }

    rows <- lo:hi
    out[rows, paste0(prefix, "_nearest_m")] <- d[, 1]
    if (ncol(d) >= 3L) out[rows, paste0(prefix, "_third_nearest_m")] <- d[, 3]
    if (ncol(d) >= 5L) out[rows, paste0(prefix, "_fifth_nearest_m")] <- d[, 5]
    out[rows, paste0(prefix, "_nearest_store_row")] <- idx[, 1]

    for (r in thresholds_m) {
      out[rows, paste0(prefix, "_count_", r, "m")] <- rowSums(d <= r)
    }
    for (h in decay_scales_m) {
      keep <- d <= max_radius_m
      score <- rowSums(exp(-d / h) * keep)
      out[rows, paste0(prefix, "_decay_", h, "m_within_", max_radius_m, "m")] <- score
    }

    qa[[length(qa) + 1L]] <- data.frame(
      prefix = prefix,
      row_start = lo,
      row_end = hi,
      k_final = k,
      min_kth_distance_m = min(kth, na.rm = TRUE),
      max_radius_m = max_radius_m,
      unresolved_at_final_k = sum(kth <= max_radius_m, na.rm = TRUE),
      stringsAsFactors = FALSE
    )

    msg(prefix, " NN chunk ", lo, "-", hi, " k=", k)
  }

  list(data = out, qa = do.call(rbind, qa))
}

safe_cor <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3L) return(NA_real_)
  stats::cor(x[ok], y[ok])
}

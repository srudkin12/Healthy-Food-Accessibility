options(stringsAsFactors = FALSE)
msg <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), paste0(..., collapse="")))
stopf <- function(...) stop(sprintf(...), call.=FALSE)
assert <- function(cond, ...) if (!isTRUE(cond)) stopf(...)
clean_names <- function(x) {
  x <- trimws(as.character(x)); x[is.na(x)] <- ""
  x <- iconv(x, to="ASCII//TRANSLIT")
  x <- tolower(gsub("[^a-zA-Z0-9]+", "_", x))
  x <- gsub("^_+|_+$", "", x); make.unique(x, sep="_")
}
sha256_file <- function(path) {
  if (requireNamespace("digest", quietly=TRUE)) return(digest::digest(file=path, algo="sha256"))
  out <- system2("shasum", c("-a","256", shQuote(path)), stdout=TRUE)
  strsplit(out[1], "\\s+")[[1]][1]
}
download_retry <- function(url, dest, tries=3) {
  dir.create(dirname(dest), recursive=TRUE, showWarnings=FALSE)
  if (file.exists(dest) && file.info(dest)$size > 1000) { msg("Using existing: ", dest); return(dest) }
  for (i in seq_len(tries)) {
    msg("Downloading [", i, "/", tries, "]: ", url)
    ok <- try(utils::download.file(url, dest, mode="wb", quiet=FALSE), silent=TRUE)
    if (!inherits(ok,"try-error") && file.exists(dest) && file.info(dest)$size > 1000) {
      msg("Saved: ", dest, " (", file.info(dest)$size, " bytes)"); return(dest)
    }
    Sys.sleep(i*2)
  }
  stopf("Download failed after %d tries: %s", tries, url)
}
find_project_root <- function() {
  args <- commandArgs(trailingOnly=FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) return(normalizePath(file.path(dirname(f[1]), ".."), mustWork=TRUE))
  normalizePath(getwd(), mustWork=TRUE)
}
read_csv_flex <- function(path) {
  if (requireNamespace("readr", quietly=TRUE)) return(as.data.frame(readr::read_csv(path, show_col_types=FALSE, progress=FALSE)))
  read.csv(path, check.names=FALSE)
}
write_csv <- function(x, path) {
  dir.create(dirname(path), recursive=TRUE, showWarnings=FALSE)
  utils::write.csv(x, path, row.names=FALSE, na="")
}
row_header_text <- function(raw, col, data_start) {
  if (data_start <= 1) return("")
  vals <- raw[seq_len(data_start-1), col, drop=TRUE]
  paste(trimws(as.character(vals[!is.na(vals) & nzchar(as.character(vals))])), collapse=" | ")
}
find_code_column <- function(raw, pattern) {
  scores <- vapply(seq_len(ncol(raw)), function(j) sum(grepl(pattern, as.character(raw[[j]]))), numeric(1))
  j <- which.max(scores)
  if (!length(j) || scores[j] < 100) return(NULL)
  list(col=j, score=scores[j])
}
arcgis_search_items <- function(query, num=100) {
  url <- paste0("https://www.arcgis.com/sharing/rest/search?f=json&num=", num, "&q=", utils::URLencode(query, reserved=TRUE))
  z <- jsonlite::fromJSON(url)
  if (is.null(z$results)) data.frame() else z$results
}
arcgis_get_json <- function(url, params=list()) {
  if (length(params)) {
    enc <- vapply(params, function(x) utils::URLencode(as.character(x), reserved=TRUE), character(1))
    url <- paste0(url, "?", paste0(names(enc), "=", enc, collapse="&"))
  }
  z <- jsonlite::fromJSON(url, simplifyVector=FALSE)
  if (!is.null(z$error)) {
    detail <- paste(unlist(z$error$details), collapse="; ")
    stopf("ArcGIS REST error at %s: %s %s", url, z$error$message %||% "unknown error", detail)
  }
  z
}
`%||%` <- function(x, y) if (is.null(x) || length(x)==0) y else x

arcgis_feature_layer_to_csv <- function(layer_url, dest, chunk_size=900L) {
  msg("Querying ArcGIS FeatureServer layer: ", layer_url)
  ids <- arcgis_get_json(paste0(layer_url, "/query"), list(
    where="1=1", returnIdsOnly="true", f="json"
  ))
  object_ids <- sort(unique(as.integer(unlist(ids$objectIds))))
  object_ids <- object_ids[is.finite(object_ids)]
  assert(length(object_ids)>30000, "ArcGIS lookup returned unexpectedly few object IDs: %d", length(object_ids))
  chunks <- split(object_ids, ceiling(seq_along(object_ids)/chunk_size))
  parts <- vector("list", length(chunks))
  for (i in seq_along(chunks)) {
    if (i==1L || i%%10L==0L || i==length(chunks)) msg("ArcGIS lookup chunk ", i, " / ", length(chunks))
    z <- arcgis_get_json(paste0(layer_url, "/query"), list(
      objectIds=paste(chunks[[i]], collapse=","),
      outFields="*", returnGeometry="false", f="json"
    ))
    feats <- z$features
    assert(length(feats)>0, "ArcGIS lookup chunk %d returned no features", i)
    rows <- lapply(feats, function(feat) as.data.frame(feat$attributes, stringsAsFactors=FALSE, check.names=FALSE))
    parts[[i]] <- do.call(rbind, rows)
  }
  out <- do.call(rbind, parts)
  names(out) <- clean_names(names(out))
  assert(any(grepl("lsoa11cd", names(out), ignore.case=TRUE)), "ArcGIS lookup lacks LSOA11CD")
  assert(any(grepl("lsoa21cd", names(out), ignore.case=TRUE)), "ArcGIS lookup lacks LSOA21CD")
  write_csv(out, dest)
  msg("Saved ArcGIS FeatureServer lookup: ", dest, " (", nrow(out), " rows)")
  dest
}

download_ons_lsoa11_lsoa21_exact_fit <- function(dest) {
  item_id <- "cbfe64cc03d74af982c1afec639bafd1"
  hub_csv <- paste0(
    "https://open-geography-portalx-ons.hub.arcgis.com/api/download/v1/items/",
    item_id, "/csv?layers=0"
  )
  dir.create(dirname(dest), recursive=TRUE, showWarnings=FALSE)
  if (file.exists(dest) && file.info(dest)$size > 1000) {
    chk <- try(read_csv_flex(dest), silent=TRUE)
    if (!inherits(chk,"try-error") && any(grepl("lsoa11cd",clean_names(names(chk)))) && any(grepl("lsoa21cd",clean_names(names(chk))))) {
      msg("Using existing validated ONS LSOA11/LSOA21 lookup: ", dest)
      return(dest)
    }
    unlink(dest)
  }
  msg("Trying ONS ArcGIS Hub CSV export: ", hub_csv)
  ok <- try(utils::download.file(hub_csv, dest, mode="wb", quiet=FALSE), silent=TRUE)
  if (!inherits(ok,"try-error") && file.exists(dest) && file.info(dest)$size > 1000) {
    chk <- try(read_csv_flex(dest), silent=TRUE)
    if (!inherits(chk,"try-error")) {
      nn <- clean_names(names(chk))
      if (any(grepl("lsoa11cd",nn)) && any(grepl("lsoa21cd",nn)) && nrow(chk)>30000) {
        msg("ONS_LOOKUP_DOWNLOAD_OK method=hub_csv rows=", nrow(chk))
        return(dest)
      }
    }
  }
  if (file.exists(dest)) unlink(dest)
  msg("Hub CSV unavailable/invalid; falling back to FeatureServer REST pagination")
  layer_url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA11_LSOA21_LAD22_EW_LU_v5/FeatureServer/0"
  arcgis_feature_layer_to_csv(layer_url, dest)
  chk <- read_csv_flex(dest)
  assert(nrow(chk)>30000, "Downloaded ONS exact-fit lookup unexpectedly small: %d", nrow(chk))
  msg("ONS_LOOKUP_DOWNLOAD_OK method=feature_service rows=", nrow(chk))
  dest
}

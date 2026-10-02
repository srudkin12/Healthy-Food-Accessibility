options(stringsAsFactors = FALSE)

hfa_root <- function() {
  root <- Sys.getenv("HFA_ROOT", unset = "")
  if (nzchar(root)) return(normalizePath(root, mustWork = FALSE))
  normalizePath(getwd(), mustWork = FALSE)
}

p <- function(...) file.path(hfa_root(), ...)

ensure_dirs <- function() {
  dirs <- c(
    "data_raw/ons", "data_raw/nomis", "data_raw/dft",
    "data_staged", "results/f0f1", "logs", "tmp"
  )
  invisible(lapply(p(dirs), dir.create, recursive = TRUE, showWarnings = FALSE))
}

say <- function(...) {
  msg <- paste0(..., collapse = "")
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

assert_true <- function(x, msg) {
  if (!isTRUE(x)) stop(msg, call. = FALSE)
}

sha256_file <- function(path) {
  digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
}

safe_download <- function(url, dest, retries = 3L) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(dest) && file.info(dest)$size > 0) {
    say("Reusing existing file: ", dest)
    return(invisible(dest))
  }
  tmp <- paste0(dest, ".part")
  if (file.exists(tmp)) unlink(tmp)
  last_err <- NULL
  for (i in seq_len(retries)) {
    say("Downloading [", i, "/", retries, "]: ", url)
    ok <- tryCatch({
      curl::curl_download(url, tmp, quiet = FALSE, mode = "wb")
      TRUE
    }, error = function(e) {
      last_err <<- e
      FALSE
    })
    if (ok && file.exists(tmp) && file.info(tmp)$size > 0) {
      if (!file.rename(tmp, dest)) {
        file.copy(tmp, dest, overwrite = TRUE)
        unlink(tmp)
      }
      say("Saved: ", dest, " (", format(file.info(dest)$size, big.mark = ","), " bytes)")
      return(invisible(dest))
    }
    Sys.sleep(2 * i)
  }
  stop("Download failed after retries: ", url, "\n", conditionMessage(last_err), call. = FALSE)
}

normalise_names <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  make.unique(x, sep = "_")
}

find_lsoa_csv_in_zip <- function(zip_path) {
  listing <- utils::unzip(zip_path, list = TRUE)
  nms <- listing$Name
  cand <- nms[grepl("lsoa", nms, ignore.case = TRUE) & grepl("\\.csv$", nms, ignore.case = TRUE)]
  cand <- cand[!grepl("msoa", cand, ignore.case = TRUE)]
  if (length(cand) == 0) {
    stop("Could not locate an LSOA CSV inside ", basename(zip_path),
         ". First archive members: ", paste(utils::head(nms, 20), collapse = "; "), call. = FALSE)
  }
  if (length(cand) > 1) {
    # Prefer the shortest/least nested filename; fail only if still ambiguous.
    cand <- cand[order(nchar(cand))]
  }
  cand[[1]]
}

read_lsoa_bulk <- function(zip_path) {
  member <- find_lsoa_csv_in_zip(zip_path)
  say("Reading archive member: ", member)
  con <- unz(zip_path, member, open = "rb")
  on.exit(close(con), add = TRUE)
  x <- readr::read_csv(con, show_col_types = FALSE, progress = FALSE)
  names(x) <- normalise_names(names(x))
  x
}

find_code_col <- function(df) {
  preferred <- names(df)[grepl("geography.*code|area.*code|lsoa.*code", names(df), ignore.case = TRUE)]
  candidates <- unique(c(preferred, names(df)))
  for (nm in candidates) {
    vals <- as.character(df[[nm]])
    prop <- mean(grepl("^[EW][0-9]{8}$", vals), na.rm = TRUE)
    if (is.finite(prop) && prop > 0.90) return(nm)
  }
  stop("Could not identify an LSOA code column from: ", paste(names(df), collapse = ", "), call. = FALSE)
}

pick_col <- function(df, patterns, label, exclude = NULL) {
  nms <- names(df)
  hits <- rep(TRUE, length(nms))
  for (pat in patterns) hits <- hits & grepl(pat, nms, ignore.case = TRUE)
  if (!is.null(exclude)) {
    for (pat in exclude) hits <- hits & !grepl(pat, nms, ignore.case = TRUE)
  }
  ans <- nms[hits]
  if (length(ans) != 1) {
    stop("Expected exactly one column for ", label, "; found ", length(ans), ": ", paste(ans, collapse = ", "),
         "\nAvailable columns: ", paste(nms, collapse = ", "), call. = FALSE)
  }
  ans
}

as_num <- function(x) suppressWarnings(as.numeric(gsub(",", "", as.character(x), fixed = TRUE)))

pct <- function(num, den) ifelse(is.na(den) | den == 0, NA_real_, 100 * num / den)

write_csv_atomic <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  readr::write_csv(df, tmp, na = "")
  if (!file.rename(tmp, path)) {
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
  invisible(path)
}

relative_to_root <- function(path, root = hfa_root()) {
  root_norm <- normalizePath(root, winslash = "/", mustWork = FALSE)
  path_norm <- normalizePath(path, winslash = "/", mustWork = FALSE)
  prefix <- paste0(root_norm, "/")
  if (identical(path_norm, root_norm)) return(".")
  if (startsWith(path_norm, prefix)) {
    return(substr(path_norm, nchar(prefix) + 1L, nchar(path_norm)))
  }
  path_norm
}

file_manifest <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (length(paths) == 0) return(tibble::tibble())
  tibble::tibble(
    file = vapply(paths, relative_to_root, character(1)),
    bytes = as.numeric(file.info(paths)$size),
    sha256 = vapply(paths, sha256_file, character(1))
  )
}

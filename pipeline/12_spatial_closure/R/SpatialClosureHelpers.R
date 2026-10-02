
options(stringsAsFactors = FALSE)

SC_N <- 33755L
SC_RADIUS <- 1.50
SC_FINGERPRINT <- "23d060eb8c4518192d55066752ca23a7d42caf0da43657eddfb3aca41b360051"
SC_K <- c(10L, 25L, 50L)
SC_PAIR_SAMPLE_N <- 500000L
SC_PAIR_SEED <- 20260925L

sc_norm <- function(x) {
  y <- tolower(gsub("[^A-Za-z0-9]+", "_", as.character(x)))
  gsub("^_+|_+$", "", y)
}

sc_sha256 <- function(path) {
  x <- system2("sha256sum", shQuote(path), stdout = TRUE, stderr = TRUE)
  if (!length(x)) stop("sha256sum failed: ", path, call. = FALSE)
  sub("[[:space:]].*$", "", x[[1L]])
}

sc_path_score <- function(path, layer = "") {
  x <- tolower(paste(path, layer))
  s <- 0
  if (grepl("lsoa21", x, fixed = TRUE)) s <- s + 500
  if (grepl("2021", x, fixed = TRUE)) s <- s + 250
  if (grepl("england", x, fixed = TRUE)) s <- s + 150
  if (grepl("geograph|boundary", x)) s <- s + 100
  if (grepl("canonical|staged", x)) s <- s + 80
  if (grepl("bgc", x, fixed = TRUE)) s <- s + 50
  if (grepl("result|output|handback|archive|old|supersed", x)) s <- s - 100
  s
}

sc_candidate_layers <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "gpkg") {
    z <- tryCatch(sf::st_layers(path)$name, error = function(e) character())
    if (!length(z)) return(character())
    score <- vapply(z, function(v) sc_path_score(path, v), numeric(1))
    z[order(-score, z)]
  } else {
    ""
  }
}

sc_geometry_files <- function(project_root) {
  override <- Sys.getenv("FOOD_LSOA_GEOMETRY", unset = "")
  if (nzchar(override)) {
    if (!file.exists(override)) {
      stop("FOOD_LSOA_GEOMETRY does not exist: ", override, call. = FALSE)
    }
    return(normalizePath(override, winslash = "/", mustWork = TRUE))
  }

  pats <- "\\.(gpkg|shp|geojson|fgb)$"
  files <- list.files(
    project_root,
    pattern = pats,
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  files <- unique(files[file.exists(files)])
  if (!length(files)) {
    stop(
      "No polygon geography file found under project root. ",
      "Set FOOD_LSOA_GEOMETRY=/absolute/path/to/lsoa21_file.gpkg if needed.",
      call. = FALSE
    )
  }
  scores <- vapply(files, sc_path_score, numeric(1))
  ord <- order(-scores, files)
  files[ord][seq_len(min(30L, length(files)))]
}

sc_find_id_col <- function(nms) {
  nn <- sc_norm(nms)
  hits <- which(nn %in% c(
    "lsoa21cd", "lsoa21_cd", "lsoa21code", "lsoa21_code",
    "lsoa_2021_code", "lsoa_code_2021"
  ))
  if (!length(hits)) return(NA_integer_)
  hits[[1L]]
}

sc_try_geometry <- function(path, layer, canonical_ids, override = FALSE) {
  d <- tryCatch(
    {
      if (nzchar(layer)) {
        sf::st_read(path, layer = layer, quiet = TRUE, stringsAsFactors = FALSE)
      } else {
        sf::st_read(path, quiet = TRUE, stringsAsFactors = FALSE)
      }
    },
    error = function(e) NULL
  )
  if (is.null(d)) {
    return(list(pass = FALSE, reason = "read_failed"))
  }

  id_i <- sc_find_id_col(names(d))
  if (is.na(id_i)) {
    return(list(pass = FALSE, reason = "lsoa21_id_not_found"))
  }

  ids <- as.character(d[[id_i]])
  if (anyDuplicated(ids)) {
    return(list(pass = FALSE, reason = "duplicate_lsoa21_ids"))
  }

  idx <- match(canonical_ids, ids)
  matched <- sum(!is.na(idx))
  if (matched != length(canonical_ids)) {
    return(list(pass = FALSE, reason = paste0("matched=", matched)))
  }

  d <- d[idx, , drop = FALSE]
  geom_type <- unique(as.character(sf::st_geometry_type(d, by_geometry = TRUE)))
  polygon_ok <- all(geom_type %in% c("POLYGON", "MULTIPOLYGON"))
  if (!polygon_ok) {
    return(list(pass = FALSE, reason = paste0("non_polygon=", paste(geom_type, collapse = ";"))))
  }

  score <- sc_path_score(path, layer) + 50000 + if (override) 100000 else 0
  list(
    pass = TRUE,
    reason = "exact_population_polygon_match",
    data = d,
    id_col = names(d)[id_i],
    score = score,
    geometry_types = paste(sort(unique(geom_type)), collapse = ";")
  )
}

sc_resolve_geometry <- function(project_root, canonical_ids, inventory_file) {
  files <- sc_geometry_files(project_root)
  override <- nzchar(Sys.getenv("FOOD_LSOA_GEOMETRY", unset = ""))
  override_layer <- Sys.getenv("FOOD_LSOA_LAYER", unset = "")

  candidates <- list()
  valid <- list()
  k <- 0L

  for (path in files) {
    layers <- if (override && nzchar(override_layer)) {
      override_layer
    } else {
      sc_candidate_layers(path)
    }
    if (!length(layers)) layers <- ""

    # Limit non-override GPKG layer exploration to the ten most plausible layers.
    if (!override && length(layers) > 10L) layers <- layers[seq_len(10L)]

    for (layer in layers) {
      z <- sc_try_geometry(path, layer, canonical_ids, override = override)
      k <- k + 1L
      candidates[[k]] <- data.frame(
        path = normalizePath(path, winslash = "/", mustWork = TRUE),
        layer = layer,
        path_layer_score = sc_path_score(path, layer),
        override = override,
        pass = isTRUE(z$pass),
        reason = z$reason,
        total_score = if (isTRUE(z$pass)) z$score else NA_real_,
        stringsAsFactors = FALSE
      )
      if (isTRUE(z$pass)) {
        valid[[length(valid) + 1L]] <- list(
          meta = candidates[[k]],
          object = z$data,
          id_col = z$id_col,
          geometry_types = z$geometry_types
        )
      }
    }
  }

  inv <- do.call(rbind, candidates)
  utils::write.csv(inv, inventory_file, row.names = FALSE)

  if (!length(valid)) {
    stop(
      "No exact 33,755-LSOA21 polygon geography candidate passed. ",
      "Inspect spatial_geometry_candidate_inventory.csv or set FOOD_LSOA_GEOMETRY.",
      call. = FALSE
    )
  }

  scores <- vapply(valid, function(x) x$meta$total_score[[1L]], numeric(1))
  best_score <- max(scores)
  best <- valid[scores == best_score]
  if (length(best) > 1L) {
    keys <- vapply(
      best,
      function(x) paste(x$meta$path[[1L]], x$meta$layer[[1L]], sep = "::"),
      character(1)
    )
    best <- best[order(keys)][1L]
    selection_reason <- "highest_predeclared_score_tie_resolved_lexicographically"
  } else {
    selection_reason <- if (override) {
      "explicit_environment_override"
    } else {
      "highest_predeclared_score_exact_population_match"
    }
  }

  x <- best[[1L]]
  list(
    sf = x$object,
    id_col = x$id_col,
    source_path = x$meta$path[[1L]],
    source_layer = x$meta$layer[[1L]],
    source_score = x$meta$total_score[[1L]],
    geometry_types = x$geometry_types,
    selection_reason = selection_reason
  )
}

sc_rowstd_moran <- function(y, neighbours) {
  y <- as.numeric(y)
  if (any(!is.finite(y))) return(NA_real_)
  z <- y - mean(y)
  den <- sum(z^2)
  if (!is.finite(den) || den <= 0) return(NA_real_)
  deg <- lengths(neighbours)
  ok <- deg > 0L
  if (!any(ok)) return(NA_real_)
  lag <- numeric(length(y))
  for (i in which(ok)) {
    lag[[i]] <- mean(z[neighbours[[i]]])
  }
  s0 <- sum(ok)
  length(y) / s0 * sum(z * lag) / den
}

sc_graph_component_summary <- function(member_idx, global_graph) {
  sg <- igraph::induced_subgraph(global_graph, vids = as.character(member_idx))
  cmp <- igraph::components(sg)
  sizes <- cmp$csize
  data.frame(
    n_spatial_components = cmp$no,
    largest_spatial_component_members = if (length(sizes)) max(sizes) else 0L,
    largest_spatial_component_share = if (length(member_idx)) max(sizes) / length(member_idx) else NA_real_,
    singleton_spatial_components = sum(sizes == 1L),
    stringsAsFactors = FALSE
  )
}

sc_drop_self_knn <- function(nn_idx, nn_dist, n_keep) {
  n <- nrow(nn_idx)
  out_i <- matrix(NA_integer_, nrow = n, ncol = n_keep)
  out_d <- matrix(NA_real_, nrow = n, ncol = n_keep)
  for (i in seq_len(n)) {
    keep <- nn_idx[i, ] != i
    ii <- nn_idx[i, keep]
    dd <- nn_dist[i, keep]
    if (length(ii) < n_keep) {
      stop("kNN query returned insufficient non-self neighbours.", call. = FALSE)
    }
    out_i[i, ] <- ii[seq_len(n_keep)]
    out_d[i, ] <- dd[seq_len(n_keep)]
  }
  list(index = out_i, distance = out_d)
}

sc_membership_matrix <- function(memberships, canonical_ids) {
  n <- length(canonical_ids)
  bmax <- max(memberships$ball_id)
  m <- matrix(FALSE, nrow = n, ncol = bmax)
  idx <- match(memberships$unit_id, canonical_ids)
  if (anyNA(idx)) stop("Membership contains unknown unit IDs.", call. = FALSE)
  m[cbind(idx, memberships$ball_id)] <- TRUE
  m
}

sc_pair_sample <- function(n, m, seed) {
  set.seed(seed)
  i <- sample.int(n, m, replace = TRUE)
  j <- sample.int(n, m, replace = TRUE)
  same <- i == j
  while (any(same)) {
    j[same] <- sample.int(n, sum(same), replace = TRUE)
    same <- i == j
  }
  data.frame(i = i, j = j)
}

sc_quantile <- function(x, p) {
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE, type = 7))
}

sc_safe_cor <- function(x, y, method = "spearman") {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3L) return(NA_real_)
  suppressWarnings(stats::cor(x[ok], y[ok], method = method))
}

sc_write_gz_csv <- function(x, path) {
  con <- gzfile(path, "wt")
  on.exit(close(con), add = TRUE)
  utils::write.csv(x, con, row.names = FALSE)
}

sc_plot_png_pdf <- function(png_file, pdf_file, draw_fun) {
  dir.create(dirname(png_file), recursive = TRUE, showWarnings = FALSE)
  grDevices::png(png_file, width = 1800, height = 1200, res = 180)
  draw_fun()
  grDevices::dev.off()
  grDevices::pdf(pdf_file, width = 10, height = 7, useDingbats = FALSE)
  draw_fun()
  grDevices::dev.off()
}

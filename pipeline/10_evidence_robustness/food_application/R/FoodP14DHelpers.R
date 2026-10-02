
options(stringsAsFactors = FALSE)

FOOD_EXPECTED_N <- 33755L
FOOD_APPROVED_RADIUS <- 1.50
FOOD_ROBUSTNESS_REPS <- suppressWarnings(as.integer(Sys.getenv("N_REPS", "1000")))
if (!is.finite(FOOD_ROBUSTNESS_REPS) || FOOD_ROBUSTNESS_REPS < 10L) {
  stop("N_REPS must be an integer >= 10.", call. = FALSE)
}
FOOD_BASE_SEED <- 20260807L

food_norm <- function(x) {
  y <- tolower(gsub("[^A-Za-z0-9]+", "_", as.character(x)))
  gsub("^_+|_+$", "", y)
}

food_sha256 <- function(path) {
  out <- system2("sha256sum", shQuote(path), stdout = TRUE, stderr = TRUE)
  if (!length(out)) stop("sha256sum failed: ", path, call. = FALSE)
  sub("[[:space:]].*$", "", out[[1L]])
}

food_project_roots <- function(project_root) {
  expected <- c(
    "pipeline/06_contextual_readouts",
    "pipeline/05_mobility_material",
    "pipeline/04_retail_access",
    "pipeline/01_source_reconstruction"
  )
  p <- file.path(project_root, expected)
  p[file.exists(p) | dir.exists(p)]
}

food_file_priority <- function(path) {
  x <- tolower(path)
  s <- 0
  if (grepl("f5_digital_ruc_closure", x, fixed = TRUE)) s <- s + 400
  if (grepl("f4_mobility_material_kill_tests", x, fixed = TRUE)) s <- s + 300
  if (grepl("f3_retail_functional_access", x, fixed = TRUE)) s <- s + 200
  if (grepl("f0f1_source_reconstruction", x, fixed = TRUE)) s <- s + 100
  if (grepl("/data_staged/", x, fixed = TRUE)) s <- s + 40
  if (grepl("/data_processed/", x, fixed = TRUE)) s <- s + 30
  if (grepl("/results/", x, fixed = TRUE)) s <- s - 30
  if (grepl("summary|audit|manifest|diagnostic|profile|association", basename(x))) s <- s - 50
  s
}

food_header_inventory <- function(project_root) {
  roots <- food_project_roots(project_root)
  if (!length(roots)) stop("No expected completed Food source-stage directories exist.", call. = FALSE)
  files <- unique(unlist(lapply(
    roots,
    function(r) list.files(r, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  ), use.names = FALSE))
  rows <- list(); k <- 0L
  for (f in sort(files)) {
    h <- tryCatch(
      names(utils::read.csv(f, nrows = 2L, check.names = FALSE, stringsAsFactors = FALSE)),
      error = function(e) character()
    )
    if (!length(h)) next
    hn <- food_norm(h)
    id_i <- match("lsoa21cd", hn)
    if (is.na(id_i)) next
    k <- k + 1L
    rows[[k]] <- data.frame(
      file = normalizePath(f, winslash = "/", mustWork = TRUE),
      file_priority = food_file_priority(f),
      column = h,
      column_normalised = hn,
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) stop("No CSV with an lsoa21cd field was found in expected Food stages.", call. = FALSE)
  do.call(rbind, rows)
}

food_read_cached <- local({
  cache <- new.env(parent = emptyenv())
  function(path) {
    key <- normalizePath(path, winslash = "/", mustWork = TRUE)
    if (!exists(key, envir = cache, inherits = FALSE)) {
      assign(
        key,
        utils::read.csv(key, check.names = FALSE, stringsAsFactors = FALSE),
        envir = cache
      )
    }
    get(key, envir = cache, inherits = FALSE)
  }
})

food_align_candidate <- function(path, column, canonical_ids) {
  d <- food_read_cached(path)
  nn <- food_norm(names(d))
  id_i <- match("lsoa21cd", nn)
  col_i <- match(column, names(d))
  if (is.na(id_i) || is.na(col_i)) stop("Candidate source structure changed.", call. = FALSE)
  ids <- as.character(d[[id_i]])
  if (anyDuplicated(ids)) return(NULL)
  idx <- match(canonical_ids, ids)
  values <- rep(NA, length(canonical_ids))
  ok <- !is.na(idx)
  values[ok] <- d[[col_i]][idx[ok]]
  values
}

food_numeric <- function(x) suppressWarnings(as.numeric(x))

food_binary01 <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  z <- food_norm(x)
  out <- rep(NA_integer_, length(z))
  out[z %in% c("1","true","yes","y","flagged")] <- 1L
  out[z %in% c("0","false","no","n","not_flagged")] <- 0L
  num <- suppressWarnings(as.numeric(as.character(x)))
  use <- is.na(out) & is.finite(num) & num %in% c(0,1)
  out[use] <- as.integer(num[use])
  out
}

food_name_score <- function(target, col_norm) {
  n <- col_norm
  exact <- function(vals, pts = 150) if (n %in% vals) pts else 0
  s <- 0

  if (target == "ruc_category") {
    s <- s + exact(c("ruc","ruc21","ruc_2021","ruc21_class","ruc21_category",
                     "rural_urban_classification","rural_urban_classification_2021"))
    if (grepl("ruc", n, fixed = TRUE)) s <- s + 80
    if (grepl("rural", n) && grepl("urban", n)) s <- s + 70
    if (grepl("name|label|category|class", n)) s <- s + 15
  }

  if (target == "iuc_category") {
    s <- s + exact(c("iuc","iuc_group","iuc_group_name","iuc_2018","iuc2018_group",
                     "internet_user_classification","internet_user_classification_2018"))
    if (grepl("iuc", n, fixed = TRUE)) s <- s + 90
    if (grepl("internet", n) && grepl("user", n)) s <- s + 80
    if (grepl("group|name|label|class", n)) s <- s + 15

    # P1.4-D requires a categorical IUC readout. Where a source carries
    # both category codes and reader-facing labels, prefer the descriptive
    # representation rather than an arbitrary numeric/code representation.
    if (grepl("group_name|category_name|class_name|name|label|description|desc", n)) {
      s <- s + 120
    }
    if (grepl("code|(^|_)id($|_)|number|(^|_)num($|_)|index", n)) {
      s <- s - 120
    }
  }

  tcm_base <- grepl("tcm|connectivity", n)
  shopping <- grepl("shop", n)
  if (grepl("^tcm_shopping", n)) s <- s + 80
  if (tcm_base) s <- s + 50
  if (shopping) s <- s + 40

  if (target == "tcm_shopping_walk" && (grepl("walk", n))) s <- s + 80
  if (target == "tcm_shopping_cycle" && (grepl("cycl", n))) s <- s + 80
  if (target == "tcm_shopping_public_transport" &&
      (grepl("public.*transport|public_transport|transit|(^|_)pt($|_)", n))) s <- s + 80
  if (target == "tcm_shopping_drive" &&
      (grepl("driv|car", n))) s <- s + 80
  if (target == "tcm_shopping_overall" &&
      (grepl("overall|all_mode|allmode|combined|composite|total", n))) s <- s + 80

  if (target == "internal_qualifying_store_count") {
    if (grepl("internal", n)) s <- s + 60
    if (grepl("store", n)) s <- s + 50
    if (grepl("count|n_", n)) s <- s + 30
    if (grepl("large|qualif|student", n)) s <- s + 25
  }

  if (target == "kmeans_cluster") {
    if (grepl("kmeans|k_means", n)) s <- s + 100
    if (grepl("cluster", n)) s <- s + 60
    if (grepl("3d|three", n)) s <- s + 15
  }

  if (target == "resource_buffered_flag") {
    if (grepl("resource", n)) s <- s + 70
    if (grepl("buffer", n)) s <- s + 70
    if (grepl("flag|indicator", n)) s <- s + 20
  }
  if (target == "proximity_capability_constrained_flag") {
    if (grepl("proxim", n)) s <- s + 60
    if (grepl("capab", n)) s <- s + 60
    if (grepl("constrain", n)) s <- s + 60
    if (grepl("flag|indicator", n)) s <- s + 20
  }
  if (target == "compound_disadvantage_flag") {
    if (grepl("compound", n)) s <- s + 80
    if (grepl("disadv", n)) s <- s + 60
    if (grepl("flag|indicator", n)) s <- s + 20
  }
  s
}

food_category_missing <- function(x) {
  is.na(x) | !nzchar(trimws(as.character(x)))
}

food_categorical_partition_equivalent <- function(x, y) {
  a <- as.character(x)
  b <- as.character(y)

  ma <- food_category_missing(a)
  mb <- food_category_missing(b)

  if (!identical(ma, mb)) return(FALSE)

  keep <- !ma
  if (!any(keep)) return(TRUE)

  aa <- a[keep]
  bb <- b[keep]

  tab <- table(aa, bb, useNA = "no")

  # The two fields encode the same partition if every level of one maps
  # to exactly one level of the other, and vice versa. This permits
  # code-versus-label representations without treating IUC as ordinal.
  all(rowSums(tab > 0) == 1L) &&
    all(colSums(tab > 0) == 1L)
}

food_category_representation_score <- function(target, column_normalised, x) {
  if (!target %in% c("iuc_category", "ruc_category")) return(0)

  n <- column_normalised
  s <- 0

  if (grepl("group_name|category_name|class_name|name|label|description|desc", n)) {
    s <- s + 200
  }
  if (grepl("code|(^|_)id($|_)|number|(^|_)num($|_)|index", n)) {
    s <- s - 160
  }

  vals <- as.character(x[!food_category_missing(x)])
  if (length(vals)) {
    num <- suppressWarnings(as.numeric(vals))
    if (all(is.finite(num))) {
      s <- s - 100
    } else {
      s <- s + 40
    }

    # Reader-facing descriptive categories generally contain alphabetic
    # content and are preferable to opaque codes when the partition is identical.
    if (mean(grepl("[A-Za-z]", vals)) > 0.8) s <- s + 40
    if (mean(nchar(vals)) >= 8) s <- s + 20
  }

  s
}

food_validate_candidate <- function(target, x) {
  if (is.null(x)) return(list(pass = FALSE, detail = "unalignable"))
  n <- length(x)
  nonmissing <- sum(!is.na(x) & nzchar(trimws(as.character(x))))
  vals_char <- as.character(x[!is.na(x) & nzchar(trimws(as.character(x)))])
  unique_n <- length(unique(vals_char))

  if (target == "ruc_category") {
    pass <- nonmissing >= 33700L && unique_n == 6L
    return(list(pass = pass, detail = paste0("coverage=",nonmissing,";unique=",unique_n)))
  }

  if (target == "iuc_category") {
    pass <- nonmissing >= 31750L && nonmissing <= 31850L && unique_n == 10L
    return(list(pass = pass, detail = paste0("coverage=",nonmissing,";unique=",unique_n)))
  }

  if (grepl("^tcm_shopping_", target)) {
    z <- food_numeric(x)
    fin <- is.finite(z)
    pass <- sum(fin) >= 33000L && length(unique(z[fin])) > 20L
    return(list(pass = pass, detail = paste0(
      "finite=",sum(fin),";unique=",length(unique(z[fin])),
      ";range=",paste(signif(range(z[fin]),6),collapse=":")
    )))
  }

  if (target == "internal_qualifying_store_count") {
    z <- food_numeric(x); fin <- is.finite(z)
    intlike <- all(abs(z[fin] - round(z[fin])) < 1e-8)
    m <- if (any(fin)) mean(z[fin]) else NA_real_
    mx <- if (any(fin)) max(z[fin]) else NA_real_
    pass <- sum(fin) == 33755L && intlike && mx >= 5 && mx <= 20 &&
      is.finite(m) && m > 0.15 && m < 0.25
    return(list(pass = pass, detail = paste0("finite=",sum(fin),";mean=",m,";max=",mx)))
  }

  if (target == "kmeans_cluster") {
    z <- vals_char
    tab <- sort(as.integer(table(z)))
    pass <- nonmissing == 33755L && unique_n == 3L &&
      identical(tab, sort(c(5265L,10731L,17759L)))
    return(list(pass = pass, detail = paste0("counts=",paste(tab,collapse=";"))))
  }

  flag_expected <- c(
    resource_buffered_flag = 2819L,
    proximity_capability_constrained_flag = 2241L,
    compound_disadvantage_flag = 295L
  )
  if (target %in% names(flag_expected)) {
    z <- food_binary01(x)
    pass <- sum(!is.na(z)) == 33755L && sum(z == 1L, na.rm = TRUE) == flag_expected[[target]]
    return(list(pass = pass, detail = paste0(
      "finite=",sum(!is.na(z)),";ones=",sum(z==1L,na.rm=TRUE)
    )))
  }

  list(pass = FALSE, detail = "no validation rule")
}

food_select_target <- function(target, inventory, canonical_ids) {
  inv <- inventory
  inv$name_score <- vapply(
    inv$column_normalised,
    function(x) food_name_score(target, x),
    numeric(1)
  )
  inv <- inv[inv$name_score > 0, , drop = FALSE]

  if (!nrow(inv)) {
    stop(
      "No named candidate columns found for required readout: ",
      target,
      call. = FALSE
    )
  }

  assessed <- list()
  aligned <- list()
  k <- 0L

  for (i in seq_len(nrow(inv))) {
    x <- tryCatch(
      food_align_candidate(inv$file[[i]], inv$column[[i]], canonical_ids),
      error = function(e) NULL
    )

    val <- food_validate_candidate(target, x)
    rep_score <- if (is.null(x)) {
      -9999
    } else {
      food_category_representation_score(
        target,
        inv$column_normalised[[i]],
        x
      )
    }

    k <- k + 1L
    key <- as.character(k)
    aligned[[key]] <- x

    assessed[[k]] <- data.frame(
      candidate_id = k,
      target = target,
      file = inv$file[[i]],
      column = inv$column[[i]],
      column_normalised = inv$column_normalised[[i]],
      file_priority = inv$file_priority[[i]],
      name_score = inv$name_score[[i]],
      representation_score = rep_score,
      validation_pass = isTRUE(val$pass),
      validation_detail = val$detail,
      stringsAsFactors = FALSE
    )
  }

  tab <- do.call(rbind, assessed)

  # Scientific source hierarchy comes first. Later accepted Food stages have
  # higher file_priority; representation/name scoring chooses among candidate
  # fields within that authoritative stage.
  valid <- tab[tab$validation_pass, , drop = FALSE]
  if (!nrow(valid)) {
    attr(tab, "failure") <- TRUE
    return(list(mapping = NULL, inventory = tab))
  }

  best_priority <- max(valid$file_priority)
  authoritative <- valid[
    valid$file_priority == best_priority,
    ,
    drop = FALSE
  ]

  authoritative$total_score <-
    authoritative$name_score +
    authoritative$representation_score

  best_score <- max(authoritative$total_score)
  preferred <- authoritative[
    authoritative$total_score == best_score,
    ,
    drop = FALSE
  ]

  chosen <- preferred[
    order(preferred$file, preferred$column),
    ,
    drop = FALSE
  ][1L, , drop = FALSE]

  x0 <- aligned[[as.character(chosen$candidate_id)]]

  selection_reason <- "authoritative_stage_highest_field_score"
  equivalent_candidate_count <- 1L

  if (target %in% c("iuc_category", "ruc_category") &&
      nrow(authoritative) > 1L) {

    equiv <- vapply(
      authoritative$candidate_id,
      function(id) {
        xj <- aligned[[as.character(id)]]
        food_categorical_partition_equivalent(x0, xj)
      },
      logical(1)
    )

    if (!all(equiv)) {
      bad <- authoritative[!equiv, c("file", "column"), drop = FALSE]
      stop(
        paste0(
          "Conflicting authoritative categorical partitions for ",
          target,
          ". The competing fields are not related by a one-to-one category ",
          "relabeling. Inspect readout_source_candidate_inventory.csv. ",
          "Conflicts: ",
          paste(
            paste0(basename(bad$file), "::", bad$column),
            collapse = " | "
          )
        ),
        call. = FALSE
      )
    }

    equivalent_candidate_count <- nrow(authoritative)
    selection_reason <-
      "authoritative_fields_partition_equivalent_prefer_descriptive_label"
  } else if (nrow(preferred) > 1L) {

    # For non-IUC/RUC targets, preserve the previous strict rule:
    # equally preferred candidates must be literally identical.
    for (j in seq_len(nrow(preferred))) {
      xj <- aligned[[as.character(preferred$candidate_id[[j]])]]
      if (!identical(as.character(x0), as.character(xj))) {
        stop(
          "Ambiguous non-equivalent top-scoring sources for ",
          target,
          ". Inspect readout_source_candidate_inventory.csv.",
          call. = FALSE
        )
      }
    }

    equivalent_candidate_count <- nrow(preferred)
    selection_reason <- "top_scoring_fields_literal_equivalence"
  }

  chosen$selection_reason <- selection_reason
  chosen$equivalent_authoritative_candidate_count <-
    equivalent_candidate_count

  list(
    mapping = cbind(
      chosen,
      source_sha256 = food_sha256(chosen$file)
    ),
    values = x0,
    inventory = tab
  )
}

food_safe_factor <- function(x, missing_level = "UNRESOLVED") {
  y <- as.character(x)
  y[is.na(y) | !nzchar(trimws(y))] <- missing_level
  factor(y)
}

food_numeric_profile_missing <- function(bm, values, variable, role) {
  z <- suppressWarnings(as.numeric(values))
  finite_all <- z[is.finite(z)]
  if (!length(finite_all)) stop("No finite values for numeric readout: ", variable, call. = FALSE)
  sm <- mean(finite_all)
  ss <- stats::sd(finite_all)
  rows <- lapply(seq_along(bm$points_covered_by_landmarks), function(ball_id) {
    idx <- unique(as.integer(bm$points_covered_by_landmarks[[ball_id]]))
    x <- z[idx]
    ok <- is.finite(x)
    xf <- x[ok]
    data.frame(
      ball_id = ball_id,
      variable = variable,
      variable_role = role,
      n_members = length(idx),
      n_nonmissing = length(xf),
      n_missing = sum(!ok),
      coverage_share = mean(ok),
      ball_mean = if (length(xf)) mean(xf) else NA_real_,
      ball_sd = if (length(xf) > 1L) stats::sd(xf) else if (length(xf) == 1L) 0 else NA_real_,
      ball_min = if (length(xf)) min(xf) else NA_real_,
      ball_max = if (length(xf)) max(xf) else NA_real_,
      sample_mean = sm,
      sample_sd = ss,
      standardized_difference = if (length(xf) && is.finite(ss) && ss > 0) (mean(xf)-sm)/ss else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

food_numeric_local_colour_missing <- function(bm, values, unit_ids, unit_labels, variable) {
  z <- suppressWarnings(as.numeric(values))
  n <- length(z)
  ball_means <- vapply(
    bm$points_covered_by_landmarks,
    function(idx) {
      x <- z[unique(as.integer(idx))]
      x <- x[is.finite(x)]
      if (length(x)) mean(x) else NA_real_
    },
    numeric(1)
  )
  memberships <- vector("list", n)
  for (b in seq_along(bm$points_covered_by_landmarks)) {
    for (i in unique(as.integer(bm$points_covered_by_landmarks[[b]]))) {
      memberships[[i]] <- c(memberships[[i]], b)
    }
  }
  local <- rep(NA_real_, n)
  count <- integer(n)
  ids <- character(n)
  for (i in seq_len(n)) {
    b <- sort(unique(as.integer(memberships[[i]])))
    ids[[i]] <- paste(b, collapse = ";")
    bmvals <- ball_means[b]
    bmvals <- bmvals[is.finite(bmvals)]
    count[[i]] <- length(b)
    if (length(bmvals)) local[[i]] <- mean(bmvals)
  }
  data.frame(
    variable = variable,
    point_index = seq_len(n),
    unit_id = unit_ids,
    unit_label = unit_labels,
    observed_value = z,
    local_colour = local,
    membership_count = count,
    ball_ids = ids,
    aggregation = "equal_ball_mean",
    stringsAsFactors = FALSE
  )
}

food_categorical_ball_profile <- function(bm, values, variable) {
  f <- food_safe_factor(values)
  levels_f <- levels(f)
  rows <- list(); k <- 0L
  for (b in seq_along(bm$points_covered_by_landmarks)) {
    idx <- unique(as.integer(bm$points_covered_by_landmarks[[b]]))
    x <- f[idx]
    tab <- table(x)
    denom <- sum(tab)
    for (lev in levels_f) {
      k <- k + 1L
      rows[[k]] <- data.frame(
        ball_id = b,
        variable = variable,
        category = lev,
        n_members = length(idx),
        category_count = as.integer(tab[[lev]]),
        category_share = as.numeric(tab[[lev]]) / denom,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

food_categorical_ball_summary <- function(profile_long) {
  split_key <- split(profile_long, profile_long$ball_id)
  rows <- lapply(split_key, function(x) {
    ord <- order(-x$category_share, x$category)
    best <- x[ord[[1L]], , drop = FALSE]
    p <- x$category_share[x$category_share > 0]
    entropy <- if (length(p)) -sum(p * log(p)) else NA_real_
    data.frame(
      ball_id = best$ball_id,
      variable = best$variable,
      modal_category = best$category,
      modal_share = best$category_share,
      category_entropy = entropy,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

food_categorical_local_probabilities <- function(bm, values, unit_ids, unit_labels, variable) {
  f <- food_safe_factor(values)
  levs <- levels(f)
  n <- length(f)
  # Per-ball category shares.
  share <- matrix(0, nrow = length(bm$points_covered_by_landmarks), ncol = length(levs))
  colnames(share) <- levs
  for (b in seq_along(bm$points_covered_by_landmarks)) {
    idx <- unique(as.integer(bm$points_covered_by_landmarks[[b]]))
    tab <- table(f[idx])
    share[b, names(tab)] <- as.numeric(tab) / sum(tab)
  }
  sums <- matrix(0, nrow = n, ncol = length(levs))
  counts <- integer(n)
  ball_ids <- vector("list", n)
  for (b in seq_along(bm$points_covered_by_landmarks)) {
    idx <- unique(as.integer(bm$points_covered_by_landmarks[[b]]))
    sums[idx, ] <- sums[idx, , drop = FALSE] + matrix(share[b, ], nrow = length(idx), ncol = ncol(share), byrow = TRUE)
    counts[idx] <- counts[idx] + 1L
    for (i in idx) ball_ids[[i]] <- c(ball_ids[[i]], b)
  }
  probs <- sums / counts
  rows <- vector("list", length(levs))
  for (j in seq_along(levs)) {
    rows[[j]] <- data.frame(
      variable = variable,
      category = levs[[j]],
      point_index = seq_len(n),
      unit_id = unit_ids,
      unit_label = unit_labels,
      observed_category = as.character(f),
      local_category_probability = probs[, j],
      membership_count = counts,
      ball_ids = vapply(ball_ids, function(x) paste(sort(unique(x)), collapse=";"), character(1)),
      aggregation = "equal_ball_share_mean",
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

food_local_matrix_from_cover <- function(cover, colour_matrix) {
  x <- as.matrix(colour_matrix)
  storage.mode(x) <- "double"
  if (any(!is.finite(x))) stop("Robustness colour matrix must be finite.", call. = FALSE)
  n <- nrow(x); p <- ncol(x)
  sums <- matrix(0, nrow = n, ncol = p)
  counts <- integer(n)
  for (b in seq_along(cover)) {
    idx <- unique(as.integer(cover[[b]]))
    means <- colMeans(x[idx, , drop = FALSE])
    sums[idx, ] <- sums[idx, , drop = FALSE] +
      matrix(means, nrow = length(idx), ncol = p, byrow = TRUE)
    counts[idx] <- counts[idx] + 1L
  }
  if (any(counts < 1L)) stop("Transient robustness cover leaves observations uncovered.", call. = FALSE)
  sums / counts
}

food_robustness_one <- function(
  axes, unit_ids, colour_matrix, canonical_local_matrix,
  radius, replicate_index, base_seed, core_indices = integer()
) {
  n <- nrow(axes)
  seed <- as.integer(base_seed + as.integer(replicate_index) * 1009L)
  set.seed(seed)
  perm <- sample.int(n, n, replace = FALSE)
  shuffled <- axes[perm, , drop = FALSE]
  const <- data.frame(interface_value = rep(1, n), stringsAsFactors = FALSE)
  bm <- SimplifiedBallMapperCppInterface(shuffled, const, radius)
  cover <- lapply(bm$points_covered_by_landmarks, function(idx) perm[as.integer(idx)])
  gm <- tdabm_ir_graph_metrics(cover, bm$edges, n)
  local <- food_local_matrix_from_cover(cover, colour_matrix)

  comps <- lapply(seq_len(ncol(colour_matrix)), function(j) {
    m <- tdabm_ir_metric_row(local[, j], canonical_local_matrix[, j])
    cbind(
      data.frame(
        replicate = replicate_index,
        replicate_seed = seed,
        radius = radius,
        approved_radius = FOOD_APPROVED_RADIUS,
        is_approved_radius = abs(radius - FOOD_APPROVED_RADIUS) <= 1e-10,
        colour_variable = colnames(colour_matrix)[[j]],
        stringsAsFactors = FALSE
      ),
      m
    )
  })

  topology <- data.frame(
    replicate = replicate_index,
    replicate_seed = seed,
    radius = radius,
    approved_radius = FOOD_APPROVED_RADIUS,
    is_approved_radius = abs(radius - FOOD_APPROVED_RADIUS) <= 1e-10,
    n_balls = gm$n_balls,
    n_edges = gm$n_edges,
    n_components = gm$n_components,
    n_isolated_balls = gm$n_isolated_balls,
    effective_n_balls = gm$effective_n_balls,
    max_ball_share_observations = gm$max_ball_share_observations,
    mean_memberships_per_observation = gm$mean_memberships_per_observation,
    fraction_multicovered = gm$fraction_multicovered,
    stringsAsFactors = FALSE
  )

  list(
    topology = topology,
    comparison = do.call(rbind, comps),
    core_local = if (length(core_indices)) local[, core_indices, drop = FALSE] else NULL
  )
}

food_categorical_plot <- function(bm, layout_table, categories, png_file, pdf_file, legend_title) {
  if (!requireNamespace("igraph", quietly = TRUE)) stop("igraph required.", call. = FALSE)
  n <- length(bm$points_covered_by_landmarks)
  if (length(categories) != n) stop("Categorical map values do not match ball count.", call. = FALSE)
  f <- factor(categories)
  lev <- levels(f)
  pal <- grDevices::hcl.colors(max(3L, length(lev)), "Set 3")[seq_along(lev)]
  cols <- pal[as.integer(f)]

  e <- tdabm_pe_edge_table(bm)
  edf <- if (nrow(e)) data.frame(from=as.character(e$from),to=as.character(e$to)) else
    data.frame(from=character(),to=character())
  g <- igraph::graph_from_data_frame(edf, directed=FALSE,
                                     vertices=data.frame(name=as.character(seq_len(n))))
  layout_table <- layout_table[match(seq_len(n), layout_table$ball_id), , drop=FALSE]
  xy <- as.matrix(layout_table[,c("x","y")])
  sizes <- vapply(bm$points_covered_by_landmarks, length, integer(1))

  draw <- function(path, type) {
    if (type=="png") grDevices::png(path,width=2000,height=1400,res=200) else
      grDevices::pdf(path,width=10,height=7,useDingbats=FALSE)
    on.exit(grDevices::dev.off(), add=TRUE)
    graphics::par(mar=c(1,1,1,1))
    plot(g, layout=xy, vertex.color=cols, vertex.size=10+3*sqrt(sizes),
         vertex.label=seq_len(n), vertex.label.cex=.85, edge.width=1, main="")
    graphics::legend("topright", legend=lev, fill=pal, cex=.75, bty="n",
                     title=legend_title)
  }
  dir.create(dirname(png_file),recursive=TRUE,showWarnings=FALSE)
  dir.create(dirname(pdf_file),recursive=TRUE,showWarnings=FALSE)
  draw(png_file,"png"); draw(pdf_file,"pdf")
  invisible(TRUE)
}

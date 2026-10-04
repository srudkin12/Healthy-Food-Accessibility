#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4) stop("Usage: 02_core_contrast_robustness.R INPUT_CSV OUTPUT_DIR WORKERS REPETITIONS")
input_csv <- normalizePath(args[[1]], mustWork = TRUE)
out_dir <- normalizePath(args[[2]], mustWork = FALSE)
workers <- as.integer(args[[3]])
repetitions <- as.integer(args[[4]])
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
if (!is.finite(workers) || workers < 1L) stop("workers must be >=1")
if (!is.finite(repetitions) || repetitions < 1L) stop("repetitions must be >=1")

options(stringsAsFactors = FALSE)
RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")

log_file <- file.path(out_dir, "R_RUN_LOG.txt")
logcon <- file(log_file, open = "wt")
on.exit(close(logcon), add = TRUE)
logmsg <- function(...) {
  s <- paste0(...)
  cat(s, "\n", file = logcon)
  flush(logcon)
  cat(s, "\n")
}

logmsg("CORE_CONTRAST_ROBUSTNESS_R_START=", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
logmsg("R_VERSION=", R.version.string)
logmsg("INPUT_CSV=", input_csv)
logmsg("WORKERS_REQUESTED=", workers)
logmsg("REPETITIONS=", repetitions)

raw <- read.csv(input_csv, check.names = FALSE)
required <- c(
  "unit_id",
  "physical_friction_log_nearest_large_store_km",
  "transport_constraint_no_car_pct",
  "material_constraint_income_deprivation_2025"
)
missing_cols <- setdiff(required, names(raw))
if (length(missing_cols)) stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
if (nrow(raw) != 33755L) stop("Expected 33755 LSOAs; found ", nrow(raw))
if (anyDuplicated(raw$unit_id)) stop("Duplicate unit_id values detected")

# Canonical input order is stable lexicographic LSOA code order.
o <- order(as.character(raw$unit_id), method = "radix")
raw <- raw[o, , drop = FALSE]
X <- as.matrix(raw[, required[-1], drop = FALSE])
storage.mode(X) <- "double"
if (any(!is.finite(X))) stop("Non-finite values in topology dimensions")
Z <- scale(X, center = TRUE, scale = TRUE)
Z <- unclass(Z)
storage.mode(Z) <- "double"
N <- nrow(Z)

radii <- c(1.40, 1.45, 1.50, 1.55, 1.60)
base_seed <- 20260807L
main_scalar_gap <- 0.25
main_profile_distance <- 1.0
score_thresholds <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30)
profile_thresholds <- c(1.0, 1.5, 2.0, 2.5, 3.0)
th_grid <- expand.grid(
  scalar_gap_threshold = score_thresholds,
  profile_distance_threshold = profile_thresholds,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

# Deliberately simple implementation of the accepted BM cover rule.
# A landmark is the first uncovered observation in the supplied order.
# Its ball contains all observations within epsilon in the 3D z-score space.
build_profiles <- function(order_idx, radius) {
  covered <- rep(FALSE, N)
  # Ball counts in the accepted range are small (<~35); grow if necessary.
  prof <- matrix(NA_real_, nrow = 64L, ncol = 3L)
  landmarks <- integer(64L)
  nb <- 0L
  r2 <- radius * radius
  tol <- 1e-12
  z1 <- Z[, 1L]; z2 <- Z[, 2L]; z3 <- Z[, 3L]
  for (idx in order_idx) {
    if (!covered[[idx]]) {
      d1 <- z1 - z1[[idx]]
      d2 <- z2 - z2[[idx]]
      d3 <- z3 - z3[[idx]]
      inside <- (d1*d1 + d2*d2 + d3*d3) <= (r2 + tol)
      covered[inside] <- TRUE
      nb <- nb + 1L
      if (nb > nrow(prof)) {
        prof <- rbind(prof, matrix(NA_real_, nrow = nrow(prof), ncol = 3L))
        landmarks <- c(landmarks, integer(length(landmarks)))
      }
      prof[nb, ] <- colMeans(Z[inside, , drop = FALSE])
      landmarks[[nb]] <- idx
    }
  }
  if (!all(covered)) stop("Internal error: not all observations covered")
  list(
    profiles = prof[seq_len(nb), , drop = FALSE],
    landmark_indices = landmarks[seq_len(nb)]
  )
}

pair_arrays <- function(P) {
  nb <- nrow(P)
  if (nb < 2L) return(list(total_pairs = 0L, scalar_gap = numeric(), profile_distance = numeric(), pair_index = matrix(integer(), nrow = 2L)))
  cmb <- utils::combn(nb, 2L)
  scores <- rowMeans(P)
  gap <- abs(scores[cmb[1L, ]] - scores[cmb[2L, ]])
  delta <- P[cmb[1L, ], , drop = FALSE] - P[cmb[2L, ], , drop = FALSE]
  pd <- sqrt(rowSums(delta * delta))
  list(total_pairs = ncol(cmb), scalar_gap = gap, profile_distance = pd, pair_index = cmb, scores = scores)
}

summarise_construction <- function(order_idx, radius, keep_pairs = FALSE) {
  b <- build_profiles(order_idx, radius)
  pa <- pair_arrays(b$profiles)
  main_flag <- pa$scalar_gap < main_scalar_gap & pa$profile_distance > main_profile_distance
  grid_counts <- vapply(seq_len(nrow(th_grid)), function(g) {
    sum(pa$scalar_gap < th_grid$scalar_gap_threshold[[g]] &
          pa$profile_distance > th_grid$profile_distance_threshold[[g]])
  }, integer(1L))
  ans <- list(
    n_balls = nrow(b$profiles),
    total_pairs = pa$total_pairs,
    n_qualifying_pairs = sum(main_flag),
    pair_prevalence = if (pa$total_pairs > 0L) sum(main_flag) / pa$total_pairs else NA_real_,
    grid_counts = grid_counts
  )
  if (keep_pairs) {
    ans$profiles <- b$profiles
    ans$landmark_indices <- b$landmark_indices
    ans$pair_index <- pa$pair_index
    ans$scalar_gap <- pa$scalar_gap
    ans$profile_distance <- pa$profile_distance
    ans$scores <- pa$scores
  }
  ans
}

# ---- Canonical validation ----
logmsg("CANONICAL_VALIDATION_START")
canonical <- summarise_construction(seq_len(N), 1.50, keep_pairs = TRUE)
canonical_landmarks <- data.frame(
  ball_id = seq_along(canonical$landmark_indices),
  point_index = canonical$landmark_indices,
  unit_id = raw$unit_id[canonical$landmark_indices],
  unit_label = if ("unit_label" %in% names(raw)) raw$unit_label[canonical$landmark_indices] else NA_character_
)
write.csv(canonical_landmarks, file.path(out_dir, "canonical_landmarks_reproduced.csv"), row.names = FALSE)

canonical_profiles <- data.frame(
  ball_id = seq_len(nrow(canonical$profiles)),
  retail_proximity_z = canonical$profiles[,1],
  household_car_availability_z = canonical$profiles[,2],
  income_deprivation_z = canonical$profiles[,3],
  equal_weight_score = rowMeans(canonical$profiles)
)
write.csv(canonical_profiles, file.path(out_dir, "canonical_ball_profiles_reproduced.csv"), row.names = FALSE)

cmb <- canonical$pair_index
canonical_pairs <- data.frame(
  ball_a = cmb[1L,],
  ball_b = cmb[2L,],
  scalar_gap = canonical$scalar_gap,
  profile_distance = canonical$profile_distance,
  qualifies_main = canonical$scalar_gap < main_scalar_gap & canonical$profile_distance > main_profile_distance
)
write.csv(canonical_pairs, file.path(out_dir, "canonical_pair_contrasts_reproduced.csv"), row.names = FALSE)

canonical_grid <- cbind(th_grid, n_qualifying_pairs = canonical$grid_counts)
write.csv(canonical_grid, file.path(out_dir, "canonical_threshold_grid_reproduced.csv"), row.names = FALSE)

idx_1318 <- which(canonical_pairs$ball_a == 13L & canonical_pairs$ball_b == 18L)
if (length(idx_1318) != 1L) stop("Could not identify canonical Ball 13 / Ball 18 pair")
canonical_metrics <- data.frame(
  metric = c("n_lsoa", "n_balls", "total_pairs", "n_qualifying_pairs", "pair_prevalence", "ball13_18_scalar_gap", "ball13_18_profile_distance"),
  observed = c(N, canonical$n_balls, canonical$total_pairs, canonical$n_qualifying_pairs, canonical$pair_prevalence,
               canonical_pairs$scalar_gap[idx_1318], canonical_pairs$profile_distance[idx_1318])
)
write.csv(canonical_metrics, file.path(out_dir, "canonical_metrics_reproduced.csv"), row.names = FALSE)
logmsg("CANONICAL_N_BALLS=", canonical$n_balls)
logmsg("CANONICAL_TOTAL_PAIRS=", canonical$total_pairs)
logmsg("CANONICAL_QUALIFYING_PAIRS=", canonical$n_qualifying_pairs)
logmsg("CANONICAL_PAIR_PREVALENCE=", sprintf("%.12f", canonical$pair_prevalence))
logmsg("CANONICAL_B13_B18_SCALAR_GAP=", sprintf("%.13f", canonical_pairs$scalar_gap[idx_1318]))
logmsg("CANONICAL_B13_B18_PROFILE_DISTANCE=", sprintf("%.13f", canonical_pairs$profile_distance[idx_1318]))

# Hard canonical guards before expensive work.
if (canonical$n_balls != 24L) stop("CANONICAL_FAIL: expected 24 balls")
if (canonical$total_pairs != 276L) stop("CANONICAL_FAIL: expected 276 ball pairs")
if (canonical$n_qualifying_pairs != 38L) stop("CANONICAL_FAIL: expected 38 qualifying pairs")
if (abs(canonical_pairs$scalar_gap[idx_1318] - 0.0018742566141) > 1e-9) stop("CANONICAL_FAIL: Ball 13/18 scalar gap mismatch")
if (abs(canonical_pairs$profile_distance[idx_1318] - 4.3972327089) > 2e-6) stop("CANONICAL_FAIL: Ball 13/18 profile distance mismatch")
logmsg("CANONICAL_NUMERIC_GATES=PASS")

# ---- Robustness constructions ----
tasks <- expand.grid(radius = radii, replicate = seq_len(repetitions), KEEP.OUT.ATTRS = FALSE)
tasks <- tasks[order(tasks$radius, tasks$replicate), , drop = FALSE]
detected_cores <- parallel::detectCores(logical = TRUE)
if (is.na(detected_cores) || detected_cores < 1L) detected_cores <- 1L
workers_use <- min(workers, detected_cores, nrow(tasks))
logmsg("WORKERS_USED=", workers_use)
logmsg("ROBUSTNESS_TASKS=", nrow(tasks))
logmsg("ROBUSTNESS_START=", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))

run_one <- function(ii) {
  rr <- tasks$radius[[ii]]
  repi <- tasks$replicate[[ii]]
  seed <- base_seed + repi * 1009L
  set.seed(seed)
  ord <- sample.int(N, N, replace = FALSE)
  st <- summarise_construction(ord, rr, keep_pairs = FALSE)
  list(
    main = c(radius = rr, replicate = repi, seed = seed,
             n_balls = st$n_balls, total_pairs = st$total_pairs,
             n_qualifying_pairs = st$n_qualifying_pairs,
             pair_prevalence = st$pair_prevalence),
    grid = st$grid_counts
  )
}

if (.Platform$OS.type == "unix" && workers_use > 1L) {
  res <- parallel::mclapply(seq_len(nrow(tasks)), run_one, mc.cores = workers_use, mc.preschedule = TRUE, mc.set.seed = FALSE)
} else {
  logmsg("PARALLEL_FALLBACK=sequential")
  res <- lapply(seq_len(nrow(tasks)), run_one)
}

main_mat <- do.call(rbind, lapply(res, `[[`, "main"))
main_df <- as.data.frame(main_mat)
main_df$replicate <- as.integer(main_df$replicate)
main_df$seed <- as.integer(main_df$seed)
main_df$n_balls <- as.integer(main_df$n_balls)
main_df$total_pairs <- as.integer(main_df$total_pairs)
main_df$n_qualifying_pairs <- as.integer(main_df$n_qualifying_pairs)
write.csv(main_df, file.path(out_dir, "core_contrast_by_replicate.csv"), row.names = FALSE)

# Full threshold-grid by replicate. 150k rows at the default design.
grid_rows <- vector("list", length(res))
for (ii in seq_along(res)) {
  g <- th_grid
  g$radius <- tasks$radius[[ii]]
  g$replicate <- tasks$replicate[[ii]]
  g$seed <- base_seed + tasks$replicate[[ii]] * 1009L
  g$n_balls <- main_df$n_balls[[ii]]
  g$total_pairs <- main_df$total_pairs[[ii]]
  g$n_qualifying_pairs <- as.integer(res[[ii]]$grid)
  g$pair_prevalence <- ifelse(g$total_pairs > 0, g$n_qualifying_pairs / g$total_pairs, NA_real_)
  grid_rows[[ii]] <- g[, c("radius","replicate","seed","n_balls","total_pairs","scalar_gap_threshold","profile_distance_threshold","n_qualifying_pairs","pair_prevalence")]
}
grid_df <- do.call(rbind, grid_rows)
# gzip CSV using base R connection.
gz <- gzfile(file.path(out_dir, "threshold_grid_by_replicate.csv.gz"), open = "wt")
write.csv(grid_df, gz, row.names = FALSE)
close(gz)

qfun <- function(x, p) as.numeric(stats::quantile(x, probs = p, names = FALSE, type = 7, na.rm = TRUE))
summary_rows <- lapply(radii, function(rr) {
  d <- main_df[abs(main_df$radius - rr) < 1e-10, ]
  data.frame(
    radius = rr,
    repetitions = nrow(d),
    median_n_balls = median(d$n_balls),
    q10_n_balls = qfun(d$n_balls, .10),
    q90_n_balls = qfun(d$n_balls, .90),
    min_n_balls = min(d$n_balls),
    max_n_balls = max(d$n_balls),
    median_qualifying_pairs = median(d$n_qualifying_pairs),
    q10_qualifying_pairs = qfun(d$n_qualifying_pairs, .10),
    q90_qualifying_pairs = qfun(d$n_qualifying_pairs, .90),
    min_qualifying_pairs = min(d$n_qualifying_pairs),
    max_qualifying_pairs = max(d$n_qualifying_pairs),
    median_pair_prevalence = median(d$pair_prevalence),
    q10_pair_prevalence = qfun(d$pair_prevalence, .10),
    q90_pair_prevalence = qfun(d$pair_prevalence, .90),
    min_pair_prevalence = min(d$pair_prevalence),
    max_pair_prevalence = max(d$pair_prevalence),
    share_constructions_with_any_qualifying_pair = mean(d$n_qualifying_pairs > 0),
    share_constructions_at_or_above_canonical_prevalence = mean(d$pair_prevalence >= (38/276))
  )
})
summary_df <- do.call(rbind, summary_rows)
write.csv(summary_df, file.path(out_dir, "core_contrast_summary_by_radius.csv"), row.names = FALSE)

# Same replicate/order schedule across radii: summarise within-replicate range.
wide_split <- split(main_df, main_df$replicate)
cross_rows <- lapply(wide_split, function(d) {
  d <- d[order(d$radius),]
  data.frame(
    replicate = d$replicate[[1]],
    seed = d$seed[[1]],
    min_n_balls = min(d$n_balls),
    max_n_balls = max(d$n_balls),
    min_qualifying_pairs = min(d$n_qualifying_pairs),
    max_qualifying_pairs = max(d$n_qualifying_pairs),
    min_pair_prevalence = min(d$pair_prevalence),
    max_pair_prevalence = max(d$pair_prevalence),
    all_five_radii_have_qualifying_pair = all(d$n_qualifying_pairs > 0)
  )
})
cross_df <- do.call(rbind, cross_rows)
write.csv(cross_df, file.path(out_dir, "cross_radius_same_order_summary.csv"), row.names = FALSE)

# Aggregate threshold grid by radius and threshold cell.
key <- interaction(grid_df$radius, grid_df$scalar_gap_threshold, grid_df$profile_distance_threshold, drop = TRUE)
groups <- split(grid_df, key)
th_sum <- do.call(rbind, lapply(groups, function(d) {
  data.frame(
    radius = d$radius[[1]],
    scalar_gap_threshold = d$scalar_gap_threshold[[1]],
    profile_distance_threshold = d$profile_distance_threshold[[1]],
    repetitions = nrow(d),
    median_qualifying_pairs = median(d$n_qualifying_pairs),
    q10_qualifying_pairs = qfun(d$n_qualifying_pairs, .10),
    q90_qualifying_pairs = qfun(d$n_qualifying_pairs, .90),
    min_qualifying_pairs = min(d$n_qualifying_pairs),
    max_qualifying_pairs = max(d$n_qualifying_pairs),
    median_pair_prevalence = median(d$pair_prevalence),
    q10_pair_prevalence = qfun(d$pair_prevalence, .10),
    q90_pair_prevalence = qfun(d$pair_prevalence, .90),
    min_pair_prevalence = min(d$pair_prevalence),
    max_pair_prevalence = max(d$pair_prevalence),
    share_constructions_with_any_qualifying_pair = mean(d$n_qualifying_pairs > 0)
  )
}))
th_sum <- th_sum[order(th_sum$radius, th_sum$profile_distance_threshold, th_sum$scalar_gap_threshold),]
write.csv(th_sum, file.path(out_dir, "threshold_grid_summary_by_radius.csv"), row.names = FALSE)

# Machine-readable overall summary.
overall <- data.frame(
  metric = c(
    "total_constructions",
    "share_all_same_order_replicates_nonzero_across_all_five_radii",
    "minimum_pair_prevalence_across_all_constructions",
    "median_pair_prevalence_across_all_constructions",
    "maximum_pair_prevalence_across_all_constructions"
  ),
  value = c(
    nrow(main_df),
    mean(cross_df$all_five_radii_have_qualifying_pair),
    min(main_df$pair_prevalence),
    median(main_df$pair_prevalence),
    max(main_df$pair_prevalence)
  )
)
write.csv(overall, file.path(out_dir, "overall_robustness_summary.csv"), row.names = FALSE)

logmsg("ROBUSTNESS_END=", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
logmsg("TOTAL_CONSTRUCTIONS=", nrow(main_df))
logmsg("SHARE_SAME_ORDER_REPLICATES_NONZERO_ALL_RADII=", sprintf("%.6f", mean(cross_df$all_five_radii_have_qualifying_pair)))
logmsg("GLOBAL_MIN_PAIR_PREVALENCE=", sprintf("%.6f", min(main_df$pair_prevalence)))
logmsg("GLOBAL_MEDIAN_PAIR_PREVALENCE=", sprintf("%.6f", median(main_df$pair_prevalence)))
logmsg("GLOBAL_MAX_PAIR_PREVALENCE=", sprintf("%.6f", max(main_df$pair_prevalence)))
logmsg("CORE_CONTRAST_ROBUSTNESS_R_STATUS=PASS")

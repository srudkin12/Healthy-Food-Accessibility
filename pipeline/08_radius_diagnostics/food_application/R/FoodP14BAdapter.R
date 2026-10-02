# Food HFA scalable adapter for TDABM Portable Framework P1.4-B v1.1.3.
# The generic framework is not modified.
# Exact graph/resolution diagnostics and full landmark stability are retained.
# Cover-relation stability is calculated on a fixed 256-LSOA audit sample.

food_p14b_atomic_save_rds <- function(object, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(file, ".tmp.", Sys.getpid())
  saveRDS(object, tmp, compress = "gzip")
  if (!file.rename(tmp, file)) {
    unlink(tmp)
    stop("Could not atomically write checkpoint: ", file, call. = FALSE)
  }
  invisible(file)
}

food_p14b_atomic_write_csv <- function(x, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(file, ".tmp.", Sys.getpid())
  utils::write.csv(x, tmp, row.names = FALSE)
  if (!file.rename(tmp, file)) {
    unlink(tmp)
    stop("Could not atomically write CSV: ", file, call. = FALSE)
  }
  invisible(file)
}

food_p14b_sample_cover_signature <- function(cover, sample_indices, n_observations) {
  m <- length(sample_indices)
  lookup <- integer(n_observations)
  lookup[sample_indices] <- seq_len(m)

  reduced <- lapply(cover, function(idx) {
    z <- lookup[as.integer(idx)]
    unique(z[z > 0L])
  })
  reduced <- reduced[lengths(reduced) >= 2L]

  tdabm_rd_cover_signature(reduced, m)
}

food_p14b_unpack_matrix <- function(signatures, n_bits) {
  if (!length(signatures)) stop("No signatures supplied.", call. = FALSE)
  out <- matrix(FALSE, nrow = length(signatures), ncol = n_bits)
  for (i in seq_along(signatures)) {
    out[i, ] <- tdabm_rd_unpack_signature(signatures[[i]], n_bits)
  }
  out
}

food_p14b_pairwise_jaccard_stats <- function(signatures, n_bits) {
  m <- food_p14b_unpack_matrix(signatures, n_bits)

  # Convert once; BLAS matrix multiplication gives all intersections efficiently.
  md <- matrix(as.numeric(m), nrow = nrow(m), ncol = ncol(m))
  intersections <- tcrossprod(md)
  totals <- rowSums(md)
  unions <- outer(totals, totals, "+") - intersections

  jj <- intersections / unions
  jj[unions == 0] <- 1

  vals <- jj[upper.tri(jj)]
  if (!length(vals)) stop("At least two repetitions are required.", call. = FALSE)

  c(
    pairwise_comparisons = length(vals),
    mean = mean(vals),
    q10 = as.numeric(stats::quantile(vals, 0.10, names = FALSE, type = 7)),
    median = stats::median(vals),
    min = min(vals)
  )
}

food_p14b_run_graph <- function(
  axes,
  unit_ids,
  radius,
  replicate_index,
  base_seed,
  sample_indices,
  canonical = FALSE
) {
  n <- nrow(axes)

  if (canonical) {
    permutation <- seq_len(n)
    seed <- 0L
  } else {
    seed <- as.integer(base_seed + as.integer(replicate_index) * 1009L)
    set.seed(seed)
    permutation <- sample.int(n, size = n, replace = FALSE)
  }

  shuffled <- axes[permutation, , drop = FALSE]
  constant_values <- data.frame(interface_value = rep(1, n), stringsAsFactors = FALSE)

  started <- proc.time()[["elapsed"]]
  bm <- SimplifiedBallMapperCppInterface(shuffled, constant_values, radius)
  elapsed <- proc.time()[["elapsed"]] - started

  cover_shuffled <- bm$points_covered_by_landmarks
  if (!is.list(cover_shuffled) || length(cover_shuffled) < 1L) {
    stop("Ball Mapper returned no cover.", call. = FALSE)
  }

  cover <- lapply(
    cover_shuffled,
    function(idx) permutation[as.integer(idx)]
  )

  landmark_positions <- tdabm_rd_cpp_landmark_positions_one_based(bm$landmarks, n)
  landmarks <- permutation[landmark_positions]

  n_balls <- length(cover)
  true_ball_sizes <- vapply(cover, length, integer(1))
  if (any(true_ball_sizes < 1L)) stop("Ball Mapper returned an empty ball.", call. = FALSE)

  resolution <- tdabm_rd_ball_resolution_metrics(cover, n)

  edges <- as.matrix(bm$edges)
  if (length(edges) == 0L) edges <- matrix(numeric(0), ncol = 2L)
  graph <- tdabm_rd_graph_components(n_balls, edges)

  membership_counts_shuffled <- vapply(bm$coverage, length, integer(1))
  if (length(membership_counts_shuffled) != n || any(membership_counts_shuffled < 1L)) {
    stop("Every observation must belong to at least one ball.", call. = FALSE)
  }

  membership_counts <- integer(n)
  membership_counts[permutation] <- membership_counts_shuffled

  sampled_cover_signature <- food_p14b_sample_cover_signature(
    cover = cover,
    sample_indices = sample_indices,
    n_observations = n
  )

  landmark_signature <- tdabm_rd_landmark_signature(landmarks, n)

  metrics <- data.frame(
    radius = as.numeric(radius),
    replicate = as.integer(replicate_index),
    replicate_seed = seed,
    canonical_order = isTRUE(canonical),
    n_observations = n,
    n_balls = n_balls,
    n_edges = nrow(edges),
    n_components = graph$n_components,
    n_nontrivial_components = graph$n_nontrivial_components,
    n_isolated_balls = graph$n_isolated_vertices,
    largest_component_share_balls = graph$largest_component_size / n_balls,
    connected = graph$n_components == 1L,
    no_isolated_balls = graph$n_isolated_vertices == 0L,
    one_ball = n_balls == 1L,
    all_points_landmarks = n_balls == n,
    min_ball_size = min(true_ball_sizes),
    median_ball_size = stats::median(true_ball_sizes),
    mean_ball_size = mean(true_ball_sizes),
    max_ball_size = max(true_ball_sizes),
    effective_n_balls = resolution$effective_n_balls,
    effective_ball_fraction = resolution$effective_ball_fraction,
    normalized_ball_size_entropy = resolution$normalized_ball_size_entropy,
    max_ball_share_observations = resolution$max_ball_share_observations,
    majority_ball = resolution$majority_ball,
    severe_dominance_ball = resolution$severe_dominance_ball,
    mean_memberships_per_observation = mean(membership_counts),
    max_memberships_per_observation = max(membership_counts),
    fraction_multicovered = mean(membership_counts > 1L),
    mean_degree = mean(graph$degree),
    max_degree = max(graph$degree),
    sd_degree = if (length(graph$degree) > 1L) stats::sd(graph$degree) else 0,
    edge_density = if (n_balls > 1L) nrow(edges) / choose(n_balls, 2L) else 0,
    cycle_rank = nrow(edges) - n_balls + graph$n_components,
    elapsed_seconds = elapsed,
    stringsAsFactors = FALSE
  )

  list(
    metrics = metrics,
    sample_cover_signature = sampled_cover_signature,
    landmark_signature = landmark_signature
  )
}

food_p14b_summarise_radius <- function(metrics, sample_signatures, landmark_signatures, sample_size, n_observations) {
  if (nrow(metrics) < 2L) stop("At least two random-order repetitions are required.", call. = FALSE)

  cover_stats <- food_p14b_pairwise_jaccard_stats(
    sample_signatures,
    choose(sample_size, 2L)
  )
  landmark_stats <- food_p14b_pairwise_jaccard_stats(
    landmark_signatures,
    n_observations
  )

  data.frame(
    radius = unique(metrics$radius)[[1L]],
    repetitions = nrow(metrics),
    pairwise_comparisons = cover_stats[["pairwise_comparisons"]],
    mean_pairwise_cover_jaccard = cover_stats[["mean"]],
    q10_pairwise_cover_jaccard = cover_stats[["q10"]],
    median_pairwise_cover_jaccard = cover_stats[["median"]],
    min_pairwise_cover_jaccard = cover_stats[["min"]],
    mean_pairwise_landmark_jaccard = landmark_stats[["mean"]],
    median_pairwise_landmark_jaccard = landmark_stats[["median"]],
    min_pairwise_landmark_jaccard = landmark_stats[["min"]],
    cover_relation_scope = paste0("fixed_", sample_size, "_LSOA_audit_sample"),
    landmark_scope = "full_33755_LSOAs",
    stringsAsFactors = FALSE
  )
}

food_p14b_metrics_summary_row <- function(metrics) {
  x <- metrics
  data.frame(
    radius = unique(x$radius)[[1L]],
    repetitions = nrow(x),
    median_n_balls = stats::median(x$n_balls),
    q10_n_balls = tdabm_rd_q(x$n_balls, 0.10),
    q90_n_balls = tdabm_rd_q(x$n_balls, 0.90),
    sd_n_balls = stats::sd(x$n_balls),
    median_n_edges = stats::median(x$n_edges),
    q10_n_edges = tdabm_rd_q(x$n_edges, 0.10),
    q90_n_edges = tdabm_rd_q(x$n_edges, 0.90),
    median_n_components = stats::median(x$n_components),
    median_n_isolated_balls = stats::median(x$n_isolated_balls),
    connected_rate = mean(x$connected),
    no_isolate_rate = mean(x$no_isolated_balls),
    one_ball_rate = mean(x$one_ball),
    all_points_landmarks_rate = mean(x$all_points_landmarks),
    median_largest_component_share_balls = stats::median(x$largest_component_share_balls),
    median_mean_ball_size = stats::median(x$mean_ball_size),
    median_max_ball_size = stats::median(x$max_ball_size),
    median_effective_n_balls = stats::median(x$effective_n_balls),
    median_effective_ball_fraction = stats::median(x$effective_ball_fraction),
    median_normalized_ball_size_entropy = stats::median(x$normalized_ball_size_entropy),
    median_max_ball_share_observations = stats::median(x$max_ball_share_observations),
    majority_ball_rate = mean(x$majority_ball),
    severe_dominance_ball_rate = mean(x$severe_dominance_ball),
    median_mean_memberships_per_observation = stats::median(x$mean_memberships_per_observation),
    median_fraction_multicovered = stats::median(x$fraction_multicovered),
    median_mean_degree = stats::median(x$mean_degree),
    median_max_degree = stats::median(x$max_degree),
    median_edge_density = stats::median(x$edge_density),
    median_cycle_rank = stats::median(x$cycle_rank),
    median_elapsed_seconds = stats::median(x$elapsed_seconds),
    stringsAsFactors = FALSE
  )
}

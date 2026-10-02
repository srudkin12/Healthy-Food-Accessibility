#!/usr/bin/env Rscript
options(warn = 1); options(stringsAsFactors = FALSE)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Usage: 04_pair_sample_and_contrasts.R <package_root>", call. = FALSE)

package_root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
source(file.path(package_root, "R", "SpatialClosureHelpers.R"))
out <- file.path(package_root, "results")
work <- file.path(package_root, "work")
p14d <- file.path(package_root, "accepted_evidence", "p1_4_d")
food24 <- file.path(package_root, "accepted_evidence", "food24")

readout <- utils::read.csv(file.path(p14d, "food_hfa_readout_frame.csv"), stringsAsFactors = FALSE, check.names = FALSE)
memberships <- utils::read.csv(file.path(p14d, "canonical_memberships.csv"), stringsAsFactors = FALSE)
points <- utils::read.csv(file.path(out, "canonical_spatial_representative_points.csv"), stringsAsFactors = FALSE)
edges <- utils::read.csv(gzfile(file.path(out, "queen_adjacency_edges.csv.gz")), stringsAsFactors = FALSE)
shortlist <- utils::read.csv(file.path(food24, "candidate_non_scalar_contrast_shortlist.csv"), stringsAsFactors = FALSE)

axis_vars <- c(
  "physical_friction_log_nearest_large_store_km",
  "transport_constraint_no_car_pct",
  "material_constraint_income_deprivation_2025"
)
z <- scale(as.matrix(readout[,axis_vars,drop=FALSE]), center = TRUE, scale = TRUE)
xy <- as.matrix(points[,c("easting","northing")])
memb_mat <- sc_membership_matrix(memberships, readout$unit_id)

# Deterministic pair sample.
pairs <- sc_pair_sample(SC_N, SC_PAIR_SAMPLE_N, SC_PAIR_SEED)
i <- pairs$i; j <- pairs$j
config_d <- sqrt(rowSums((z[i,,drop=FALSE] - z[j,,drop=FALSE])^2))
geo_d <- sqrt((xy[i,1]-xy[j,1])^2 + (xy[i,2]-xy[j,2])^2) / 1000
shared <- rowSums(memb_mat[i,,drop=FALSE] & memb_mat[j,,drop=FALSE])

pair_frame <- data.frame(
  unit_id_a = readout$unit_id[i],
  unit_id_b = readout$unit_id[j],
  configuration_distance_z = config_d,
  geographic_distance_km = geo_d,
  shared_ball_count = shared,
  any_shared_ball = shared > 0L,
  stringsAsFactors = FALSE
)
sc_write_gz_csv(pair_frame, file.path(out, "configuration_geography_pair_sample.csv.gz"))

# Decile profiles.
config_dec <- cut(
  config_d,
  breaks = stats::quantile(config_d, probs = seq(0,1,.1), names = FALSE),
  include.lowest = TRUE,
  labels = FALSE
)
geo_dec <- cut(
  geo_d,
  breaks = stats::quantile(geo_d, probs = seq(0,1,.1), names = FALSE),
  include.lowest = TRUE,
  labels = FALSE
)

by_config <- do.call(rbind, lapply(sort(unique(config_dec)), function(d) {
  q <- config_dec == d
  data.frame(
    conditioning = "configuration_distance_decile",
    decile = d,
    n = sum(q),
    median_configuration_distance_z = stats::median(config_d[q]),
    median_geographic_distance_km = stats::median(geo_d[q]),
    share_pairs_gt50km = mean(geo_d[q] > 50),
    share_pairs_gt100km = mean(geo_d[q] > 100),
    share_pairs_with_shared_ball = mean(shared[q] > 0L),
    stringsAsFactors = FALSE
  )
}))
by_geo <- do.call(rbind, lapply(sort(unique(geo_dec)), function(d) {
  q <- geo_dec == d
  data.frame(
    conditioning = "geographic_distance_decile",
    decile = d,
    n = sum(q),
    median_configuration_distance_z = stats::median(config_d[q]),
    median_geographic_distance_km = stats::median(geo_d[q]),
    share_pairs_gt50km = mean(geo_d[q] > 50),
    share_pairs_gt100km = mean(geo_d[q] > 100),
    share_pairs_with_shared_ball = mean(shared[q] > 0L),
    stringsAsFactors = FALSE
  )
}))
utils::write.csv(rbind(by_config, by_geo), file.path(out, "configuration_geography_distance_deciles.csv"), row.names = FALSE)

pair_summary <- data.frame(
  pair_sample_n = length(config_d),
  sample_seed = SC_PAIR_SEED,
  pearson_config_geo = sc_safe_cor(config_d, geo_d, "pearson"),
  spearman_config_geo = sc_safe_cor(config_d, geo_d, "spearman"),
  median_geo_distance_km = stats::median(geo_d),
  median_geo_distance_lowest_config_decile_km = stats::median(geo_d[config_dec == min(config_dec, na.rm=TRUE)]),
  share_lowest_config_decile_pairs_gt50km = mean(geo_d[config_dec == min(config_dec, na.rm=TRUE)] > 50),
  share_lowest_config_decile_pairs_gt100km = mean(geo_d[config_dec == min(config_dec, na.rm=TRUE)] > 100),
  shared_ball_pair_share = mean(shared > 0L),
  stringsAsFactors = FALSE
)
utils::write.csv(pair_summary, file.path(out, "configuration_geography_pair_summary.csv"), row.names = FALSE)

# Spatial context for the accepted non-scalar contrast shortlist.
ball_members <- lapply(seq_len(24L), function(b) which(memb_mat[,b]))
contrast_rows <- lapply(seq_len(nrow(shortlist)), function(r) {
  a <- as.integer(shortlist$ball_a[[r]])
  b <- as.integer(shortlist$ball_b[[r]])
  ia <- ball_members[[a]]
  ib <- ball_members[[b]]
  center_a <- colMeans(xy[ia,,drop=FALSE])
  center_b <- colMeans(xy[ib,,drop=FALSE])
  centroid_d <- sqrt(sum((center_a-center_b)^2)) / 1000

  shared_idx <- intersect(ia, ib)
  a_only <- setdiff(ia, ib)
  b_only <- setdiff(ib, ia)

  min_exclusive <- NA_real_
  med_a_to_b <- NA_real_
  if (length(a_only) && length(b_only)) {
    if (length(b_only) >= 2L) {
      nn <- RANN::nn2(xy[b_only,,drop=FALSE], query=xy[a_only,,drop=FALSE], k=1L)
      d <- as.numeric(nn$nn.dists[,1]) / 1000
      min_exclusive <- min(d)
      med_a_to_b <- stats::median(d)
    } else {
      d <- sqrt((xy[a_only,1]-xy[b_only,1])^2 + (xy[a_only,2]-xy[b_only,2])^2) / 1000
      min_exclusive <- min(d)
      med_a_to_b <- stats::median(d)
    }
  }

  a_flag <- logical(SC_N); a_flag[ia] <- TRUE
  b_flag <- logical(SC_N); b_flag[ib] <- TRUE
  cross_adj <- sum(
    (a_flag[edges$from] & b_flag[edges$to]) |
    (a_flag[edges$to] & b_flag[edges$from])
  )

  data.frame(
    ball_a = a,
    ball_b = b,
    n_members_a = length(ia),
    n_members_b = length(ib),
    shared_members = length(shared_idx),
    membership_jaccard = length(shared_idx) / length(union(ia,ib)),
    geographic_centroid_distance_km = centroid_d,
    minimum_exclusive_member_distance_km = min_exclusive,
    median_a_to_nearest_b_exclusive_distance_km = med_a_to_b,
    cross_ball_queen_adjacency_edges = cross_adj,
    scalar_gap = shortlist$scalar_gap[[r]],
    composition_euclidean_distance = shortlist$composition_euclidean_distance[[r]],
    ruc_total_variation = shortlist$ruc_total_variation[[r]],
    iuc_total_variation = shortlist$iuc_total_variation[[r]],
    signature_a = shortlist$signature_a[[r]],
    signature_b = shortlist$signature_b[[r]],
    stringsAsFactors = FALSE
  )
})
contrast_spatial <- do.call(rbind, contrast_rows)
utils::write.csv(contrast_spatial, file.path(out, "non_scalar_contrast_spatial_context.csv"), row.names = FALSE)

# Scatter based on deterministic pair sample.
sub <- seq_len(min(50000L, nrow(pair_frame)))
sc_plot_png_pdf(
  file.path(package_root, "figures", "configuration_vs_geographic_distance.png"),
  file.path(package_root, "figures", "configuration_vs_geographic_distance.pdf"),
  function() {
    graphics::plot(
      pair_frame$geographic_distance_km[sub],
      pair_frame$configuration_distance_z[sub],
      xlab = "Geographic distance (km)",
      ylab = "Configuration distance (z-space)",
      pch = "."
    )
  }
)

cat("PAIR_AND_CONTRAST_SPATIAL_OK pairs=", nrow(pair_frame),
    " contrasts=", nrow(contrast_spatial), "\n", sep="")

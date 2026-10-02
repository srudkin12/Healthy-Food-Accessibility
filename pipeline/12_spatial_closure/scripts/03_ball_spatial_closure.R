#!/usr/bin/env Rscript
options(warn = 1); options(stringsAsFactors = FALSE)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Usage: 03_ball_spatial_closure.R <package_root>", call. = FALSE)

package_root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
source(file.path(package_root, "R", "SpatialClosureHelpers.R"))
out <- file.path(package_root, "results")
work <- file.path(package_root, "work")
p14d <- file.path(package_root, "accepted_evidence", "p1_4_d")
food24 <- file.path(package_root, "accepted_evidence", "food24")

readout <- utils::read.csv(file.path(p14d, "food_hfa_readout_frame.csv"), stringsAsFactors = FALSE, check.names = FALSE)
memberships <- utils::read.csv(file.path(p14d, "canonical_memberships.csv"), stringsAsFactors = FALSE)
points <- utils::read.csv(file.path(out, "canonical_spatial_representative_points.csv"), stringsAsFactors = FALSE)
neigh <- readRDS(file.path(work, "queen_neighbour_list.rds"))
g <- readRDS(file.path(work, "queen_adjacency_graph.rds"))
interp <- utils::read.csv(file.path(food24, "24_ball_interpretation_matrix.csv"), stringsAsFactors = FALSE)

memb_mat <- sc_membership_matrix(memberships, readout$unit_id)
xy <- as.matrix(points[,c("easting","northing")])

rows <- lapply(seq_len(24L), function(b) {
  idx <- which(memb_mat[,b])
  center <- colMeans(xy[idx,,drop=FALSE])
  dkm <- sqrt((xy[idx,1]-center[[1]])^2 + (xy[idx,2]-center[[2]])^2) / 1000
  comp <- sc_graph_component_summary(idx, g)

  within_edges <- 0L
  for (i in idx) {
    within_edges <- within_edges + sum(neigh[[i]] %in% idx)
  }
  within_edges <- as.integer(within_edges / 2L)

  data.frame(
    ball_id = b,
    n_members = length(idx),
    geographic_centroid_easting = center[[1]],
    geographic_centroid_northing = center[[2]],
    median_member_distance_to_ball_centroid_km = stats::median(dkm),
    q90_member_distance_to_ball_centroid_km = sc_quantile(dkm, .90),
    max_member_distance_to_ball_centroid_km = max(dkm),
    within_ball_queen_edges = within_edges,
    mean_within_ball_queen_degree = 2 * within_edges / length(idx),
    n_spatial_components = comp$n_spatial_components,
    largest_spatial_component_members = comp$largest_spatial_component_members,
    largest_spatial_component_share = comp$largest_spatial_component_share,
    singleton_spatial_components = comp$singleton_spatial_components,
    moran_i_membership = sc_rowstd_moran(as.integer(memb_mat[,b]), neigh),
    stringsAsFactors = FALSE
  )
})
ball_spatial <- do.call(rbind, rows)

# Add accepted substantive signature for interpretation.
keep_cols <- c(
  "ball_id", "axis_signature", "axis_mean_constraint_z",
  "mode__ruc_category", "mode_share__ruc_category",
  "mode__iuc_category", "mode_share__iuc_category"
)
ball_spatial <- merge(
  ball_spatial,
  interp[,keep_cols,drop=FALSE],
  by = "ball_id",
  all.x = TRUE,
  sort = TRUE
)
utils::write.csv(ball_spatial, file.path(out, "24_ball_spatial_closure.csv"), row.names = FALSE)

# Figure: membership Moran I versus spatial dispersion.
sc_plot_png_pdf(
  file.path(package_root, "figures", "ball_membership_moran_vs_dispersion.png"),
  file.path(package_root, "figures", "ball_membership_moran_vs_dispersion.pdf"),
  function() {
    graphics::plot(
      ball_spatial$q90_member_distance_to_ball_centroid_km,
      ball_spatial$moran_i_membership,
      xlab = "90th percentile distance to ball centroid (km)",
      ylab = "Moran's I of ball membership",
      pch = 1
    )
    graphics::text(
      ball_spatial$q90_member_distance_to_ball_centroid_km,
      ball_spatial$moran_i_membership,
      labels = ball_spatial$ball_id,
      pos = 3,
      cex = 0.8
    )
  }
)

cat("BALL_SPATIAL_CLOSURE_OK balls=", nrow(ball_spatial),
    " multi_component_balls=", sum(ball_spatial$n_spatial_components > 1L), "\n", sep="")

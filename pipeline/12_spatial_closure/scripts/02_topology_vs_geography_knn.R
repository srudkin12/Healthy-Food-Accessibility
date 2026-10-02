#!/usr/bin/env Rscript
options(warn = 1); options(stringsAsFactors = FALSE)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Usage: 02_topology_vs_geography_knn.R <package_root>", call. = FALSE)

package_root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
source(file.path(package_root, "R", "SpatialClosureHelpers.R"))
out <- file.path(package_root, "results")
p14d <- file.path(package_root, "accepted_evidence", "p1_4_d")

readout <- utils::read.csv(file.path(p14d, "food_hfa_readout_frame.csv"), stringsAsFactors = FALSE, check.names = FALSE)
points <- utils::read.csv(file.path(out, "canonical_spatial_representative_points.csv"), stringsAsFactors = FALSE)
if (!identical(readout$unit_id, points$unit_id)) stop("Point/readout order mismatch.", call. = FALSE)

axis_vars <- c(
  "physical_friction_log_nearest_large_store_km",
  "transport_constraint_no_car_pct",
  "material_constraint_income_deprivation_2025"
)
z <- scale(as.matrix(readout[,axis_vars,drop=FALSE]), center = TRUE, scale = TRUE)
geo_xy <- as.matrix(points[,c("easting","northing")])

maxk <- max(SC_K)
top_raw <- RANN::nn2(z, query = z, k = maxk + 1L)
geo_raw <- RANN::nn2(geo_xy, query = geo_xy, k = maxk + 1L)

top <- sc_drop_self_knn(top_raw$nn.idx, top_raw$nn.dists, maxk)
geo <- sc_drop_self_knn(geo_raw$nn.idx, geo_raw$nn.dists, maxk)

# Geographic distance of topology neighbours.
geo_dist_of_top <- matrix(NA_real_, nrow = SC_N, ncol = maxk)
top_dist_of_geo <- matrix(NA_real_, nrow = SC_N, ncol = maxk)
for (j in seq_len(maxk)) {
  tj <- top$index[,j]
  gj <- geo$index[,j]
  dx <- geo_xy[,1] - geo_xy[tj,1]
  dy <- geo_xy[,2] - geo_xy[tj,2]
  geo_dist_of_top[,j] <- sqrt(dx^2 + dy^2) / 1000

  dz <- z - z[gj,,drop=FALSE]
  top_dist_of_geo[,j] <- sqrt(rowSums(dz^2))
}

point_rows <- list()
summary_rows <- list()
rr <- 0L
ss <- 0L

for (k in SC_K) {
  inter <- integer(SC_N)
  for (i in seq_len(SC_N)) {
    inter[[i]] <- sum(top$index[i,seq_len(k)] %in% geo$index[i,seq_len(k)])
  }
  jac <- inter / (2*k - inter)
  gdt <- geo_dist_of_top[,seq_len(k),drop=FALSE]

  rr <- rr + 1L
  point_rows[[rr]] <- data.frame(
    point_index = readout$point_index,
    unit_id = readout$unit_id,
    unit_label = readout$unit_label,
    k = k,
    shared_neighbours = inter,
    jaccard_topological_vs_geographic = jac,
    median_geo_distance_of_topological_neighbours_km = apply(gdt, 1, stats::median),
    share_topological_neighbours_gt25km = rowMeans(gdt > 25),
    share_topological_neighbours_gt50km = rowMeans(gdt > 50),
    share_topological_neighbours_gt100km = rowMeans(gdt > 100),
    stringsAsFactors = FALSE
  )

  ss <- ss + 1L
  summary_rows[[ss]] <- data.frame(
    k = k,
    mean_jaccard = mean(jac),
    median_jaccard = stats::median(jac),
    q10_jaccard = sc_quantile(jac, .10),
    q90_jaccard = sc_quantile(jac, .90),
    zero_overlap_share = mean(inter == 0L),
    mean_shared_neighbours = mean(inter),
    median_geo_distance_of_topological_neighbours_km = stats::median(as.vector(gdt)),
    share_topological_neighbour_links_gt25km = mean(gdt > 25),
    share_topological_neighbour_links_gt50km = mean(gdt > 50),
    share_topological_neighbour_links_gt100km = mean(gdt > 100),
    stringsAsFactors = FALSE
  )
}

point_summary <- do.call(rbind, point_rows)
knn_summary <- do.call(rbind, summary_rows)
sc_write_gz_csv(point_summary, file.path(out, "topology_geography_knn_point_summary.csv.gz"))
utils::write.csv(knn_summary, file.path(out, "topology_geography_knn_summary.csv"), row.names = FALSE)

# Long-distance topological-neighbour examples at k=25.
k <- 25L
rows <- vector("list", SC_N * k)
q <- 0L
for (j in seq_len(k)) {
  idx <- top$index[,j]
  for (i in seq_len(SC_N)) {
    q <- q + 1L
    rows[[q]] <- data.frame(
      unit_id = readout$unit_id[[i]],
      unit_label = readout$unit_label[[i]],
      neighbour_unit_id = readout$unit_id[[idx[[i]]]],
      neighbour_unit_label = readout$unit_label[[idx[[i]]]],
      topological_rank = j,
      topological_distance_z = top$distance[i,j],
      geographic_distance_km = geo_dist_of_top[i,j],
      stringsAsFactors = FALSE
    )
  }
}
links <- do.call(rbind, rows)
links <- links[order(-links$geographic_distance_km, links$topological_distance_z), , drop = FALSE]
links <- links[!duplicated(t(apply(links[,c("unit_id","neighbour_unit_id")],1,sort))), , drop=FALSE]
utils::write.csv(head(links, 500L), file.path(out, "long_distance_topological_neighbour_examples.csv"), row.names = FALSE)

# Plots.
sc_plot_png_pdf(
  file.path(package_root, "figures", "knn_jaccard_by_k.png"),
  file.path(package_root, "figures", "knn_jaccard_by_k.pdf"),
  function() {
    graphics::boxplot(
      jaccard_topological_vs_geographic ~ factor(k),
      data = point_summary,
      xlab = "k",
      ylab = "Jaccard overlap"
    )
  }
)

cat("TOPOLOGY_GEOGRAPHY_KNN_OK k=", paste(SC_K, collapse=";"),
    " median_jaccard_k25=", knn_summary$median_jaccard[knn_summary$k==25L], "\n", sep="")

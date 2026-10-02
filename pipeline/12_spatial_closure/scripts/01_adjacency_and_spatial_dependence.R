#!/usr/bin/env Rscript
options(warn = 1); options(stringsAsFactors = FALSE)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Usage: 01_adjacency_and_spatial_dependence.R <package_root>", call. = FALSE)

package_root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
source(file.path(package_root, "R", "SpatialClosureHelpers.R"))
out <- file.path(package_root, "results")
work <- file.path(package_root, "work")
p14d <- file.path(package_root, "accepted_evidence", "p1_4_d")

geo <- readRDS(file.path(work, "canonical_lsoa21_geometry_bng.rds"))
readout <- utils::read.csv(file.path(p14d, "food_hfa_readout_frame.csv"), stringsAsFactors = FALSE, check.names = FALSE)
memberships <- utils::read.csv(file.path(p14d, "canonical_memberships.csv"), stringsAsFactors = FALSE)

cat("BUILDING_QUEEN_ADJACENCY\n")
neigh <- sf::st_touches(geo, sparse = TRUE)
deg <- lengths(neigh)
from <- rep(seq_len(SC_N), deg)
to <- unlist(neigh, use.names = FALSE)
keep <- from < to
edges <- data.frame(from = from[keep], to = to[keep])
edges$from_unit_id <- readout$unit_id[edges$from]
edges$to_unit_id <- readout$unit_id[edges$to]
sc_write_gz_csv(edges, file.path(out, "queen_adjacency_edges.csv.gz"))

adj_summary <- data.frame(
  n_observations = SC_N,
  n_queen_edges = nrow(edges),
  n_isolates = sum(deg == 0L),
  min_degree = min(deg),
  median_degree = stats::median(deg),
  mean_degree = mean(deg),
  max_degree = max(deg),
  stringsAsFactors = FALSE
)
utils::write.csv(adj_summary, file.path(out, "queen_adjacency_summary.csv"), row.names = FALSE)

# Global graph for induced-ball component analysis downstream.
g <- igraph::graph_from_data_frame(
  data.frame(from = as.character(edges$from), to = as.character(edges$to)),
  directed = FALSE,
  vertices = data.frame(name = as.character(seq_len(SC_N)))
)
saveRDS(g, file.path(work, "queen_adjacency_graph.rds"), compress = "gzip")
saveRDS(neigh, file.path(work, "queen_neighbour_list.rds"), compress = "gzip")

# Descriptive Moran's I for core continuous/binary readouts.
vars <- c(
  "physical_friction_log_nearest_large_store_km",
  "transport_constraint_no_car_pct",
  "material_constraint_income_deprivation_2025",
  "tcm_shopping_overall",
  "internal_qualifying_store_presence"
)
moran_rows <- lapply(vars, function(v) {
  data.frame(
    variable = v,
    variable_role = if (v %in% vars[1:3]) "topology_axis" else "post_freeze_readout",
    prevalence_or_mean = mean(as.numeric(readout[[v]])),
    moran_i_row_standardised_queen = sc_rowstd_moran(readout[[v]], neigh),
    expected_i_under_random_labelling = -1 / (SC_N - 1),
    inference = "DESCRIPTIVE_ONLY",
    stringsAsFactors = FALSE
  )
})
moran <- do.call(rbind, moran_rows)

# Binary ball-membership Moran's I.
memb_mat <- sc_membership_matrix(memberships, readout$unit_id)
ball_moran <- lapply(seq_len(ncol(memb_mat)), function(b) {
  y <- as.integer(memb_mat[,b])
  data.frame(
    variable = paste0("ball_", b, "_membership"),
    variable_role = "canonical_ball_membership_binary",
    prevalence_or_mean = mean(y),
    moran_i_row_standardised_queen = sc_rowstd_moran(y, neigh),
    expected_i_under_random_labelling = -1 / (SC_N - 1),
    inference = "DESCRIPTIVE_ONLY",
    stringsAsFactors = FALSE
  )
})
moran <- rbind(moran, do.call(rbind, ball_moran))
utils::write.csv(moran, file.path(out, "spatial_autocorrelation_descriptive.csv"), row.names = FALSE)

cat("SPATIAL_ADJACENCY_OK queen_edges=", nrow(edges),
    " isolates=", sum(deg == 0L), "\n", sep = "")

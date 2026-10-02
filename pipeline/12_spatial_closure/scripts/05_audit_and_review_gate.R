#!/usr/bin/env Rscript
options(warn = 1); options(stringsAsFactors = FALSE)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Usage: 05_audit_and_review_gate.R <package_root>", call. = FALSE)

package_root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
source(file.path(package_root, "R", "SpatialClosureHelpers.R"))
out <- file.path(package_root, "results")
food24 <- file.path(package_root, "accepted_evidence", "food24")
p14d <- file.path(package_root, "accepted_evidence", "p1_4_d")

topo <- utils::read.csv(file.path(p14d, "canonical_topology_summary.csv"), stringsAsFactors = FALSE)
knn <- utils::read.csv(file.path(out, "topology_geography_knn_summary.csv"), stringsAsFactors = FALSE)
ball <- utils::read.csv(file.path(out, "24_ball_spatial_closure.csv"), stringsAsFactors = FALSE)
pair <- utils::read.csv(file.path(out, "configuration_geography_pair_summary.csv"), stringsAsFactors = FALSE)
contrast <- utils::read.csv(file.path(out, "non_scalar_contrast_spatial_context.csv"), stringsAsFactors = FALSE)
adj <- utils::read.csv(file.path(out, "queen_adjacency_summary.csv"), stringsAsFactors = FALSE)
geom <- utils::read.csv(file.path(out, "spatial_geometry_manifest.csv"), stringsAsFactors = FALSE)

required <- c(
  "spatial_geometry_manifest.csv",
  "canonical_spatial_representative_points.csv",
  "queen_adjacency_edges.csv.gz",
  "queen_adjacency_summary.csv",
  "spatial_autocorrelation_descriptive.csv",
  "topology_geography_knn_point_summary.csv.gz",
  "topology_geography_knn_summary.csv",
  "long_distance_topological_neighbour_examples.csv",
  "24_ball_spatial_closure.csv",
  "configuration_geography_pair_sample.csv.gz",
  "configuration_geography_distance_deciles.csv",
  "configuration_geography_pair_summary.csv",
  "non_scalar_contrast_spatial_context.csv"
)

checks <- list()
add <- function(check, pass, detail, blocking = TRUE) {
  checks[[length(checks)+1L]] <<- data.frame(
    check = check,
    pass = isTRUE(pass),
    detail = as.character(detail),
    blocking = isTRUE(blocking),
    stringsAsFactors = FALSE
  )
}

add("canonical_fingerprint", topo$topology_fingerprint_sha256[[1L]] == SC_FINGERPRINT, topo$topology_fingerprint_sha256[[1L]])
add("canonical_radius", abs(topo$radius[[1L]] - SC_RADIUS) <= 1e-10, topo$radius[[1L]])
add("population", topo$n_observations[[1L]] == SC_N, topo$n_observations[[1L]])
add("required_outputs", all(file.exists(file.path(out, required))), paste(required[!file.exists(file.path(out, required))], collapse=";"))
add("knn_k_values", setequal(knn$k, SC_K), paste(knn$k, collapse=";"))
add("ball_count", nrow(ball) == 24L && setequal(ball$ball_id, 1:24), nrow(ball))
add("pair_sample", pair$pair_sample_n[[1L]] == SC_PAIR_SAMPLE_N, pair$pair_sample_n[[1L]])
add("contrast_rows", nrow(contrast) > 0L, nrow(contrast))
add("queen_adjacency", adj$n_queen_edges[[1L]] > 0L, adj$n_queen_edges[[1L]])

validation <- do.call(rbind, checks)
utils::write.csv(validation, file.path(out, "SPATIAL_CLOSURE_validation_report.csv"), row.names = FALSE)
if (any(validation$blocking & !validation$pass)) stop("Spatial closure validation contains blocking failures.", call. = FALSE)

k25 <- knn[knn$k == 25L, , drop = FALSE]
review <- data.frame(
  evidence_id = c(
    "S01","S02","S03","S04","S05","S06","S07"
  ),
  question = c(
    "How much do topological and geographic k=25 neighbour sets overlap?",
    "How often are topological-neighbour links geographically long-range?",
    "How strongly are pairwise configuration and geographic distance associated?",
    "Do canonical balls occupy one contiguous spatial region or multiple spatial components?",
    "How spatially autocorrelated are canonical ball memberships?",
    "What is the geographic context of accepted non-scalar contrast pairs?",
    "Does this stage modify the frozen Ball Mapper topology?"
  ),
  evidence_value = c(
    paste0("median Jaccard=", signif(k25$median_jaccard[[1L]],4),
           "; mean Jaccard=", signif(k25$mean_jaccard[[1L]],4)),
    paste0("share >50km=", signif(k25$share_topological_neighbour_links_gt50km[[1L]],4),
           "; share >100km=", signif(k25$share_topological_neighbour_links_gt100km[[1L]],4)),
    paste0("Spearman=", signif(pair$spearman_config_geo[[1L]],4),
           "; Pearson=", signif(pair$pearson_config_geo[[1L]],4)),
    paste0(sum(ball$n_spatial_components > 1L), " of 24 balls have >1 Queen-connected spatial component"),
    paste0("median membership Moran I=", signif(stats::median(ball$moran_i_membership),4),
           "; range=", signif(min(ball$moran_i_membership),4), " to ", signif(max(ball$moran_i_membership),4)),
    paste0(nrow(contrast), " accepted screened contrast pairs receive centroid, adjacency and exclusive-nearest-distance evidence"),
    "FALSE"
  ),
  status = c(
    rep("READY_FOR_HUMAN_INTERPRETATION", 6),
    "VERIFIED_IMMUTABLE"
  ),
  interpretation_limit = c(
    "Low overlap would support non-equivalence; high overlap would indicate stronger spatial alignment. Human interpretation required.",
    "Distance thresholds are descriptive and not universal definitions of spatial separation.",
    "Correlation is descriptive and does not identify a causal spatial process.",
    "Multiple components can arise from boundary/island structure and must be read with dispersion evidence.",
    "Moran's I is descriptive only; no p-value or causal claim is made.",
    "Geographic separation does not invalidate the configurational contrast; it addresses an alternative explanation.",
    "All spatial computations consume accepted memberships/readouts and never reconstruct Ball Mapper."
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(review, file.path(out, "spatial_claim_evidence_register.csv"), row.names = FALSE)

gate <- data.frame(
  extension = c(
    "spatial_closure",
    "spatial_regression",
    "spatial_hac",
    "additional_spatial_randomisation",
    "additional_clustering",
    "pca_comparator",
    "scaling_robustness",
    "final_analytical_evidence_freeze"
  ),
  gate_status = c(
    "COMPLETE_PENDING_HUMAN_INTERPRETATION",
    "NOT_APPLICABLE_NO_OUTCOME_ESTIMAND",
    "NOT_APPLICABLE_NO_REGRESSION_ESTIMAND",
    "NOT_AUTOMATICALLY_TRIGGERED",
    "NOT_AUTOMATICALLY_TRIGGERED_EXISTING_KMEANS_BENCHMARK",
    "NOT_AUTOMATICALLY_TRIGGERED",
    "NOT_TESTED",
    "READY_FOR_HUMAN_REVIEW"
  ),
  rationale = c(
    "Adjacency, distance, spatial autocorrelation, kNN overlap, ball spatial components and contrast geography completed.",
    "The current paper claim is descriptive/configurational rather than an outcome model requiring spatial regression.",
    "No regression coefficient or standard-error estimand is currently being claimed.",
    "Activate only if the spatial closure leaves a specific null claim unresolved.",
    "Existing F4 k=3 benchmark already supplies hard-clustering comparison.",
    "Activate only if a linear-latent-structure claim becomes necessary.",
    "Activate only if evidence suggests interpretation may depend materially on z-score scaling or a referee challenges it.",
    "Human review should determine whether spatial evidence closes the final material alternative explanation."
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(gate, file.path(out, "post_spatial_extension_gate.csv"), row.names = FALSE)

# Machine-readable manifest.
files <- list.files(out, full.names = TRUE, recursive = TRUE)
files <- files[file.info(files)$isdir == FALSE]
manifest <- data.frame(
  artifact = basename(files),
  bytes = file.info(files)$size,
  sha256 = vapply(files, sc_sha256, character(1)),
  stage = "targeted_spatial_closure",
  topology_fingerprint_sha256 = SC_FINGERPRINT,
  stringsAsFactors = FALSE
)
utils::write.csv(manifest, file.path(out, "SPATIAL_CLOSURE_artifact_manifest.csv"), row.names = FALSE)

# Human-review markdown.
geom_val <- setNames(geom$value, geom$field)
lines <- c(
  "# Targeted spatial closure review",
  "",
  "## Frozen analytical identity",
  "",
  paste0("- Radius: ", SC_RADIUS),
  paste0("- LSOAs: ", SC_N),
  paste0("- Canonical topology fingerprint: `", SC_FINGERPRINT, "`"),
  "- Canonical topology modified: **FALSE**",
  "",
  "## Geography",
  "",
  paste0("- Source: `", geom_val[["source_path"]], "`"),
  paste0("- Layer: `", geom_val[["source_layer"]], "`"),
  paste0("- Working CRS: ", geom_val[["working_crs"]]),
  paste0("- Geometry fingerprint: `", geom_val[["geometry_fingerprint_sha256"]], "`"),
  paste0("- Queen adjacency edges: ", adj$n_queen_edges[[1L]]),
  paste0("- Queen isolates: ", adj$n_isolates[[1L]]),
  "",
  "## Topological versus geographic neighbours",
  "",
  paste0("- k=25 median Jaccard: ", signif(k25$median_jaccard[[1L]],4)),
  paste0("- k=25 mean Jaccard: ", signif(k25$mean_jaccard[[1L]],4)),
  paste0("- k=25 zero-overlap share: ", signif(k25$zero_overlap_share[[1L]],4)),
  paste0("- Topological-neighbour links >50 km: ", signif(k25$share_topological_neighbour_links_gt50km[[1L]],4)),
  paste0("- Topological-neighbour links >100 km: ", signif(k25$share_topological_neighbour_links_gt100km[[1L]],4)),
  "",
  "## Pair-distance association",
  "",
  paste0("- Spearman(configuration distance, geographic distance): ", signif(pair$spearman_config_geo[[1L]],4)),
  paste0("- Pearson(configuration distance, geographic distance): ", signif(pair$pearson_config_geo[[1L]],4)),
  paste0("- Median geographic distance among lowest configuration-distance decile: ",
         signif(pair$median_geo_distance_lowest_config_decile_km[[1L]],4), " km"),
  paste0("- Share of lowest configuration-distance-decile pairs >50 km: ",
         signif(pair$share_lowest_config_decile_pairs_gt50km[[1L]],4)),
  paste0("- Share of lowest configuration-distance-decile pairs >100 km: ",
         signif(pair$share_lowest_config_decile_pairs_gt100km[[1L]],4)),
  "",
  "## Ball spatial structure",
  "",
  paste0("- Balls with more than one Queen-connected spatial component: ", sum(ball$n_spatial_components > 1L), " / 24"),
  paste0("- Median ball-membership Moran's I: ", signif(stats::median(ball$moran_i_membership),4)),
  paste0("- Ball-membership Moran's I range: ", signif(min(ball$moran_i_membership),4),
         " to ", signif(max(ball$moran_i_membership),4)),
  "",
  "## Interpretation boundary",
  "",
  "These results test whether accepted configurational structure is equivalent to or wholly explained by spatial proximity. They are descriptive. No spatial-regression, causal, or significance claim is made.",
  "",
  "## Decision gate",
  "",
  "**READY_FOR_FINAL_ANALYTICAL_FREEZE_REVIEW = TRUE**",
  "",
  "The analytical evidence is not automatically frozen by this script. Human review should decide whether any specific residual spatial or scaling question remains."
)
writeLines(lines, file.path(out, "SPATIAL_REVIEW.md"))

status <- c(
  "TARGETED_SPATIAL_CLOSURE_STATUS=PASS",
  "CANONICAL_TOPOLOGY_MODIFIED=FALSE",
  paste0("APPROVED_RADIUS=", format(SC_RADIUS, nsmall=2)),
  paste0("TOPOLOGY_FINGERPRINT_SHA256=", SC_FINGERPRINT),
  "GEOGRAPHY_VINTAGE=LSOA21",
  "QUEEN_ADJACENCY_COMPLETE=TRUE",
  "REPRESENTATIVE_POINT_DISTANCE_COMPLETE=TRUE",
  "SPATIAL_AUTOCORRELATION_DESCRIPTIVE_COMPLETE=TRUE",
  "TOPOLOGY_GEOGRAPHY_KNN_COMPLETE=TRUE",
  "PAIR_DISTANCE_ASSOCIATION_COMPLETE=TRUE",
  "BALL_SPATIAL_COMPONENTS_COMPLETE=TRUE",
  "NON_SCALAR_CONTRAST_SPATIAL_CONTEXT_COMPLETE=TRUE",
  "SPATIAL_REGRESSION=NOT_ACTIVATED_NO_OUTCOME_ESTIMAND",
  "SPATIAL_HAC=NOT_ACTIVATED_NO_REGRESSION_ESTIMAND",
  "READY_FOR_FINAL_ANALYTICAL_FREEZE_REVIEW=TRUE",
  "FINAL_ANALYTICAL_FREEZE=NOT_YET_DECLARED"
)
writeLines(status, file.path(out, "SPATIAL_CLOSURE_STATUS.txt"))

cat("TARGETED_SPATIAL_CLOSURE_PASS\n")
cat("READY_FOR_FINAL_ANALYTICAL_FREEZE_REVIEW=TRUE\n")

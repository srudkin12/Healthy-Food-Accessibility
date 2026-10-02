#!/usr/bin/env Rscript
options(warn = 1); options(stringsAsFactors = FALSE)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: 00_preflight_geometry.R <package_root> <project_root>", call. = FALSE)

package_root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
project_root <- normalizePath(args[[2L]], winslash = "/", mustWork = TRUE)
source(file.path(package_root, "R", "SpatialClosureHelpers.R"))

out <- file.path(package_root, "results")
work <- file.path(package_root, "work")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
dir.create(work, recursive = TRUE, showWarnings = FALSE)

p14d <- file.path(package_root, "accepted_evidence", "p1_4_d")
food24 <- file.path(package_root, "accepted_evidence", "food24")

p14d_status <- readLines(file.path(p14d, "P1_4_D_STATUS.txt"), warn = FALSE)
food24_status <- readLines(file.path(food24, "EVIDENCE_CLOSURE_STATUS.txt"), warn = FALSE)

if (!any(grepl("^P1_4_D_STATUS=PASS$", p14d_status))) stop("Accepted P1.4-D is not PASS.", call. = FALSE)
if (!any(grepl("^FOOD24_EVIDENCE_CLOSURE_STATUS=PASS$", food24_status))) stop("Food24 closure is not PASS.", call. = FALSE)

topo <- utils::read.csv(file.path(p14d, "canonical_topology_summary.csv"), stringsAsFactors = FALSE)
if (nrow(topo) != 1L ||
    topo$n_observations[[1L]] != SC_N ||
    abs(topo$radius[[1L]] - SC_RADIUS) > 1e-10 ||
    topo$n_balls[[1L]] != 24L ||
    topo$topology_fingerprint_sha256[[1L]] != SC_FINGERPRINT) {
  stop("Accepted canonical topology identity mismatch.", call. = FALSE)
}

readout <- utils::read.csv(
  file.path(p14d, "food_hfa_readout_frame.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
if (nrow(readout) != SC_N || anyDuplicated(readout$unit_id)) stop("Accepted readout frame identity failure.", call. = FALSE)

memberships <- utils::read.csv(file.path(p14d, "canonical_memberships.csv"), stringsAsFactors = FALSE)
if (length(unique(memberships$ball_id)) != 24L) stop("Membership table does not contain 24 balls.", call. = FALSE)
if (!setequal(unique(memberships$unit_id), readout$unit_id)) stop("Membership/readout population mismatch.", call. = FALSE)

inv_file <- file.path(out, "spatial_geometry_candidate_inventory.csv")
geo_res <- sc_resolve_geometry(project_root, readout$unit_id, inv_file)
geo <- geo_res$sf

original_crs <- sf::st_crs(geo)
if (is.na(original_crs)) stop("Selected LSOA21 geography has no CRS.", call. = FALSE)

valid_before <- sf::st_is_valid(geo)
invalid_n <- sum(!valid_before)
if (invalid_n > 0L) {
  geo <- sf::st_make_valid(geo)
}
if (any(!sf::st_is_valid(geo))) stop("Selected LSOA21 geography remains invalid after st_make_valid.", call. = FALSE)

geo <- sf::st_transform(geo, 27700)
id_col <- geo_res$id_col
geo <- geo[match(readout$unit_id, as.character(geo[[id_col]])), , drop = FALSE]
if (!identical(as.character(geo[[id_col]]), as.character(readout$unit_id))) {
  stop("Geometry cannot be aligned exactly to canonical readout order.", call. = FALSE)
}

# Deterministic point-on-surface representative points.
pts <- suppressWarnings(sf::st_point_on_surface(geo))
xy <- sf::st_coordinates(pts)
if (nrow(xy) != SC_N || any(!is.finite(xy[,1:2]))) stop("Representative-point construction failed.", call. = FALSE)

point_table <- data.frame(
  point_index = readout$point_index,
  unit_id = readout$unit_id,
  unit_label = readout$unit_label,
  easting = xy[,1],
  northing = xy[,2],
  stringsAsFactors = FALSE
)
utils::write.csv(point_table, file.path(out, "canonical_spatial_representative_points.csv"), row.names = FALSE)

# Geometry fingerprint from the exact canonical-order BNG geometry object.
tmp_rds <- file.path(work, "canonical_lsoa21_geometry_bng.rds")
saveRDS(geo, tmp_rds, compress = FALSE)
geometry_fingerprint <- sc_sha256(tmp_rds)
unlink(tmp_rds)

manifest <- data.frame(
  field = c(
    "source_path", "source_layer", "source_id_column", "selection_reason",
    "source_selection_score", "original_crs_wkt", "working_crs",
    "n_features", "geometry_types", "invalid_geometries_repaired",
    "representative_point_rule", "geometry_fingerprint_sha256"
  ),
  value = c(
    geo_res$source_path, geo_res$source_layer, id_col, geo_res$selection_reason,
    geo_res$source_score, original_crs$wkt, "EPSG:27700",
    nrow(geo), geo_res$geometry_types, invalid_n,
    "sf::st_point_on_surface after transformation to EPSG:27700",
    geometry_fingerprint
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(manifest, file.path(out, "spatial_geometry_manifest.csv"), row.names = FALSE)

# Save geometry only as a stage-local working object; handback uses fingerprint + source provenance.
saveRDS(geo, file.path(work, "canonical_lsoa21_geometry_bng.rds"), compress = "gzip")

audit <- data.frame(
  check = c(
    "p1_4_d_pass", "food24_pass", "canonical_radius", "canonical_fingerprint",
    "population", "geometry_exact_id_match", "polygon_geometry", "working_crs",
    "representative_points"
  ),
  pass = TRUE,
  detail = c(
    "PASS", "PASS", "1.50", SC_FINGERPRINT, SC_N,
    "33755/33755", geo_res$geometry_types, "EPSG:27700", SC_N
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(audit, file.path(out, "00_spatial_preflight_audit.csv"), row.names = FALSE)

cat("SPATIAL_PREFLIGHT_PASS observations=", SC_N,
    " geometry_fingerprint=", geometry_fingerprint, "\n", sep = "")
cat("SPATIAL_GEOMETRY_SOURCE=", geo_res$source_path,
    if (nzchar(geo_res$source_layer)) paste0("::", geo_res$source_layer) else "", "\n", sep = "")

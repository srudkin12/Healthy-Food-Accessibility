#!/usr/bin/env Rscript
options(warn = 1)
options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Usage: 01_food_p1_4c_freeze.R <package_root> <framework_root>", call. = FALSE)
}

package_root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
framework_root <- normalizePath(args[[2L]], winslash = "/", mustWork = TRUE)
app_root <- file.path(package_root, "food_application")
input_root <- file.path(app_root, "input")
result_root <- file.path(app_root, "results", "p1_4_c")
dir.create(result_root, recursive = TRUE, showWarnings = FALSE)

source(file.path(framework_root, "framework", "R", "TDABMUserRadiusContract.R"))
source(file.path(framework_root, "framework", "R", "TDABMTopologyFreeze.R"))
source(file.path(framework_root, "framework", "R", "TDABMFrozenTopology.R"))
source(file.path(framework_root, "framework", "R", "TDABMRadiusDiagnostics.R"))

sha256_file <- function(path) tdabm_tf_sha256(path)

read_expected_sha <- function(path) {
  x <- readLines(path, warn = FALSE)
  if (!length(x)) stop("Empty SHA file: ", path, call. = FALSE)
  tolower(strsplit(trimws(x[[1L]]), "[[:space:]]+")[[1L]][[1L]])
}

# -------------------------------------------------------------------------
# 1. Verify accepted upstream evidence.
# -------------------------------------------------------------------------
frozen_file <- file.path(input_root, "food_hfa_analysis_input.csv")
expected_frozen_sha <- read_expected_sha(file.path(package_root, "FROZEN_INPUT_SHA256.txt"))
observed_frozen_sha <- sha256_file(frozen_file)
if (!identical(expected_frozen_sha, observed_frozen_sha)) {
  stop("Frozen P1.4-A input SHA-256 mismatch.", call. = FALSE)
}

p1a_status <- readLines(file.path(input_root, "P1_4_A_STATUS.txt"), warn = FALSE)
if (!any(grepl("^P1_4_A_STATUS=PASS$", p1a_status))) {
  stop("P1.4-A is not recorded as PASS.", call. = FALSE)
}

p1b_status <- readLines(file.path(input_root, "P1_4_B_STATUS.txt"), warn = FALSE)
if (!any(grepl("^P1_4_B_STATUS=PASS$", p1b_status))) {
  stop("P1.4-B is not recorded as PASS.", call. = FALSE)
}

expected_audit_sha <- read_expected_sha(file.path(package_root, "P1_4_B_DIAGNOSTIC_AUDIT_SHA256.txt"))
observed_audit_sha <- sha256_file(file.path(input_root, "P1_4_B_validation_report.csv"))
if (!identical(expected_audit_sha, observed_audit_sha)) {
  stop("P1.4-B diagnostic audit SHA-256 mismatch.", call. = FALSE)
}

audit <- utils::read.csv(
  file.path(input_root, "P1_4_B_validation_report.csv"),
  stringsAsFactors = FALSE
)
if (any(audit$blocking & !audit$pass)) {
  stop("P1.4-B diagnostic audit contains a blocking failure.", call. = FALSE)
}

# -------------------------------------------------------------------------
# 2. Construct and validate the explicit user-radius contract.
# -------------------------------------------------------------------------
decision <- utils::read.csv(
  file.path(app_root, "config", "approved_radius_decision_input.csv"),
  stringsAsFactors = FALSE
)
decision_value <- function(field) {
  hit <- decision$value[decision$field == field]
  if (length(hit) != 1L) stop("Decision field missing/nonunique: ", field, call. = FALSE)
  hit[[1L]]
}

axes <- strsplit(decision_value("topology_axes"), ";", fixed = TRUE)[[1L]]

contract <- tdabm_urc_contract(
  application = decision_value("application"),
  target_radius = as.numeric(decision_value("approved_radius")),
  selection_basis = decision_value("selection_basis"),
  interpretation = decision_value("interpretation"),
  decision_rationale = decision_value("decision_rationale"),
  topology_axes = axes,
  scaling = decision_value("scaling"),
  metric = decision_value("metric"),
  precision_policy = decision_value("precision_policy"),
  pipeline_reference_radius = as.numeric(decision_value("pipeline_reference_radius")),
  pipeline_reference_method = decision_value("pipeline_reference_method"),
  review_band_lower = as.numeric(decision_value("review_band_lower")),
  review_band_upper = as.numeric(decision_value("review_band_upper")),
  diagnostic_audit_sha256 = decision_value("diagnostic_audit_sha256"),
  approved_by = decision_value("decision_maker"),
  approved_at = decision_value("decision_date"),
  automatic_selection_performed = FALSE,
  optimality_claim = "NONE"
)

contract_validation <- tdabm_urc_validate(contract, stop_on_error = TRUE)
tdabm_tf_write_csv_atomic(
  tdabm_urc_table(contract),
  file.path(result_root, "approved_radius_contract.csv")
)
tdabm_tf_write_csv_atomic(
  contract_validation,
  file.path(result_root, "approved_radius_contract_validation.csv")
)
tdabm_tf_save_rds_atomic(
  contract,
  file.path(result_root, "approved_radius_contract.rds")
)

# -------------------------------------------------------------------------
# 3. Prepare the canonical point order and z-score geometry.
# -------------------------------------------------------------------------
dat <- utils::read.csv(
  frozen_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

id_col <- "lsoa21cd"
label_col <- if ("lsoa21nm" %in% names(dat)) "lsoa21nm" else NULL

canonical <- tdabm_tf_canonicalize(
  data = dat,
  id_col = id_col,
  topology_axes = axes
)

# Application-level row-order invariant check without reconstructing BM.
set.seed(20260920L)
perm <- sample.int(nrow(dat))
canonical_reordered <- tdabm_tf_canonicalize(
  data = dat[perm, , drop = FALSE],
  id_col = id_col,
  topology_axes = axes
)
if (!identical(canonical$ids, canonical_reordered$ids) ||
    !identical(as.matrix(canonical$axes), as.matrix(canonical_reordered$axes))) {
  stop("Canonical point preparation is not row-order invariant.", call. = FALSE)
}

centres <- vapply(canonical$axes, mean, numeric(1))
scales <- vapply(canonical$axes, stats::sd, numeric(1))
if (any(!is.finite(scales)) || any(scales <= 0)) {
  stop("Invalid z-score scaling parameters.", call. = FALSE)
}

scaled_axes <- canonical$axes
for (nm in axes) {
  scaled_axes[[nm]] <- as.numeric((scaled_axes[[nm]] - centres[[nm]]) / scales[[nm]])
}
if (any(!is.finite(as.matrix(scaled_axes)))) {
  stop("Scaled canonical topology contains non-finite values.", call. = FALSE)
}

labels <- if (!is.null(label_col)) {
  enc2utf8(as.character(canonical$data[[label_col]]))
} else {
  canonical$ids
}

point_order <- data.frame(
  point_index = seq_len(nrow(canonical$data)),
  source_row_index = canonical$source_row_index,
  unit_id = canonical$ids,
  unit_label = labels,
  stringsAsFactors = FALSE
)
for (nm in axes) {
  point_order[[paste0(nm, "_unscaled")]] <- as.numeric(canonical$axes[[nm]])
  point_order[[paste0(nm, "_z")]] <- as.numeric(scaled_axes[[nm]])
}
tdabm_tf_write_csv_atomic(
  point_order,
  file.path(result_root, "canonical_point_order.csv")
)

scaling <- data.frame(
  axis_order = seq_along(axes),
  axis = axes,
  centre = as.numeric(centres[axes]),
  scale_sd = as.numeric(scales[axes]),
  scaling = "z_score",
  metric = "euclidean",
  stringsAsFactors = FALSE
)
tdabm_tf_write_csv_atomic(
  scaling,
  file.path(result_root, "canonical_scaling.csv")
)

# -------------------------------------------------------------------------
# 4. Construct exactly one canonical Ball Mapper topology at approved radius.
# -------------------------------------------------------------------------
tdabm_tf_compile_cpp(file.path(framework_root, "framework", "BallMapper.cpp"))

approved_radius <- as.numeric(contract$target_radius)
bm <- tdabm_tf_build(
  axes = scaled_axes,
  radius = approved_radius
)

topology_validation <- tdabm_tf_validate_object(
  bm = bm,
  n_points = nrow(scaled_axes),
  expected_radius = approved_radius
)
if (!all(topology_validation$pass)) {
  bad <- topology_validation[!topology_validation$pass, , drop = FALSE]
  stop(
    paste(
      c(
        "Canonical topology validation failed:",
        paste0("- ", bad$check, ": ", bad$detail)
      ),
      collapse = "\n"
    ),
    call. = FALSE
  )
}

# Verify arbitrary recolouring is read-only with respect to topology.
probe1 <- tdabm_ft_recolour(bm, seq_len(nrow(scaled_axes)))
probe2 <- tdabm_ft_recolour(bm, rev(seq_len(nrow(scaled_axes))))
rv1 <- tdabm_ft_validate_recoloured(bm, probe1$object)
rv1$detail <- paste0("ascending_index probe; framework check=", rv1$check)
rv2 <- tdabm_ft_validate_recoloured(bm, probe2$object)
rv2$detail <- paste0("descending_index probe; framework check=", rv2$check)
recolour_validation <- rbind(
  cbind(probe = "ascending_index", rv1),
  cbind(probe = "descending_index", rv2)
)
if (!all(recolour_validation$pass)) {
  stop("Frozen-topology recolouring invariant failed.", call. = FALSE)
}

# -------------------------------------------------------------------------
# 5. Export canonical topology evidence.
# -------------------------------------------------------------------------
landmarks <- tdabm_tf_landmark_table(bm, canonical$ids, labels)
memberships <- tdabm_tf_membership_table(bm, canonical$ids, labels)
edges <- tdabm_tf_edge_table(bm)
vertices <- tdabm_tf_vertex_table(bm)

n_balls <- nrow(landmarks)
edge_matrix <- if (nrow(edges)) {
  as.matrix(edges[, c("from", "to"), drop = FALSE])
} else {
  matrix(numeric(0), ncol = 2L)
}
graph <- tdabm_rd_graph_components(n_balls, edge_matrix)

membership_counts <- tabulate(
  memberships$point_index,
  nbins = nrow(scaled_axes)
)
if (any(membership_counts < 1L)) {
  stop("At least one LSOA is absent from canonical memberships.", call. = FALSE)
}

ball_sizes <- tabulate(memberships$ball_id, nbins = n_balls)
if (!identical(as.integer(ball_sizes), as.integer(vertices$ball_size))) {
  stop("Membership-derived and vertex-derived ball sizes disagree.", call. = FALSE)
}

tdabm_tf_write_csv_atomic(landmarks, file.path(result_root, "canonical_landmarks.csv"))
tdabm_tf_write_csv_atomic(memberships, file.path(result_root, "canonical_memberships.csv"))
tdabm_tf_write_csv_atomic(edges, file.path(result_root, "canonical_edges.csv"))
tdabm_tf_write_csv_atomic(vertices, file.path(result_root, "canonical_vertices.csv"))

component_table <- data.frame(
  component_id = seq_along(graph$component_sizes),
  component_size_balls = graph$component_sizes,
  stringsAsFactors = FALSE
)
tdabm_tf_write_csv_atomic(
  component_table,
  file.path(result_root, "canonical_components.csv")
)

summary <- data.frame(
  approved_radius = approved_radius,
  n_observations = nrow(scaled_axes),
  n_balls = n_balls,
  n_edges = nrow(edges),
  n_components = graph$n_components,
  n_nontrivial_components = graph$n_nontrivial_components,
  n_isolated_balls = graph$n_isolated_vertices,
  largest_component_balls = graph$largest_component_size,
  largest_component_share_balls = graph$largest_component_size / n_balls,
  min_ball_size = min(ball_sizes),
  median_ball_size = stats::median(ball_sizes),
  mean_ball_size = mean(ball_sizes),
  max_ball_size = max(ball_sizes),
  max_ball_share_observations = max(ball_sizes) / nrow(scaled_axes),
  mean_memberships_per_observation = mean(membership_counts),
  median_memberships_per_observation = stats::median(membership_counts),
  max_memberships_per_observation = max(membership_counts),
  fraction_multicovered = mean(membership_counts > 1L),
  landmark_index_contract = as.character(bm$tdabm_landmark_index_contract),
  stringsAsFactors = FALSE
)
tdabm_tf_write_csv_atomic(
  summary,
  file.path(result_root, "canonical_topology_summary.csv")
)

fingerprint <- tdabm_tf_topology_fingerprint(bm, canonical$ids)
tdabm_tf_write_lines_atomic(
  c(
    paste0("TOPOLOGY_FINGERPRINT_SHA256=", fingerprint),
    paste0("APPROVED_RADIUS=", format(approved_radius, nsmall = 2)),
    "POINT_ORDER=stable_radix_sort_by_lsoa21cd",
    "LANDMARK_INDEX_BASE=R_ONE_BASED_NORMALIZED_FROM_CPP_ZERO_BASED"
  ),
  file.path(result_root, "canonical_topology_fingerprint.txt")
)

canonical_object_file <- file.path(result_root, "canonical_ballmapper_object.rds")
tdabm_tf_save_rds_atomic(bm, canonical_object_file)

bundle <- list(
  application_id = contract$application,
  approved_radius_contract = contract,
  topology_variables = axes,
  scaling = scaling,
  metric = "euclidean",
  point_order = point_order,
  canonical_ballmapper = bm,
  topology_fingerprint_sha256 = fingerprint,
  frozen_input_sha256 = observed_frozen_sha,
  diagnostic_audit_sha256 = observed_audit_sha,
  framework_version = "TDABM Portable Framework 1.1.2 / P1.4-C v1.0.3"
)
bundle_file <- file.path(result_root, "canonical_topology_bundle.rds")
tdabm_tf_save_rds_atomic(bundle, bundle_file)

# -------------------------------------------------------------------------
# 6. Unified validation report and artifact manifest.
# -------------------------------------------------------------------------
identity_checks <- data.frame(
  check = c(
    "frozen_input_sha256",
    "p1_4_a_pass",
    "p1_4_b_pass",
    "diagnostic_audit_sha256",
    "canonical_point_order_unique",
    "canonical_point_order_sorted",
    "membership_point_coverage",
    "membership_vertex_ball_sizes",
    "topology_fingerprint_present",
    "approved_radius_exact",
    "single_canonical_topology_constructed"
  ),
  pass = c(
    identical(expected_frozen_sha, observed_frozen_sha),
    TRUE,
    TRUE,
    identical(expected_audit_sha, observed_audit_sha),
    !anyDuplicated(point_order$unit_id),
    identical(point_order$unit_id, sort(point_order$unit_id, method = "radix")),
    all(membership_counts >= 1L),
    identical(as.integer(ball_sizes), as.integer(vertices$ball_size)),
    nzchar(fingerprint) && nchar(fingerprint) == 64L,
    abs(as.numeric(bm$epsilon) - 1.50) <= 1e-8,
    TRUE
  ),
  detail = c(
    observed_frozen_sha,
    "P1.4-A PASS",
    "P1.4-B PASS",
    observed_audit_sha,
    paste0("unique IDs=", length(unique(point_order$unit_id))),
    "stable radix sort by lsoa21cd",
    paste0("min memberships=", min(membership_counts)),
    paste0("balls=", n_balls),
    fingerprint,
    format(as.numeric(bm$epsilon), digits = 17),
    "one tdabm_tf_build call in application stage"
  ),
  stringsAsFactors = FALSE
)

topology_validation$group <- "canonical_topology"
contract_validation$group <- "user_radius_contract"
identity_checks$group <- "application_integrity"
recolour_validation$group <- "recolour_invariant"

validation_report <- rbind(
  contract_validation[, c("check", "pass", "detail", "group")],
  topology_validation[, c("check", "pass", "detail", "group")],
  identity_checks[, c("check", "pass", "detail", "group")],
  recolour_validation[, c("check", "pass", "detail", "group")]
)

tdabm_tf_write_csv_atomic(
  validation_report,
  file.path(result_root, "P1_4_C_validation_report.csv")
)

if (!all(validation_report$pass)) {
  stop("P1.4-C unified validation report contains a failure.", call. = FALSE)
}

manifest_files <- c(
  "approved_radius_contract.csv",
  "approved_radius_contract_validation.csv",
  "approved_radius_contract.rds",
  "canonical_point_order.csv",
  "canonical_scaling.csv",
  "canonical_landmarks.csv",
  "canonical_memberships.csv",
  "canonical_edges.csv",
  "canonical_vertices.csv",
  "canonical_components.csv",
  "canonical_topology_summary.csv",
  "canonical_topology_fingerprint.txt",
  "canonical_ballmapper_object.rds",
  "canonical_topology_bundle.rds",
  "P1_4_C_validation_report.csv"
)

manifest <- do.call(
  rbind,
  lapply(manifest_files, function(nm) {
    p <- file.path(result_root, nm)
    data.frame(
      artifact = nm,
      bytes = file.info(p)$size,
      sha256 = sha256_file(p),
      stringsAsFactors = FALSE
    )
  })
)
tdabm_tf_write_csv_atomic(
  manifest,
  file.path(result_root, "P1_4_C_artifact_manifest.csv")
)

status_lines <- c(
  "P1_4_C_STATUS=PASS",
  "CANONICAL_TOPOLOGY_FROZEN=TRUE",
  "APPLICATION_ID=food_deserts_hfa_england_lsoa21",
  "APPROVED_RADIUS=1.50",
  "RADIUS_DECISION=EXPLICIT_USER_APPROVAL",
  "SELECTION_BASIS=substantively_interpretable_fixed",
  paste0("REVIEW_BAND_POSITION=", contract$review_band_position),
  paste0("PIPELINE_REFERENCE_RADIUS=", format(contract$pipeline_reference_radius, digits = 17)),
  "AUTOMATIC_SELECTION_PERFORMED=FALSE",
  "OPTIMALITY_CLAIM=NONE",
  paste0("N_OBSERVATIONS=", nrow(scaled_axes)),
  paste0("N_BALLS=", n_balls),
  paste0("N_EDGES=", nrow(edges)),
  paste0("N_COMPONENTS=", graph$n_components),
  paste0("TOPOLOGY_FINGERPRINT_SHA256=", fingerprint),
  paste0("CANONICAL_OBJECT_SHA256=", sha256_file(canonical_object_file)),
  paste0("CANONICAL_BUNDLE_SHA256=", sha256_file(bundle_file)),
  paste0("FROZEN_INPUT_SHA256=", observed_frozen_sha),
  paste0("DIAGNOSTIC_AUDIT_SHA256=", observed_audit_sha),
  "P1_4_D_AUTHORISED=TRUE"
)
tdabm_tf_write_lines_atomic(
  status_lines,
  file.path(result_root, "P1_4_C_STATUS.txt")
)

cat("FOOD_HFA_RADIUS_DECISION_RECORDED radius=1.50 position=", contract$review_band_position, "\n", sep = "")
cat("FOOD_HFA_CANONICAL_TOPOLOGY_BUILT balls=", n_balls, " edges=", nrow(edges), " components=", graph$n_components, "\n", sep = "")
cat("FOOD_HFA_TOPOLOGY_FINGERPRINT=", fingerprint, "\n", sep = "")
cat("FOOD_HFA_P1_4_C_PASS\n")

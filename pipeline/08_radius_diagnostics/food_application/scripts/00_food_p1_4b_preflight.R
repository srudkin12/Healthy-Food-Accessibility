#!/usr/bin/env Rscript
options(warn = 1)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Usage: 00_food_p1_4b_preflight.R <package_root> <framework_root>", call. = FALSE)
}

package_root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
framework_root <- normalizePath(args[[2L]], winslash = "/", mustWork = TRUE)
app_root <- file.path(package_root, "food_application")

# ZIP archives do not reliably preserve empty directories.
# Create every preflight output directory explicitly before first write.
dir.create(file.path(app_root, "results"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(app_root, "input"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(app_root, "config"), recursive = TRUE, showWarnings = FALSE)

source(file.path(framework_root, "framework/R/TDABMValueContracts.R"))
source(file.path(framework_root, "framework/R/TDABMRadiusDiagnostics.R"))
source(file.path(app_root, "R/FoodP14BAdapter.R"))

frozen_file <- file.path(app_root, "input/food_hfa_analysis_input.csv")
expected_sha <- strsplit(
  readLines(file.path(package_root, "FROZEN_INPUT_SHA256.txt"), warn = FALSE)[[1L]],
  "[[:space:]]+"
)[[1L]][[1L]]

observed_sha <- unname(tools::md5sum(frozen_file))
# SHA256 is checked by the shell before this script. Keep MD5 only as a local runtime fingerprint.

dat <- utils::read.csv(frozen_file, stringsAsFactors = FALSE, check.names = FALSE)
topology_variables <- c(
  "physical_friction_log_nearest_large_store_km",
  "transport_constraint_no_car_pct",
  "material_constraint_income_deprivation_2025"
)

prep <- tdabm_rd_prepare_topology(
  data = dat,
  id_col = "lsoa21cd",
  topology_variables = topology_variables,
  scaling = "z_score",
  metric = "euclidean"
)

grid <- utils::read.csv(file.path(app_root, "config/radius_grid.csv"))
if (nrow(grid) != 915L) stop("Radius grid must contain 915 points.", call. = FALSE)
if (abs(grid$radius[[1L]] - 0.01) > 1e-12) stop("Radius grid minimum mismatch.", call. = FALSE)
if (abs(grid$radius[[nrow(grid)]] - 9.15) > 1e-12) stop("Radius grid maximum mismatch.", call. = FALSE)
if (any(abs(diff(grid$radius) - 0.01) > 1e-12)) stop("Radius grid must use exact 0.01 spacing.", call. = FALSE)

sample_file <- file.path(app_root, "config/cover_stability_audit_sample.csv")
if (!file.exists(sample_file)) {
  set.seed(20260920L)
  idx <- sort(sample.int(nrow(prep$axes), 256L, replace = FALSE))
  sample <- data.frame(
    sample_position = seq_along(idx),
    canonical_row = idx,
    lsoa21cd = prep$ids[idx],
    stringsAsFactors = FALSE
  )
  utils::write.csv(sample, sample_file, row.names = FALSE)
}

sample <- utils::read.csv(sample_file, stringsAsFactors = FALSE)
if (nrow(sample) != 256L || anyDuplicated(sample$lsoa21cd)) {
  stop("Cover-stability audit sample is malformed.", call. = FALSE)
}

cpp_path <- file.path(framework_root, "framework/BallMapper.cpp")
tdabm_rd_compile_cpp(cpp_path)

pilot_radii <- c(0.01, 1.00, 9.15)
pilot_rows <- list()
for (r in pilot_radii) {
  z <- food_p14b_run_graph(
    axes = prep$axes,
    unit_ids = prep$ids,
    radius = r,
    replicate_index = 1L,
    base_seed = 20260807L,
    sample_indices = sample$canonical_row,
    canonical = FALSE
  )
  pilot_rows[[length(pilot_rows) + 1L]] <- z$metrics
  cat(
    "PILOT_RADIUS_OK radius=", r,
    " balls=", z$metrics$n_balls,
    " seconds=", round(z$metrics$elapsed_seconds, 3),
    "\n", sep = ""
  )
}

pilot <- do.call(rbind, pilot_rows)
utils::write.csv(
  pilot,
  file.path(app_root, "results/pilot_runtime_and_structure.csv"),
  row.names = FALSE
)

saveRDS(
  list(
    ids = prep$ids,
    axes = prep$axes,
    scaling = prep$scaling,
    metric = prep$metric,
    frozen_input_runtime_md5 = observed_sha,
    expected_frozen_sha256 = expected_sha
  ),
  file.path(app_root, "input/prepared_topology.rds"),
  compress = "gzip"
)

cat("FOOD_HFA_P1_4_B_PREFLIGHT_PASS\n")

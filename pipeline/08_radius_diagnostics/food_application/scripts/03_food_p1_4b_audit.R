#!/usr/bin/env Rscript
options(warn = 1)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Usage: 03_food_p1_4b_audit.R <package_root>", call. = FALSE)

package_root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
app_root <- file.path(package_root, "food_application")
outdir <- file.path(app_root, "results/p1_4_b")
repetitions <- suppressWarnings(as.integer(Sys.getenv("N_REPS", "1000")))
if (!is.finite(repetitions) || repetitions < 10L) {
  stop("N_REPS must be an integer >= 10.", call. = FALSE)
}

grid <- utils::read.csv(file.path(app_root, "config/radius_grid.csv"))
summary <- utils::read.csv(file.path(outdir, "radius_summary.csv"))
runs <- utils::read.csv(file.path(outdir, "all_random_order_diagnostics.csv"))
reference <- utils::read.csv(file.path(outdir, "resolution_reference.csv"))
crossings <- utils::read.csv(file.path(outdir, "dominance_crossings.csv"))
status <- utils::read.csv(file.path(outdir, "radius_execution_status.csv"))

checks <- list()
add <- function(name, pass, detail, blocking = TRUE) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = name,
    pass = isTRUE(pass),
    detail = as.character(detail),
    blocking = isTRUE(blocking),
    stringsAsFactors = FALSE
  )
}

add("radius_grid_count", nrow(grid) == 915L, nrow(grid))
add("radius_grid_min", abs(min(grid$radius) - 0.01) < 1e-12, min(grid$radius))
add("radius_grid_max", abs(max(grid$radius) - 9.15) < 1e-12, max(grid$radius))
add("radius_grid_step", all(abs(diff(grid$radius) - 0.01) < 1e-12), "0.01")
add("radius_summary_complete", nrow(summary) == 915L, nrow(summary))
add("random_order_rows", nrow(runs) == 915L * repetitions, nrow(runs))
add("all_radius_tasks_complete", !any(grepl("^FAILED:", status$status)), paste(table(status$status), collapse = ";"))
add("repetitions_each_radius", all(table(round(runs$radius, 2)) == repetitions), paste0(repetitions, " per radius"))
add("finite_ball_counts", all(is.finite(runs$n_balls) & runs$n_balls >= 1), "all")
add("finite_dominance", all(is.finite(runs$max_ball_share_observations) & runs$max_ball_share_observations >= 0 & runs$max_ball_share_observations <= 1), "all")
add("finite_sampled_cover_stability", all(is.finite(summary$mean_pairwise_cover_jaccard) & summary$mean_pairwise_cover_jaccard >= 0 & summary$mean_pairwise_cover_jaccard <= 1), "all")
add("finite_full_landmark_stability", all(is.finite(summary$mean_pairwise_landmark_jaccard) & summary$mean_pairwise_landmark_jaccard >= 0 & summary$mean_pairwise_landmark_jaccard <= 1), "all")
add("cover_stability_scope_recorded", all(summary$cover_relation_scope == "fixed_256_LSOA_audit_sample"), unique(summary$cover_relation_scope))
add("landmark_stability_scope_recorded", all(summary$landmark_scope == "full_33755_LSOAs"), unique(summary$landmark_scope))
add("automatic_approval_false", identical(as.logical(reference$automatic_approval[[1L]]), FALSE), reference$automatic_approval[[1L]])
add("optimality_claim_none", identical(toupper(as.character(reference$optimality_claim[[1L]])), "NONE"), reference$optimality_claim[[1L]])
add("majority_reference_finite", is.finite(reference$reference_radius[[1L]]), reference$reference_radius[[1L]], blocking = FALSE)
add(
  "majority_reference_not_at_grid_boundary",
  is.finite(reference$reference_radius[[1L]]) &&
    reference$reference_radius[[1L]] > min(grid$radius) + 0.01 &&
    reference$reference_radius[[1L]] < max(grid$radius) - 0.01,
  reference$reference_radius[[1L]],
  blocking = FALSE
)
add(
  "dominance_40_50_60_crossings_present",
  all(c(0.40, 0.50, 0.60) %in% crossings$target_share) &&
    all(is.finite(crossings$reference_radius[crossings$target_share %in% c(0.40,0.50,0.60)])),
  paste(crossings$target_share, crossings$status, sep = ":", collapse = ";"),
  blocking = FALSE
)

audit <- do.call(rbind, checks)
utils::write.csv(audit, file.path(outdir, "P1_4_B_validation_report.csv"), row.names = FALSE)

blocking_failure <- any(audit$blocking & !audit$pass)
warnings <- audit[!audit$blocking & !audit$pass, , drop = FALSE]

stage_status <- if (blocking_failure) {
  "BLOCKED"
} else if (nrow(warnings)) {
  "PASS_WITH_WARNINGS"
} else {
  "PASS"
}

writeLines(
  c(
    paste0("P1_4_B_STATUS=", stage_status),
    "DIAGNOSTICS_COMPLETE=TRUE",
    "RADIUS_MIN=0.01",
    "RADIUS_MAX=9.15",
    "RADIUS_STEP=0.01",
    "RADIUS_COUNT=915",
    paste0("RANDOM_ORDERS_PER_RADIUS=", repetitions),
    paste0("TOTAL_RANDOM_BM_GRAPHS=", 915L * repetitions),
    "CANONICAL_ORDER_DIAGNOSTICS_PER_RADIUS=1",
    "COVER_STABILITY_SCOPE=fixed_reproducible_256_LSOA_audit_sample",
    "LANDMARK_STABILITY_SCOPE=full_33755_LSOAs",
    paste0("PIPELINE_RESOLUTION_REFERENCE=", reference$reference_radius[[1L]]),
    paste0("REVIEW_BAND_LOWER=", reference$review_band_lower[[1L]]),
    paste0("REVIEW_BAND_UPPER=", reference$review_band_upper[[1L]]),
    "RADIUS_DECISION=PENDING_USER_DECISION",
    "AUTOMATIC_RADIUS_SELECTION=FALSE",
    "OPTIMALITY_CLAIM=NONE"
  ),
  file.path(outdir, "P1_4_B_STATUS.txt")
)

cat("P1_4_B_STATUS=", stage_status, "\n", sep = "")
if (nrow(warnings)) {
  cat("P1_4_B_WARNINGS=", paste(warnings$check, collapse = ";"), "\n", sep = "")
}
if (blocking_failure) stop("P1.4-B audit has blocking failures.", call. = FALSE)

cat("FOOD_HFA_P1_4_B_AUDIT_OK\n")

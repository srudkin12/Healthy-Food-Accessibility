#!/usr/bin/env Rscript
options(warn = 1)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Usage: 02_food_p1_4b_aggregate.R <package_root> <framework_root>", call. = FALSE)
}

package_root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
framework_root <- normalizePath(args[[2L]], winslash = "/", mustWork = TRUE)
app_root <- file.path(package_root, "food_application")

# Create output parents explicitly; empty directories may not survive ZIP extraction.
dir.create(file.path(app_root, "results", "p1_4_b"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(app_root, "figures"), recursive = TRUE, showWarnings = FALSE)

source(file.path(framework_root, "framework/R/TDABMRadiusDiagnostics.R"))
source(file.path(app_root, "R/FoodP14BAdapter.R"))

grid <- utils::read.csv(file.path(app_root, "config/radius_grid.csv"))
repetitions <- suppressWarnings(as.integer(Sys.getenv("N_REPS", "1000")))
if (!is.finite(repetitions) || repetitions < 10L) {
  stop("N_REPS must be an integer >= 10.", call. = FALSE)
}

read_all <- function(pattern, dir) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (!length(files)) stop("No files matching ", pattern, call. = FALSE)
  do.call(rbind, lapply(files, utils::read.csv, stringsAsFactors = FALSE))
}

metric_summaries <- read_all(
  "_metrics_summary\\.csv$",
  file.path(app_root, "results/p1_4_b/radius_summaries")
)
stability <- read_all(
  "_stability\\.csv$",
  file.path(app_root, "results/p1_4_b/radius_summaries")
)
canonical <- read_all(
  "_canonical\\.csv$",
  file.path(app_root, "results/p1_4_b/radius_summaries")
)
all_runs <- read_all(
  "_runs\\.csv$",
  file.path(app_root, "results/p1_4_b/radius_runs")
)

summary <- merge(
  metric_summaries,
  stability[
    ,
    c(
      "radius",
      "pairwise_comparisons",
      "mean_pairwise_cover_jaccard",
      "q10_pairwise_cover_jaccard",
      "median_pairwise_cover_jaccard",
      "min_pairwise_cover_jaccard",
      "mean_pairwise_landmark_jaccard",
      "median_pairwise_landmark_jaccard",
      "min_pairwise_landmark_jaccard",
      "cover_relation_scope",
      "landmark_scope"
    )
  ],
  by = "radius",
  all = FALSE,
  sort = TRUE
)

summary <- summary[order(summary$radius), , drop = FALSE]
canonical <- canonical[order(canonical$radius), , drop = FALSE]
all_runs <- all_runs[order(all_runs$radius, all_runs$replicate), , drop = FALSE]

if (nrow(summary) != nrow(grid)) {
  stop("Aggregated radius summary does not contain all 915 radii.", call. = FALSE)
}
if (nrow(all_runs) != nrow(grid) * repetitions) {
  stop(paste0("Expected ", nrow(grid) * repetitions,
              " random-order diagnostic rows (", nrow(grid),
              " radii x ", repetitions, " repetitions)."), call. = FALSE)
}
if (nrow(canonical) != nrow(grid)) {
  stop("Expected one canonical-order diagnostic row per radius.", call. = FALSE)
}

# Feed the sampled-cover stability field to the generic transition-report API.
# The handback always records the scope explicitly.
transition_input <- summary
transition_input$mean_pairwise_cover_jaccard <- summary$mean_pairwise_cover_jaccard

report <- tdabm_rd_resolution_transition_report(
  transition_input,
  n_observations = 33755L,
  n_dimensions = 3L,
  dominance_targets = c(0.40, 0.50, 0.60, 0.75)
)

report$reference$cover_stability_scope <- "fixed_reproducible_256_LSOA_audit_sample"
report$reference$automatic_approval <- FALSE
report$reference$optimality_claim <- "NONE"

review <- if (is.finite(report$reference$reference_radius[[1L]])) {
  tdabm_rd_resolution_review_rows(
    transition_input,
    reference_radius = report$reference$reference_radius[[1L]],
    points_each_side = 5L
  )
} else {
  transition_input[0, , drop = FALSE]
}

outdir <- file.path(app_root, "results/p1_4_b")
food_p14b_atomic_write_csv(all_runs, file.path(outdir, "all_random_order_diagnostics.csv"))
food_p14b_atomic_write_csv(canonical, file.path(outdir, "canonical_order_diagnostics.csv"))
food_p14b_atomic_write_csv(summary, file.path(outdir, "radius_summary.csv"))
food_p14b_atomic_write_csv(report$transitions, file.path(outdir, "topology_transitions.csv"))
food_p14b_atomic_write_csv(report$dominance_crossings, file.path(outdir, "dominance_crossings.csv"))
food_p14b_atomic_write_csv(report$reference, file.path(outdir, "resolution_reference.csv"))
food_p14b_atomic_write_csv(review, file.path(outdir, "resolution_review_rows.csv"))

plot_input <- summary
plot_input$mean_pairwise_cover_jaccard <- summary$mean_pairwise_cover_jaccard
plots <- tdabm_rd_write_diagnostic_plots(
  plot_input,
  file.path(app_root, "figures")
)
food_p14b_atomic_write_csv(
  plots,
  file.path(outdir, "figure_register.csv")
)

cat(
  "FOOD_HFA_P1_4_B_AGGREGATE_OK radii=", nrow(summary),
  " random_graphs=", nrow(all_runs),
  "\n", sep = ""
)

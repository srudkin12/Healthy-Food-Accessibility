#!/usr/bin/env Rscript
options(warn = 1)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Usage: 01_food_p1_4b_dense_runner.R <package_root> <framework_root>", call. = FALSE)
}

package_root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
framework_root <- normalizePath(args[[2L]], winslash = "/", mustWork = TRUE)
app_root <- file.path(package_root, "food_application")

source(file.path(framework_root, "framework/R/TDABMRadiusDiagnostics.R"))
source(file.path(app_root, "R/FoodP14BAdapter.R"))

obj <- readRDS(file.path(app_root, "input/prepared_topology.rds"))
grid <- utils::read.csv(file.path(app_root, "config/radius_grid.csv"))
sample <- utils::read.csv(file.path(app_root, "config/cover_stability_audit_sample.csv"))

repetitions <- suppressWarnings(as.integer(Sys.getenv("N_REPS", "1000")))
if (!is.finite(repetitions) || repetitions < 10L) {
  stop("N_REPS must be an integer >= 10.", call. = FALSE)
}
base_seed <- 20260807L

workers <- suppressWarnings(as.integer(Sys.getenv("N_WORKERS", "50")))
if (!is.finite(workers) || workers < 1L) workers <- 1L
detected <- suppressWarnings(parallel::detectCores(logical = FALSE))
if (is.finite(detected) && detected > 0) workers <- min(workers, detected)
workers <- min(workers, nrow(grid))

for (d in c(
  "results/p1_4_b/radius_runs",
  "results/p1_4_b/radius_summaries",
  "checkpoints/p1_4_b"
)) {
  dir.create(file.path(app_root, d), recursive = TRUE, showWarnings = FALSE)
}

# Compile once in the Linux parent. Fork workers inherit the loaded shared object.
tdabm_rd_compile_cpp(file.path(framework_root, "framework/BallMapper.cpp"))

run_radius <- function(i) {
  radius <- as.numeric(grid$radius[[i]])
  tag <- sprintf("%04d_%0.2f", i, radius)
  tag <- gsub("\\.", "p", tag)

  run_file <- file.path(
    app_root, "results/p1_4_b/radius_runs",
    paste0("radius_", tag, "_runs.csv")
  )
  stability_file <- file.path(
    app_root, "results/p1_4_b/radius_summaries",
    paste0("radius_", tag, "_stability.csv")
  )
  summary_file <- file.path(
    app_root, "results/p1_4_b/radius_summaries",
    paste0("radius_", tag, "_metrics_summary.csv")
  )
  canonical_file <- file.path(
    app_root, "results/p1_4_b/radius_summaries",
    paste0("radius_", tag, "_canonical.csv")
  )
  checkpoint_file <- file.path(
    app_root, "checkpoints/p1_4_b",
    paste0("radius_", tag, "_checkpoint.rds")
  )

  if (file.exists(run_file) && file.exists(stability_file) && file.exists(summary_file) && file.exists(canonical_file)) {
    old <- tryCatch(utils::read.csv(run_file), error = function(e) NULL)
    if (!is.null(old) && nrow(old) == repetitions) {
      cat("RADIUS_SKIP_COMPLETE radius=", sprintf("%.2f", radius), "\n", sep = "")
      return(data.frame(radius = radius, status = "SKIPPED_COMPLETE", stringsAsFactors = FALSE))
    }
  }

  cat("RADIUS_START radius=", sprintf("%.2f", radius), "\n", sep = "")

  if (file.exists(checkpoint_file)) {
    cp <- readRDS(checkpoint_file)
    metrics <- cp$metrics
    sample_signatures <- cp$sample_signatures
    landmark_signatures <- cp$landmark_signatures
  } else {
    metrics <- NULL
    sample_signatures <- list()
    landmark_signatures <- list()
  }

  done <- if (is.null(metrics)) integer() else as.integer(metrics$replicate)
  todo <- setdiff(seq_len(repetitions), done)

  for (rep_id in todo) {
    z <- food_p14b_run_graph(
      axes = obj$axes,
      unit_ids = obj$ids,
      radius = radius,
      replicate_index = rep_id,
      base_seed = base_seed,
      sample_indices = sample$canonical_row,
      canonical = FALSE
    )

    metrics <- if (is.null(metrics)) z$metrics else rbind(metrics, z$metrics)
    sample_signatures[[as.character(rep_id)]] <- z$sample_cover_signature
    landmark_signatures[[as.character(rep_id)]] <- z$landmark_signature

    if ((rep_id %% 10L) == 0L || rep_id == repetitions) {
      metrics <- metrics[order(metrics$replicate), , drop = FALSE]
      food_p14b_atomic_save_rds(
        list(
          metrics = metrics,
          sample_signatures = sample_signatures,
          landmark_signatures = landmark_signatures,
          radius = radius,
          repetitions = repetitions,
          cover_sample_size = nrow(sample)
        ),
        checkpoint_file
      )
      cat(
        "RADIUS_CHECKPOINT radius=", sprintf("%.2f", radius),
        " completed=", nrow(metrics), "/", repetitions,
        "\n", sep = ""
      )
    }

    rm(z)
    if ((rep_id %% 10L) == 0L) gc(verbose = FALSE)
  }

  metrics <- metrics[order(metrics$replicate), , drop = FALSE]
  if (nrow(metrics) != repetitions || !identical(as.integer(metrics$replicate), seq_len(repetitions))) {
    stop("Incomplete repetition set for radius ", radius, call. = FALSE)
  }

  sample_signatures <- sample_signatures[as.character(seq_len(repetitions))]
  landmark_signatures <- landmark_signatures[as.character(seq_len(repetitions))]

  stability <- food_p14b_summarise_radius(
    metrics = metrics,
    sample_signatures = sample_signatures,
    landmark_signatures = landmark_signatures,
    sample_size = nrow(sample),
    n_observations = nrow(obj$axes)
  )

  metric_summary <- food_p14b_metrics_summary_row(metrics)

  canonical <- food_p14b_run_graph(
    axes = obj$axes,
    unit_ids = obj$ids,
    radius = radius,
    replicate_index = 0L,
    base_seed = base_seed,
    sample_indices = sample$canonical_row,
    canonical = TRUE
  )$metrics

  food_p14b_atomic_write_csv(metrics, run_file)
  food_p14b_atomic_write_csv(stability, stability_file)
  food_p14b_atomic_write_csv(metric_summary, summary_file)
  food_p14b_atomic_write_csv(canonical, canonical_file)

  unlink(checkpoint_file)

  cat(
    "RADIUS_COMPLETE radius=", sprintf("%.2f", radius),
    " median_balls=", metric_summary$median_n_balls,
    " max_ball_share=", round(metric_summary$median_max_ball_share_observations, 6),
    "\n", sep = ""
  )

  data.frame(radius = radius, status = "COMPLETE", stringsAsFactors = FALSE)
}

indices <- seq_len(nrow(grid))

if (.Platform$OS.type == "unix" && workers > 1L) {
  status <- parallel::mclapply(
    indices,
    function(i) {
      tryCatch(
        run_radius(i),
        error = function(e) data.frame(
          radius = as.numeric(grid$radius[[i]]),
          status = paste0("FAILED: ", conditionMessage(e)),
          stringsAsFactors = FALSE
        )
      )
    },
    mc.cores = workers,
    mc.preschedule = FALSE,
    mc.set.seed = FALSE
  )
} else {
  status <- lapply(
    indices,
    function(i) {
      tryCatch(
        run_radius(i),
        error = function(e) data.frame(
          radius = as.numeric(grid$radius[[i]]),
          status = paste0("FAILED: ", conditionMessage(e)),
          stringsAsFactors = FALSE
        )
      )
    }
  )
}

status <- do.call(rbind, status)
food_p14b_atomic_write_csv(
  status,
  file.path(app_root, "results/p1_4_b/radius_execution_status.csv")
)

failed <- grepl("^FAILED:", status$status)
if (any(failed)) {
  stop(
    "At least one radius failed. Re-run the stage to resume after inspecting radius_execution_status.csv.",
    call. = FALSE
  )
}

cat(
  "FOOD_HFA_P1_4_B_DENSE_RUN_COMPLETE radii=", nrow(grid),
  " repetitions_per_radius=", repetitions,
  " workers=", workers,
  "\n", sep = ""
)

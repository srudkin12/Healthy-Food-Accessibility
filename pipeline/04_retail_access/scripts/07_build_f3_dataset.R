source("R/functions.R")

ROOT <- root_dir()

base <- read.csv(
  file.path(ROOT, "data_staged", "hfa_f2a_inherited.csv"),
  check.names = FALSE
)
retail <- read.csv(
  file.path(ROOT, "data_staged", "f3_student_sample_reconstruction.csv"),
  check.names = FALSE
)

retail <- retail[match(base$lsoa21cd, retail$lsoa21cd), ]
assert_true(identical(as.character(base$lsoa21cd), as.character(retail$lsoa21cd)),
            "Retail measures cannot be aligned exactly to F2A baseline")

new_cols <- setdiff(names(retail), "lsoa21cd")
out <- cbind(base, retail[, new_cols, drop = FALSE])

out$geolytix_source_vintage <- "Q1_2023_reconstruction_feature_service"
out$geolytix_large_rule <- "size_band_B_C_D_ge_280m2"
out$geolytix_distance_origin <- "ONS_LSOA21_population_weighted_centroid"
out$geolytix_distance_metric <- "BNG_Euclidean"
out$geolytix_internal_count_boundary <- "ONS_LSOA21_BGC_V5"
out$jts2019_public_transport_downstream_status <- "EXCLUDE_SOURCE_CONFLICT"

assert_true(nrow(out) == 33755L, "F3 final dataset row count is not 33755")
assert_true(length(unique(out$lsoa21cd)) == 33755L, "F3 final LSOA codes are not unique")
assert_true(all(is.finite(out$geolytix_large_nearest_m)),
            "Missing/non-finite large-store nearest distances")

# Crosswalk correlations are descriptive validation only.
candidate <- c(
  "geolytix_large_nearest_km",
  "jts2019_car_min",
  "jts2019_cycling_min",
  "jts2019_walking_min",
  grep("^tcm2025_.*shopping_", names(out), value = TRUE)
)
candidate <- unique(candidate[candidate %in% names(out)])

corr_rows <- list()
for (nm in setdiff(candidate, "geolytix_large_nearest_km")) {
  corr_rows[[length(corr_rows) + 1L]] <- data.frame(
    x = "geolytix_large_nearest_km",
    y = nm,
    correlation = safe_cor(out$geolytix_large_nearest_km, suppressWarnings(as.numeric(out[[nm]]))),
    n_complete = sum(is.finite(out$geolytix_large_nearest_km) &
                       is.finite(suppressWarnings(as.numeric(out[[nm]])))),
    stringsAsFactors = FALSE
  )
}
corr <- if (length(corr_rows)) do.call(rbind, corr_rows) else data.frame()
write.csv(corr,
          file.path(ROOT, "results", "f3", "physical_access_crosswalk_correlations.csv"),
          row.names = FALSE)

outfile <- file.path(ROOT, "data_staged", "hfa_f3_functional_retail_access.csv")
write.csv(out, outfile, row.names = FALSE)

cat("HFA_F3_DATASET_OK rows=", nrow(out),
    " columns=", ncol(out), "\n", sep = "")

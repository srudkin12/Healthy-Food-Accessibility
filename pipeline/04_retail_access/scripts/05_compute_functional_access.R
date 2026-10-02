source("R/functions.R")

ROOT <- root_dir()

pwc_file <- file.path(ROOT, "data_raw", "ons", "lsoa21_pwc_v4_england.csv")
stores_file <- file.path(ROOT, "data_staged", "geolytix_q1_2023_with_lsoa21.csv")
f2a_file <- file.path(ROOT, "data_staged", "hfa_f2a_inherited.csv")

assert_true(file.exists(pwc_file), "PWC file missing")
assert_true(file.exists(stores_file), "Store assignment file missing")
assert_true(file.exists(f2a_file), "Inherited F2A dataset missing")

pwc <- read.csv(pwc_file, check.names = FALSE)
stores <- read.csv(stores_file, check.names = FALSE)
f2a <- read.csv(f2a_file, check.names = FALSE)

pwc <- pwc[match(f2a$lsoa21cd, pwc$lsoa21cd), ]
assert_true(identical(as.character(pwc$lsoa21cd), as.character(f2a$lsoa21cd)),
            "PWC could not be aligned exactly to inherited LSOA21 spine")

pwc$pwc_e <- as.numeric(pwc$pwc_e)
pwc$pwc_n <- as.numeric(pwc$pwc_n)

stores$bng_e <- suppressWarnings(as.numeric(stores$bng_e))
stores$bng_n <- suppressWarnings(as.numeric(stores$bng_n))

valid <- is.finite(stores$bng_e) & is.finite(stores$bng_n)
all_stores <- stores[valid, , drop = FALSE]
large_stores <- stores[valid & stores$large_store_student_rule, , drop = FALSE]

msg("Computing functional access to all Geolytix retail points")
aa <- compute_nn_access(
  query_df = pwc,
  store_df = all_stores,
  prefix = "geolytix_any",
  thresholds_m = c(500, 1000, 2000, 5000, 10000),
  decay_scales_m = c(2000, 5000)
)

msg("Computing functional access to >=280m2 Geolytix stores")
ll <- compute_nn_access(
  query_df = pwc,
  store_df = large_stores,
  prefix = "geolytix_large",
  thresholds_m = c(500, 1000, 2000, 5000, 10000),
  decay_scales_m = c(2000, 5000)
)

a <- aa$data
l <- ll$data

a$geolytix_any_nearest_store_id <- all_stores$id[a$geolytix_any_nearest_store_row]
a$geolytix_any_nearest_retailer <- all_stores$retailer[a$geolytix_any_nearest_store_row]
a$geolytix_any_nearest_size_band <- all_stores$size_band_clean[a$geolytix_any_nearest_store_row]

l$geolytix_large_nearest_store_id <- large_stores$id[l$geolytix_large_nearest_store_row]
l$geolytix_large_nearest_retailer <- large_stores$retailer[l$geolytix_large_nearest_store_row]
l$geolytix_large_nearest_size_band <- large_stores$size_band_clean[l$geolytix_large_nearest_store_row]

out <- cbind(
  pwc[, c("lsoa21cd", "pwc_e", "pwc_n")],
  a[, setdiff(names(a), c("row_id", "geolytix_any_nearest_store_row"))],
  l[, setdiff(names(l), c("row_id", "geolytix_large_nearest_store_row"))]
)

for (nm in grep("_m$", names(out), value = TRUE)) {
  out[[sub("_m$", "_km", nm)]] <- out[[nm]] / 1000
}

qa <- rbind(aa$qa, ll$qa)
write.csv(qa,
          file.path(ROOT, "results", "f3", "nearest_neighbour_radius_completeness.csv"),
          row.names = FALSE)

assert_true(all(qa$unresolved_at_final_k == 0L),
            "Radius completeness audit failed")
assert_true(nrow(out) == 33755L, "Functional access output does not have 33755 rows")

write.csv(out,
          file.path(ROOT, "data_staged", "f3_geolytix_functional_access.csv"),
          row.names = FALSE)

cat("FUNCTIONAL_ACCESS_OK rows=", nrow(out),
    " all_stores=", nrow(all_stores),
    " large_stores=", nrow(large_stores), "\n", sep = "")

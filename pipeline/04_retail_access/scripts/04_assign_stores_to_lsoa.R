source("R/functions.R")

ROOT <- root_dir()

stores_file <- file.path(ROOT, "data_staged", "geolytix_q1_2023_classified.csv")
gpkg <- file.path(ROOT, "data_raw", "ons", "lsoa21_bgc_v5_england.gpkg")
assert_true(file.exists(stores_file), "Classified Geolytix file missing")
assert_true(file.exists(gpkg), "ONS LSOA boundary GPKG missing")

stores <- read.csv(stores_file, check.names = FALSE)
stores$bng_e <- suppressWarnings(as.numeric(stores$bng_e))
stores$bng_n <- suppressWarnings(as.numeric(stores$bng_n))

valid <- is.finite(stores$bng_e) & is.finite(stores$bng_n)
boundaries <- sf::st_read(gpkg, quiet = TRUE)
boundaries <- sf::st_transform(boundaries, 27700)

pts <- sf::st_as_sf(
  stores[valid, , drop = FALSE],
  coords = c("bng_e", "bng_n"),
  crs = 27700,
  remove = FALSE
)

hits <- sf::st_intersects(pts, boundaries, sparse = TRUE)
n_hits <- lengths(hits)

assigned <- rep(NA_character_, nrow(pts))
has_hit <- n_hits >= 1L
assigned[has_hit] <- vapply(
  hits[has_hit],
  function(ii) as.character(boundaries$lsoa21cd[ii[1]]),
  character(1)
)

stores$internal_lsoa21cd <- NA_character_
stores$internal_lsoa21_match_n <- 0L
stores$internal_lsoa21cd[valid] <- assigned
stores$internal_lsoa21_match_n[valid] <- n_hits

qa <- data.frame(
  metric = c(
    "stores_total",
    "stores_valid_bng",
    "stores_assigned_to_english_lsoa21",
    "stores_not_assigned_to_english_lsoa21",
    "stores_with_multiple_lsoa_intersections",
    "large_stores_total_valid_bng",
    "large_stores_assigned_to_english_lsoa21"
  ),
  value = c(
    nrow(stores),
    sum(valid),
    sum(!is.na(stores$internal_lsoa21cd)),
    sum(valid & is.na(stores$internal_lsoa21cd)),
    sum(stores$internal_lsoa21_match_n > 1L),
    sum(valid & stores$large_store_student_rule),
    sum(valid & stores$large_store_student_rule & !is.na(stores$internal_lsoa21cd))
  ),
  stringsAsFactors = FALSE
)

write.csv(qa,
          file.path(ROOT, "results", "f3", "store_to_lsoa_assignment_qa.csv"),
          row.names = FALSE)

if (sum(stores$internal_lsoa21_match_n > 1L) > 0L) {
  write.csv(
    stores[stores$internal_lsoa21_match_n > 1L,
           c("OBJECTID", "id", "store_name", "postcode", "bng_e", "bng_n",
             "internal_lsoa21cd", "internal_lsoa21_match_n")],
    file.path(ROOT, "results", "f3", "store_boundary_ambiguities.csv"),
    row.names = FALSE
  )
}

write.csv(stores,
          file.path(ROOT, "data_staged", "geolytix_q1_2023_with_lsoa21.csv"),
          row.names = FALSE)

cat("STORES_TO_LSOA_OK assigned_english=",
    sum(!is.na(stores$internal_lsoa21cd)),
    " ambiguous=", sum(stores$internal_lsoa21_match_n > 1L), "\n", sep = "")

source("R/functions.R")

ROOT <- root_dir()

geolytix_layer <- paste0(
  "https://services.arcgis.com/WQ9KVmV6xGGMnCiQ/ArcGIS/rest/services/",
  "Geolytix_Retail_Points_Q1_2023/FeatureServer/7"
)
pwc_layer <- paste0(
  "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/",
  "LSOA_PopCentroids_EW_2021_V4/FeatureServer/0"
)
boundary_layer <- paste0(
  "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/",
  "Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BGC_V5/",
  "FeatureServer/0"
)

geo_dir <- file.path(ROOT, "data_raw", "geolytix", "chunks")
pwc_dir <- file.path(ROOT, "data_raw", "ons", "pwc_chunks")
bdy_dir <- file.path(ROOT, "data_raw", "ons", "boundary_chunks")

geo_fields <- c(
  "OBJECTID", "id", "retailer", "fascia", "store_name",
  "add_one", "add_two", "town", "suburb", "postcode",
  "long_wgs", "lat_wgs", "bng_e", "bng_n", "pqi",
  "open_date", "size_band", "revised_county"
)

msg("Staging Geolytix Retail Points Q1 2023 reconstruction source")
stores <- arcgis_download_table(
  geolytix_layer,
  fields = geo_fields,
  dest_dir = geo_dir,
  where = "1=1",
  order_field = "OBJECTID",
  include_geometry = FALSE
)

for (nm in c("bng_e", "bng_n", "long_wgs", "lat_wgs")) {
  stores[[nm]] <- suppressWarnings(as.numeric(stores[[nm]]))
}

assert_true(nrow(stores) > 5000L && nrow(stores) < 100000L,
            paste0("Implausible Geolytix row count: ", nrow(stores)))
assert_true(mean(is.finite(stores$bng_e) & is.finite(stores$bng_n)) > 0.95,
            "Too many missing Geolytix BNG coordinates")

geo_csv <- file.path(ROOT, "data_raw", "geolytix", "geolytix_q1_2023_reconstruction.csv")
write.csv(stores, geo_csv, row.names = FALSE)

msg("Staging ONS LSOA21 population-weighted centroids")
pwc <- arcgis_download_table(
  pwc_layer,
  fields = c("FID", "LSOA21CD"),
  dest_dir = pwc_dir,
  where = "LSOA21CD LIKE 'E%'",
  order_field = "FID",
  include_geometry = TRUE,
  out_sr = 27700L
)

names(pwc)[names(pwc) == "LSOA21CD"] <- "lsoa21cd"
names(pwc)[names(pwc) == "arcgis_x"] <- "pwc_e"
names(pwc)[names(pwc) == "arcgis_y"] <- "pwc_n"
pwc$pwc_e <- as.numeric(pwc$pwc_e)
pwc$pwc_n <- as.numeric(pwc$pwc_n)

assert_true(nrow(pwc) == 33755L,
            paste0("ONS PWC England row count is ", nrow(pwc), " not 33755"))
assert_true(length(unique(pwc$lsoa21cd)) == 33755L, "ONS PWC codes are not unique")
assert_true(all(is.finite(pwc$pwc_e) & is.finite(pwc$pwc_n)),
            "ONS PWC contains missing/non-finite coordinates")

pwc_csv <- file.path(ROOT, "data_raw", "ons", "lsoa21_pwc_v4_england.csv")
write.csv(pwc[, c("lsoa21cd", "pwc_e", "pwc_n")], pwc_csv, row.names = FALSE)

msg("Staging ONS LSOA21 England boundaries")
bdy <- arcgis_download_geojson_sf(
  boundary_layer,
  fields = c("FID", "LSOA21CD"),
  dest_dir = bdy_dir,
  where = "LSOA21CD LIKE 'E%'",
  order_field = "FID",
  out_sr = 27700L
)

names(bdy)[names(bdy) == "LSOA21CD"] <- "lsoa21cd"
assert_true(nrow(bdy) == 33755L,
            paste0("ONS boundary England row count is ", nrow(bdy), " not 33755"))
assert_true(length(unique(bdy$lsoa21cd)) == 33755L, "ONS boundary codes are not unique")

gpkg <- file.path(ROOT, "data_raw", "ons", "lsoa21_bgc_v5_england.gpkg")
if (file.exists(gpkg)) file.remove(gpkg)
sf::st_write(bdy[, "lsoa21cd"], gpkg, quiet = TRUE)

raw_files <- c(
  geo_csv,
  pwc_csv,
  gpkg,
  list.files(geo_dir, full.names = TRUE),
  list.files(pwc_dir, full.names = TRUE),
  list.files(bdy_dir, full.names = TRUE)
)
raw_files <- raw_files[file.exists(raw_files) & !dir.exists(raw_files)]

manifest <- data.frame(
  path = sub(paste0("^", ROOT, "/?"), "", raw_files),
  bytes = as.numeric(file.info(raw_files)$size),
  sha256 = vapply(raw_files, sha256_file, character(1)),
  stringsAsFactors = FALSE
)
write.csv(manifest,
          file.path(ROOT, "results", "f3", "source_checksums.csv"),
          row.names = FALSE)

write.csv(
  data.frame(
    source = c("Geolytix Q1 2023 reconstruction Feature Service",
               "ONS LSOA21 Population Weighted Centroids V4",
               "ONS LSOA21 BGC V5 boundaries"),
    url = c(geolytix_layer, pwc_layer, boundary_layer),
    role = c("retail point reconstruction source",
             "representative location for functional accessibility",
             "internal-LSOA store assignment"),
    rows = c(nrow(stores), nrow(pwc), nrow(bdy)),
    staged_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    stringsAsFactors = FALSE
  ),
  file.path(ROOT, "results", "f3", "source_register.csv"),
  row.names = FALSE
)

cat("F3_SOURCES_STAGED_OK geolytix_rows=", nrow(stores),
    " pwc_rows=", nrow(pwc),
    " boundaries_rows=", nrow(bdy), "\n", sep = "")

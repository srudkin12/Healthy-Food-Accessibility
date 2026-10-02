source("R/functions.R")

ROOT <- root_dir()

base <- read.csv(file.path(ROOT, "data_staged", "hfa_f3_inherited.csv"), check.names = FALSE)
imd <- read.csv(file.path(ROOT, "data_staged", "imd2025_income_lsoa21.csv"), check.names = FALSE)
ruc <- read.csv(file.path(ROOT, "data_staged", "ruc2021_lsoa21.csv"), check.names = FALSE)

imd <- imd[match(base$lsoa21cd, imd$lsoa21cd), ]
ruc <- ruc[match(base$lsoa21cd, ruc$lsoa21cd), ]

assert_true(identical(as.character(base$lsoa21cd), as.character(imd$lsoa21cd)),
            "IMD cannot align exactly to F3 spine")
assert_true(identical(as.character(base$lsoa21cd), as.character(ruc$lsoa21cd)),
            "RUC cannot align exactly to F3 spine")

out <- cbind(
  base,
  imd[, setdiff(names(imd), "lsoa21cd"), drop = FALSE],
  ruc[, setdiff(names(ruc), "lsoa21cd"), drop = FALSE]
)

out$log_large_nearest_km <- log1p(out$geolytix_large_nearest_km)
out$z_physical_distance <- zscore(out$log_large_nearest_km)
out$z_car_none <- zscore(out$car_none_pct)
out$z_income_deprivation <- zscore(out$income_deprivation_score_2025)

assert_true(all(is.finite(out$z_physical_distance)), "Physical z-score missing")
assert_true(all(is.finite(out$z_car_none)), "Car-none z-score missing")
assert_true(all(is.finite(out$z_income_deprivation)), "Income z-score missing")

write.csv(out,
          file.path(ROOT, "data_staged", "hfa_f4_capability_dataset.csv"),
          row.names = FALSE)

cat("F4_CAPABILITY_DATASET_OK rows=", nrow(out), " columns=", ncol(out), "\n", sep = "")

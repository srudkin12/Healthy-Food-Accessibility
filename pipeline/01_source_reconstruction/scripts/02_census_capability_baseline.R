source(file.path(Sys.getenv("HFA_ROOT"), "R", "functions.R"))
ensure_dirs()

urls <- c(
  ts045 = "https://www.nomisweb.co.uk/output/census/2021/census2021-ts045.zip",
  ts011 = "https://www.nomisweb.co.uk/output/census/2021/census2021-ts011.zip"
)
paths <- c(
  ts045 = p("data_raw", "nomis", "census2021-ts045.zip"),
  ts011 = p("data_raw", "nomis", "census2021-ts011.zip")
)

safe_download(urls[["ts045"]], paths[["ts045"]])
safe_download(urls[["ts011"]], paths[["ts011"]])

spine <- readr::read_csv(p("data_staged", "lsoa21_spine.csv"), show_col_types = FALSE)
assert_true(nrow(spine) == 33755L, "Canonical LSOA21 spine is missing or wrong size")

# ---- TS045 car/van availability ----
car <- read_lsoa_bulk(paths[["ts045"]])
car_code <- find_code_col(car)
names(car)[names(car) == car_code] <- "lsoa21cd"
car <- car[grepl("^E[0-9]{8}$", car$lsoa21cd), , drop = FALSE]

car_total <- pick_col(car, c("total", "all_households"), "TS045 total all households")
car_none  <- pick_col(car, c("no_cars_or_vans", "household"), "TS045 no cars/vans")
car_one   <- pick_col(car, c("(^|_)1_car_or_van", "household"), "TS045 one car/van")
car_two   <- pick_col(car, c("(^|_)2_cars_or_vans", "household"), "TS045 two cars/vans")
car_three <- pick_col(car, c("3_or_more_cars_or_vans", "household"), "TS045 three-plus cars/vans")

car_out <- tibble::tibble(
  lsoa21cd = car$lsoa21cd,
  households_total = as_num(car[[car_total]]),
  car_none_n = as_num(car[[car_none]]),
  car_1_n = as_num(car[[car_one]]),
  car_2_n = as_num(car[[car_two]]),
  car_3plus_n = as_num(car[[car_three]])
) |>
  dplyr::mutate(
    car_none_pct = pct(car_none_n, households_total),
    car_1_pct = pct(car_1_n, households_total),
    car_2_pct = pct(car_2_n, households_total),
    car_3plus_pct = pct(car_3plus_n, households_total),
    car_any_pct = pct(car_1_n + car_2_n + car_3plus_n, households_total)
  ) |>
  dplyr::distinct(lsoa21cd, .keep_all = TRUE)

# ---- TS011 deprivation dimensions ----
dep <- read_lsoa_bulk(paths[["ts011"]])
dep_code <- find_code_col(dep)
names(dep)[names(dep) == dep_code] <- "lsoa21cd"
dep <- dep[grepl("^E[0-9]{8}$", dep$lsoa21cd), , drop = FALSE]

dep_total <- pick_col(dep, c("total", "all_households"), "TS011 total all households")
dep_zero  <- pick_col(dep, c("not_deprived", "any_dimension"), "TS011 deprived in zero dimensions")
dep_one   <- pick_col(dep, c("deprived", "one_dimension"), "TS011 deprived in one dimension")
dep_two   <- pick_col(dep, c("deprived", "two_dimensions"), "TS011 deprived in two dimensions")
dep_three <- pick_col(dep, c("deprived", "three_dimensions"), "TS011 deprived in three dimensions")
dep_four  <- pick_col(dep, c("deprived", "four_dimensions"), "TS011 deprived in four dimensions")

dep_out <- tibble::tibble(
  lsoa21cd = dep$lsoa21cd,
  deprivation_households_total = as_num(dep[[dep_total]]),
  deprived_0_n = as_num(dep[[dep_zero]]),
  deprived_1_n = as_num(dep[[dep_one]]),
  deprived_2_n = as_num(dep[[dep_two]]),
  deprived_3_n = as_num(dep[[dep_three]]),
  deprived_4_n = as_num(dep[[dep_four]])
) |>
  dplyr::mutate(
    deprived_0_pct = pct(deprived_0_n, deprivation_households_total),
    deprived_1_pct = pct(deprived_1_n, deprivation_households_total),
    deprived_2_pct = pct(deprived_2_n, deprivation_households_total),
    deprived_3_pct = pct(deprived_3_n, deprivation_households_total),
    deprived_4_pct = pct(deprived_4_n, deprivation_households_total),
    deprived_any_pct = pct(deprived_1_n + deprived_2_n + deprived_3_n + deprived_4_n,
                           deprivation_households_total)
  ) |>
  dplyr::distinct(lsoa21cd, .keep_all = TRUE)

assert_true(nrow(car_out) == 33755L, paste0("TS045 England LSOA count = ", nrow(car_out), ", expected 33,755"))
assert_true(nrow(dep_out) == 33755L, paste0("TS011 England LSOA count = ", nrow(dep_out), ", expected 33,755"))

baseline <- spine |>
  dplyr::left_join(car_out, by = "lsoa21cd") |>
  dplyr::left_join(dep_out, by = "lsoa21cd")

assert_true(nrow(baseline) == 33755L, "Census join changed LSOA21 row count")
assert_true(sum(is.na(baseline$households_total)) == 0L, "TS045 missing after join")
assert_true(sum(is.na(baseline$deprivation_households_total)) == 0L, "TS011 missing after join")

# Internal composition checks. Allow tiny perturbation differences but not material failures.
car_gap <- with(baseline, abs((car_none_n + car_1_n + car_2_n + car_3plus_n) - households_total))
dep_gap <- with(baseline, abs((deprived_0_n + deprived_1_n + deprived_2_n + deprived_3_n + deprived_4_n) - deprivation_households_total))
assert_true(max(car_gap, na.rm = TRUE) <= 5, paste0("Unexpected TS045 composition gap; max = ", max(car_gap, na.rm = TRUE)))
assert_true(max(dep_gap, na.rm = TRUE) <= 5, paste0("Unexpected TS011 composition gap; max = ", max(dep_gap, na.rm = TRUE)))

write_csv_atomic(car_out, p("data_staged", "census_ts045_car_availability_lsoa21.csv"))
write_csv_atomic(dep_out, p("data_staged", "census_ts011_deprivation_lsoa21.csv"))
write_csv_atomic(baseline, p("data_staged", "census_capability_baseline.csv"))
write_csv_atomic(baseline, p("data_staged", "hfa_f0f1_baseline.csv"))

say("CENSUS_CAPABILITY_BASELINE_OK rows=", nrow(baseline),
    " car_gap_max=", max(car_gap, na.rm = TRUE),
    " deprivation_gap_max=", max(dep_gap, na.rm = TRUE))

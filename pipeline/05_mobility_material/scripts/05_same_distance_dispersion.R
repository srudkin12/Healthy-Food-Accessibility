source("R/functions.R")

ROOT <- root_dir()
d <- read.csv(file.path(ROOT, "data_staged", "hfa_f4_capability_dataset.csv"),
              check.names = FALSE)

# Equal-frequency deciles of physical distance.
br <- stats::quantile(d$geolytix_large_nearest_km, probs = seq(0, 1, .1),
                      na.rm = TRUE, type = 7)
br <- unique(br)
d$physical_distance_decile <- cut(
  d$geolytix_large_nearest_km,
  breaks = br,
  include.lowest = TRUE,
  labels = FALSE
)

summ <- do.call(rbind, lapply(sort(unique(d$physical_distance_decile)), function(g) {
  z <- d[d$physical_distance_decile == g, ]
  data.frame(
    physical_distance_decile = g,
    n = nrow(z),
    distance_min_km = min(z$geolytix_large_nearest_km),
    distance_median_km = median(z$geolytix_large_nearest_km),
    distance_max_km = max(z$geolytix_large_nearest_km),
    car_none_p25 = quantile(z$car_none_pct, .25),
    car_none_median = median(z$car_none_pct),
    car_none_p75 = quantile(z$car_none_pct, .75),
    income_score_p25 = quantile(z$income_deprivation_score_2025, .25),
    income_score_median = median(z$income_deprivation_score_2025),
    income_score_p75 = quantile(z$income_deprivation_score_2025, .75),
    stringsAsFactors = FALSE
  )
}))

write.csv(summ,
          file.path(ROOT, "results", "f4", "same_distance_capability_dispersion.csv"),
          row.names = FALSE)

# Within-distance spread relative to total spread.
disp <- data.frame(
  measure = c("car_none_pct", "income_deprivation_score_2025"),
  overall_sd = c(sd(d$car_none_pct), sd(d$income_deprivation_score_2025)),
  mean_within_distance_decile_sd = c(
    mean(tapply(d$car_none_pct, d$physical_distance_decile, sd)),
    mean(tapply(d$income_deprivation_score_2025, d$physical_distance_decile, sd))
  ),
  stringsAsFactors = FALSE
)
disp$within_to_overall_sd_ratio <- disp$mean_within_distance_decile_sd / disp$overall_sd

write.csv(disp,
          file.path(ROOT, "results", "f4", "same_distance_dispersion_ratio.csv"),
          row.names = FALSE)

cat("SAME_DISTANCE_DISPERSION_OK deciles=", nrow(summ), "\n", sep = "")

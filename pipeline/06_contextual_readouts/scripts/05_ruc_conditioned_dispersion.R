source("R/functions.R")

ROOT <- root_dir()
d <- read.csv(file.path(ROOT, "data_staged", "hfa_f4_inherited.csv"),
              check.names = FALSE)

rows <- list()

for (ruc in sort(unique(d$ruc2021))) {
  z <- d[d$ruc2021 == ruc, ]
  # Within-RUC deciles of physical distance.
  br <- unique(quantile(z$geolytix_large_nearest_km,
                        probs = seq(0, 1, .1), type = 7))
  grp <- cut(z$geolytix_large_nearest_km, breaks = br,
             include.lowest = TRUE, labels = FALSE)

  for (nm in c("car_none_pct","income_deprivation_score_2025")) {
    overall_sd <- sd(z[[nm]])
    within_sd <- mean(tapply(z[[nm]], grp, sd), na.rm = TRUE)
    rows[[length(rows) + 1L]] <- data.frame(
      ruc2021 = ruc,
      n = nrow(z),
      measure = nm,
      overall_sd = overall_sd,
      mean_within_ruc_distance_decile_sd = within_sd,
      within_to_overall_sd_ratio = within_sd / overall_sd,
      stringsAsFactors = FALSE
    )
  }
}

out <- do.call(rbind, rows)
write.csv(out,
          file.path(ROOT, "results", "f5", "ruc_conditioned_same_distance_dispersion.csv"),
          row.names = FALSE)

cat("RUC_CONDITIONED_DISPERSION_OK rows=", nrow(out), "\n", sep = "")

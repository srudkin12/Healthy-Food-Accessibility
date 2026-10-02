source("R/functions.R")

ROOT <- root_dir()
d <- read.csv(file.path(ROOT, "data_staged", "hfa_f4_capability_dataset.csv"),
              check.names = FALSE)

dist_hi <- quartile_flag(d$geolytix_large_nearest_km, "high")
dist_lo <- quartile_flag(d$geolytix_large_nearest_km, "low")
car_hi <- quartile_flag(d$car_none_pct, "high")
car_lo <- quartile_flag(d$car_none_pct, "low")
inc_hi <- quartile_flag(d$income_deprivation_score_2025, "high")
inc_lo <- quartile_flag(d$income_deprivation_score_2025, "low")

d$flag_resource_buffered_distance <- dist_hi & car_lo & inc_lo
d$flag_proximity_capability_constrained <- dist_lo & car_hi & inc_hi
d$flag_compound_physical_capability_disadvantage <- dist_hi & car_hi & inc_hi

flags <- c(
  "flag_resource_buffered_distance",
  "flag_proximity_capability_constrained",
  "flag_compound_physical_capability_disadvantage"
)

flag_summary <- do.call(rbind, lapply(flags, function(f) {
  z <- d[d[[f]], ]
  data.frame(
    flag = f,
    n = nrow(z),
    share = nrow(z) / nrow(d),
    mean_distance_km = if (nrow(z)) mean(z$geolytix_large_nearest_km) else NA_real_,
    mean_car_none_pct = if (nrow(z)) mean(z$car_none_pct) else NA_real_,
    mean_income_deprivation_score = if (nrow(z)) mean(z$income_deprivation_score_2025) else NA_real_,
    stringsAsFactors = FALSE
  )
}))
write.csv(flag_summary,
          file.path(ROOT, "results", "f4", "candidate_configuration_flags.csv"),
          row.names = FALSE)

# Distribution across RUC classes: if a candidate configuration is entirely one RUC class,
# the student story may be reducible to rural/urban structure.
ruc_rows <- list()
for (f in flags) {
  tt <- table(d$ruc2021[d[[f]]])
  if (length(tt)) {
    ruc_rows[[length(ruc_rows) + 1L]] <- data.frame(
      flag = f,
      ruc2021 = names(tt),
      n = as.integer(tt),
      share_within_flag = as.integer(tt) / sum(tt),
      stringsAsFactors = FALSE
    )
  }
}
ruc_out <- if (length(ruc_rows)) do.call(rbind, ruc_rows) else data.frame()
write.csv(ruc_out,
          file.path(ROOT, "results", "f4", "candidate_configuration_by_ruc.csv"),
          row.names = FALSE)

# TCM mode summaries for the diagnostic configurations.
tcm_cols <- grep("^tcm2025_.*shopping_(walking|cycling|public_transport|driving|overall)$",
                 names(d), value = TRUE)

mode_rows <- list()
for (f in flags) {
  z <- d[d[[f]], , drop = FALSE]
  for (nm in tcm_cols) {
    v <- suppressWarnings(as.numeric(z[[nm]]))
    mode_rows[[length(mode_rows) + 1L]] <- data.frame(
      flag = f,
      tcm_measure = nm,
      n_complete = sum(is.finite(v)),
      mean = if (any(is.finite(v))) mean(v, na.rm = TRUE) else NA_real_,
      median = if (any(is.finite(v))) median(v, na.rm = TRUE) else NA_real_,
      stringsAsFactors = FALSE
    )
  }
}
write.csv(do.call(rbind, mode_rows),
          file.path(ROOT, "results", "f4", "configuration_tcm_mode_profiles.csv"),
          row.names = FALSE)

# Conventional linear closure: how much of physical distance is explained by RUC alone
# and how much capability covariates add? Descriptive, not causal.
m_ruc <- lm(log_large_nearest_km ~ factor(ruc2021), data = d)
m_full <- lm(log_large_nearest_km ~ factor(ruc2021) +
               z_car_none + z_income_deprivation +
               z_car_none:z_income_deprivation, data = d)

model_qa <- data.frame(
  model = c("ruc_only", "ruc_plus_capability_interaction"),
  adjusted_r2 = c(summary(m_ruc)$adj.r.squared, summary(m_full)$adj.r.squared),
  AIC = c(AIC(m_ruc), AIC(m_full)),
  stringsAsFactors = FALSE
)
write.csv(model_qa,
          file.path(ROOT, "results", "f4", "physical_distance_linear_closure.csv"),
          row.names = FALSE)

coefs <- as.data.frame(summary(m_full)$coefficients)
coefs$term <- rownames(coefs)
rownames(coefs) <- NULL
write.csv(coefs,
          file.path(ROOT, "results", "f4", "physical_distance_full_model_coefficients.csv"),
          row.names = FALSE)

write.csv(d[, c("lsoa21cd", "ruc2021", flags)],
          file.path(ROOT, "results", "f4", "candidate_configuration_assignments.csv"),
          row.names = FALSE)

cat("CONFIGURATION_KILL_TESTS_OK buffered=",
    sum(d$flag_resource_buffered_distance),
    " proximity_constrained=", sum(d$flag_proximity_capability_constrained),
    " compound=", sum(d$flag_compound_physical_capability_disadvantage),
    "\n", sep = "")

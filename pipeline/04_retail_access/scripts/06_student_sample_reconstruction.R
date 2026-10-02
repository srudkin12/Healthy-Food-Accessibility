source("R/functions.R")

ROOT <- root_dir()

stores <- read.csv(
  file.path(ROOT, "data_staged", "geolytix_q1_2023_with_lsoa21.csv"),
  check.names = FALSE
)
fa <- read.csv(
  file.path(ROOT, "data_staged", "f3_geolytix_functional_access.csv"),
  check.names = FALSE
)

codes <- fa$lsoa21cd

count_by_code <- function(x) {
  tab <- table(x[!is.na(x) & nzchar(x)])
  out <- integer(length(codes))
  m <- match(names(tab), codes)
  ok <- !is.na(m)
  out[m[ok]] <- as.integer(tab[ok])
  out
}

internal_any <- count_by_code(stores$internal_lsoa21cd)
internal_large <- count_by_code(
  ifelse(stores$large_store_student_rule, stores$internal_lsoa21cd, NA_character_)
)

served <- internal_large > 0L

recon <- fa
recon$geolytix_internal_any_count <- internal_any
recon$geolytix_internal_large_count <- internal_large
recon$student_served_rule_reconstructed <- served

n_served <- sum(served)
n_unserved <- sum(!served)
target1 <- 3814L
target2 <- 3813L

sum_large_assigned <- sum(
  stores$large_store_student_rule & !is.na(stores$internal_lsoa21cd),
  na.rm = TRUE
)
assert_true(sum(internal_large) == sum_large_assigned,
            "Internal large-store count does not reconcile to assigned store points")

summary <- data.frame(
  metric = c(
    "english_lsoa21_total",
    "lsoa_with_internal_large_store",
    "lsoa_without_internal_large_store",
    "share_with_internal_large_store",
    "mean_internal_large_store_count_full_population",
    "sd_internal_large_store_count_full_population",
    "min_internal_large_store_count_full_population",
    "max_internal_large_store_count_full_population",
    "large_store_points_assigned_to_english_lsoa21",
    "student_report_main_text_served_lsoa_target",
    "difference_from_3814",
    "student_report_robustness_served_lsoa_target",
    "difference_from_3813"
  ),
  value = c(
    length(codes),
    n_served,
    n_unserved,
    mean(served),
    mean(internal_large),
    stats::sd(internal_large),
    min(internal_large),
    max(internal_large),
    sum_large_assigned,
    target1,
    n_served - target1,
    target2,
    n_served - target2
  ),
  stringsAsFactors = FALSE
)

write.csv(summary,
          file.path(ROOT, "results", "f3", "student_sample_reconstruction_summary.csv"),
          row.names = FALSE)

d <- recon$geolytix_large_nearest_m

discord <- data.frame(
  group = c(
    rep("no_internal_large_store", 4),
    rep("has_internal_large_store", 3)
  ),
  condition = c(
    "nearest_large_within_500m",
    "nearest_large_within_1000m",
    "nearest_large_within_2000m",
    "nearest_large_within_5000m",
    "pwc_nearest_large_farther_than_1000m",
    "pwc_nearest_large_farther_than_2000m",
    "pwc_nearest_large_farther_than_5000m"
  ),
  n_lsoa = c(
    sum(!served & d <= 500),
    sum(!served & d <= 1000),
    sum(!served & d <= 2000),
    sum(!served & d <= 5000),
    sum(served & d > 1000),
    sum(served & d > 2000),
    sum(served & d > 5000)
  ),
  denominator = c(
    rep(n_unserved, 4),
    rep(n_served, 3)
  ),
  stringsAsFactors = FALSE
)
discord$share <- discord$n_lsoa / discord$denominator

write.csv(discord,
          file.path(ROOT, "results", "f3", "boundary_functional_discordance.csv"),
          row.names = FALSE)

write.csv(recon,
          file.path(ROOT, "data_staged", "f3_student_sample_reconstruction.csv"),
          row.names = FALSE)

cat("STUDENT_SAMPLE_RECONSTRUCTION_OK served_lsoas=", n_served,
    " diff_from_3814=", n_served - target1,
    " mean_count=", sprintf("%.4f", mean(internal_large)),
    " max_count=", max(internal_large), "\n", sep = "")

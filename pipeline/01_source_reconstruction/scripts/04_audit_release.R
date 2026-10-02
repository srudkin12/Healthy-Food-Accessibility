source(file.path(Sys.getenv("HFA_ROOT"), "R", "functions.R"))
ensure_dirs()

spine <- readr::read_csv(p("data_staged", "lsoa21_spine.csv"), show_col_types = FALSE)
base <- readr::read_csv(p("data_staged", "hfa_f0f1_baseline.csv"), show_col_types = FALSE)
src <- readr::read_csv(p("config", "source_register_seed.csv"), show_col_types = FALSE)
student_audit <- readr::read_csv(p("config", "student_audit_targets.csv"), show_col_types = FALSE)

qa <- tibble::tribble(
  ~check, ~value, ~expected, ~pass,
  "lsoa21_rows", nrow(spine), 33755, nrow(spine) == 33755,
  "lsoa21_unique_codes", dplyr::n_distinct(spine$lsoa21cd), 33755, dplyr::n_distinct(spine$lsoa21cd) == 33755,
  "baseline_rows", nrow(base), 33755, nrow(base) == 33755,
  "missing_car_total", sum(is.na(base$households_total)), 0, sum(is.na(base$households_total)) == 0,
  "missing_deprivation_total", sum(is.na(base$deprivation_households_total)), 0, sum(is.na(base$deprivation_households_total)) == 0,
  "non_english_lsoa_codes", sum(!grepl("^E[0-9]{8}$", base$lsoa21cd)), 0, sum(!grepl("^E[0-9]{8}$", base$lsoa21cd)) == 0,
  "duplicate_lsoa_codes", nrow(base) - dplyr::n_distinct(base$lsoa21cd), 0, nrow(base) == dplyr::n_distinct(base$lsoa21cd)
)
write_csv_atomic(qa, p("results", "f0f1", "qa_summary.csv"))

# Lightweight descriptive checks useful for comparison with the student report.
desc_vars <- c("car_none_pct", "car_1_pct", "car_2_pct", "car_3plus_pct",
               "deprived_1_pct", "deprived_2_pct", "deprived_3_pct", "deprived_4_pct")
desc <- purrr::map_dfr(desc_vars, function(v) {
  x <- base[[v]]
  tibble::tibble(
    variable = v,
    n = sum(!is.na(x)),
    mean = mean(x, na.rm = TRUE),
    sd = stats::sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    p25 = as.numeric(stats::quantile(x, .25, na.rm = TRUE)),
    median = stats::median(x, na.rm = TRUE),
    p75 = as.numeric(stats::quantile(x, .75, na.rm = TRUE)),
    max = max(x, na.rm = TRUE)
  )
})
write_csv_atomic(desc, p("results", "f0f1", "baseline_descriptives.csv"))

# Source register augmented with local file state when applicable.
raw_map <- c(
  NOMIS_TS045 = p("data_raw", "nomis", "census2021-ts045.zip"),
  NOMIS_TS011 = p("data_raw", "nomis", "census2021-ts011.zip"),
  DFT_JTS0507 = p("data_raw", "dft", "jts0507_food_stores_2019.ods"),
  DFT_TCM_2025 = p("data_raw", "dft", "connectivity_metrics_2025.ods")
)
src$local_file <- unname(raw_map[src$source_id])
src$local_bytes <- ifelse(!is.na(src$local_file) & file.exists(src$local_file), file.info(src$local_file)$size, NA_real_)
src$local_sha256 <- vapply(src$local_file, function(f) {
  if (is.na(f) || !file.exists(f)) return(NA_character_)
  sha256_file(f)
}, character(1))
write_csv_atomic(src, p("results", "f0f1", "source_register.csv"))
write_csv_atomic(student_audit, p("results", "f0f1", "student_audit_targets.csv"))

manifest_paths <- c(
  list.files(p("data_raw"), recursive = TRUE, full.names = TRUE),
  list.files(p("data_staged"), recursive = TRUE, full.names = TRUE),
  list.files(p("config"), recursive = TRUE, full.names = TRUE)
)
manifest <- file_manifest(manifest_paths)
write_csv_atomic(manifest, p("results", "f0f1", "file_manifest_sha256.csv"))

pass <- all(qa$pass)
report <- c(
  "FOOD DESERTS / HEALTHY FOOD ACCESS — HFA-F0/F1 RELEASE AUDIT",
  paste0("timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste0("project_root: ", hfa_root()),
  "",
  paste0("lsoa21 rows: ", nrow(spine)),
  paste0("baseline rows: ", nrow(base)),
  paste0("core QA: ", if (pass) "PASS" else "FAIL"),
  "",
  "Guardrails:",
  "- publication geography = England LSOA21",
  "- no served-only supermarket filter applied",
  "- DfT 2019 source staged but not merged across LSOA vintages",
  "- TCM 2025 staged as benchmark only",
  "- Geolytix and IUC ingestion deferred to construct/provenance closure",
  "",
  if (pass) "PASS_FOR_HFA_F2_ACCESS_CONSTRUCT" else "STOP_BEFORE_HFA_F2"
)
writeLines(report, p("results", "f0f1", "AUDIT_F0F1.txt"))
cat(paste(report, collapse = "\n"), "\n")
if (!pass) stop("HFA-F0/F1 audit failed; inspect results/f0f1/qa_summary.csv", call. = FALSE)

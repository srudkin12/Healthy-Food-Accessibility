source("R/functions.R")

ROOT <- root_dir()

base <- read.csv(file.path(ROOT, "data_staged", "hfa_f4_inherited.csv"),
                 check.names = FALSE)
iuc <- read.csv(file.path(ROOT, "data_staged", "iuc2018_lsoa21_strict.csv"),
                check.names = FALSE)
flags <- read.csv(file.path(ROOT, "data_staged", "f4_candidate_configuration_assignments.csv"),
                  check.names = FALSE)
km <- read.csv(file.path(ROOT, "data_staged", "f4_kmeans_best_assignments.csv"),
               check.names = FALSE)

m <- match(base$lsoa21cd, iuc$lsoa21cd)
assert_true(all(!is.na(m)), "IUC LSOA21 spine mismatch")
base$iuc_group_label <- iuc$iuc_group_label[m]
base$iuc_transfer_status <- iuc$iuc_transfer_status[m]

mf <- match(base$lsoa21cd, flags$lsoa21cd)
mk <- match(base$lsoa21cd, km$lsoa21cd)
assert_true(all(!is.na(mf)) && all(!is.na(mk)), "F4 diagnostic assignment mismatch")

flag_names <- grep("^flag_", names(flags), value = TRUE)
for (nm in flag_names) base[[nm]] <- flags[[nm]][mf]
base$kmeans_killtest_cluster <- km$kmeans_killtest_cluster[mk]

valid <- !is.na(base$iuc_group_label)

# IUC by candidate configuration.
rows <- list()
for (fl in flag_names) {
  z <- base[valid & base[[fl]], ]
  tt <- sort(table(z$iuc_group_label), decreasing = TRUE)
  if (length(tt)) {
    rows[[length(rows)+1L]] <- data.frame(
      configuration = fl,
      iuc_group_label = names(tt),
      n = as.integer(tt),
      share_within_configuration = as.integer(tt)/sum(tt),
      entropy_normalised = entropy_normalised(z$iuc_group_label),
      stringsAsFactors = FALSE
    )
  }
}
iuc_flags <- do.call(rbind, rows)
write.csv(iuc_flags,
          file.path(ROOT, "results", "f5", "iuc_by_f4_configuration.csv"),
          row.names = FALSE)

# IUC by kmeans cluster.
rows2 <- list()
for (cl in sort(unique(base$kmeans_killtest_cluster))) {
  z <- base[valid & base$kmeans_killtest_cluster == cl, ]
  tt <- sort(table(z$iuc_group_label), decreasing = TRUE)
  rows2[[length(rows2)+1L]] <- data.frame(
    kmeans_cluster = cl,
    iuc_group_label = names(tt),
    n = as.integer(tt),
    share_within_cluster = as.integer(tt)/sum(tt),
    entropy_normalised = entropy_normalised(z$iuc_group_label),
    stringsAsFactors = FALSE
  )
}
iuc_km <- do.call(rbind, rows2)
write.csv(iuc_km,
          file.path(ROOT, "results", "f5", "iuc_by_kmeans_cluster.csv"),
          row.names = FALSE)

assoc <- data.frame(
  relationship = c(
    "IUC_vs_RUC",
    "IUC_vs_kmeans3D",
    paste0("IUC_vs_", flag_names)
  ),
  cramers_v = c(
    cramers_v(base$iuc_group_label, base$ruc2021),
    cramers_v(base$iuc_group_label, base$kmeans_killtest_cluster),
    vapply(flag_names, function(fl)
      cramers_v(base$iuc_group_label, base[[fl]]), numeric(1))
  ),
  n_complete = c(
    sum(valid),
    sum(valid & !is.na(base$kmeans_killtest_cluster)),
    vapply(flag_names, function(fl)
      sum(valid & !is.na(base[[fl]])), integer(1))
  ),
  stringsAsFactors = FALSE
)
write.csv(assoc,
          file.path(ROOT, "results", "f5", "iuc_association_strength.csv"),
          row.names = FALSE)

# Optional cluster-centre audit: identify a uniquely named grocery/online-shop variable.
cc_path <- file.path(ROOT, "data_raw", "iuc", "iuc2018_cluster_centres.csv")
audit <- data.frame(
  status = "NO_CLUSTER_CENTRES_AVAILABLE",
  candidate_field = NA_character_,
  n_candidates = 0L,
  stringsAsFactors = FALSE
)

if (file.exists(cc_path) && file.info(cc_path)$size > 1000) {
  cc <- read.csv(cc_path, check.names = FALSE)
  nn <- normalise_name(names(cc))
  cand <- which(
    grepl("groc", nn) |
    grepl("food.*online|online.*food", nn) |
    grepl("shop.*online|online.*shop", nn)
  )
  audit <- data.frame(
    status = if (length(cand) == 1L)
      "UNIQUE_CONTINUOUS_DIGITAL_CANDIDATE_FOUND"
    else if (length(cand) == 0L)
      "NO_CONTINUOUS_DIGITAL_CANDIDATE_FOUND"
    else
      "MULTIPLE_CONTINUOUS_DIGITAL_CANDIDATES_FOUND",
    candidate_field = if (length(cand)) paste(names(cc)[cand], collapse = " | ") else NA_character_,
    n_candidates = length(cand),
    stringsAsFactors = FALSE
  )
}
write.csv(audit,
          file.path(ROOT, "results", "f5", "digital_continuous_candidate_audit.csv"),
          row.names = FALSE)

write.csv(base,
          file.path(ROOT, "data_staged", "hfa_f5_digital_readout_dataset.csv"),
          row.names = FALSE)

cat("DIGITAL_READOUT_CLOSURE_OK iuc_complete=", sum(valid),
    " iuc_groups=", length(unique(na.omit(base$iuc_group_label))),
    " optional_continuous_status=", audit$status[1], "\n", sep = "")

source("R/functions.R")

ROOT <- root_dir()

d <- read.csv(
  file.path(ROOT, "data_staged", "hfa_f3_functional_retail_access.csv"),
  check.names = FALSE
)
sr <- read.csv(
  file.path(ROOT, "results", "f3", "student_sample_reconstruction_summary.csv"),
  check.names = FALSE
)
disc <- read.csv(
  file.path(ROOT, "results", "f3", "boundary_functional_discordance.csv"),
  check.names = FALSE
)
assign <- read.csv(
  file.path(ROOT, "results", "f3", "store_to_lsoa_assignment_qa.csv"),
  check.names = FALSE
)

get_metric <- function(tbl, nm) {
  z <- tbl$value[tbl$metric == nm]
  if (!length(z)) NA_real_ else as.numeric(z[1])
}

n_served <- get_metric(sr, "lsoa_with_internal_large_store")
diff3814 <- get_metric(sr, "difference_from_3814")
mean_count <- get_metric(sr, "mean_internal_large_store_count_full_population")
max_count <- get_metric(sr, "max_internal_large_store_count_full_population")

near2 <- disc$n_lsoa[
  disc$group == "no_internal_large_store" &
    disc$condition == "nearest_large_within_2000m"
]
near2_share <- disc$share[
  disc$group == "no_internal_large_store" &
    disc$condition == "nearest_large_within_2000m"
]

assert_true(nrow(d) == 33755L, "Release audit: wrong row count")
assert_true(length(unique(d$lsoa21cd)) == 33755L, "Release audit: duplicate LSOAs")
assert_true(all(!d$student_served_rule_reconstructed |
                  d$geolytix_internal_large_count > 0),
            "Release audit: served flag inconsistent")
assert_true(all(is.na(d$jts2019_public_transport_min_validated)),
            "Release audit: validated JTS2019 PT should remain excluded/NA")

audit <- c(
  "FOOD DESERTS / HEALTHY FOOD ACCESS — HFA-F3 RELEASE AUDIT",
  paste0("timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste0("rows: ", nrow(d)),
  paste0("unique LSOA21: ", length(unique(d$lsoa21cd))),
  "",
  "Construct boundary:",
  "- no served-only filter is applied to the publication population;",
  "- Geolytix internal-store presence is reconstructed only as a historical/student comparator;",
  "- functional proximity uses ONS LSOA21 population-weighted centroids;",
  "- distances are straight-line British National Grid distances, not network travel times;",
  "- functional nearest/catchment measures may cross England's administrative border;",
  "- internal-LSOA counts use English ONS LSOA21 boundaries;",
  "- units under 280 m² are excluded only for the reconstructed student 'large store' rule;",
  "- JTS2019 public-transport minimum remains excluded under the F2A source-conflict contract;",
  "- TDABM remains deferred.",
  "",
  "Student sample reconstruction:",
  paste0("- reconstructed LSOAs with >=1 internal >=280m2 store: ", n_served),
  paste0("- difference from main-text target 3814: ", diff3814),
  paste0("- full-population mean internal large-store count: ", sprintf("%.6f", mean_count)),
  paste0("- full-population maximum internal large-store count: ", max_count),
  "",
  "Boundary-versus-functional diagnostic:",
  paste0("- LSOAs with no internal large store but nearest >=280m2 store within 2 km: ",
         near2, " (", sprintf("%.3f", near2_share), " of internally-unserved LSOAs)"),
  "",
  "Source/provenance note:",
  "- the Q1 2023 public Feature Service is treated as a reconstruction source;",
  "- closeness to the student's 3814/3813 counts is evidence about version compatibility, not proof of byte-identical source provenance;",
  "- raw source payloads are not included in the handback; checksums and URLs are retained.",
  "",
  "PASS_FOR_HFA_F4_CAPABILITY_AND_EMPIRICAL_KILL_TESTS",
  "NEXT_STAGE=F4_CAPABILITY_DIGITAL_AND_PRE_TDABM_CLOSURE"
)

writeLines(audit, file.path(ROOT, "results", "f3", "AUDIT_F3.txt"))
cat(paste(audit, collapse = "\n"), "\n")

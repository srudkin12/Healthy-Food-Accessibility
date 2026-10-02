source("R/functions.R")

ROOT <- root_dir()

d <- read.csv(file.path(ROOT, "data_staged", "hfa_f4_capability_dataset.csv"),
              check.names = FALSE)
peer <- read.csv(file.path(ROOT, "results", "f4", "scalar_vs_capability_peer_overlap.csv"))
disp <- read.csv(file.path(ROOT, "results", "f4", "same_distance_dispersion_ratio.csv"))
km <- read.csv(file.path(ROOT, "results", "f4", "kmeans_k2_k8_benchmark.csv"))
flags <- read.csv(file.path(ROOT, "results", "f4", "candidate_configuration_flags.csv"))
lin <- read.csv(file.path(ROOT, "results", "f4", "physical_distance_linear_closure.csv"))

assert_true(nrow(d) == 33755L, "F4 release row count wrong")
assert_true(length(unique(d$lsoa21cd)) == 33755L, "F4 release duplicate LSOAs")
assert_true(all(is.finite(d$income_deprivation_score_2025)), "F4 income missing")
assert_true(all(!is.na(d$ruc2021)), "F4 RUC missing")

k25 <- peer[peer$k == 25, ]
bestk <- km$k[which.max(km$calinski_harabasz)]
r2_ruc <- lin$adjusted_r2[lin$model == "ruc_only"]
r2_full <- lin$adjusted_r2[lin$model == "ruc_plus_capability_interaction"]

audit <- c(
  "FOOD DESERTS / HEALTHY FOOD ACCESS — HFA-F4 RELEASE AUDIT",
  paste0("timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste0("rows: ", nrow(d)),
  paste0("unique LSOA21: ", length(unique(d$lsoa21cd))),
  "",
  "Construct boundary:",
  "- physical opportunity remains separate from household capability;",
  "- primary physical friction = F3 Geolytix >=280m2 nearest-store distance;",
  "- mobility capability = Census 2021 no-car percentage;",
  "- material capability = IoD2025 Income Deprivation score;",
  "- ONS 2021 RUC is a geography closure variable, not a definition of the regimes;",
  "- JTS2019 public transport remains excluded;",
  "- digital capability remains deferred to F5;",
  "- no scalar effective-access index is constructed;",
  "- no TDABM is run.",
  "",
  "Key falsification diagnostics:",
  paste0("- k=25 scalar-distance vs 3D capability-space mean Jaccard: ",
         sprintf("%.6f", k25$mean_jaccard)),
  paste0("- k=25 median Jaccard: ", sprintf("%.6f", k25$median_jaccard)),
  paste0("- k=25 zero-overlap share: ", sprintf("%.6f", k25$share_zero_jaccard)),
  paste0("- k-means CH-selected k (diagnostic only): ", bestk),
  paste0("- RUC-only adjusted R2 for log nearest-store distance: ", sprintf("%.6f", r2_ruc)),
  paste0("- RUC + capability interaction adjusted R2: ", sprintf("%.6f", r2_full)),
  "",
  "Same-distance dispersion ratios:",
  paste0("- ", disp$measure, ": ",
         sprintf("%.6f", disp$within_to_overall_sd_ratio),
         collapse = "\n"),
  "",
  "Candidate configuration counts (quartile kill-test flags; NOT publication typology):",
  paste0("- ", flags$flag, ": n=", flags$n, " share=",
         sprintf("%.6f", flags$share), collapse = "\n"),
  "",
  "Interpretation gate:",
  "- F4 is supportive only if capability heterogeneity remains substantial within similar-distance strata;",
  "- low scalar-vs-capability neighbour overlap supports a multidimensional structural-peer problem;",
  "- candidate configurations must not be treated as final regimes;",
  "- RUC concentration is explicitly reported so an urban/rural explanation can be assessed before TDABM.",
  "",
  "PASS_FOR_HFA_F5_DIGITAL_CLOSURE",
  "NEXT_STAGE=F5_DIGITAL_CAPABILITY_AND_FINAL_PRE_TDABM_CLOSURE"
)

writeLines(audit, file.path(ROOT, "results", "f4", "AUDIT_F4.txt"))
cat(paste(audit, collapse = "\n"), "\n")

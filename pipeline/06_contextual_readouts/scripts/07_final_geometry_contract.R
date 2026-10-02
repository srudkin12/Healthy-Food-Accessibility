source("R/functions.R")

ROOT <- root_dir()

peer <- read.csv(file.path(ROOT, "results", "f5", "ruc_conditioned_peer_overlap.csv"),
                 check.names = FALSE)
disp <- read.csv(file.path(ROOT, "results", "f5", "ruc_conditioned_same_distance_dispersion.csv"),
                 check.names = FALSE)
iucqa <- read.csv(file.path(ROOT, "results", "f5", "iuc_crosswalk_qa.csv"),
                  check.names = FALSE)
assoc <- read.csv(file.path(ROOT, "results", "f5", "iuc_association_strength.csv"),
                  check.names = FALSE)
dc <- read.csv(file.path(ROOT, "results", "f5", "digital_continuous_candidate_audit.csv"),
               check.names = FALSE)

agg25 <- peer[peer$ruc2021 == "WEIGHTED_ACROSS_RUC_CLASSES" &
                peer$k_requested == 25, ]

# Conservative decision rule: structural peer divergence is considered to survive
# RUC conditioning when weighted mean overlap remains below 0.10.
survives_ruc <- nrow(agg25) == 1L && is.finite(agg25$mean_jaccard) &&
                agg25$mean_jaccard < 0.10

contract <- c(
  "HFA-F5 FINAL PRE-TDABM GEOMETRY CONTRACT",
  "",
  paste0("RUC-conditioned scalar-vs-capability k25 weighted mean Jaccard: ",
         sprintf("%.6f", agg25$mean_jaccard)),
  paste0("RUC-conditioned divergence survives: ", survives_ruc),
  "",
  "PRIMARY TDABM AXES FOR F6 (if radius/order tests support them):",
  "1. log1p nearest >=280m2 grocery-store distance (physical opportunity/friction)",
  "2. Census 2021 household no-car percentage (mobility capability)",
  "3. IoD2025 Income Score (material capability)",
  "",
  "READOUTS / RECOLOURINGS, NOT PRIMARY AXES:",
  "- ONS 2021 Rural–Urban Classification",
  "- IUC 2018 categorical group (strict one-to-one LSOA11→LSOA21 transfers only)",
  "- DfT 2025 TCM shopping mode scores",
  "- F3 internal-store presence/count",
  "- F4 k-means diagnostic cluster",
  "- F4 quartile configuration flags",
  "",
  "EXCLUSIONS:",
  "- JTS2019 public-transport minimum: excluded source conflict",
  "- ordinal IUC 1–10 coding: prohibited",
  "- student served-only filter: prohibited",
  "- omnibus effective-access scalar: not constructed",
  "",
  paste0("IUC continuous grocery-propensity source status: ", dc$status[1]),
  "",
  "F6 MUST TEST:",
  "- radius selection without visual tuning to a preferred story;",
  "- input-order sensitivity / deterministic ordering policy;",
  "- topology stability across defensible radius neighbourhood;",
  "- whether TDABM adds overlapping/local configuration information beyond k-means and RUC;",
  "- recolouring with digital category and TCM mode scores.",
  "",
  if (survives_ruc)
    "DECISION=PROCEED_TO_TDABM_RADIUS_AND_ORDERING"
  else
    "DECISION=HOLD_TDABM_AND_REASSESS_GEOGRAPHIC_REDUCTION"
)

writeLines(contract,
           file.path(ROOT, "results", "f5", "FINAL_PRE_TDABM_GEOMETRY_CONTRACT.txt"))

cat("FINAL_GEOMETRY_CONTRACT_OK decision=",
    if (survives_ruc) "PROCEED" else "HOLD", "\n", sep = "")

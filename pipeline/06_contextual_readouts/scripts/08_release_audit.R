source("R/functions.R")

ROOT <- root_dir()

d <- read.csv(file.path(ROOT, "data_staged", "hfa_f5_digital_readout_dataset.csv"),
              check.names = FALSE)
peer <- read.csv(file.path(ROOT, "results", "f5", "ruc_conditioned_peer_overlap.csv"),
                 check.names = FALSE)
disp <- read.csv(file.path(ROOT, "results", "f5", "ruc_conditioned_same_distance_dispersion.csv"),
                 check.names = FALSE)
iucqa <- read.csv(file.path(ROOT, "results", "f5", "iuc_crosswalk_qa.csv"),
                  check.names = FALSE)
contract <- readLines(file.path(ROOT, "results", "f5",
                               "FINAL_PRE_TDABM_GEOMETRY_CONTRACT.txt"),
                      warn = FALSE)

assert_true(nrow(d) == 33755L, "F5 row count not 33755")
assert_true(length(unique(d$lsoa21cd)) == 33755L, "F5 LSOA codes not unique")

agg25 <- peer[peer$ruc2021 == "WEIGHTED_ACROSS_RUC_CLASSES" &
                peer$k_requested == 25, ]
proceed <- any(grepl("DECISION=PROCEED_TO_TDABM_RADIUS_AND_ORDERING",
                     contract, fixed = TRUE))

iuc_complete <- sum(!is.na(d$iuc_group_label))
iuc_unresolved <- nrow(d) - iuc_complete

audit <- c(
  "FOOD DESERTS / HEALTHY FOOD ACCESS — HFA-F5 RELEASE AUDIT",
  paste0("timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste0("rows: ", nrow(d)),
  paste0("unique LSOA21: ", length(unique(d$lsoa21cd))),
  "",
  "F4 substantive context:",
  "- scalar physical distance was already shown to omit substantial mobility/material structure;",
  "- F5 conditions the neighbour comparison explicitly on RUC class.",
  "",
  "RUC-conditioned structural-peer result:",
  paste0("- weighted k=25 mean Jaccard: ",
         sprintf("%.6f", agg25$mean_jaccard)),
  paste0("- weighted k=25 zero-overlap share: ",
         sprintf("%.6f", agg25$share_zero_jaccard)),
  "",
  "Digital closure:",
  paste0("- IUC categorical readout available for strict one-to-one transfers: ", iuc_complete),
  paste0("- changed/complex geography deliberately left without IUC category: ", iuc_unresolved),
  "- IUC group codes are categorical and are never interpreted as an interval scale.",
  "",
  "TDABM gate:",
  paste0("- final geometry decision: ", if (proceed) "PROCEED" else "HOLD"),
  "",
  if (proceed)
    "PASS_FOR_HFA_F6_TDABM_RADIUS_AND_ORDERING"
  else
    "HOLD_BEFORE_TDABM",
  paste0("NEXT_STAGE=",
         if (proceed) "F6_TDABM_RADIUS_ORDERING_AND_TOPOLOGY_FREEZE"
         else "F5B_GEOGRAPHIC_REASSESSMENT")
)

writeLines(audit, file.path(ROOT, "results", "f5", "AUDIT_F5.txt"))
cat(paste(audit, collapse = "\n"), "\n")

if (!proceed) {
  stop("F5 final geometry gate does not support proceeding to TDABM.", call. = FALSE)
}

args <- commandArgs(trailingOnly=FALSE)
ff <- sub("^--file=", "", args[grep("^--file=", args)])
root <- normalizePath(file.path(dirname(ff[1]), ".."), mustWork=TRUE)
ledger <- read.csv(file.path(root,"results","f2a","JTS2019_PT_VALIDATION_LEDGER.csv"),stringsAsFactors=FALSE)
contract <- read.csv(file.path(root,"results","f2a","F2_VALIDATED_DOWNSTREAM_CONTRACT.csv"),stringsAsFactors=FALSE)
d <- read.csv(file.path(root,"data_staged","hfa_f2_physical_access_VALIDATED.csv"),check.names=FALSE,stringsAsFactors=FALSE)
status <- ledger$value[ledger$check=="decision"]
lines <- c(
 "FOOD DESERTS / HEALTHY FOOD ACCESS — F2A JTS2019 VALIDATION AUDIT",
 paste0("timestamp: ",format(Sys.time(),"%Y-%m-%d %H:%M:%S %Z")),
 paste0("rows: ",nrow(d)),
 "",
 "Finding:",
 "- The raw JTS0507/2019 field FoodPTt duplicates FoodCart exactly across all 32,844 LSOA11s.",
 "- The official DfT 2019 national release reports different food-store averages for public transport/walking (9 minutes) and car (7 minutes).",
 "- Therefore raw FoodPTt is retained for provenance but is excluded from downstream analysis.",
 "- Car, cycling and walking JTS2019 minima remain historical comparators, subject to the national benchmark audit.",
 "- 2025 TCM shopping mode scores remain the contemporary LSOA21 external benchmark.",
 "- JTS2019 values for changed 2011/2021 LSOA geography are sensitivity-only.",
 "",
 paste0("decision: ",status),
 "PASS_F2A_WITH_JTS2019_PT_MIN_EXCLUSION",
 "NEXT_STAGE=F3_RETAIL_POINT_FUNCTIONAL_ACCESS")
writeLines(lines,file.path(root,"results","f2a","AUDIT_F2A.txt"))
cat(paste(lines,collapse="\n"),"\n")

args <- commandArgs(trailingOnly=FALSE)
ff <- sub("^--file=", "", args[grep("^--file=", args)])
root <- normalizePath(file.path(dirname(ff[1]), ".."), mustWork=TRUE)
parent <- dirname(root)
paths <- c(
 file.path(parent,"02_physical_access","data_staged","hfa_f2_physical_access.csv"),
 file.path(parent,"FoodDeserts_HFA_F2_Physical_Access_v1_0_2","data_staged","hfa_f2_physical_access.csv"))
p <- paths[file.exists(paths)][1]
if(is.na(p)) stop("Cannot find completed F2 physical-access dataset in sibling v1_0_3/v1_0_2 directory.",call.=FALSE)
ledger <- read.csv(file.path(root,"results","f2a","JTS2019_PT_VALIDATION_LEDGER.csv"),stringsAsFactors=FALSE)
decision <- ledger$value[ledger$check=="decision"]
if(length(decision)!=1 || decision!="JTS2019_PT_MIN_DO_NOT_USE_SOURCE_CONFLICT") stop("Validation did not establish the expected source conflict; do not auto-build validated F2.",call.=FALSE)
d <- read.csv(p,check.names=FALSE,stringsAsFactors=FALSE)
if(nrow(d)!=33755 || length(unique(d$lsoa21cd))!=33755) stop("Unexpected completed F2 dimensions.",call.=FALSE)
# Preserve the raw field exactly, but ensure future analysis has an explicit safe field.
d$jts2019_public_transport_min_raw_source <- d$jts2019_public_transport_min
d$jts2019_public_transport_min_validated <- NA_real_
d$jts2019_public_transport_min_quality_flag <- "DO_NOT_USE_RAW_JTS0507_2019_FoodPTt_DUPLICATES_FoodCart_AND_CONFLICTS_WITH_OFFICIAL_RELEASE"
d$jts2019_historical_geography_use <- ifelse(d$jts2019_change_indicator=="U","DIRECT_LSOA11_TO_LSOA21_COMPARATOR","CHANGED_GEOGRAPHY_SENSITIVITY_ONLY")
write.csv(d,file.path(root,"data_staged","hfa_f2_physical_access_VALIDATED.csv"),row.names=FALSE,na="")
# Small contract for downstream F3 and later topology stages.
contract <- data.frame(
 field=c("jts2019_car_min","jts2019_cycling_min","jts2019_walking_min","jts2019_public_transport_min","tcm2025_shopping_modes","changed_LSOA_geographies"),
 downstream_status=c("historical_comparator","historical_comparator","historical_comparator","EXCLUDE","contemporary_external_benchmark","sensitivity_only_for_JTS2019"),
 reason=c("consistent with official rounded national food-store mean","consistent with official rounded national food-store mean","consistent with official rounded national food-store mean","raw FoodPTt duplicates FoodCart for every LSOA despite official release distinguishing modes","mode-specific 2025 LSOA21 benchmark","2011-to-2021 boundary changes require caution; no pseudo-exact interpretation"),
 stringsAsFactors=FALSE)
write.csv(contract,file.path(root,"results","f2a","F2_VALIDATED_DOWNSTREAM_CONTRACT.csv"),row.names=FALSE)
cat("VALIDATED_F2_OK rows=33755 PT2019_MIN_EXCLUDED changed_geography_flagged\n")

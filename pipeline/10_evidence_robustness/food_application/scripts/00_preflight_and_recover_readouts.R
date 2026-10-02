#!/usr/bin/env Rscript
options(warn=1);options(stringsAsFactors=FALSE)
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=3L)stop("Usage: 00_preflight_and_recover_readouts.R <package_root> <project_root> <framework_root>",call.=FALSE)
package_root<-normalizePath(args[[1L]],winslash="/",mustWork=TRUE)
project_root<-normalizePath(args[[2L]],winslash="/",mustWork=TRUE)
framework_root<-normalizePath(args[[3L]],winslash="/",mustWork=TRUE)
app_root<-file.path(package_root,"food_application")
result_root<-file.path(app_root,"results","p1_4_d")
dir.create(result_root,recursive=TRUE,showWarnings=FALSE)
source(file.path(framework_root,"framework/R/TDABMTopologyFreeze.R"))
source(file.path(framework_root,"framework/R/TDABMPaperEvidence.R"))
source(file.path(framework_root,"framework/R/TDABMInterpretationRobustness.R"))
source(file.path(app_root,"R/FoodP14DHelpers.R"))

p1c_root<-file.path(package_root,"accepted_p1_4_c","p1_4_c")
status<-readLines(file.path(p1c_root,"P1_4_C_STATUS.txt"),warn=FALSE)
if(!any(grepl("^P1_4_C_STATUS=PASS$",status)))stop("Accepted P1.4-C status is not PASS.",call.=FALSE)
if(!any(grepl("^CANONICAL_TOPOLOGY_FROZEN=TRUE$",status)))stop("Canonical topology is not frozen.",call.=FALSE)
if(!any(grepl("^APPROVED_RADIUS=1.50$",status)))stop("Accepted radius is not 1.50.",call.=FALSE)

bm<-readRDS(file.path(p1c_root,"canonical_ballmapper_object.rds"))
point_order<-utils::read.csv(file.path(p1c_root,"canonical_point_order.csv"),stringsAsFactors=FALSE,check.names=FALSE)
if(nrow(point_order)!=FOOD_EXPECTED_N)stop("Canonical point order row count mismatch.",call.=FALSE)

fp_lines<-readLines(file.path(p1c_root,"canonical_topology_fingerprint.txt"),warn=FALSE)
fp<-sub("^TOPOLOGY_FINGERPRINT_SHA256=","",grep("^TOPOLOGY_FINGERPRINT_SHA256=",fp_lines,value=TRUE))
if(length(fp)!=1L)stop("Canonical fingerprint missing/nonunique.",call.=FALSE)
fp2<-tdabm_tf_topology_fingerprint(bm,point_order$unit_id)
if(!identical(fp,fp2))stop("Canonical topology fingerprint does not reproduce.",call.=FALSE)

# Generic P1.4-D smoke tests are run from shell too; here ensure modules are read-only.
before<-serialize(list(bm$vertices,bm$edges,bm$points_covered_by_landmarks,bm$landmarks,bm$epsilon),NULL)
invisible(tdabm_pe_topology_summary(bm,nrow(point_order),fp))
after<-serialize(list(bm$vertices,bm$edges,bm$points_covered_by_landmarks,bm$landmarks,bm$epsilon),NULL)
if(!identical(before,after))stop("P1.4-D paper-evidence module modified frozen topology.",call.=FALSE)

inventory<-food_header_inventory(project_root)
utils::write.csv(inventory,file.path(result_root,"readout_header_inventory.csv"),row.names=FALSE)

targets<-c(
 "ruc_category","iuc_category",
 "tcm_shopping_walk","tcm_shopping_cycle","tcm_shopping_public_transport",
 "tcm_shopping_drive","tcm_shopping_overall",
 "internal_qualifying_store_count","kmeans_cluster",
 "resource_buffered_flag","proximity_capability_constrained_flag","compound_disadvantage_flag"
)

candidate_tabs<-list(); mappings<-list(); vals<-list()
for(t in targets){
  z<-food_select_target(t,inventory,point_order$unit_id)
  candidate_tabs[[t]]<-z$inventory
  if(is.null(z$mapping)){
    alltab<-do.call(rbind,candidate_tabs)
    utils::write.csv(alltab,file.path(result_root,"readout_source_candidate_inventory.csv"),row.names=FALSE)
    stop("No validated source mapping found for required readout: ",t,". Inspect candidate inventory.",call.=FALSE)
  }
  mappings[[t]]<-z$mapping
  vals[[t]]<-z$values
}
alltab<-do.call(rbind,candidate_tabs)
mapping<-do.call(rbind,mappings)
utils::write.csv(alltab,file.path(result_root,"readout_source_candidate_inventory.csv"),row.names=FALSE)
utils::write.csv(mapping,file.path(result_root,"readout_source_manifest.csv"),row.names=FALSE)

iuc_map <- mapping[mapping$target=="iuc_category",,drop=FALSE]
if(nrow(iuc_map)!=1L)stop("IUC source mapping is missing/nonunique.",call.=FALSE)
utils::write.csv(
  iuc_map,
  file.path(result_root,"iuc_source_resolution.csv"),
  row.names=FALSE
)
cat(
  "IUC_SOURCE_RESOLVED file=",basename(iuc_map$file),
  " column=",iuc_map$column,
  " reason=",iuc_map$selection_reason,
  " equivalent_authoritative_candidates=",
  iuc_map$equivalent_authoritative_candidate_count,
  "\n",sep=""
)

readouts<-data.frame(
  point_index=point_order$point_index,
  unit_id=point_order$unit_id,
  unit_label=point_order$unit_label,
  physical_friction_log_nearest_large_store_km=
    point_order$physical_friction_log_nearest_large_store_km_unscaled,
  transport_constraint_no_car_pct=
    point_order$transport_constraint_no_car_pct_unscaled,
  material_constraint_income_deprivation_2025=
    point_order$material_constraint_income_deprivation_2025_unscaled,
  ruc_category=as.character(vals$ruc_category),
  iuc_category=as.character(vals$iuc_category),
  tcm_shopping_walk=food_numeric(vals$tcm_shopping_walk),
  tcm_shopping_cycle=food_numeric(vals$tcm_shopping_cycle),
  tcm_shopping_public_transport=food_numeric(vals$tcm_shopping_public_transport),
  tcm_shopping_drive=food_numeric(vals$tcm_shopping_drive),
  tcm_shopping_overall=food_numeric(vals$tcm_shopping_overall),
  internal_qualifying_store_count=food_numeric(vals$internal_qualifying_store_count),
  kmeans_cluster=as.character(vals$kmeans_cluster),
  resource_buffered_flag=food_binary01(vals$resource_buffered_flag),
  proximity_capability_constrained_flag=food_binary01(vals$proximity_capability_constrained_flag),
  compound_disadvantage_flag=food_binary01(vals$compound_disadvantage_flag),
  stringsAsFactors=FALSE
)
readouts$internal_qualifying_store_presence<-as.integer(readouts$internal_qualifying_store_count>0)

# Fail closed against the known source contracts.
if(sum(!is.na(readouts$iuc_category)&nzchar(trimws(readouts$iuc_category)))!=31810L)
  stop("IUC coverage does not equal accepted 31,810 LSOA21s.",call.=FALSE)
if(length(unique(na.omit(readouts$iuc_category)))!=10L)
  stop("IUC does not contain exactly 10 accepted categories.",call.=FALSE)
if(length(unique(readouts$ruc_category))!=6L)
  stop("RUC does not contain exactly six categories.",call.=FALSE)
if(sum(readouts$internal_qualifying_store_presence)!=4680L)
  stop("Internal qualifying-store presence count does not reproduce accepted 4,680 LSOAs.",call.=FALSE)
if(sum(readouts$resource_buffered_flag)!=2819L ||
   sum(readouts$proximity_capability_constrained_flag)!=2241L ||
   sum(readouts$compound_disadvantage_flag)!=295L)
  stop("F4 diagnostic flag counts do not reproduce accepted totals.",call.=FALSE)

utils::write.csv(readouts,file.path(result_root,"food_hfa_readout_frame.csv"),row.names=FALSE)

audit<-data.frame(
  check=c("p1_4_c_pass","approved_radius","fingerprint_reproduces","rows","ruc_classes",
          "iuc_valid","iuc_groups","internal_store_presence","resource_buffered",
          "proximity_constrained","compound_disadvantage"),
  pass=TRUE,
  detail=c("PASS","1.50",fp,nrow(readouts),length(unique(readouts$ruc_category)),
           sum(!is.na(readouts$iuc_category)&nzchar(trimws(readouts$iuc_category))),
           length(unique(na.omit(readouts$iuc_category))),
           sum(readouts$internal_qualifying_store_presence),
           sum(readouts$resource_buffered_flag),
           sum(readouts$proximity_capability_constrained_flag),
           sum(readouts$compound_disadvantage_flag)),
  stringsAsFactors=FALSE
)
utils::write.csv(audit,file.path(result_root,"P1_4_D_preflight_audit.csv"),row.names=FALSE)

cat("FOOD_HFA_P1_4_D_SOURCE_RECOVERY_OK\n")
cat("CANONICAL_TOPOLOGY_FINGERPRINT=",fp,"\n",sep="")
cat("READOUTS_RECOVERED=",paste(targets,collapse=";"),"\n",sep="")

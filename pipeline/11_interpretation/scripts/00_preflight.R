#!/usr/bin/env Rscript
options(warn=1); options(stringsAsFactors=FALSE)
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=1L) stop("Usage: 00_preflight.R <package_root>",call.=FALSE)
root <- normalizePath(args[[1L]],winslash="/",mustWork=TRUE)
source(file.path(root,"R","Food24Helpers.R"))
src <- file.path(root,"accepted_p1_4_d")
out <- file.path(root,"results")
dir.create(out,recursive=TRUE,showWarnings=FALSE)
contract <- food24_read_contract(file.path(root,"config","source_contract.csv"))

required <- c(
 "p1_4_d/P1_4_D_STATUS.txt",
 "p1_4_d/P1_4_D_validation_report.csv",
 "p1_4_d/canonical_topology_summary.csv",
 "p1_4_d/canonical_overlap_summary.csv",
 "p1_4_d/canonical_ball_profile_wide.csv",
 "p1_4_d/canonical_ball_categorical_composition.csv",
 "p1_4_d/canonical_edges.csv",
 "p1_4_d/canonical_memberships.csv",
 "p1_4_d/food_hfa_readout_frame.csv",
 "p1_4_d/radius_robustness_mean_pattern_comparison.csv",
 "p1_4_d/landmark_order_robustness_summary.csv",
 "p1_4_d/radius_robustness_comparison_runs.csv",
 "p1_4_d/readout_source_manifest.csv",
 "p1_4_d/P1_4_D_source_lineage.csv",
 "p1_4_d/canonical_paper_layout.csv",
 "canonical_identity/canonical_topology_fingerprint.txt"
)
missing <- required[!file.exists(file.path(src,required))]
if(length(missing)) stop("Missing accepted P1.4-D inputs: ",paste(missing,collapse="; "),call.=FALSE)

status <- readLines(file.path(src,"p1_4_d","P1_4_D_STATUS.txt"),warn=FALSE)
if(!any(status=="P1_4_D_STATUS=PASS")) stop("P1.4-D status is not PASS.",call.=FALSE)
if(!any(status=="CANONICAL_TOPOLOGY_MODIFIED=FALSE")) stop("P1.4-D did not preserve canonical topology.",call.=FALSE)
if(!any(status=="APPROVED_RADIUS=1.50")) stop("Approved radius is not 1.50.",call.=FALSE)
fp_line <- grep("^TOPOLOGY_FINGERPRINT_SHA256=",status,value=TRUE)
if(length(fp_line)!=1L) stop("P1.4-D fingerprint missing/nonunique.",call.=FALSE)
fp <- sub("^TOPOLOGY_FINGERPRINT_SHA256=","",fp_line)
if(!identical(fp,unname(contract[["topology_fingerprint_sha256"]]))) stop("Fingerprint differs from package source contract.",call.=FALSE)

val <- utils::read.csv(file.path(src,"p1_4_d","P1_4_D_validation_report.csv"),stringsAsFactors=FALSE)
if(any(val$blocking & !val$pass)) stop("Accepted P1.4-D validation contains a blocking failure.",call.=FALSE)
profile <- utils::read.csv(file.path(src,"p1_4_d","canonical_ball_profile_wide.csv"),stringsAsFactors=FALSE,check.names=FALSE)
if(nrow(profile)!=24L || !identical(sort(profile$ball_id),1:24)) stop("Canonical profile does not contain balls 1:24 exactly.",call.=FALSE)
readout <- utils::read.csv(file.path(src,"p1_4_d","food_hfa_readout_frame.csv"),stringsAsFactors=FALSE,check.names=FALSE)
if(nrow(readout)!=33755L || anyDuplicated(readout$unit_id)) stop("Readout population/identity mismatch.",call.=FALSE)

checks <- data.frame(
 check=c("p1_4_d_pass","topology_unmodified","approved_radius","fingerprint","validation","balls","observations"),
 pass=TRUE,
 detail=c("PASS","FALSE mutation", "1.50",fp,"all blocking checks pass",nrow(profile),nrow(readout)),
 stringsAsFactors=FALSE
)
utils::write.csv(checks,file.path(out,"00_preflight_audit.csv"),row.names=FALSE)
cat("FOOD24_PREFLIGHT_PASS balls=24 observations=33755 fingerprint=",fp,"\n",sep="")

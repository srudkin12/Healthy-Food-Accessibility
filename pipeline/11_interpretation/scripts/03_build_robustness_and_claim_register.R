#!/usr/bin/env Rscript
options(warn=1); options(stringsAsFactors=FALSE)
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=1L) stop("Usage: 03_build_robustness_and_claim_register.R <package_root>",call.=FALSE)
root <- normalizePath(args[[1L]],winslash="/",mustWork=TRUE)
source(file.path(root,"R","Food24Helpers.R"))
src <- file.path(root,"accepted_p1_4_d","p1_4_d")
out <- file.path(root,"results")
conf <- food24_read_contract(file.path(root,"config","interpretation_contract.csv"))

lor <- utils::read.csv(file.path(src,"landmark_order_robustness_summary.csv"),stringsAsFactors=FALSE)
rmp <- utils::read.csv(file.path(src,"radius_robustness_mean_pattern_comparison.csv"),stringsAsFactors=FALSE)
rr <- utils::read.csv(file.path(src,"radius_robustness_comparison_runs.csv"),stringsAsFactors=FALSE)
interp <- utils::read.csv(file.path(out,"24_ball_interpretation_matrix.csv"),stringsAsFactors=FALSE,check.names=FALSE)
pairs <- utils::read.csv(file.path(out,"ball_pairwise_configuration_contrasts.csv"),stringsAsFactors=FALSE)
kmeans <- utils::read.csv(file.path(out,"kmeans_alignment_by_ball.csv"),stringsAsFactors=FALSE)
overlap <- utils::read.csv(file.path(src,"canonical_overlap_summary.csv"),stringsAsFactors=FALSE)
topo <- utils::read.csv(file.path(src,"canonical_topology_summary.csv"),stringsAsFactors=FALSE)

# Core variable robustness: order sensitivity + mean-pattern nearby-radius sensitivity.
vars <- unique(c(lor$colour_variable,rmp$colour_variable))
core <- lapply(vars,function(v){
 a<-lor[lor$colour_variable==v,,drop=FALSE]
 b<-rmp[rmp$colour_variable==v & !rmp$is_approved_radius,,drop=FALSE]
 order_sp<-if(nrow(a))a$canonical_vs_repeated_mean_spearman[1] else NA_real_
 radius_min<-if(nrow(b))min(b$spearman_to_approved_repeated_mean,na.rm=TRUE) else NA_real_
 data.frame(colour_variable=v,canonical_vs_repeated_mean_spearman=order_sp,min_nearby_radius_repeated_mean_spearman=radius_min,
  order_robustness_flag=food24_spearman_flag(order_sp,as.numeric(conf[["strong_order_spearman"]]),as.numeric(conf[["moderate_order_spearman"]])),
  nearby_radius_flag=ifelse(!is.finite(radius_min),"NOT_AVAILABLE",ifelse(radius_min>=as.numeric(conf[["strong_nearby_radius_spearman"]]),"STRONG","QUALIFIED")),stringsAsFactors=FALSE)
})
core<-do.call(rbind,core)
utils::write.csv(core,file.path(out,"core_variable_robustness_evidence.csv"),row.names=FALSE)

# All summary colour variables: repeated-order distribution by radius.
splitkey <- split(rr,interaction(rr$colour_variable,rr$radius,drop=TRUE))
ss <- lapply(splitkey,function(x)data.frame(
 colour_variable=x$colour_variable[1],radius=x$radius[1],repetitions=nrow(x),
 median_spearman=stats::median(x$spearman_to_canonical,na.rm=TRUE),
 q10_spearman=as.numeric(stats::quantile(x$spearman_to_canonical,.10,na.rm=TRUE,names=FALSE,type=8)),
 q90_spearman=as.numeric(stats::quantile(x$spearman_to_canonical,.90,na.rm=TRUE,names=FALSE,type=8)),
 median_pearson=stats::median(x$pearson_to_canonical,na.rm=TRUE),
 median_rmse=stats::median(x$rmse_to_canonical,na.rm=TRUE),stringsAsFactors=FALSE))
ss<-do.call(rbind,ss); ss<-ss[order(ss$colour_variable,ss$radius),]
utils::write.csv(ss,file.path(out,"all_readout_order_radius_robustness_summary.csv"),row.names=FALSE)

family <- function(v){
 if(grepl("^ruc_category__",v))return("ruc_category")
 if(grepl("^iuc_category__",v))return("iuc_category")
 if(grepl("^kmeans_cluster__",v))return("kmeans_cluster")
 v
}
ss$readout_family<-vapply(ss$colour_variable,family,character(1))
catfam<-ss[ss$readout_family %in% c("ruc_category","iuc_category","kmeans_cluster"),,drop=FALSE]
cf<-do.call(rbind,lapply(split(catfam,interaction(catfam$readout_family,catfam$radius,drop=TRUE)),function(x)data.frame(
 readout_family=x$readout_family[1],radius=x$radius[1],n_indicators=nrow(x),worst_indicator_median_spearman=min(x$median_spearman,na.rm=TRUE),median_indicator_median_spearman=stats::median(x$median_spearman,na.rm=TRUE),worst_indicator_q10_spearman=min(x$q10_spearman,na.rm=TRUE),stringsAsFactors=FALSE)))
utils::write.csv(cf,file.path(out,"categorical_readout_robustness_evidence.csv"),row.names=FALSE)

# Mechanical claim-evidence candidates. These are descriptive, not manuscript conclusions.
axis_unique <- length(unique(interp$axis_signature))
non_scalar_n <- sum(pairs$candidate_non_scalar_contrast)
core_min_order <- min(core$canonical_vs_repeated_mean_spearman[is.finite(core$canonical_vs_repeated_mean_spearman)])
core_min_radius <- min(core$min_nearby_radius_repeated_mean_spearman[is.finite(core$min_nearby_radius_repeated_mean_spearman)])
k_mixed <- sum(kmeans$modal_kmeans_share < .80)
claims <- data.frame(
 claim_id=sprintf("C%02d",1:9),
 claim_short=c(
  "Canonical evidence consists of 24 overlapping Ball Mapper neighbourhoods at approved radius 1.50.",
  "Multiple membership is the dominant observation-level condition.",
  "The 24 balls exhibit multiple distinct three-axis constraint signatures.",
  "Pairs exist with similar mean constraint but materially different axis composition.",
  "Core continuous readout patterns are robust to landmark order.",
  "Core continuous readout patterns are robust across the pre-declared nearby-radius range.",
  "Retail and mobility readouts vary across canonical balls and are post-freeze context rather than topology inputs.",
  "RUC and IUC remain categorical contextual compositions attached after topology construction.",
  "Hard k-means alignment is not uniformly exclusive across all Ball Mapper balls."
 ),
 claim_type=c("topology","overlap","configuration","configuration_contrast","robustness","robustness","context_readout","categorical_context","comparator_context"),
 evidence_file=c("canonical_topology_summary.csv","canonical_overlap_summary.csv","24_ball_descriptive_signatures.csv","candidate_non_scalar_contrast_shortlist.csv","core_variable_robustness_evidence.csv","core_variable_robustness_evidence.csv","24_ball_interpretation_matrix.csv","24_ball_categorical_composition_full.csv","kmeans_alignment_by_ball.csv"),
 evidence_status=c("SUPPORTED_DESCRIPTIVE","SUPPORTED_DESCRIPTIVE",ifelse(axis_unique>1,"SUPPORTED_DESCRIPTIVE","NOT_SUPPORTED"),ifelse(non_scalar_n>0,"SUPPORTED_DESCRIPTIVE","NOT_SUPPORTED"),ifelse(core_min_order>=.90,"SUPPORTED_DESCRIPTIVE","QUALIFIED"),ifelse(core_min_radius>=.95,"SUPPORTED_DESCRIPTIVE","QUALIFIED"),"SUPPORTED_DESCRIPTIVE","SUPPORTED_DESCRIPTIVE",ifelse(k_mixed>0,"SUPPORTED_DESCRIPTIVE","NOT_SUPPORTED")),
 evidence_value=c("24 balls; radius 1.50",paste0(round(overlap$share_multicovered[1]*100,2),"% multicovered"),paste0(axis_unique," unique thresholded axis signatures"),paste0(non_scalar_n," screened contrast pairs"),paste0("minimum core Spearman=",round(core_min_order,4)),paste0("minimum nearby-radius mean-pattern Spearman=",round(core_min_radius,4)),"ball means and standardized differences retained","full categorical composition tables retained",paste0(k_mixed," of 24 balls have modal k-means share < 0.80")),
 interpretation_limit=c("Neighbourhoods overlap and are not classes.","Multiple membership is not automatically a bridge.","Thresholded signatures are descriptive aids, not regimes.","Screening thresholds identify candidates for human interpretation only.","High correlation does not imply exact point-level invariance.","Sensitivity radii do not replace approved radius 1.50.","Readouts are descriptive associations, not causal outcomes.","IUC is nominal; unresolved observations remain unresolved.","Existing k-means is a benchmark only; this is not a superiority test."),
 stringsAsFactors=FALSE)
utils::write.csv(claims,file.path(out,"candidate_claim_evidence_register.csv"),row.names=FALSE)

extensions <- data.frame(
 extension=c("spatial_closure","hard_clustering_comparator","pca_comparator","scaling_robustness","alternative_axis_definitions","simulation_null"),
 gate_status=c("HUMAN_REVIEW_REQUIRED","EXISTING_BENCHMARK_EVIDENCE_AVAILABLE","NOT_AUTOMATICALLY_TRIGGERED","NOT_TESTED_IN_CURRENT_FREEZE","NOT_AUTOMATICALLY_TRIGGERED","NOT_AUTOMATICALLY_TRIGGERED"),
 current_evidence=c(
  "Geographical LSOAs and RUC context are present, but P1.4-D does not test spatial dependence or geographic adjacency.",
  "F4 k-means k=3 composition is already attached to every ball and can assess alignment/mixing.",
  "No claim about linear latent structure has yet been tested in this closure pass.",
  "Canonical geometry is frozen under z-score scaling; nearby-radius/order robustness does not constitute scaling robustness.",
  "No demonstrated defect in the three-axis scientific contract in accepted stages.",
  "No null/simulation claim is required merely to describe observed configuration structure."
 ),
 decision_rule=c(
  "Activate if manuscript claims need to distinguish configurational similarity from geographic proximity/spatial dependence.",
  "Additional clustering work only if a specific claim requires more than the existing k-means benchmark.",
  "Activate only if a paper claim requires comparison with linear dimension reduction.",
  "Activate if substantive interpretation appears sensitive to the declared scaling or a referee challenges scale dependence.",
  "Activate only for a theoretically credible competing construction.",
  "Activate only for a claim needing a defined null or performance benchmark."
 ),stringsAsFactors=FALSE)
utils::write.csv(extensions,file.path(out,"conditional_extension_trigger_register.csv"),row.names=FALSE)

cat("FOOD24_ROBUSTNESS_CLAIM_REGISTER_OK core_variables=",nrow(core)," claims=",nrow(claims),"\n",sep="")

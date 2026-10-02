#!/usr/bin/env Rscript
options(warn=1); options(stringsAsFactors=FALSE)
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=1L) stop("Usage: 04_audit_and_close.R <package_root>",call.=FALSE)
root <- normalizePath(args[[1L]],winslash="/",mustWork=TRUE)
source(file.path(root,"R","Food24Helpers.R"))
out <- file.path(root,"results")
src <- file.path(root,"accepted_p1_4_d","p1_4_d")
figdir <- file.path(root,"figures"); dir.create(figdir,recursive=TRUE,showWarnings=FALSE)
contract <- food24_read_contract(file.path(root,"config","source_contract.csv"))

required <- c("00_preflight_audit.csv","24_ball_interpretation_matrix.csv","24_ball_descriptive_signatures.csv","24_ball_graph_context.csv","24_ball_membership_context.csv","24_ball_representative_lsoas.csv","24_ball_categorical_composition_full.csv","ball_pairwise_configuration_contrasts.csv","candidate_non_scalar_contrast_shortlist.csv","ball_nearest_profile_comparators.csv","kmeans_alignment_by_ball.csv","core_variable_robustness_evidence.csv","all_readout_order_radius_robustness_summary.csv","categorical_readout_robustness_evidence.csv","candidate_claim_evidence_register.csv","conditional_extension_trigger_register.csv")
missing <- required[!file.exists(file.path(out,required))]
if(length(missing)) stop("Missing closure artifacts: ",paste(missing,collapse="; "),call.=FALSE)
interp <- utils::read.csv(file.path(out,"24_ball_interpretation_matrix.csv"),stringsAsFactors=FALSE,check.names=FALSE)
claims <- utils::read.csv(file.path(out,"candidate_claim_evidence_register.csv"),stringsAsFactors=FALSE)
pre <- utils::read.csv(file.path(out,"00_preflight_audit.csv"),stringsAsFactors=FALSE)
if(!all(pre$pass)) stop("Preflight audit failed.",call.=FALSE)
if(nrow(interp)!=24L || !identical(sort(interp$ball_id),1:24)) stop("Interpretation matrix ball identity failure.",call.=FALSE)
if(any(grepl("bridge",tolower(interp$axis_signature)))) stop("Prohibited bridge language detected.",call.=FALSE)

# Two compact evidence figures, each backed by CSV data already written.
axes <- c("zdiff__physical_friction_log_nearest_large_store_km","zdiff__transport_constraint_no_car_pct","zdiff__material_constraint_income_deprivation_2025")
mat <- as.matrix(interp[,axes,drop=FALSE]); rownames(mat)<-paste0("B",interp$ball_id); colnames(mat)<-c("Physical","Transport","Material")
plot_heat <- function(file,type,m,title=""){
 if(type=="png") grDevices::png(file,width=1600,height=1200,res=180) else grDevices::pdf(file,width=9,height=7,useDingbats=FALSE)
 on.exit(grDevices::dev.off(),add=TRUE)
 op<-graphics::par(mar=c(7,5,2,2)); on.exit(graphics::par(op),add=TRUE)
 zlim<-max(abs(m),na.rm=TRUE); pal<-grDevices::hcl.colors(101,"Blue-Red 3",rev=TRUE)
 graphics::image(seq_len(nrow(m)),seq_len(ncol(m)),m,zlim=c(-zlim,zlim),col=pal,axes=FALSE,xlab="Ball",ylab="")
 graphics::axis(1,at=seq_len(nrow(m)),labels=rownames(m),las=2,cex.axis=.65)
 graphics::axis(2,at=seq_len(ncol(m)),labels=colnames(m),las=1)
 graphics::box()
 }
plot_heat(file.path(figdir,"24_ball_topology_axis_heatmap.png"),"png",mat)
plot_heat(file.path(figdir,"24_ball_topology_axis_heatmap.pdf"),"pdf",mat)

tcmcols <- c("zdiff__tcm_shopping_walk","zdiff__tcm_shopping_cycle","zdiff__tcm_shopping_public_transport","zdiff__tcm_shopping_drive","zdiff__tcm_shopping_overall","zdiff__internal_qualifying_store_count")
tm <- as.matrix(interp[,tcmcols,drop=FALSE]); rownames(tm)<-paste0("B",interp$ball_id); colnames(tm)<-c("Walk","Cycle","PT","Drive","Overall","Store count")
plot_heat(file.path(figdir,"24_ball_context_readout_heatmap.png"),"png",tm)
plot_heat(file.path(figdir,"24_ball_context_readout_heatmap.pdf"),"pdf",tm)

# Human-readable review brief, generated entirely from machine-readable evidence.
sig <- utils::read.csv(file.path(out,"24_ball_descriptive_signatures.csv"),stringsAsFactors=FALSE)
short <- utils::read.csv(file.path(out,"candidate_non_scalar_contrast_shortlist.csv"),stringsAsFactors=FALSE)
core <- utils::read.csv(file.path(out,"core_variable_robustness_evidence.csv"),stringsAsFactors=FALSE)
ext <- utils::read.csv(file.path(out,"conditional_extension_trigger_register.csv"),stringsAsFactors=FALSE)
lines <- c(
 "# Food HFA 24-ball interpretation review",
 "",
 paste0("**Approved radius:** 1.50  "),
 paste0("**Frozen topology fingerprint:** `",contract[["topology_fingerprint_sha256"]],"`  "),
 "**Status:** evidence assembled; final analytical freeze requires human review.",
 "",
 "## Ball-by-ball descriptive signatures",
 "",
 food24_write_md_table(sig,c("ball_id","landmark_unit_label","n_members","axis_mean_constraint_z","axis_profile_dispersion","axis_signature","strongest_neighbour"),digits=3),
 "",
 "## Mechanically screened non-scalar contrast candidates",
 "",
 if(nrow(short)) food24_write_md_table(short[seq_len(min(15,nrow(short))),],c("ball_a","ball_b","scalar_gap","composition_euclidean_distance","ruc_total_variation","iuc_total_variation","signature_a","signature_b"),digits=3) else "No pair met the pre-declared screen.",
 "",
 "## Core robustness evidence",
 "",
 food24_write_md_table(core,c("colour_variable","canonical_vs_repeated_mean_spearman","min_nearby_radius_repeated_mean_spearman","order_robustness_flag","nearby_radius_flag"),digits=4),
 "",
 "## Conditional extension gates",
 "",
 food24_write_md_table(ext,c("extension","gate_status","current_evidence"),digits=3),
 "",
 "## Interpretation guardrails",
 "",
 "- Balls are overlapping local neighbourhoods, not mutually exclusive classes.",
 "- Multiple membership must not automatically be described as a bridge.",
 "- Thresholded axis signatures are descriptive aids, not a new typology.",
 "- TCM, retail, RUC, IUC, k-means and flags are post-freeze readouts/context.",
 "- Evidence is descriptive and associational; causal claims require a causal design."
)
writeLines(lines,file.path(out,"INTERPRETATION_REVIEW.md"))

# Robustness/evidence status register.
robstat <- data.frame(
 evidence_item=c("canonical_24_ball_profiles","graph_overlap_context","representative_lsoa_evidence","non_scalar_contrast_screen","landmark_order_robustness","nearby_radius_robustness","categorical_context_composition","claim_evidence_register","conditional_extension_gate","final_analytical_freeze"),
 status=c(rep("completed",9),"pending_human_review"),
 stringsAsFactors=FALSE)
utils::write.csv(robstat,file.path(out,"robustness_evidence_status.csv"),row.names=FALSE)

# Audit and artifact manifest.
checks <- data.frame(
 check=c("source_preflight_pass","24_balls_exact","claim_register_present","extension_gate_present","topology_fingerprint_frozen","automatic_final_freeze_prohibited"),
 pass=c(all(pre$pass),nrow(interp)==24L,nrow(claims)>=1L,nrow(ext)>=1L,nzchar(contract[["topology_fingerprint_sha256"]]),identical(contract[["final_analytical_freeze"]],"AUTOMATICALLY_PROHIBITED")),
 detail=c("PASS",nrow(interp),nrow(claims),nrow(ext),contract[["topology_fingerprint_sha256"]],contract[["final_analytical_freeze"]]),
 blocking=TRUE,stringsAsFactors=FALSE)
utils::write.csv(checks,file.path(out,"EVIDENCE_CLOSURE_validation_report.csv"),row.names=FALSE)
if(any(checks$blocking & !checks$pass)) stop("Evidence-closure audit failed.",call.=FALSE)

files <- c(list.files(out,full.names=TRUE),list.files(figdir,full.names=TRUE))
files <- files[file.info(files)$isdir==FALSE]
# Build relative paths and hashes for all closure artifacts.
manifest <- data.frame(relative_path=sub(paste0("^",root,"/"),"",files),bytes=file.info(files)$size,sha256=vapply(files,food24_sha256,character(1)),topology_fingerprint_sha256=contract[["topology_fingerprint_sha256"]],stringsAsFactors=FALSE)
utils::write.csv(manifest,file.path(out,"EVIDENCE_CLOSURE_artifact_manifest.csv"),row.names=FALSE)

status <- c(
 "FOOD24_EVIDENCE_CLOSURE_STATUS=PASS",
 "SOURCE_P1_4_D_STATUS=PASS",
 "APPROVED_RADIUS=1.50",
 paste0("TOPOLOGY_FINGERPRINT_SHA256=",contract[["topology_fingerprint_sha256"]]),
 "CANONICAL_TOPOLOGY_MODIFIED=FALSE",
 "BALLS_INTERPRETED=24",
 "READY_FOR_HUMAN_INTERPRETATION_REVIEW=TRUE",
 "FINAL_ANALYTICAL_FREEZE=NOT_YET_DECLARED",
 "NEXT_GATE=HUMAN_REVIEW_OF_24_BALL_INTERPRETATION_AND_CONDITIONAL_EXTENSIONS"
)
writeLines(status,file.path(out,"EVIDENCE_CLOSURE_STATUS.txt"))

cat("FOOD24_EVIDENCE_CLOSURE_PASS balls=24 final_freeze=NOT_YET_DECLARED\n")

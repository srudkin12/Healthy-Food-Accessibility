#!/usr/bin/env Rscript
options(warn=1);options(stringsAsFactors=FALSE)
args<-commandArgs(trailingOnly=TRUE)
if(length(args)!=2L)stop("Usage: 02_interpretation_robustness.R <package_root> <framework_root>",call.=FALSE)
package_root<-normalizePath(args[[1L]],winslash="/",mustWork=TRUE)
framework_root<-normalizePath(args[[2L]],winslash="/",mustWork=TRUE)
app_root<-file.path(package_root,"food_application")
out<-file.path(app_root,"results","p1_4_d")
cp_root<-file.path(app_root,"checkpoints","p1_4_d_robustness")
dir.create(cp_root,recursive=TRUE,showWarnings=FALSE)
source(file.path(framework_root,"framework/R/TDABMTopologyFreeze.R"))
source(file.path(framework_root,"framework/R/TDABMInterpretationRobustness.R"))
source(file.path(framework_root,"framework/R/TDABMPaperEvidence.R"))
source(file.path(app_root,"R/FoodP14DHelpers.R"))

p1c<-file.path(package_root,"accepted_p1_4_c","p1_4_c")
bm_canon<-readRDS(file.path(p1c,"canonical_ballmapper_object.rds"))
point<-utils::read.csv(file.path(p1c,"canonical_point_order.csv"),stringsAsFactors=FALSE,check.names=FALSE)
readouts<-utils::read.csv(file.path(out,"food_hfa_readout_frame.csv"),stringsAsFactors=FALSE,check.names=FALSE)
radii<-utils::read.csv(file.path(app_root,"config","robustness_radii.csv"))$radius

if(!identical(point$unit_id,readouts$unit_id))stop("Readout order mismatch.",call.=FALSE)

# Frozen z-space, exactly as recorded in P1.4-C.
zcols<-c(
 "physical_friction_log_nearest_large_store_km_z",
 "transport_constraint_no_car_pct_z",
 "material_constraint_income_deprivation_2025_z"
)
axes<-as.data.frame(point[,zcols,drop=FALSE])
names(axes)<-c("physical_friction","transport_constraint","material_constraint")
if(any(!is.finite(as.matrix(axes))))stop("Frozen z-space contains non-finite values.",call.=FALSE)

# Robustness variables:
# continuous/ordinal-free numeric readouts plus one-hot categorical context.
numeric_candidates<-c(
 "physical_friction_log_nearest_large_store_km",
 "transport_constraint_no_car_pct",
 "material_constraint_income_deprivation_2025",
 "tcm_shopping_walk","tcm_shopping_cycle","tcm_shopping_public_transport",
 "tcm_shopping_drive","tcm_shopping_overall",
 "internal_qualifying_store_count","internal_qualifying_store_presence",
 "resource_buffered_flag","proximity_capability_constrained_flag","compound_disadvantage_flag"
)
complete_numeric<-numeric_candidates[vapply(numeric_candidates,function(v)
 all(is.finite(suppressWarnings(as.numeric(readouts[[v]])))),logical(1))]

colour<-as.data.frame(lapply(readouts[complete_numeric],as.numeric),check.names=FALSE)

# Categorical variables are represented only as one-hot probabilities.
# IUC unresolved geography is an explicit nominal level, never an ordinal code.
for(v in c("ruc_category","iuc_category","kmeans_cluster")){
 f<-food_safe_factor(readouts[[v]])
 mm<-stats::model.matrix(~f-1)
 colnames(mm)<-paste0(v,"__",make.names(levels(f),unique=TRUE))
 colour<-cbind(colour,as.data.frame(mm,check.names=FALSE))
}

if(any(!is.finite(as.matrix(colour))))stop("Robustness colour matrix contains non-finite values.",call.=FALSE)
colour_matrix<-as.matrix(colour)
storage.mode(colour_matrix)<-"double"

# Canonical local-colour matrix from the frozen topology.
canonical_local_matrix<-food_local_matrix_from_cover(
 bm_canon$points_covered_by_landmarks,colour_matrix
)
colnames(canonical_local_matrix)<-colnames(colour_matrix)

# Core variables receive point-indexed robustness summaries at every sensitivity radius.
core_names<-intersect(c(
 "physical_friction_log_nearest_large_store_km",
 "transport_constraint_no_car_pct",
 "material_constraint_income_deprivation_2025",
 "tcm_shopping_walk","tcm_shopping_cycle","tcm_shopping_public_transport",
 "tcm_shopping_drive","tcm_shopping_overall",
 "internal_qualifying_store_count"
),colnames(colour_matrix))
core_indices<-match(core_names,colnames(colour_matrix))

utils::write.csv(data.frame(
 colour_variable=colnames(colour_matrix),
 role=ifelse(colnames(colour_matrix)%in%core_names,
             "core_point_and_summary_robustness","summary_only_robustness"),
 stringsAsFactors=FALSE
),file.path(out,"P1_4_D_robustness_variable_register.csv"),row.names=FALSE)

# Compile once in parent; fork workers inherit loaded shared object.
tdabm_ir_compile_cpp(file.path(framework_root,"framework","BallMapper.cpp"))

workers<-suppressWarnings(as.integer(Sys.getenv("ROBUSTNESS_WORKERS", Sys.getenv("N_WORKERS","1"))))
if(!is.finite(workers)||workers<1L)workers<-1L
det<-parallel::detectCores(logical=FALSE)
if(is.finite(det)&&det>0)workers<-min(workers,det)
batch_size<-workers

all_topology_files<-character()
all_comparison_files<-character()
all_point_files<-character()

for(radius in radii){
 tag<-gsub("\\.","p",sprintf("%.2f",radius))
 final_top<-file.path(out,paste0("robustness_topology_runs_r",tag,".csv"))
 final_cmp<-file.path(out,paste0("robustness_comparison_runs_r",tag,".csv"))
 final_pts<-file.path(out,paste0("radius_point_mean_summary_r",tag,".csv.gz"))

 if(file.exists(final_top)&&file.exists(final_cmp)&&file.exists(final_pts)){
   cat("ROBUSTNESS_RADIUS_SKIP_COMPLETE radius=",sprintf("%.2f",radius),"\n",sep="")
   all_topology_files<-c(all_topology_files,final_top)
   all_comparison_files<-c(all_comparison_files,final_cmp)
   all_point_files<-c(all_point_files,final_pts)
   next
 }

 radius_cp<-file.path(cp_root,paste0("r",tag))
 dir.create(radius_cp,recursive=TRUE,showWarnings=FALSE)
 batches<-split(seq_len(FOOD_ROBUSTNESS_REPS),
                ceiling(seq_len(FOOD_ROBUSTNESS_REPS)/batch_size))

 for(bi in seq_along(batches)){
   bf<-file.path(radius_cp,sprintf("batch_%04d.rds",bi))
   if(file.exists(bf))next
   reps<-batches[[bi]]
   runner<-function(rep_id){
     food_robustness_one(
       axes=axes,unit_ids=point$unit_id,colour_matrix=colour_matrix,
       canonical_local_matrix=canonical_local_matrix,radius=radius,
       replicate_index=rep_id,base_seed=FOOD_BASE_SEED,core_indices=core_indices
     )
   }
   if(.Platform$OS.type=="unix"&&workers>1L){
     z<-parallel::mclapply(reps,runner,mc.cores=min(workers,length(reps)),
                          mc.preschedule=FALSE,mc.set.seed=FALSE)
   }else z<-lapply(reps,runner)
   if(any(vapply(z,inherits,logical(1),"try-error")))stop("Robustness worker failure.",call.=FALSE)

   # Compact checkpoint: small run summaries + one matrix per core variable.
   top<-do.call(rbind,lapply(z,`[[`,"topology"))
   cmp<-do.call(rbind,lapply(z,`[[`,"comparison"))
   local_by_var<-lapply(seq_along(core_names),function(j)
     do.call(cbind,lapply(z,function(x)x$core_local[,j])))
   names(local_by_var)<-core_names
   saveRDS(list(reps=reps,topology=top,comparison=cmp,local=local_by_var),
           bf,compress="gzip")
   rm(z,top,cmp,local_by_var);gc(FALSE)
   cat("ROBUSTNESS_CHECKPOINT radius=",sprintf("%.2f",radius),
       " batch=",bi,"/",length(batches),"\n",sep="")
 }

 bfs<-file.path(radius_cp,sprintf("batch_%04d.rds",seq_along(batches)))
 if(!all(file.exists(bfs)))stop("Incomplete robustness checkpoints.",call.=FALSE)
 objs<-lapply(bfs,readRDS)
 top<-do.call(rbind,lapply(objs,`[[`,"topology"))
 cmp<-do.call(rbind,lapply(objs,`[[`,"comparison"))
 top<-top[order(top$replicate),,drop=FALSE]
 cmp<-cmp[order(cmp$colour_variable,cmp$replicate),,drop=FALSE]
 if(nrow(top)!=FOOD_ROBUSTNESS_REPS)stop("Topology robustness repetition count mismatch.",call.=FALSE)
 utils::write.csv(top,final_top,row.names=FALSE)
 utils::write.csv(cmp,final_cmp,row.names=FALSE)

 # Exact point-indexed repeated-order summaries for core readouts.
 point_rows<-list()
 for(v in core_names){
   mats<-lapply(objs,function(x)x$local[[v]])
   m<-do.call(cbind,mats)
   if(ncol(m)!=FOOD_ROBUSTNESS_REPS)stop("Point robustness column count mismatch.",call.=FALSE)
   canon<-canonical_local_matrix[,match(v,colnames(canonical_local_matrix))]
   point_rows[[v]]<-data.frame(
     colour_variable=v,
     radius=radius,
     approved_radius=FOOD_APPROVED_RADIUS,
     is_approved_radius=abs(radius-FOOD_APPROVED_RADIUS)<=1e-10,
     point_index=seq_len(nrow(m)),
     unit_id=point$unit_id,
     unit_label=point$unit_label,
     canonical_local_colour=canon,
     repeated_order_mean_local_colour=matrixStats::rowMeans2(m),
     repeated_order_sd_local_colour=matrixStats::rowSds(m),
     repeated_order_q10_local_colour=as.numeric(matrixStats::rowQuantiles(m,probs=.10)),
     repeated_order_q90_local_colour=as.numeric(matrixStats::rowQuantiles(m,probs=.90)),
     stringsAsFactors=FALSE
   )
 }
 pts<-do.call(rbind,point_rows)
 gz<-gzfile(final_pts,"wt");utils::write.csv(pts,gz,row.names=FALSE);close(gz)

 all_topology_files<-c(all_topology_files,final_top)
 all_comparison_files<-c(all_comparison_files,final_cmp)
 all_point_files<-c(all_point_files,final_pts)

 # Radius is now recoverable from final files; remove large local checkpoint matrices.
 unlink(radius_cp,recursive=TRUE,force=TRUE)
 rm(objs,top,cmp,pts);gc(FALSE)
 cat("ROBUSTNESS_RADIUS_COMPLETE radius=",sprintf("%.2f",radius),"\n",sep="")
}

# Consolidate summaries.
topology<-do.call(rbind,lapply(all_topology_files,utils::read.csv,stringsAsFactors=FALSE))
comparison<-do.call(rbind,lapply(all_comparison_files,utils::read.csv,stringsAsFactors=FALSE))
run<-list(topology=topology,comparison=comparison,points=data.frame())
radius_summary<-tdabm_ir_summarise_radius(run)
utils::write.csv(radius_summary,file.path(out,"radius_robustness_summary.csv"),row.names=FALSE)

# Read point summaries back one radius at a time and create pattern comparisons to approved repeated mean.
point_means<-do.call(rbind,lapply(all_point_files,function(f)
 utils::read.csv(gzfile(f),stringsAsFactors=FALSE)))
approved<-point_means[abs(point_means$radius-FOOD_APPROVED_RADIUS)<=1e-10,
 c("colour_variable","point_index","repeated_order_mean_local_colour"),drop=FALSE]
names(approved)[3]<-"approved_repeated_mean"
pattern_rows<-list();k<-0L
for(v in core_names){
 base<-approved[approved$colour_variable==v,,drop=FALSE]
 for(r in radii){
  x<-point_means[point_means$colour_variable==v&abs(point_means$radius-r)<=1e-10,
                 c("point_index","repeated_order_mean_local_colour"),drop=FALSE]
  m<-merge(base,x,by="point_index",sort=TRUE)
  k<-k+1L
  pattern_rows[[k]]<-data.frame(
   colour_variable=v,radius=r,approved_radius=FOOD_APPROVED_RADIUS,
   is_approved_radius=abs(r-FOOD_APPROVED_RADIUS)<=1e-10,
   pearson_to_approved_repeated_mean=tdabm_ir_safe_cor(m$repeated_order_mean_local_colour,m$approved_repeated_mean,"pearson"),
   spearman_to_approved_repeated_mean=tdabm_ir_safe_cor(m$repeated_order_mean_local_colour,m$approved_repeated_mean,"spearman"),
   rmse_to_approved_repeated_mean=sqrt(mean((m$repeated_order_mean_local_colour-m$approved_repeated_mean)^2)),
   mae_to_approved_repeated_mean=mean(abs(m$repeated_order_mean_local_colour-m$approved_repeated_mean)),
   stringsAsFactors=FALSE)
 }
}
pattern<-do.call(rbind,pattern_rows)
utils::write.csv(pattern,file.path(out,"radius_robustness_mean_pattern_comparison.csv"),row.names=FALSE)

# Approved-radius observation and aggregate landmark-order robustness.
approved_pts<-point_means[abs(point_means$radius-FOOD_APPROVED_RADIUS)<=1e-10,,drop=FALSE]
approved_pts$observed_value<-NA_real_
for(v in core_names){
 approved_pts$observed_value[approved_pts$colour_variable==v]<-
  as.numeric(readouts[[v]])
}
approved_pts$canonical_minus_repeated_mean<-
 approved_pts$canonical_local_colour-approved_pts$repeated_order_mean_local_colour
approved_pts$absolute_canonical_minus_repeated_mean<-
 abs(approved_pts$canonical_minus_repeated_mean)
approved_pts$canonical_inside_q10_q90<-
 approved_pts$canonical_local_colour>=approved_pts$repeated_order_q10_local_colour&
 approved_pts$canonical_local_colour<=approved_pts$repeated_order_q90_local_colour

obs<-data.frame(
 colour_variable=approved_pts$colour_variable,
 point_index=approved_pts$point_index,
 unit_id=approved_pts$unit_id,
 unit_label=approved_pts$unit_label,
 observed_value=approved_pts$observed_value,
 canonical_local_colour=approved_pts$canonical_local_colour,
 repeated_order_mean=approved_pts$repeated_order_mean_local_colour,
 repeated_order_sd=approved_pts$repeated_order_sd_local_colour,
 repeated_order_q10=approved_pts$repeated_order_q10_local_colour,
 repeated_order_q90=approved_pts$repeated_order_q90_local_colour,
 canonical_minus_repeated_mean=approved_pts$canonical_minus_repeated_mean,
 absolute_canonical_minus_repeated_mean=approved_pts$absolute_canonical_minus_repeated_mean,
 canonical_inside_q10_q90=approved_pts$canonical_inside_q10_q90,
 stringsAsFactors=FALSE
)
gz<-gzfile(file.path(out,"landmark_order_observation_robustness.csv.gz"),"wt")
utils::write.csv(obs,gz,row.names=FALSE);close(gz)
landmark_summary<-tdabm_ir_landmark_order_summary(obs)
utils::write.csv(landmark_summary,file.path(out,"landmark_order_robustness_summary.csv"),row.names=FALSE)

# Compact all-run files.
utils::write.csv(topology,file.path(out,"radius_robustness_topology_runs.csv"),row.names=FALSE)
utils::write.csv(comparison,file.path(out,"radius_robustness_comparison_runs.csv"),row.names=FALSE)

method<-data.frame(
 field=c("approved_radius","sensitivity_radii","repetitions_per_radius","base_seed",
         "workers_default","order_schedule","local_colour_aggregation",
         "canonical_topology_written","transient_topologies_retained",
         "categorical_handling","iuc_missing_handling"),
 value=c("1.50","1.40;1.45;1.50;1.55;1.60",as.character(FOOD_ROBUSTNESS_REPS),"20260807",as.character(workers),
         "same seed formula base_seed + replicate*1009 at every radius",
         "equal_ball_mean","FALSE","FALSE",
         "one-hot category probabilities for summary robustness",
         "UNRESOLVED is an explicit nominal level; no ordinal coding"),
 stringsAsFactors=FALSE
)
utils::write.csv(method,file.path(out,"P1_4_D_robustness_method_contract.csv"),row.names=FALSE)

cat("FOOD_HFA_P1_4_D_ROBUSTNESS_OK radii=",length(radii),
    " reps_per_radius=",FOOD_ROBUSTNESS_REPS,
    " summary_colours=",ncol(colour_matrix),
    " point_core_colours=",length(core_names),"\n",sep="")

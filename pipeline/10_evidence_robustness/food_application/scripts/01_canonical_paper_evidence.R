#!/usr/bin/env Rscript
options(warn=1);options(stringsAsFactors=FALSE)
args<-commandArgs(trailingOnly=TRUE)
if(length(args)!=2L)stop("Usage: 01_canonical_paper_evidence.R <package_root> <framework_root>",call.=FALSE)
package_root<-normalizePath(args[[1L]],winslash="/",mustWork=TRUE)
framework_root<-normalizePath(args[[2L]],winslash="/",mustWork=TRUE)
app_root<-file.path(package_root,"food_application")
out<-file.path(app_root,"results","p1_4_d")
fig<-file.path(app_root,"figures","canonical")
dir.create(out,recursive=TRUE,showWarnings=FALSE);dir.create(fig,recursive=TRUE,showWarnings=FALSE)
source(file.path(framework_root,"framework/R/TDABMTopologyFreeze.R"))
source(file.path(framework_root,"framework/R/TDABMPaperEvidence.R"))
source(file.path(app_root,"R/FoodP14DHelpers.R"))

p1c<-file.path(package_root,"accepted_p1_4_c","p1_4_c")
bm<-readRDS(file.path(p1c,"canonical_ballmapper_object.rds"))
point<-utils::read.csv(file.path(p1c,"canonical_point_order.csv"),stringsAsFactors=FALSE,check.names=FALSE)
readouts<-utils::read.csv(file.path(out,"food_hfa_readout_frame.csv"),stringsAsFactors=FALSE,check.names=FALSE)

if(!identical(as.character(point$unit_id),as.character(readouts$unit_id)))
 stop("Readout frame order differs from frozen canonical point order.",call.=FALSE)

fp_lines<-readLines(file.path(p1c,"canonical_topology_fingerprint.txt"),warn=FALSE)
fp<-sub("^TOPOLOGY_FINGERPRINT_SHA256=","",grep("^TOPOLOGY_FINGERPRINT_SHA256=",fp_lines,value=TRUE))
if(!identical(fp,tdabm_tf_topology_fingerprint(bm,point$unit_id)))
 stop("Frozen fingerprint mismatch before evidence generation.",call.=FALSE)

before<-serialize(list(bm$vertices,bm$edges,bm$points_covered_by_landmarks,bm$landmarks,bm$epsilon),NULL)

# Core canonical topology evidence.
topo<-tdabm_pe_topology_summary(bm,nrow(point),fp)
membership<-tdabm_pe_membership_table(bm,point$unit_id,point$unit_label)
landmarks<-tdabm_pe_landmark_table(bm,point$unit_id,point$unit_label)
edges<-tdabm_pe_edge_table(bm)
components<-tdabm_pe_component_tables(bm,point$unit_id)
overlap<-tdabm_pe_overlap_tables(bm,point$unit_id,point$unit_label)

utils::write.csv(topo,file.path(out,"canonical_topology_summary.csv"),row.names=FALSE)
utils::write.csv(membership,file.path(out,"canonical_memberships.csv"),row.names=FALSE)
utils::write.csv(landmarks,file.path(out,"canonical_landmarks.csv"),row.names=FALSE)
utils::write.csv(edges,file.path(out,"canonical_edges.csv"),row.names=FALSE)
utils::write.csv(components$ball,file.path(out,"canonical_ball_components.csv"),row.names=FALSE)
utils::write.csv(components$component,file.path(out,"canonical_component_summary.csv"),row.names=FALSE)
utils::write.csv(overlap$observation,file.path(out,"canonical_observation_overlap.csv"),row.names=FALSE)
utils::write.csv(overlap$frequency,file.path(out,"canonical_overlap_frequency.csv"),row.names=FALSE)
utils::write.csv(overlap$summary,file.path(out,"canonical_overlap_summary.csv"),row.names=FALSE)

topology_vars<-c(
 "physical_friction_log_nearest_large_store_km",
 "transport_constraint_no_car_pct",
 "material_constraint_income_deprivation_2025"
)
numeric_readouts<-c(
 "tcm_shopping_walk","tcm_shopping_cycle","tcm_shopping_public_transport",
 "tcm_shopping_drive","tcm_shopping_overall",
 "internal_qualifying_store_count","internal_qualifying_store_presence",
 "resource_buffered_flag","proximity_capability_constrained_flag","compound_disadvantage_flag"
)
categorical_vars<-c("ruc_category","iuc_category","kmeans_cluster")

# Numeric ball profiles, supporting missing values without changing the population.
axis_profiles<-do.call(rbind,lapply(topology_vars,function(v)
 food_numeric_profile_missing(bm,readouts[[v]],v,"topology_axis")))
colour_profiles<-do.call(rbind,lapply(numeric_readouts,function(v)
 food_numeric_profile_missing(bm,readouts[[v]],v,"post_freeze_numeric_readout")))
utils::write.csv(axis_profiles,file.path(out,"canonical_ball_axis_profiles.csv"),row.names=FALSE)
utils::write.csv(colour_profiles,file.path(out,"canonical_ball_colour_profiles_numeric.csv"),row.names=FALSE)

# Categorical composition and local probability evidence.
cat_long<-do.call(rbind,lapply(categorical_vars,function(v)
 food_categorical_ball_profile(bm,readouts[[v]],v)))
cat_summary<-do.call(rbind,lapply(split(cat_long,cat_long$variable),food_categorical_ball_summary))
utils::write.csv(cat_long,file.path(out,"canonical_ball_categorical_composition.csv"),row.names=FALSE)
utils::write.csv(cat_summary,file.path(out,"canonical_ball_categorical_summary.csv"),row.names=FALSE)

# Point-indexed numeric local colour.
local_num<-do.call(rbind,lapply(c(topology_vars,numeric_readouts),function(v)
 food_numeric_local_colour_missing(bm,readouts[[v]],point$unit_id,point$unit_label,v)))
utils::write.csv(local_num,file.path(out,"canonical_point_local_colours_numeric.csv"),row.names=FALSE)

# Point-indexed categorical local probabilities.
local_cat<-do.call(rbind,lapply(categorical_vars,function(v)
 food_categorical_local_probabilities(bm,readouts[[v]],point$unit_id,point$unit_label,v)))
gz<-gzfile(file.path(out,"canonical_point_local_category_probabilities.csv.gz"),"wt")
utils::write.csv(local_cat,gz,row.names=FALSE);close(gz)

# Wide ball profile: structural fields + numeric means + categorical modes.
graph<-tdabm_pe_graph_structure(bm)
wide<-data.frame(
 ball_id=seq_along(bm$points_covered_by_landmarks),
 landmark_unit_id=landmarks$unit_id,
 landmark_unit_label=landmarks$unit_label,
 n_members=vapply(bm$points_covered_by_landmarks,length,integer(1)),
 component_id=graph$component,
 degree=graph$degree,
 isolated_ball=graph$degree==0L,
 stringsAsFactors=FALSE
)
for(v in c(topology_vars,numeric_readouts)){
 p<-if(v %in% topology_vars)axis_profiles else colour_profiles
 q<-p[p$variable==v,c("ball_id","ball_mean","standardized_difference","coverage_share")]
 names(q)[-1]<-paste0(c("mean__","zdiff__","coverage__"),v)
 wide<-merge(wide,q,by="ball_id",all.x=TRUE,sort=FALSE)
}
for(v in categorical_vars){
 q<-cat_summary[cat_summary$variable==v,c("ball_id","modal_category","modal_share","category_entropy")]
 names(q)[-1]<-paste0(c("mode__","mode_share__","entropy__"),v)
 wide<-merge(wide,q,by="ball_id",all.x=TRUE,sort=FALSE)
}
wide<-wide[order(wide$ball_id),]
utils::write.csv(wide,file.path(out,"canonical_ball_profile_wide.csv"),row.names=FALSE)

# Fixed layout and figure sidecars.
layout<-tdabm_pe_fixed_layout(bm,seed=20260807L)
utils::write.csv(layout,file.path(out,"canonical_paper_layout.csv"),row.names=FALSE)

figure_rows<-list();fk<-0L
numeric_figvars<-c(topology_vars,
 "tcm_shopping_walk","tcm_shopping_cycle","tcm_shopping_public_transport",
 "tcm_shopping_drive","tcm_shopping_overall","internal_qualifying_store_count",
 "internal_qualifying_store_presence",
 "resource_buffered_flag","proximity_capability_constrained_flag","compound_disadvantage_flag")
for(v in numeric_figvars){
 p<-if(v %in% topology_vars)axis_profiles else colour_profiles
 vals<-p$ball_mean[p$variable==v]
 ids<-p$ball_id[p$variable==v]
 vals<-vals[match(seq_along(bm$points_covered_by_landmarks),ids)]
 if(any(!is.finite(vals)))next
 sidecar<-data.frame(ball_id=seq_along(vals),value=vals)
 sidefile<-file.path(out,paste0("figure_sidecar__",v,".csv"))
 utils::write.csv(sidecar,sidefile,row.names=FALSE)
 png<-file.path(fig,paste0("canonical_r150_",v,".png"))
 pdf<-file.path(fig,paste0("canonical_r150_",v,".pdf"))
 tdabm_pe_plot_fixed_map(bm,layout,vals,title="",legend_title=v,png_file=png,pdf_file=pdf)
 fk<-fk+1L;figure_rows[[fk]]<-data.frame(
  figure_id=paste0("canonical_r150_",v),variable=v,kind="numeric_ball_mean",
  topology_fingerprint_sha256=fp,layout_seed=20260807L,
  sidecar=basename(sidefile),png=basename(png),pdf=basename(pdf),stringsAsFactors=FALSE)
}
for(v in categorical_vars){
 q<-cat_summary[cat_summary$variable==v,]
 cats<-q$modal_category[match(seq_along(bm$points_covered_by_landmarks),q$ball_id)]
 sidecar<-data.frame(ball_id=seq_along(cats),modal_category=cats,
                     modal_share=q$modal_share[match(seq_along(cats),q$ball_id)])
 sidefile<-file.path(out,paste0("figure_sidecar__",v,".csv"))
 utils::write.csv(sidecar,sidefile,row.names=FALSE)
 png<-file.path(fig,paste0("canonical_r150_",v,".png"))
 pdf<-file.path(fig,paste0("canonical_r150_",v,".pdf"))
 food_categorical_plot(bm,layout,cats,png,pdf,v)
 fk<-fk+1L;figure_rows[[fk]]<-data.frame(
  figure_id=paste0("canonical_r150_",v),variable=v,kind="categorical_modal_composition",
  topology_fingerprint_sha256=fp,layout_seed=20260807L,
  sidecar=basename(sidefile),png=basename(png),pdf=basename(pdf),stringsAsFactors=FALSE)
}
figure_register<-do.call(rbind,figure_rows)
utils::write.csv(figure_register,file.path(out,"figure_register.csv"),row.names=FALSE)

after<-serialize(list(bm$vertices,bm$edges,bm$points_covered_by_landmarks,bm$landmarks,bm$epsilon),NULL)
if(!identical(before,after))stop("Canonical evidence generation modified frozen topology.",call.=FALSE)
if(!identical(fp,tdabm_tf_topology_fingerprint(bm,point$unit_id)))
 stop("Topology fingerprint changed during P1.4-D evidence generation.",call.=FALSE)

cat("FOOD_HFA_P1_4_D_CANONICAL_EVIDENCE_OK balls=",topo$n_balls,
    " readouts=",length(topology_vars)+length(numeric_readouts)+length(categorical_vars),"\n",sep="")

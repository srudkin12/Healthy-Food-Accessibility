#!/usr/bin/env Rscript
options(warn=1); options(stringsAsFactors=FALSE)
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=1L) stop("Usage: 01_build_ball_interpretation.R <package_root>",call.=FALSE)
root <- normalizePath(args[[1L]],winslash="/",mustWork=TRUE)
source(file.path(root,"R","Food24Helpers.R"))
src <- file.path(root,"accepted_p1_4_d","p1_4_d")
out <- file.path(root,"results"); dir.create(out,recursive=TRUE,showWarnings=FALSE)
conf <- food24_read_contract(file.path(root,"config","interpretation_contract.csv"))

p <- utils::read.csv(file.path(src,"canonical_ball_profile_wide.csv"),stringsAsFactors=FALSE,check.names=FALSE)
comp <- utils::read.csv(file.path(src,"canonical_ball_categorical_composition.csv"),stringsAsFactors=FALSE)
edges <- utils::read.csv(file.path(src,"canonical_edges.csv"),stringsAsFactors=FALSE)
mem <- utils::read.csv(file.path(src,"canonical_memberships.csv"),stringsAsFactors=FALSE)
rd <- utils::read.csv(file.path(src,"food_hfa_readout_frame.csv"),stringsAsFactors=FALSE,check.names=FALSE)

axes <- c("physical_friction_log_nearest_large_store_km","transport_constraint_no_car_pct","material_constraint_income_deprivation_2025")
zcols <- paste0("zdiff__",axes)
if(!all(zcols %in% names(p))) stop("Missing topology-axis standardized-difference fields.",call.=FALSE)

low <- as.numeric(conf[["axis_low_threshold"]]); high <- as.numeric(conf[["axis_high_threshold"]])
vl <- as.numeric(conf[["axis_very_low_threshold"]]); vh <- as.numeric(conf[["axis_very_high_threshold"]])
for(j in seq_along(axes)) p[[paste0("level__",axes[[j]])]] <- food24_level(p[[zcols[[j]]]],low,high,vl,vh)

zmat <- as.matrix(p[,zcols,drop=FALSE]); storage.mode(zmat)<-"double"
p$axis_mean_constraint_z <- rowMeans(zmat)
p$axis_profile_dispersion <- apply(zmat,1,stats::sd)
p$axis_profile_l2_from_sample_mean <- sqrt(rowSums(zmat^2))
p$highest_constraint_axis <- axes[max.col(zmat,ties.method="first")]
p$highest_constraint_z <- apply(zmat,1,max)
p$lowest_constraint_axis <- axes[max.col(-zmat,ties.method="first")]
p$lowest_constraint_z <- apply(zmat,1,min)
p$dominant_absolute_axis <- axes[max.col(abs(zmat),ties.method="first")]
p$dominant_absolute_direction <- ifelse(zmat[cbind(seq_len(nrow(zmat)),max.col(abs(zmat),ties.method="first"))] >= 0,"higher_constraint","lower_constraint")
p$axis_signature <- paste0(
 "physical=",p[[paste0("level__",axes[1])]]," | ",
 "transport=",p[[paste0("level__",axes[2])]]," | ",
 "material=",p[[paste0("level__",axes[3])]])

# Graph context from frozen overlap edges.
gctx <- lapply(1:24,function(b){
 e <- edges[edges$from==b | edges$to==b,,drop=FALSE]
 if(!nrow(e)) return(data.frame(ball_id=b,degree_edges=0,weighted_overlap_degree=0,strongest_neighbour=NA_integer_,strongest_edge_strength=0,neighbour_ids=""))
 nb <- ifelse(e$from==b,e$to,e$from)
 ord <- order(-e$strength,nb)
 data.frame(ball_id=b,degree_edges=nrow(e),weighted_overlap_degree=sum(e$strength),strongest_neighbour=nb[ord[1]],strongest_edge_strength=e$strength[ord[1]],neighbour_ids=paste(sort(nb),collapse=";"),stringsAsFactors=FALSE)
})
gctx <- do.call(rbind,gctx)

# Membership context.
mcnt <- table(mem$point_index)
mem$global_membership_count <- as.integer(mcnt[as.character(mem$point_index)])
mctx <- do.call(rbind,lapply(split(mem,mem$ball_id),function(x)data.frame(
 ball_id=x$ball_id[1],
 n_members=nrow(x),
 mean_member_membership_count=mean(x$global_membership_count),
 median_member_membership_count=stats::median(x$global_membership_count),
 share_members_multicovered=mean(x$global_membership_count>1L),
 stringsAsFactors=FALSE)))

# Descriptive top-three categories, retaining full long compositions separately.
topcats <- do.call(rbind,lapply(c("ruc_category","iuc_category","kmeans_cluster"),function(v)food24_top_categories(comp,v,3L)))
utils::write.csv(topcats,file.path(out,"ball_top_categorical_composition.csv"),row.names=FALSE)

# Representative LSOA = ball member nearest to ball centroid in the same z-scored 3-axis geometry.
x <- as.matrix(rd[,axes,drop=FALSE]); storage.mode(x)<-"double"
mu <- colMeans(x); ss <- apply(x,2,stats::sd); zx <- sweep(sweep(x,2,mu,"-"),2,ss,"/")
rep_rows <- lapply(1:24,function(b){
 m <- mem[mem$ball_id==b,,drop=FALSE]
 idx <- match(m$unit_id,rd$unit_id)
 if(anyNA(idx)) stop("Membership ID absent from readout frame for ball ",b,call.=FALSE)
 zz <- zx[idx,,drop=FALSE]; centroid <- colMeans(zz)
 d <- sqrt(rowSums((zz-matrix(centroid,nrow=nrow(zz),ncol=3,byrow=TRUE))^2))
 k <- which.min(d); r <- rd[idx[k],,drop=FALSE]
 data.frame(ball_id=b,representative_role="member_nearest_ball_axis_centroid",unit_id=r$unit_id,unit_label=r$unit_label,distance_to_ball_axis_centroid=d[k],
  physical_friction_log_nearest_large_store_km=r$physical_friction_log_nearest_large_store_km,
  transport_constraint_no_car_pct=r$transport_constraint_no_car_pct,
  material_constraint_income_deprivation_2025=r$material_constraint_income_deprivation_2025,
  ruc_category=r$ruc_category,iuc_category=r$iuc_category,tcm_shopping_overall=r$tcm_shopping_overall,
  internal_qualifying_store_count=r$internal_qualifying_store_count,kmeans_cluster=r$kmeans_cluster,
  resource_buffered_flag=r$resource_buffered_flag,proximity_capability_constrained_flag=r$proximity_capability_constrained_flag,compound_disadvantage_flag=r$compound_disadvantage_flag,
  stringsAsFactors=FALSE)
})
representatives <- do.call(rbind,rep_rows)

interp <- merge(p,gctx,by="ball_id",all.x=TRUE,sort=FALSE)
interp <- merge(interp,mctx,by="ball_id",all.x=TRUE,sort=FALSE,suffixes=c("","__membership"))
interp <- interp[order(interp$ball_id),]

utils::write.csv(interp,file.path(out,"24_ball_interpretation_matrix.csv"),row.names=FALSE)
utils::write.csv(interp[,c("ball_id","landmark_unit_id","landmark_unit_label","n_members","axis_mean_constraint_z","axis_profile_dispersion","axis_profile_l2_from_sample_mean","highest_constraint_axis","highest_constraint_z","lowest_constraint_axis","lowest_constraint_z","dominant_absolute_axis","dominant_absolute_direction","axis_signature","degree_edges","weighted_overlap_degree","strongest_neighbour","strongest_edge_strength","share_members_multicovered")],file.path(out,"24_ball_descriptive_signatures.csv"),row.names=FALSE)
utils::write.csv(gctx,file.path(out,"24_ball_graph_context.csv"),row.names=FALSE)
utils::write.csv(mctx,file.path(out,"24_ball_membership_context.csv"),row.names=FALSE)
utils::write.csv(representatives,file.path(out,"24_ball_representative_lsoas.csv"),row.names=FALSE)
utils::write.csv(comp,file.path(out,"24_ball_categorical_composition_full.csv"),row.names=FALSE)

cat("FOOD24_BALL_INTERPRETATION_OK balls=",nrow(interp)," representatives=",nrow(representatives),"\n",sep="")

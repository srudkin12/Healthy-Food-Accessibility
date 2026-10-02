#!/usr/bin/env Rscript
options(warn=1); options(stringsAsFactors=FALSE)
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=1L) stop("Usage: 02_build_contrast_evidence.R <package_root>",call.=FALSE)
root <- normalizePath(args[[1L]],winslash="/",mustWork=TRUE)
source(file.path(root,"R","Food24Helpers.R"))
src <- file.path(root,"accepted_p1_4_d","p1_4_d")
out <- file.path(root,"results")
conf <- food24_read_contract(file.path(root,"config","interpretation_contract.csv"))
interp <- utils::read.csv(file.path(out,"24_ball_interpretation_matrix.csv"),stringsAsFactors=FALSE,check.names=FALSE)
comp <- utils::read.csv(file.path(out,"24_ball_categorical_composition_full.csv"),stringsAsFactors=FALSE)
edges <- utils::read.csv(file.path(src,"canonical_edges.csv"),stringsAsFactors=FALSE)

axes <- c("physical_friction_log_nearest_large_store_km","transport_constraint_no_car_pct","material_constraint_income_deprivation_2025")
zcols <- paste0("zdiff__",axes)
tcm <- c("tcm_shopping_walk","tcm_shopping_cycle","tcm_shopping_public_transport","tcm_shopping_drive","tcm_shopping_overall")
tcmz <- paste0("zdiff__",tcm)
scalar_gap_max <- as.numeric(conf[["similar_scalar_gap_max"]])
comp_min <- as.numeric(conf[["composition_distance_min"]])

pairs <- utils::combn(interp$ball_id,2)
rows <- vector("list",ncol(pairs))
for(k in seq_len(ncol(pairs))){
 a<-pairs[1,k]; b<-pairs[2,k]
 ia<-match(a,interp$ball_id); ib<-match(b,interp$ball_id)
 za<-as.numeric(unlist(interp[ia,zcols,drop=FALSE],use.names=FALSE)); zb<-as.numeric(unlist(interp[ib,zcols,drop=FALSE],use.names=FALSE))
 sa<-mean(za); sb<-mean(zb)
 ca<-za-sa; cb<-zb-sb
 ez <- edges[(edges$from==a & edges$to==b)|(edges$from==b & edges$to==a),,drop=FALSE]
 edge_strength <- if(nrow(ez)) ez$strength[1] else 0
 ta<-as.numeric(unlist(interp[ia,tcmz,drop=FALSE],use.names=FALSE)); tb<-as.numeric(unlist(interp[ib,tcmz,drop=FALSE],use.names=FALSE))
 rows[[k]]<-data.frame(
  ball_a=a,ball_b=b,
  scalar_mean_constraint_a=sa,scalar_mean_constraint_b=sb,scalar_gap=abs(sa-sb),
  axis_euclidean_distance=sqrt(sum((za-zb)^2)),
  composition_euclidean_distance=sqrt(sum((ca-cb)^2)),
  sign_disagreement_count=sum(sign(za)!=sign(zb) & abs(za)>.10 & abs(zb)>.10),
  tcm_profile_euclidean_distance=sqrt(sum((ta-tb)^2)),
  ruc_total_variation=food24_total_variation(comp,"ruc_category",a,b),
  iuc_total_variation=food24_total_variation(comp,"iuc_category",a,b),
  kmeans_total_variation=food24_total_variation(comp,"kmeans_cluster",a,b),
  graph_adjacent=nrow(ez)>0,
  edge_strength=edge_strength,
  similar_scalar_constraint=abs(sa-sb)<=scalar_gap_max,
  different_axis_composition=sqrt(sum((ca-cb)^2))>=comp_min,
  candidate_non_scalar_contrast=abs(sa-sb)<=scalar_gap_max && sqrt(sum((ca-cb)^2))>=comp_min,
  signature_a=interp$axis_signature[ia],signature_b=interp$axis_signature[ib],
  stringsAsFactors=FALSE)
}
pairdf<-do.call(rbind,rows)
pairdf<-pairdf[order(!pairdf$candidate_non_scalar_contrast,-pairdf$composition_euclidean_distance,pairdf$scalar_gap,pairdf$ball_a,pairdf$ball_b),]
utils::write.csv(pairdf,file.path(out,"ball_pairwise_configuration_contrasts.csv"),row.names=FALSE)
short<-pairdf[pairdf$candidate_non_scalar_contrast,,drop=FALSE]
maxn<-as.integer(conf[["contrast_shortlist_max"]]); if(nrow(short)>maxn)short<-short[seq_len(maxn),]
utils::write.csv(short,file.path(out,"candidate_non_scalar_contrast_shortlist.csv"),row.names=FALSE)

# For each ball: closest ball on scalar mean and nearest ball in 3-axis configuration.
nearest <- lapply(interp$ball_id,function(b){
 q<-pairdf[pairdf$ball_a==b | pairdf$ball_b==b,,drop=FALSE]
 other<-ifelse(q$ball_a==b,q$ball_b,q$ball_a)
 ks<-which.min(q$scalar_gap); kc<-which.min(q$axis_euclidean_distance)
 data.frame(ball_id=b,nearest_scalar_ball=other[ks],nearest_scalar_gap=q$scalar_gap[ks],nearest_scalar_composition_distance=q$composition_euclidean_distance[ks],nearest_axis_profile_ball=other[kc],nearest_axis_profile_distance=q$axis_euclidean_distance[kc],stringsAsFactors=FALSE)
})
utils::write.csv(do.call(rbind,nearest),file.path(out,"ball_nearest_profile_comparators.csv"),row.names=FALSE)

# Comparator context: how exclusively do balls align with the existing k-means benchmark?
k <- comp[comp$variable=="kmeans_cluster",,drop=FALSE]
kmodal <- do.call(rbind,lapply(split(k,k$ball_id),function(x){x<-x[order(-x$category_share,x$category),]; data.frame(ball_id=x$ball_id[1],modal_kmeans=x$category[1],modal_kmeans_share=x$category_share[1],kmeans_entropy=-sum(x$category_share[x$category_share>0]*log(x$category_share[x$category_share>0])),stringsAsFactors=FALSE)}))
utils::write.csv(kmodal,file.path(out,"kmeans_alignment_by_ball.csv"),row.names=FALSE)

cat("FOOD24_CONTRAST_EVIDENCE_OK pairs=",nrow(pairdf)," non_scalar_candidates=",sum(pairdf$candidate_non_scalar_contrast),"\n",sep="")

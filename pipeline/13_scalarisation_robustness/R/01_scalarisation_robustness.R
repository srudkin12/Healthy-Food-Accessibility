
args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 3) stop("Usage: Rscript script.R BASELINE EXPECTED RESULTS")
baseline <- normalizePath(args[1], mustWork=TRUE)
expected_file <- normalizePath(args[2], mustWork=TRUE)
outdir <- args[3]
dir.create(outdir, recursive=TRUE, showWarnings=FALSE)
options(stringsAsFactors=FALSE, digits=15)

read_csv <- function(x) read.csv(x, check.names=FALSE, stringsAsFactors=FALSE)

find_unique <- function(target) {
  hits <- list.files(baseline, pattern=paste0("^",gsub("([.])","\\\\\\1",target),"$"),
                     recursive=TRUE, full.names=TRUE)
  if (length(hits)==0) stop("Required file not found: ",target)
  prefs <- c("/frozen_input/","/frozen_topology/","/canonical_evidence/",
             "/interpretation/24_ball/","/interpretation_24_ball/")
  if (length(hits)>1) {
    score <- sapply(hits,function(h) sum(sapply(prefs,function(p) grepl(p,h,fixed=TRUE))))
    hits <- hits[score==max(score)]
  }
  if (length(hits)!=1) stop("Could not uniquely resolve ",target,": ",paste(hits,collapse=" | "))
  normalizePath(hits,mustWork=TRUE)
}

sha256_file <- function(path) {
  x <- system2("sha256sum", shQuote(path), stdout=TRUE)
  strsplit(x,"[[:space:]]+")[[1]][1]
}

expdf <- read_csv(expected_file)
expv <- setNames(expdf$value,expdf$key)
numexp <- function(k) as.numeric(expv[[k]])

input <- find_unique("food_hfa_analysis_input.csv")
memberships <- find_unique("canonical_memberships.csv")
fingerprint_file <- find_unique("canonical_topology_fingerprint.txt")

validation <- data.frame(check=character(),status=character(),observed=character(),
                         expected=character(),tolerance=character(),stringsAsFactors=FALSE)
add <- function(check,status,obs,ex,tol="") {
  validation <<- rbind(validation,data.frame(check=check,status=status,
    observed=as.character(obs),expected=as.character(ex),tolerance=as.character(tol),
    stringsAsFactors=FALSE))
}

ih <- sha256_file(input)
add("frozen_input_sha256",ifelse(ih==expv[["frozen_input_sha256"]],"PASS","FAIL"),
    ih,expv[["frozen_input_sha256"]],"exact")

fp <- trimws(readLines(fingerprint_file,warn=FALSE)[1])
add("topology_fingerprint",
    ifelse(grepl(expv[["topology_fingerprint_sha256"]],fp,fixed=TRUE),"PASS","FAIL"),
    fp,expv[["topology_fingerprint_sha256"]],"contains exact fingerprint")

dat <- read_csv(input)
memb <- read_csv(memberships)
add("n_observations",ifelse(nrow(dat)==numexp("n_observations"),"PASS","FAIL"),
    nrow(dat),numexp("n_observations"),"exact")

axis_vars <- c(
  "physical_friction_log_nearest_large_store_km",
  "transport_constraint_no_car_pct",
  "material_constraint_income_deprivation_2025"
)

Z <- as.data.frame(lapply(dat[,axis_vars,drop=FALSE],
                          function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)))
names(Z) <- axis_vars
Z$lsoa21cd <- dat$lsoa21cd

mm <- merge(memb[,c("ball_id","unit_id")],Z,by.x="unit_id",by.y="lsoa21cd",
            all.x=TRUE,sort=FALSE)

balls <- sort(unique(mm$ball_id))
B <- matrix(NA_real_,nrow=length(balls),ncol=3,
            dimnames=list(as.character(balls),c("physical","transport","material")))
for (i in seq_along(balls)) {
  sb <- mm[mm$ball_id==balls[i],,drop=FALSE]
  B[i,] <- c(mean(sb[[axis_vars[1]]]),mean(sb[[axis_vars[2]]]),mean(sb[[axis_vars[3]]]))
}
add("n_balls",ifelse(nrow(B)==numexp("n_balls"),"PASS","FAIL"),
    nrow(B),numexp("n_balls"),"exact")

write.csv(data.frame(ball_id=balls,B),file.path(outdir,"ball_profiles_recomputed.csv"),row.names=FALSE)

# all unordered ball pairs
cmb <- t(combn(seq_along(balls),2))
profile_dist <- apply(cmb,1,function(ix) sqrt(sum((B[ix[1],]-B[ix[2],])^2)))

score_pairs <- function(w) {
  s <- as.numeric(B %*% w)
  gaps <- apply(cmb,1,function(ix) abs(s[ix[1]]-s[ix[2]]))
  data.frame(
    ball_a=balls[cmb[,1]],ball_b=balls[cmb[,2]],
    scalar_gap=gaps,profile_distance=profile_dist
  )
}

eq <- score_pairs(c(1/3,1/3,1/3))
eq$n_main <- (eq$scalar_gap < 0.25 & eq$profile_distance > 1.0)
nmain <- sum(eq$n_main)
add("equal_weight_main_screen_count",
    ifelse(nmain==numexp("equal_weight_contrasts_main_screen"),"PASS","FAIL"),
    nmain,numexp("equal_weight_contrasts_main_screen"),"exact")

p1318 <- eq[eq$ball_a==13 & eq$ball_b==18,]
add("ball13_18_scalar_gap",
    ifelse(abs(p1318$scalar_gap-numexp("ball13_18_scalar_gap"))<1e-10,"PASS","FAIL"),
    p1318$scalar_gap,numexp("ball13_18_scalar_gap"),"1e-10")
add("ball13_18_profile_distance",
    ifelse(abs(p1318$profile_distance-numexp("ball13_18_profile_distance"))<5e-6,"PASS","FAIL"),
    p1318$profile_distance,numexp("ball13_18_profile_distance"),"5e-6")
write.csv(eq,file.path(outdir,"equal_weight_all_pair_metrics.csv"),row.names=FALSE)

# Analysis A: threshold sensitivity
gap_thr <- c(0.05,0.10,0.15,0.20,0.25,0.30)
dist_thr <- c(1.0,1.5,2.0,2.5,3.0)
grid <- expand.grid(scalar_gap_threshold=gap_thr,
                    profile_distance_threshold=dist_thr)
grid$n_qualifying_pairs <- mapply(function(g,d)
  sum(eq$scalar_gap < g & eq$profile_distance > d),
  grid$scalar_gap_threshold,grid$profile_distance_threshold)
write.csv(grid,file.path(outdir,"screen_threshold_sensitivity.csv"),row.names=FALSE)

# wide table useful for supplement
wide <- reshape(grid,idvar="scalar_gap_threshold",
                timevar="profile_distance_threshold",direction="wide")
write.csv(wide,file.path(outdir,"screen_threshold_sensitivity_wide.csv"),row.names=FALSE)

# Analysis B: deterministic weight simplex at 0.05 resolution
weights <- list()
k <- 1
for (ip in 0:20) {
  for (it in 0:(20-ip)) {
    im <- 20-ip-it
    weights[[k]] <- c(ip/20,it/20,im/20)
    k <- k+1
  }
}
W <- do.call(rbind,weights)
colnames(W) <- c("w_physical","w_transport","w_material")

wrows <- vector("list",nrow(W))
pair_freq <- matrix(FALSE,nrow=nrow(W),ncol=nrow(cmb))
for (r in seq_len(nrow(W))) {
  q <- score_pairs(W[r,])
  pair_freq[r,] <- (q$scalar_gap < 0.25 & q$profile_distance > 1.0)
  wrows[[r]] <- data.frame(
    weight_id=r,
    w_physical=W[r,1],w_transport=W[r,2],w_material=W[r,3],
    min_weight=min(W[r,]),
    max_weight=max(W[r,]),
    n_gap025_dist1=sum(q$scalar_gap<0.25 & q$profile_distance>1.0),
    n_gap025_dist2=sum(q$scalar_gap<0.25 & q$profile_distance>2.0),
    n_gap025_dist3=sum(q$scalar_gap<0.25 & q$profile_distance>3.0)
  )
}
wg <- do.call(rbind,wrows)
write.csv(wg,file.path(outdir,"scalar_weight_grid_005.csv"),row.names=FALSE)

subset_summary <- function(label,keep) {
  x <- wg[keep,,drop=FALSE]
  data.frame(
    subset=label,
    n_weight_schemes=nrow(x),
    min_weight_required=if (label=="all_nonnegative") 0 else
                        if (label=="each_weight_ge_010") 0.10 else 0.20,
    dist1_min=min(x$n_gap025_dist1),dist1_median=median(x$n_gap025_dist1),
    dist1_max=max(x$n_gap025_dist1),
    dist2_min=min(x$n_gap025_dist2),dist2_median=median(x$n_gap025_dist2),
    dist2_max=max(x$n_gap025_dist2),
    dist3_min=min(x$n_gap025_dist3),dist3_median=median(x$n_gap025_dist3),
    dist3_max=max(x$n_gap025_dist3)
  )
}
ss <- rbind(
  subset_summary("all_nonnegative",rep(TRUE,nrow(wg))),
  subset_summary("each_weight_ge_010",wg$min_weight>=0.10-1e-12),
  subset_summary("each_weight_ge_020",wg$min_weight>=0.20-1e-12)
)
write.csv(ss,file.path(outdir,"scalar_weight_sensitivity_summary.csv"),row.names=FALSE)

# Pair qualification frequency in the balanced (>= .20 each) subset
bal <- wg$min_weight>=0.20-1e-12
freq <- colMeans(pair_freq[bal,,drop=FALSE])
pf <- data.frame(
  ball_a=balls[cmb[,1]],ball_b=balls[cmb[,2]],
  profile_distance=profile_dist,
  qualifying_share_balanced_weights=freq
)
pf <- pf[order(-pf$qualifying_share_balanced_weights,-pf$profile_distance),]
write.csv(pf,file.path(outdir,"pair_frequency_balanced_weights.csv"),row.names=FALSE)

# Main pre-submission summary
strict <- grid[abs(grid$scalar_gap_threshold-0.05)<1e-12 &
               abs(grid$profile_distance_threshold-3.0)<1e-12,]
balrow <- ss[ss$subset=="each_weight_ge_020",]
main_summary <- data.frame(
  metric=c(
    "Equal-weight qualifying pairs: gap<0.25, distance>1",
    "Strict threshold qualifying pairs: gap<0.05, distance>3",
    "Balanced weight schemes (each dimension >=0.20)",
    "Balanced weights: minimum qualifying pairs, distance>1",
    "Balanced weights: median qualifying pairs, distance>1",
    "Balanced weights: maximum qualifying pairs, distance>1",
    "Balanced weights: minimum qualifying pairs, distance>2",
    "Balanced weights: minimum qualifying pairs, distance>3",
    "Ball 13/18 qualifying share across balanced weights"
  ),
  value=c(
    nmain,
    strict$n_qualifying_pairs,
    balrow$n_weight_schemes,
    balrow$dist1_min,
    balrow$dist1_median,
    balrow$dist1_max,
    balrow$dist2_min,
    balrow$dist3_min,
    pf$qualifying_share_balanced_weights[pf$ball_a==13 & pf$ball_b==18]
  )
)
write.csv(main_summary,file.path(outdir,"pre_submission_scalarisation_summary.csv"),row.names=FALSE)

write.csv(validation,file.path(outdir,"validation_report.csv"),row.names=FALSE)
status <- if (all(validation$status=="PASS")) "PASS" else "FAIL"
writeLines(c(
  paste0("HFA_SCALARISATION_ROBUSTNESS=",status),
  paste0("CHECKS=",nrow(validation)),
  paste0("FAILURES=",sum(validation$status!="PASS"))
),file.path(outdir,"STATUS.txt"))

if (status!="PASS") stop("Validation failed. Inspect results/validation_report.csv")

cat("HFA_SCALARISATION_ROBUSTNESS=PASS\n")
cat("Equal-weight main-screen pairs:",nmain,"\n")
cat("Strict screen (gap<0.05, distance>3) pairs:",strict$n_qualifying_pairs,"\n")
cat("Balanced weighting schemes:",balrow$n_weight_schemes,"\n")
cat("Balanced weights, distance>1 count range:",
    balrow$dist1_min,"to",balrow$dist1_max,
    "(median",balrow$dist1_median,")\n")

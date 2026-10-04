args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 4L) stop("usage: 02_extended_knn.R WORKTREE OUTPUT_DIR EXPECTED_CSV MAX_K")
root <- normalizePath(args[[1]], mustWork=TRUE)
outdir <- normalizePath(args[[2]], mustWork=FALSE)
expected_path <- normalizePath(args[[3]], mustWork=TRUE)
max_k <- as.integer(args[[4]])
if (!is.finite(max_k) || max_k < 50L || max_k > 500L) stop("MAX_K must be an integer between 50 and 500")
dir.create(outdir, recursive=TRUE, showWarnings=FALSE)

stage12_lib <- file.path(root, "pipeline", "12_spatial_closure", ".Rlib")
if (dir.exists(stage12_lib)) .libPaths(unique(c(stage12_lib, .libPaths())))
need <- c("sf","RANN")
missing <- need[!vapply(need, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing)) stop("Missing required R package(s): ", paste(missing, collapse=", "), ". Stage 12 should already have installed these in its .Rlib.")

input_path <- file.path(root, "pipeline", "07_input_freeze", "food_application", "results", "p1_4_a", "frozen", "food_hfa_analysis_input.csv")
geom_path <- file.path(root, "pipeline", "04_retail_access", "data_raw", "ons", "lsoa21_bgc_v5_england.gpkg")
old_summary_path <- file.path(root, "pipeline", "12_spatial_closure", "results", "topology_geography_knn_summary.csv")
if (!file.exists(input_path)) stop("Frozen analytical input not found: ", input_path)
if (!file.exists(geom_path)) stop("Canonical geometry not found: ", geom_path)
if (!file.exists(old_summary_path)) stop("Existing Stage 12 KNN summary not found: ", old_summary_path)

x <- read.csv(input_path, stringsAsFactors=FALSE, check.names=FALSE)
if (nrow(x) != 33755L) stop("Unexpected analytical population: ", nrow(x), " != 33755")
if (!"lsoa21cd" %in% names(x) || anyDuplicated(x$lsoa21cd)) stop("lsoa21cd missing or duplicated in frozen input")
axis_cols <- c(
  "physical_friction_log_nearest_large_store_km",
  "transport_constraint_no_car_pct",
  "material_constraint_income_deprivation_2025"
)
if (!all(axis_cols %in% names(x))) stop("Required frozen constraint columns are missing")
X <- scale(as.matrix(x[, axis_cols, drop=FALSE]))
if (any(!is.finite(X))) stop("Non-finite standardised constraint value")

layers <- sf::st_layers(geom_path)$name
layer <- if ("lsoa21_bgc_v5_england" %in% layers) "lsoa21_bgc_v5_england" else if (length(layers)==1L) layers[[1]] else stop("Could not uniquely identify LSOA geometry layer")
g <- suppressMessages(sf::st_read(geom_path, layer=layer, quiet=TRUE))
id_candidates <- names(g)[tolower(names(g)) == "lsoa21cd"]
if (length(id_candidates) != 1L) stop("Could not uniquely identify lsoa21cd in geometry")
id_col <- id_candidates[[1]]
ids <- as.character(g[[id_col]])
if (anyDuplicated(ids)) stop("Duplicate LSOA IDs in geometry")
idx <- match(x$lsoa21cd, ids)
if (anyNA(idx)) stop("Geometry is missing ", sum(is.na(idx)), " analytical LSOAs")
g <- g[idx, , drop=FALSE]
if (is.na(sf::st_crs(g))) stop("Geometry CRS is missing")
g <- sf::st_transform(g, 27700)
pts <- suppressWarnings(sf::st_point_on_surface(g))
coords <- sf::st_coordinates(pts)
if (nrow(coords) != nrow(x) || ncol(coords) < 2L) stop("Representative-point coordinates are malformed")
coords <- coords[,1:2,drop=FALSE]
if (any(!is.finite(coords))) stop("Non-finite representative-point coordinate")

n <- nrow(x)
message("Computing exact configurational and geographic nearest neighbours through k=", max_k, " for n=", n)
cfg_raw <- RANN::nn2(data=X, query=X, k=max_k+1L, eps=0)$nn.idx
geo_raw <- RANN::nn2(data=coords, query=coords, k=max_k+1L, eps=0)$nn.idx
self <- seq_len(n)
if (!all(cfg_raw[,1L] == self)) stop("Configurational self-neighbour is not consistently first; fail closed")
if (!all(geo_raw[,1L] == self)) stop("Geographic self-neighbour is not consistently first; fail closed")
cfg <- cfg_raw[,-1L,drop=FALSE]
geo <- geo_raw[,-1L,drop=FALSE]

# For each area and k, count common members of the first-k configurational and geographic sets.
# A configurational neighbour at rank p enters the intersection when k reaches max(p, geographic-rank).
shared <- matrix(0L, nrow=n, ncol=max_k)
pos <- seq_len(max_k)
for (i in seq_len(n)) {
  m <- match(cfg[i,], geo[i,], nomatch=0L)
  threshold <- rep.int(max_k+1L, max_k)
  hit <- m > 0L
  threshold[hit] <- pmax(pos[hit], m[hit])
  freq <- tabulate(threshold[threshold <= max_k], nbins=max_k)
  shared[i,] <- cumsum(freq)
  if (i %% 5000L == 0L) message("  overlap rows completed: ", i, "/", n)
}

core <- vector("list", max_k)
for (k in seq_len(max_k)) {
  s <- shared[,k]
  j <- s / (2*k - s)
  core[[k]] <- data.frame(
    k=k,
    mean_jaccard=mean(j),
    median_jaccard=median(j),
    q10_jaccard=as.numeric(quantile(j,0.10,names=FALSE,type=7)),
    q90_jaccard=as.numeric(quantile(j,0.90,names=FALSE,type=7)),
    zero_overlap_share=mean(s==0L),
    mean_shared_neighbours=mean(s),
    stringsAsFactors=FALSE
  )
}
core <- do.call(rbind, core)
write.csv(core, file.path(outdir,"knn_overlap_extended_k1_100.csv"), row.names=FALSE, quote=TRUE)

# Compute the original distance-link columns for the manuscript display grid only.
dx <- matrix(coords[as.vector(cfg),1L], nrow=n, ncol=max_k) - coords[,1L]
dy <- matrix(coords[as.vector(cfg),2L], nrow=n, ncol=max_k) - coords[,2L]
d_km <- sqrt(dx*dx + dy*dy) / 1000
rm(dx,dy,cfg_raw,geo_raw); invisible(gc())
display_k <- seq.int(5L, min(100L,max_k), by=5L)
display <- core[match(display_k, core$k),,drop=FALSE]
display$median_geo_distance_of_topological_neighbours_km <- NA_real_
display$share_topological_neighbour_links_gt25km <- NA_real_
display$share_topological_neighbour_links_gt50km <- NA_real_
display$share_topological_neighbour_links_gt100km <- NA_real_
for (r in seq_along(display_k)) {
  k <- display_k[[r]]
  z <- as.vector(d_km[,seq_len(k),drop=FALSE])
  display$median_geo_distance_of_topological_neighbours_km[r] <- median(z)
  display$share_topological_neighbour_links_gt25km[r] <- mean(z > 25)
  display$share_topological_neighbour_links_gt50km[r] <- mean(z > 50)
  display$share_topological_neighbour_links_gt100km[r] <- mean(z > 100)
}
# Maintain the exact column order consumed by Stage 14.
display <- display[,c(
  "k","mean_jaccard","median_jaccard","q10_jaccard","q90_jaccard","zero_overlap_share","mean_shared_neighbours",
  "median_geo_distance_of_topological_neighbours_km","share_topological_neighbour_links_gt25km",
  "share_topological_neighbour_links_gt50km","share_topological_neighbour_links_gt100km"
)]
write.csv(display, file.path(outdir,"topology_geography_knn_summary_extended_display.csv"), row.names=FALSE, quote=TRUE)

expected <- read.csv(expected_path, stringsAsFactors=FALSE, check.names=FALSE)
observed <- display[match(expected$k, display$k), names(expected), drop=FALSE]
if (anyNA(observed$k)) stop("Display grid does not contain all frozen benchmark k values")
validation <- list(); v <- 0L
for (i in seq_len(nrow(expected))) {
  for (nm in setdiff(names(expected),"k")) {
    v <- v+1L
    e <- expected[[nm]][i]; o <- observed[[nm]][i]
    tol <- if (grepl("distance",nm)) 1e-8 else 1e-12
    validation[[v]] <- data.frame(k=expected$k[i], metric=nm, expected=e, observed=o, abs_diff=abs(o-e), tolerance=tol, pass=is.finite(o) && abs(o-e)<=tol)
  }
}
validation <- do.call(rbind, validation)
write.csv(validation, file.path(outdir,"knn_frozen_benchmark_validation.csv"), row.names=FALSE, quote=TRUE)
if (!all(validation$pass)) {
  bad <- validation[!validation$pass,,drop=FALSE]
  print(bad)
  stop("FAIL_CLOSED: extended KNN does not reproduce one or more frozen k=10/25/50 benchmark values")
}

# Also verify that the local Stage 12 three-row table itself still equals the expected benchmark file.
old <- read.csv(old_summary_path, stringsAsFactors=FALSE, check.names=FALSE)
if (!identical(as.integer(old$k), as.integer(expected$k))) stop("Existing Stage 12 KNN k grid is no longer 10,25,50")
for (nm in setdiff(names(expected),"k")) {
  tol <- if (grepl("distance",nm)) 1e-8 else 1e-12
  if (any(abs(old[[nm]] - expected[[nm]]) > tol)) stop("Existing Stage 12 KNN summary differs from frozen benchmark in ",nm)
}

writeLines(c(
  "EXTENDED_KNN_STATUS=PASS",
  paste0("N_LSOA=",n),
  paste0("MAX_K=",max_k),
  paste0("DISPLAY_K=",paste(display_k,collapse=";")),
  "FROZEN_BENCHMARK_K=10;25;50",
  "FROZEN_BENCHMARK_REPRODUCTION=PASS"
), file.path(outdir,"EXTENDED_KNN_STATUS.txt"))
message("EXTENDED_KNN_STATUS=PASS")

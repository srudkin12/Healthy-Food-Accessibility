source("R/functions.R")

ROOT <- root_dir()
d <- read.csv(file.path(ROOT, "data_staged", "hfa_f4_capability_dataset.csv"),
              check.names = FALSE)

scalar <- matrix(d$z_physical_distance, ncol = 1)
cap3 <- as.matrix(d[, c("z_physical_distance", "z_car_none", "z_income_deprivation")])

run_k <- function(k) {
  # k+1 because self is returned first.
  ns <- RANN::nn2(scalar, scalar, k = k + 1)$nn.idx[, -1, drop = FALSE]
  nc <- RANN::nn2(cap3, cap3, k = k + 1)$nn.idx[, -1, drop = FALSE]
  jac <- jaccard_rows(ns, nc)

  data.frame(
    k = k,
    mean_jaccard = mean(jac, na.rm = TRUE),
    median_jaccard = median(jac, na.rm = TRUE),
    p25_jaccard = quantile(jac, .25, na.rm = TRUE),
    p75_jaccard = quantile(jac, .75, na.rm = TRUE),
    share_zero_jaccard = mean(jac == 0, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

res <- rbind(run_k(25L), run_k(50L))
write.csv(res,
          file.path(ROOT, "results", "f4", "scalar_vs_capability_peer_overlap.csv"),
          row.names = FALSE)

cat("STRUCTURAL_PEER_OVERLAP_OK k25_mean=",
    sprintf("%.6f", res$mean_jaccard[res$k == 25]), "\n", sep = "")

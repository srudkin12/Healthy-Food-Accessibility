source("R/functions.R")

ROOT <- root_dir()
d <- read.csv(file.path(ROOT, "data_staged", "hfa_f4_inherited.csv"),
              check.names = FALSE)

res <- list()

for (ruc in sort(unique(d$ruc2021))) {
  z <- d[d$ruc2021 == ruc, ]
  scalar <- matrix(z$z_physical_distance, ncol = 1)
  cap3 <- as.matrix(z[, c("z_physical_distance","z_car_none","z_income_deprivation")])

  for (k0 in c(25L, 50L)) {
    k <- min(k0, nrow(z) - 1L)
    assert_true(k >= 5L, paste0("RUC class too small: ", ruc))

    ns <- RANN::nn2(scalar, scalar, k = k + 1L)$nn.idx[, -1, drop = FALSE]
    nc <- RANN::nn2(cap3, cap3, k = k + 1L)$nn.idx[, -1, drop = FALSE]
    jac <- jaccard_rows(ns, nc)

    res[[length(res) + 1L]] <- data.frame(
      ruc2021 = ruc,
      n = nrow(z),
      k_requested = k0,
      k_used = k,
      mean_jaccard = mean(jac),
      median_jaccard = median(jac),
      p25_jaccard = quantile(jac, .25),
      p75_jaccard = quantile(jac, .75),
      share_zero_jaccard = mean(jac == 0),
      stringsAsFactors = FALSE
    )
  }
}

out <- do.call(rbind, res)

# Weighted aggregate across RUC classes.
agg <- do.call(rbind, lapply(c(25L,50L), function(k0) {
  z <- out[out$k_requested == k0, ]
  data.frame(
    ruc2021 = "WEIGHTED_ACROSS_RUC_CLASSES",
    n = sum(z$n),
    k_requested = k0,
    k_used = NA_integer_,
    mean_jaccard = weighted.mean(z$mean_jaccard, z$n),
    median_jaccard = NA_real_,
    p25_jaccard = NA_real_,
    p75_jaccard = NA_real_,
    share_zero_jaccard = weighted.mean(z$share_zero_jaccard, z$n),
    stringsAsFactors = FALSE
  )
}))

out2 <- rbind(out, agg)
write.csv(out2,
          file.path(ROOT, "results", "f5", "ruc_conditioned_peer_overlap.csv"),
          row.names = FALSE)

cat("RUC_CONDITIONED_PEER_OVERLAP_OK weighted_k25_mean=",
    sprintf("%.6f", agg$mean_jaccard[agg$k_requested == 25]),
    "\n", sep = "")

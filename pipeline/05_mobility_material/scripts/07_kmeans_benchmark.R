source("R/functions.R")

ROOT <- root_dir()
d <- read.csv(file.path(ROOT, "data_staged", "hfa_f4_capability_dataset.csv"),
              check.names = FALSE)

X <- as.matrix(d[, c("z_physical_distance", "z_car_none", "z_income_deprivation")])
set.seed(20260918)

fits <- list()
qa <- list()

for (k in 2:8) {
  fit <- stats::kmeans(X, centers = k, nstart = 50, iter.max = 200)
  W <- fit$tot.withinss
  B <- fit$betweenss
  n <- nrow(X)
  ch <- (B / (k - 1)) / (W / (n - k))

  qa[[length(qa) + 1L]] <- data.frame(
    k = k,
    total_withinss = W,
    betweenss = B,
    between_share = B / fit$totss,
    calinski_harabasz = ch,
    min_cluster_n = min(fit$size),
    max_cluster_n = max(fit$size),
    stringsAsFactors = FALSE
  )
  fits[[as.character(k)]] <- fit
}

qa_df <- do.call(rbind, qa)
write.csv(qa_df,
          file.path(ROOT, "results", "f4", "kmeans_k2_k8_benchmark.csv"),
          row.names = FALSE)

best_k <- qa_df$k[which.max(qa_df$calinski_harabasz)]
best <- fits[[as.character(best_k)]]
d$kmeans_killtest_cluster <- best$cluster

profiles <- aggregate(
  cbind(
    geolytix_large_nearest_km,
    car_none_pct,
    income_deprivation_score_2025
  ) ~ kmeans_killtest_cluster,
  data = d,
  FUN = mean
)
profiles$n <- as.integer(table(d$kmeans_killtest_cluster)[as.character(profiles$kmeans_killtest_cluster)])

write.csv(profiles,
          file.path(ROOT, "results", "f4", "kmeans_best_cluster_profiles.csv"),
          row.names = FALSE)
write.csv(d[, c("lsoa21cd", "kmeans_killtest_cluster")],
          file.path(ROOT, "results", "f4", "kmeans_best_assignments.csv"),
          row.names = FALSE)

cat("KMEANS_BENCHMARK_OK best_k=", best_k, "\n", sep = "")

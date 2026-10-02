source("R/functions.R")

ROOT <- root_dir()
infile <- file.path(ROOT, "data_raw", "geolytix", "geolytix_q1_2023_reconstruction.csv")
assert_true(file.exists(infile), "Geolytix staged CSV missing")

x <- read.csv(infile, check.names = FALSE)
x$size_band_clean <- clean_size_band(x$size_band)
x$size_class_reconstructed <- classify_size_band(x$size_band_clean)

freq <- as.data.frame(table(x$size_band_clean, useNA = "ifany"), stringsAsFactors = FALSE)
names(freq) <- c("size_band", "n")
freq$size_class_reconstructed <- classify_size_band(freq$size_band)

write.csv(freq,
          file.path(ROOT, "results", "f3", "geolytix_size_band_audit.csv"),
          row.names = FALSE)

unknown <- !is.na(x$size_band_clean) & is.na(x$size_class_reconstructed)
if (any(unknown)) {
  write.csv(
    sort(table(x$size_band_clean[unknown]), decreasing = TRUE),
    file.path(ROOT, "results", "f3", "geolytix_unclassified_size_bands.csv")
  )
  stop("Unclassified non-empty Geolytix size bands found. See geolytix_unclassified_size_bands.csv",
       call. = FALSE)
}

x$large_store_student_rule <- x$size_class_reconstructed %in%
  c("B_280_to_1400m2", "C_1400_to_2800m2", "D_2800m2_plus")

x$valid_bng <- is.finite(suppressWarnings(as.numeric(x$bng_e))) &
               is.finite(suppressWarnings(as.numeric(x$bng_n)))

n_large <- sum(x$large_store_student_rule, na.rm = TRUE)
n_small <- sum(x$size_class_reconstructed == "A_under_280m2", na.rm = TRUE)

assert_true(n_large > 1000L, paste0("Implausibly few >=280m2 stores: ", n_large))
assert_true(n_small > 1000L, paste0("Implausibly few <280m2 stores: ", n_small))

outfile <- file.path(ROOT, "data_staged", "geolytix_q1_2023_classified.csv")
write.csv(x, outfile, row.names = FALSE)

cat("GEOLYTIX_CLASSIFICATION_OK rows=", nrow(x),
    " large_student_rule=", n_large,
    " small_under_280m2=", n_small, "\n", sep = "")

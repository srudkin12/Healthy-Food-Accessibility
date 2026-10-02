source("R/functions.R")

ROOT <- root_dir()
PARENT <- dirname(ROOT)
f3_dir <- file.path(PARENT, "04_retail_access")
dataset_in <- file.path(f3_dir, "data_staged", "hfa_f3_functional_retail_access.csv")
audit_in <- file.path(f3_dir, "results", "f3", "AUDIT_F3.txt")

assert_true(file.exists(dataset_in), paste0("Missing F3 dataset: ", dataset_in))
assert_true(file.exists(audit_in), paste0("Missing F3 audit: ", audit_in))

x <- read.csv(dataset_in, check.names = FALSE)
assert_true(nrow(x) == 33755L, "F3 row count is not 33755")
assert_true(length(unique(x$lsoa21cd)) == 33755L, "F3 LSOA21 codes not unique")
assert_true(all(is.finite(x$geolytix_large_nearest_km)),
            "F3 nearest large-store distance has missing/non-finite values")
assert_true(all(is.na(x$jts2019_public_transport_min_validated)),
            "F2A PT exclusion was not preserved into F3")

audit <- paste(readLines(audit_in, warn = FALSE), collapse = "\n")
assert_true(grepl("PASS_FOR_HFA_F4_CAPABILITY_AND_EMPIRICAL_KILL_TESTS", audit, fixed = TRUE),
            "F3 pass marker missing")

file.copy(dataset_in, file.path(ROOT, "data_staged", "hfa_f3_inherited.csv"),
          overwrite = TRUE)

writeLines(c(
  "F3 ERRATUM RECORDED AT F4 INHERITANCE",
  "",
  "The F3 descriptive parser labelled raw size band:",
  "15,069 < 30,138 ft2 (1,400 < 2,800 m2)",
  "as B_280_to_1400m2 rather than C_1400_to_2800m2.",
  "",
  "Impact assessment:",
  "- no F3 inclusion/exclusion decision changes;",
  "- both B and C exceed 280 m2 and were included in large_store_student_rule;",
  "- nearest distances, catchment counts, decay scores, internal large-store counts, and student-sample reconstruction are unchanged;",
  "- F4 does not use the erroneous descriptive B/C class label."
), file.path(ROOT, "results", "f4", "F3_ERRATUM.txt"))

cat("F3_INHERITANCE_OK rows=33755\n")

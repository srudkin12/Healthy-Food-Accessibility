source("R/functions.R")

ROOT <- root_dir()
PARENT <- dirname(ROOT)
f4_dir <- file.path(PARENT, "05_mobility_material")

dataset <- file.path(f4_dir, "data_staged", "hfa_f4_capability_dataset.csv")
audit <- file.path(f4_dir, "results", "f4", "AUDIT_F4.txt")
assign <- file.path(f4_dir, "results", "f4", "candidate_configuration_assignments.csv")
km <- file.path(f4_dir, "results", "f4", "kmeans_best_assignments.csv")

for (p in c(dataset, audit, assign, km)) {
  assert_true(file.exists(p), paste0("Missing F4 dependency: ", p))
}

d <- read.csv(dataset, check.names = FALSE)
assert_true(nrow(d) == 33755L, "F4 dataset is not 33755 rows")
assert_true(length(unique(d$lsoa21cd)) == 33755L, "F4 LSOA codes not unique")

audit_txt <- paste(readLines(audit, warn = FALSE), collapse = "\n")
assert_true(grepl("PASS_FOR_HFA_F5_DIGITAL_CLOSURE", audit_txt, fixed = TRUE),
            "F4 pass marker missing")

file.copy(dataset, file.path(ROOT, "data_staged", "hfa_f4_inherited.csv"), overwrite = TRUE)
file.copy(assign, file.path(ROOT, "data_staged", "f4_candidate_configuration_assignments.csv"),
          overwrite = TRUE)
file.copy(km, file.path(ROOT, "data_staged", "f4_kmeans_best_assignments.csv"),
          overwrite = TRUE)

cat("F4_INHERITANCE_OK rows=33755\n")

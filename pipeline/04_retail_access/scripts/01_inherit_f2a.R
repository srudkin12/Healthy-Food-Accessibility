source("R/functions.R")

ROOT <- root_dir()
PARENT <- dirname(ROOT)

f2a_dir <- file.path(PARENT, "03_jts_validation")
dataset_in <- file.path(f2a_dir, "data_staged", "hfa_f2_physical_access_VALIDATED.csv")
contract_in <- file.path(f2a_dir, "results", "f2a", "F2_VALIDATED_DOWNSTREAM_CONTRACT.csv")
audit_in <- file.path(f2a_dir, "results", "f2a", "AUDIT_F2A.txt")

assert_true(file.exists(dataset_in),
            paste0("Missing F2A validated dataset: ", dataset_in))
assert_true(file.exists(contract_in),
            paste0("Missing F2A downstream contract: ", contract_in))
assert_true(file.exists(audit_in),
            paste0("Missing F2A audit: ", audit_in))

x <- read.csv(dataset_in, check.names = FALSE)
assert_true(nrow(x) == 33755L, paste0("F2A row count is ", nrow(x), " not 33755"))
assert_true(length(unique(x$lsoa21cd)) == 33755L, "F2A LSOA21 codes are not unique")

contract <- read.csv(contract_in, check.names = FALSE)
pt_row <- contract[contract$field == "jts2019_public_transport_min", , drop = FALSE]
assert_true(nrow(pt_row) == 1L, "F2A PT contract row missing")
assert_true(pt_row$downstream_status[1] == "EXCLUDE",
            "F2A contract does not exclude JTS2019 public transport")

audit_txt <- paste(readLines(audit_in, warn = FALSE), collapse = "\n")
assert_true(grepl("PASS_F2A_WITH_JTS2019_PT_MIN_EXCLUSION", audit_txt, fixed = TRUE),
            "F2A pass marker missing")

ensure_dir(file.path(ROOT, "data_staged"))
file.copy(dataset_in,
          file.path(ROOT, "data_staged", "hfa_f2a_inherited.csv"),
          overwrite = TRUE)

write.csv(
  data.frame(
    source_directory = f2a_dir,
    rows = nrow(x),
    unique_lsoa21 = length(unique(x$lsoa21cd)),
    jts2019_pt_status = pt_row$downstream_status[1],
    inherited_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    stringsAsFactors = FALSE
  ),
  file.path(ROOT, "results", "f3", "f2a_inheritance.csv"),
  row.names = FALSE
)

cat("F2A_INHERITANCE_OK rows=33755 JTS2019_PT=EXCLUDE\n")

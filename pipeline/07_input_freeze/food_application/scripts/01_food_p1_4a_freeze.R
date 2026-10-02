#!/usr/bin/env Rscript
options(warn = 1)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop(
    "Usage: 01_food_p1_4a_freeze.R <package_root> <project_root> <framework_root>",
    call. = FALSE
  )
}

package_root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
project_root <- normalizePath(args[[2L]], winslash = "/", mustWork = TRUE)
framework_root <- normalizePath(args[[3L]], winslash = "/", mustWork = TRUE)

app_root <- file.path(package_root, "food_application")
result_root <- file.path(app_root, "results", "p1_4_a")
work_root <- file.path(app_root, "work")
dir.create(result_root, recursive = TRUE, showWarnings = FALSE)
dir.create(work_root, recursive = TRUE, showWarnings = FALSE)

source(file.path(framework_root, "framework", "R", "TDABMArtifactProvenance.R"))
source(file.path(framework_root, "framework", "R", "TDABMValueContracts.R"))
source(file.path(framework_root, "framework", "R", "TDABMInputFreeze.R"))
source(file.path(app_root, "R", "FoodHFAAdapter.R"))

expected_rows <- 33755L
preferred_roots <- food_candidate_roots(project_root)

if (!length(preferred_roots)) {
  stop(
    "Neither expected previous Food parent exists under ",
    project_root,
    call. = FALSE
  )
}

csv_files <- unique(unlist(lapply(
  preferred_roots,
  function(r) list.files(
    r,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
), use.names = FALSE))

if (!length(csv_files)) {
  stop("No CSV files were found in the expected Food parent releases.", call. = FALSE)
}

inventory_rows <- list()
valid <- list()

for (f in sort(csv_files)) {
  z <- food_read_candidate(f, expected_rows = expected_rows)
  inventory_rows[[length(inventory_rows) + 1L]] <- data.frame(
    file = normalizePath(f, winslash = "/", mustWork = TRUE),
    parent = if (grepl("Radius_Refinement_and_Equivalence_v3_0", f, fixed = TRUE)) {
      "F6B_V3_0"
    } else if (grepl("F5_Digital_RUC_Closure_v1_0_2", f, fixed = TRUE)) {
      "F5_V1_0_2"
    } else {
      "OTHER"
    },
    matched_required_columns = z$matched %||% 0L,
    rows = if (!is.null(z$data)) nrow(z$data) else NA_integer_,
    label_present = isTRUE(z$label_present),
    valid = isTRUE(z$ok),
    reason = z$reason %||% "UNKNOWN",
    stringsAsFactors = FALSE
  )
  if (isTRUE(z$ok)) {
    valid[[f]] <- z$data
  }
}

inventory <- do.call(rbind, inventory_rows)
utils::write.csv(
  inventory,
  file.path(result_root, "source_candidate_inventory.csv"),
  row.names = FALSE
)

if (!length(valid)) {
  stop(
    "No 33,755-row candidate containing the complete verified Food HFA source-variable contract was found. See source_candidate_inventory.csv.",
    call. = FALSE
  )
}

valid_paths <- names(valid)
semantic_sha <- vapply(
  valid,
  food_semantic_sha256,
  character(1),
  sha_fun = tdabm_if_sha256
)

candidate_equivalence <- data.frame(
  file = valid_paths,
  semantic_sha256 = unname(semantic_sha),
  stringsAsFactors = FALSE
)
utils::write.csv(
  candidate_equivalence,
  file.path(result_root, "candidate_semantic_equivalence.csv"),
  row.names = FALSE
)

signature_groups <- unique(semantic_sha)
if (length(signature_groups) != 1L) {
  stop(
    "Multiple non-equivalent valid Food analytical candidates were found. Selection is blocked rather than guessed. See candidate_semantic_equivalence.csv.",
    call. = FALSE
  )
}

preference_rank <- function(f) {
  if (grepl(
    "/pipeline/06_contextual_readouts/",
    f,
    fixed = TRUE
  )) return(1L)
  if (grepl(
    "/pipeline/06_contextual_readouts/",
    f,
    fixed = TRUE
  )) return(2L)
  3L
}

rank <- vapply(valid_paths, preference_rank, integer(1))
selected_path <- valid_paths[order(rank, nchar(valid_paths), valid_paths)][[1L]]
selected <- valid[[selected_path]]

source_sha <- tdabm_if_sha256(selected_path)

candidate <- food_build_p1_4a_input(selected)
candidate_file <- file.path(work_root, "food_hfa_p1_4a_candidate.csv")
utils::write.csv(
  candidate,
  candidate_file,
  row.names = FALSE,
  fileEncoding = "UTF-8",
  na = ""
)

topology_variables <- c(
  "physical_friction_log_nearest_large_store_km",
  "transport_constraint_no_car_pct",
  "material_constraint_income_deprivation_2025"
)

label_col <- if ("lsoa21nm" %in% names(candidate)) "lsoa21nm" else NULL

contract <- tdabm_input_contract(
  application_id = "food_deserts_hfa_england_lsoa21",
  id_col = "lsoa21cd",
  label_col = label_col,
  topology_variables = topology_variables,
  colour_variables = character(0),
  scaling = "z_score",
  metric = "euclidean",
  expected_rows = expected_rows,
  numeric_lower = NULL,
  numeric_upper = NULL,
  require_nonconstant_topology = TRUE,
  approved_exclusions = "NONE",
  radius_status = "PENDING_USER_DECISION"
)

frozen_dir <- file.path(result_root, "frozen")
dir.create(frozen_dir, recursive = TRUE, showWarnings = FALSE)
frozen_file <- file.path(frozen_dir, "food_hfa_analysis_input.csv")

framework_zip_sha <- readLines(
  file.path(package_root, "BM_CODE_SOURCE_SHA256.txt"),
  warn = FALSE
)[[1L]]
framework_zip_sha <- strsplit(framework_zip_sha, "[[:space:]]+")[[1L]][[1L]]

freeze <- tdabm_freeze_input_csv(
  candidate_file = candidate_file,
  frozen_file = frozen_file,
  contract = contract,
  metadata = list(
    source_pack_sha256 = source_sha,
    parent_framework_sha256 = framework_zip_sha
  ),
  allow_existing = TRUE
)

if (!isTRUE(freeze$pass)) {
  stop("P1.4-A freeze did not pass.", call. = FALSE)
}

utils::write.csv(
  freeze$manifest,
  file.path(result_root, "analysis_input_manifest.csv"),
  row.names = FALSE
)
utils::write.csv(
  freeze$validation,
  file.path(result_root, "validation_report.csv"),
  row.names = FALSE
)
utils::write.csv(
  freeze$comparison,
  file.path(result_root, "semantic_comparison_report.csv"),
  row.names = FALSE
)
utils::write.csv(
  freeze$column_comparison,
  file.path(result_root, "semantic_column_comparison.csv"),
  row.names = FALSE
)

source_provenance <- data.frame(
  selected_source_file = normalizePath(selected_path, winslash = "/", mustWork = TRUE),
  selected_source_sha256 = source_sha,
  semantic_sha256 = unique(semantic_sha),
  selected_preference = if (preference_rank(selected_path) == 1L) {
    "F6B_V3_0"
  } else if (preference_rank(selected_path) == 2L) {
    "F5_V1_0_2"
  } else {
    "OTHER"
  },
  equivalent_valid_candidate_count = length(valid),
  stringsAsFactors = FALSE
)
utils::write.csv(
  source_provenance,
  file.path(result_root, "source_provenance.csv"),
  row.names = FALSE
)

sample_audit <- data.frame(
  candidate_rows = nrow(candidate),
  unique_lsoa21cd = length(unique(candidate$lsoa21cd)),
  missing_id = sum(is.na(candidate$lsoa21cd) | !nzchar(trimws(candidate$lsoa21cd))),
  missing_physical_friction = sum(!is.finite(candidate$physical_friction_log_nearest_large_store_km)),
  missing_transport_constraint = sum(!is.finite(candidate$transport_constraint_no_car_pct)),
  missing_material_constraint = sum(!is.finite(candidate$material_constraint_income_deprivation_2025)),
  approved_substantive_exclusions = "NONE",
  frozen_rows = nrow(freeze$frozen_data),
  stringsAsFactors = FALSE
)
utils::write.csv(
  sample_audit,
  file.path(result_root, "sample_audit.csv"),
  row.names = FALSE
)

axis_definition_register <- data.frame(
  axis_order = 1:3,
  axis_name = topology_variables,
  source_variable = c(
    "geolytix_large_nearest_km",
    "car_none_pct",
    "income_deprivation_score_2025"
  ),
  exact_formula = c(
    "log1p(geolytix_large_nearest_km)",
    "car_none_pct",
    "income_deprivation_score_2025"
  ),
  sign_convention = c(
    "higher = greater physical friction",
    "higher = greater transport constraint",
    "higher = greater material disadvantage"
  ),
  scaling = "z_score",
  metric = "euclidean",
  stringsAsFactors = FALSE
)
utils::write.csv(
  axis_definition_register,
  file.path(result_root, "axis_definition_register.csv"),
  row.names = FALSE
)

input_sha <- tdabm_if_sha256(frozen_file)
writeLines(
  c(
    paste0("P1_4_A_STATUS=PASS"),
    paste0("INPUT_FROZEN=TRUE"),
    paste0("APPLICATION_ID=food_deserts_hfa_england_lsoa21"),
    paste0("ROWS=", nrow(freeze$frozen_data)),
    paste0("STABLE_ID=lsoa21cd"),
    paste0("SCALING=z_score"),
    paste0("METRIC=euclidean"),
    paste0("RADIUS_STATUS=PENDING_USER_DECISION"),
    paste0("AUTOMATIC_RADIUS_SELECTION=FALSE"),
    paste0("OPTIMALITY_CLAIM=NONE"),
    paste0("FROZEN_INPUT_SHA256=", input_sha),
    paste0("SELECTED_SOURCE_SHA256=", source_sha),
    paste0("FRAMEWORK_SOURCE_ZIP_SHA256=", framework_zip_sha)
  ),
  file.path(result_root, "P1_4_A_STATUS.txt")
)

writeLines(
  tdabm_if_session_lines(),
  file.path(result_root, "session_info.txt")
)

cat("FOOD_HFA_P1_4_A_SOURCE_SELECTED\n")
cat("SOURCE=", selected_path, "\n", sep = "")
cat("SOURCE_SHA256=", source_sha, "\n", sep = "")
cat("FROZEN_INPUT=", frozen_file, "\n", sep = "")
cat("FROZEN_INPUT_SHA256=", input_sha, "\n", sep = "")
cat("FOOD_HFA_P1_4_A_PASS\n")

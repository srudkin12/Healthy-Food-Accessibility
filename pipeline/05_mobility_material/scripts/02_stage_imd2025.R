source("R/functions.R")

ROOT <- root_dir()

# Current corrected official File 7 for English Indices of Deprivation 2025.
url <- paste0(
  "https://assets.publishing.service.gov.uk/media/68ff5daabcb10f6bf9bef911/",
  "File_7_IoD2025_All_Ranks_Scores_Deciles_Population_Denominators.csv"
)
dest <- file.path(
  ROOT, "data_raw", "imd2025",
  "File_7_IoD2025_All_Ranks_Scores_Deciles_Population_Denominators.csv"
)

# v1.0 used an earlier asset URL. v1.0.1 deliberately refreshes against the
# corrected official asset and does not reuse that previous raw file.
download_with_retry(url, dest, quiet = FALSE)

x <- read.csv(dest, check.names = FALSE)

write.csv(
  data.frame(column_number = seq_along(names(x)), column_name = names(x)),
  file.path(ROOT, "results", "f4", "imd2025_column_inventory.csv"),
  row.names = FALSE
)

# File 7 uses "Income Score (rate)", not "Income Deprivation Score".
# Prefer exact authoritative headers and retain a defensive semantic fallback.
required_exact <- c(
  "LSOA code (2021)",
  "Income Score (rate)",
  "Income Rank (where 1 is most deprived)",
  "Income Decile (where 1 is most deprived 10% of LSOAs)"
)

exact_ok <- required_exact %in% names(x)

if (all(exact_ok)) {
  code_idx <- match("LSOA code (2021)", names(x))
  income_score_idx <- match("Income Score (rate)", names(x))
  income_rank_idx <- match("Income Rank (where 1 is most deprived)", names(x))
  income_decile_idx <- match(
    "Income Decile (where 1 is most deprived 10% of LSOAs)",
    names(x)
  )
  match_method <- "exact_authoritative_header"
} else {
  # Fallbacks are deliberately narrow and exclude IDACI/IDAOPI.
  nn <- normalise_name(names(x))

  code_candidates <- which(grepl("^lsoa.*code.*2021$|^lsoa_code_2021$", nn))
  score_candidates <- which(
    grepl("^income_score_rate$|^income_score$", nn) &
      !grepl("idaci|idaopi|children|older", nn)
  )
  rank_candidates <- which(
    grepl("^income_rank", nn) &
      !grepl("idaci|idaopi|children|older", nn)
  )
  decile_candidates <- which(
    grepl("^income_decile", nn) &
      !grepl("idaci|idaopi|children|older", nn)
  )

  if (length(code_candidates) != 1L ||
      length(score_candidates) != 1L ||
      length(rank_candidates) != 1L ||
      length(decile_candidates) != 1L) {

    write.csv(
      data.frame(
        field = names(x),
        normalised = nn,
        stringsAsFactors = FALSE
      ),
      file.path(ROOT, "results", "f4", "imd2025_failed_field_matching.csv"),
      row.names = FALSE
    )

    stop(
      "Could not uniquely identify IoD2025 income fields. ",
      "See imd2025_column_inventory.csv and imd2025_failed_field_matching.csv",
      call. = FALSE
    )
  }

  code_idx <- code_candidates
  income_score_idx <- score_candidates
  income_rank_idx <- rank_candidates
  income_decile_idx <- decile_candidates
  match_method <- "narrow_semantic_fallback"
}

out <- data.frame(
  lsoa21cd = as.character(x[[code_idx]]),
  income_deprivation_score_2025 =
    suppressWarnings(as.numeric(x[[income_score_idx]])),
  income_deprivation_rank_2025 =
    suppressWarnings(as.numeric(x[[income_rank_idx]])),
  income_deprivation_decile_2025 =
    suppressWarnings(as.numeric(x[[income_decile_idx]])),
  stringsAsFactors = FALSE
)

out <- out[grepl("^E", out$lsoa21cd), ]

assert_true(
  nrow(out) == 33755L,
  paste0("IMD2025 England LSOA count ", nrow(out), " != 33755")
)
assert_true(
  length(unique(out$lsoa21cd)) == 33755L,
  "IMD2025 LSOA codes not unique"
)
assert_true(
  all(is.finite(out$income_deprivation_score_2025)),
  "Missing/non-finite IMD2025 income scores"
)

# Basic range checks: the Income Score is a rate/proportion.
assert_true(
  min(out$income_deprivation_score_2025) >= 0,
  "IMD2025 Income Score contains negative values"
)
assert_true(
  max(out$income_deprivation_score_2025) <= 1,
  paste0(
    "IMD2025 Income Score max exceeds 1: ",
    max(out$income_deprivation_score_2025)
  )
)

write.csv(
  out,
  file.path(ROOT, "data_staged", "imd2025_income_lsoa21.csv"),
  row.names = FALSE
)

write.csv(
  data.frame(
    source_url = url,
    bytes = file.info(dest)$size,
    sha256 = sha256_file(dest),
    field_match_method = match_method,
    code_field = names(x)[code_idx],
    income_score_field = names(x)[income_score_idx],
    income_rank_field = names(x)[income_rank_idx],
    income_decile_field = names(x)[income_decile_idx],
    stringsAsFactors = FALSE
  ),
  file.path(ROOT, "results", "f4", "imd2025_source_audit.csv"),
  row.names = FALSE
)

cat(
  "IMD2025_INCOME_OK rows=33755",
  " field=", names(x)[income_score_idx],
  " method=", match_method,
  "\n",
  sep = ""
)

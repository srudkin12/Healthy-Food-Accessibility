source("R/functions.R")

ROOT <- root_dir()

base <- read.csv(file.path(ROOT, "data_staged", "hfa_f4_inherited.csv"),
                 check.names = FALSE)
iuc <- read.csv(file.path(ROOT, "data_staged", "iuc2018_standardised.csv"),
                check.names = FALSE)
lu <- read.csv(file.path(ROOT, "data_raw", "ons", "lsoa11_lsoa21_exact_fit.csv"),
               check.names = FALSE)

lu <- lu[grepl("^E", lu$LSOA11CD) & grepl("^E", lu$LSOA21CD),
         c("LSOA11CD","LSOA21CD"), drop = FALSE]

n21_per_11 <- table(lu$LSOA11CD)
n11_per_21 <- table(lu$LSOA21CD)

lu$one_to_one <- as.integer(n21_per_11[lu$LSOA11CD]) == 1L &
                 as.integer(n11_per_21[lu$LSOA21CD]) == 1L

one <- lu[lu$one_to_one, ]
assert_true(!anyDuplicated(one$LSOA11CD), "One-to-one lookup duplicates LSOA11")
assert_true(!anyDuplicated(one$LSOA21CD), "One-to-one lookup duplicates LSOA21")

eng_iuc <- iuc[grepl("^E", iuc$lsoa11cd), ]
m <- match(one$LSOA11CD, eng_iuc$lsoa11cd)
assert_true(all(!is.na(m)), "Some one-to-one England LSOA11 codes missing from IUC")

cross <- data.frame(
  lsoa21cd = one$LSOA21CD,
  lsoa11cd = one$LSOA11CD,
  iuc_group_code = eng_iuc$iuc_group_code[m],
  iuc_group_label = eng_iuc$iuc_group_label[m],
  iuc_transfer_status = "STRICT_ONE_TO_ONE",
  stringsAsFactors = FALSE
)

out <- data.frame(
  lsoa21cd = base$lsoa21cd,
  lsoa11cd_iuc = NA_character_,
  iuc_group_code = NA_character_,
  iuc_group_label = NA_character_,
  iuc_transfer_status = "CHANGED_OR_COMPLEX_GEOGRAPHY",
  stringsAsFactors = FALSE
)
mm <- match(out$lsoa21cd, cross$lsoa21cd)
ok <- !is.na(mm)
out$lsoa11cd_iuc[ok] <- cross$lsoa11cd[mm[ok]]
out$iuc_group_code[ok] <- cross$iuc_group_code[mm[ok]]
out$iuc_group_label[ok] <- cross$iuc_group_label[mm[ok]]
out$iuc_transfer_status[ok] <- "STRICT_ONE_TO_ONE"

qa <- data.frame(
  metric = c(
    "f4_lsoa21_total",
    "strict_one_to_one_iuc_transfers",
    "changed_or_complex_without_iuc",
    "share_with_iuc",
    "unique_iuc_groups"
  ),
  value = c(
    nrow(out),
    sum(out$iuc_transfer_status == "STRICT_ONE_TO_ONE"),
    sum(out$iuc_transfer_status != "STRICT_ONE_TO_ONE"),
    mean(out$iuc_transfer_status == "STRICT_ONE_TO_ONE"),
    length(unique(na.omit(out$iuc_group_label)))
  ),
  stringsAsFactors = FALSE
)
write.csv(qa, file.path(ROOT, "results", "f5", "iuc_crosswalk_qa.csv"),
          row.names = FALSE)
write.csv(out, file.path(ROOT, "data_staged", "iuc2018_lsoa21_strict.csv"),
          row.names = FALSE)

cat("IUC_CROSSWALK_OK transferred=",
    sum(out$iuc_transfer_status == "STRICT_ONE_TO_ONE"),
    " unresolved=", sum(out$iuc_transfer_status != "STRICT_ONE_TO_ONE"),
    " groups=", length(unique(na.omit(out$iuc_group_label))), "\n", sep = "")

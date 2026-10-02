source("R/functions.R")

ROOT <- root_dir()
ensure_dir(file.path(ROOT, "data_raw", "iuc"))
ensure_dir(file.path(ROOT, "data_raw", "ons"))

# ------------------------------------------------------------------
# IUC classification source
#
# Primary provenance remains GeoDS. Its API may require authentication.
# If unauthenticated API/resource retrieval is unavailable, use the public
# Greater London Authority LOTI ArcGIS mirror of the same IUC 2018 fields.
# ------------------------------------------------------------------
iuc_dest <- file.path(ROOT, "data_raw", "iuc", "iuc_gb_2018.csv")
manual_iuc <- file.path(ROOT, "manual_inputs", "iuc_gb_2018.csv")

iuc_method <- NA_character_
iuc_source_url <- NA_character_

if (file.exists(manual_iuc) && file.info(manual_iuc)$size > 100000) {
  file.copy(manual_iuc, iuc_dest, overwrite = TRUE)
  iuc_method <- "manual_input_geods_download"
  iuc_source_url <- "https://data.geods.ac.uk/dataset/internet-user-classification"

} else {
  # Attempt the original GeoDS route first.
  res <- try_geods_ckan_resource(
    "IUC\\s*2018.*CSV|Data:\\s*IUC\\s*2018\\s*\\(CSV\\)",
    iuc_dest
  )

  if (isTRUE(res$ok)) {
    iuc_method <- "geods_ckan_resource"
    iuc_source_url <- as.character(res$resource$url[1])

  } else {
    # Public mirror fallback. Layer 33 is the "all" IUC 2018 layer
    # and exposes the original classification identifiers and labels.
    gla_layer <- paste0(
      "https://gis.london.gov.uk/arcgis/rest/services/apps/",
      "LOTI_Digital_Exclusion_open_2023/MapServer/33"
    )

    msg("GeoDS unauthenticated retrieval unavailable (", res$reason, ").")
    msg("Falling back to public GLA LOTI IUC 2018 mirror: ", gla_layer)

    mirror <- arcgis_download_attribute_table(
      layer_url = gla_layer,
      fields = c(
        "objectid", "shp_id", "lsoa11_cd", "lsoa11_nm",
        "grp_cd", "grp_label"
      ),
      order_field = "objectid",
      dest_dir = file.path(ROOT, "data_raw", "iuc", "gla_mirror_chunks"),
      where = "1=1",
      requested_page_size = 2000L
    )

    # Preserve the five fields used by the original GeoDS CSV interface.
    iuc_mirror <- data.frame(
      SHP_ID = mirror$shp_id,
      LSOA11_CD = as.character(mirror$lsoa11_cd),
      LSOA11_NM = as.character(mirror$lsoa11_nm),
      GRP_CD = mirror$grp_cd,
      GRP_LABEL = as.character(mirror$grp_label),
      stringsAsFactors = FALSE
    )

    write.csv(iuc_mirror, iuc_dest, row.names = FALSE)

    iuc_method <- "public_GLA_LOTI_ArcGIS_mirror"
    iuc_source_url <- gla_layer
  }
}

iuc <- read_csv_guess(iuc_dest)

write.csv(
  data.frame(
    column_number = seq_along(names(iuc)),
    column_name = names(iuc)
  ),
  file.path(ROOT, "results", "f5", "iuc_column_inventory.csv"),
  row.names = FALSE
)

nn <- normalise_name(names(iuc))
code_idx <- which(nn %in% c("lsoa11_cd", "lsoa11cd"))
group_code_idx <- which(nn %in% c("grp_cd", "group_code", "group"))
group_label_idx <- which(nn %in% c("grp_label", "group_label", "label"))

assert_true(length(code_idx) == 1L,
            "Could not uniquely identify IUC LSOA11 code")
assert_true(length(group_code_idx) == 1L,
            "Could not uniquely identify IUC group code")
assert_true(length(group_label_idx) == 1L,
            "Could not uniquely identify IUC group label")

iuc_std <- data.frame(
  lsoa11cd = as.character(iuc[[code_idx]]),
  iuc_group_code = as.character(iuc[[group_code_idx]]),
  iuc_group_label = as.character(iuc[[group_label_idx]]),
  stringsAsFactors = FALSE
)

# Strong mirror/source validation against the published GB IUC structure.
assert_true(
  nrow(iuc_std) == 41729L,
  paste0("IUC GB row count ", nrow(iuc_std), " != 41729")
)
assert_true(
  length(unique(iuc_std$lsoa11cd)) == 41729L,
  "IUC LSOA/DZ codes not unique"
)

eng_n <- sum(grepl("^E", iuc_std$lsoa11cd))
assert_true(
  eng_n == 32844L,
  paste0("IUC England LSOA11 count ", eng_n, " != 32844")
)

n_groups <- length(unique(iuc_std$iuc_group_code))
assert_true(
  n_groups == 10L,
  paste0("IUC group count ", n_groups, " != 10")
)

assert_true(
  all(!is.na(iuc_std$iuc_group_label) & nzchar(iuc_std$iuc_group_label)),
  "IUC group labels contain missing/empty values"
)

write.csv(
  iuc_std,
  file.path(ROOT, "data_staged", "iuc2018_standardised.csv"),
  row.names = FALSE
)

write.csv(
  as.data.frame(table(
    iuc_std$iuc_group_code,
    iuc_std$iuc_group_label
  ), stringsAsFactors = FALSE),
  file.path(ROOT, "results", "f5", "iuc_group_code_label_audit.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------------
# Optional cluster centres.
# This remains optional. GeoDS API authentication must not block F5.
# ------------------------------------------------------------------
cc_dest <- file.path(ROOT, "data_raw", "iuc", "iuc2018_cluster_centres.csv")
manual_cc <- file.path(ROOT, "manual_inputs", "iuc2018_cluster_centres.csv")

cc_method <- "not_available"
cc_source_url <- NA_character_
cc_ok <- FALSE

if (file.exists(manual_cc) && file.info(manual_cc)$size > 1000) {
  file.copy(manual_cc, cc_dest, overwrite = TRUE)
  cc_method <- "manual_input"
  cc_source_url <- "https://data.geods.ac.uk/dataset/internet-user-classification"
  cc_ok <- TRUE
} else {
  ccres <- try_geods_ckan_resource("IUC\\s*2018.*Cluster\\s*Cent", cc_dest)
  if (isTRUE(ccres$ok)) {
    cc_method <- "geods_ckan_resource"
    cc_source_url <- as.character(ccres$resource$url[1])
    cc_ok <- TRUE
  } else {
    cc_method <- paste0("optional_unavailable:", ccres$reason)
  }
}

# ------------------------------------------------------------------
# ONS exact-fit lookup
# ------------------------------------------------------------------
lookup_url <- paste0(
  "https://open-geography-portalx-ons.hub.arcgis.com/api/download/v1/items/",
  "cbfe64cc03d74af982c1afec639bafd1/csv?layers=0"
)
lookup_dest <- file.path(
  ROOT, "data_raw", "ons", "lsoa11_lsoa21_exact_fit.csv"
)

ok <- download_with_retry(
  lookup_url, lookup_dest, tries = 3L, quiet = FALSE
)
assert_true(ok, "ONS LSOA11-LSOA21 lookup download failed")

lu <- read.csv(lookup_dest, check.names = FALSE)
assert_true(
  all(c("LSOA11CD", "LSOA21CD") %in% names(lu)),
  "ONS exact-fit lookup missing LSOA11CD/LSOA21CD"
)

write.csv(
  data.frame(
    source = c(
      "IUC 2018 classification",
      "GeoDS IUC 2018 cluster centres (optional)",
      "ONS LSOA11-LSOA21 exact-fit lookup V3"
    ),
    method = c(
      iuc_method,
      cc_method,
      "ONS Hub CSV"
    ),
    source_url = c(
      iuc_source_url,
      cc_source_url,
      lookup_url
    ),
    path = c(
      iuc_dest,
      if (cc_ok) cc_dest else NA_character_,
      lookup_dest
    ),
    bytes = c(
      file.info(iuc_dest)$size,
      if (cc_ok) file.info(cc_dest)$size else NA_real_,
      file.info(lookup_dest)$size
    ),
    sha256 = c(
      sha256_file(iuc_dest),
      if (cc_ok) sha256_file(cc_dest) else NA_character_,
      sha256_file(lookup_dest)
    ),
    stringsAsFactors = FALSE
  ),
  file.path(ROOT, "results", "f5", "source_audit.csv"),
  row.names = FALSE
)

writeLines(
  c(
    "IUC SOURCE PROVENANCE NOTE",
    "",
    "Primary canonical source:",
    "GeoDS Internet User Classification 2018.",
    "",
    paste0("Classification acquisition method in this run: ", iuc_method),
    paste0("Classification source URL in this run: ", iuc_source_url),
    "",
    "If the GLA LOTI ArcGIS mirror is used, F5 accepts it only after validating:",
    "- 41,729 unique GB LSOA/Data Zone identifiers;",
    "- 32,844 England LSOA11 identifiers;",
    "- exactly 10 IUC groups;",
    "- non-missing group labels.",
    "",
    "The mirror is used for the categorical classification only.",
    "The optional cluster-centre resource remains GeoDS/manual-input only."
  ),
  file.path(ROOT, "results", "f5", "IUC_SOURCE_PROVENANCE.txt")
)

cat(
  "F5_SOURCES_STAGED_OK",
  " IUC_rows=", nrow(iuc_std),
  " England=", eng_n,
  " groups=", n_groups,
  " method=", iuc_method,
  " cluster_centres=", cc_ok,
  " lookup_rows=", nrow(lu),
  "\n",
  sep = ""
)

source("R/functions.R")

ROOT <- root_dir()
item_id <- "9dbf7613cbb147b8bb8627ddb3568cff"
meta_url <- paste0("https://www.arcgis.com/sharing/rest/content/items/", item_id, "?f=json")
meta_file <- file.path(ROOT, "data_raw", "ruc2021", "item_metadata.json")
meta <- arcgis_get_json(meta_url, meta_file)

assert_true(!is.null(meta$url) && nzchar(meta$url), "RUC ArcGIS item did not expose a service URL")
layer_url <- paste0(sub("/+$", "", meta$url), "/0")

layer_meta_url <- paste0(layer_url, "?f=json")
layer_meta_file <- file.path(ROOT, "data_raw", "ruc2021", "layer_metadata.json")
lm <- arcgis_get_json(layer_meta_url, layer_meta_file)

field_names <- lm$fields$name
field_aliases <- lm$fields$alias
write.csv(data.frame(field_name = field_names, alias = field_aliases),
          file.path(ROOT, "results", "f4", "ruc2021_field_inventory.csv"),
          row.names = FALSE)

code_candidates <- which(toupper(field_names) == "LSOA21CD")
if (length(code_candidates) != 1L) {
  code_candidates <- grep("LSOA.*21.*CD|LSOA21", toupper(field_names))
}
assert_true(length(code_candidates) == 1L, "Could not uniquely identify RUC LSOA21 code field")
code_field <- field_names[code_candidates]

nm_norm <- normalise_name(paste(field_names, field_aliases))
class_candidates <- grep("rural.*urban|ruc", nm_norm)
class_candidates <- setdiff(class_candidates, code_candidates)

# Prefer name/description rather than numeric code.
pref <- class_candidates[
  grepl("nm|name|classification|description", normalise_name(field_names[class_candidates])) |
  grepl("name|classification|description", normalise_name(field_aliases[class_candidates]))
]
if (length(pref) >= 1L) class_candidates <- pref
assert_true(length(class_candidates) >= 1L, "Could not identify RUC classification field")
class_field <- field_names[class_candidates[1]]

out_fields <- unique(c(code_field, class_field))
where <- paste0(code_field, " LIKE 'E%'")

tbl <- arcgis_table_all(
  layer_url = layer_url,
  out_fields = out_fields,
  code_field = code_field,
  where = where,
  order_field = code_field,
  dest_dir = file.path(ROOT, "data_raw", "ruc2021", "chunks"),
  requested_page_size = 2000L
)

out <- data.frame(
  lsoa21cd = as.character(tbl[[code_field]]),
  ruc2021 = as.character(tbl[[class_field]]),
  stringsAsFactors = FALSE
)

write.csv(
  data.frame(
    metric = c("rows_received", "unique_lsoa21cd", "expected_english_lsoa21"),
    value = c(nrow(tbl), length(unique(tbl[[code_field]])), 33755L),
    stringsAsFactors = FALSE
  ),
  file.path(ROOT, "results", "f4", "ruc2021_pagination_qa.csv"),
  row.names = FALSE
)

assert_true(nrow(out) == 33755L,
            paste0("RUC2021 England LSOA count ", nrow(out), " != 33755"))
assert_true(length(unique(out$lsoa21cd)) == 33755L, "RUC LSOA codes not unique")
assert_true(all(!is.na(out$ruc2021) & nzchar(out$ruc2021)), "Missing RUC classification")

write.csv(out, file.path(ROOT, "data_staged", "ruc2021_lsoa21.csv"), row.names = FALSE)
write.csv(as.data.frame(table(out$ruc2021), stringsAsFactors = FALSE),
          file.path(ROOT, "results", "f4", "ruc2021_distribution.csv"), row.names = FALSE)

cat("RUC2021_OK rows=33755 classes=", length(unique(out$ruc2021)), "\n", sep = "")

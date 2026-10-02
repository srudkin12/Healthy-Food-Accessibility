source(file.path(Sys.getenv("HFA_ROOT"), "R", "functions.R"))
ensure_dirs()

base <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BGC_V5/FeatureServer/0/query"
where <- "LSOA21CD LIKE 'E%'"

say("Querying ONS FeatureServer for England LSOA21 count")
count_req <- httr2::request(base) |>
  httr2::req_url_query(where = where, returnCountOnly = "true", f = "json") |>
  httr2::req_retry(max_tries = 4)
count_resp <- httr2::req_perform(count_req)
count_json <- httr2::resp_body_json(count_resp, simplifyVector = TRUE)
expected_remote <- as.integer(count_json$count)
assert_true(!is.na(expected_remote), "ONS FeatureServer did not return a record count")
assert_true(expected_remote == 33755L,
            paste0("Expected 33,755 English LSOA21 records; ONS service returned ", expected_remote,
                   ". Stop and inspect source version/geography before proceeding."))

page_size <- 2000L
all_pages <- list()
for (offset in seq.int(0L, expected_remote - 1L, by = page_size)) {
  say("ONS LSOA21 attributes offset ", offset, " / ", expected_remote)
  req <- httr2::request(base) |>
    httr2::req_url_query(
      where = where,
      outFields = "LSOA21CD,LSOA21NM,LSOA21NMW,BNG_E,BNG_N,LAT,LONG",
      returnGeometry = "false",
      orderByFields = "LSOA21CD",
      resultOffset = offset,
      resultRecordCount = page_size,
      f = "json"
    ) |>
    httr2::req_retry(max_tries = 4)
  resp <- httr2::req_perform(req)
  txt <- httr2::resp_body_string(resp)
  raw_path <- p("data_raw", "ons", sprintf("lsoa21_attributes_%05d.json", offset))
  writeLines(txt, raw_path, useBytes = TRUE)
  obj <- jsonlite::fromJSON(txt, simplifyDataFrame = TRUE)
  if (!is.null(obj$error)) stop("ArcGIS error at offset ", offset, ": ", jsonlite::toJSON(obj$error, auto_unbox = TRUE))
  attrs <- obj$features$attributes
  if (is.null(attrs) || nrow(attrs) == 0) stop("No features returned at offset ", offset)
  all_pages[[length(all_pages) + 1L]] <- tibble::as_tibble(attrs)
}

spine <- dplyr::bind_rows(all_pages) |>
  dplyr::distinct(LSOA21CD, .keep_all = TRUE) |>
  dplyr::arrange(LSOA21CD) |>
  dplyr::rename(
    lsoa21cd = LSOA21CD,
    lsoa21nm = LSOA21NM,
    lsoa21nmw = LSOA21NMW,
    bng_e = BNG_E,
    bng_n = BNG_N,
    lat = LAT,
    long = LONG
  )

assert_true(nrow(spine) == 33755L, paste0("LSOA21 spine row count is ", nrow(spine), ", expected 33,755"))
assert_true(dplyr::n_distinct(spine$lsoa21cd) == 33755L, "LSOA21 codes are not unique")
assert_true(all(grepl("^E[0-9]{8}$", spine$lsoa21cd)), "Non-English or malformed LSOA21 codes detected")
assert_true(all(!is.na(spine$lat) & !is.na(spine$long)), "Missing representative coordinates detected")

write_csv_atomic(spine, p("data_staged", "lsoa21_spine.csv"))
say("LSOA21_SPINE_OK rows=", nrow(spine))

source(file.path(Sys.getenv("HFA_ROOT"), "R", "functions.R"))
ensure_dirs()

sources <- tibble::tribble(
  ~source_id, ~url, ~dest, ~expected_min_bytes,
  "DFT_JTS0507", "https://assets.publishing.service.gov.uk/media/6182b95bd3bf7f56003e98e0/jts0507.ods", p("data_raw", "dft", "jts0507_food_stores_2019.ods"), 1000000,
  "DFT_TCM_2025", "https://assets.publishing.service.gov.uk/media/68c966fc07d9e92bc5517b80/connectivity_metrics_2025.ods", p("data_raw", "dft", "connectivity_metrics_2025.ods"), 1000000
)

for (i in seq_len(nrow(sources))) {
  safe_download(sources$url[[i]], sources$dest[[i]])
  bytes <- file.info(sources$dest[[i]])$size
  assert_true(bytes >= sources$expected_min_bytes[[i]], paste0(sources$source_id[[i]], " download unexpectedly small: ", bytes, " bytes"))
}

# ODS is a ZIP container. Inventory archive members without interpreting worksheets yet.
inv <- purrr::map_dfr(seq_len(nrow(sources)), function(i) {
  z <- utils::unzip(sources$dest[[i]], list = TRUE)
  tibble::tibble(
    source_id = sources$source_id[[i]],
    archive_member = z$Name,
    member_bytes = z$Length,
    date = as.character(z$Date)
  )
})
write_csv_atomic(inv, p("results", "f0f1", "dft_ods_archive_inventory.csv"))

checks <- sources |>
  dplyr::mutate(
    bytes = as.numeric(file.info(dest)$size),
    sha256 = vapply(dest, sha256_file, character(1))
  ) |>
  dplyr::select(source_id, url, file = dest, bytes, sha256)
write_csv_atomic(checks, p("results", "f0f1", "dft_source_checksums.csv"))

say("DFT_SOURCES_STAGED_OK files=", nrow(sources))

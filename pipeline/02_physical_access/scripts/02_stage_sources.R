root <- normalizePath(file.path(dirname(sub("^--file=", "", commandArgs(trailingOnly=FALSE)[grep("^--file=", commandArgs(trailingOnly=FALSE))][1])), ".."), mustWork=TRUE)
source(file.path(root,"R","functions.R"))
dir.create(file.path(root,"data_raw","dft"),recursive=TRUE,showWarnings=FALSE)
dir.create(file.path(root,"data_raw","ons"),recursive=TRUE,showWarnings=FALSE)

jts_url <- "https://assets.publishing.service.gov.uk/media/6182b95bd3bf7f56003e98e0/jts0507.ods"
tcm_url <- "https://assets.publishing.service.gov.uk/media/68c966fc07d9e92bc5517b80/connectivity_metrics_2025.ods"

# v1.0.3 can reuse byte-identical DfT downloads from the immediately preceding failed F2
# runs, avoiding another ~93 MB transfer.  Reuse is allowed only at the exact observed
# authoritative byte sizes; checksums are still written below.  Otherwise download afresh.
reuse_or_download <- function(name,url,dest,expected_bytes) {
  parent <- dirname(root)
  siblings <- c("FoodDeserts_HFA_F2_Physical_Access_v1_0_2","FoodDeserts_HFA_F2_Physical_Access_v1_0_1","FoodDeserts_HFA_F2_Physical_Access_v1_0")
  rel <- file.path("data_raw","dft",name)
  for (d in siblings) {
    cand <- file.path(parent,d,rel)
    if (file.exists(cand) && isTRUE(file.info(cand)$size==expected_bytes)) {
      dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE)
      ok <- file.copy(cand,dest,overwrite=TRUE)
      if (isTRUE(ok) && file.exists(dest) && file.info(dest)$size==expected_bytes) {
        msg("Reused previously downloaded authoritative DfT file: ",cand)
        return(dest)
      }
    }
  }
  download_retry(url,dest)
}

jts <- reuse_or_download("jts0507_food_stores_2019.ods",jts_url,file.path(root,"data_raw","dft","jts0507_food_stores_2019.ods"),29144861)
tcm <- reuse_or_download("connectivity_metrics_2025.ods",tcm_url,file.path(root,"data_raw","dft","connectivity_metrics_2025.ods"),68164574)

lookup <- file.path(root,"data_raw","ons","lsoa11_lsoa21_exact_fit.csv")
download_ons_lsoa11_lsoa21_exact_fit(lookup)
files <- c(jts,tcm,lookup)
assert(all(file.exists(files)), "One or more F2 sources were not staged")
assert(all(file.info(files)$size > 1000), "One or more F2 staged sources are unexpectedly small")
inv <- data.frame(file=basename(files),bytes=file.info(files)$size,sha256=vapply(files,sha256_file,character(1)))
write_csv(inv,file.path(root,"results","f2","source_checksums.csv"))
cat("F2_SOURCES_STAGED_OK files=3\n")

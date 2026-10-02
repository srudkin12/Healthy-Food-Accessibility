root <- normalizePath(file.path(dirname(sub("^--file=", "", commandArgs(trailingOnly=FALSE)[grep("^--file=", commandArgs(trailingOnly=FALSE))][1])), ".."), mustWork=TRUE)
source(file.path(root,"R","functions.R"))

path <- file.path(root,"data_raw","dft","jts0507_food_stores_2019.ods")
sheets <- readODS::list_ods_sheets(path)
write_csv(data.frame(sheet=sheets),file.path(root,"results","f2","jts2019_sheets.csv"))
assert("2019" %in% sheets, "JTS0507 workbook does not contain a 2019 sheet. Sheets: %s", paste(sheets,collapse=", "))
msg("Reading JTS0507 sheet: 2019 only")
raw <- readODS::read_ods(path,sheet="2019",col_names=FALSE)
assert(nrow(raw)>32000 && ncol(raw)==41L, "Unexpected 2019 JTS0507 dimensions: %d rows x %d cols; expected 41 columns", nrow(raw), ncol(raw))

# readODS can return a tibble.  Always use [[j]] to obtain an atomic vector;
# [rows,j,drop=TRUE] is not reliably vectorising for tibble-like objects.
col_vector <- function(j, rows=NULL) {
  z <- raw[[j]]
  if (!is.null(rows)) z <- z[rows]
  z
}
to_numeric <- function(z) {
  if (is.numeric(z)) return(as.numeric(z))
  z <- trimws(as.character(z))
  z[z %in% c("", ":", "..", "-", "—", "NA", "N/A")] <- NA_character_
  z <- gsub(",", "", z, fixed=TRUE)
  suppressWarnings(as.numeric(z))
}

cc <- find_code_column(raw,"^E010[0-9]{5}$")
assert(!is.null(cc), "Could not identify the LSOA11 code column in JTS0507/2019")
code_rows <- which(grepl("^E010[0-9]{5}$",as.character(col_vector(cc$col))))
assert(length(code_rows)==32844, "Expected 32,844 England LSOA11 data rows in JTS0507/2019; found %d", length(code_rows))
data_start <- min(code_rows)

# Reconstruct literal header evidence for audit.  The uploaded diagnostics from v1.0.2
# show the compact variable names on header row 7: LSOA_code, FoodPTt, FoodCyct,
# FoodCart and FoodWalkt.  Select those exact DfT variables directly rather than infer
# travel-time fields from prose or mode-block position.
header_rows <- seq_len(data_start-1L)
if (length(header_rows)>60L) header_rows <- tail(header_rows,60L)
header_mat <- matrix("",nrow=length(header_rows),ncol=ncol(raw))
for (ii in seq_along(header_rows)) {
  vals <- vapply(seq_len(ncol(raw)), function(j) {
    z <- col_vector(j, header_rows[ii])
    if (!length(z) || is.na(z[1])) "" else trimws(as.character(z[1]))
  }, character(1))
  header_mat[ii,] <- vals
}
collapse_header <- function(mat,j) {
  z <- trimws(mat[,j]); z <- z[nzchar(z)]
  if (!length(z)) return("")
  paste(unique(z),collapse=" | ")
}
hdr_raw <- vapply(seq_len(ncol(raw)),function(j)collapse_header(header_mat,j),character(1))
normalise_token <- function(x) {
  x <- iconv(as.character(x),to="ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  tolower(gsub("[^a-z0-9]+","",x))
}

# Search individual header cells for an exact compact DfT token.
find_header_token <- function(token) {
  key <- normalise_token(token)
  hits <- integer(0)
  for (j in seq_len(ncol(raw))) {
    vals <- header_mat[,j]
    if (any(normalise_token(vals)==key)) hits <- c(hits,j)
  }
  unique(hits)
}

expected <- c(
  public_transport="FoodPTt",
  cycling="FoodCyct",
  car="FoodCart",
  walking="FoodWalkt"
)
selected <- setNames(rep(NA_integer_,length(expected)),names(expected))
method <- setNames(rep("",length(expected)),names(expected))
for (m in names(expected)) {
  hits <- find_header_token(expected[[m]])
  assert(length(hits)==1L, "Expected exactly one JTS2019 header token %s for %s; found columns: %s", expected[[m]], m, paste(hits,collapse=","))
  selected[[m]] <- hits[[1]]
  method[[m]] <- "exact_dft_header_token"
}
assert(length(unique(selected))==4L, "JTS2019 exact-token selection did not identify four unique travel-time columns")

# Preserve full header evidence and selected-column evidence.
inv <- data.frame(
  col=seq_len(ncol(raw)),
  raw_header=hdr_raw,
  code_column=seq_len(ncol(raw))==cc$col,
  stringsAsFactors=FALSE
)
write_csv(inv,file.path(root,"results","f2","jts2019_column_inventory.csv"))
preview <- as.data.frame(header_mat,stringsAsFactors=FALSE,check.names=FALSE)
preview <- cbind(header_row=header_rows,preview)
write_csv(preview,file.path(root,"results","f2","jts2019_header_preview.csv"))

numeric_coverage <- vapply(selected,function(j)sum(is.finite(to_numeric(col_vector(j,code_rows)))),numeric(1))
sel_audit <- data.frame(
  mode=names(selected),
  dft_variable=unname(expected[names(selected)]),
  col=as.integer(selected),
  method=unname(method),
  raw_header=hdr_raw[selected],
  numeric_coverage=as.integer(numeric_coverage),
  stringsAsFactors=FALSE
)
write_csv(sel_audit,file.path(root,"results","f2","jts2019_selected_columns.csv"))

# Write a tiny raw-value preview before enforcing numeric coverage.  If DfT/readODS
# representation changes in future, this provides enough evidence for a parser patch.
preview_values <- data.frame(
  lsoa11cd=as.character(col_vector(cc$col,code_rows[seq_len(min(12L,length(code_rows)))])),
  stringsAsFactors=FALSE
)
for (m in names(selected)) {
  preview_values[[paste0(m,"_raw")]] <- as.character(col_vector(selected[[m]],code_rows[seq_len(min(12L,length(code_rows)))]))
  preview_values[[paste0(m,"_numeric")]] <- to_numeric(col_vector(selected[[m]],code_rows[seq_len(min(12L,length(code_rows)))]))
}
write_csv(preview_values,file.path(root,"results","f2","jts2019_value_preview.csv"))

assert(all(numeric_coverage>32000), "Exact JTS travel-time columns were found, but numeric coverage is unexpectedly low: %s. See jts2019_value_preview.csv", paste(names(numeric_coverage),numeric_coverage,sep="=",collapse=", "))

# Extract and enforce the published JTS0507 2019 population contract.
dat <- data.frame(lsoa11cd=as.character(col_vector(cc$col,code_rows)),stringsAsFactors=FALSE)
for (m in names(selected)) {
  dat[[paste0("jts2019_",m,"_min")]] <- to_numeric(col_vector(selected[[m]],code_rows))
}
dat$jts_sheet <- "2019"
assert(nrow(dat)==32844,"JTS0507 2019 extracted rows %d != 32,844",nrow(dat))
assert(length(unique(dat$lsoa11cd))==32844,"JTS0507 2019 LSOA11 codes are not unique")
num <- names(dat)[grepl("_min$",names(dat))]
coverage <- vapply(dat[num],function(z)sum(is.finite(z)),numeric(1))
assert(all(coverage>32000),"One or more JTS travel-time fields have unexpectedly low coverage: %s",paste(names(coverage),coverage,sep="=",collapse=", "))
write_csv(dat,file.path(root,"data_staged","jts2019_food_access_lsoa11.csv"))

# Direct audit of the student report's identical PT/car summary statistics.
pt <- "jts2019_public_transport_min"; car <- "jts2019_car_min"
d <- dat[[pt]]-dat[[car]]
audit <- rbind(
  data.frame(metric="n_complete_pt_car",value=sum(is.finite(d))),
  data.frame(metric="n_exact_equal_pt_car",value=sum(is.finite(d)&abs(d)<1e-12)),
  data.frame(metric="share_exact_equal_pt_car",value=mean(abs(d[is.finite(d)])<1e-12)),
  data.frame(metric="cor_pt_car",value=stats::cor(dat[[pt]],dat[[car]],use="complete.obs")),
  data.frame(metric="mean_pt",value=mean(dat[[pt]],na.rm=TRUE)),
  data.frame(metric="mean_car",value=mean(dat[[car]],na.rm=TRUE)),
  data.frame(metric="sd_pt",value=stats::sd(dat[[pt]],na.rm=TRUE)),
  data.frame(metric="sd_car",value=stats::sd(dat[[car]],na.rm=TRUE)),
  data.frame(metric="min_pt",value=min(dat[[pt]],na.rm=TRUE)),
  data.frame(metric="min_car",value=min(dat[[car]],na.rm=TRUE)),
  data.frame(metric="max_pt",value=max(dat[[pt]],na.rm=TRUE)),
  data.frame(metric="max_car",value=max(dat[[car]],na.rm=TRUE))
)
write_csv(audit,file.path(root,"results","f2","student_pt_car_audit.csv"))
cat(sprintf("JTS2019_EXTRACTION_OK rows=%d modes=4 selected=%s\n",nrow(dat),paste(paste(names(selected),selected,method,sep=":"),collapse=",")))

root <- normalizePath(file.path(dirname(sub("^--file=", "", commandArgs(trailingOnly=FALSE)[grep("^--file=", commandArgs(trailingOnly=FALSE))][1])), ".."), mustWork=TRUE)
source(file.path(root,"R","functions.R"))
path <- file.path(root,"data_raw","dft","connectivity_metrics_2025.ods")
sheets <- readODS::list_ods_sheets(path)
write_csv(data.frame(sheet=sheets),file.path(root,"results","f2","tcm2025_sheets.csv"))

# Prefer sheets whose names advertise LSOA, then inspect remaining sheets only if needed.
ord <- unique(c(grep("lsoa",sheets,ignore.case=TRUE),seq_along(sheets)))
inv <- list(); candidates <- list()
for (idx in ord) {
  s <- sheets[idx]
  msg("Scanning TCM sheet: ", s)
  raw <- try(readODS::read_ods(path,sheet=s,col_names=FALSE),silent=TRUE)
  if (inherits(raw,"try-error") || !nrow(raw) || !ncol(raw)) next
  cc <- find_code_column(raw,"^E010[0-9]{5}$")
  if (is.null(cc)) next
  rows <- which(grepl("^E010[0-9]{5}$",as.character(raw[[cc$col]])))
  if (length(rows)<30000) next
  ds <- min(rows)
  hr <- seq_len(ds-1L); if(length(hr)>60L) hr <- tail(hr,60L)
  hm <- matrix("",nrow=length(hr),ncol=ncol(raw))
  for (ii in seq_along(hr)) {
    vals <- as.character(unlist(raw[hr[ii],,drop=TRUE],use.names=FALSE)); length(vals)<-ncol(raw)
    vals[is.na(vals)]<-""; hm[ii,]<-trimws(vals)
  }
  hf <- hm
  for(i in seq_len(nrow(hf))){last<-"";for(j in seq_len(ncol(hf))){v<-trimws(hf[i,j]);if(nzchar(v))last<-v else if(nzchar(last))hf[i,j]<-last}}
  ch <- function(mat,j){z<-trimws(mat[,j]);z<-z[nzchar(z)];if(!length(z))"" else paste(unique(z),collapse=" | ")}
  hraw <- vapply(seq_len(ncol(raw)),function(j)ch(hm,j),character(1))
  hfill <- vapply(seq_len(ncol(raw)),function(j)ch(hf,j),character(1))
  inv[[length(inv)+1]] <- data.frame(sheet=s,col=seq_len(ncol(raw)),raw_header=hraw,filled_header=hfill,stringsAsFactors=FALSE)

  # F2 uses the TCM only as a contemporary benchmark.  Retain purpose-level shopping
  # connectivity (and any explicitly supermarket/convenience-labelled score) rather than
  # treating the broader TCM as a replacement for supermarket-specific access.
  shop <- which(grepl("shopping|supermarket|convenience|grocery|food shopping",hfill,ignore.case=TRUE))
  shop <- setdiff(shop,cc$col)
  if (!length(shop)) next
  dat <- data.frame(lsoa21cd=as.character(raw[[cc$col]][rows]),stringsAsFactors=FALSE)
  used <- character(0)
  for (j in shop) {
    z <- suppressWarnings(as.numeric(as.character(raw[[j]][rows])))
    if (sum(is.finite(z))<=10000) next
    label <- clean_names(hfill[j]); if(!nzchar(label)) label<-paste0("col",j)
    nm <- paste0("tcm2025_",label)
    if (nm %in% used) nm <- paste0(nm,"_c",j)
    used <- c(used,nm); dat[[nm]] <- z
  }
  if(ncol(dat)>1) {
    dat$tcm_sheet <- s
    candidates[[length(candidates)+1]] <- dat
    # A single comprehensive LSOA sheet is sufficient; avoid re-reading large workbooks
    # once an England LSOA21 shopping benchmark with strong coverage has been found.
    if(length(rows)>=33755 || nrow(dat)>=33000) break
  }
}
if(length(inv)) write_csv(do.call(rbind,inv),file.path(root,"results","f2","tcm2025_column_inventory.csv"))
assert(length(candidates)>0,"No LSOA21 shopping-related TCM columns identified after merged-header reconstruction. See tcm2025_column_inventory.csv.")
out <- Reduce(function(a,b) merge(a,b,by="lsoa21cd",all=TRUE,suffixes=c("","_dup")),candidates)
out <- out[grepl("^E010",out$lsoa21cd),,drop=FALSE]
out <- out[!duplicated(out$lsoa21cd),]
assert(nrow(out)>=33000,"TCM shopping benchmark has unexpectedly few English LSOA21s: %d",nrow(out))
write_csv(out,file.path(root,"data_staged","tcm2025_shopping_benchmark_lsoa21.csv"))
cat(sprintf("TCM2025_EXTRACTION_OK rows=%d benchmark_columns=%d\n",nrow(out),ncol(out)-1))

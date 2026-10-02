args <- commandArgs(trailingOnly=FALSE)
ff <- sub("^--file=", "", args[grep("^--file=", args)])
root <- normalizePath(file.path(dirname(ff[1]), ".."), mustWork=TRUE)
parent <- dirname(root)
dir.create(file.path(root,"results","f2a"), recursive=TRUE, showWarnings=FALSE)

find_first <- function(paths) {
  for (p in paths) if (file.exists(p) && file.info(p)$size > 1000) return(normalizePath(p))
  NA_character_
}
raw_candidates <- c(
 file.path(parent,"02_physical_access","data_raw","dft","jts0507_food_stores_2019.ods"),
 file.path(parent,"FoodDeserts_HFA_F2_Physical_Access_v1_0_2","data_raw","dft","jts0507_food_stores_2019.ods"),
 file.path(parent,"FoodDeserts_HFA_F2_Physical_Access_v1_0_1","data_raw","dft","jts0507_food_stores_2019.ods"),
 file.path(parent,"FoodDeserts_HFA_F2_Physical_Access_v1_0","data_raw","dft","jts0507_food_stores_2019.ods")
)
path <- find_first(raw_candidates)
if(is.na(path)) stop("Could not find the frozen JTS0507 ODS in a sibling F2 directory.", call.=FALSE)
cat("Using frozen JTS0507: ", path, "\n", sep="")

clean <- function(z) {
 z <- trimws(as.character(z)); z[z %in% c("",":","..","-","—","NA","N/A")] <- NA_character_
 z <- gsub(",", "", z, fixed=TRUE); suppressWarnings(as.numeric(z))
}
read_sheet <- function(sheet) {
 raw <- readODS::read_ods(path, sheet=sheet, col_names=FALSE)
 code_score <- vapply(seq_len(ncol(raw)), function(j) sum(grepl("^E010[0-9]{5}$", as.character(raw[[j]]))), numeric(1))
 code_col <- which.max(code_score); rows <- which(grepl("^E010[0-9]{5}$", as.character(raw[[code_col]])))
 if(length(rows) < 32000) stop("Could not locate LSOA rows in sheet ",sheet,call.=FALSE)
 hdr_rows <- seq_len(min(rows)-1L)
 find_token <- function(token) {
   key <- tolower(gsub("[^a-z0-9]+","",token))
   hits <- integer(0)
   for(j in seq_len(ncol(raw))) {
     z <- tolower(gsub("[^a-z0-9]+","",trimws(as.character(raw[[j]][hdr_rows]))))
     if(any(z==key,na.rm=TRUE)) hits <- c(hits,j)
   }
   unique(hits)
 }
 get_token <- function(token) {
   h <- find_token(token)
   if(length(h)!=1L) stop("Expected exactly one token ",token," in sheet ",sheet,"; found ",paste(h,collapse=","),call.=FALSE)
   clean(raw[[h]][rows])
 }
 list(raw=raw,rows=rows,code_col=code_col,find_token=find_token,get_token=get_token,
      lsoa=as.character(raw[[code_col]][rows]))
}

s19 <- read_sheet("2019")
# exact DfT compact tokens from JTS0507
vars <- c(
 Food_pop="Food_pop",
 FoodPTt="FoodPTt",FoodPT15n="FoodPT15n",FoodPT30n="FoodPT30n",FoodPT45n="FoodPT45n",FoodPT60n="FoodPT60n",FoodPT15pct="FoodPT15pct",FoodPT30pct="FoodPT30pct",FoodPT45pct="FoodPT45pct",FoodPT60pct="FoodPT60pct",
 FoodCyct="FoodCyct",FoodCyc15n="FoodCyc15n",FoodCyc30n="FoodCyc30n",FoodCyc45n="FoodCyc45n",FoodCyc60n="FoodCyc60n",FoodCyc15pct="FoodCyc15pct",FoodCyc30pct="FoodCyc30pct",FoodCyc45pct="FoodCyc45pct",FoodCyc60pct="FoodCyc60pct",
 FoodCart="FoodCart",FoodCar15n="FoodCar15n",FoodCar30n="FoodCar30n",FoodCar45n="FoodCar45n",FoodCar60n="FoodCar60n",FoodCar15pct="FoodCar15pct",FoodCar30pct="FoodCar30pct",FoodCar45pct="FoodCar45pct",FoodCar60pct="FoodCar60pct",
 FoodWalkt="FoodWalkt",FoodWalk15n="FoodWalk15n",FoodWalk30n="FoodWalk30n",FoodWalk45n="FoodWalk45n",FoodWalk60n="FoodWalk60n",FoodWalk15pct="FoodWalk15pct",FoodWalk30pct="FoodWalk30pct",FoodWalk45pct="FoodWalk45pct",FoodWalk60pct="FoodWalk60pct")
d <- data.frame(lsoa11cd=s19$lsoa, stringsAsFactors=FALSE)
for(nm in names(vars)) d[[nm]] <- s19$get_token(vars[[nm]])

# Pairwise PT-v-car audit for every corresponding metric.
pairs <- data.frame(
 metric=c("t","15n","30n","45n","60n","15pct","30pct","45pct","60pct"),
 pt=c("FoodPTt","FoodPT15n","FoodPT30n","FoodPT45n","FoodPT60n","FoodPT15pct","FoodPT30pct","FoodPT45pct","FoodPT60pct"),
 car=c("FoodCart","FoodCar15n","FoodCar30n","FoodCar45n","FoodCar60n","FoodCar15pct","FoodCar30pct","FoodCar45pct","FoodCar60pct"), stringsAsFactors=FALSE)
pair_audit <- do.call(rbind,lapply(seq_len(nrow(pairs)),function(i){
 a<-d[[pairs$pt[i]]]; b<-d[[pairs$car[i]]]; ok<-is.finite(a)&is.finite(b)
 data.frame(metric=pairs$metric[i],pt_variable=pairs$pt[i],car_variable=pairs$car[i],n_complete=sum(ok),n_exact_equal=sum(ok & abs(a-b)<1e-12),share_exact_equal=mean(abs(a[ok]-b[ok])<1e-12),correlation=if(sum(ok)>2) cor(a[ok],b[ok]) else NA_real_,mean_pt=mean(a,na.rm=TRUE),mean_car=mean(b,na.rm=TRUE))
}))
write.csv(pair_audit,file.path(root,"results","f2a","jts2019_pt_car_full_block_audit.csv"),row.names=FALSE)

# Reconstruct England values from LSOA file using published service-user weights.
w <- d$Food_pop
wmean <- function(x) weighted.mean(x,w,na.rm=TRUE)
recon <- data.frame(
 metric=rep(c("average_min_travel_time_min","pct_within_15","pct_within_30","pct_within_45","pct_within_60"),each=4),
 mode=rep(c("car","cycling","public_transport","walking"),5),
 reconstructed=NA_real_, stringsAsFactors=FALSE)
lookup <- list(
 average_min_travel_time_min=c(car="FoodCart",cycling="FoodCyct",public_transport="FoodPTt",walking="FoodWalkt"),
 pct_within_15=c(car="FoodCar15pct",cycling="FoodCyc15pct",public_transport="FoodPT15pct",walking="FoodWalk15pct"),
 pct_within_30=c(car="FoodCar30pct",cycling="FoodCyc30pct",public_transport="FoodPT30pct",walking="FoodWalk30pct"),
 pct_within_45=c(car="FoodCar45pct",cycling="FoodCyc45pct",public_transport="FoodPT45pct",walking="FoodWalk45pct"),
 pct_within_60=c(car="FoodCar60pct",cycling="FoodCyc60pct",public_transport="FoodPT60pct",walking="FoodWalk60pct"))
for(i in seq_len(nrow(recon))) recon$reconstructed[i] <- wmean(d[[lookup[[recon$metric[i]]][[recon$mode[i]]]]])
bench <- read.csv(file.path(root,"config","jts2019_food_store_official_benchmarks.csv"),stringsAsFactors=FALSE)
recon <- merge(recon,bench,by=c("metric","mode"),all.x=TRUE,sort=FALSE)
recon$rounded_reconstructed <- round(recon$reconstructed)
recon$matches_published_rounded <- recon$rounded_reconstructed==recon$published_value
write.csv(recon,file.path(root,"results","f2a","jts2019_national_reconstruction_vs_official.csv"),row.names=FALSE)

# Check prior-year sheet: if 2017 PT and car are distinct, the 2019 duplication is year-specific.
s17 <- read_sheet("2017")
pt17 <- s17$get_token("FoodPTt")
car17 <- s17$get_token("FoodCart")
w17 <- s17$get_token("Food_pop")
ok17 <- is.finite(pt17)&is.finite(car17)
a17 <- data.frame(
 sheet="2017",
 n_complete=sum(ok17),
 n_exact_equal=sum(ok17 & abs(pt17-car17)<1e-12),
 share_exact_equal=mean(abs(pt17[ok17]-car17[ok17])<1e-12),
 correlation=cor(pt17[ok17],car17[ok17]),
 weighted_mean_pt=weighted.mean(pt17,w17,na.rm=TRUE),
 weighted_mean_car=weighted.mean(car17,w17,na.rm=TRUE),
 stringsAsFactors=FALSE)
write.csv(a17,file.path(root,"results","f2a","jts2017_pt_car_comparator.csv"),row.names=FALSE)

# Compact evidence ledger and machine decision.
pt_t <- pair_audit[pair_audit$metric=="t",]
other_pairs <- pair_audit[pair_audit$metric!="t",]
pt_off <- recon[recon$metric=="average_min_travel_time_min" & recon$mode=="public_transport",]
car_off <- recon[recon$metric=="average_min_travel_time_min" & recon$mode=="car",]
raw_min_duplicate <- isTRUE(pt_t$share_exact_equal==1)
official_distinguishes_modes <- is.finite(pt_off$published_value) && is.finite(car_off$published_value) && pt_off$published_value != car_off$published_value
threshold_blocks_distinct <- any(other_pairs$share_exact_equal < 0.999999, na.rm=TRUE)
prior_year_distinct <- a17$share_exact_equal < 0.999999
status <- if(raw_min_duplicate && official_distinguishes_modes) {
  "JTS2019_PT_MIN_DO_NOT_USE_SOURCE_CONFLICT"
} else {
  "MANUAL_REVIEW_REQUIRED"
}
ledger <- data.frame(
 check=c("raw_2019_FoodPTt_equals_FoodCart_all_LSOAs","official_2019_food_store_means_distinguish_PT_and_car","2019_PT_and_car_threshold_blocks_are_distinct","2017_FoodPTt_and_FoodCart_are_distinct","decision"),
 value=c(as.character(raw_min_duplicate),as.character(official_distinguishes_modes),as.character(threshold_blocks_distinct),as.character(prior_year_distinct),status),
 stringsAsFactors=FALSE)
write.csv(ledger,file.path(root,"results","f2a","JTS2019_PT_VALIDATION_LEDGER.csv"),row.names=FALSE)
cat("JTS2019_VALIDATION_AUDIT_COMPLETE status=",status,"\n",sep="")

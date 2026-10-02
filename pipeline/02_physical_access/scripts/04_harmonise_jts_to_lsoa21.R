root <- normalizePath(file.path(dirname(sub("^--file=", "", commandArgs(trailingOnly=FALSE)[grep("^--file=", commandArgs(trailingOnly=FALSE))][1])), ".."), mustWork=TRUE)
source(file.path(root,"R","functions.R"))
jts <- read_csv_flex(file.path(root,"data_staged","jts2019_food_access_lsoa11.csv"))
lk <- read_csv_flex(file.path(root,"data_raw","ons","lsoa11_lsoa21_exact_fit.csv"))
names(lk) <- clean_names(names(lk))
# tolerate ObjectId and ordering changes
c11 <- names(lk)[grepl("lsoa11cd",names(lk),ignore.case=TRUE)][1]
c21 <- names(lk)[grepl("lsoa21cd",names(lk),ignore.case=TRUE)][1]
chg <- names(lk)[grepl("chgind|change",names(lk),ignore.case=TRUE)][1]
assert(!is.na(c11)&&!is.na(c21),"Lookup lacks LSOA11CD/LSOA21CD columns. Columns: %s",paste(names(lk),collapse=", "))
lk2 <- data.frame(lsoa11cd=as.character(lk[[c11]]),lsoa21cd=as.character(lk[[c21]]),change_indicator=if(!is.na(chg)) as.character(lk[[chg]]) else NA_character_)
lk2 <- lk2[grepl("^E010",lk2$lsoa11cd)&grepl("^E010",lk2$lsoa21cd),]
assert(nrow(lk2)>32000,"England lookup rows unexpectedly low: %d",nrow(lk2))
x <- merge(lk2,jts,by="lsoa11cd",all.x=TRUE)
num <- names(jts)[grepl("_min$",names(jts))]
out <- dplyr::as_tibble(x) |>
  dplyr::group_by(.data$lsoa21cd) |>
  dplyr::summarise(
    dplyr::across(dplyr::all_of(num), ~ if(all(is.na(.x))) NA_real_ else mean(.x,na.rm=TRUE)),
    jts2019_n_source_lsoa11=dplyr::n_distinct(.data$lsoa11cd),
    jts2019_change_indicator=paste(sort(unique(stats::na.omit(.data$change_indicator))),collapse=";"),
    .groups="drop") |>
  as.data.frame()
base <- read_csv_flex(file.path(root,"data_staged","f0f1_baseline_inherited.csv"))
out <- merge(base[,c("lsoa21cd","lsoa21nm")],out,by="lsoa21cd",all.x=TRUE)
out$jts2019_harmonisation <- ifelse(out$jts2019_n_source_lsoa11==1 & out$jts2019_change_indicator %in% c("U",""),"unchanged_or_single_source","changed_geometry_mean_or_replication")
write_csv(out,file.path(root,"data_staged","jts2019_food_access_lsoa21_harmonised.csv"))
miss <- sapply(out[num],function(z)sum(is.na(z)))
qa <- data.frame(variable=names(miss),missing=as.integer(miss),coverage=nrow(out)-as.integer(miss))
write_csv(qa,file.path(root,"results","f2","jts2019_harmonisation_qa.csv"))
assert(nrow(out)==33755,"Harmonised JTS output rows %d != 33755",nrow(out))
cat(sprintf("JTS2019_HARMONISATION_OK rows=%d max_missing=%d changed_or_complex=%d\n",nrow(out),max(miss),sum(out$jts2019_harmonisation!="unchanged_or_single_source",na.rm=TRUE)))

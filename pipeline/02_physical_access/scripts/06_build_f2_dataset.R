root <- normalizePath(file.path(dirname(sub("^--file=", "", commandArgs(trailingOnly=FALSE)[grep("^--file=", commandArgs(trailingOnly=FALSE))][1])), ".."), mustWork=TRUE)
source(file.path(root,"R","functions.R"))
base <- read_csv_flex(file.path(root,"data_staged","f0f1_baseline_inherited.csv"))
jts <- read_csv_flex(file.path(root,"data_staged","jts2019_food_access_lsoa21_harmonised.csv"))
tcm <- read_csv_flex(file.path(root,"data_staged","tcm2025_shopping_benchmark_lsoa21.csv"))
jts_keep <- setdiff(names(jts),c("lsoa21nm"))
x <- merge(base,jts[,jts_keep,drop=FALSE],by="lsoa21cd",all.x=TRUE)
y <- merge(x,tcm,by="lsoa21cd",all.x=TRUE)
assert(nrow(y)==33755,"F2 merged row count %d != 33755",nrow(y))
assert(length(unique(y$lsoa21cd))==33755,"F2 merged codes are not unique")
write_csv(y,file.path(root,"data_staged","hfa_f2_physical_access.csv"))
# concise descriptives for numeric physical-access fields
phys <- names(y)[grepl("^jts2019_|^tcm2025_",names(y))]
num <- phys[vapply(y[phys],is.numeric,logical(1))]
desc <- do.call(rbind,lapply(num,function(v){z<-y[[v]];data.frame(variable=v,n=sum(is.finite(z)),mean=mean(z,na.rm=TRUE),sd=sd(z,na.rm=TRUE),min=min(z,na.rm=TRUE),median=median(z,na.rm=TRUE),max=max(z,na.rm=TRUE))}))
write_csv(desc,file.path(root,"results","f2","physical_access_descriptives.csv"))
cat(sprintf("HFA_F2_DATASET_OK rows=%d physical_numeric_fields=%d\n",nrow(y),length(num)))

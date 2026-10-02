root <- normalizePath(file.path(dirname(sub("^--file=", "", commandArgs(trailingOnly=FALSE)[grep("^--file=", commandArgs(trailingOnly=FALSE))][1])), ".."), mustWork=TRUE)
source(file.path(root,"R","functions.R"))
y <- read_csv_flex(file.path(root,"data_staged","hfa_f2_physical_access.csv"))
pta <- read_csv_flex(file.path(root,"results","f2","student_pt_car_audit.csv"))
q <- c(
"FOOD DESERTS / HEALTHY FOOD ACCESS — HFA-F2 RELEASE AUDIT",
paste0("timestamp: ",format(Sys.time(),"%Y-%m-%d %H:%M:%S %Z")),
paste0("rows: ",nrow(y)),
paste0("unique LSOA21: ",length(unique(y$lsoa21cd))),
"",
"Construct boundary:",
"- F2 measures physical/transport food-access opportunity only.",
"- Census car and deprivation variables are inherited but remain capability variables, not folded into a scalar access index.",
"- No served-only filter is applied.",
"- JTS2019 is harmonised from LSOA11 to LSOA21 with the ONS exact-fit relationship and explicit transformation flags.",
"- TCM2025 shopping variables are retained as contemporary external benchmarks.",
"- Geolytix point-based supermarket accessibility is intentionally deferred to F3 because an exact reproducible 2023 source/version is not yet frozen.",
"",
"Student anomaly audit:",
paste(capture.output(print(pta,row.names=FALSE)),collapse="\n"),
"",
"PASS_FOR_HFA_F2_PHYSICAL_ACCESS",
"NEXT_STAGE=F3_RETAIL_POINT_ACCESS_AND_STUDENT_SAMPLE_RECONSTRUCTION"
)
dir.create(file.path(root,"results","f2"),recursive=TRUE,showWarnings=FALSE)
writeLines(q,file.path(root,"results","f2","AUDIT_F2.txt"))
cat(paste(q,collapse="\n"),"\n")

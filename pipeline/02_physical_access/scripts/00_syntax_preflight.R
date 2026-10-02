root <- normalizePath(file.path(dirname(sub("^--file=", "", commandArgs(trailingOnly=FALSE)[grep("^--file=", commandArgs(trailingOnly=FALSE))][1])), ".."), mustWork=TRUE)
files <- c(Sys.glob(file.path(root,"R","*.R")), Sys.glob(file.path(root,"scripts","*.R")))
for (f in files) tryCatch(parse(f), error=function(e) stop(sprintf("SYNTAX_FAIL %s: %s", basename(f), conditionMessage(e)), call.=FALSE))
cat(sprintf("R_SYNTAX_PREFLIGHT_OK files=%d\n", length(files)))

options(stringsAsFactors = FALSE)

food24_sha256 <- function(path) {
  out <- system2("sha256sum", shQuote(path), stdout=TRUE, stderr=TRUE)
  if (!length(out)) stop("sha256sum failed: ", path, call.=FALSE)
  sub("[[:space:]].*$", "", out[[1L]])
}

food24_read_contract <- function(path) {
  x <- utils::read.csv(path, stringsAsFactors=FALSE, check.names=FALSE)
  stats::setNames(x$value, x$field)
}

food24_level <- function(z, low=-0.5, high=0.5, very_low=-1, very_high=1) {
  out <- rep("near_sample_mean", length(z))
  out[z <= low] <- "low"
  out[z >= high] <- "high"
  out[z <= very_low] <- "very_low"
  out[z >= very_high] <- "very_high"
  out[!is.finite(z)] <- NA_character_
  out
}

food24_spearman_flag <- function(x, strong=0.90, moderate=0.75) {
  ifelse(!is.finite(x), "NOT_AVAILABLE",
         ifelse(x >= strong, "STRONG",
                ifelse(x >= moderate, "MODERATE", "CAUTIOUS")))
}

food24_top_categories <- function(comp, variable, n=3L) {
  z <- comp[comp$variable == variable, , drop=FALSE]
  balls <- sort(unique(z$ball_id))
  rows <- lapply(balls, function(b) {
    q <- z[z$ball_id == b, , drop=FALSE]
    q <- q[order(-q$category_share, q$category), , drop=FALSE]
    q <- q[seq_len(min(n, nrow(q))), , drop=FALSE]
    data.frame(
      ball_id=b,
      rank=seq_len(nrow(q)),
      variable=variable,
      category=q$category,
      share=q$category_share,
      stringsAsFactors=FALSE
    )
  })
  do.call(rbind, rows)
}

food24_total_variation <- function(comp, variable, a, b) {
  x <- comp[comp$variable==variable & comp$ball_id==a, c("category","category_share")]
  y <- comp[comp$variable==variable & comp$ball_id==b, c("category","category_share")]
  lev <- union(x$category, y$category)
  px <- setNames(rep(0,length(lev)),lev); py <- px
  px[x$category] <- x$category_share
  py[y$category] <- y$category_share
  0.5 * sum(abs(px-py))
}

food24_safe_cor <- function(x,y,method="spearman") {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok)<3L || stats::sd(x[ok])==0 || stats::sd(y[ok])==0) return(NA_real_)
  suppressWarnings(stats::cor(x[ok],y[ok],method=method))
}

food24_write_md_table <- function(df, cols, digits=3) {
  x <- df[,cols,drop=FALSE]
  for (j in seq_along(x)) if (is.numeric(x[[j]])) x[[j]] <- format(round(x[[j]],digits), trim=TRUE, scientific=FALSE)
  hdr <- paste0("| ",paste(names(x),collapse=" | ")," |")
  sep <- paste0("| ",paste(rep("---",ncol(x)),collapse=" | ")," |")
  rows <- apply(x,1,function(r) paste0("| ",paste(r,collapse=" | ")," |"))
  c(hdr,sep,rows)
}

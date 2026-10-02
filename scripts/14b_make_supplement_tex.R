#!/usr/bin/env Rscript
options(stringsAsFactors = FALSE, warn = 1)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop('Usage: 14b_make_supplement_tex.R <repo_root>', call. = FALSE)
root <- normalizePath(args[[1L]], winslash = '/', mustWork = TRUE)
suppdir <- file.path(root, 'outputs', 'manuscript', 'supplementary')
tex <- file.path(root, 'manuscript', 'supplementary_material.tex')

s1 <- read.csv(file.path(suppdir, 'supplementary_table_S1_38_non_scalar_contrasts.csv'), check.names = FALSE)
s2 <- read.csv(file.path(suppdir, 'supplementary_table_S2_screen_threshold_sensitivity.csv'), check.names = FALSE)
s3 <- read.csv(file.path(suppdir, 'supplementary_table_S3_scalar_weight_sensitivity_summary.csv'), check.names = FALSE)

latex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub('\\\\', '\\\\textbackslash{}', x)
  x <- gsub('([#$%&_{}])', '\\\\\\1', x, perl = TRUE)
  x <- gsub('~', '\\\\textasciitilde{}', x, fixed = TRUE)
  x <- gsub('\\^', '\\\\textasciicircum{}', x)
  x
}
pretty_sig <- function(x) {
  x <- gsub('_', ' ', as.character(x), fixed = TRUE)
  x <- gsub('physical=', 'P=', x, fixed = TRUE)
  x <- gsub('transport=', 'T=', x, fixed = TRUE)
  x <- gsub('material=', 'M=', x, fixed = TRUE)
  latex_escape(x)
}

con <- file(tex, open = 'w', encoding = 'UTF-8')
on.exit(close(con), add = TRUE)
wl <- function(x = '') writeLines(x, con)
wl(c(
  '\\documentclass[10pt,a4paper]{article}',
  '\\usepackage[margin=1.4cm]{geometry}',
  '\\usepackage{booktabs}',
  '\\usepackage{longtable}',
  '\\usepackage{array}',
  '\\usepackage[T1]{fontenc}',
  '\\begin{document}',
  '\\section*{Supplementary material}',
  'Machine-readable versions of Tables S1--S3 and the complete scalar-weight sensitivity results are included with the accompanying replication package.',
  '',
  '\\subsection*{Table S1. Non-scalar Ball Mapper contrasts}',
  'Table S1 reports all 38 ball pairs meeting the main equal-weight screen: absolute scalar gap below 0.25 and three-axis composition distance above 1.0. Rows are ordered by increasing scalar gap.',
  '\\small',
  '\\begin{longtable}{rrrrp{4.0cm}p{4.0cm}}',
  '\\toprule',
  'Ball A & Ball B & Scalar gap & Composition distance & Configuration A & Configuration B \\\\',
  '\\midrule',
  '\\endfirsthead',
  '\\toprule',
  'Ball A & Ball B & Scalar gap & Composition distance & Configuration A & Configuration B \\\\',
  '\\midrule',
  '\\endhead'
))
for (i in seq_len(nrow(s1))) {
  wl(sprintf('%d & %d & %.4f & %.4f & %s & %s \\\\',
             s1$ball_a[i], s1$ball_b[i], s1$scalar_gap[i],
             s1$composition_euclidean_distance[i], pretty_sig(s1$signature_a[i]),
             pretty_sig(s1$signature_b[i])))
}
wl(c('\\bottomrule', '\\end{longtable}', '\\normalsize', ''))

wl(c(
  '\\subsection*{Table S2. Screen-threshold sensitivity}',
  'Counts of ball pairs meeting alternative combinations of scalar-gap and composition-distance thresholds.',
  '\\begin{center}',
  '\\begin{tabular}{rrr}',
  '\\toprule',
  'Scalar-gap threshold & Composition-distance threshold & Qualifying pairs \\\\',
  '\\midrule'
))
for (i in seq_len(nrow(s2))) {
  wl(sprintf('%.2f & %.1f & %d \\\\', s2$scalar_gap_threshold[i],
             s2$profile_distance_threshold[i], s2$n_qualifying_pairs[i]))
}
wl(c('\\bottomrule', '\\end{tabular}', '\\end{center}', ''))

wl(c(
  '\\subsection*{Table S3. Scalar-weight sensitivity}',
  'Summary counts across deterministic scalar-weight schemes. The complete 231-weight grid is included as machine-readable data.',
  '\\begin{center}',
  '\\begin{tabular}{lrrrr}',
  '\\toprule',
  'Weight subset & Schemes & Minimum & Median & Maximum \\\\',
  '\\midrule'
))
for (i in seq_len(nrow(s3))) {
  label <- latex_escape(gsub('_', ' ', s3$subset[i], fixed = TRUE))
  wl(sprintf('%s & %d & %g & %g & %g \\\\', label, s3$n_weight_schemes[i],
             s3$dist1_min[i], s3$dist1_median[i], s3$dist1_max[i]))
}
wl(c('\\bottomrule', '\\end{tabular}', '\\end{center}', '\\end{document}'))

cat('HFA_SUPPLEMENT_TEX=PASS\n')

#!/usr/bin/env Rscript
options(stringsAsFactors = FALSE, warn = 1)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop('Usage: 14_make_manuscript_outputs.R <repo_root>', call. = FALSE)
root <- normalizePath(args[[1L]], winslash = '/', mustWork = TRUE)
out <- file.path(root, 'outputs', 'manuscript')
fig <- file.path(out, 'figures')
tab <- file.path(out, 'tables')
supp <- file.path(out, 'supplementary')
dir.create(fig, recursive = TRUE, showWarnings = FALSE)
dir.create(tab, recursive = TRUE, showWarnings = FALSE)
dir.create(supp, recursive = TRUE, showWarnings = FALSE)

p14d <- file.path(root, 'pipeline', '10_evidence_robustness', 'food_application', 'results', 'p1_4_d')
food24 <- file.path(root, 'pipeline', '11_interpretation', 'results')
sp <- file.path(root, 'pipeline', '12_spatial_closure', 'results')
sc <- file.path(root, 'pipeline', '13_scalarisation_robustness', 'results')

readc <- function(p) {
  if (!file.exists(p)) stop('Required manuscript evidence missing: ', p, call. = FALSE)
  utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
}

# -------------------------------------------------------------------------
# Machine-readable manuscript and supplementary tables.
# -------------------------------------------------------------------------
contr <- readc(file.path(food24, 'ball_pairwise_configuration_contrasts.csv'))
is_candidate <- as.logical(contr$candidate_non_scalar_contrast)
s1 <- contr[is_candidate %in% TRUE, , drop = FALSE]
s1 <- s1[order(s1$scalar_gap, -s1$composition_euclidean_distance), , drop = FALSE]
if (nrow(s1) != 38L) stop('Expected 38 non-scalar contrasts; observed ', nrow(s1), call. = FALSE)
utils::write.csv(s1, file.path(supp, 'supplementary_table_S1_38_non_scalar_contrasts.csv'), row.names = FALSE)

copy_map <- c(
  screen_threshold_sensitivity.csv = 'supplementary_table_S2_screen_threshold_sensitivity.csv',
  scalar_weight_sensitivity_summary.csv = 'supplementary_table_S3_scalar_weight_sensitivity_summary.csv',
  scalar_weight_grid_005.csv = 'supplementary_data_S3_full_weight_grid.csv',
  pair_frequency_balanced_weights.csv = 'supplementary_data_pair_frequency_balanced_weights.csv'
)
for (src_name in names(copy_map)) {
  src <- file.path(sc, src_name)
  if (!file.exists(src)) stop('Scalarisation output missing: ', src, call. = FALSE)
  file.copy(src, file.path(supp, unname(copy_map[[src_name]])), overwrite = TRUE)
}

p1318 <- contr[(contr$ball_a == 13 & contr$ball_b == 18) |
               (contr$ball_a == 18 & contr$ball_b == 13), , drop = FALSE]
if (nrow(p1318) != 1L) stop('Ball 13/18 contrast is not unique.', call. = FALSE)
utils::write.csv(p1318, file.path(tab, 'table1_principal_non_scalar_contrast.csv'), row.names = FALSE)

# -------------------------------------------------------------------------
# Heatmap helper.
# -------------------------------------------------------------------------
draw_heatmap <- function(mat, row_labels, pdf_file, png_file, width = 12, height = 3.4,
                         zlim = c(-4, 4)) {
  stopifnot(nrow(mat) == length(row_labels))
  nr <- nrow(mat); nc <- ncol(mat)
  cols <- grDevices::colorRampPalette(c('#2166AC', '#F7F7F7', '#B2182B'))(201)
  # Preserve matrix dimensions while clipping to the plotting range.
  # pmax()/pmin() with a scalar first argument can drop matrix dimensions.
  z <- mat
  z[is.finite(z) & z < zlim[[1L]]] <- zlim[[1L]]
  z[is.finite(z) & z > zlim[[2L]]] <- zlim[[2L]]
  draw <- function() {
    op <- graphics::par(mar = c(4.4, 12.0, 0.7, 1.0), xpd = NA)
    on.exit(graphics::par(op), add = TRUE)
    graphics::image(
      x = seq_len(nc), y = seq_len(nr), z = t(z[nr:1, , drop = FALSE]),
      col = cols, zlim = zlim, axes = FALSE, xlab = '', ylab = ''
    )
    graphics::axis(1, at = seq_len(nc), labels = seq_len(nc), cex.axis = 0.72)
    graphics::axis(2, at = seq_len(nr), labels = rev(row_labels), las = 1, cex.axis = 0.82)
    graphics::mtext('Ball', side = 1, line = 2.8)
    graphics::box()
  }
  grDevices::pdf(pdf_file, width = width, height = height, useDingbats = FALSE)
  draw(); grDevices::dev.off()
  grDevices::png(png_file, width = 2000, height = round(2000 * height / width), res = 170)
  draw(); grDevices::dev.off()
}

# -------------------------------------------------------------------------
# Figure 2: topology-axis profiles.
# -------------------------------------------------------------------------
axis_profiles <- readc(file.path(p14d, 'canonical_ball_axis_profiles.csv'))
axis_vars <- c(
  'physical_friction_log_nearest_large_store_km',
  'transport_constraint_no_car_pct',
  'material_constraint_income_deprivation_2025'
)
axis_labels <- c('Physical friction', 'Transport constraint', 'Material constraint')
m2 <- matrix(NA_real_, nrow = length(axis_vars), ncol = 24L)
for (i in seq_along(axis_vars)) {
  q <- axis_profiles[axis_profiles$variable == axis_vars[[i]], , drop = FALSE]
  if (nrow(q) != 24L) stop('Expected 24 axis-profile rows for ', axis_vars[[i]], call. = FALSE)
  m2[i, q$ball_id] <- q$standardized_difference
}
draw_heatmap(
  m2, axis_labels,
  file.path(fig, 'figure2_topology_axis_heatmap.pdf'),
  file.path(fig, 'figure2_topology_axis_heatmap.png'),
  height = 3.0
)

# -------------------------------------------------------------------------
# Figure 3: selected contextual readouts.
# -------------------------------------------------------------------------
colour_profiles <- readc(file.path(p14d, 'canonical_ball_colour_profiles_numeric.csv'))
ctx_vars <- c(
  'tcm_shopping_walk', 'tcm_shopping_cycle', 'tcm_shopping_public_transport',
  'tcm_shopping_drive', 'tcm_shopping_overall', 'internal_qualifying_store_count'
)
ctx_labels <- c(
  'Shopping: walk', 'Shopping: cycle', 'Shopping: public transport',
  'Shopping: drive', 'Shopping: overall', 'Qualifying-store count'
)
m3 <- matrix(NA_real_, nrow = length(ctx_vars), ncol = 24L)
for (i in seq_along(ctx_vars)) {
  q <- colour_profiles[colour_profiles$variable == ctx_vars[[i]], , drop = FALSE]
  if (nrow(q) != 24L) stop('Expected 24 contextual-profile rows for ', ctx_vars[[i]], call. = FALSE)
  m3[i, q$ball_id] <- q$standardized_difference
}
draw_heatmap(
  m3, ctx_labels,
  file.path(fig, 'figure3_context_readout_heatmap.pdf'),
  file.path(fig, 'figure3_context_readout_heatmap.png'),
  height = 4.7
)

# -------------------------------------------------------------------------
# Figure 1: one fixed topology/layout, recoloured by each topology dimension.
# -------------------------------------------------------------------------
layout_tab <- readc(file.path(p14d, 'canonical_paper_layout.csv'))
edges <- readc(file.path(p14d, 'canonical_edges.csv'))
wide <- readc(file.path(p14d, 'canonical_ball_profile_wide.csv'))
layout_tab <- layout_tab[order(layout_tab$ball_id), , drop = FALSE]
wide <- wide[match(layout_tab$ball_id, wide$ball_id), , drop = FALSE]
if (!all(c('x', 'y') %in% names(layout_tab))) stop('Canonical paper layout lacks x/y columns.', call. = FALSE)
zdiff_cols <- paste0('zdiff__', axis_vars)
if (!all(zdiff_cols %in% names(wide))) stop('Canonical wide profile lacks topology z-difference columns.', call. = FALSE)
if (!all(c('from', 'to') %in% names(edges))) stop('Canonical edge table lacks from/to columns.', call. = FALSE)

X <- layout_tab$x; Y <- layout_tab$y
names(X) <- as.character(layout_tab$ball_id); names(Y) <- as.character(layout_tab$ball_id)
pal <- grDevices::colorRampPalette(c('#2166AC', '#F7F7F7', '#B2182B'))(201)
map_col <- function(v, zlim = c(-4, 4)) {
  vv <- pmax(zlim[[1L]], pmin(zlim[[2L]], v))
  idx <- round((vv - zlim[[1L]]) / diff(zlim) * 200) + 1L
  pal[pmax(1L, pmin(201L, idx))]
}
draw_bm <- function(v, title) {
  xr <- range(X); yr <- range(Y)
  graphics::plot(
    xr + c(-0.08, 0.08) * diff(xr), yr + c(-0.08, 0.08) * diff(yr),
    type = 'n', axes = FALSE, xlab = '', ylab = '', asp = 1, main = title
  )
  if (nrow(edges)) {
    for (i in seq_len(nrow(edges))) {
      a <- as.character(edges$from[[i]]); b <- as.character(edges$to[[i]])
      graphics::segments(X[[a]], Y[[a]], X[[b]], Y[[b]],
                         col = grDevices::adjustcolor('grey25', alpha.f = 0.32), lwd = 0.8)
    }
  }
  sz <- sqrt(wide$n_members)
  cex <- 0.80 + 1.45 * (sz - min(sz)) / (max(sz) - min(sz) + 1e-12)
  graphics::points(X, Y, pch = 21, bg = map_col(v), col = 'grey15', cex = cex, lwd = 0.65)
  graphics::text(X, Y, labels = layout_tab$ball_id, cex = 0.60)
}
draw_fig1 <- function() {
  graphics::par(mfrow = c(1, 3), mar = c(1.0, 0.8, 2.1, 0.4), oma = c(2.5, 0.2, 0.2, 0.2))
  for (i in seq_along(axis_vars)) draw_bm(wide[[zdiff_cols[[i]]]], axis_labels[[i]])
  graphics::mtext('Standardised difference from England mean', side = 1, outer = TRUE, line = 0.7)
}
grDevices::pdf(file.path(fig, 'figure1_canonical_bm_three_axes.pdf'), width = 13.5, height = 4.4, useDingbats = FALSE)
draw_fig1(); grDevices::dev.off()
grDevices::png(file.path(fig, 'figure1_canonical_bm_three_axes.png'), width = 2400, height = 780, res = 180)
draw_fig1(); grDevices::dev.off()

# -------------------------------------------------------------------------
# Figure 4: spatial closure, using the accepted fresh spatial outputs.
# -------------------------------------------------------------------------
knn <- readc(file.path(sp, 'topology_geography_knn_summary.csv'))
dec <- readc(file.path(sp, 'configuration_geography_distance_deciles.csv'))

plot4a <- function() {
  op <- graphics::par(mar = c(4.5, 5.4, 0.8, 0.8)); on.exit(graphics::par(op), add = TRUE)
  graphics::plot(
    knn$k, 100 * knn$zero_overlap_share, type = 'b', pch = 19, lwd = 1.4,
    xlab = 'Number of neighbours, k', ylab = 'LSOAs with zero shared neighbours (%)',
    ylim = c(0, 100), xaxt = 'n'
  )
  graphics::axis(1, at = knn$k)
}
grDevices::pdf(file.path(fig, 'figure4a_zero_overlap_share.pdf'), width = 5.5, height = 4.5, useDingbats = FALSE)
plot4a(); grDevices::dev.off()
grDevices::png(file.path(fig, 'figure4a_zero_overlap_share.png'), width = 1000, height = 820, res = 160)
plot4a(); grDevices::dev.off()

plot4b <- function() {
  op <- graphics::par(mar = c(4.5, 5.6, 0.8, 0.8)); on.exit(graphics::par(op), add = TRUE)
  graphics::plot(
    dec$decile, dec$median_geographic_distance_km, type = 'b', pch = 19, lwd = 1.4,
    xlab = 'Configuration-distance decile (1 = most similar)',
    ylab = 'Median geographic distance (km)', xaxt = 'n'
  )
  graphics::axis(1, at = dec$decile)
}
grDevices::pdf(file.path(fig, 'figure4b_geographic_distance_deciles.pdf'), width = 5.5, height = 4.5, useDingbats = FALSE)
plot4b(); grDevices::dev.off()
grDevices::png(file.path(fig, 'figure4b_geographic_distance_deciles.png'), width = 1000, height = 820, res = 160)
plot4b(); grDevices::dev.off()

writeLines('HFA_MANUSCRIPT_OUTPUTS=PASS', file.path(out, 'STATUS.txt'))
cat('HFA_MANUSCRIPT_OUTPUTS=PASS\n')

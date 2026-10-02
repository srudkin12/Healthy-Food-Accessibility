#!/usr/bin/env python3
from __future__ import annotations

import csv
import hashlib
import json
import os
from pathlib import Path
import sys


def sha256(path: Path) -> str:
    h = hashlib.sha256()
    with path.open('rb') as f:
        for block in iter(lambda: f.read(1024 * 1024), b''):
            h.update(block)
    return h.hexdigest()


def read_csv(path: Path):
    if not path.is_file():
        raise FileNotFoundError(f'Required verification file missing: {path}')
    with path.open(newline='', encoding='utf-8') as f:
        return list(csv.DictReader(f))


def as_float(x):
    return float(x)


def main():
    if len(sys.argv) != 2:
        raise SystemExit('Usage: 15_verify_replication.py <repo_root>')
    root = Path(sys.argv[1]).resolve()
    exp = json.loads((root / 'config' / 'expected_results.json').read_text(encoding='utf-8'))
    n_reps = int(os.environ.get('N_REPS', '1000'))
    canonical_reps = int(exp.get('canonical_repetitions', 1000))
    canonical_mode = (n_reps == canonical_reps)
    out = root / 'outputs' / 'verification'
    out.mkdir(parents=True, exist_ok=True)
    checks = []

    def add(name, observed, expected, tol=0.0, blocking=True, note=''):
        if isinstance(observed, (int, float)) and isinstance(expected, (int, float)):
            passed = abs(float(observed) - float(expected)) <= tol
        else:
            passed = str(observed) == str(expected)
        checks.append({
            'check': name, 'status': 'PASS' if passed else 'FAIL', 'blocking': blocking,
            'observed': observed, 'expected': expected, 'tolerance': tol, 'note': note
        })
        if not passed:
            print(f"{'FAIL' if blocking else 'WARN'} {name}: observed={observed!r} expected={expected!r}")

    # Frozen analytical input identity.
    p1a = root / 'pipeline' / '07_input_freeze' / 'food_application' / 'results' / 'p1_4_a'
    frozen = p1a / 'frozen' / 'food_hfa_analysis_input.csv'
    add('frozen_input_sha256', sha256(frozen), exp['frozen_input_sha256'])

    # Canonical topology identity and summary.
    p1c = root / 'pipeline' / '09_canonical_topology' / 'food_application' / 'results' / 'p1_4_c'
    fp_lines = (p1c / 'canonical_topology_fingerprint.txt').read_text(encoding='utf-8').splitlines()
    fp = [x.split('=', 1)[1] for x in fp_lines if x.startswith('TOPOLOGY_FINGERPRINT_SHA256=')]
    if len(fp) != 1:
        raise RuntimeError('Topology fingerprint line missing/nonunique')
    add('topology_fingerprint', fp[0], exp['topology_fingerprint_sha256'])
    add('canonical_object_sha256', sha256(p1c / 'canonical_ballmapper_object.rds'),
        exp['canonical_object_sha256'], blocking=False,
        note='RDS byte identity is environment-sensitive; topology fingerprint is the blocking identity check.')

    top = read_csv(p1c / 'canonical_topology_summary.csv')[0]
    add('n_observations', int(float(top['n_observations'])), int(exp['n_observations']))
    add('n_balls', int(float(top['n_balls'])), int(exp['n_balls']))
    add('n_edges', int(float(top['n_edges'])), int(exp['n_edges']))
    add('n_components', int(float(top['n_components'])), int(exp['n_components']))
    add('fraction_multicovered', as_float(top['fraction_multicovered']),
        as_float(exp['fraction_multicovered']), 5e-7)
    add('approved_radius', as_float(top['approved_radius']), as_float(exp['approved_radius']), 1e-12)

    # Evidence / robustness.
    # Stage 10 produces the raw robustness summaries; Stage 11 consolidates them
    # into the manuscript-facing core_variable_robustness_evidence.csv.
    p1d = root / 'pipeline' / '10_evidence_robustness' / 'food_application' / 'results' / 'p1_4_d'
    food24 = root / 'pipeline' / '11_interpretation' / 'results'
    robust = read_csv(food24 / 'core_variable_robustness_evidence.csv')
    order_vals = [float(r['canonical_vs_repeated_mean_spearman']) for r in robust
                  if r.get('canonical_vs_repeated_mean_spearman', '') not in ('', 'NA')]
    rad_vals = [float(r['min_nearby_radius_repeated_mean_spearman']) for r in robust
                if r.get('min_nearby_radius_repeated_mean_spearman', '') not in ('', 'NA')]
    add('min_order_spearman', min(order_vals), exp['min_order_spearman'], 5e-7,
        blocking=canonical_mode,
        note='Exact manuscript value is required only for canonical N_REPS=1000 runs.')
    add('min_nearby_radius_spearman', min(rad_vals), exp['min_nearby_radius_spearman'], 5e-7,
        blocking=canonical_mode,
        note='Exact manuscript value is required only for canonical N_REPS=1000 runs.')

    # Interpretation / contrasts.
    contrasts = read_csv(food24 / 'ball_pairwise_configuration_contrasts.csv')
    candidates = [r for r in contrasts if str(r['candidate_non_scalar_contrast']).upper() in {'TRUE', '1'}]
    add('n_non_scalar_contrasts', len(candidates), int(exp['n_non_scalar_contrasts']))
    p = [r for r in contrasts if {int(r['ball_a']), int(r['ball_b'])} == {13, 18}]
    if len(p) != 1:
        add('ball13_18_unique', len(p), 1)
    else:
        add('ball13_18_scalar_gap', float(p[0]['scalar_gap']), exp['ball13_18_scalar_gap'], 1e-10)
        add('ball13_18_composition_distance', float(p[0]['composition_euclidean_distance']),
            exp['ball13_18_composition_distance'], 1e-8)

    km = read_csv(food24 / 'kmeans_alignment_by_ball.csv')
    n_low = sum(float(r['modal_kmeans_share']) < 0.80 for r in km)
    add('kmeans_balls_modal_share_lt_080', n_low, int(exp['kmeans_balls_modal_share_lt_080']))

    # Spatial closure.
    spatial = root / 'pipeline' / '12_spatial_closure' / 'results'
    knn = read_csv(spatial / 'topology_geography_knn_summary.csv')
    k25 = [r for r in knn if int(float(r['k'])) == 25]
    if len(k25) != 1:
        add('knn25_unique', len(k25), 1)
    else:
        r = k25[0]
        add('knn25_mean_jaccard', float(r['mean_jaccard']), exp['knn25_mean_jaccard'], 5e-7)
        add('knn25_zero_overlap_share', float(r['zero_overlap_share']), exp['knn25_zero_overlap_share'], 5e-7)
        add('knn25_median_geo_distance_km',
            float(r['median_geo_distance_of_topological_neighbours_km']),
            exp['knn25_median_geo_distance_km'], 1e-5)

    pair = read_csv(spatial / 'configuration_geography_pair_summary.csv')[0]
    add('pair500k_pearson', float(pair['pearson_config_geo']), exp['pair500k_pearson'], 5e-7)
    add('pair500k_spearman', float(pair['spearman_config_geo']), exp['pair500k_spearman'], 5e-7)

    spatial_contrasts = read_csv(spatial / 'non_scalar_contrast_spatial_context.csv')
    b716 = [r for r in spatial_contrasts if {int(r['ball_a']), int(r['ball_b'])} == {7, 16}]
    if len(b716) != 1:
        add('ball7_16_unique', len(b716), 1)
    else:
        r = b716[0]
        add('ball7_16_centroid_km', float(r['geographic_centroid_distance_km']), exp['ball7_16_centroid_km'], 0.001)
        add('ball7_16_min_exclusive_km', float(r['minimum_exclusive_member_distance_km']), exp['ball7_16_min_exclusive_km'], 0.001)
        add('ball7_16_queen_edges', int(float(r['cross_ball_queen_adjacency_edges'])), int(exp['ball7_16_queen_edges']))

    # Scalarisation robustness.
    scal = root / 'pipeline' / '13_scalarisation_robustness' / 'results'
    ss = read_csv(scal / 'pre_submission_scalarisation_summary.csv')
    vals = {r['metric']: float(r['value']) for r in ss}
    add('strict_scalar_screen_pairs',
        vals['Strict threshold qualifying pairs: gap<0.05, distance>3'], exp['strict_scalar_screen_pairs'])
    add('balanced_weight_schemes',
        vals['Balanced weight schemes (each dimension >=0.20)'], exp['balanced_weight_schemes'])
    add('balanced_weight_min_pairs_dist1',
        vals['Balanced weights: minimum qualifying pairs, distance>1'], exp['balanced_weight_min_pairs_dist1'])
    add('balanced_weight_median_pairs_dist1',
        vals['Balanced weights: median qualifying pairs, distance>1'], exp['balanced_weight_median_pairs_dist1'])
    add('balanced_weight_max_pairs_dist1',
        vals['Balanced weights: maximum qualifying pairs, distance>1'], exp['balanced_weight_max_pairs_dist1'])

    # Manuscript-output presence.
    mroot = root / 'outputs' / 'manuscript'
    mstatus = mroot / 'STATUS.txt'
    add('manuscript_output_status',
        mstatus.is_file() and 'HFA_MANUSCRIPT_OUTPUTS=PASS' in mstatus.read_text(encoding='utf-8'),
        True)
    for rel in [
        'figures/figure1_canonical_bm_three_axes.pdf',
        'figures/figure2_topology_axis_heatmap.pdf',
        'figures/figure3_context_readout_heatmap.pdf',
        'figures/figure4a_zero_overlap_share.pdf',
        'figures/figure4b_geographic_distance_deciles.pdf',
        'supplementary/supplementary_table_S1_38_non_scalar_contrasts.csv',
        'supplementary/supplementary_table_S2_screen_threshold_sensitivity.csv',
        'supplementary/supplementary_table_S3_scalar_weight_sensitivity_summary.csv',
        'supplementary/supplementary_data_S3_full_weight_grid.csv',
    ]:
        pth = mroot / rel
        add('output_exists:' + rel, pth.is_file() and pth.stat().st_size > 0, True)

    report = out / 'replication_validation_report.csv'
    with report.open('w', newline='', encoding='utf-8') as f:
        fields = ['check', 'status', 'blocking', 'observed', 'expected', 'tolerance', 'note']
        w = csv.DictWriter(f, fieldnames=fields)
        w.writeheader(); w.writerows(checks)

    blocking_failures = [r for r in checks if r['blocking'] and r['status'] != 'PASS']
    nonblocking_failures = [r for r in checks if not r['blocking'] and r['status'] != 'PASS']
    status = 'PASS' if not blocking_failures else 'FAIL'
    mode = 'CANONICAL' if canonical_mode else 'QUICK_NONCANONICAL'
    (out / 'STATUS.txt').write_text(
        f'HFA_REPLICATION_VERIFICATION={status}\n'
        f'REPLICATION_MODE={mode}\n'
        f'N_REPS={n_reps}\n'
        f'CANONICAL_REPS={canonical_reps}\n'
        f'CHECKS={len(checks)}\n'
        f'BLOCKING_FAILURES={len(blocking_failures)}\n'
        f'NONBLOCKING_WARNINGS={len(nonblocking_failures)}\n', encoding='utf-8'
    )
    if blocking_failures:
        raise SystemExit(f'Replication verification failed: {len(blocking_failures)} blocking checks')
    print('HFA_REPLICATION_VERIFICATION=PASS')
    print(f'CHECKS={len(checks)} NONBLOCKING_WARNINGS={len(nonblocking_failures)}')


if __name__ == '__main__':
    main()

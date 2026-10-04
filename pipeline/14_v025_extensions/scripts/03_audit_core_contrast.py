#!/usr/bin/env python3
from pathlib import Path
import csv, gzip, os, sys, json, math

OUT=Path(os.environ['RUN_OUTPUT_DIR']).resolve()
PKG=Path(os.environ['PACKAGE_DIR']).resolve()
REPS=int(os.environ.get('REPETITIONS','1000'))

def fail(msg):
    (OUT/'AUDIT_STATUS.txt').write_text('AUDIT=FAIL\n'+msg+'\n')
    print('AUDIT_FAIL:',msg,file=sys.stderr)
    raise SystemExit(3)

def read_csv(path, gz=False):
    op = gzip.open if gz else open
    mode = 'rt' if gz else 'r'
    with op(path, mode, newline='') as f:
        return list(csv.DictReader(f))

def f(x): return float(x)
def i(x): return int(round(float(x)))
def close(a,b,tol=1e-9): return abs(float(a)-float(b)) <= tol

checks=[]
def check(name, observed, expected, tol=0):
    ok=close(observed,expected,tol) if tol else float(observed)==float(expected)
    checks.append([name,observed,expected,tol,'PASS' if ok else 'FAIL'])
    if not ok: fail(f'{name}: observed={observed}, expected={expected}, tol={tol}')

# Canonical hard gates.
met={r['metric']:f(r['observed']) for r in read_csv(OUT/'canonical_metrics_reproduced.csv')}
check('canonical_n_lsoa',met['n_lsoa'],33755)
check('canonical_n_balls',met['n_balls'],24)
check('canonical_total_pairs',met['total_pairs'],276)
check('canonical_n_qualifying_pairs',met['n_qualifying_pairs'],38)
check('canonical_pair_prevalence',met['pair_prevalence'],38/276,1e-12)
check('canonical_ball13_18_scalar_gap',met['ball13_18_scalar_gap'],0.0018742566141,1e-9)
check('canonical_ball13_18_profile_distance',met['ball13_18_profile_distance'],4.3972327089,2e-6)

exp_land=[r['unit_id'] for r in read_csv(PKG/'config/expected_canonical_landmarks.csv')]
obs_land=[r['unit_id'] for r in read_csv(OUT/'canonical_landmarks_reproduced.csv')]
if exp_land!=obs_land: fail('canonical landmark unit-id sequence mismatch')
checks.append(['canonical_landmark_sequence','exact','exact',0,'PASS'])

exp_grid=read_csv(PKG/'config/expected_canonical_threshold_grid.csv')
obs_grid=read_csv(OUT/'canonical_threshold_grid_reproduced.csv')
obs_map={(round(f(r['scalar_gap_threshold']),8),round(f(r['profile_distance_threshold']),8)):i(r['n_qualifying_pairs']) for r in obs_grid}
for r in exp_grid:
    k=(round(f(r['scalar_gap_threshold']),8),round(f(r['profile_distance_threshold']),8))
    if k not in obs_map or obs_map[k]!=i(r['n_qualifying_pairs']):
        fail(f'canonical threshold grid mismatch at {k}: observed={obs_map.get(k)}, expected={r["n_qualifying_pairs"]}')
checks.append(['canonical_threshold_grid_30_cells','exact','exact',0,'PASS'])

main=read_csv(OUT/'core_contrast_by_replicate.csv')
if len(main)!=5*REPS: fail(f'expected {5*REPS} robustness rows, found {len(main)}')
for r in main:
    nb=i(r['n_balls']); tp=i(r['total_pairs']); nq=i(r['n_qualifying_pairs']); pp=f(r['pair_prevalence'])
    if nq<0 or nq>tp or not (0 <= pp <= 1): fail('invalid main robustness row: '+str(r))
checks.append(['robustness_row_count',len(main),5*REPS,0,'PASS'])

# Accepted P1.4-D ball-count scale validation for the full run.
exp=read_csv(PKG/'config/expected_radius_ballcount_summary.csv')
sumrows=read_csv(OUT/'core_contrast_summary_by_radius.csv')
hist=[]
if REPS==1000:
    for e in exp:
        rr=f(e['radius'])
        cand=[r for r in sumrows if abs(f(r['radius'])-rr)<1e-9]
        if len(cand)!=1: fail(f'missing radius summary {rr}')
        o=cand[0]
        med_ok=abs(f(o['median_n_balls'])-f(e['median_n_balls']))<=0.0
        q10_ok=abs(f(o['q10_n_balls'])-f(e['q10_n_balls']))<=1.0
        q90_ok=abs(f(o['q90_n_balls'])-f(e['q90_n_balls']))<=1.0
        status='PASS' if med_ok and q10_ok and q90_ok else 'FAIL'
        row=[rr,e['median_n_balls'],o['median_n_balls'],e['q10_n_balls'],o['q10_n_balls'],e['q90_n_balls'],o['q90_n_balls'],status]
        hist.append(row)
        if status!='PASS': fail('historical ball-count distribution mismatch: '+str(row))
    checks.append(['historical_ballcount_distribution','accepted P1.4-D','reproduced',0,'PASS'])
else:
    hist.append(['','','','','','','','SKIPPED_SMOKE_TEST'])
    checks.append(['historical_ballcount_distribution','1000 repetitions required',REPS,0,'SKIPPED_SMOKE_TEST'])
with (OUT/'historical_ballcount_validation.csv').open('w',newline='') as fh:
    w=csv.writer(fh); w.writerow(['radius','expected_median','observed_median','expected_q10','observed_q10','expected_q90','observed_q90','status']); w.writerows(hist)

# Threshold grid size + main-cell equivalence.
main_map={(round(f(r['radius']),8),i(r['replicate'])):i(r['n_qualifying_pairs']) for r in main}
grid_count=0; maincell_count=0
with gzip.open(OUT/'threshold_grid_by_replicate.csv.gz','rt',newline='') as fh:
    for r in csv.DictReader(fh):
        grid_count+=1
        if abs(f(r['scalar_gap_threshold'])-0.25)<1e-10 and abs(f(r['profile_distance_threshold'])-1.0)<1e-10:
            maincell_count+=1
            k=(round(f(r['radius']),8),i(r['replicate']))
            if k not in main_map or main_map[k]!=i(r['n_qualifying_pairs']): fail(f'main-cell mismatch for {k}')
if grid_count!=5*REPS*30: fail(f'expected {5*REPS*30} threshold-grid rows, found {grid_count}')
if maincell_count!=5*REPS: fail(f'expected {5*REPS} main threshold cells, found {maincell_count}')
checks.append(['threshold_grid_main_cell_equivalence','exact','exact',0,'PASS'])

with (OUT/'VALIDATION_CHECKS.csv').open('w',newline='') as fh:
    w=csv.writer(fh); w.writerow(['check','observed','expected','tolerance','status']); w.writerows(checks)

# Scientific result summary: not used as an execution PASS/FAIL gate.
pps=[f(r['pair_prevalence']) for r in main]
nqs=[i(r['n_qualifying_pairs']) for r in main]
cross=read_csv(OUT/'cross_radius_same_order_summary.csv')
allfive=[str(r['all_five_radii_have_qualifying_pair']).strip().upper() in ('TRUE','T','1') for r in cross]
scientific={
    'canonical_qualifying_pairs':38,
    'canonical_total_pairs':276,
    'canonical_pair_prevalence':38/276,
    'minimum_qualifying_pairs_across_all_constructions':min(nqs),
    'maximum_qualifying_pairs_across_all_constructions':max(nqs),
    'minimum_pair_prevalence_across_all_constructions':min(pps),
    'median_pair_prevalence_across_all_constructions':sorted(pps)[len(pps)//2] if len(pps)%2 else (sorted(pps)[len(pps)//2-1]+sorted(pps)[len(pps)//2])/2,
    'maximum_pair_prevalence_across_all_constructions':max(pps),
    'share_constructions_with_any_qualifying_pair':sum(x>0 for x in nqs)/len(nqs),
    'share_same_order_replicates_with_nonzero_contrast_at_all_five_radii':sum(allfive)/len(allfive),
}
(OUT/'SCIENTIFIC_RESULT_SUMMARY.json').write_text(json.dumps(scientific,indent=2)+'\n')
with (OUT/'SCIENTIFIC_RESULT_SUMMARY.txt').open('w') as fh:
    for k,v in scientific.items(): fh.write(f'{k}={v}\n')

(OUT/'AUDIT_STATUS.txt').write_text('AUDIT=PASS\nSCIENTIFIC_FINDING_NOT_GATED=TRUE\n')
print('AUDIT=PASS')
print('SCIENTIFIC_FINDING_NOT_GATED=TRUE')
for k,v in scientific.items(): print(f'{k}={v}')

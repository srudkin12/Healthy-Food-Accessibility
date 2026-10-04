#!/usr/bin/env python3
from pathlib import Path
import csv, gzip, math, shutil, sys, json

if len(sys.argv) != 4:
    raise SystemExit("usage: 04_build_public_outputs.py REPO_ROOT RESULTS_ROOT REPETITIONS")

repo = Path(sys.argv[1]).resolve()
results = Path(sys.argv[2]).resolve()
reps = int(sys.argv[3])
knn_raw = results / "knn_raw"
core_raw = results / "core_raw"
public = results / "public"
public.mkdir(parents=True, exist_ok=True)
expected = repo / "expected" / "manuscript_v0_25" / "supplementary"

def read_csv(path):
    with open(path, newline="") as fh:
        return list(csv.DictReader(fh))

def write_csv(path, fieldnames, rows):
    with open(path, "w", newline="") as fh:
        w=csv.DictWriter(fh, fieldnames=fieldnames)
        w.writeheader()
        for r in rows: w.writerow(r)

def f(x): return float(x)
def i(x): return int(round(float(x)))

# ---- KNN public outputs ----
core = read_csv(knn_raw / "knn_overlap_extended_k1_100.csv")
display = read_csv(knn_raw / "topology_geography_knn_summary_extended_display.csv")
if len(core) != 100 or [i(r["k"]) for r in core] != list(range(1,101)):
    raise SystemExit("FAIL_CLOSED: KNN k=1..100 output malformed")

N = 33755
M = N - 1
def zero_prob(k):
    # Exact combinatorial ratio C(M-k,k)/C(M,k), evaluated as a stable product.
    p=1.0
    for j in range(k):
        p *= (M-k-j)/(M-j)
    return p

all_bench=[]
for r in core:
    k=i(r["k"])
    z=zero_prob(k)
    rr=dict(r)
    rr["independent_set_zero_overlap_share"]=format(z, ".16g")
    rr["independent_set_mean_shared_neighbours"]=format(k*k/M, ".16g")
    all_bench.append(rr)

display_map={i(r["k"]):r for r in display}
core_map={i(r["k"]):r for r in core}
display_bench=[]
for k in range(5,101,5):
    if k not in display_map or k not in core_map:
        raise SystemExit(f"FAIL_CLOSED: display KNN missing k={k}")
    # The benchmark display file intentionally contains the seven overlap
    # metrics plus the two exact independent-set quantities. Distance-link
    # columns remain in supplementary_data_knn_overlap_display_k5_100.csv.
    rr=dict(core_map[k])
    rr["independent_set_zero_overlap_share"]=format(zero_prob(k), ".16g")
    rr["independent_set_mean_shared_neighbours"]=format(k*k/M, ".16g")
    display_bench.append(rr)

shutil.copy2(knn_raw/"knn_overlap_extended_k1_100.csv",
             public/"supplementary_data_knn_overlap_k1_100.csv")
shutil.copy2(knn_raw/"topology_geography_knn_summary_extended_display.csv",
             public/"supplementary_data_knn_overlap_display_k5_100.csv")
write_csv(public/"supplementary_data_knn_overlap_with_independent_set_benchmark.csv",
          list(all_bench[0].keys()), all_bench)
write_csv(public/"supplementary_data_knn_overlap_display_with_benchmark_k5_100.csv",
          list(display_bench[0].keys()), display_bench)

# ---- Core contrast public outputs ----
main = read_csv(core_raw/"core_contrast_by_replicate.csv")
if len(main) != 5*reps:
    raise SystemExit(f"FAIL_CLOSED: expected {5*reps} core-contrast rows, found {len(main)}")
shutil.copy2(core_raw/"core_contrast_by_replicate.csv",
             public/"supplementary_data_core_contrast_by_replicate.csv")
shutil.copy2(core_raw/"threshold_grid_by_replicate.csv.gz",
             public/"supplementary_data_core_contrast_threshold_by_replicate.csv.gz")

summary = read_csv(core_raw/"core_contrast_summary_by_radius.csv")
s4_cols=[
    "radius","repetitions","median_n_balls","q10_n_balls","q90_n_balls",
    "median_qualifying_pairs","q10_qualifying_pairs","q90_qualifying_pairs",
    "min_qualifying_pairs","max_qualifying_pairs","median_pair_prevalence",
    "q10_pair_prevalence","q90_pair_prevalence","min_pair_prevalence",
    "max_pair_prevalence","share_constructions_with_any_qualifying_pair",
    "share_constructions_at_or_above_canonical_prevalence"
]
for c in s4_cols:
    if any(c not in r for r in summary):
        raise SystemExit("FAIL_CLOSED: S4 source missing column "+c)
write_csv(public/"supplementary_table_S4_core_contrast_cover_robustness.csv",
          s4_cols, [{c:r[c] for c in s4_cols} for r in summary])

def q_type7(values,p):
    x=sorted(values); n=len(x)
    if n==0: return float("nan")
    if n==1: return x[0]
    h=(n-1)*p
    lo=int(math.floor(h)); hi=int(math.ceil(h))
    if lo==hi: return x[lo]
    return x[lo] + (h-lo)*(x[hi]-x[lo])

groups={}
with gzip.open(core_raw/"threshold_grid_by_replicate.csv.gz","rt",newline="") as fh:
    for r in csv.DictReader(fh):
        key=(round(f(r["scalar_gap_threshold"]),10),round(f(r["profile_distance_threshold"]),10))
        groups.setdefault(key,[]).append((i(r["n_qualifying_pairs"]),f(r["pair_prevalence"])))
if len(groups)!=30:
    raise SystemExit(f"FAIL_CLOSED: expected 30 threshold cells, found {len(groups)}")
s5_cols=[
    "scalar_gap_threshold","profile_distance_threshold","constructions",
    "median_qualifying_pairs","q10_qualifying_pairs","q90_qualifying_pairs",
    "min_qualifying_pairs","max_qualifying_pairs","median_pair_prevalence",
    "q10_pair_prevalence","q90_pair_prevalence","min_pair_prevalence",
    "max_pair_prevalence","share_constructions_with_any_qualifying_pair"
]
s5=[]
for (sg,pd), vals in sorted(groups.items(), key=lambda kv:(kv[0][0],kv[0][1])):
    counts=[v[0] for v in vals]; prev=[v[1] for v in vals]
    s5.append({
        "scalar_gap_threshold":sg,
        "profile_distance_threshold":pd,
        "constructions":len(vals),
        "median_qualifying_pairs":q_type7(counts,.5),
        "q10_qualifying_pairs":q_type7(counts,.1),
        "q90_qualifying_pairs":q_type7(counts,.9),
        "min_qualifying_pairs":min(counts),
        "max_qualifying_pairs":max(counts),
        "median_pair_prevalence":q_type7(prev,.5),
        "q10_pair_prevalence":q_type7(prev,.1),
        "q90_pair_prevalence":q_type7(prev,.9),
        "min_pair_prevalence":min(prev),
        "max_pair_prevalence":max(prev),
        "share_constructions_with_any_qualifying_pair":sum(c>0 for c in counts)/len(counts),
    })
write_csv(public/"supplementary_table_S5_core_contrast_threshold_cover_robustness.csv",
          s5_cols, s5)

# ---- Numeric validation against frozen v0.25 outputs for canonical full run ----
checks=[]
def check(name, ok, detail=""):
    checks.append((name,"PASS" if ok else "FAIL",detail))
    if not ok:
        raise SystemExit(f"FAIL_CLOSED: {name}: {detail}")

# Critical KNN benchmark gate is always enforced.
obs25=next(r for r in all_bench if i(r["k"])==25)
check("knn_k25_zero_overlap_share",
      abs(f(obs25["zero_overlap_share"])-0.893941638275811)<1e-12,
      obs25["zero_overlap_share"])
check("knn_k25_independent_zero_overlap_share",
      abs(f(obs25["independent_set_zero_overlap_share"])-0.9816408454710995)<1e-12,
      obs25["independent_set_zero_overlap_share"])

if reps == 1000:
    if not expected.is_dir():
        raise SystemExit("FAIL_CLOSED: frozen v0.25 expected supplementary directory is missing")

    # Compare CSVs numerically/string-wise by column and row, allowing formatting differences.
    compare_names=[
        "supplementary_data_knn_overlap_k1_100.csv",
        "supplementary_data_knn_overlap_display_k5_100.csv",
        "supplementary_data_knn_overlap_with_independent_set_benchmark.csv",
        "supplementary_data_knn_overlap_display_with_benchmark_k5_100.csv",
        "supplementary_data_core_contrast_by_replicate.csv",
        "supplementary_table_S4_core_contrast_cover_robustness.csv",
        "supplementary_table_S5_core_contrast_threshold_cover_robustness.csv",
    ]
    def csv_equiv(a,b,tol=1e-11):
        ra=read_csv(a); rb=read_csv(b)
        if len(ra)!=len(rb): return False,f"row_count {len(ra)} != {len(rb)}"
        if not ra and not rb: return True,""
        ca=list(ra[0].keys()); cb=list(rb[0].keys())
        if ca!=cb: return False,f"columns {ca} != {cb}"
        for idx,(x,y) in enumerate(zip(ra,rb),start=2):
            for c in ca:
                sx=str(x[c]).strip(); sy=str(y[c]).strip()
                try:
                    fx=float(sx); fy=float(sy)
                    if math.isfinite(fx) and math.isfinite(fy):
                        if abs(fx-fy) > tol*max(1.0,abs(fx),abs(fy)):
                            return False,f"row {idx} col {c}: {fx} != {fy}"
                    elif sx!=sy: return False,f"row {idx} col {c}: {sx} != {sy}"
                except Exception:
                    if sx!=sy: return False,f"row {idx} col {c}: {sx} != {sy}"
        return True,""
    for nm in compare_names:
        ok,detail=csv_equiv(public/nm,expected/nm)
        check("v025_expected_"+nm,ok,detail)

    # Compare decompressed threshold-grid rows without relying on gzip metadata.
    def gz_csv_equiv(a,b,tol=1e-11):
        with gzip.open(a,"rt",newline="") as fa, gzip.open(b,"rt",newline="") as fb:
            ra=csv.DictReader(fa); rb=csv.DictReader(fb)
            if ra.fieldnames!=rb.fieldnames: return False,"fieldnames differ"
            count=0
            for count,(x,y) in enumerate(zip(ra,rb),start=1):
                for c in ra.fieldnames:
                    try:
                        fx=float(x[c]); fy=float(y[c])
                        if abs(fx-fy)>tol*max(1.0,abs(fx),abs(fy)):
                            return False,f"row {count} col {c}: {fx} != {fy}"
                    except Exception:
                        if x[c]!=y[c]: return False,f"row {count} col {c}: {x[c]} != {y[c]}"
            # Ensure neither has extra rows.
            extra_a=next(ra,None); extra_b=next(rb,None)
            if extra_a is not None or extra_b is not None: return False,"row counts differ"
        return True,""
    ok,detail=gz_csv_equiv(
        public/"supplementary_data_core_contrast_threshold_by_replicate.csv.gz",
        expected/"supplementary_data_core_contrast_threshold_by_replicate.csv.gz")
    check("v025_expected_threshold_grid_gz",ok,detail)
else:
    checks.append(("v025_full_expected_comparison","SKIPPED_NONCANONICAL",f"REPETITIONS={reps}"))

with open(results/"PUBLIC_OUTPUT_VALIDATION.csv","w",newline="") as fh:
    w=csv.writer(fh); w.writerow(["check","status","detail"]); w.writerows(checks)
(results/"PUBLIC_OUTPUT_STATUS.txt").write_text(
    "PUBLIC_OUTPUT_STATUS=PASS\n"
    + ("FULL_V025_EXPECTED_COMPARISON=PASS\n" if reps==1000 else "FULL_V025_EXPECTED_COMPARISON=SKIPPED_NONCANONICAL\n")
)
print("PUBLIC_OUTPUT_STATUS=PASS")

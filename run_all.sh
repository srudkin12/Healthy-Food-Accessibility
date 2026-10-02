#!/usr/bin/env bash
set -Eeuo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export FOOD_PROJECT_ROOT="$ROOT"

detect_physical_cores() {
  local x=""
  if command -v Rscript >/dev/null 2>&1; then
    x="$(Rscript --vanilla -e 'z<-suppressWarnings(parallel::detectCores(logical=FALSE)); if(!is.finite(z)||z<1) z<-1; cat(as.integer(z))' 2>/dev/null || true)"
  fi
  if [[ ! "$x" =~ ^[0-9]+$ ]] || [[ "$x" -lt 1 ]]; then x=1; fi
  echo "$x"
}

export N_WORKERS="${N_WORKERS:-$(detect_physical_cores)}"
export N_REPS="${N_REPS:-1000}"

if [[ ! "$N_WORKERS" =~ ^[0-9]+$ ]] || [[ "$N_WORKERS" -lt 1 ]]; then
  echo "ERROR: N_WORKERS must be a positive integer." >&2; exit 2
fi
if [[ ! "$N_REPS" =~ ^[0-9]+$ ]] || [[ "$N_REPS" -lt 10 ]]; then
  echo "ERROR: N_REPS must be an integer >= 10." >&2; exit 2
fi

if [[ "$N_REPS" -eq 1000 ]]; then
  export HFA_REPLICATION_MODE="CANONICAL"
else
  export HFA_REPLICATION_MODE="QUICK_NONCANONICAL"
fi

if [[ -z "${ROBUSTNESS_WORKERS:-}" ]]; then
  if [[ "$N_WORKERS" -gt 80 ]]; then export ROBUSTNESS_WORKERS=80; else export ROBUSTNESS_WORKERS="$N_WORKERS"; fi
fi
if [[ ! "$ROBUSTNESS_WORKERS" =~ ^[0-9]+$ ]] || [[ "$ROBUSTNESS_WORKERS" -lt 1 ]]; then
  echo "ERROR: ROBUSTNESS_WORKERS must be a positive integer." >&2; exit 2
fi

export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1
FRAMEWORK="$ROOT/framework/TDABM_Portability"
mkdir -p "$ROOT/logs" "$ROOT/.status" "$ROOT/work" "$ROOT/outputs" "$ROOT/manuscript/figures"
LOG="$ROOT/logs/run_all_$(date +%Y%m%d_%H%M%S).log"
exec > >(tee -a "$LOG") 2>&1

CURRENT_STAGE="initialisation"
trap 'rc=$?; echo "HFA_REPLICATION_RUN=FAIL stage=$CURRENT_STAGE rc=$rc"; echo "RESUME: N_WORKERS=$N_WORKERS N_REPS=$N_REPS bash $ROOT/run_all.sh"; exit $rc' ERR

echo "HFA_REPLICATION_RUN_START"
echo "ROOT=$ROOT"
echo "N_WORKERS=$N_WORKERS"
echo "ROBUSTNESS_WORKERS=$ROBUSTNESS_WORKERS"
echo "N_REPS=$N_REPS"
echo "REPLICATION_MODE=$HFA_REPLICATION_MODE"
echo "START=$(date -Is)"

command -v Rscript >/dev/null 2>&1 || { echo "ERROR: Rscript not found" >&2; exit 2; }
command -v python3 >/dev/null 2>&1 || { echo "ERROR: python3 not found" >&2; exit 2; }
command -v sha256sum >/dev/null 2>&1 || { echo "ERROR: sha256sum not found" >&2; exit 2; }

PROFILE="$ROOT/.runtime_profile"
if [[ -f "$PROFILE" ]]; then
  OLD_REPS="$(awk -F= '$1=="N_REPS"{print $2}' "$PROFILE" | tail -1)"
  if [[ -n "$OLD_REPS" && "$OLD_REPS" != "$N_REPS" ]]; then
    echo "ERROR: existing stochastic outputs/checkpoints were created with N_REPS=$OLD_REPS, but N_REPS=$N_REPS was requested." >&2
    echo "Run: bash scripts/reset_repetition_outputs.sh" >&2
    exit 3
  fi
else
  { echo "N_REPS=$N_REPS"; echo "CREATED=$(date -Is)"; } > "$PROFILE"
fi

run_stage() {
  local id="$1"; shift
  local label="$1"; shift
  local marker="$ROOT/.status/${id}_${label}.done"
  CURRENT_STAGE="${id}_${label}"
  echo; echo "============================================================"; echo "STAGE $id: $label"; echo "============================================================"
  if [[ -f "$marker" ]]; then echo "STAGE_SKIP_ALREADY_PASS $id $label"; return 0; fi
  "$@"
  date -Is > "$marker"
  echo "STAGE_PASS $id $label"
}

run_stage 01 source_reconstruction bash "$ROOT/pipeline/01_source_reconstruction/run_hfa_f0f1.sh"
run_stage 02 physical_access bash "$ROOT/pipeline/02_physical_access/run_hfa_f2.sh"
run_stage 03 jts_validation bash "$ROOT/pipeline/03_jts_validation/run_hfa_f2a.sh"
run_stage 04 retail_access bash "$ROOT/pipeline/04_retail_access/run_hfa_f3.sh"
run_stage 05 mobility_material bash "$ROOT/pipeline/05_mobility_material/run_hfa_f4.sh"
run_stage 06 contextual_readouts bash "$ROOT/pipeline/06_contextual_readouts/run_hfa_f5.sh"
run_stage 06b framework_selftest bash -lc "set -Eeuo pipefail; cd '$FRAMEWORK' && bash setup_tdabm_clean.sh && rm -rf outputs"

run_stage 07 input_freeze bash -lc "set -Eeuo pipefail;
  S='$ROOT/pipeline/07_input_freeze'; rm -rf \"\$S/food_application/results\" \"\$S/food_application/logs\";
  mkdir -p \"\$S/food_application/results/p1_4_a\" \"\$S/food_application/logs\";
  Rscript --vanilla \"\$S/food_application/scripts/01_food_p1_4a_freeze.R\" \"\$S\" '$ROOT' '$FRAMEWORK'"

run_stage 08 radius_diagnostics bash -lc "set -Eeuo pipefail;
  S='$ROOT/pipeline/08_radius_diagnostics'; A='$ROOT/pipeline/07_input_freeze/food_application/results/p1_4_a';
  rm -rf \"\$S/food_application/input\"; mkdir -p \"\$S/food_application/input\" \"\$S/food_application/results\" \"\$S/food_application/logs\";
  cp \"\$A/frozen/food_hfa_analysis_input.csv\" \"\$S/food_application/input/\"; cp \"\$A/P1_4_A_STATUS.txt\" \"\$S/food_application/input/\";
  [[ -f \"\$A/analysis_input_manifest.csv\" ]] && cp \"\$A/analysis_input_manifest.csv\" \"\$S/food_application/input/\" || true;
  [[ -f \"\$A/axis_definition_register.csv\" ]] && cp \"\$A/axis_definition_register.csv\" \"\$S/food_application/input/\" || true;
  sha256sum \"\$S/food_application/input/food_hfa_analysis_input.csv\" > \"\$S/FROZEN_INPUT_SHA256.txt\";
  (cd '$FRAMEWORK' && rm -rf outputs && bash setup_tdabm_clean.sh --static-only);
  Rscript --vanilla \"\$S/food_application/scripts/00_food_p1_4b_preflight.R\" \"\$S\" '$FRAMEWORK';
  N_WORKERS='$N_WORKERS' N_REPS='$N_REPS' Rscript --vanilla \"\$S/food_application/scripts/01_food_p1_4b_dense_runner.R\" \"\$S\" '$FRAMEWORK';
  N_REPS='$N_REPS' Rscript --vanilla \"\$S/food_application/scripts/02_food_p1_4b_aggregate.R\" \"\$S\" '$FRAMEWORK';
  N_REPS='$N_REPS' Rscript --vanilla \"\$S/food_application/scripts/03_food_p1_4b_audit.R\" \"\$S\""

run_stage 09 canonical_topology bash -lc "set -Eeuo pipefail;
  S='$ROOT/pipeline/09_canonical_topology'; A='$ROOT/pipeline/07_input_freeze/food_application/results/p1_4_a'; B='$ROOT/pipeline/08_radius_diagnostics/food_application/results/p1_4_b';
  rm -rf \"\$S/food_application/input\" \"\$S/food_application/results\"; mkdir -p \"\$S/food_application/input\" \"\$S/food_application/results/p1_4_c\" \"\$S/food_application/logs\";
  cp \"\$A/frozen/food_hfa_analysis_input.csv\" \"\$S/food_application/input/\"; cp \"\$A/P1_4_A_STATUS.txt\" \"\$S/food_application/input/\";
  cp \"\$B/P1_4_B_STATUS.txt\" \"\$S/food_application/input/\"; cp \"\$B/P1_4_B_validation_report.csv\" \"\$S/food_application/input/\";
  sha256sum \"\$S/food_application/input/food_hfa_analysis_input.csv\" > \"\$S/FROZEN_INPUT_SHA256.txt\";
  sha256sum \"\$S/food_application/input/P1_4_B_validation_report.csv\" > \"\$S/P1_4_B_DIAGNOSTIC_AUDIT_SHA256.txt\";
  AUD=\$(awk '{print \$1}' \"\$S/P1_4_B_DIAGNOSTIC_AUDIT_SHA256.txt\");
  python3 - \"\$S/food_application/config/approved_radius_decision_input.csv\" \"\$AUD\" <<'PY'
import csv,sys
p,sha=sys.argv[1:3]; rows=list(csv.DictReader(open(p,encoding='utf-8')))
for r in rows:
    if r.get('field')=='diagnostic_audit_sha256': r['value']=sha
with open(p,'w',newline='',encoding='utf-8') as f:
    w=csv.DictWriter(f,fieldnames=rows[0].keys()); w.writeheader(); w.writerows(rows)
PY
  (cd '$FRAMEWORK' && rm -rf outputs && bash setup_tdabm_clean.sh --static-only);
  Rscript --vanilla '$FRAMEWORK/tools/test_p1_4_c_topology_contracts.R' '$FRAMEWORK';
  Rscript --vanilla \"\$S/food_application/scripts/01_food_p1_4c_freeze.R\" \"\$S\" '$FRAMEWORK'"

run_stage 10 evidence_robustness bash -lc "set -Eeuo pipefail;
  S='$ROOT/pipeline/10_evidence_robustness'; C='$ROOT/pipeline/09_canonical_topology'; rm -rf \"\$S/accepted_p1_4_c\";
  mkdir -p \"\$S/accepted_p1_4_c/p1_4_c\" \"\$S/food_application/results/p1_4_d\" \"\$S/food_application/.Rlib\" \"\$S/food_application/logs\";
  cp -a \"\$C/food_application/results/p1_4_c/.\" \"\$S/accepted_p1_4_c/p1_4_c/\";
  export R_LIBS_USER=\"\$S/food_application/.Rlib\${R_LIBS_USER:+:\$R_LIBS_USER}\"; export N_WORKERS='$N_WORKERS'; export ROBUSTNESS_WORKERS='$ROBUSTNESS_WORKERS'; export N_REPS='$N_REPS';
  Rscript --vanilla \"\$S/food_application/scripts/install_dependencies.R\"; (cd '$FRAMEWORK' && rm -rf outputs && bash setup_tdabm_clean.sh --static-only);
  Rscript --vanilla '$FRAMEWORK/tools/test_p1_4_d_paper_evidence.R' '$FRAMEWORK'; Rscript --vanilla '$FRAMEWORK/tools/test_p1_4_d_interpretation_robustness.R' '$FRAMEWORK';
  Rscript --vanilla \"\$S/food_application/scripts/00_preflight_and_recover_readouts.R\" \"\$S\" '$ROOT' '$FRAMEWORK';
  Rscript --vanilla \"\$S/food_application/scripts/01_canonical_paper_evidence.R\" \"\$S\" '$FRAMEWORK';
  Rscript --vanilla \"\$S/food_application/scripts/02_interpretation_robustness.R\" \"\$S\" '$FRAMEWORK';
  Rscript --vanilla \"\$S/food_application/scripts/03_audit_and_provenance.R\" \"\$S\" '$FRAMEWORK'"

run_stage 11 interpretation bash -lc "set -Eeuo pipefail;
  S='$ROOT/pipeline/11_interpretation'; D='$ROOT/pipeline/10_evidence_robustness/food_application/results/p1_4_d'; C='$ROOT/pipeline/09_canonical_topology/food_application/results/p1_4_c';
  rm -rf \"\$S/accepted_p1_4_d\" \"\$S/results\" \"\$S/figures\" \"\$S/logs\"; mkdir -p \"\$S/accepted_p1_4_d/p1_4_d\" \"\$S/accepted_p1_4_d/canonical_identity\" \"\$S/results\" \"\$S/figures\" \"\$S/logs\";
  cp -a \"\$D/.\" \"\$S/accepted_p1_4_d/p1_4_d/\"; cp \"\$C/canonical_topology_fingerprint.txt\" \"\$S/accepted_p1_4_d/canonical_identity/\"; cp \"\$C/P1_4_C_STATUS.txt\" \"\$S/accepted_p1_4_d/canonical_identity/\";
  Rscript --vanilla \"\$S/scripts/00_preflight.R\" \"\$S\"; Rscript --vanilla \"\$S/scripts/01_build_ball_interpretation.R\" \"\$S\";
  Rscript --vanilla \"\$S/scripts/02_build_contrast_evidence.R\" \"\$S\"; Rscript --vanilla \"\$S/scripts/03_build_robustness_and_claim_register.R\" \"\$S\"; Rscript --vanilla \"\$S/scripts/04_audit_and_close.R\" \"\$S\""

run_stage 12 spatial_closure bash -lc "set -Eeuo pipefail;
  S='$ROOT/pipeline/12_spatial_closure'; D='$ROOT/pipeline/10_evidence_robustness/food_application/results/p1_4_d'; F='$ROOT/pipeline/11_interpretation/results';
  rm -rf \"\$S/accepted_evidence\" \"\$S/results\" \"\$S/work\" \"\$S/logs\"; mkdir -p \"\$S/accepted_evidence/p1_4_d\" \"\$S/accepted_evidence/food24\" \"\$S/results\" \"\$S/work\" \"\$S/.Rlib\" \"\$S/logs\";
  cp -a \"\$D/.\" \"\$S/accepted_evidence/p1_4_d/\"; cp -a \"\$F/.\" \"\$S/accepted_evidence/food24/\"; export R_LIBS_USER=\"\$S/.Rlib\${R_LIBS_USER:+:\$R_LIBS_USER}\";
  Rscript --vanilla \"\$S/scripts/install_dependencies.R\"; Rscript --vanilla \"\$S/scripts/00_preflight_geometry.R\" \"\$S\" '$ROOT';
  Rscript --vanilla \"\$S/scripts/01_adjacency_and_spatial_dependence.R\" \"\$S\"; Rscript --vanilla \"\$S/scripts/02_topology_vs_geography_knn.R\" \"\$S\";
  Rscript --vanilla \"\$S/scripts/03_ball_spatial_closure.R\" \"\$S\"; Rscript --vanilla \"\$S/scripts/04_pair_sample_and_contrasts.R\" \"\$S\"; Rscript --vanilla \"\$S/scripts/05_audit_and_review_gate.R\" \"\$S\""

run_stage 13 scalarisation_robustness bash -lc "set -Eeuo pipefail;
  S='$ROOT/pipeline/13_scalarisation_robustness'; E='$ROOT/work/replicated_evidence'; rm -rf \"\$E\" \"\$S/results\" \"\$S/logs\";
  mkdir -p \"\$E/frozen_input\" \"\$E/topology\" \"\$S/results\" \"\$S/logs\";
  cp '$ROOT/pipeline/07_input_freeze/food_application/results/p1_4_a/frozen/food_hfa_analysis_input.csv' \"\$E/frozen_input/\";
  cp '$ROOT/pipeline/09_canonical_topology/food_application/results/p1_4_c/canonical_memberships.csv' \"\$E/topology/\";
  cp '$ROOT/pipeline/09_canonical_topology/food_application/results/p1_4_c/canonical_topology_fingerprint.txt' \"\$E/topology/\";
  Rscript --vanilla \"\$S/R/01_scalarisation_robustness.R\" \"\$E\" \"\$S/config/expected_values.csv\" \"\$S/results\""

run_stage 14 manuscript_outputs bash -lc "set -Eeuo pipefail;
  Rscript --vanilla '$ROOT/scripts/14_make_manuscript_outputs.R' '$ROOT'; mkdir -p '$ROOT/manuscript/figures'; cp -a '$ROOT/outputs/manuscript/figures/.' '$ROOT/manuscript/figures/';
  cp '$ROOT/outputs/manuscript/supplementary/'*.csv '$ROOT/manuscript/'; Rscript --vanilla '$ROOT/scripts/14b_make_supplement_tex.R' '$ROOT';
  if command -v pdflatex >/dev/null 2>&1; then cd '$ROOT/manuscript'; pdflatex -interaction=nonstopmode -halt-on-error manuscript.tex; pdflatex -interaction=nonstopmode -halt-on-error manuscript.tex; pdflatex -interaction=nonstopmode -halt-on-error supplementary_material.tex; pdflatex -interaction=nonstopmode -halt-on-error supplementary_material.tex; else echo 'PDFLATEX_NOT_FOUND: numerical/table/figure replication completed; PDF compilation skipped'; fi"

run_stage 15 verification env N_REPS="$N_REPS" python3 "$ROOT/scripts/15_verify_replication.py" "$ROOT"

echo; echo "END=$(date -Is)"
if [[ "$HFA_REPLICATION_MODE" == "CANONICAL" ]]; then echo "HFA_CANONICAL_REPLICATION=PASS"; else echo "HFA_QUICK_REPLICATION=PASS"; echo "NONCANONICAL_NOTICE=N_REPS=$N_REPS; exact paper robustness uses N_REPS=1000"; fi

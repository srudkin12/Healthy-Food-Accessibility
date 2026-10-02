#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")" && pwd)"
export HFA_ROOT="$ROOT"
mkdir -p "$ROOT/logs"
LOG="$ROOT/logs/HFA_F0F1_$(date +%Y%m%d_%H%M%S).log"

exec > >(tee -a "$LOG") 2>&1

echo "============================================================"
echo "Food Deserts / Healthy Food Access — HFA-F0/F1"
echo "ROOT=$ROOT"
echo "START=$(date)"
echo "============================================================"

if ! command -v Rscript >/dev/null 2>&1; then
  echo "ERROR: Rscript is not on PATH. Install/activate R and rerun."
  exit 1
fi

echo
echo "============================================================"
echo "R syntax preflight"
echo "============================================================"
Rscript "$ROOT/scripts/00_syntax_preflight.R"

run_step () {
  local script="$1"
  echo
  echo "============================================================"
  echo "$script"
  echo "============================================================"
  Rscript "$ROOT/scripts/$script"
}

run_step 00_bootstrap.R
run_step 01_build_lsoa21_spine.R
run_step 02_census_capability_baseline.R
run_step 03_stage_dft_sources.R
run_step 04_audit_release.R

echo
echo "============================================================"
echo "HFA_F0F1_COMPLETE"
echo "END=$(date)"
echo "Audit: $ROOT/results/f0f1/AUDIT_F0F1.txt"
echo "Baseline: $ROOT/data_staged/hfa_f0f1_baseline.csv"
echo "============================================================"

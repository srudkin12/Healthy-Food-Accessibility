#!/bin/bash
set -euo pipefail
ROOT="$(cd "$(dirname "$0")" && pwd)"
cd "$ROOT"
mkdir -p logs results/f2 data_raw/dft data_raw/ons data_staged
LOG="logs/HFA_F2_$(date +%Y%m%d_%H%M%S).log"
exec > >(tee -a "$LOG") 2>&1
banner(){ echo "============================================================"; echo "$1"; echo "============================================================"; }
banner "Food Deserts / Healthy Food Access — HFA-F2 Physical Access v1.0.3"
echo "ROOT=$ROOT"; echo "START=$(date)"
banner "R syntax preflight"; Rscript scripts/00_syntax_preflight.R
for s in 00_bootstrap.R 01_inherit_f0f1.R 02_stage_sources.R 03_extract_jts2019.R 04_harmonise_jts_to_lsoa21.R 05_extract_tcm2025.R 06_build_f2_dataset.R 07_audit_release.R; do
  banner "$s"; Rscript "scripts/$s"
done
banner "HFA_F2_COMPLETE"; echo "END=$(date)"; echo "Audit: $ROOT/results/f2/AUDIT_F2.txt"; echo "Dataset: $ROOT/data_staged/hfa_f2_physical_access.csv"

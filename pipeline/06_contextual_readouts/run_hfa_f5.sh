#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")" && pwd)"
cd "$ROOT"

export R_LIBS_USER="$ROOT/.Rlib"
mkdir -p "$R_LIBS_USER"
mkdir -p logs results/f5 data_staged data_raw/iuc data_raw/ons

LOG="logs/HFA_F5_$(date +%Y%m%d_%H%M%S).log"
exec > >(tee "$LOG") 2>&1

echo "============================================================"
echo "Food Deserts / Healthy Food Access — HFA-F5 Digital + RUC Closure"
echo "ROOT=$ROOT"
echo "R_LIBS_USER=$R_LIBS_USER"
echo "START=$(date)"
echo "============================================================"

echo "============================================================"
echo "R syntax preflight"
echo "============================================================"
Rscript scripts/00_syntax_preflight.R

for f in \
  scripts/00_preflight.R \
  scripts/01_inherit_f4.R \
  scripts/02_stage_iuc_and_lookup.R \
  scripts/03_crosswalk_iuc.R \
  scripts/04_ruc_conditioned_peer_overlap.R \
  scripts/05_ruc_conditioned_dispersion.R \
  scripts/06_digital_readout_closure.R \
  scripts/07_final_geometry_contract.R \
  scripts/08_release_audit.R
do
  echo "============================================================"
  echo "$(basename "$f")"
  echo "============================================================"
  Rscript "$f"
done

echo "============================================================"
echo "HFA_F5_COMPLETE"
echo "END=$(date)"
echo "============================================================"

# replication worktree: historical handback call removed

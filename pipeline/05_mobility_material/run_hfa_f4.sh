#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")" && pwd)"
cd "$ROOT"

export R_LIBS_USER="$ROOT/.Rlib"
mkdir -p "$R_LIBS_USER"
mkdir -p logs results/f4 data_staged data_raw/imd2025 data_raw/ruc2021

LOG="logs/HFA_F4_$(date +%Y%m%d_%H%M%S).log"
exec > >(tee "$LOG") 2>&1

echo "============================================================"
echo "Food Deserts / Healthy Food Access — HFA-F4 Mobility & Material Kill Tests"
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
  scripts/01_inherit_f3.R \
  scripts/02_stage_imd2025.R \
  scripts/03_stage_ruc2021.R \
  scripts/04_build_f4_capability_dataset.R \
  scripts/05_same_distance_dispersion.R \
  scripts/06_structural_peer_overlap.R \
  scripts/07_kmeans_benchmark.R \
  scripts/08_configuration_kill_tests.R \
  scripts/09_release_audit.R
do
  echo "============================================================"
  echo "$(basename "$f")"
  echo "============================================================"
  Rscript "$f"
done

echo "============================================================"
echo "HFA_F4_COMPLETE"
echo "END=$(date)"
echo "============================================================"

# Successful analytical completion automatically creates a handback.
# replication worktree: historical handback call removed

#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")" && pwd)"
cd "$ROOT"

# Project-local R package library. Missing CRAN packages are installed here
# by scripts/00_preflight.R and all subsequent Rscript calls inherit it.
export R_LIBS_USER="$ROOT/.Rlib"
mkdir -p "$R_LIBS_USER"

mkdir -p logs results/f3 data_staged data_raw/geolytix/chunks data_raw/ons/pwc_chunks data_raw/ons/boundary_chunks

LOG="logs/HFA_F3_$(date +%Y%m%d_%H%M%S).log"
exec > >(tee "$LOG") 2>&1

echo "============================================================"
echo "Food Deserts / Healthy Food Access — HFA-F3 Retail Functional Access"
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
  scripts/01_inherit_f2a.R \
  scripts/02_stage_sources.R \
  scripts/03_classify_geolytix.R \
  scripts/04_assign_stores_to_lsoa.R \
  scripts/05_compute_functional_access.R \
  scripts/06_student_sample_reconstruction.R \
  scripts/07_build_f3_dataset.R \
  scripts/08_release_audit.R
do
  echo "============================================================"
  echo "$(basename "$f")"
  echo "============================================================"
  Rscript "$f"
done

echo "============================================================"
echo "HFA_F3_COMPLETE"
echo "END=$(date)"
echo "============================================================"

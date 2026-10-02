#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "$0")" && pwd)"
cd "$ROOT"
mkdir -p logs results/f2a data_staged
LOG="logs/HFA_F2A_$(date +%Y%m%d_%H%M%S).log"
exec > >(tee "$LOG") 2>&1
echo "============================================================"
echo "Food Deserts / Healthy Food Access — F2A JTS2019 Validation"
echo "ROOT=$ROOT"
echo "START=$(date)"
echo "============================================================"
for f in scripts/*.R; do Rscript -e "parse(file='$f'); cat('PARSE_OK: $f\\n')"; done
echo "R_SYNTAX_PREFLIGHT_OK"
for f in scripts/00_preflight.R scripts/01_audit_jts2019.R scripts/02_build_validated_f2.R scripts/03_release_audit.R; do
 echo "============================================================"
 echo "$(basename "$f")"
 echo "============================================================"
 Rscript "$f"
done
echo "============================================================"
echo "HFA_F2A_COMPLETE"
echo "END=$(date)"
echo "============================================================"

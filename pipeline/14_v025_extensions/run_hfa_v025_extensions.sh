#!/usr/bin/env bash
set -euo pipefail

STAGE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$STAGE_DIR/../.." && pwd)"
RESULTS="$STAGE_DIR/results"
KNN_OUT="$RESULTS/knn_raw"
CORE_OUT="$RESULTS/core_raw"
PUBLIC_OUT="$RESULTS/public"
CONFIG="$STAGE_DIR/config"

REPETITIONS="${N_REPS:-1000}"
if [[ ! "$REPETITIONS" =~ ^[0-9]+$ ]] || [[ "$REPETITIONS" -lt 1 ]]; then
  echo "FAIL_CLOSED: N_REPS must be a positive integer" >&2
  exit 2
fi

if [[ -n "${N_WORKERS:-}" ]]; then
  WORKERS="$N_WORKERS"
else
  WORKERS="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1)"
fi
if [[ ! "$WORKERS" =~ ^[0-9]+$ ]] || [[ "$WORKERS" -lt 1 ]]; then
  echo "FAIL_CLOSED: N_WORKERS must be a positive integer" >&2
  exit 2
fi

command -v Rscript >/dev/null || { echo "FAIL_CLOSED: Rscript not found" >&2; exit 2; }
command -v python3 >/dev/null || { echo "FAIL_CLOSED: python3 not found" >&2; exit 2; }

rm -rf "$RESULTS"
mkdir -p "$KNN_OUT" "$CORE_OUT" "$PUBLIC_OUT"

echo "=== v0.25 extension: exact KNN k=1..100 ==="
Rscript "$STAGE_DIR/scripts/01_extended_knn.R" \
  "$REPO_ROOT" "$KNN_OUT" "$CONFIG/expected_knn_benchmarks.csv" 100

echo "=== v0.25 extension: locate checksum-gated canonical readout frame ==="
INPUT_SHA="b7172b17c788f8be3ec844029dd62cf4c1339d698fc33ee6df52115646eb8764"
mapfile -t INPUT_CANDIDATES < <(find "$REPO_ROOT/pipeline" -type f -name 'food_hfa_readout_frame.csv' -print 2>/dev/null | sort)
INPUT=""
for p in "${INPUT_CANDIDATES[@]}"; do
  a="$(sha256sum "$p" | awk '{print $1}')"
  if [[ "$a" == "$INPUT_SHA" ]]; then
    INPUT="$p"
    break
  fi
done
if [[ -z "$INPUT" ]]; then
  echo "FAIL_CLOSED: no generated food_hfa_readout_frame.csv matches authoritative SHA-256 $INPUT_SHA" >&2
  exit 2
fi
printf 'INPUT_PATH=%s\nINPUT_SHA256=%s\n' "$INPUT" "$INPUT_SHA" > "$RESULTS/INPUT_SOURCE.txt"

echo "=== v0.25 extension: direct core-contrast robustness ==="
Rscript "$STAGE_DIR/scripts/02_core_contrast_robustness.R" \
  "$INPUT" "$CORE_OUT" "$WORKERS" "$REPETITIONS"

echo "=== v0.25 extension: core-contrast audit ==="
RUN_OUTPUT_DIR="$CORE_OUT" PACKAGE_DIR="$STAGE_DIR" REPETITIONS="$REPETITIONS" \
  python3 "$STAGE_DIR/scripts/03_audit_core_contrast.py"

echo "=== v0.25 extension: manuscript-facing public outputs and benchmark ==="
python3 "$STAGE_DIR/scripts/04_build_public_outputs.py" \
  "$REPO_ROOT" "$RESULTS" "$REPETITIONS"

printf '%s\n' \
  "HFA_V025_EXTENSIONS=PASS" \
  "REPETITIONS=$REPETITIONS" \
  "WORKERS=$WORKERS" \
  "KNN_MAX_K=100" \
  "CORE_CONTRAST_CONSTRUCTIONS=$((5 * REPETITIONS))" \
  > "$RESULTS/STATUS.txt"

echo "HFA_V025_EXTENSIONS=PASS"

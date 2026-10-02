#!/usr/bin/env bash
set -Eeuo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export N_REPS="${N_REPS:-100}"
exec bash "$ROOT/run_all.sh"

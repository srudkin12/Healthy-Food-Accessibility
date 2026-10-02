#!/usr/bin/env bash
set -Eeuo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
echo "Resetting repetition-dependent outputs only; Stages 01-07 are preserved."
rm -rf "$ROOT/pipeline/08_radius_diagnostics/food_application/results" "$ROOT/pipeline/08_radius_diagnostics/food_application/checkpoints"
rm -rf "$ROOT/pipeline/09_canonical_topology/food_application/results"
rm -rf "$ROOT/pipeline/10_evidence_robustness/food_application/results" "$ROOT/pipeline/10_evidence_robustness/food_application/checkpoints" "$ROOT/pipeline/10_evidence_robustness/accepted_p1_4_c"
rm -rf "$ROOT/pipeline/11_interpretation/results" "$ROOT/pipeline/11_interpretation/accepted_p1_4_d"
rm -rf "$ROOT/pipeline/12_spatial_closure/results" "$ROOT/pipeline/12_spatial_closure/accepted_evidence" "$ROOT/pipeline/12_spatial_closure/work"
rm -rf "$ROOT/pipeline/13_scalarisation_robustness/results" "$ROOT/outputs/manuscript" "$ROOT/outputs/verification" "$ROOT/manuscript/figures"
rm -f "$ROOT/manuscript/"supplementary_*.csv
rm -f "$ROOT/.status/08_radius_diagnostics.done" "$ROOT/.status/09_canonical_topology.done" "$ROOT/.status/10_evidence_robustness.done" "$ROOT/.status/11_interpretation.done" "$ROOT/.status/12_spatial_closure.done" "$ROOT/.status/13_scalarisation_robustness.done" "$ROOT/.status/14_manuscript_outputs.done" "$ROOT/.status/15_verification.done" "$ROOT/.runtime_profile"
echo "HFA_REPETITION_OUTPUT_RESET=PASS"

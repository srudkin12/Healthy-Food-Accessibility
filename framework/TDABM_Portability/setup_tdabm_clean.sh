#!/usr/bin/env bash
set -Eeuo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"; TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT; MODE="full"
case "${1:-}" in "") ;; --static-only) MODE="static-only" ;; --help|-h) echo "Usage: bash setup_tdabm_clean.sh [--static-only]"; exit 0 ;; *) echo "ERROR: unknown option: $1" >&2; exit 64 ;; esac
pass_marker(){ echo "TDABM_TEST|$1|PASS"; }; run_check(){ local name="$1"; shift; echo "---- $name ----"; "$@"; pass_marker "$name"; }
echo "============================================================"; echo "TDABM Portable Framework — clean baseline self-test"; echo "Root: $ROOT"; echo "Mode: $MODE"; echo "============================================================"
for exe in python3 sha256sum; do command -v "$exe" >/dev/null 2>&1 || { echo "TDABM CLEAN BASELINE SELF-TEST: FAIL"; echo "ERROR: missing $exe"; exit 1; }; done
run_check static_clean_framework python3 "$ROOT/tools/check_clean_framework.py" "$ROOT"
run_check static_current_version_consistency python3 "$ROOT/tools/check_current_baseline_versions.py" "$ROOT"
run_check static_radius_policy python3 "$ROOT/tools/check_radius_policy.py" "$ROOT"
run_check static_packed_signature python3 "$ROOT/tools/check_p1_4_b_signature_storage.py" "$ROOT"
run_check static_landmark_index python3 "$ROOT/tools/check_p1_4_landmark_index_contract.py" "$ROOT"
run_check static_namespace python3 "$ROOT/tools/check_namespace_clean.py" "$ROOT"
run_check static_data_table_mutation python3 "$ROOT/tools/check_r_data_table_mutation_contracts.py" "$ROOT" --output "$TMP/data_table.csv"
run_check static_dynamic_regex python3 "$ROOT/tools/check_r_regex_pattern_contracts.py" "$ROOT" --output "$TMP/regex.csv"
run_check static_r_structure python3 "$ROOT/tools/check_r_structure.py" "$ROOT"
run_check clean_manifest python3 "$ROOT/tools/check_clean_manifest.py" "$ROOT"
if [[ "$MODE" == "static-only" ]]; then echo; echo "TDABM CLEAN BASELINE SELF-TEST: STATIC-ONLY PASS / FULL VALIDATION NOT PERFORMED"; exit 0; fi
if [[ "${TDABM_TEST_FORCE_R_UNAVAILABLE:-0}" == "1" ]] || ! command -v Rscript >/dev/null 2>&1; then echo; echo "TDABM CLEAN BASELINE SELF-TEST: FAIL / FULL VALIDATION NOT PERFORMED"; echo "ERROR: Rscript is required for ordinary clean-baseline validation."; echo "For a partial check use: bash setup_tdabm_clean.sh --static-only"; exit 2; fi
Rscript --vanilla - "$ROOT" <<'RS'
args<-commandArgs(trailingOnly=TRUE); root<-args[[1L]]; dep<-utils::read.csv(file.path(root,'framework','dependencies.csv'),stringsAsFactors=FALSE); pkgs<-unique(dep$package); missing<-pkgs[!vapply(pkgs,requireNamespace,logical(1),quietly=TRUE)]; if(length(missing)){cat('ERROR: missing R packages:',paste(missing,collapse=', '),'\n'); quit(status=1)}; cat('PASS: declared R dependencies are available.\n')
RS
pass_marker r_dependencies
run_check r_framework_source_loading Rscript --vanilla "$ROOT/tools/test_framework_source_loading.R" "$ROOT"
run_check r_exclusion_outlier_contract Rscript --vanilla "$ROOT/tools/test_exclusion_contract_and_outlier_suggestions.R" "$ROOT"
run_check r_semantic_equivalence Rscript --vanilla "$ROOT/tools/test_semantic_equivalence_comparator.R"
run_check r_p1_4_a_input_contracts Rscript --vanilla "$ROOT/tools/test_p1_4_a_input_contracts.R"
run_check r_p1_4_b_radius_diagnostics Rscript --vanilla "$ROOT/tools/test_p1_4_b_radius_diagnostics.R"
run_check r_radius_policy_nonbinding_contract Rscript --vanilla "$ROOT/tools/test_radius_policy_nonbinding_contract.R" "$ROOT"
run_check r_p1_4_c_topology_contracts Rscript --vanilla "$ROOT/tools/test_p1_4_c_topology_contracts.R" "$ROOT"
run_check r_p1_4_d_paper_evidence Rscript --vanilla "$ROOT/tools/test_p1_4_d_paper_evidence.R" "$ROOT"
run_check r_p1_4_d_interpretation_robustness Rscript --vanilla "$ROOT/tools/test_p1_4_d_interpretation_robustness.R" "$ROOT"
echo; echo "TDABM CLEAN BASELINE SELF-TEST: COMPLETE PASS"; echo "Static checks: PASS"; echo "R dependency checks: PASS"; echo "R runtime suite: PASS"

# Beyond Severity: Ball Mapper Reveals Overlapping Configurations of Healthy Food Access in England

Replication materials for the paper by **Barry Malachy, Tegan Massey, Simon Rudkin, Frances Stratton-Killick and Martha Surman**, School of Social Sciences, University of Manchester. **Corresponding author: Simon Rudkin.**

## Authors

- Barry Malachy
- Tegan Massey
- Simon Rudkin — corresponding author
- Frances Stratton-Killick
- Martha Surman

All authors are affiliated with the **School of Social Sciences, University of Manchester**.


## What this repository reproduces

The repository reconstructs the English LSOA analytical frame from the documented source vintages, builds the three-dimensional healthy-food-access constraint space, runs the Ball Mapper radius diagnostics, freezes the researcher-approved radius **1.50** canonical topology, and reproduces the paper's robustness, spatial-closure, scalarisation-sensitivity, figures and machine-readable supplementary outputs.

Canonical empirical identity:

- 33,755 English LSOAs;
- Ball Mapper radius 1.50;
- 24 overlapping local configuration neighbourhoods;
- 92 graph edges;
- one connected component.

The radius is an explicit scientific choice. The code never selects an "optimal" radius automatically.

## Runtime controls

Two environment variables are intentionally exposed.

`N_WORKERS` controls parallel workers. If omitted, the runner asks R for the number of physical cores and falls back conservatively if that is unavailable.

`N_REPS` controls the number of deterministic random landmark-order repetitions used by the radius diagnostics and interpretation-robustness analysis.

The paper uses:

```bash
N_REPS=1000
```

A smaller value is useful for testing the workflow:

```bash
N_WORKERS=8 N_REPS=100 bash run_all.sh
```

or simply:

```bash
bash run_quick.sh
```

Reduced-repetition runs are labelled **QUICK_NONCANONICAL**. They are not expected to reproduce the paper's exact repeated-order robustness statistics.

## Canonical replication

After obtaining the source data described in `DATA_SOURCES.md`:

```bash
N_WORKERS=8 N_REPS=1000 bash run_all.sh
```

The final canonical markers are:

```text
HFA_REPLICATION_VERIFICATION=PASS
HFA_CANONICAL_REPLICATION=PASS
```

The run is restartable. Expensive repeated-order calculations checkpoint their work.

## Changing the repetition count

Do not mix checkpoints created with different `N_REPS` values. If you change the repetition count after a run has started, execute:

```bash
bash scripts/reset_repetition_outputs.sh
```

Then rerun `run_all.sh`. Source construction through Stage 07 is preserved.

## Repository structure

```text
pipeline/       accepted source-construction and analysis code
framework/      TDABM/Ball Mapper framework used by the paper
scripts/        repository-level output, validation and maintenance scripts
config/         canonical expected values and runtime example
expected/       compact outputs from the accepted 1,000-repetition clean-room run
provenance/     code-lineage and source-provenance material
manuscript/     manuscript source used during replication development
```

Generated raw data, staged data, results, checkpoints, R libraries and logs are ignored by Git and are not distributed in the repository.

## Reproducibility status

The repository was assembled only after a complete source-to-paper clean-room test reproduced the frozen analytical input, canonical topology fingerprint, manuscript contrasts, spatial closure and scalarisation robustness.

See `REPLICATION.md` for the exact canonical/quick-run distinction and `PUBLIC_RELEASE_CHECKLIST.md` for release metadata still to be filled after the GitHub repository and SSRN record exist.

## Licence

Software and computational replication materials in this repository are released under the MIT License; see `LICENSE` and `LICENSE_SCOPE.md`.

The manuscript source and article text under `manuscript/` are not covered by the MIT License. They remain under the joint copyright of Barry Malachy, Tegan Massey, Simon Rudkin, Frances Stratton-Killick and Martha Surman pending publication and publisher licensing arrangements. Third-party/provider datasets are not redistributed.

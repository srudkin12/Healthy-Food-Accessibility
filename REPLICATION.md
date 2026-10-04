# Replication instructions

## 1. Software

Required command-line tools:

- R / Rscript
- a working C++ compiler for `Rcpp::sourceCpp`
- Python 3
- `sha256sum`
- `curl` / HTTPS access for source acquisition where used by the source scripts

R package installation is performed by the accepted stage-specific dependency scripts. The repository records the package imports used by the analytical code.

LaTeX is optional. If `pdflatex` is installed, the runner compiles the manuscript and supplement; otherwise numerical, table and figure replication still completes.

## 2. Data

Follow `DATA_SOURCES.md`. Raw/provider data are not redistributed.

## 3. Canonical paper replication

```bash
N_WORKERS=8 N_REPS=1000 bash run_all.sh
```

`N_WORKERS` can be increased or reduced to suit the machine. Worker count does not alter deterministic seeds or the scientific contract.

`N_REPS=1000` is the paper's setting and is required for exact comparison with the frozen repeated-order robustness results.

## 4. Faster workflow test

```bash
N_WORKERS=8 N_REPS=100 bash run_all.sh
```

or:

```bash
bash run_quick.sh
```

The quick run executes the same analytical sequence but uses fewer random landmark orders. It is useful for software/data-path validation and approximate robustness checks. The final verifier treats paper-specific repeated-order values as non-blocking under this mode.

## 5. Repetition-count changes

Checkpoint files encode the repetition schedule. If `N_REPS` changes after a run has generated Stage 08 or Stage 10 outputs, the runner refuses to continue.

Reset only the stochastic and dependent outputs with:

```bash
bash scripts/reset_repetition_outputs.sh
```

Stages 01–07 remain intact.

## 6. Parallelism

If `N_WORKERS` is omitted, the launcher uses R's physical-core detection.

Stage 10 also exposes:

```bash
ROBUSTNESS_WORKERS=...
```

Its default is the smaller of `N_WORKERS` and 80 because that stage can have a higher per-worker memory footprint. Users with sufficient RAM can raise it explicitly.

## 7. Verification

Canonical verification checks the frozen input hash, canonical topology fingerprint, topology summary, manuscript contrasts, spatial closure, scalarisation sensitivity and the 1,000-repetition robustness statistics.

A noncanonical `N_REPS` run still checks all deterministic/fixed-topology evidence, but the two repetition-sensitive robustness comparisons are warnings rather than blocking failures.

## 8. Radius decision

The full radius diagnostic grid is reproduced for transparency. Radius 1.50 is then applied as the documented researcher-approved choice. No automatic radius optimisation or reselection occurs.

## 9. Expected output

Canonical run:

```text
HFA_REPLICATION_VERIFICATION=PASS
HFA_CANONICAL_REPLICATION=PASS
```

Reduced-repetition run:

```text
HFA_REPLICATION_VERIFICATION=PASS
HFA_QUICK_REPLICATION=PASS
```

## Manuscript v0.25 post-baseline extensions

The original `v1.0.0` baseline remains preserved. Manuscript v0.25 adds a checksum-gated post-baseline stage at `pipeline/14_v025_extensions/`.

A canonical run uses `N_REPS=1000`. The extension stage reproduces:

- exact geographic-versus-barrier-profile KNN overlap for `k=1,...,100`;
- the independent-set combinatorial benchmark used by current Figure 4;
- the direct near-equal-score/different-profile criterion over 5 radii × 1,000 seeded landmark orderings;
- Supplementary Tables S4-S5 and their construction-level source data.

Current manuscript-facing frozen outputs are stored under `expected/manuscript_v0_25/`; historical `v1.0.0` expected outputs remain in place. The current manuscript source and figures remain outside the MIT software licence as documented in `LICENSE_SCOPE.md`.

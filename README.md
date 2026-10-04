# Similar Food-Access Scores, Different Barriers: Capturing Food-Access Profiles in England

**Repository:** https://github.com/srudkin12/Healthy-Food-Accessibility

Replication materials for the paper by **Barry Malachy, Tegan Massey, Simon Rudkin, Frances Stratton-Killick and Martha Surman**, Department of Social Statistics, University of Manchester. **Corresponding author: Simon Rudkin.**

## Authors

- Barry Malachy
- Tegan Massey
- Simon Rudkin — corresponding author
- Frances Stratton-Killick
- Martha Surman

All authors are affiliated with the **Department of Social Statistics, University of Manchester**.

## What this repository reproduces

The repository reconstructs the English LSOA analytical frame from the documented source vintages, builds the three-dimensional food-access constraint space, runs Ball Mapper radius diagnostics, freezes the researcher-approved radius **1.50** canonical topology, and reproduces the paper's interpretation, robustness, spatial comparison, scalarisation sensitivity, figures and machine-readable supplementary outputs.

The three measured constraints are:

- retail-proximity constraint;
- household car-availability constraint;
- income deprivation.

Canonical empirical identity:

- 33,755 English LSOAs;
- Ball Mapper radius 1.50;
- 24 overlapping BM balls;
- 92 graph edges;
- one connected component;
- 38 of 276 BM-ball pair comparisons meet the paper's near-equal-score/different-profile criterion.

The 38/276 quantity is a descriptive prevalence among **BM-ball profile comparisons**. It is not a percentage of LSOAs, households or people.

The radius is an explicit scientific choice. The code never selects an "optimal" radius automatically.

## v0.25 extensions

The repository now also reproduces two accepted analyses added after the original `v1.0.0` release:

1. geographic-versus-barrier-profile nearest-neighbour overlap for every `k=1,...,100`, with manuscript display at `k=5,10,...,100` and the exact independent-set combinatorial benchmark;
2. direct robustness of the headline BM-ball contrast over **5 radii × 1,000 seeded landmark orderings = 5,000 BM constructions**.

At `k=25`, 89.39% of LSOAs have zero shared members between their geographic and barrier-profile neighbour sets, compared with an independent-set zero-overlap benchmark of approximately 98.16%. Median Jaccard overlap remains zero across `k=1,...,100`.

Across the 5,000 accepted BM constructions, every construction contains at least one qualifying near-equal-score/different-profile pair; median pair prevalence is 15.79%, with the canonical cover at 13.77%.

These extensions preserve the frozen analytical input and canonical scientific design.

## Runtime controls

`N_WORKERS` controls parallel workers. `N_REPS` controls deterministic random landmark-order repetitions.

The canonical manuscript run uses:

```bash
N_WORKERS=8 N_REPS=1000 bash run_all.sh
```

A reduced-repetition smoke test can be run with:

```bash
bash run_quick.sh
```

Reduced-repetition runs are non-canonical and are not expected to reproduce the manuscript's exact 5,000-construction robustness evidence.

## Canonical replication

After obtaining the source data described in `DATA_SOURCES.md`:

```bash
N_WORKERS=8 N_REPS=1000 bash run_all.sh
```

The final canonical markers include:

```text
HFA_REPLICATION_VERIFICATION=PASS
HFA_V025_EXTENSIONS=PASS
HFA_MANUSCRIPT_OUTPUTS=PASS
HFA_CANONICAL_REPLICATION=PASS
```

The run is restartable. Expensive repeated-order calculations should not be mixed across different `N_REPS` values.

## Repository structure

```text
pipeline/       accepted source-construction and analysis code
framework/      TDABM/Ball Mapper framework used by the paper
scripts/        repository-level output, validation and maintenance scripts
config/         canonical expected values and runtime example
expected/       accepted compact evidence, including manuscript v0.25 outputs
provenance/     code-lineage and source-provenance material
manuscript/     manuscript v0.25 source and submission figures
```

`pipeline/14_v025_extensions/` contains the accepted k=1..100 KNN and 5,000-construction core-contrast extensions. `expected/manuscript_v0_25/` separates current manuscript-facing evidence from the historical `v1.0.0` baseline.

Generated raw data, staged data, full results, checkpoints, R libraries and logs are ignored by Git and are not redistributed.

## Current manuscript-facing spatial interpretation

Geography contains some information about barrier-profile similarity, because observed neighbour-set overlap is greater than the independent-set benchmark. Geographic proximity nevertheless does not reproduce most multivariate barrier-profile neighbour relationships. Figure 4 reports the observed zero-overlap share against the exact independent-set expectation.

Historical sampled-pair spatial diagnostics remain useful provenance material but are not presented as current v0.25 manuscript evidence.

## Reproducibility status

The original repository release followed a complete source-to-paper clean-room replication of the frozen analytical input and canonical topology. The v0.25 refresh adds checksum-gated accepted extension code and frozen manuscript-facing evidence without reopening the scientific design.

See `REPLICATION.md` for canonical versus reduced-repetition execution details and `pipeline/14_v025_extensions/PROVENANCE.md` for the post-release extension lineage.

## Licence

Software and computational replication materials in this repository are released under the MIT License; see `LICENSE` and `LICENSE_SCOPE.md`.

The manuscript source, supplementary article text and submission figures under `manuscript/` are not covered by the MIT License. They remain under the joint copyright of Barry Malachy, Tegan Massey, Simon Rudkin, Frances Stratton-Killick and Martha Surman pending publication and publisher licensing arrangements. Third-party/provider datasets are not redistributed.

# v0.25 post-baseline extensions

This stage integrates the accepted analyses added after the repository's original `v1.0.0` baseline so that the public replication tree reflects manuscript v0.25.

It adds two accepted calculations without changing the frozen scientific design:

1. exact geographic-versus-barrier-profile nearest-neighbour overlap for `k = 1,...,100`, together with the exact independent-set combinatorial benchmark;
2. direct robustness of the near-equal-score/different-profile BM-ball contrast across 5 radii × 1,000 seeded landmark orderings.

## Frozen scientific design

The stage does not select a new Ball Mapper radius, alter the three topology dimensions, change standardisation, change the canonical point order, or redefine the paper's headline contrast.

Canonical checks include:

- 33,755 English LSOAs;
- radius 1.50;
- 24 canonical BM balls;
- 276 unordered BM-ball pairs;
- 38 qualifying pairs under score gap `< 0.25` and profile distance `> 1.0`;
- Ball 13/18 score gap `0.0018742566141`;
- Ball 13/18 profile distance approximately `4.3972327089`;
- reproduction of the frozen Stage-12 KNN values at `k = 10, 25, 50`.

## Running

The repository-level `run_all.sh` invokes this stage before manuscript-output generation.

For a direct full run:

```bash
N_WORKERS=8 N_REPS=1000 bash pipeline/14_v025_extensions/run_hfa_v025_extensions.sh
```

Reduced `N_REPS` values are non-canonical smoke tests. Only `N_REPS=1000` is compared with the frozen v0.25 supplementary evidence.

## Outputs

Generated results are written below `pipeline/14_v025_extensions/results/` and remain generated artefacts rather than version-controlled raw data. Compact accepted v0.25 outputs are stored under `expected/manuscript_v0_25/supplementary/`.

## Provenance

`PROVENANCE.md` records the accepted package and handback lineages used to construct this repository-native stage.

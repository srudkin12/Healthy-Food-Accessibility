# Pipeline

`run_all.sh` at the repository root is the supported public entry point.

| Stage | Purpose |
|---|---|
| 01 | Reconstruct the English LSOA21 source frame and core source register |
| 02 | Build physical-access and transport-source components |
| 03 | Validate and harmonise the 2019 DfT journey-time comparator |
| 04 | Reconstruct Q1-2023 retail supply and functional physical access |
| 05 | Add household car-availability and income-deprivation constraints and diagnostic comparator structures |
| 06 | Add RUC, IUC and DfT TCM contextual readouts |
| 07 | Freeze the three-axis analytical input |
| 08 | Run the 915-radius Ball Mapper diagnostic grid and repeated landmark orders |
| 09 | Construct the researcher-approved radius-1.50 canonical topology |
| 10 | Produce canonical evidence and repeated-order/nearby-radius robustness |
| 11 | Build 24-ball interpretation and severity/configuration contrasts |
| 12 | Run the targeted spatial closure |
| 13 | Run scalar-threshold and scalar-weight robustness |
| 14 | Regenerate manuscript-facing figures/tables |
| 15 | Verify the replication against the canonical evidence contract |

## Runtime controls

`N_WORKERS` controls parallel workers.

`N_REPS` controls the repeated landmark-order calculations in Stages 08 and 10.
The paper used `N_REPS=1000`. Smaller values are supported for faster workflow
tests but are labelled `QUICK_NONCANONICAL`.

Stage 10 also supports `ROBUSTNESS_WORKERS`, which defaults to
`min(N_WORKERS, 80)` because that stage can have a larger per-worker memory
footprint.

Do not invoke historical stage-package launchers from the development workflow;
they are intentionally not distributed. The root `run_all.sh` provides the
de-patched, restartable public execution sequence.

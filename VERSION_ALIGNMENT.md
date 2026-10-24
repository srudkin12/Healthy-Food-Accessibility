# Version alignment

## Current publication pair

| Item | Current version |
|---|---|
| Working-paper / submission manuscript | **v0.26** |
| GitHub replication release | **v1.1.1** |
| Release date | **24 October 2026** |
| Empirical evidence freeze | **v0.25 analytical contract, unchanged in v0.26** |

Repository `v1.1.1` is the release intended to accompany manuscript `v0.26`.

## Exact manuscript-source identity

`manuscript/manuscript.tex` is byte-identical to the frozen v0.26 TeX source.

SHA-256:

```text
dcc562028cae969d7538d0fbdbd9e6985c474ebbcb31a6741af82cea96c5ff79
```

## Why some paths still contain `v025`

The following names are intentionally retained:

- `pipeline/14_v025_extensions/`
- `expected/manuscript_v0_25/`
- `config/v0_25_expected_results.json`

Those names identify the empirical-development stage at which the additional KNN and
5,000-construction robustness evidence was frozen. Manuscript v0.26 does **not**
replace or recompute that evidence. Renaming the directories would weaken provenance
without changing their scientific content.

## What changed between manuscript v0.25 and v0.26

Manuscript v0.26:

- uses the final readable versions of Figures 2 and 3;
- corrects the prose definition of Jaccard similarity;
- tightens several descriptions of the main result;
- clarifies the sequential policy use of an overall score, barrier profile and
  additional causal/local evidence;
- sharpens the interpretation of geographically distant profile comparators.

No empirical estimate, topology, threshold, weighting result, spatial result or
robustness result changed.

## Final Figure 2/3 identities

| File | SHA-256 |
|---|---|
| `figure2_topology_axis_heatmap.pdf` | `98bb2ba8e0bffef5ec7e78d6e30f518dcfe2932d41be4bdae1321e5e92ac470b` |
| `figure2_topology_axis_heatmap.png` | `1e0ad6d51f52a55fb4abc5788fd857057a8f99f37b6ed2787d617b3b1b6237f0` |
| `figure3_context_readout_heatmap.pdf` | `b7425447ac72895b2c919084cf40e0f382f305b9c2e66e302f6b3ccfca738671` |
| `figure3_context_readout_heatmap.png` | `56fce5e20470552ab2f99f7d171350b30f7f8858bba3ea23998b03c31eeb779b` |

## Historical releases

- `v1.0.0` remains the original repository release.
- `v1.1.0` remains the October 2026 empirical-extension release and is not moved.
- `v1.1.1` adds manuscript-v0.26 alignment and final presentation/readability changes.

No historical tag is rewritten.

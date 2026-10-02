# NEW CONVERSATION HANDOVER — TDABM Portable Framework

Use this document as the authoritative starting state when this folder is
uploaded to a new conversation.

## What this folder is

This is the application-neutral clean TDABM portability baseline.

It contains the latest accepted generic analytical machinery through:

- P1.4-A source/input freeze contracts;
- P1.4-B v1.1.3 resolution-aware topology-only radius diagnostics;
- P1.4-C v1.0.3 canonical fixed-topology contracts;
- P1.4-D v1.0 paper-evidence and interpretation-robustness contracts.

No real application is active in this folder.

## Outbound application request

When the portability conversation needs information from an existing
application conversation, send that conversation:

`APPLICATION_HANDOVER_REQUEST.md`

Do not send `NEW_CONVERSATION_HANDOVER.md` for that purpose. This document is
the inbound operating handover for the portability conversation itself.

## What the new conversation should do first

1. Run or review the output of `bash setup_tdabm_clean.sh`.
2. Read `docs/01_PIPELINE_ARCHITECTURE.md`.
3. Obtain an application handover/source pack.
4. Recover the application contract from evidence rather than memory.
5. Create a new adapter under `applications/<application_id>/`.
6. Begin at P1.4-A unless an accepted immutable input freeze with exact
   cryptographic provenance is supplied.
7. Proceed stage by stage; never silently skip the user-radius decision.

## Non-negotiable framework rules

- Framework code must remain application-neutral.
- Diagnostics may suggest observations but may not choose exclusions.
- A historical radius is provenance only until explicitly approved for the new
  application.
- Colour/outcome variables may not choose radius.
- Connectedness is not a hard radius gate.
- Radius-selection evidence is non-binding and does not establish mathematical
  optimality.
- Ball Mapper raw landmark positions from the C++ interface are zero-based and
  must be normalized exactly once before R indexing.
- The accepted canonical topology is immutable.
- Recolouring must preserve memberships, landmarks, edges, radius and point
  order.
- P1.4-D robustness topologies are transient and may not become canonical
  without a new explicit analysis/freeze decision.
- Process exit code alone is insufficient: stage semantic postconditions must
  be checked.
- Paper-facing artifacts require cryptographic provenance.
- Application paper interpretation belongs outside the generic framework.

## Current radius-diagnostic interpretation

The generic P1.4-B reference is the interpolated radius at which the median
largest ball first covers 50% of observations.

The 40% and 60% largest-ball dominance crossings form a non-binding review
region. A 75% crossing is useful as a severe-coarsening marker. A user may
explicitly approve a radius outside these anchors; the position and warnings
are recorded but do not become automatic admissibility rules.

These are diagnostic anchors, not an automatic optimum.

A final application radius must be explicitly approved by the user and should
be recorded with:

- pipeline diagnostic reference;
- approved radius;
- selection basis;
- precision policy;
- topology variables;
- scaling;
- metric;
- user rationale;
- automatic_selection = FALSE;
- optimality_claim = NONE.

## New-application workflow

```text
authoritative source material
          ↓
P1.4-A reconstruction + immutable input freeze
          ↓
P1.4-B topology-only radius diagnostics
          ↓
explicit user radius decision
          ↓
P1.4-C canonical topology freeze
          ↓
post-freeze recolouring
          ↓
P1.4-D paper evidence + interpretation robustness
          ↓
application/paper handover
```

## What not to do

Do not:

- resurrect an old application from filenames or memory;
- copy a complete-case sample from another topology specification;
- use colour variables to tune radius;
- force the graph to be globally connected;
- freeze a topology before explicit radius approval;
- reconstruct topology inside a plotting or paper-output stage;
- put application data or results into `framework/`;
- treat candidate analytical figures as automatically publication-ready.

## Files a new conversation should normally request

A new application should provide, where possible:

- raw/source data or an authoritative derived dataset;
- source-to-derived variable definitions;
- stable ID and human-readable labels;
- exact topology-variable set;
- scaling and metric;
- complete-case and exclusion rules;
- colour/context variables;
- any historical settings as provenance;
- prior accepted hashes/audits if a stage is being resumed.

If any of these are unverified, stop and mark the field unresolved rather than
guessing.

## Machine portability

This clean folder contains no machine-specific application paths. The usual
environment variables are:

```bash
export TDABM_PORTABILITY_ROOT="/path/to/TDABM_Portability"
export TDABM_FRAMEWORK_ROOT="$TDABM_PORTABILITY_ROOT/framework"
```

Worker counts belong in application configuration/environment variables and
must not be hard-coded to a particular computer.

## Baseline provenance

The clean source was curated from the accepted P1.4-D framework tree after the
generic P1.4-B v1.1.3 and P1.4-C v1.0.3 corrections had been incorporated.

`manifests/SOURCE_LINEAGE.csv` records every inherited source file and its
source/clean SHA-256.

`manifests/CLEAN_BASELINE_MANIFEST.csv` records the exact clean package state.

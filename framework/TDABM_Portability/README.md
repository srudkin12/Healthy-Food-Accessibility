# TDABM Portable Framework — Clean Start

This directory is the **application-neutral TDABM portability baseline**.

It is intended to be copied to any computer, uploaded to a new ChatGPT
conversation, and used as the starting point for a new TDABM application.

## File to send to an application conversation

Use:

`APPLICATION_HANDOVER_REQUEST.md`

Send that file to the conversation that owns the application/data/paper. It
requests the source files, variable definitions, sample rules, historical
settings and accepted artifacts needed to activate the application.

`NEW_CONVERSATION_HANDOVER.md` serves a different purpose: it tells a **new
portability conversation** how to operate this framework.

## Start here

1. Keep this directory intact.
2. On a new computer, open a shell in this directory.
3. Run:

```bash
bash setup_tdabm_clean.sh
```

4. Read `NEW_CONVERSATION_HANDOVER.md`.
5. Put each real application under `applications/<application_id>/`.
6. Do not put application data, results, audit ZIPs, or manuscript files inside
   `framework/`.

`setup_tdabm_clean.sh` verifies the clean package, checks dependencies and runs
the generic synthetic contract tests. It does not run an application.

## Environment

Recommended variables:

```bash
export TDABM_PORTABILITY_ROOT="/path/to/TDABM_Portability"
export TDABM_FRAMEWORK_ROOT="$TDABM_PORTABILITY_ROOT/framework"
```

Application-specific roots should be declared separately by the application
adapter. No machine-specific absolute path is embedded in the framework.

## Current methodological pipeline

### P1.4-A — source reconstruction and immutable input freeze

Purpose:

- reconstruct declared variables from authoritative source material;
- verify identifiers, labels, formulas and complete-case rules;
- record source and derived-file SHA-256 hashes;
- freeze an immutable analytical input.

P1.4-A must not construct Ball Mapper topology and must not choose substantive
exclusions. Diagnostics may identify observations for review, but only the user
may approve an exclusion.

### P1.4-B — topology-only radius diagnostics

Current generic radius methodology is the corrected **P1.4-B v1.1.3** logic.

Key rules:

- radius diagnostics use topology variables only;
- colour/outcome variables cannot select a radius;
- connectedness and isolated balls are descriptive, not admissibility gates;
- actual and effective ball counts, largest-ball dominance, overlap and
  repeated-order stability are reported;
- the interpolated 50% majority-cover transition is a non-binding reference;
- the 40%–60% dominance crossings form a non-binding review region;
- an explicitly approved radius may lie outside that region; the position is recorded diagnostically rather than used as an admissibility gate;
- no automatic radius approval;
- no optimality claim;
- the user explicitly approves the application radius.

The C++ `landmarks` field is zero-based at the interface boundary. Generic R
code normalizes it exactly once before R indexing. Membership and graph indices
retain their existing one-based contract.

### P1.4-C — canonical fixed topology

Current canonical-topology/user-radius contract is **P1.4-C v1.0.3**.

After explicit user radius approval:

- canonicalize point order using a stable identifier;
- construct Ball Mapper exactly once at the approved radius;
- validate landmark-index normalization;
- freeze point order, landmarks, memberships, edges and topology fingerprint;
- downstream recolouring may change colours only;
- downstream paper/output stages may not silently reconstruct topology.

### P1.4-D — paper evidence and interpretation robustness

After the canonical freeze:

- generate topology, membership, component, overlap and ball-profile evidence;
- generate point-indexed local-colour evidence;
- run post-approval local-radius robustness;
- run landmark-order interpretation robustness;
- create candidate fixed-layout paper figures;
- preserve source-to-output SHA-256 provenance;
- create a compact analytical handover for the application/paper conversation.

Alternative-radius objects in P1.4-D are transient robustness objects. They
cannot replace the canonical topology.

## What is deliberately NOT in this clean package

The cleanup removed:

- all real application adapters;
- all application data and source packs;
- all application run directories;
- all application audit ZIPs and handover ZIPs;
- all earlier extracted framework releases;
- all earlier release ZIPs and installers;
- legacy application-specific paper scripts;
- development logs, local runs and acceptance folders;
- duplicate provenance snapshots of old source trees.

Those materials should be archived outside this directory if historical
recovery is required.

## Core directories

- `framework/` — current reusable R/C++ source.
- `tools/` — clean-package checks and synthetic tests.
- `templates/new_application/` — starting scaffold for a new application.
- `applications/` — intentionally empty except for instructions.
- `docs/` — current methodological and portability contracts.
- `manifests/` — clean baseline hashes and source lineage.

## Important distinction

The framework supplies reusable analytical machinery. An application must
supply and justify its own:

- source data;
- identifier and label fields;
- derived-variable formulas;
- topology axes;
- scaling;
- metric;
- complete-case rule;
- colour/context variables;
- any substantive exclusions;
- historical settings used only as provenance;
- final user-approved radius.

Do not infer those items from a previous application.

## Updating the framework

Do not overwrite this baseline in place while developing a methodological
change. Create a new versioned sibling release, run generic tests, and update
the clean manifest only after acceptance.

For a new application, do not edit files under `framework/`; create an
application adapter under `applications/<application_id>/`.

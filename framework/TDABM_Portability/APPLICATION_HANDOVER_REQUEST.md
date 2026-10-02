# TDABM New-Application Handover Request

**Send this file to the conversation that currently owns the application, data,
paper, or historical analysis.**

The purpose of this request is to recover everything required to activate the
application in the current **application-neutral TDABM Portable Framework**.

Please answer from the evidence available in your conversation and its files.
Treat the application as though the portability conversation knows nothing
about it.

Do **not** fill gaps from memory, inference, or a similar application. Mark
unsupported items explicitly as `UNRESOLVED`.

---

# 1. Application identity

Please provide:

- proposed application ID;
- application/paper/project name;
- purpose of the TDABM analysis;
- whether this is a main specification, robustness specification, illustration,
  replication, or other role;
- manuscript section/figure/table associated with it, if relevant;
- historical code/file labels used for the application.

---

# 2. Authoritative source data

Identify every file needed to reconstruct the application.

For each source file provide:

- filename;
- role;
- whether it is raw, derived, lookup, metadata, or output;
- source/provenance;
- date/version if known;
- SHA-256 if available;
- whether it can be included in the response ZIP.

Please distinguish authoritative analytical source files from old outputs or
figures that are useful only for validation.

---

# 3. Unit identifier and labels

Provide:

- stable unit/observation identifier;
- human-readable label;
- source file containing each;
- uniqueness rule;
- any historical ordering rule.

The portable framework will impose deterministic canonical point ordering using
the stable identifier. Historical row order is provenance only unless the
application can demonstrate that it is analytically required.

---

# 4. Exact topology-variable contract

List every variable that belongs in the Ball Mapper topology.

For each topology variable provide:

- final variable name;
- exact source variable(s);
- exact derivation formula;
- units;
- transformation;
- expected range if known;
- interpretation of higher/lower values;
- evidence/source supporting the definition.

Do not substitute related variables.

If the exact formula or source cannot be verified, mark it `UNRESOLVED`.

Please also state explicitly which available variables do **not** belong in the
topology.

---

# 5. Scaling and metric

Recover and report the intended or historical:

- scaling/standardisation policy;
- distance metric;
- axis weighting, if any;
- preprocessing before Ball Mapper.

Examples include raw units, raw percentages, z-scores, min-max scaling,
Euclidean distance, Manhattan distance, or a weighted metric.

Do not infer these settings from a figure alone.

---

# 6. Sample construction and missing-data rule

Please recover the application-specific sample rule.

Report:

- number of candidate observations before filtering;
- variables required to be non-missing;
- complete-case rule;
- duplicate handling;
- other mechanical exclusions;
- substantive/manual exclusions;
- final historical sample size, if one exists;
- identities and reasons for excluded observations where recoverable.

**Do not inherit the complete-case sample from another topology specification.**

Diagnostics may identify observations for review, but the portable framework
will not approve substantive exclusions automatically.

If an old exclusion exists but its meaning cannot be verified, mark it
`UNRESOLVED`.

---

# 7. Colouring/context variables

List every variable that should be used after topology construction to colour,
summarise, or interpret the fixed topology.

For each provide:

- variable name;
- source variable(s);
- exact construction formula;
- units;
- manuscript/analytical role;
- whether it is colouring-only;
- evidence/source.

Colouring/context variables must be kept separate from topology variables
unless the application contract explicitly says otherwise.

They cannot be used to select the Ball Mapper radius.

---

# 8. Historical Ball Mapper/radius information

Recover, where available:

- historical radius/epsilon;
- historical number of balls;
- edges;
- components;
- isolated balls;
- overlap/membership evidence;
- `min_points` or equivalent settings;
- Ball Mapper implementation/version;
- landmark-order policy;
- seed;
- any historical plots or output tables.

Any historical radius must be labelled:

`HISTORICAL_REFERENCE_ONLY`

The current portable pipeline will run fresh topology-only radius diagnostics
and require a new explicit user approval unless an already accepted current
radius contract is supplied.

---

# 9. Existing code

Please identify and provide any code that materially supports:

- source reconstruction;
- variable derivation;
- sample construction;
- Ball Mapper construction;
- colouring;
- robustness;
- figures/tables;
- model diagnostics.

For each code file state whether it should be treated as:

- authoritative reconstruction code;
- historical reference code;
- obsolete/superseded code;
- validation-only code.

Do not ask the new portability conversation to guess which of several versions
is authoritative.

---

# 10. Existing accepted analytical artifacts

If the application has already completed any current portable-framework stage,
provide the accepted artifacts and their SHA-256 hashes.

Relevant items may include:

- frozen input;
- source/reconstruction audit;
- radius-diagnostic audit;
- explicit user-approved radius record;
- canonical topology object;
- topology fingerprint;
- membership/landmark/edge tables;
- paper-evidence/robustness audit;
- application-paper handover.

If those artifacts do not exist, state that the new run should begin at
P1.4-A.

---

# 11. Model-diagnostic requirements

If the application uses TDABM for model criticism, recover any verified
historical model specification.

For each model provide:

- dependent variable;
- predictors;
- functional form;
- estimation sample;
- transformations;
- fitted-value definition;
- residual definition;
- classification rule and threshold if applicable;
- paper/figure role.

If no verified historical model exists, state:

`NO VERIFIED HISTORICAL MODEL SPECIFICATION FOUND`

A new benchmark model can later be approved separately.

---

# 12. Publication-facing labels

Where relevant, provide:

- reader-facing names for topology axes;
- reader-facing names for colouring variables;
- original figure titles;
- captions;
- terminology used in the manuscript.

Internal variable names should remain preserved in the analytical contract.
Figure-title/caption editing is a presentation task and does not require
topology reconstruction.

---

# 13. Multiple specifications

If the project contains more than one topology specification, treat each as an
independently auditable application unless their relationship is explicitly
defined.

For each specification provide separate:

- topology variables;
- source formulas;
- sample/complete-case rule;
- historical radius;
- colour variables;
- accepted outputs.

Do not silently reuse one specification's sample or radius for another.

---

# 14. Files requested in the response

Please return two principal deliverables.

## A. Response document

Suggested name:

`<APPLICATION_ID>_TDABM_Application_Handover_Response_v1_0.md`

It should contain:

- executive summary;
- application contract;
- source/data inventory;
- variable register;
- sample construction;
- colouring/context register;
- historical-radius/settings record;
- model-diagnostic record;
- unresolved items;
- recommended starting stage for the portability conversation.

## B. Evidence/source ZIP

Suggested name:

`<APPLICATION_ID>_TDABM_Application_Source_and_Evidence_Pack_v1_0.zip`

Include only files materially required to reconstruct or verify the application.

Please also provide:

`<APPLICATION_ID>_TDABM_Application_Source_and_Evidence_Pack_v1_0.zip.sha256`

---

# 15. Preferred machine-readable registers

Where possible include:

- `application_contract.csv`
- `source_provenance.csv`
- `variable_register.csv`
- `sample_audit.csv`
- `colour_variable_register.csv`
- `historical_radius_register.csv`
- `historical_topology_summary.csv`
- `model_diagnostic_register.csv`
- `accepted_artifact_register.csv`
- `unresolved_items.csv`
- `SHA256SUMS.txt`

---

# 16. Status vocabulary

Use these statuses wherever useful:

- `VERIFIED`
- `HISTORICAL_REFERENCE_ONLY`
- `UNRESOLVED`
- `NOT_APPLICABLE`
- `NO_VERIFIED_HISTORICAL_EVIDENCE`
- `ACCEPTED_CURRENT_ARTIFACT`

Do not resolve uncertainty by assumption.

---

# 17. Current portable-framework rules

The receiving portability conversation will apply the current generic sequence:

```text
authoritative application evidence
            ↓
P1.4-A source reconstruction + immutable input freeze
            ↓
P1.4-B topology-only radius diagnostics
            ↓
explicit user radius decision
            ↓
P1.4-C canonical fixed-topology freeze
            ↓
post-freeze recolouring
            ↓
P1.4-D paper evidence + interpretation robustness
            ↓
application/paper handover
```

Key rules:

- framework source remains application-neutral;
- colour/outcome variables cannot select radius;
- connectedness/isolation are descriptive, not hard radius gates;
- the topology-only radius reference and review region are non-binding;
- an explicitly approved radius may lie outside the review region when rationale and provenance are recorded;
- automatic radius selection is forbidden;
- no optimality claim is made;
- raw C++ landmark positions are normalized exactly once before R indexing;
- canonical topology is immutable after freeze;
- P1.4-D alternative-radius topologies are transient robustness objects;
- downstream paper/output code may not silently reconstruct topology;
- analytical artifacts require SHA-256 provenance.

---

# 18. Key question

The most important question for your response is:

> Can this application be reconstructed as an explicit, independently auditable
> TDABM application contract from authoritative evidence, without relying on
> another conversation's memory?

If yes, provide the response document and evidence ZIP.

If not, identify exactly what remains unresolved and what source material is
needed before the application can enter the portable pipeline.

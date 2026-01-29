# Antagonist test triage (draft)

This document groups the current antagonist cases into failure categories and
maps each category to defensive checks that would mitigate multiple issues at
once. No code changes are proposed here; this is triage only.

## Triage: fix effort vs impact

### Trivial / validation‑only (low risk, low redesign)

These are mostly preflight checks or clearer errors. They can be added without
changing the data model or output structure (aside from new warnings/errors).

1) **Time axis validity checks (per group)**
   - Require finite `x`/`xend` (no NA/NaN/Inf) before compute or alignment.
   - Enforce `x` monotonic non‑decreasing in samples mode; error on reversal.
   - Reject character `x`/`xend` (require numeric/datetime).
   - Require `interval > 0` when `interval_mode = "explicit"`.
   - Error if `xend < x` in ranges mode; warn if `xend == x`.
   - **Covers:** NA in x/xend, unsorted/non‑monotonic x, explicit interval NA/negative, negative durations, zero durations, character x/xend, alignment NA collapse.

2) **Auto‑mode dispatcher guardrails**
   - If `xend` present and `x` missing: error with guidance.
   - If `x` present and `y` missing: error with guidance.
   - **Covers:** auto mode ambiguities and confusing downstream errors.

3) **Behaviour integrity checks**
   - Require atomic `behaviour` (no list columns).
   - If `behaviour` contains NA and `remove_nas=FALSE`, emit warning about run splits.
   - **Covers:** list behaviour errors, NA‑split runs.

4) **Alignment preflight**
   - Require finite `x/xend` for alignment; error on NA.
   - If `by` selects zero columns, include candidate column names in error.
   - **Covers:** alignment NA collapse, clearer missing‑column errors.

5) **PANEL completeness**
   - If `PANEL` exists, require it is non‑missing; error or warn when partially NA.
   - **Covers:** PANEL missing rows, inconsistent panel grouping.

### Medium effort (some behavior change / more careful design)

These are still mostly validation, but may affect output semantics or require
user‑visible policy decisions.

1) **Irregular sampling detection**
   - Warn when `diff(x)` is not constant within a group; optionally require
     explicit interval.
   - **Covers:** irregular intervals, outlier gaps, DST distortions, min‑diff
     underestimation.

2) **Duplicate time conflict detection**
   - Detect same `(x, y, group, PANEL)` with multiple `behaviour` values and
     warn/error.
   - **Covers:** duplicated times with conflicting labels.

3) **Gap‑aware run splitting**
   - If gaps exceed k * interval, break runs even if behaviour unchanged.
   - **Covers:** large‑gap same behaviour collapsing into a single segment.

4) **Overlap checks in ranges mode**
   - Detect overlapping intervals within a group and warn/error.
   - **Covers:** overlapping segments in ranges data.

### Heavier redesign (back burner)

These likely require a clearer data model or additional API surface to resolve
ambiguities without surprising users.

1) **Grouping inference and trial ambiguity**
   - Auto‑grouping by `y` + `PANEL` merges independent trials/subjects.
   - Fixing this requires either strict requirement for `group` or additional
     metadata conventions.
   - **Covers:** time resets without group, mixed conditions without facets,
     multiple subjects sharing y, implied trials collapsed by y.

2) **Timezone/DST semantics**
   - Correct handling requires explicit policy: force UTC, require explicit
     interval, or normalize datetimes with a documented expectation.
   - **Covers:** DST gap/repeat and mixed timezone inputs.

3) **Performance scaling**
   - Current pipelines are slow for 1e6 rows; real fixes may require backend
     changes (data.table, vectorized C++ run detection) or alternative APIs.
   - **Covers:** 1e6 row cases and many‑group high‑volume workloads.

## Categories and shared mitigations

### 1) Time axis validity (monotonicity, uniqueness, NA, type)

**Symptoms**
- NA in `x` or `xend` yields NA intervals or alignment collapse.
- Unsorted or non‑monotonic `x` changes inferred intervals and run collapsing.
- Duplicate `x` leads to zero‑length segments or conflicting labels.
- Character `x`/`xend` silently treated as non‑numeric.
- Negative or NA intervals (explicit) produce invalid segments.
- DST gaps/repeats distort inferred intervals.

**Shared mitigation**
- Preflight check per group: `x` is non‑NA, monotonic non‑decreasing, and
  (optionally) strictly increasing for samples mode.
- Validate `x`/`xend` types are numeric/datetime; reject character.
- If `interval_mode="explicit"`, require finite `interval > 0`.
- For datetime: warn when time zone changes or DST discontinuities are detected
  (or require explicit `interval`).

**Covers**
- `samples: NA in x`, `samples: unsorted x`, `samples: non‑monotonic`,
  `samples: time reversal`, `samples: duplicated x`, `samples: fractional
  seconds repeated`, `samples: explicit interval NA`, `samples: explicit
  negative interval`, `intervals: NA in xend`, `intervals: x/xend as character`,
  DST spring/fall cases.

### 2) Interval consistency and overlaps

**Symptoms**
- `min(diff(x))` underestimates intervals when sampling is irregular.
- Overlapping or negative intervals in ranges mode go unchecked.
- Large gaps in samples collapse into one run with no warning.

**Shared mitigation**
- If inferred intervals vary within a group, warn (or error in strict mode).
- In ranges mode, enforce `xend >= x` and warn on overlaps within group.
- Optional rule: gap‑aware run breaking (if gaps exceed k * interval).

**Covers**
- `samples: irregular intervals`, `samples: extreme outlier gap`,
  `samples: large gap same behaviour`, `intervals: negative durations`,
  `intervals: overlapping segments`, `intervals: zero duration`.

### 3) Grouping ambiguity (mis‑grouped trials / missing faceting)

**Symptoms**
- Distinct trials or subjects merged because `group` defaults to `y` (and
  `PANEL` only if present).
- Mixed conditions without facets produce overlapping runs.
- `group` column present but unintentionally used (or all NA).

**Shared mitigation**
- Require explicit `group` when duplicate `x` values appear across rows with
  different non‑time identifiers.
- Detect repeated time resets within a group (e.g., `x` returns to 0) and warn
  that `group` is likely missing.
- If `group` exists but all NA, warn and ignore.

**Covers**
- `samples: time reset without group`, `samples: mixed conditions without
  group`, `samples: multiple subjects sharing y`, `implied: multiple trials
  collapsed by y`, `samples: group column all NA`.

### 4) Behaviour column integrity

**Symptoms**
- List‑columns or non‑atomic `behaviour` break run detection.
- NA `behaviour` within a run splits the run unexpectedly when `remove_nas=FALSE`.
- Conflicting labels at the same time are silently accepted.

**Shared mitigation**
- Validate `behaviour` is atomic and length‑consistent.
- If `behaviour` contains NA and `remove_nas=FALSE`, warn about run splits.
- Detect same `(x, y, group)` with multiple `behaviour` values and warn/error.

**Covers**
- `implied: list behaviour`, `samples: behaviour as list`,
  `samples: NA in behaviour within run`, `samples: duplicated x conflicting labels`.

### 5) Auto mode ambiguity (dispatcher)

**Symptoms**
- `xend` without `x` falls back to implied mode silently.
- `x` without `y` triggers confusing errors downstream.

**Shared mitigation**
- Strengthen dispatcher: if `xend` present but `x` missing, error with guidance.
- Validate required columns before mode selection.

**Covers**
- `auto mode: xend only`, `auto mode: x without y`, `auto mode: x/xend without y`.

### 6) Alignment safety

**Symptoms**
- Alignment with NA in `x/xend` collapses everything to NA.
- Alignment by missing columns fails (good) but error could be clearer.

**Shared mitigation**
- Require finite `x/xend` before alignment; warn or error if NA present.
- Include group summary in alignment errors (which columns were missing).

**Covers**
- `align: NA in x`, `align: by missing column`, `align: by columns with NA`.

### 7) PANEL/facet interactions

**Symptoms**
- PANEL present but partially missing or non‑numeric creates inconsistent group
  boundaries.
- Same `y` across panels can be mis‑grouped if PANEL not respected.

**Shared mitigation**
- If `PANEL` exists, enforce it is complete and non‑missing.
- Warn if `PANEL` exists but is partially NA.

**Covers**
- `samples: PANEL separates identical y`, `samples: PANEL missing in some rows`,
  `samples: PANEL as character`, `intervals: PANEL with overlapping segments`.

### 8) Performance stress (scale)

**Symptoms**
- 1e6 rows runs are tens of seconds; grouped summarise + run detection are
  the primary cost.

**Shared mitigation (future work)**
- Provide optional fast‑path for already‑sorted data (skip checks).
- Consider data.table backend or vectorized run‑length encoding per group.
- Offer a `strict = FALSE` mode that skips expensive validations.

**Covers**
- `samples: 1e6 rows single group`, `samples: many groups`, `implied: 1e6 rows`.

## Priority suggestions (no code changes)

1) Implement a strict validation layer (toggleable) covering time validity,
   grouping ambiguity, and behaviour integrity.
2) Strengthen auto‑mode dispatcher errors to prevent silent mode mismatch.
3) Add explicit warnings for irregular sampling and overlaps.
4) Provide documentation examples on grouping and facets to avoid human errors.

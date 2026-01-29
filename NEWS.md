# ggethos development news

## 0.0.0.9000

### Breaking-ish changes
- Stricter time validation: `x`/`xend` must be complete and time-like; non-monotonic `x` in samples now errors; explicit `interval` must be finite and > 0.
- Intervals mode now errors on `xend < x` and warns on `xend == x`.
- Alignment now errors if `x`/`xend` contain missing/non-finite values.

### New
- Antagonist test suite with combined entrypoint and fixture data for edge-case coverage.
- Antagonist triage document and roadmap updates.
- Ethogram demo data and additional compute tests.

### Changed
- Compute helpers refactored; documentation and snapshots refreshed.

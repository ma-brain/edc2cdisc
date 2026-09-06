# Overnight Runbook — QS Conformance Completion

Date: 2026-09-06. Same contract as the previous runs.

## Pre-approved

- Design doc: `docs/superpowers/specs/2026-09-06-qs-conformance-design.md`
  (user gives one "go" before starting).
- End state: **merge locally, never push.**

## Kickoff prompt

Execute `docs/superpowers/plans/2026-09-06-qs-conformance.md` via
superpowers:subagent-driven-development on branch `qs-conformance`,
following the plan's unattended-overnight rules and this runbook.

## Session rules

- Full subagent-driven-development process; ledger at
  `.superpowers/sdd/progress.md`; review packages per task.
- Byte-freedom accounting: `qs.rds` re-frozen deliberately at Task 4
  (three new columns); **every other reference byte-identical**; digests
  untouched (no generator change). A moved reference outside this list =
  STOP.
- **Known red window spanning Tasks 1–3**: the `qs.rds` regression compare
  fails from Task 1 until Task 4's deliberate re-freeze. That single
  failure is planned; any OTHER failure is not.
- Stop conditions: BLOCKED twice on the same task; byte-freedom violation;
  Needs-fixes surviving two fix cycles; decisions the docs don't make.
- Never push; never re-freeze outside Task 4's gate.

## Morning report contents

Task-vs-commit map, final review verdict, merge commit, suite/check/lint
evidence, carried findings on the ledger, anything that stopped the run.

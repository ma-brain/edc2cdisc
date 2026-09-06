# Overnight Runbook — ADQS (BDS)

Date: 2026-09-06. Same contract as the previous runs.

## Pre-approved

- Design doc: `docs/superpowers/specs/2026-09-06-adqs-bds-design.md`
  (user gives one "go" before starting).
- End state: **merge locally, never push.**

## Kickoff prompt

Execute `docs/superpowers/plans/2026-09-06-adqs-bds.md` via
superpowers:subagent-driven-development on branch `adqs-bds`, following the
plan's unattended-overnight rules and this runbook.

## Session rules

- Full subagent-driven-development process; ledger at
  `.superpowers/sdd/progress.md`; review packages per task.
- Byte-freedom accounting (simple this run): `adqs.rds` new at Task 4;
  **every other reference byte-identical**; digests untouched (no generator
  change). A moved reference outside this list = STOP.
- `validate_adam()` signature grows exactly once (Task 3); every call site
  updated in that task — grep `validate_adam(` across R/ and tests/.
- Stop conditions: BLOCKED twice on the same task; byte-freedom violation;
  Needs-fixes surviving two fix cycles; decisions the docs don't make.
- Never push; never re-freeze outside Task 4's gate.

## Morning report contents

Task-vs-commit map, final review verdict, merge commit, suite/check/lint
evidence, carried findings on the ledger, anything that stopped the run.

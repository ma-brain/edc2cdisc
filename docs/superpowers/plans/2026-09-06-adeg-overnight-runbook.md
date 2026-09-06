# Overnight Runbook — ADEG (BDS)

Date: 2026-09-06. Same contract as the previous runs.

## Pre-approved

- Design doc: `docs/superpowers/specs/2026-09-06-adeg-bds-design.md`
  (user gives one "go" before resting).
- End state: **merge locally, never push.**

## Kickoff prompt

Execute `docs/superpowers/plans/2026-09-06-adeg-bds.md` via
superpowers:subagent-driven-development on branch `adeg-bds`, following the
plan's unattended-overnight rules and this runbook.

## Session rules

- Full subagent-driven-development process; ledger at
  `.superpowers/sdd/progress.md`; review packages per task.
- Byte-freedom accounting (differs per task — read the plan): EG.csv digest
  moves (value change on the seeded QT/QTCF row, diff confined to that row);
  `eg.rds` re-frozen deliberately at Task 2 (same confined diff);
  `adeg.rds` new at Task 5; **every other digest and reference
  byte-identical**. A moved reference outside this list = STOP.
- `validate_adam()`'s signature grows exactly once (Task 4); every call site
  updated in that task.
- Stop conditions: BLOCKED twice on the same task; byte-freedom violation;
  Needs-fixes surviving two fix cycles; decisions the docs don't make.
- Never push; never re-freeze outside the plan's freeze tasks.

## Morning report contents

Task-vs-commit map, final review verdict, merge commit, suite/check/lint
evidence, carried findings on the ledger, anything that stopped the run.

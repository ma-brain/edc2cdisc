# Overnight Runbook — SE + SUPPMH + SUPPVS

Date: 2026-09-06. Contract for the unattended session; same rules as the
2026-09-05 run (which completed cleanly).

## Pre-approved

- Design doc: `docs/superpowers/specs/2026-09-06-se-supp-qualifiers-design.md`
  (decisions documented; user gives one "go" before resting).
- End state: **merge locally, never push.** Final review verdict gates the
  merge; suite re-verified on the merge result.

## Kickoff prompt

Execute `docs/superpowers/plans/2026-09-06-se-supp-qualifiers.md` via
superpowers:subagent-driven-development on branch `se-supp-qualifiers`,
following that plan's unattended-overnight rules and this runbook.

## Session rules

- Full subagent-driven-development process: fresh implementer + task reviewer
  per task, final whole-branch review, ledger at `.superpowers/sdd/progress.md`,
  review packages per task, model the dispatches on the 2026-09-05 run.
- Byte-freedom accounting for this run (differs from last run — read the plan):
  MH.csv/VS.csv digests move (additive columns, prove additive); **MH.rds
  moves deliberately** (gains MHSPID); se/suppmh/suppvs.rds new; ALL other
  references byte-identical. A moved reference outside this list = STOP.
- Stop conditions: BLOCKED twice on the same task; byte-freedom violation;
  Needs-fixes verdict surviving two fix cycles; any decision the docs don't
  make. Never push, never re-freeze to pass a test outside the plan's freeze
  task, never skip a task review.

## Morning report contents

Task-vs-commit map, final review verdict, merge commit, suite/check/lint
evidence, carried findings appended to the ledger, anything that stopped the
run. Local main will be ahead of origin; pushing stays the user's call.

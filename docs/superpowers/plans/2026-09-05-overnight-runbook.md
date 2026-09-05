# Overnight Runbook — QS, then PE/EG

Date: 2026-09-05. This runbook is the contract for the unattended session.

## What was pre-approved

- Design docs (skim gate): `docs/superpowers/specs/2026-09-05-qs-domain-design.md` and `docs/superpowers/specs/2026-09-05-pe-eg-domains-design.md`. The user gives one "go" before resting; without it, the session does not start.
- End-state authority: **merge locally, never push.** After a green final whole-branch review, each feature branch is merged into local `main` with the suite re-verified on the merge result. `git push` is forbidden all night.

## How to run (the kickoff prompt)

Run two sequential subagent-driven sessions, in this order, waiting for the first to finish:

1. Execute `docs/superpowers/plans/2026-09-05-qs-domain.md` via superpowers:subagent-driven-development, on branch `qs-domain`, per that plan's unattended-overnight rules.
2. When QS is merged, execute `docs/superpowers/plans/2026-09-05-pe-eg-domains.md` the same way, on branch `pe-eg-domains`.

## Session rules (bind both runs)

- Subagent-driven-development process in full: fresh implementer per task, task reviewer per task, final whole-branch review per feature, the progress ledger at `.superpowers/sdd/progress.md`, and review packages per task.
- Stop conditions (write the state to the ledger + a session-final report, then stop):
  - Any implementer reports BLOCKED twice on the same task.
  - Any byte-freedom check fails (a digest or reference that must be unchanged has moved) — this indicates RNG leakage or an accidental rewrite; do not "fix" by re-freezing.
  - A reviewer verdict of Needs fixes that survives two fix cycles.
  - Anything that would require a design decision the docs do not make.
- Never: push, force-anything, re-freeze a digest/reference to make a failing test pass (re-freezing is only legitimate at the plan's explicit freeze task, after the byte-identity check), or skip a task review.
- The `.Rbuildignore` already excludes `.superpowers`/docs from the tarball; do not ship a release, bump versions, or touch DESCRIPTION/NEWS headings beyond what the plans' docs tasks say.

## Morning report (what the user wakes up to)

One summary covering: both features' status (task lists vs commits), final review verdicts, the merge commits on `main`, suite/check/lint evidence, any deferred findings carried to `.superpowers/sdd/progress.md`, and anything that stopped the run. Local `main` will be ahead of `origin/main`; pushing is the user's call.

## Deferred-findings ledger (context for reviewers)

`.superpowers/sdd/progress.md` carries the trial-design branch's deferred minors (no-day-0 triplication, README mapper illustration, validator hardening for hand-crafted input, test idioms). Out of scope tonight unless a plan task touches the same lines — then fix opportunistically and note it.

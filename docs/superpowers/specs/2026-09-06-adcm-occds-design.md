# Design: ADCM (OCCDS) — the concomitant-medication analysis dataset

Date: 2026-09-06
Status: Executed in parallel with the `se-supp-qualifiers` run, on branch
`adcm-occds` cut from `main` (worktree `.worktrees/adcm-occds`).

## Decisions made on your behalf

1. **Scope: one ADaM dataset, the ADAE pattern minus the SUPP pivot.**
   ADCM is the OCCDS structure for CM. There is no SUPPCM (the SE design
   deliberately deferred it), so `derive_adcm(cm, adsl)` takes two arguments
   and needs no qualifier anti-join guard — the ADAE shape without the
   merge-back.
2. **One analysis record per collected CM record; ASEQ = CMSEQ** (the ADAE
   precedent: the SDTM sequence is the analysis sequence, so the two cannot
   silently drift apart).
3. **Variables:** STUDYID, USUBJID, ASEQ, CMTRT, CMDECOD, CMINDC, CMDOSE,
   CMDOSU, CMDOSFRQ, CMROUTE, SAFFL, TRTSDT, TRTEDT, ASTDT, ASTDTF, AENDT,
   AENDTF, ASTDY, AENDY. CMSPID, the --DTC pair and the --STDY/--ENDY pair
   stay SDTM-only: analysis wants the imputed dates and the anchored days,
   not the sponsor ID or the raw precision. CMENRTPT/CMENRF stay SDTM-only
   too — an ongoing medication is visible in ADCM as a missing AENDT, the
   same way ADAE shows an ongoing event.
4. **No treatment-emergent flag.** No SAP defines a medication-level
   TRTEMFL, and this house does not invent rules it cannot argue with (the
   ADAE roxygen states the window precisely because a SAP does not). The
   OCCDS anchors SAFFL/TRTSDT/TRTEDT are carried per the ADAE convention —
   any windowing stays checkable in place. In this extract every dosed
   subject's medication starts before first dose, so ASTDY is negative on
   those rows and the negative-days path of `derive_dy_d()` is exercised by
   the pinned tests, not just by construction.
5. **Dates: the shared `impute_dtc()` first-of rule** (`""`/`"D"`/`"M"`
   flags). The seeded CM carries year-only ("2021"), month-only ("2024-02"),
   full-date and datetime precision — all four imputation paths are live in
   the data, so the pins cover them without synthetic inputs.
6. **`validate_adam()` grows honestly.** New signature
   `(adsl, adae, adcm, advs, adlb, dm, ds, ae, cm, vs, lb, suppae, spec)` —
   every call site updated, no NULL-default escape hatch: a silently skipped
   validation section is the drift this codebase exists to prevent. ADCM
   checks mirror ADAE's: `adcm-required-vars`, `adcm-key-not-unique`,
   `adcm-lost-record` / `adcm-extra-record` (coverage both ways against the
   SDTM CM keys), `adcm-imputation-flag-bad`, `adcm-astdy-wrong-anchor`,
   `adcm-study-day-zero`, `adcm-astdy-vs-sdtm-cmstdy` (full-precision
   starts, where ASTDY must reproduce CMSTDY), `adcm-aendt-before-astdt`.
7. **`build_all()` returns 5 ADaM datasets.** No define.xml change (the
   stub documents SDTM only); no spec change (OCCDS has no `spec$bds` rows,
   same as ADAE).

## Parallelism with the SE + SUPP run (merge notes)

Cut from `main`, the branch shares no file content with the SE work except
three deliberate, adjacent-region touchpoints, all trivially mergeable:

- `R/build-all.R` — this branch adds two lines in the ADaM block and the
  adam-count docstrings; the SE branch edits the SDTM mapping block.
- `NEWS.md`, `README.md` — additive bullets/wording.
- `tests/testthat/test-build-all.R` — this branch changes the adam
  expectations (5 datasets); the SE branch changes the sdtm expectations.

Everything else (`validate-adam.R`, `test-validators.R`, `adam-adcm.R`,
`test-adcm.R`, `tests/reference/adam/adcm.rds`) is untouched by the SE run.
The SE branch moves SDTM-layer references (`mh.rds`) — this branch touches
no existing reference; `adcm.rds` is the only new one and the other 26
references must stay byte-identical (verified after `update_reference.R`).

## Testing

`test-adcm.R`: pins probed from the actual extract before the assertions
were written — the full-precision row (3021-101-007 / CMSEQ 1 METFORMIN,
CMSTDTC 2021-08-29, ASTDY negative per the TRTSDT 2024-02-15 anchor), the
month-only row (3021-101-010 / CMSEQ 1, "2024-02" → ASTDT 2024-02-01,
ASTDTF "D"), the year-only row (3021-102-005 / CMSEQ 1, "2021" →
2021-01-01, ASTDTF "M"), the ongoing row (3021-101-022 / CMSEQ 2, missing
CMENDTC → missing AENDT/AENDTF). Row count = CM row count; key equality
with (USUBJID, CMSEQ); label check; expected ASTDY computed in-test from
the pinned dates via the stated day rule, never from the implementation.
Validator meta-tests in `test-validators.R`: corrupt an ADCM study day, a
date order, an imputation flag, drop a row — the right check trips.
`test-build-all.R`: adam dir has 5 rds + 5 xpt.

## Out of scope

TRTEMFL-style flags, ATC/WHO-DD classes (CMDECOD is the dict decode here),
daily-medication flags, SUPPCM, define.xml for ADaM.

## Run shape

Direct implementation on `adcm-occds` (no overnight contract): TDD per
commit, suite green, `R CMD check` + lint clean, references frozen via
`tests/update_reference.R`, never push, no merge — the branch is left
reviewable; merging is the user's call after the SE run lands.

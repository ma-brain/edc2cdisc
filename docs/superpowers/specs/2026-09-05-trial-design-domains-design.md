# Design: Trial design SDTM domains (TA, TE, TI, TV, TS)

Date: 2026-09-05
Status: Approved design, awaiting implementation plan

## Motivation

The package maps 14 subject-level SDTM domains (DM, EX, VS, AE, CM, DS, SV,
LB, MH, SUPPDM, SUPPAE, SUPPEX, CO, RELREC) plus four ADaM datasets. The
SDTMIG trial design family is the cheapest next tier: it is protocol fact,
not collected data, so it needs no new CRF forms in the generator, no new
extract shapes, and no mapper changes for existing studies. TS is required
in any real submission, and TV falls almost entirely out of the spec's
existing `visits` table.

## Domain set

The standard SDTMIG trial design family: **TS** (Trial Summary), **TA**
(Trial Arms), **TE** (Trial Elements), **TI** (Trial Inclusion/Exclusion
Criteria), **TV** (Trial Visits).

"TX" is not a standard SDTM domain. The original request listed TX; this was
corrected to TV during design (the SDTMIG trial design domains are TS, TA,
TE, TI, TV, plus TD, which stays out of scope here).

## Architecture (decided)

Dedicated spec tables + small spec-only builders (Approach 1). Rejected:
forcing trial design through the row-oriented `variables` mapping engine
(no CRF rows exist to iterate), and one generic long trial-design table
(loses per-domain shape validation, reads worse than one table per concept).

## New spec tables

All four are **optional** arguments to `new_study_spec()`, defaulting to
empty tibbles, so existing user specs keep constructing unchanged.

| Table | Columns | One row per |
|---|---|---|
| `elements` | ETCD (≤8 chars, unique), ELEMENT (≤40 chars), TESTRL, TEENRL, TEDUR (ISO 8601 duration string, e.g. `P2W`) | trial element |
| `ta` | ARMCD, ETCD, TAETORD (integer), EPOCH, TABRANCH, TATRANS | arm per element in planned order |
| `ie` | IETESTCD, IETEST, IECAT (INCLUSION / EXCLUSION) | criterion |
| `ts` | TSPARMCD, TSPARM, TSVAL, TSVALNF | trial summary parameter; exactly one of TSVAL / TSVALNF filled |

**TV has no new table.** It is a transform of the existing `visits` table:
VISITNUM, VISIT, EPOCH carried over; VISITDY is TargetDays under the
package's no-day-0 planned-day rule (`TargetDays >= 0` → `+1`), the same
convention `map_sv()` already applies, so a planned day of 0 never
appears and TV agrees with SV. "Two things read the spec" holds by
construction.

### Constructor validation (`new_study_spec()`)

- ETCD: unique, ≤8 characters; ELEMENT ≤40 characters (XPT v5 limits).
- `ta$ARMCD` ⊆ `arms$ARMCD`; `ta$ETCD` ⊆ `elements$ETCD`.
- `(ARMCD, TAETORD)` unique in `ta`.
- `ie$IECAT` ∈ {INCLUSION, EXCLUSION}.
- `ts$TSPARMCD` unique; exactly one of TSVAL / TSVALNF filled per row
  (ERROR when both or neither).
- `tedur` matches an ISO 8601 duration pattern (e.g. `^P(\d+[YMDW])+$`).

A spec naming trial design it cannot honour fails at construction, like
every other spec table.

### Shipped spec content (spec_synth01, spec_synth02)

- `elements`: SCRN (SCREENING, TEDUR `P2W`) and TREAT (TREATMENT,
  TEDUR `P12W`), with prose TESTRL/TEENRL rules.
- `ta`: 3 arms × 2 elements = 6 rows. Per SDTMIG semantics the transition
  rule sits on the element being left and the branch on the element the
  branch leads to: SCRN rows carry TATRANS "Randomised"; TREAT rows carry
  TABRANCH "Randomised to \<arm\>".
- `ie`: 6 criteria (3 INCLUSION, 3 EXCLUSION; the first inclusion is the
  age 18–100 rule mirroring `study$age_min`/`age_max`).
- `ts`: TITLE, PHASE, SPONSOR, INDIC, TRT, NARMS, PLANSUB (7 rows).

## Builders and data flow

Five builders in one new `R/trial-design.R` (they share no engine
machinery; five files would be five 40-line shells):

```
map_ts(spec), map_te(spec), map_ta(spec), map_ti(spec), map_tv(spec)
```

Spec-only signatures — no forms, no `refs` — because trial design is
protocol fact, not collected data. `map_ta()` joins ARM from `spec$arms`
(an IG-required TA variable no `ta` table row should repeat) and ELEMENT
from `spec$elements`. Each applies IG variable labels inline, like every
existing mapper (see `map-dm.R`).

`build_all()` maps them after RELREC and appends to the `sdtm` list in the
order `TA, TE, TI, TV, TS`, so they flow through validation and writing
like every other domain. No `--SEQ` variables (these domains have none).

## What deliberately does not change

`.sdtm_domains` in `spec.R` stays as-is: the mapping engine cannot drive
these domains (no CRF rows), so a `spec$variables` row naming TA/TE/TI/TV/TS
must keep failing loudly as "unbuildable domain" rather than silently
becoming legal.

## Validation (`validate_sdtm()`)

All new checks are conditional on the domain being present in `domains`,
matching the existing loop style.

**Required variables** via the existing `.sdtm_req_static` fallback (these
domains have no `spec$variables` rows):

- TA: STUDYID, DOMAIN, ARMCD, ARM, TAETORD, ETCD, ELEMENT, EPOCH
- TE: STUDYID, DOMAIN, ETCD, ELEMENT
- TI: STUDYID, DOMAIN, IETESTCD, IETEST, IECAT
- TV: STUDYID, DOMAIN, VISITNUM, VISIT, VISITDY, EPOCH
- TS: STUDYID, DOMAIN, TSPARMCD, TSPARM, TSVAL

**Recompute, don't trust** (spec-gated, ERROR on mismatch — the same
philosophy as the ADaM validators):

- `tv-vs-spec-visits`: TV's (VISITNUM, VISIT, VISITDY) must equal
  `spec$visits` exactly.
- `ts-narms`: a NARMS row's TSVAL must equal `nrow(spec$arms)`.
- `ts-plansub`: a PLANSUB row's TSVAL must equal `spec$study$n`.
- `ta-armcd`: every TA ARMCD exists in `spec$arms`.
- `ta-epoch`: every TA EPOCH appears in `spec$visits$EPOCH`.
- `sv-visit-not-planned`: every SV VISITNUM exists in TV (a scheduled visit
  that was never planned is a mapping bug).

**Internal integrity** (ERROR):

- `ta-etcd`: every TA ETCD exists in TE.
- `ta-dup-key` / `te-dup-etcd` / `ti-dup-testcd` / `ts-dup-parmcd`:
  key uniqueness.
- `ie-cat`: IECAT ∈ {INCLUSION, EXCLUSION}.
- `ts-valnf`: exactly one of TSVAL / TSVALNF filled per row — ERROR when
  both are filled and when neither is.

**WARN**: `trial-design-empty` — any of the five has 0 rows, so a user spec
that has not filled the new tables still builds but is told.

## define.xml

Add entries to the two per-domain lookup tables (`key_spec`,
`structure_spec`); the ItemGroupDef loop is already generic.

- Keys: TA (STUDYID, ARMCD, TAETORD), TE (STUDYID, ETCD),
  TI (STUDYID, IETESTCD), TV (STUDYID, VISITNUM), TS (STUDYID, TSPARMCD).
- Structures: "One record per arm per element" (TA), "One record per trial
  element" (TE), "One record per inclusion/exclusion criterion" (TI),
  "One record per planned visit" (TV), "One record per trial summary
  parameter" (TS).
- Origins: `var_origin()` is name-based, so only names unique to trial
  design go into the global `Protocol` list (TSPARMCD, TSPARM, TSVAL,
  TSVALNF, IETESTCD, IETEST, IECAT, ETCD, ELEMENT, TESTRL, TEENRL, TEDUR,
  TAETORD, TABRANCH, TATRANS) — a global ARMCD entry would
  mislabel DM's ARMCD. Names shared with other domains (ARMCD, ARM, EPOCH
  in TA; VISITNUM, VISIT in TV) get a small per-domain override map so TA/TV
  carry `Protocol` without touching what DM/SV declare. `VISITDY` stays on
  the existing `DY` suffix rule (`Derived`), which is what SV's VISITDY
  already gets.

## Write path

`write_sdtm()` needs no changes: its guards are generic and every new
variable name passes the 8-character XPT v5 limit (TSPARMCD, IETESTCD,
TAETORD, TABRANCH, TATRANS, VISITDY, TSVALNF, TESTRL, TEENRL, TEDUR).

## Testing

- New `tests/testthat/test-trial-design.R`: builder shapes from
  `spec_synth01` (TE = 2 rows, TA = 6, TI = 6, TV = 6, TS = 7), variable
  labels present, XPT-safe names, TV derived from `visits`.
- Meta-tests (corrupt-input style, like the existing validator tests):
  TA referencing a missing ETCD, NARMS disagreeing with `spec$arms`, TV
  drifting from `spec$visits`, SV carrying an unplanned visit — each must
  produce its ERROR.
- `test-build-all.R`: file and ItemGroupDef counts go 14 → 19.
- `test-synth02.R`: both shipped specs produce all five domains non-empty.
- Regression: the 14 frozen reference `.rds` files are untouched (the
  harness compares only files that exist in `tests/reference/`); the five
  new references are frozen deliberately via `update_reference.R`.

## Docs

- README: "14 SDTM domains" → 19, names updated (also the quick-start
  comment).
- NEWS.md entry.
- `new_study_spec()` roxygen: document the four tables and their checks.
- `new-study` and `design` vignettes: short trial-design passage — protocol
  facts live in the spec; TV is derived, not specified.
- `print.study_spec()`: show trial-design table counts.

## Out of scope (deliberately)

- TD (Trial Disease Assessment) — therapeutic-area domain, no use here.
- TD/TX naming: no sponsor-custom TX domain.
- SDTMIG 3.3 TS CT-annotation variables (TSVALCD, TSVCDREF, TSVCDAT).
- ARMCD on TV (visits differ by arm) — SYNTH01/SYNTH02 have one schedule.
- TAETORD/ETCD/ELEMENT links on TV.
- Engine (`variables` mapping) support for trial design domains.
- TI ↔ DM cross-subject conformance (e.g. checking collected ages against
  criteria) — criteria are protocol prose here, not structured ranges.

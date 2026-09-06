# Design: ADQS (BDS) — the questionnaire analysis dataset, with item-to-total scoring

Date: 2026-09-06
Status: Proposed — awaiting evening skim, then overnight execution

## Decisions made on your behalf

1. **`derive_adqs(qs, adsl, spec)` — the ADVS/ADEG BDS clone for the items,
   plus the package's first derived analysis parameter.** Items are one
   record per subject per visit per QS item (AVAL = QSSTRESN); the total is
   one record per subject per visit where **all items are present**. No
   NOT-DONE rows (dropped like ADVS/ADEG — a STAT row has nothing to
   analyse); a not-done visit therefore produces no items and no total.
2. **Scoring enters the spec, not the mapper.** A new optional spec table
   **`totals`** — `domain, paramcd, param, paramn, anrlo, anrhi, src_items`
   — declares each derived parameter: its code/name/order/range and the
   `src_items` it sums (a delimited string, e.g.
   `"MOS01;MOS02;MOS03;MOS04"`). `derive_adqs()` computes every `totals`
   row generically: sum the named items per (USUBJID, VISITNUM); emit only
   where **all** src items are present (the all-required rule — the strict,
   SAP-defensible default; proration is a rule this house does not invent).
   A future questionnaire adds spec rows, not code. The mapper errors
   loudly if a `totals` row names an item that has no `bds` row in the same
   domain (constructor-time, like every spec cross-check).
3. **Items carry no reference ranges — by design, not omission.** Ordinal
   0–3 mood items have no meaningful ANRIND; their `bds` rows declare
   `anrlo`/`anrhi` as NA and ANRIND stays missing (the WEIGHT/HEIGHT
   precedent). The **total** gets the declared range — ANRLO 4 / ANRHI 12
   for SYNTH01 (4 items × 0–3; below 4 = concerning), 4 / 15 for SYNTH02
   (5 items) — and the deterministic data classifies both LOW and NORMAL
   rows, so ANRIND is genuinely exercised with zero generator changes.
4. **The total's baseline is derived, not carried.** QS has no QSBLFL for a
   parameter that does not exist in SDTM. ADEL-style: after totals are
   computed per visit, the total row at the subject's item-baseline visit
   (the visit carrying item QSBLFL == "Y") gets ABLFL = "Y"; BASE/CHG/PCHG
   then flow through the shared rules for items and totals alike. Item
   ABLFL is carried from QSBLFL (the ADVS/ADEG convention).
5. **Parameters.** Items: PARAMCD = QSTESTCD (MOS01…), PARAM = QSTEST,
   PARAMN 1–4 from `bds` (SYNTH02: 1–5). Total: PARAMCD = "MOSTOT",
   PARAM = from the `totals` table ("MOOD SCALE Total"), PARAMN = 5
   (SYNTH02: 6). ADQS = items + totals, arranged USUBJID, PARAMN, AVISITN.
6. **`validate_adam()` grows once more**: `(adsl, adae, adcm, advs, adeg,
   adlb, adqs, dm, ds, ae, cm, vs, qs, eg, lb, suppae, spec)` — `adqs`
   after `adlb`, `qs` after `vs`. The `adqs-*` check set mirrors the ADVS
   block, plus two ADQS-specific recompute checks: **`adqs-total-wrong`**
   (recompute every total from the SDTM QS items — the recompute-don't-
   trust centrepiece) and **`adqs-total-coverage`** (a visit with all items
   present must have a total row; a not-done visit must not).
7. **No generator changes.** QS already emits every item on every performed
   visit with QSSTRESN standardized. **Byte accounting**: `adqs.rds` new;
   every other reference byte-identical; digests untouched. **Counts**:
   ADaM 6 → 7; SDTM stays 26; define.xml unchanged (the stub documents
   SDTM only).

## Constructor additions

`totals` is an optional `new_study_spec()` table with constructor checks:
`src_items` parses to a non-empty set; every named item is a `bds` row of
the same domain; `(domain, paramcd)` unique; ranges not both NA (a total
with no declared range cannot classify — refuse it rather than ship an
always-missing ANRIND).

## Validator (ADQS block, mirrored from ADVS + the two recomputes)

required-vars, key-not-unique, coverage, orphan-record, ablfl-multi,
ablfl-missing, ablfl-screenfail, ablfl-not-from-qs (items only),
base-wrong, base-screenfail, base-missing, chg-wrong, anrind-wrong,
ady-wrong, study-day-zero, param-not-in-spec — plus `adqs-total-wrong` and
`adqs-total-coverage` reading SDTM QS a second time.

## Out of scope

Prorated (k-of-n) totals; PARCAT1/PARCATN; treatment-emergent flags;
ADaM define.xml; half-items/normalized scores.

## Run shape

One branch (`adqs-bds`), 6 tasks: (1) spec tables + constructor checks,
(2) `derive_adqs()` + tests, (3) validator mirror + the two recomputes +
meta-tests, (4) wiring + counts + `adqs.rds` freeze + check/lint, (5) docs,
with the P2 docs-polish fold-ins (QS design-doc data-model bullet,
`.sdtm_req_static` listing — from the carried ledger). Same contract:
merge locally, never push.

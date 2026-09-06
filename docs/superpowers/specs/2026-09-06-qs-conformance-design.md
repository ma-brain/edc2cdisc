# Design: QS conformance completion — QSSTRESC / QSSTRESN / QSSTRESU

Date: 2026-09-06
Status: Proposed — awaiting skim, then execution

## Decisions made on your behalf

1. **Promote the mapper's internal `QSSTRESN` to an output column, and add
   `QSSTRESC` and `QSSTRESU`.** The numeric result already exists inside
   `map_qs()` (it drives the baseline rank); the conformance gap was that it
   never left the mapper. QSSTRESC = `as.character(QSSTRESN)` (the ADVS
   `VSSTRESC` idiom); QSSTRESU is an **explicit all-NA column** — ordinal
   scale items have no unit, and a present-but-empty `--STRESU` is more
   honest than omitting an Expected variable. NOT DONE rows carry NA across
   all three (blank result standardizes to nothing), exactly as the STAT
   semantics require.
2. **Column order**: `QSORRES, QSSTAT, QSREASND, QSSTRESC, QSSTRESN,
   QSSTRESU, QSBLFL, …` — standardized results after the reason pair, before
   the baseline flag.
3. **define.xml gets its QS ValueListDef** — the thing the original QS design
   wanted and Task 6 of that run had to skip. The `findings` list gains a QS
   entry (`distinct(QSTESTCD, QSTEST, QSSTRESU)`), and the description
   construction becomes unit-conditional (name only when the unit is blank —
   resurrecting the tweak the QS run dropped): `Sleep Quality` instead of
   `Sleep Quality (NA)`. VS/LB/EG rendering is untouched (their units are
   never blank). **ValueListDef count 3 → 4.**
4. **Validator honesty**: `.sdtm_req_static$QS` gains the three names, and a
   new `qstresn-not-numeric` check (recompute-don't-trust): on answered rows
   (QSORRES non-blank), QSSTRESN must be non-NA — a non-numeric collected
   value can never silently produce a missing standard result.
5. **References**: `qs.rds` re-frozen deliberately (three new columns);
   everything else byte-identical. This is also the SDTM groundwork ADQS
   stands on.
6. **Not a domain-count change** (QS already counted); build_all stays at
   26 SDTM / 6 ADaM.

## Doc amendments owed

- `2026-09-05-qs-domain-design.md`: define.xml section (VLM now exists, with
  the unit-conditional rule) and the out-of-scope list (QSSTRESC/QSSTRESN/
  QSSTRESU are in).
- `2026-09-05-pe-eg-domains-design.md`: ValueListDef arithmetic 3 → 4 (VS,
  LB, EG, QS).

## Run shape

Small run — one branch (`qs-conformance`), 4 tasks: (1) mapper columns +
tests, (2) validator + meta-tests, (3) define.xml VLM + counts, (4) qs.rds
re-freeze + docs. Same contract: merge locally, never push.

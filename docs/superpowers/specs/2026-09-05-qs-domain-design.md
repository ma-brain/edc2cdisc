# Design: QS (Questionnaires) domain

Date: 2026-09-05
Status: Proposed — awaiting evening skim, then overnight execution

## Decisions made on your behalf

Flagged because you were resting; each is reversible with one design-doc edit.

1. **One instrument, five items (4 in SYNTH01), wide form.** QSCAT "MOOD SCALE"
   with ordinal items MOS01–MOS04 (SYNTH02 adds MOS05), collected 0–3 as text,
   QSSTRESN numeric. Wide CRF fields (like VS), one row per item in SDTM
   (like VS/LB). No scoring/total scores in SDTM — totals are an ADaM concern
   (ADQS, out of scope).
2. **Zero RNG for the new forms.** Messiness is config-driven
   (`cfg$idx$qs_not_done` etc.), item values are deterministic index arithmetic.
   This is the load-bearing decision: consuming main-stream RNG draws would
   change every existing subject's data and move all 10 existing digest files;
   config-driven content keeps them byte-identical (provable, reviewed).
   Task 1 discovered `form_add()` itself draws SaveTs values from the main
   stream, so QS rows are emitted inside a per-subject private seeded stream
   (`apply_mh` contract, save/restore) — the zero-RNG property is byte-proven,
   the mechanism is a private stream.
3. **Scheduled event form on existing folders.** QS sits on the same visit
   folders as VS with the same `vdates`, so SV is untouched (min/max record
   dates per subject/folder cannot change).
4. **QS follows the map_vs pattern** (spec$tests pivot, NOT DONE rows kept,
   baseline rank block, derive_seq), not the engine. `check_ct` does not fire
   (no variables rows) — QS's collected values are numeric strings.
5. **No SUPPQS, no ADQS, no EG conversions** — YAGNI for this pass.
6. **Domain count 19 → 20.** build_all returns QS in addition to everything
   existing; references for the existing 19 must stay byte-identical.

## Data model

### CRF form `QS` ("Questionnaires", scheduled event form)

Fields (wide, per subject per visit): `QSDAT_*` (date parts), `QSPERF`,
`QSNRA` (not-done reason), `MOS01_RAW` … `MOS04_RAW` (SYNTH02: `+ MOS05_RAW`).

- Item values: `as.character(((subject_idx + visit_pos + item_no) %% 4))` —
  deterministic, no RNG.
- Not-done visit (config `cfg$idx$qs_not_done = c(7L, 4L)` = subject index 7,
  visit position 4): all item fields blank, `QSPERF = "0"`,
  `QSNRA = "Subject refused"`.
- Screen failures: SCRN rows only, same as VS (falls out of the shared
  `visit_dates()` behaviour).
- `recordposition` stays 0 (event-form rule).

### SDTM QS (pragmatic subset)

STUDYID, DOMAIN, USUBJID, QSSEQ, QSCAT, QSTESTCD, QSTEST, QSORRES, QSSTAT,
QSREASND, QSSTRESC, QSSTRESN, QSSTRESU, QSBLFL, VISITNUM, VISIT, QSDTC, QSDY.

- QSORRES from `MOS0x_RAW`; blank item → row dropped (map_vs idiom).
  Amended by the conformance completion: the standardized results are output
  columns too (QSSTRESC/QSSTRESN/QSSTRESU, after the QSSTAT/QSREASND pair).
  QSSTRESN began as the mapper-internal intermediate for the baseline rank;
  QSSTRESU is an explicit all-NA column — ordinal items have no unit.
- Not-done visit → one row per item with QSSTAT "NOT DONE", QSREASND populated,
  blank results.
- QSBLFL: the VS baseline-rank block over numeric results.
- QSSEQ via `derive_seq("QSSEQ", VISITNUM, QSTESTCD)`; QSDY via `derive_dy`.

### Spec wiring (both shipped specs)

- `forms`: `"QS", "event", TRUE`.
- `tests` rows: `domain=QS, field=MOS01.., testcd=MOS01.., test="Mood: …", cat="MOOD SCALE"`.
- `.sdtm_domains` in `spec.R` gains "QS" (it is engine-family buildable, like VS/LB).

## Validation

- `.sdtm_req_static$QS`: STUDYID, DOMAIN, USUBJID, QSSEQ, QSCAT, QSTESTCD,
  QSTEST, QSORRES, QSDTC.
- New checks: `qscat-not-in-spec` (spec-gated: QSCAT ⊆ spec$tests$cat for QS);
  `stat-reason` (QSSTAT "NOT DONE" ⇒ QSREASND non-blank and QSORRES blank —
  generic for the new findings domains); screen-failure DY-leak loop gains QS.
- Meta-tests: corrupt QSCAT → check fires; fake NOT DONE without reason → fires;
  QSSTAT NOT DONE with a result → fires.

## define.xml

key_spec `QS = c(STUDYID, USUBJID, QSSEQ)`; structure "One record per subject
per questionnaire item per visit". QS value-level metadata exists since the
conformance completion: one value-level ItemDef per item on QSSTRESN, with
unit-conditional descriptions (ordinal items carry no unit, so the
description is the bare test name). VS/LB/EG descriptions are unchanged —
their units are never blank.

## Testing

- Digest fixture: 10 existing entries byte-identical; fixture regenerated to
  11 files (QS.csv added). Procedure documented in the plan (manual re-freeze;
  no committed script exists — the digests header comment is the contract).
- Regression: existing 19 reference `.rds` byte-identical; one new `qs.rds`.
- Mapper tests pin: per-(subject,visit) item completeness (4 rows vs the VS
  key set), the seeded not-done subject/visit, baseline flag on numeric items
  only, screen-failure SCRN rows.
- Meta-tests per above; build-all counts updated.

## Out of scope

ADQS; QS scoring/total scores; SUPPQS; item-level NOT DONE; EG/PE
(covered by their own design); visit-level `QSPERF` beyond the one seeded
not-done visit.

# Design: ADEG (BDS) — the ECG analysis dataset

Date: 2026-09-06
Status: Proposed — awaiting evening skim, then overnight execution

## Decisions made on your behalf

1. **`derive_adeg(eg, adsl, spec)` is a `derive_advs()` clone** — same BDS
   tail (ABLFL carried from SDTM's EGBLFL, BASE from the baseline record,
   CHG/PCHG via the shared rules, ANRIND via `.rule_anrind` against declared
   ranges), same NOT-DONE drop (SDTM keeps the STAT row; BDS has no result
   to analyse), same PARAM construction with the unit in parentheses.
   `R/adam-advs.R` is the template; `R/adam-adlb.R` shows the
   ranges-arrive-otherwise variant ADEG does not need.
2. **`spec$bds` gains five ADEG rows** (SAP stand-in ranges, msec):
   PR 120–200, QRS 60–100, QT 350–450, QTCF 350–450, RR 600–1200; PARAMN
   1–5. Plausible clinical ranges — which means the deterministic values
   (PR 120–136, QRS 90–98, QT 380–398, QTCF 400–415, RR 900–1000) would all
   classify NORMAL.
3. **One config-driven HIGH seed** so ANRIND actually classifies:
   `cfg$idx$eg_qt_high = c(<idx>, <vpos>)` overrides that visit's
   `QT_RAW = "520"` / `QTCF_RAW = "537"` (mirroring LB's `force_high`
   precedent). This changes EG's SDTM output on that row → **eg.rds is
   re-frozen deliberately** (diff must be exactly the seeded row's
   QT/QTCF values) and its digest moves (a value change, not column-additive
   — the proof is a diff confined to the seeded row). ADEG then carries
   ANRIND = HIGH on two parameters for that subject-visit.
4. **The reduced-precision EGDTC row is a feature**: `dtc_date()` handles
   date-only DTCs, so the seeded late-time row yields ADT without time and
   the pins cover it.
5. **No TRTEMFL** (design decision inherited from ADCM: nothing to invent),
   no ATC-class-style extras, no SUPP interaction. `build_all()` returns
   **6 ADaM datasets**; SDTM stays at 26.
6. **`validate_adam()` grows once more**: signature
   `(adsl, adae, adcm, advs, adeg, adlb, dm, ds, ae, cm, vs, eg, lb,
   suppae, spec)` — `adeg` and `eg` added, every call site updated, no
   NULL-default escape. The ADEG check set mirrors the ADVS set
   (`adeg-required-vars`, `adeg-key-not-unique`, `adeg-coverage`,
   `adeg-orphan-record`, `adeg-ablfl-multi/missing/screenfail/
   not-from-eg`, `adeg-base-wrong/screenfail/missing`, `adeg-chg-wrong`,
   `adeg-range-spec-drift`, `adeg-anrind-wrong`, `adeg-ady-wrong`,
   `adeg-study-day-zero`, `adeg-param-not-in-spec`).

## Out of scope

QTcB/other corrections (QTCF is collected, not derived), treatment-emergent
windows, ADaM define.xml, SUPP interaction.

## Run shape

One branch (`adeg-bds`), ~6 tasks: (1) generator HIGH seed, (2) spec$bds
rows + digest/eg.rds re-freeze, (3) `derive_adeg()` + tests, (4) validator
mirror + meta-tests, (5) wiring + counts + adeg.rds freeze + check/lint,
(6) docs. Same contract as previous runs: merge locally, never push.

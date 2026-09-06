# Design: PE (Physical Examination) and EG (ECG) domains, plus SUPPPE

Date: 2026-09-05
Status: Proposed — awaiting evening skim, then overnight execution

## Decisions made on your behalf

1. **Same architecture as QS** (its design doc is a sibling): scheduled event
   forms on existing folders, zero RNG (config-driven messiness, deterministic
   index-arithmetic values), map_vs-pattern mappers over `spec$tests`, existing
   digests/references byte-identical. Read that doc first; this one covers only
   what differs.
2. **PE = body-system findings.** Four systems (SYNTH01): CV, RESP, ABDO,
   NEURO; PEORRES = NORMAL/ABNORMAL (collected via a `PEFIND` decode codelist),
   PECLSIG = "N"/"Y" mirroring PEORRES. One seeded abnormal finding
   (`cfg$idx$pe_abnormal = c(3L, 2L)` — subject 3, BASE, cardiovascular) with a
   free-text "specify" field `CVSPEC`, delivered as **SUPPPE** (qnam PEABNDT,
   idvar PESEQ) — this is the first SUPP on a findings domain and proves the
   join pattern generalises beyond the AE/EX precedents.
3. **EG = interval measurements only.** PR, QRS, QT, QTCF, RRI (RR interval) in
   msec — no overall-interpretation test (avoids inventing CT), no unit
   conversions (ECG machines report msec everywhere; the imperial-site messiness
   does not apply), no ranges in SDTM. Messiness: one seeded not-done ECG
   (`cfg$idx$eg_not_done = c(9L, 5L)`, reason "Equipment failure") and one
   seeded missing EGTIM (`cfg$idx$eg_late_time = c(2L, 3L)`) so EGDTC has
   reduced precision on that row.
4. **No PE baseline flag.** QSBLFL/EGBLFL exist (the VS rank block); PE has no
   numeric result to rank and clinically meaningless as a baseline flag —
   PEBLFL is deliberately omitted (PE carries no --BLFL in common practice).
5. **Domain count 20 → 23** (PE, EG, SUPPPE after QS's 20). Existing 19
   references remain byte-identical; SUPPPE follows SUPPDM/AE/EX into the
   related-records family.

## Data model

### PE CRF form ("Physical Exam", scheduled)

Fields: `PEDAT_*` (date parts), `PEPERF`, `PENRA`, `CARDIO_RAW` + `CARDIO_DECODE`
(via `coded_cols`, codelist `PEFIND`: Normal/Abnormal — raw kept as the
"(as entered)" value), same for `RESPIR_*`, `ABDO_*`, `NEURO_*` (SYNTH02 adds
`SKIN_*`), and `CVSPEC` (free text, populated only on the seeded abnormal row).

### PE SDTM (pragmatic subset)

STUDYID, DOMAIN, USUBJID, PESEQ, PETESTCD (CV/RESP/ABDO/NEURO[/SKIN]), PETEST
("Cardiovascular"/…), PEORRES (NORMAL/ABNORMAL), PECLSIG (N/Y), PESTAT,
PEREASND, VISITNUM, VISIT, PEDTC, PEDY. Not-done visit ⇒ one row per system
with PESTAT "NOT DONE" + PEREASND, blank results.

### EG CRF form ("ECG", scheduled)

Fields: `EGDAT_*`, `EGTIM`, `EGPERF`, `EGNRA`, `PR_RAW`, `QRS_RAW`, `QT_RAW`,
`QTCF_RAW`, `RRI_RAW` (numeric text, msec). Values are deterministic index
arithmetic around realistic midpoints (e.g. QT = 380 + …), no RNG.

### EG SDTM (pragmatic subset)

STUDYID, DOMAIN, USUBJID, EGSEQ, EGTESTCD, EGTEST, EGORRES (character),
EGSTRESN (numeric), EGSTRESU ("msec" on every row), EGSTAT, EGREASND, EGBLFL,
VISITNUM, VISIT, EGDTC (with time where EGTIM present), EGDY.

### Spec wiring (both shipped specs)

`forms` rows (event, scheduled); `tests` rows for PE (cat/specimen NA) and EG
(cat "SINGLE ECG" as EGCAT? — no: EGCAT omitted from the subset, tests `cat`
stays NA); `.sdtm_domains` gains "PE" and "EG". SYNTH02 differences: extra PE
system (SKIN) — QS's extra item is that spec's parallel.

### SUPPPE

`spec$supp` row: rdomain "PE", idvar "PESEQ", qnam "PEABNDT", qlabel
"Abnormality Details", src "CVSPEC", transform "squish", qorig "CRF". New
`map_supppe(forms$PE, pe, spec)` joining raw rows to built PE on
(USUBJID, VISITNUM, PETESTCD == "CV", PEORRES == "ABNORMAL") to recover PESEQ,
with the map_suppae-style row-count guard.

## Validation

- `.sdtm_req_static`: PE (…, PESEQ, PETESTCD, PETEST, PEORRES, PEDTC), EG
  (…, EGSEQ, EGTESTCD, EGTEST, EGORRES, EGSTRESN, EGSTRESU, EGDTC), SUPPPE
  (the standard SUPP column set).
- New checks: `peorres-bad-value` (NORMAL/ABNORMAL, LBNRIND-style hardcoded CT);
  `peclsig-coherence` (PECLSIG "Y" ⇔ PEORRES "ABNORMAL"); `egstresu-fixed`
  ("msec" whenever EGSTRESN is non-missing); `stat-reason` coherence extended
  to PE/EG (the QS design's generic check); screen-failure DY-leak loop gains
  PE and EG; SUPPPE rides the existing related-records loop via the
  `.related` vector (SUPPPE = "PE").
- Meta-tests for each; SUPPPE orphan/dup coverage via the existing
  related-records meta-test patterns.

## define.xml

Keys (PE: STUDYID, USUBJID, PESEQ; EG: STUDYID, USUBJID, EGSEQ; SUPPPE: the
SUPP key set); structures ("One record per subject per body system per visit",
"One record per subject per ECG test per visit", SUPP structure pattern);
ValueListDef on EGSTRESN (PE has no --STRESN; the PEORRES codelist is curated
via `codelist_vars` + "PEORRES"). Counts after both plans: ItemGroupDef 23,
ValueListDef 3 (VS, LB, EG), CodeList 11 (PEORRES). Amended from the draft's
"ValueListDef 4 (QS, EG)": a ValueListDef annotates a --STRESN column, so only
VS/LB/EG carry one — the QS amendment skipped that domain's VLM for want of
QSSTRESN, and PE reports character PEORRES, leaving EG as the only new entry.

## Testing

Identical strategy to the QS design: digest fixture grows by PE.csv/EG.csv with
all prior entries byte-identical; regression references grow by `pe.rds`,
`eg.rds`, `supppe.rds` with all prior files byte-identical; mapper tests pin
per-(subject,visit) completeness against the VS key set, the seeded abnormal /
not-done / missing-time rows (all config indices), screen-failure SCRN rows;
meta-tests for every new check; build-all counts updated.

## Out of scope

PE location/severity detail variables, EG overall interpretation, ECG vendor
extracts, unit conversions for EG, ADEG/ADPE, extra PEFIND values beyond
Normal/Abnormal.

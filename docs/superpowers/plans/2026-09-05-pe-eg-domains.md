# PE + EG (+ SUPPPE) Domains Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Unattended-overnight rules apply: never push, commit per task, full suite green at every commit, stop cleanly on BLOCKED. **Prerequisite:** the QS plan (`2026-09-05-qs-domain.md`) must be merged to `main` first — this plan reuses its patterns (deterministic forms, stat-reason check, VLM unit tweak) and its global-constraints discipline.

**Goal:** Add SDTM PE (body-system exam), EG (ECG intervals) and SUPPPE (the seeded abnormality detail), with generator forms, mappers, validation, define.xml, frozen references and docs.

**Architecture:** Same findings architecture as QS/VS/LB (spec$tests pivot, map_vs pattern, config-driven deterministic content, zero RNG). SUPPPE generalises the SUPP mappers to a findings domain via a (USUBJID, VISITNUM, PETESTCD) → PESEQ join.

**Tech Stack:** unchanged. **Spec:** `docs/superpowers/specs/2026-09-05-pe-eg-domains-design.md`.

## Global Constraints

Same as the QS plan (zero RNG, byte-frozen existing artifacts, XPT limits, feature branch `pe-eg-domains`, suite stays at its current all-green count). After this plan: **23 SDTM domains**; ItemGroupDef 23; ValueListDef 3 (VS, LB, EG; the plan's original 4 miscounted — QS and PE have no --STRESN output column); CodeList 11.

## File Structure

As QS plus: `R/map-pe.R`, `R/map-eg.R`, `R/map-supp.R` (fourth mapper), `tests/testthat/test-pe-eg.R` (new).

---

### Task 1: PE and EG CRF forms in the generator

**Files:** `R/generate-rave-extract.R`, `R/generator-configs.R`.

- [ ] Config: `idx$pe_not_done = c(12L, 2L)`, `idx$pe_abnormal = c(3L, 2L)`, `idx$eg_not_done = c(9L, 5L)`, `idx$eg_late_time = c(2L, 3L)` per study (verify each chosen subject has the visit position; SF indices are `c(5L, 17L)`; adjust and record). `pe_systems`: SYNTH01 `c(CARDIO, RESPIR, ABDO, NEURO)`, SYNTH02 adds `SKIN`. `eg_tests`: both `c(PR, QRS, QT, QTCF, RRI)`.
- [ ] `.FORM_ID_OFFSET`: `PE = 91000L`, `EG = 93000L`. `cfg$fields$PE`: `c("PEDAT_YYYY","PEDAT_MM","PEDAT_DD","PEPERF","PENRA", <per system: str_c(sys, c("_RAW","_DECODE"))>, "CVSPEC")`; `cfg$fields$EG`: `c("EGDAT_YYYY","EGDAT_MM","EGDAT_DD","EGTIM","EGPERF","EGNRA","PR_RAW","QRS_RAW","QT_RAW","QTCF_RAW","RRI_RAW")`.
- [ ] `.CODELISTS` += `PEFIND = c(Normal = "NORMAL", Abnormal = "ABNORMAL")` (check `.CODELISTS` structure — named decode vector per OID, lines ~130–158); `.CODELIST_OF`: the five PE system `_DECODE` columns → `PEFIND`. `.FIELD_LABELS`: `PEPERF "Exam Performed"`, `PENRA "Reason Not Performed"`, `CARDIO "Cardiovascular"`, …, `CVSPEC "Specify Abnormality"`, `EGTIM "ECG Time"`, `PR "PR Interval"`, `QRS "QRS Duration"`, `QT "QT Interval"`, `QTCF "QTcF Interval"`, `RRI "RR Interval"`. `.FLOAT_FIELDS`: the `*_RAW` EG names.
- [ ] Populate blocks after the QS block (same zero-RNG discipline, `vpos <- match(folder, cfg$visit_folders)`):

```r
    # ---- PE (physical exam) -------------------------------------------------
    pe_perf  <- !(idx == cfg$idx$pe_not_done[1] && vpos == cfg$idx$pe_not_done[2])
    abnormal <- idx == cfg$idx$pe_abnormal[1] && vpos == cfg$idx$pe_abnormal[2]
    pe_fields <- list(PEPERF = if (pe_perf) "1" else "0", CVSPEC = "")
    if (!pe_perf) pe_fields$PENRA <- "Subject unwell"
    for (sys in cfg$pe_systems) {
      finding <- if (pe_perf && abnormal && sys == "CARDIO") "Abnormal" else "Normal"
      pe_fields <- modifyList(pe_fields, coded_cols(cfg, sys, toupper(sys), finding))
    }
    if (pe_perf && abnormal) pe_fields$CVSPEC <- "Systolic murmur"
    form_add(pe, sub, folder, vdate, pe_fields)

    # ---- EG (ECG) -----------------------------------------------------------
    eg_perf <- !(idx == cfg$idx$eg_not_done[1] && vpos == cfg$idx$eg_not_done[2])
    eg_fields <- list(EGPERF = if (eg_perf) "1" else "0",
                      EGTIM = if (idx == cfg$idx$eg_late_time[1] &&
                                    vpos == cfg$idx$eg_late_time[2]) "" else "09:30")
    if (!eg_perf) eg_fields$EGNRA <- "Equipment failure"
    if (eg_perf) {
      base <- idx + vpos
      eg_fields <- modifyList(eg_fields, list(
        PR_RAW = as.character(120 + (base %% 5L) * 4L),
        QRS_RAW = as.character(90 + (base %% 3L) * 4L),
        QT_RAW = as.character(380 + (base %% 4L) * 6L),
        QTCF_RAW = as.character(400 + (base %% 4L) * 5L),
        RRI_RAW = as.character(900 + (base %% 6L) * 20L)
      ))
    }
    form_add(eg, sub, folder, vdate, eg_fields)
```

Check `coded_cols(cfg, prefix, codelist, value)` argument order against its definition (lines 221–228) and adapt. Verify: 13 extract files; **all 11 prior digests unchanged**; commit `feat(generator): deterministic PE and EG forms`.

### Task 2: Reader, spec wiring, digest re-freeze

- [ ] `form_oids` += `"PE", "EG"`; `.sdtm_domains` += `"PE", "EG"`; `spec$forms` rows for both; `spec$tests` rows — PE: `("PE","CARDIO","CV","Cardiovascular",NA,NA)`, `("PE","RESPIR","RESP","Respiratory",NA,NA)`, `("PE","ABDO","ABDO","Abdomen",NA,NA)`, `("PE","NEURO","NEURO","Neurological",NA,NA)` (+ SKIN row SYNTH02); EG: `("EG","PR","PR","PR Interval",NA,NA)`, QRS/QT/QTCF/RRI likewise. `spec$supp` row (both specs): `"PE","PESEQ","PEABNDT","Abnormality Details","CVSPEC","squish","CRF",NA`.
- [ ] Re-freeze digests (13 files, 11 unchanged); tests green; commit `feat(spec): PE and EG in reader, spec tables, digests`.

### Task 3: `map_pe()` and `map_eg()`

**Files:** Create `R/map-pe.R`, `R/map-eg.R`, `tests/testthat/test-pe-eg.R`; `R/globals.R`.

**Interfaces:** `map_pe(pe, spec, refs)`: STUDYID, DOMAIN, USUBJID, PESEQ, PETESTCD, PETEST, PEORRES, PECLSIG, PESTAT, PEREASND, VISITNUM, VISIT, PEDTC, PEDY. `map_eg(eg, spec, refs)`: STUDYID, DOMAIN, USUBJID, EGSEQ, EGTESTCD, EGTEST, EGORRES, EGSTRESN, EGSTRESU, EGSTAT, EGREASND, EGBLFL, VISITNUM, VISIT, EGDTC, EGDY.

- [ ] Failing tests first: per-key completeness (4 PE rows / 5 EG rows per VS key set); the seeded abnormal row (PETESTCD "CV", PEORRES "ABNORMAL", PECLSIG "Y", exactly one, with a matching SUPPPE row once Task 5 lands); not-done visits produce STAT rows with reasons and blank results; EG values parse and EGDTC is reduced-precision (no time) on the seeded row; EGBLFL on numeric rows at/before RFSTDTC (VS rank block); screen failures SCRN-only, PEDY/EGDY NA.
- [ ] `map_pe` pivot reads **`_DECODE`** (`PEORRES = col_or_na(pe, str_c(field, "_DECODE"))`; the `_RAW` column is the as-entered twin and is not mapped), then `PEORRES = str_to_upper(PEORRES)` (VSPOS idiom, `map-vs.R:75`), `PECLSIG = if_else(PEORRES == "ABNORMAL", "Y", "N")`; not-done rows mirror QS's STAT block (`PESTAT`, `PEREASND = PENRA`). No baseline flag (design decision 4). `derive_seq("PESEQ", VISITNUM, PETESTCD)`.
- [ ] `map_eg` pivot reads `_RAW`; `EGSTRESN = suppressWarnings(as.numeric(EGORRES))`, `EGSTRESU = if_else(is.na(EGSTRESN), NA_character_, "msec")`; `EGDTC = rave_dtc(EGDAT_YYYY, EGDAT_MM, EGDAT_DD, time = EGTIM)`; EGBLFL via the VS rank block; `derive_seq("EGSEQ", VISITNUM, EGTESTCD)`.
- [ ] globals.R additions (PETESTCD, PETEST, PEORRES, PECLSIG, PESTAT, PEREASND, PEDAT_*, EGTIM, EGSTAT, EGREASND, EGNRA, EGPERF, EGDAT_*, EG raw names…). Tests green + full suite; commit `feat(pe-eg): map_pe() and map_eg()`.

### Task 4: build_all wiring + counts

- [ ] `pe`, `eg` mapped after `qs`; sdtm list order `…, MH, QS, PE, EG, SUPPDM, …`; counts: rds set + pe/eg (22), xpt 22, ItemGroupDef 22; cross-study assertions (SYNTH02 PE has 5 systems per key, EG 5 tests). Commit `feat(build): wire PE and EG into build_all()`.

### Task 5: SUPPPE

**Files:** `R/map-supp.R`, `R/build-all.R`, `tests/testthat/test-pe-eg.R`, counts tests.

- [ ] Failing test: exactly one SUPPPE row (the seeded abnormality), QNAM "PEABNDT", QVAL "Systolic murmur", IDVAR "PESEQ", IDVARVAL = the CV row's PESEQ, QORIG "CRF".
- [ ] `map_supppe(forms$PE, pe, spec)` following `map_suppae` (`R/map-supp.R:94-114`): parent = `supp_parent(forms$PE, spec, "PE")` joined to built PE on the raw row's `usubjid` + Folder→VISITNUM (join `spec$visits`) + `PETESTCD == "CV"` + `PEORRES == "ABNORMAL"` to recover PESEQ; hard `nrow(parent) != nrow(pe_abnormal_rows)` guard (map_suppae lines 101–106); `make_supp(parent, rdomain = "PE", idvar = "PESEQ", qnams = supp_qnams(spec, "PE"))`. Wire into `build_all` after `suppex` (sdtm list + rds set + xpt 23 + ItemGroupDef 23). Also add `SUPPPE = "PE"` to the `.related` vector in `validate-sdtm.R` (Task 6) — do it here so the wiring task's suite run is meaningful.
- [ ] Green; commit `feat(supp): SUPPPE for the PE abnormality detail`.

### Task 6: Validation

- [ ] `.sdtm_req_static`: PE / EG per the design; SUPPPE standard SUPP set. New checks (guarded, before `report`): `peorres-bad-value` (NORMAL/ABNORMAL); `peclsig-coherence` (Y ⇔ ABNORMAL); `egstresu-fixed` ("msec" whenever EGSTRESN non-missing); extend the QS `stat-reason` check to PE (PESTAT/PEREASND/PEORRES) and EG (EGSTAT/EGREASND/EGORRES); screen-fail DY-leak list += "PE","EG"; `.related` vector gains `SUPPPE = "PE"` if not already (the related-records loop then covers parent orphans, IDVAR consistency, and --SEQ keys automatically — verify the loop's `parent_keys` path uses PESEQ correctly, it reads `IDVAR` from the data).
- [ ] Meta-tests: PEORRES "MAYBE" → fires; PECLSIG flipped → fires; EGSTRESU "usec" → fires; PESTAT NOT DONE reason-blanked → fires; SUPPPE orphan (USUBJID not in PE) → related-parent-orphan. Clean build zero findings. Commit `feat(validate): PE, EG and SUPPPE checks`.

### Task 7: define.xml

- [ ] `key_spec`: PE `c("STUDYID","USUBJID","PESEQ")`, EG `c("STUDYID","USUBJID","EGSEQ")`, SUPPPE `c("STUDYID","RDOMAIN","USUBJID","IDVAR","IDVARVAL","QNAM")`; `structure_spec`: "One record per subject per body system per visit" / "One record per subject per ECG test per visit" / SUPP pattern. VLM: `eg_params` + findings entry for EGSTRESN (unit "msec" — exercises the QS unit-conditional path's else-branch); `codelist_vars` += "PEORRES". Counts: ItemGroupDef 23, ValueListDef 4, CodeList 11. Commit `feat(define-xml): document PE, EG and SUPPPE`.

### Task 8: Freeze, document, check, lint

- [ ] `update_reference.R` → 23 sdtm; `git status --porcelain tests/reference` → exactly `pe.rds`, `eg.rds`, `supppe.rds` new, zero modified. `document()`, full suite FAIL 0 WARN 0, check clean, lint clean. Commit `test: freeze the PE, EG and SUPPPE reference outputs`.

### Task 9: Docs

- [ ] README 20 → 23 (names + quick-start); NEWS section extended under the same `0.4.0.9000` heading (PE/EG/SUPPPE bullets); vignettes: tests/forms passages mention PE/EG; design vignette findings-family prose. Commit `docs: PE, EG and SUPPPE domains`.

---

## Self-review

Both design-doc sections map to tasks; SUPPPE rides the existing related-records loop (Task 5 wiring + Task 6 `.related`), the four counts (23/23/4/11) are consistent across Tasks 4–7, and every check name in Task 6 has a meta-test.

# QS Conformance Completion Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Unattended-overnight rules: never push, commit per task, suite green (the planned qs.rds re-freeze window called out), stop cleanly on BLOCKED. Branch: `qs-conformance`.

**Goal:** Complete the QS domain's SDTM conformance: output QSSTRESC, QSSTRESN and QSSTRESU from `map_qs()`, add the `qstresn-not-numeric` validator check, and give define.xml its QS ValueListDef — closing the P21 gap both QS-run reviews documented.

**Architecture:** The numeric result already exists inside `map_qs()` as the baseline-rank intermediate; this plan promotes it to the interface and adds the character/unit companions. define.xml's findings loop then applies to QS with a unit-conditional description (QS items have no unit).

**Tech Stack:** unchanged. **Spec:** `docs/superpowers/specs/2026-09-06-qs-conformance-design.md`.

## Global Constraints

- **Byte-freedom accounting**: `qs.rds` re-frozen deliberately (three new columns); **every other reference byte-identical**; digests untouched (no generator change).
- Suite baseline 777 pass / 0 warnings; ends ≥ 790 / 0 warnings.
- Column order: `…, QSORRES, QSSTAT, QSREASND, QSSTRESC, QSSTRESN, QSSTRESU, QSBLFL, …` (18 columns total).
- NOT DONE rows: QSORRES blank, QSSTRESC/QSSTRESN/QSSTRESU all NA.
- Labels ≤ 40: QSSTRESC "Character Result/Finding in Std Units", QSSTRESN "Numeric Result/Finding in Std Units", QSSTRESU "Standard Units".

## File Structure

- `R/map-qs.R` — select/order + QSSTRESC/QSSTRESU.
- `R/validate-sdtm.R` — `.sdtm_req_static$QS` + `qstresn-not-numeric`.
- `R/define-xml.R` — `qs_params` + findings entry + unit-conditional description.
- `tests/testthat/test-qs.R` (column pins, meta-tests, ValueListDef count), `tests/testthat/test-build-all.R` (ValueListDef count if asserted there).
- `tests/reference/sdtm/qs.rds` (re-frozen); design-doc amendments in `docs/superpowers/specs/`.

---

### Task 1: Mapper columns

**Files:** `R/map-qs.R`, `tests/testthat/test-qs.R`.

- [ ] TDD: update the interface-order pin (15 → 18 names) and add failing expectations: every answered row has non-NA QSSTRESN and QSSTRESC = as.character(QSSTRESN); QSSTRESU is NA everywhere (ordinal items carry no unit); NOT DONE rows have all three NA. Red → implement → green.
- [ ] Implement: in the final `select()`, insert after `QSREASND`: `QSSTRESC = as.character(QSSTRESN), QSSTRESN, QSSTRESU = NA_character_`. (QSSTRESN already exists on both branches of the bind: answered parses, not_done is `NA_real_`.) Add the three labels after QSREASND's.
- [ ] Full suite: **expected red** — test-qs.R's ValueListDef-related assertions and anything pinning the 15-column interface stay green, but the regression `qs.rds` compare fails (three new columns). **That failure is Task 4's freeze window.** Report the exact failure list.
- [ ] Commit: `feat(qs): QSSTRESC, QSSTRESN and QSSTRESU in the interface`.

### Task 2: Validator

**Files:** `R/validate-sdtm.R`, `tests/testthat/test-qs.R`.

- [ ] `.sdtm_req_static$QS` += "QSSTRESC", "QSSTRESN", "QSSTRESU" (after QSREASND).
- [ ] New check `qstresn-not-numeric` (guarded on QS present/non-empty and all of QSORRES/QSSTAT/QSSTRESN present, matching the stat-reason tuple style): on rows where QSORRES is non-blank AND QSSTAT is not "NOT DONE", QSSTRESN must be non-NA → else ERROR with count + example. Comment voice: the collected answer must standardize or the record lies about itself.
- [ ] Meta-tests: corrupt a QSSTRESN to NA on an answered row → fires; clean build zero findings. TDD.
- [ ] Full suite green **except** the known qs.rds window; lint clean; document() if roxygen touched (check enumeration).
- [ ] Commit: `feat(validate): the QS standardization contract`.

### Task 3: define.xml QS ValueListDef

**Files:** `R/define-xml.R`, `tests/testthat/test-build-all.R`, `tests/testthat/test-qs.R` (its ValueListDef assertion).

- [ ] `qs_params <- domains$QS |> distinct(QSTESTCD, QSTEST, QSSTRESU) |> arrange(QSTESTCD)`; findings entry: domain "QS", var "QSSTRESN", codevar "QSTESTCD", namevar "QSTEST", unitvar "QSSTRESU".
- [ ] **Unit-conditional description** (resurrected from the QS design): build `desc <- p[[f$namevar]][1]` then append `" (unit)"` only when the unit is non-blank — for ALL findings domains (VS/LB/EG units are never blank, so their rendered text is unchanged; QS gets bare names). Verify in the built XML: VS/LB/EG descriptions unchanged, QS descriptions have no "NA".
- [ ] TDD: ValueListDef assertions 3 → 4 (grep every assertion — test-qs.R:83 has one, test-build-all.R has one) red → implement → green. Also assert a QS description equals the bare test name and `IT.QS.QSSTRESN` has a hooked `def:ValueListRef`.
- [ ] Full suite green except qs.rds window; lint. Commit: `feat(define-xml): QS value-level metadata`.

### Task 4: Freeze + docs

**Files:** `tests/reference/sdtm/qs.rds`, `README.md` (only if a stale claim exists — grep; no counts change), `NEWS.md`, both design docs.

- [ ] `Rscript tests/update_reference.R` → 26 sdtm / 6 adam; `git status --porcelain tests/reference` → exactly `M qs.rds`, zero others. **Else STOP.**
- [ ] Full suite FAIL 0 WARN 0; `R CMD build && R CMD check` Status: OK; lint clean.
- [ ] NEWS: bullet under `0.4.5.9000`:

```markdown
* QS conformance completion: `map_qs()` now outputs QSSTRESC, QSSTRESN and
  QSSTRESU (blank for ordinal items), closing the subset gap the QS run
  documented; define.xml gains the QS value-level metadata with
  unit-conditional descriptions; `validate_sdtm()` gains
  `qstresn-not-numeric`.
```

- [ ] Design-doc amendments: QS design doc define.xml section ("No value-level metadata…" replaced by the VLM-now-exists + unit-conditional rule) and out-of-scope list (drop the three variables); PE/EG design doc ValueListDef arithmetic 3 → 4.
- [ ] Commits: `test: re-freeze qs.rds for the standardized results` then `docs: QS conformance completion`.

---

## Self-review

Design decisions → tasks: mapper columns (1), validator (2), VLM (3), freeze/docs/amendments (4). The known red window spans Tasks 1–3 and closes at Task 4. Interface order and label texts specified; every ValueListDef assertion must be found by grep, not memory.

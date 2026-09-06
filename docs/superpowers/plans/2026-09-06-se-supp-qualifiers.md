# SE + SUPPMH + SUPPVS Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Unattended-overnight rules: never push, commit per task, full suite green at every commit (reference re-freeze windows as planned), stop cleanly on BLOCKED. Branch: `se-supp-qualifiers`.

**Goal:** Add SE (Subject Elements, derived from DM + `spec$elements`) and two SUPP qualifiers — SUPPMH (new `MHSPEC` field on the MH log form, joined via a new `MHSPID`) and SUPPVS (new `VSCOMT` field on the VS event form, joined via visit+testcd → VSSEQ) — taking `build_all()` to 26 SDTM domains.

**Architecture:** SE is a DM-derived domain consuming `spec$elements` (the trial-design table) with the validator cross-checking SE↔TE; the two SUPP extensions clone the two existing join patterns (`map_suppae`'s SPID join, `map_supppe`'s visit+testcd join). Generator changes are additive columns on existing forms with config-driven seeds — zero RNG, byte-additive digests.

**Tech Stack:** unchanged. **Spec:** `docs/superpowers/specs/2026-09-06-se-supp-qualifiers-design.md` — read first; the in-repo exemplars are `R/map-qs.R`, `R/map-supp.R` (`map_suppae` :94-114, `map_supppe` :206-243), and the QS/PE-EG review lessons recorded in `.superpowers/sdd/progress.md`.

## Global Constraints

- **Zero RNG** for new generator content; messiness via `cfg$idx` entries, values via index arithmetic (established contract).
- **Byte-freedom accounting** for this plan: MH.csv and VS.csv digests move (additive columns — prove via strip-and-compare); the other 11 stay identical. References: **MH.rds re-frozen deliberately** (gains MHSPID, the AESPID precedent); `se.rds`/`suppmh.rds`/`suppvs.rds` new; the other 22 byte-identical — `sv.rds` included (SE adds no SV rows... SE isn't scheduled; verify SV untouched).
- Suite baseline 632 pass / 0 warnings; ends ≥ 640 / 0 warnings.
- XPT v5 limits; labels ≤ 40 chars.
- `.sdtm_domains` += all three new names **in the same task as their build wiring** (the Task-5 lesson from PE/EG).
- Two task-reviewer gates per task as usual; final whole-branch review before merge.

## File Structure

- `R/generate-rave-extract.R` — MH `MHSPEC`, VS `VSCOMT` fields + seeding.
- `R/generator-configs.R` — `idx$mh_spec`, `idx$vs_comment`.
- `R/spec.R` — `.sdtm_domains`.
- `R/zzz-spec-synth01.R` / `-02.R` — `variables` MHSPID row, `supp` rows.
- `R/map-se.R` (new), `R/map-supp.R` (two new mappers).
- `R/build-all.R`, `R/validate-sdtm.R`, `R/define-xml.R`, `R/globals.R`.
- Tests: `tests/testthat/test-se.R` (new), `test-pe-eg.R` → rename additions into a shared `test-supp.R`? No — put SUPPMH/SUPPVS tests in `test-se.R`'s sibling `test-supp-qualifiers.R` (new); count updates in `test-build-all.R`, `test-synth02.R`, `test-trial-design.R`.

---

### Task 1: Generator fields + seeds

**Files:** `R/generate-rave-extract.R`, `R/generator-configs.R`.

- [ ] Config: `idx$mh_spec = c(<idx>, <log_pos>)` and `idx$vs_comment = c(<idx>, <vpos>, "TEMP")` per study — probe `build_subjects`/MH row structure to pick alive indices (SYNTH01 SF = {5,17}, ET = {3,11,20}; SYNTH02 SF = {6,14}, ET = {2,9}); MH log rows per subject are numbered from 1 (`recordposition`); adjust to real data and record the choice in-config with a comment (PE/EG precedent).
- [ ] `cfg$fields$MH` += `"MHSPEC"`; `cfg$fields$VS` += `"VSCOMT"`; `.FIELD_LABELS`: `MHSPEC = "If Related, Specify"`, `VSCOMT = "Comment"`.
- [ ] MH block (~:1219 area): seed `MHSPEC = "Autoimmune thyroiditis"` on exactly the `idx$mh_spec` row (zero RNG — the condition is `idx == cfg$idx$mh_spec[1] && pos == cfg$idx$mh_spec[2]`).
- [ ] VS block: seed `VSCOMT = "Repeated after arm reposition"` on the `idx$vs_comment` subject-visit, attached to the `TEMP` field row content (the field lives on the wide form; set it once per that visit's VS row).
- [ ] Verify: 13 files; **11 of 13 prior digests unchanged**; MH.csv/VS.csv additive (strip-and-comprove: delete the `MHSPEC`/`VSCOMT` column → bytes identical to prior). Digest-guard failures confined to MH/VS/metadata entries + file-count. Full suite otherwise at baseline.
- [ ] Commit: `feat(generator): MHSPEC and VSCOMT qualifier fields`.

### Task 2: Reader/spec wiring + MHSPID + digest re-freeze

**Files:** `R/zzz-spec-synth01.R`, `R/zzz-spec-synth02.R`, `R/spec.R`, digest fixture.

- [ ] `spec$variables` (both specs): add the MHSPID row after the AESPID-analogue position — `"MH","MHSPID","recordposition","character", NA, NA, NA, NA` (match the AE AESPID row's exact shape: `"AE","AESPID","recordposition","character",…`).
- [ ] `spec$supp` (both): `"MH","MHSPID","MHSPECD","If Related, Specify","MHSPEC","squish","CRF",NA` and `"VS","VSSEQ","VSCOMTL","Comment","VSCOMT","squish","CRF",NA`.
- [ ] `.sdtm_domains` += `"SE", "SUPPMH", "SUPPVS"` — **note SE is added here so Task 3's mapper is legal, even though its builder lands next task; the vector gates `spec$variables` rows, and SE has none — same as the TA/TE/TI/TV/TS decision** (if the vector comment reads "domains build_all() can produce", update it to also say trial-design/spec-driven domains qualify — one comment line).
- [ ] Digest re-freeze to 13 with MH.csv/VS.csv moved, others identical (strip-and-compare proof in the report).
- [ ] Full suite: MH.rds is NOT yet re-frozen → the regression harness will FAIL on mh.rds (new MHSPID column). **Sanctioned red window** — the reference re-freeze is Task 5's freeze step. Expected failures: regression mh.rds compare only; everything else green. Verify exactly that.
- [ ] Commit: `feat(spec): MHSPID and qualifier rows in the spec; digests`.

### Task 3: `map_se()`

**Files:** Create `R/map-se.R`, `tests/testthat/test-se.R`; `R/globals.R`.

**Interfaces:** `map_se(dm, spec) -> tibble`: STUDYID, DOMAIN, USUBJID, SESEQ, ETCD, ELEMENT, SESTDTC, SEENDTC.

- [ ] Failing tests first. Pins (probe the built DM for exact USUBJIDs, don't invent): randomized subjects have exactly 2 SE rows (SCRN, TREAT); SF subjects exactly 1 (SCRN, SEENDTC NA); SCRN: SESTDTC = RFICDTC, SEENDTC = RFXSTDTC; TREAT: SESTDTC = RFXSTDTC, SEENDTC = RFXENDTC; ETCD/ELEMENT from `spec_synth01$elements`; SESEQ 1,2 within subject (SCRN first); all-subject seq assertion (the tightened `.by = USUBJID` pattern).
- [ ] Implement (mirror `map_qs`'s structure — prep, pivot, join, seq, select, arrange, labels):

```r
map_se <- function(dm, spec) {
  dm |>                       # dm already has USUBJID + the RF* dates
    transmute(
      STUDYID = spec$study$STUDYID, DOMAIN = "SE", USUBJID,
      ETCD = "SCRN", ELEMENT = spec$elements$ELEMENT[spec$elements$ETCD == "SCRN"],
      SESTDTC = RFICDTC, SEENDTC = RFXSTDTC
    ) |>
    bind_rows(
      dm |>
        filter(!is.na(RFXSTDTC), RFXSTDTC != "") |>
        transmute(
          STUDYID = spec$study$STUDYID, DOMAIN = "SE", USUBJID,
          ETCD = "TREAT", ELEMENT = spec$elements$ELEMENT[spec$elements$ETCD == "TREAT"],
          SESTDTC = RFXSTDTC, SEENDTC = RFXENDTC
        )
    ) |>
    filter(!is.na(SESTDTC), SESTDTC != "") |>
    mutate(SESEQ = row_number(), .by = USUBJID) |>   # or derive_seq("SESEQ", ETCD)
    arrange(USUBJID, SESEQ) |>
    apply_labels(c(…))                                # labels in the design doc
}
```

Better: drive the pivot from `spec$elements` via `pmap` (one branch per element with a date-source mapping `SCRN → c(RFICDTC, RFXSTDTC)`, `TREAT → c(RFXSTDTC, RFXENDTC)`) so a study adding an element extends the spec plus a date-map row, not the if-tree — implementer judgment, document the choice. ETCD/ELEMENT must come from `spec$elements`, never literals.
- [ ] globals.R: ETCD/ELEMENT already declared (trial design); add SESTDTC/SEENDTC/SESEQ.
- [ ] `devtools::test(filter = "^se$")` green; full suite (mh.rds regression failure still the only red).
- [ ] Commit: `feat(se): map_se()`.

### Task 4: `map_suppmh()` + `map_suppvs()`

**Files:** `R/map-supp.R`, `tests/testthat/test-supp-qualifiers.R` (new), `R/globals.R`.

**Interfaces:** `map_suppmh(forms$MH, mh, spec)`, `map_suppvs(forms$VS, vs, spec)` — standard SUPP output (STUDYID, RDOMAIN, USUBJID, IDVAR, IDVARVAL, QNAM, QLABEL, QVAL, QORIG, QEVAL).

- [ ] Failing tests: SUPPMH — exactly one row per study (SYNTH01: QNAM "MHSPECD", QVAL "Autoimmune thyroiditis", IDVAR "MHSEQ", IDVARVAL = the seeded MH row's MHSEQ, USUBJID pinned by probing); SUPPVS — exactly one row (QNAM "VSCOMTL", QVAL "Repeated after arm reposition", IDVAR "VSSEQ", IDVARVAL = the TEMP row's VSSEQ at the seeded visit); orphan corruptions fire `related-parent-orphan` (assert after Task 6 wiring — write the tests, mark skip-if-domain-missing if needed, or land them with Task 5's wiring; implementer's choice, documented).
- [ ] `map_suppmh`: `map_suppae` clone (`R/map-supp.R:94-114`): raw MH rows joined to built MH by `MHSPID = as.character(recordposition)`; filter to non-blank MHSPEC; `nrow` guard; `make_supp(parent, "MH", idvar = "MHSEQ", qnams = supp_qnams(spec, "MH"))`.
- [ ] `map_suppvs`: `map_supppe` pattern (`R/map-supp.R:206-243`): raw VS rows with non-blank VSCOMT, joined to built VS on (USUBJID, VISITNUM from Folder via `spec$visits`, VSTESTCD == the seeded testcd — read it from `cfg$idx$vs_comment[3]`… cfg is not available in the mapper; the spec's `src` names the FIELD ("VSCOMT") but the TESTCD must come from somewhere: **add it to the raw→built join by keeping only rows where the comment field is non-blank, then join on (USUBJID, VISITNUM) against built VS filtered to the VSTESTCD whose raw comment was populated** — the raw VS row is wide, so derive the testcd by which seeded field this is: simplest correct approach is a `vs_comment_testcd` entry next to `idx$vs_comment` in the config, passed through… but mappers don't see cfg. Resolution: the mapper joins on (USUBJID, VISITNUM) and takes the built VS row matching the pivot of the RAW row's non-blank comment field — since only TEMP comments exist, hardcoding is wrong; instead the mapper receives the raw VS frame and the built `vs`, maps the raw comment to `(USUBJID, VISITNUM, <testcd read from a spec-side source>)`. **Decision: extend `spec$supp` row with the testcd encoded in `src` as `"VSCOMT:TEMP"`? No — ugly. Better: `spec$supp$idvar` stays "VSSEQ" and the join uses the RAW row's `_RAW`-carrying field**… simplest defensible: `map_suppvs(forms$VS, vs, spec)` joins raw-vs-built on (USUBJID, VISITNUM, VSTESTCD) where VSTESTCD comes from filtering `spec$tests` (domain VS) rows whose `field` matches the supp `src` stem — i.e. the comment is declared on the TEMP test by naming the supp row's `src` "VSCOMT" and adding a `tests`-side convention: the qualifier attaches to the test whose spec$tests row's field is the FIRST field of the raw frame that has the comment… this is getting over-engineered. **Final decision: change the supp row's `src` to `"TEMP"`-scoped form — no. Keep it simple: the VS comment is attached to a testcd the mapper takes from a new optional column in `spec$supp`: none exists. USE the map_supppe precedent exactly: map_supppe knows "CV" because the join filters `PETESTCD == "CV"` hardcoded with a comment (approved in review). Do the same: `map_suppvs` filters `VSTESTCD == "TEMP"` with a comment that the seeded qualifier attaches to temperature; the guard fails loudly if a future study moves it. Document in roxygen.** Keep the row-count guard.
- [ ] globals.R as needed. `devtools::test(filter = "^supp-qualifiers$")` green (or skip-landed-in-task-5 as decided); full suite unchanged-red on mh.rds only.
- [ ] Commit: `feat(supp): map_suppmh() and map_suppvs()`.

### Task 5: build_all wiring + counts

**Files:** `R/build-all.R`, count tests.

- [ ] `se <- map_se(dm, spec)` after `dm` (before refs? SE needs only dm+spec — place right after `dm <- map_dm(...)`/refs creation); `suppmh <- map_suppmh(forms$MH, mh, spec)` after `mh`; `suppvs <- map_suppvs(forms$VS, vs, spec)` after `vs`. sdtm list: `DM, SE` after DM? — order: `…, LB, MH, QS, PE, EG, SE, SUPPDM, SUPPAE, SUPPEX, SUPPPE, SUPPMH, SUPPVS, CO, RELREC, TA, TE, TI, TV, TS` (SE grouped with subject findings, SUPPMH/SUPPVS with the SUPP family). Roxygen 23 → 26 + document().
- [ ] Counts: rds set + se/suppmh/suppvs (26), xpt 26, ItemGroupDef 26; cross-study test extensions (SE non-empty both studies; SUPPMH/SUPPVS single-row both studies).
- [ ] Re-freeze references NOW (this task completes the domain set): run `Rscript tests/update_reference.R`, verify `git status --porcelain tests/reference` shows EXACTLY `mh.rds` modified + `se.rds`/`suppmh.rds`/`suppvs.rds` new, zero others — **stop and investigate if sv.rds or anything else moved**. Full suite green (632+new). Commit: `feat(build): wire SE, SUPPMH and SUPPVS into build_all()` + `test: re-freeze mh.rds and freeze SE/SUPP references` (two commits).
- [ ] NOTE: Tasks 6–7 (validators, define) come AFTER the freeze in this plan — new validator checks and define entries do not move references (they only read built data). If a Task 6 check unexpectedly changes a reference, that is a bug — stop.

### Task 6: Validation

**Files:** `R/validate-sdtm.R`, `tests/testthat/test-se.R`.

- [ ] `.sdtm_req_static$SE` per design; stat-reason untouched; `.related` += `SUPPMH = "MH"`, `SUPPVS = "VS"`.
- [ ] New checks: `se-etcd-not-in-te` (setdiff SE ETCDs vs TE ETCDs — guard both present); `se-element-continuity` (per USUBJID: duplicate (USUBJID, ETCD) → ERROR; TREAT SESTDTC < SCRN SEENDTC → ERROR; guard on columns).
- [ ] Meta-tests: ETCD corruption; TREAT-before-SCRN swap; SUPPMH/SUPPVS orphans; clean build zero findings. TDD.
- [ ] Roxygen + document(). Full suite FAIL 0 WARN 0; lint clean. Commit: `feat(validate): SE and SUPP qualifier checks`.

### Task 7: define.xml + docs

**Files:** `R/define-xml.R`, `tests/testthat/test-build-all.R`, `README.md`, `NEWS.md`, design doc if needed.

- [ ] `key_spec`: SE (STUDYID, USUBJID, SESEQ), SUPPMH/SUPPVS (SUPP sets); `structure_spec`: "One record per subject per element" + SUPP patterns. No VLM. No new codelists. TDD assertions: ItemGroupDef 26 (already), SE Structure attribute + SESEQ KeySequence "3", SUPPMH/SUPPVS ItemGroupDefs exist.
- [ ] README: 23 → 26 (Maps bullet names + quick-start comment) and mapper illustration gains `map_se()`. NEWS: bullet under `0.4.0.9000` (SE + the two qualifiers, the validator additions, 26 domains). build_vignettes check.
- [ ] Full suite + lint + `R CMD build && R CMD check` Status OK (expect the PESEQ/EGSEQ-style globals note to not reappear — SESEQ/… used via derive_seq strings; if a NOTE appears, fix via globals precedent).
- [ ] Commits: `feat(define-xml): document SE, SUPPMH and SUPPVS` then `docs: SE and SUPP qualifier domains`.

---

## Self-review

Design-doc sections → tasks: SE data model (T3), SUPPMH (T1/T2/T4), SUPPVS (T1/T2/T4), validation (T6), define (T7), byte-freedom accounting (T1/T2/T5). Interface signatures consistent (`map_se(dm, spec)`, `map_suppmh(forms$MH, mh, spec)`, `map_suppvs(forms$VS, vs, spec)`). Known plan-risk flagged for implementers: the SUPPVS testcd-to-join decision is resolved as "hardcode TEMP with a loud guard + roxygen comment" (the approved map_supppe precedent); the design doc's spec$supp row carries no testcd.

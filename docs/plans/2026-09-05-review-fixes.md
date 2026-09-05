# REVIEW-2026-09-05 Fix Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Close the nine findings in `REVIEW-2026-09-05.md` against `402a874` (v0.3.0), starting with the silent-wrong `map_lb()` range bug that reaches ADLB shift tables.

**Architecture:** Keep the v0.3.0 contract: one rule, two callers; fail at spec construction, not mid-build; hand-built fixtures in `test-edge-cases.R` for inputs the seeded generator cannot produce. Finding A changes how `LBSTNRLO/HI` are scaled. Findings D/E finish the finding-12 work in `new_study_spec()`. Findings C/F/G/H harden validators, define.xml, and RNG restore using patterns already in the tree.

**Tech Stack:** R (>= 4.1), dplyr/stringr/tibble, xml2 (Suggests), testthat 3, existing internal `.rule_*` helpers in `R/adam-rules.R`.

---

## Phase 0 — Verification (done before this plan was written)

Reviewed against the current tree, not the review's line numbers alone. All nine findings are still true. Finding 6 from 2026-09-03 stays deferred (#5); do not reopen it.

| # | Verdict | Why |
|---|---------|-----|
| **A** | Fix | `R/map-lb.R:90–91` still scale ranges by `.factor`. When `RAVE_STD` is set and collected unit ≠ `CONV_FROM`, `.conv` is FALSE, `.factor` is 1, `LBSTRESN` is in standard units, ranges stay collected. `LBNRIND` then lies, and `adlb-range-not-from-lb` agrees with the lie. |
| **B** | Fix | `NEWS.md:97` is the 0.2.0 body sitting under the 0.3.0 heading. One restored `# edc2cdisc 0.2.0` line. |
| **C** | Fix | Four bare `!=` filters in `R/validate-adam.R`. `adlb-ady-wrong` at `:668–669` is the idiom to copy. `.rule_trtemfl()` never returns `NA`, so a blanked `TRTEMFL` is exactly the corruption that check exists to catch. |
| **D** | Fix | `derivations=` is validated at `R/spec.R:136–142` and never stored. `map_variables()` defaults to `list()`. `test-spec.R:95–97` accepts the resulting spec. `build_all()` on that spec dies in the engine. |
| **E** | Fix | Nine registry entries read `row$crf_field`. `new_study_spec()` allows `crf_field = NA` on those rows. Build dies with a vctrs subscript error. |
| **F** | Fix by sharing, not a comment | `R/map-lb.R:92–97` and `R/validate-sdtm.R:167–170` duplicate `.rule_anrind()`. `adam-rules.R` exists because "two copies that agree today" produced findings 1, 2, and 15. A comment is the review's fallback; sharing is the package rule. |
| **G** | Fix | XPath typo is gone; `xml_add_child()` on `xml_missing` is still a silent no-op (`R/define-xml.R:267–269`). Happy-path test only counts synth01's two refs. |
| **H** | Fix | `rng_hold` is `NULL` when `.Random.seed` did not exist; `on.exit` no-ops; `set.seed()` leaves a seed. `test-edge-cases.R:145–153` covers only the exists branch. |
| **I** | Fix last | Trailing comma at `R/adam-adae.R:91` is legal (`rlang::enquos()` ignores it) and cosmetic. One character. |

**Allowed APIs (copy these; do not invent):**

- NA-safe filter: `R/validate-adam.R:668–669`
- Range indicator: `.rule_anrind(aval, anrlo, anrhi)` in `R/adam-rules.R:51–57`
- Spec constructor + reference checks: `R/spec.R:75–179`
- Engine merge: `R/engine.R:28–29` `registry <- c(.derivations, derivations)`
- Hand-built LB fixture shape: `tests/testthat/test-edge-cases.R:6–25`
- Validator meta-test shape: `tests/testthat/test-validators.R:27–38`
- RNG exists-branch test: `tests/testthat/test-edge-cases.R:145–153`

**Anti-patterns:**

- Do not scale ranges by `LBSTRESN / LBORRES`. The review's concrete failure has both in mmol/L and ranges in mg/dL; the ratio is 1 and the bug remains.
- Do not invent EDC standard-range columns (`*_STD_NRLO`). The CRF does not have them.
- Do not edit all six mappers to pass `derivations=`. Default inside `map_variables()` from `spec$derivations`.
- Do not add `needs_field` attributes on function objects. A parallel character vector next to `.derivations` is what the review allows and what the constructor can read.
- Do not reopen VS/LB baseline datetime (finding 6 / #5).
- Do not commit generated extracts or bump to 0.4.0. This is a 0.3.1 patch.

**Follow** @superpowers:test-driven-development — no production code before a failing test.

---

### Task 1: Finding A — `map_lb()` range scaling reads `.conv`

**Files:**
- Modify: `tests/testthat/test-edge-cases.R` (append after the existing `map_lb` tests, ~line 43)
- Modify: `R/map-lb.R:86–91`
- Modify: `NEWS.md` (only if you also do Task 8; otherwise leave NEWS for Task 8)

**What this fixes:** When the EDC supplies `RAVE_STD` and the collected unit is not `CONV_FROM`, `.factor` is 1. Ranges must not be multiplied by that identity. Leave `LBSTNRLO/HI` missing so `LBNRIND` stays blank (already the missing-bound rule). Identity path (no EDC standard, no local conversion) keeps collected ranges so the existing blank-unit test still sees `LBNRIND == "NORMAL"`.

**Step 1: Write the failing test**

Append to `tests/testthat/test-edge-cases.R`:

```r
test_that("map_lb: an EDC standard value does not keep collected-unit ranges", {
  refs <- tibble(USUBJID = "3021-101-001", RFSTDTC = "2024-04-01",
                 RFENDTC = "2024-06-01")
  # glucose collected already in mmol/L, EDC standard 5.0, ranges still
  # the site's mg/dL 70/100. .conv is FALSE (mmol/L != MG/DL), .factor
  # is 1: scaling ranges by .factor marks a normal glucose LOW.
  lb_raw <- tibble(
    Subject = "101-001", Folder = spec_synth01$visits$Folder[1],
    LBPERF = "1", LBFAST = "0",
    LBDAT_YYYY = "2024", LBDAT_MM = "03", LBDAT_DD = "20", LBTIM = NA,
    GLUC_RAW = "5.0", GLUC_UN = "mmol/L",
    GLUC_STD = "5.0", GLUC_STD_UN = NA,
    GLUC_NRLO = "70", GLUC_NRHI = "100"
  )
  lb <- map_lb(lb_raw, spec_synth01, refs)
  row <- lb[lb$LBTESTCD == "GLUC", ]
  expect_equal(nrow(row), 1)
  expect_equal(as.vector(row$LBSTRESN), 5)
  expect_equal(as.vector(row$LBSTRESU), "mmol/L")
  expect_true(is.na(as.vector(row$LBSTNRLO)))
  expect_true(is.na(as.vector(row$LBSTNRHI)))
  expect_true(is.na(as.vector(row$LBNRIND)))
})

test_that("map_lb: local conversion still scales collected ranges", {
  refs <- tibble(USUBJID = "3021-101-001", RFSTDTC = "2024-04-01",
                 RFENDTC = "2024-06-01")
  lb_raw <- tibble(
    Subject = "101-001", Folder = spec_synth01$visits$Folder[1],
    LBPERF = "1", LBFAST = "0",
    LBDAT_YYYY = "2024", LBDAT_MM = "03", LBDAT_DD = "20", LBTIM = NA,
    GLUC_RAW = "90", GLUC_UN = "mg/dL",
    GLUC_STD = NA, GLUC_STD_UN = NA,
    GLUC_NRLO = "70", GLUC_NRHI = "100"
  )
  lb <- map_lb(lb_raw, spec_synth01, refs)
  row <- lb[lb$LBTESTCD == "GLUC", ]
  expect_equal(as.vector(row$LBNRIND), "NORMAL")
  expect_equal(as.vector(row$LBSTNRLO), round(70 / 18.0156, 4))
  expect_equal(as.vector(row$LBSTNRHI), round(100 / 18.0156, 4))
})
```

**Step 2: Run test to verify it fails**

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-edge-cases.R")'
```

Expected: FAIL — first new test, `LBSTNRLO` is 70 and `LBNRIND` is `"LOW"`.

**Step 3: Write minimal implementation**

In `R/map-lb.R`, replace the range block (the comment at lines 86–91 and the two `round(... * .factor)` assignments) with:

```r
      # reference range: scale only when the local factor was actually
      # applied (.conv). An EDC-supplied standard value with .conv FALSE
      # is already in standard units; the collected ranges are not, and
      # multiplying by identity .factor = 1 compares mmol/L results to
      # mg/dL bounds. Missing standard ranges leave LBNRIND blank.
      LBORNRLO = suppressWarnings(as.numeric(NRLO_O)),
      LBORNRHI = suppressWarnings(as.numeric(NRHI_O)),
      LBSTNRLO = case_when(
        .conv           ~ round(LBORNRLO * .factor, 4),
        !is.na(.rave_n) ~ NA_real_,
        .default        = LBORNRLO
      ),
      LBSTNRHI = case_when(
        .conv           ~ round(LBORNRHI * .factor, 4),
        !is.na(.rave_n) ~ NA_real_,
        .default        = LBORNRHI
      ),
```

Leave the existing `LBNRIND = case_when(...)` block untouched (Task 6).

Related smaller WARN (blank `RAVE_STDU` while `RAVE_STD` is set): after the `mutate(...)` and before the `filter(!is.na(LBORRES) | ...)`, add:

```r
  blank_stdu <- !is.na(lb$.rave_n) &
    (is.na(lb$RAVE_STDU) | lb$RAVE_STDU == "")
  if (any(blank_stdu)) {
    warning(
      sprintf(paste("lb-std-unit-blank: %d row(s) have a populated EDC",
                    "standard value and a blank standard unit;",
                    "LBSTRESU is assumed from CONV_TO or LBORRESU"),
              sum(blank_stdu)),
      call. = FALSE
    )
  }
```

Wrap the first new test's `map_lb()` call in `suppressWarnings()` (the fixture is exactly this case), or assert the warning:

```r
  expect_warning(
    lb <- map_lb(lb_raw, spec_synth01, refs),
    "lb-std-unit-blank"
  )
```

If the full suite then warns on `build_all()` / `generate_rave_extract()`, the generator left `*_STD_UN` blank — do **not** silence the suite. Check one synth GLUC row; if `GLUC_STD_UN` is populated, the warning is fixture-only.

**Step 4: Run tests**

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-edge-cases.R")'
```

Expected: PASS, including the existing blank-unit test (`LBNRIND == "NORMAL"` on unconverted 90 / 70–100).

**Step 5: Commit**

```bash
git add tests/testthat/test-edge-cases.R R/map-lb.R
git commit -m "$(cat <<'EOF'
fix(map-lb): do not scale collected ranges when the EDC supplied the standard result

EOF
)"
```

---

### Task 2: Finding B — restore the 0.2.0 NEWS heading

**Files:**
- Modify: `NEWS.md:96–97`

**Step 1: Write the failing check**

```bash
Rscript -e 'news <- readLines("NEWS.md"); stopifnot(any(grepl("^# edc2cdisc 0.2.0$", news)))'
```

Expected: FAIL — `stopifnot` error.

**Step 2: Confirm the insertion point**

`NEWS.md` line 97 currently starts `Proved the package's central claim`. That paragraph and the `## New` / `## Fixed` blocks through the end of the file are the 0.2.0 notes.

**Step 3: Restore the heading**

Insert a blank line and `# edc2cdisc 0.2.0` immediately before `Proved the package's central claim`, so the file has two top-level headings (`# edc2cdisc 0.3.0` then `# edc2cdisc 0.2.0`). Do not rewrite the 0.2.0 body.

**Step 4: Re-run the check**

```bash
Rscript -e 'news <- readLines("NEWS.md"); stopifnot(any(grepl("^# edc2cdisc 0.2.0$", news))); cat("ok\n")'
```

Expected: `ok`.

**Step 5: Commit**

```bash
git add NEWS.md
git commit -m "$(cat <<'EOF'
docs(news): restore the 0.2.0 heading consumed by the 0.3.0 notes

EOF
)"
```

---

### Task 3: Finding C — four `validate_adam()` checks become NA-safe

**Files:**
- Modify: `tests/testthat/test-validators.R` (append after the flipped-TRTEMFL test, ~line 38)
- Modify: `R/validate-adam.R:117–119`, `:306`, `:329`, `:511–514`

**Idiom to copy** (`R/validate-adam.R:668–669`):

```r
filter(xor(is.na(ADY), is.na(.expect)) |
         (!is.na(ADY) & !is.na(.expect) & ADY != .expect))
```

**Step 1: Write the failing tests**

Append to `tests/testthat/test-validators.R`:

```r
test_that("a blanked TRTEMFL is recomputed and flagged", {
  built <- build_fixtures()$built
  adae <- built$adam$ADAE
  treated <- which(adae$TRTEMFL == "Y")
  adae$TRTEMFL[treated[1]] <- NA_character_

  issues <- validate_adam(built$adam$ADSL, adae, built$adam$ADVS,
                          built$adam$ADLB, built$sdtm$DM, built$sdtm$DS,
                          built$sdtm$AE, built$sdtm$VS, built$sdtm$LB,
                          built$sdtm$SUPPAE, spec_synth01)
  expect_true("trtemfl-not-derivable" %in% issues$check)
})

test_that("a blanked TRTDURD is flagged", {
  built <- build_fixtures()$built
  adsl <- built$adam$ADSL
  i <- which(!is.na(adsl$TRTSDT) & !is.na(adsl$TRTEDT) & !is.na(adsl$TRTDURD))[1]
  adsl$TRTDURD[i] <- NA_integer_

  issues <- validate_adam(adsl, built$adam$ADAE, built$adam$ADVS,
                          built$adam$ADLB, built$sdtm$DM, built$sdtm$DS,
                          built$sdtm$AE, built$sdtm$VS, built$sdtm$LB,
                          built$sdtm$SUPPAE, spec_synth01)
  expect_true("trtdurd-wrong" %in% issues$check)
})

test_that("a blanked ADVS ADY is flagged", {
  built <- build_fixtures()$built
  advs <- built$adam$ADVS
  i <- which(!is.na(advs$ADY))[1]
  advs$ADY[i] <- NA_integer_

  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, advs,
                          built$adam$ADLB, built$sdtm$DM, built$sdtm$DS,
                          built$sdtm$AE, built$sdtm$VS, built$sdtm$LB,
                          built$sdtm$SUPPAE, spec_synth01)
  expect_true("advs-ady-wrong" %in% issues$check)
})
```

Three tests, four code sites: `astdy-wrong-anchor` is the same idiom as `advs-ady-wrong` and is covered by the production change; do not add a fourth fixture unless one of the three stays green after the first two replacements.

**Step 2: Run tests to verify they fail**

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-validators.R")'
```

Expected: FAIL — `trtemfl-not-derivable` / `trtdurd-wrong` / `advs-ady-wrong` not in `issues$check` (NA `!=` drops the row).

**Step 3: Write minimal implementation**

Replace the four bare comparisons.

`trtdurd-wrong` (`R/validate-adam.R:117–119`):

```r
  bad_dur <- adsl |>
    filter(!is.na(TRTSDT), !is.na(TRTEDT)) |>
    mutate(.expect = .rule_trtdurd(TRTSDT, TRTEDT)) |>
    filter(xor(is.na(TRTDURD), is.na(.expect)) |
             (!is.na(TRTDURD) & !is.na(.expect) & TRTDURD != .expect))
```

`trtemfl-not-derivable` (`:306`):

```r
  bad_te <- te_expected |>
    filter(xor(is.na(TRTEMFL), is.na(.expect)) |
             (!is.na(TRTEMFL) & !is.na(.expect) & TRTEMFL != .expect))
```

`astdy-wrong-anchor` (`:329`):

```r
    filter(xor(is.na(ASTDY), is.na(.expect)) |
             (!is.na(ASTDY) & !is.na(.expect) & ASTDY != .expect))
```

`advs-ady-wrong` (`:514`):

```r
    filter(xor(is.na(ADY), is.na(.expect)) |
             (!is.na(ADY) & !is.na(.expect) & ADY != .expect))
```

**Step 4: Run tests**

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-validators.R")'
```

Expected: PASS, including the original flipped-TRTEMFL test.

**Step 5: Commit**

```bash
git add tests/testthat/test-validators.R R/validate-adam.R
git commit -m "$(cat <<'EOF'
fix(validate-adam): treat NA built columns as disagreements, not drops

EOF
)"
```

---

### Task 4: Findings D and E — store derivations; require `crf_field`

**Files:**
- Modify: `tests/testthat/test-spec.R`
- Modify: `R/spec.R` (constructor list, roxygen, new check)
- Modify: `R/engine.R:28–29`
- Modify: `R/derivations.R` (vector after the header comment, before `.derivations <-`)

**Step 1: Write the failing tests**

In `test-spec.R`, after the existing `dmdy_local` block (`:95–97`), add:

```r
  spec_local <- do.call(new_study_spec, c(local_ok, list(derivations = local_fns)))
  expect_true("dmdy_local" %in% names(spec_local$derivations))
  mapped <- map_variables(
    tibble(Subject = "101-001"),
    spec_local,
    local_ok$variables[2, ]
  )
  expect_equal(mapped$ARM, 1L)
```

And a new `test_that` in the same file:

```r
test_that("new_study_spec requires crf_field on field-dependent derivations", {
  base <- list(
    study = tibble(STUDYID = "S", PROJECT = "P", seed = 1, n = 1L,
                   age_min = 18, age_max = 100),
    sites = tibble(SiteNumber = "1", siteid = "1", Site = "s",
                   SiteGroup = "g", StudySiteId = "1", COUNTRY = "ROU"),
    arms = tibble(ARMCD_DECODE = "d", ARMCD = "A", ARM = "a"),
    visits = tibble(Folder = "F", VISITNUM = 1L, VISIT = "V", EPOCH = "E",
                    TargetDays = 0L),
    codelists = tibble(ct = "X", rave_decode = "r", cdisc_term = "c"),
    supp = tibble(rdomain = "DM", idvar = NA, qnam = "Q", qlabel = "ql",
                  src = "Q", transform = "yn", qorig = "CRF", qeval = NA),
    units = tibble(testcd = "T", conv_from = "A", conv_to = "B",
                   conv_factor = 1),
    forms = tibble(form_oid = "DM", type = "event", scheduled = TRUE),
    tests = tibble(domain = "VS", field = "F", testcd = "T", test = "t",
                   cat = NA, specimen = NA),
    bds = tibble(domain = "ADVS", paramcd = "SYSBP", paramn = 1,
                 anrlo = 90, anrhi = 140),
    variables = tibble(
      domain    = "DM",
      variable  = "USUBJID",
      crf_field = NA_character_,
      transform = "derivation",
      ref       = "usubjid",
      value     = NA, aux = NA, default = NA
    )
  )
  expect_error(do.call(new_study_spec, base), "missing crf_field")
  base$variables$crf_field <- "Subject"
  expect_s3_class(do.call(new_study_spec, base), "study_spec")
})
```

Existing tests that use `crf_field = NA` on `study_id` must keep passing — `study_id` is not field-dependent.

**Step 2: Run tests to verify they fail**

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-spec.R")'
```

Expected: FAIL — `spec_local$derivations` is `NULL`; `map_variables` errors `unknown derivation 'dmdy_local'`; the `usubjid` NA-field spec still constructs.

**Step 3: Write minimal implementation**

`R/derivations.R` — immediately before `.derivations <- list(`:

```r
.derivations_need_field <- c(
  "usubjid", "country", "dsdecod",
  "enrtpt_ongoing", "enrf_ongoing",
  "enrtpt_ongoing_cm", "enrf_ongoing_cm",
  "enrtpt_ongoing_mh", "enrf_ongoing_mh"
)
```

`R/spec.R` — add `derivations = derivations` to the list at `:78–90`. After the `unknown_der` block (`:142`), add:

```r
  need_fld <- spec$variables$transform == "derivation" &
    spec$variables$ref %in% .derivations_need_field &
    (is.na(spec$variables$crf_field) | spec$variables$crf_field == "")
  if (any(need_fld)) {
    bad <- paste0(spec$variables$domain, "/", spec$variables$variable)[need_fld]
    stop(sprintf("study spec: derivation row(s) missing crf_field: %s",
                 str_flatten_comma(bad)), call. = FALSE)
  }
```

Update the `@param derivations` roxygen (`:68–72`) so it says the list is stored on `spec$derivations` and `map_variables()` reads it by default. Keep the signature `function(data, spec, row)`.

`R/engine.R:28–29`:

```r
map_variables <- function(data, spec, rows, derivations = NULL) {
  if (is.null(derivations)) {
    derivations <- spec$derivations %||% list()
  }
  registry <- c(.derivations, derivations)
```

Do not touch `map-dm.R` / `map-ex.R` / `map-ds.R` / `map-ae.R` / `map-cm.R` / `map-mh.R`. Explicit `derivations=` in `test-spec.R:166–170` still overrides.

After the roxygen edit, regenerate the man page if the package uses roxygen on commit (`devtools::document()`), or leave the `.Rd` for Task 8 if the repo regenerates docs in a batch. If `man/new_study_spec.Rd` is already committed and stale after this, update it in this commit.

**Step 4: Run tests**

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-spec.R")'
```

Expected: PASS, including the original `dmdy_local` construction test and `map_variables passes the spec row to derivations`.

**Step 5: Commit**

```bash
git add tests/testthat/test-spec.R R/spec.R R/engine.R R/derivations.R man/new_study_spec.Rd
git commit -m "$(cat <<'EOF'
fix(spec): store map-time derivations and require crf_field where the registry reads it

EOF
)"
```

---

### Task 5: Finding G — `build_define_xml()` stops on a missing ItemRef

**Files:**
- Modify: `tests/testthat/test-build-all.R`
- Modify: `R/define-xml.R:267–269`

**Step 1: Write the failing test**

Append to `tests/testthat/test-build-all.R`:

```r
test_that("build_define_xml errors when the ValueList parent ItemRef is missing", {
  out <- file.path(tempdir(), "edc2cdisc-define-missing-ref")
  on.exit(unlink(out, recursive = TRUE, force = TRUE), add = TRUE)
  ext <- file.path(out, "rave")
  suppressMessages(generate_rave_extract(out = ext))
  built <- suppressMessages(build_all(ext))

  domains <- built$sdtm
  domains$VS$VSSTRESN <- NULL
  expect_error(
    build_define_xml(domains, spec_synth01, file.path(out, "define.xml")),
    "IT.VS.VSSTRESN"
  )
})
```

**Step 2: Run test to verify it fails**

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-build-all.R")'
```

Expected: FAIL — `xml_add_child()` on `xml_missing` is a silent no-op, so `expect_error` does not see an error.

**Step 3: Write minimal implementation**

Replace `R/define-xml.R:267–269` with:

```r
    parent_ref <- xml2::xml_find_first(mdv, xpath)
    if (inherits(parent_ref, "xml_missing")) {
      stop(sprintf("build_define_xml: no ItemRef for %s",
                   str_c("IT.", d, ".", f$var)), call. = FALSE)
    }
    xml2::xml_add_child(parent_ref, "def:ValueListRef",
                        ValueListOID = str_c("VL.", d, ".", f$var))
```

**Step 4: Run tests**

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-build-all.R")'
```

Expected: PASS, including the existing two-`ValueListRef` count.

**Step 5: Commit**

```bash
git add tests/testthat/test-build-all.R R/define-xml.R
git commit -m "$(cat <<'EOF'
fix(define-xml): error when a ValueListRef parent ItemRef is missing

EOF
)"
```

---

### Task 6: Finding F — `LBNRIND` calls `.rule_anrind()`

**Files:**
- Modify: `R/map-lb.R:92–97`
- Modify: `R/validate-sdtm.R:165–172`

No new fixture required: `test-adam-rules.R` already pins the rule; `test-edge-cases.R` already pins `LBNRIND == "NORMAL"` on the blank-unit row. After Task 1 the EDC-standard fixture pins the missing-bound path.

**Step 1: Write the failing characterization (optional but keep TDD honest)**

If you want a lock before the swap, add to `test-edge-cases.R`:

```r
test_that("map_lb LBNRIND matches .rule_anrind on the same triple", {
  refs <- tibble(USUBJID = "3021-101-001", RFSTDTC = "2024-04-01",
                 RFENDTC = "2024-06-01")
  lb_raw <- tibble(
    Subject = "101-001", Folder = spec_synth01$visits$Folder[1],
    LBPERF = "1", LBFAST = "0",
    LBDAT_YYYY = "2024", LBDAT_MM = "03", LBDAT_DD = "20", LBTIM = NA,
    GLUC_RAW = "90", GLUC_UN = "mg/dL", GLUC_STD = NA, GLUC_STD_UN = NA,
    GLUC_NRLO = "70", GLUC_NRHI = "100"
  )
  lb <- map_lb(lb_raw, spec_synth01, refs)
  row <- lb[lb$LBTESTCD == "GLUC", ]
  expect_identical(
    as.vector(row$LBNRIND),
    .rule_anrind(row$LBSTRESN, row$LBSTNRLO, row$LBSTNRHI)
  )
})
```

This should already PASS before the swap (same arithmetic). That is fine — the production change is a dedup, and this test is the guard that they stay the same.

**Step 2: Run it**

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-edge-cases.R")'
```

Expected: PASS (characterization).

**Step 3: Dedup**

`R/map-lb.R` LBNRIND assignment:

```r
      LBNRIND  = .rule_anrind(LBSTRESN, LBSTNRLO, LBSTNRHI)
```

`R/validate-sdtm.R:165–172`:

```r
  lb_ind_bad <- domains$LB |>
    mutate(expect = .rule_anrind(LBSTRESN, LBSTNRLO, LBSTNRHI)) |>
    filter(xor(is.na(LBNRIND), is.na(expect)) |
             (!is.na(LBNRIND) & !is.na(expect) & LBNRIND != expect))
```

**Step 4: Run tests**

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-edge-cases.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-validators.R")'
```

Expected: PASS.

**Step 5: Commit**

```bash
git add R/map-lb.R R/validate-sdtm.R tests/testthat/test-edge-cases.R
git commit -m "$(cat <<'EOF'
refactor(lb): derive LBNRIND from the shared ANRIND rule

EOF
)"
```

---

### Task 7: Finding H — restore a missing RNG to missing

**Files:**
- Modify: `tests/testthat/test-edge-cases.R` (after the existing RNG test, ~line 153)
- Modify: `R/generate-rave-extract.R:1277–1282`

**Step 1: Write the failing test**

```r
test_that("generate_rave_extract does not create a seed the caller lacked", {
  out <- file.path(tempdir(), "edc2cdisc-rng-absent")
  on.exit(unlink(out, recursive = TRUE, force = TRUE), add = TRUE)

  had_seed <- exists(".Random.seed", envir = globalenv())
  seed_hold <- if (had_seed) .Random.seed # nolint: object_name_linter.
  if (had_seed) {
    rm(".Random.seed", envir = globalenv()) # nolint: object_name_linter.
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", seed_hold, envir = globalenv()) # nolint: object_name_linter.
    } else if (exists(".Random.seed", envir = globalenv())) {
      rm(".Random.seed", envir = globalenv()) # nolint: object_name_linter.
    }
  }, add = TRUE)

  suppressMessages(generate_rave_extract(out = file.path(out, "rave")))
  expect_false(exists(".Random.seed", envir = globalenv()))
})
```

**Step 2: Run test to verify it fails**

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-edge-cases.R")'
```

Expected: FAIL — `.Random.seed` exists after the call.

**Step 3: Write minimal implementation**

Replace the `on.exit` at `R/generate-rave-extract.R:1277–1282` with:

```r
  on.exit({
    if (is.null(rng_hold)) {
      if (exists(".Random.seed", envir = globalenv())) {
        rm(".Random.seed", envir = globalenv()) # nolint: object_name_linter. (.Random.seed is a required base name)
      }
    } else {
      assign(".Random.seed", rng_hold, envir = globalenv()) # nolint: object_name_linter. (.Random.seed is a required base name)
    }
  }, add = TRUE)
```

**Step 4: Run tests**

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-edge-cases.R")'
```

Expected: PASS, including the exists-branch test at line 145.

**Step 5: Commit**

```bash
git add tests/testthat/test-edge-cases.R R/generate-rave-extract.R
git commit -m "$(cat <<'EOF'
fix(generator): restore a missing RNG to missing after generate_rave_extract()

EOF
)"
```

---

### Task 8: Finding I, NEWS 0.3.1, full suite

**Files:**
- Modify: `R/adam-adae.R:91`
- Modify: `NEWS.md` (prepend a 0.3.1 section)
- Modify: `DESCRIPTION` (`Version: 0.3.1`)

**Step 1: Trailing comma**

`R/adam-adae.R:91` is currently:

```r
      TRTEMFL = .rule_trtemfl(ASTDT, TRTSDT, TRTEDT),
```

Remove the trailing comma (the last argument before `)`).

**Step 2: Prepend NEWS 0.3.1** above `# edc2cdisc 0.3.0`:

```markdown
# edc2cdisc 0.3.1

Closes REVIEW-2026-09-05.

## Fixed

* `map_lb()` no longer scales collected reference ranges by the identity
  factor when the EDC already supplied the standard result. `LBSTNRLO/HI`
  stay missing and `LBNRIND` stays blank rather than calling a mmol/L
  glucose LOW against mg/dL bounds. A blank EDC standard unit on a
  populated standard value is a `lb-std-unit-blank` warning.
* Four `validate_adam()` checks (`trtdurd-wrong`, `trtemfl-not-derivable`,
  `astdy-wrong-anchor`, `advs-ady-wrong`) now treat an NA built column as
  a disagreement. A blanked `TRTEMFL` is the corruption those checks exist
  to catch.
* `new_study_spec(derivations = )` stores the list on the spec, and
  `map_variables()` reads `spec$derivations` by default, so a declared
  map-time derivation survives `build_all()`.
* Derivation rows whose registry entry reads `row$crf_field` must name
  that field at construction time.
* `build_define_xml()` errors when the ValueList parent `ItemRef` is
  missing instead of silently orphaning the value list.
* `generate_rave_extract()` restores a caller that had no `.Random.seed`
  to that state.

## Changed

* SDTM `LBNRIND` (mapper and `validate_sdtm()`) calls the shared
  `.rule_anrind()`.

```

**Step 3: Bump `DESCRIPTION` Version to `0.3.1`.**

**Step 4: Full suite**

```bash
Rscript -e 'testthat::test_local(".")'
```

Expected: PASS. If roxygen is stale, `devtools::document()` and include `man/`.

**Step 5: Commit**

```bash
git add R/adam-adae.R NEWS.md DESCRIPTION man
git commit -m "$(cat <<'EOF'
chore: drop the ADAE trailing comma and note the 0.3.1 review fixes

EOF
)"
```

---

## Final verification

1. `Rscript -e 'testthat::test_local(".")'` — all green.
2. `git diff 402a874 -- NEWS.md` — `# edc2cdisc 0.2.0` is present; 0.3.1 is above 0.3.0.
3. Grep guards:
   - `LBSTNRLO = round(LBORNRLO * .factor` must not remain in `R/map-lb.R`.
   - `filter(TRTEMFL != .expect)` / `filter(ADY != .expect)` (bare) must not remain in `R/validate-adam.R`.
   - `spec$derivations` must be assigned in `R/spec.R`.
   - `inherits(parent_ref, "xml_missing")` must exist in `R/define-xml.R`.
   - `rm(".Random.seed"` must exist in `R/generate-rave-extract.R`.
4. Do not reopen finding 6 (VS/LB baseline datetime).

---

## Self-review

| Review item | Task |
|-------------|------|
| A silent wrong LBNRIND / shift tables | 1 |
| A related blank `RAVE_STDU` WARN | 1 |
| B NEWS 0.2.0 heading | 2 |
| C four NA-unsafe checks + blank TRTEMFL test | 3 |
| D store + wire `derivations` | 4 |
| E `needs_field` via `.derivations_need_field` | 4 |
| G xpath `xml_missing` guard | 5 |
| F share `.rule_anrind()` | 6 |
| H absent RNG | 7 |
| I trailing comma + 0.3.1 notes | 8 |

# Trial Design SDTM Domains (TA, TE, TI, TV, TS) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add the SDTMIG trial design family — TA, TE, TI, TV, TS — built from four new optional spec tables, so `build_all()` returns 19 SDTM domains with validation, define.xml coverage and frozen references.

**Architecture:** Trial design is protocol fact, not collected data, so it never touches the row-oriented `variables` mapping engine. Four optional tables (`elements`, `ta`, `ie`, `ts`) join `new_study_spec()`; five small spec-only builders in one new `R/trial-design.R` reshape them; TV is derived from the existing `visits` table. `validate_sdtm()` recomputes the derivable facts instead of trusting the builders.

**Tech Stack:** R package; dplyr/stringr/labelled (already imported); testthat; haven XPT v5; xml2 for define.xml.

**Spec:** `docs/superpowers/specs/2026-09-05-trial-design-domains-design.md`

## Global Constraints

- `.sdtm_domains` in `R/spec.R` stays **unchanged** — a `variables` row naming TA/TE/TI/TV/TS must keep failing loudly as "unbuildable domain".
- The 14 existing frozen reference files under `tests/reference/` must stay byte-identical; only five new files are added (Task 10).
- XPT v5 limits: every variable name ≤ 8 chars, every label ≤ 40 chars. All planned names/labels comply — do not deviate.
- Every new `map_*()` applies IG variable labels inline via `apply_labels()`, like the existing mappers (see `R/map-dm.R:77`).
- No new package dependencies. No changes to `generate_rave_extract()`.
- New data-masked column names must be added to `R/globals.R` (see each task).
- Run tests from the repo root with `Rscript -e 'devtools::test(filter = "^<file>$")'`; a green run is `Fail: 0`.
- Commit after every task. Conventional commit style, lowercase summary.
- Comments in this repo explain *why*, in the voice of the existing files — match it.

## File Structure

- `R/spec.R` — `new_study_spec()`: four optional tables, defaults, constructor validation; `print.study_spec()`.
- `R/zzz-spec-synth01.R`, `R/zzz-spec-synth02.R` — trial-design content for both shipped studies.
- `R/trial-design.R` (**new**) — `map_te()`, `map_ta()`, `map_ti()`, `map_ts()`, `map_tv()`.
- `R/build-all.R` — wire the five builders into the SDTM list.
- `R/validate-sdtm.R` — static required-variable lists + the new checks.
- `R/define-xml.R` — `key_spec`, `structure_spec`, origins, `IECAT` codelist.
- `R/globals.R` — new data-masked names.
- `tests/testthat/test-spec.R` — constructor tests for the new tables.
- `tests/testthat/test-trial-design.R` (**new**) — builder tests + validator meta-tests.
- `tests/testthat/test-build-all.R` — output-tree counts 14 → 19.
- `tests/reference/sdtm/` — five new frozen `.rds` files.
- `README.md`, `NEWS.md`, `vignettes/new-study.Rmd`, `vignettes/design.Rmd`, `docs/superpowers/specs/2026-09-05-trial-design-domains-design.md` — docs.

---

### Task 1: `new_study_spec()` gains four optional trial-design tables

**Files:**
- Modify: `R/spec.R` (signature ~line 75, body ~line 93, checks after line 183, print ~line 193)
- Test: `tests/testthat/test-spec.R` (append)

**Interfaces:**
- Consumes: nothing new.
- Produces: `new_study_spec(..., variables, elements = NULL, ta = NULL, ie = NULL, ts = NULL, derivations = list())`; `spec$elements`, `spec$ta`, `spec$ie`, `spec$ts` are always present, defaulting to zero-row typed tibbles. Later tasks rely on those column sets:
  - `elements`: ETCD, ELEMENT, TESTRL, TEENRL, TEDUR (all character)
  - `ta`: ARMCD, ETCD (character), TAETORD (integer), EPOCH, TABRANCH, TATRANS (character)
  - `ie`: IETESTCD, IETEST, IECAT (character)
  - `ts`: TSPARMCD, TSPARM, TSVAL, TSVALNF (character)

- [ ] **Step 1: Write the failing constructor tests**

Append to `tests/testthat/test-spec.R`:

```r
# Trial design tables ----------------------------------------------------
# Optional at construction, always present on the spec: a spec without
# trial design still builds, one with trial design is checked here rather
# than halfway through a build.

td_spec <- function(...) {
  new_study_spec(
    study     = spec_synth01$study,
    sites     = spec_synth01$sites,
    arms      = spec_synth01$arms,
    visits    = spec_synth01$visits,
    codelists = spec_synth01$codelists,
    supp      = spec_synth01$supp,
    units     = spec_synth01$units,
    forms     = spec_synth01$forms,
    tests     = spec_synth01$tests,
    bds       = spec_synth01$bds,
    variables = spec_synth01$variables,
    ...
  )
}

test_that("trial design tables default to empty typed tibbles", {
  s <- td_spec()
  expect_equal(nrow(s$elements), 0)
  expect_setequal(names(s$elements),
                  c("ETCD", "ELEMENT", "TESTRL", "TEENRL", "TEDUR"))
  expect_setequal(names(s$ta),
                  c("ARMCD", "ETCD", "TAETORD", "EPOCH", "TABRANCH", "TATRANS"))
  expect_setequal(names(s$ie), c("IETESTCD", "IETEST", "IECAT"))
  expect_setequal(names(s$ts), c("TSPARMCD", "TSPARM", "TSVAL", "TSVALNF"))
})

test_that("a well-formed trial design spec constructs", {
  s <- td_spec(
    elements = tibble(ETCD = "A", ELEMENT = "Element A", TESTRL = "consent",
                      TEENRL = "randomised", TEDUR = "P2W"),
    ta = tibble(ARMCD = "PBO", ETCD = "A", TAETORD = 1L, EPOCH = "SCREENING",
                TABRANCH = NA_character_, TATRANS = "Randomised"),
    ie = tibble(IETESTCD = "INC1", IETEST = "Age 18-100", IECAT = "INCLUSION"),
    ts = tibble(TSPARMCD = "PHASE", TSPARM = "Trial Phase",
                TSVAL = "PHASE II", TSVALNF = NA_character_)
  )
  expect_equal(nrow(s$ta), 1)
})

test_that("trial design shape errors fire at construction", {
  expect_error(td_spec(ta = tibble(ARMCD = "NOPE", ETCD = "A", TAETORD = 1L,
                                   EPOCH = "SCREENING", TABRANCH = NA,
                                   TATRANS = NA)),
               "ta: ARMCD not in spec\\$arms")
  expect_error(td_spec(elements = tibble(ETCD = "A", ELEMENT = "Element A",
                                         TESTRL = "r", TEENRL = "r",
                                         TEDUR = "P2W"),
                       ta = tibble(ARMCD = "PBO", ETCD = "B", TAETORD = 1L,
                                   EPOCH = "SCREENING", TABRANCH = NA,
                                   TATRANS = NA)),
               "ta: ETCD not in spec\\$elements")
  expect_error(td_spec(elements = tibble(ETCD = c("A", "A"), ELEMENT = c("x", "y"),
                                         TESTRL = "r", TEENRL = "r",
                                         TEDUR = "P2W")),
               "duplicate ETCD")
  expect_error(td_spec(elements = tibble(ETCD = "TOOLONGCD", ELEMENT = "x",
                                         TESTRL = "r", TEENRL = "r",
                                         TEDUR = "P2W")),
               "ETCD longer than 8")
  expect_error(td_spec(elements = tibble(ETCD = "A", ELEMENT = "x",
                                         TESTRL = "r", TEENRL = "r",
                                         TEDUR = "2 weeks")),
               "TEDUR is not an ISO 8601 duration")
  expect_error(td_spec(ta = tibble(ARMCD = c("PBO", "PBO"), ETCD = c("A", "A"),
                                   TAETORD = c(1L, 1L), EPOCH = "SCREENING",
                                   TABRANCH = NA, TATRANS = NA)),
               "duplicated ARMCD/TAETORD")
  expect_error(td_spec(ie = tibble(IETESTCD = "INC1", IETEST = "x",
                                   IECAT = "MAYBE")),
               "ie: IECAT not INCLUSION/EXCLUSION")
  expect_error(td_spec(ts = tibble(TSPARMCD = "X", TSPARM = "x", TSVAL = "v",
                                   TSVALNF = "N/A")),
               "both TSVAL and TSVALNF")
  expect_error(td_spec(ts = tibble(TSPARMCD = "X", TSPARM = "x", TSVAL = NA,
                                   TSVALNF = NA)),
               "neither TSVAL nor TSVALNF")
})
```

Note: `td_spec(ta = ...)` passes only one trial table; the others default — that is exactly the optionality under test.

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'devtools::test(filter = "^spec$")'`
Expected: FAIL — `td_spec` errors with `unused arguments (elements = NULL, ...)`, or the default-shape test fails on `s$elements` being NULL.

- [ ] **Step 3: Implement in `R/spec.R`**

3a. Next to `.transforms` (top of file), add the typed defaults:

```r
# Trial design tables: optional constructor arguments, always present on
# the built spec so the builders can rely on their columns. Protocol fact,
# not collected data - see map_te()/map_ta()/map_ti()/map_ts()/map_tv().
.trial_defaults <- list(
  elements = tibble(ETCD = character(), ELEMENT = character(),
                    TESTRL = character(), TEENRL = character(),
                    TEDUR = character()),
  ta = tibble(ARMCD = character(), ETCD = character(), TAETORD = integer(),
              EPOCH = character(), TABRANCH = character(),
              TATRANS = character()),
  ie = tibble(IETESTCD = character(), IETEST = character(),
              IECAT = character()),
  ts = tibble(TSPARMCD = character(), TSPARM = character(),
              TSVAL = character(), TSVALNF = character())
)
```

3b. Change the signature (insert the four arguments between `variables` and `derivations`):

```r
new_study_spec <- function(study, sites, arms, visits, codelists,
                           supp, units, forms, tests, bds, variables,
                           elements = NULL, ta = NULL, ie = NULL, ts = NULL,
                           derivations = list()) {
  spec <- list(
    study     = study,
    sites     = sites,
    arms      = arms,
    visits    = visits,
    codelists = codelists,
    supp      = supp,
    units     = units,
    forms     = forms,
    tests     = tests,
    bds       = bds,
    variables = variables,
    elements  = elements,
    ta        = ta,
    ie        = ie,
    ts        = ts,
    derivations = derivations
  )

  # optional tables get their typed defaults before the required-column
  # loop below runs, so a user-supplied wrong shape still fails there
  for (nm in names(.trial_defaults)) {
    if (is.null(spec[[nm]])) spec[[nm]] <- .trial_defaults[[nm]]
  }
```

3c. Extend the `required` list (after the `variables` entry):

```r
    elements  = c("ETCD", "ELEMENT", "TESTRL", "TEENRL", "TEDUR"),
    ta        = c("ARMCD", "ETCD", "TAETORD", "EPOCH", "TABRANCH", "TATRANS"),
    ie        = c("IETESTCD", "IETEST", "IECAT"),
    ts        = c("TSPARMCD", "TSPARM", "TSVAL", "TSVALNF")
```

3d. Add the trial-design checks after the duplicate-BDS check (`dup_bds` block, ~line 186), before `class(spec) <- ...`:

```r
  # Trial design: the protocol must resolve against the tables it names,
  # and the XPT v5 limits are enforced here, not at write time.
  if (nrow(spec$elements) > 0) {
    if (anyDuplicated(spec$elements$ETCD) > 0) {
      stop("study spec: duplicate ETCD in elements", call. = FALSE)
    }
    long_etcd <- spec$elements$ETCD[nchar(spec$elements$ETCD) > 8]
    if (length(long_etcd) > 0) {
      stop(sprintf("study spec: ETCD longer than 8 characters: %s",
                   str_flatten_comma(long_etcd)), call. = FALSE)
    }
    long_elem <- spec$elements$ELEMENT[!is.na(spec$elements$ELEMENT) &
                                         nchar(spec$elements$ELEMENT) > 40]
    if (length(long_elem) > 0) {
      stop(sprintf("study spec: ELEMENT longer than 40 characters: %s",
                   str_flatten_comma(long_elem)), call. = FALSE)
    }
    bad_dur <- spec$elements$TEDUR[
      !is.na(spec$elements$TEDUR) &
        !str_detect(str_to_upper(spec$elements$TEDUR), "^P(\\d+[YMDW])+$")
    ]
    if (length(bad_dur) > 0) {
      stop(sprintf("study spec: TEDUR is not an ISO 8601 duration: %s",
                   str_flatten_comma(bad_dur)), call. = FALSE)
    }
  }
  if (nrow(spec$ta) > 0) {
    unknown_arm <- setdiff(unique(spec$ta$ARMCD), unique(spec$arms$ARMCD))
    if (length(unknown_arm) > 0) {
      stop(sprintf("study spec: ta: ARMCD not in spec$arms: %s",
                   str_flatten_comma(unknown_arm)), call. = FALSE)
    }
    unknown_etcd <- setdiff(unique(spec$ta$ETCD), unique(spec$elements$ETCD))
    if (length(unknown_etcd) > 0) {
      stop(sprintf("study spec: ta: ETCD not in spec$elements: %s",
                   str_flatten_comma(unknown_etcd)), call. = FALSE)
    }
    dup_ta <- spec$ta |> count(ARMCD, TAETORD) |> filter(n > 1)
    if (nrow(dup_ta) > 0) {
      stop("study spec: duplicated ARMCD/TAETORD in ta", call. = FALSE)
    }
  }
  if (nrow(spec$ie) > 0) {
    bad_cat <- setdiff(unique(spec$ie$IECAT), c("INCLUSION", "EXCLUSION"))
    if (length(bad_cat) > 0) {
      stop(sprintf("study spec: ie: IECAT not INCLUSION/EXCLUSION: %s",
                   str_flatten_comma(bad_cat)), call. = FALSE)
    }
  }
  if (nrow(spec$ts) > 0) {
    if (anyDuplicated(spec$ts$TSPARMCD) > 0) {
      stop("study spec: duplicate TSPARMCD in ts", call. = FALSE)
    }
    both <- !is.na(spec$ts$TSVAL) & spec$ts$TSVAL != "" &
      !is.na(spec$ts$TSVALNF) & spec$ts$TSVALNF != ""
    if (any(both)) {
      stop(sprintf("study spec: both TSVAL and TSVALNF filled for: %s",
                   str_flatten_comma(spec$ts$TSPARMCD[both])), call. = FALSE)
    }
    neither <- (is.na(spec$ts$TSVAL) | spec$ts$TSVAL == "") &
      (is.na(spec$ts$TSVALNF) | spec$ts$TSVALNF == "")
    if (any(neither)) {
      stop(sprintf("study spec: neither TSVAL nor TSVALNF filled for: %s",
                   str_flatten_comma(spec$ts$TSPARMCD[neither])),
           call. = FALSE)
    }
  }
```

3e. Extend `print.study_spec()`:

```r
  cat(sprintf("  trial design: elements %d   ta %d   ie %d   ts %d\n",
              nrow(x$elements), nrow(x$ta), nrow(x$ie), nrow(x$ts)))
```

3f. Update `new_study_spec()` roxygen: add `@param elements,ta,ie,ts` and four bullets in the table list:

```r
#' @param elements,ta,ie,ts Optional trial design tibbles: `elements`
#'   (ETCD, ELEMENT, TESTRL, TEENRL, TEDUR), `ta` (ARMCD, ETCD, TAETORD,
#'   EPOCH, TABRANCH, TATRANS), `ie` (IETESTCD, IETEST, IECAT) and `ts`
#'   (TSPARMCD, TSPARM, TSVAL, TSVALNF). Missing ones default to zero-row
#'   tables; the trial design domains built from them (see
#'   [map_te()]) then come out empty.
```

And four bullets after the `variables` bullet:

```r
#' * `elements`  - trial elements: `ETCD` (<=8 chars, unique), `ELEMENT`
#'   (<=40 chars), `TESTRL` / `TEENRL` (start/end rules), `TEDUR` (ISO 8601
#'   duration, e.g. `P2W`; NA allowed)
#' * `ta`        - planned arm x element order: `ARMCD`, `ETCD`, `TAETORD`
#'   (integer), `EPOCH`, `TABRANCH`, `TATRANS`
#' * `ie`        - inclusion/exclusion criteria: `IETESTCD`, `IETEST`,
#'   `IECAT` (INCLUSION / EXCLUSION)
#' * `ts`        - trial summary parameters: `TSPARMCD` (unique),
#'   `TSPARM`, `TSVAL`, `TSVALNF` - exactly one of TSVAL/TSVALNF filled
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `Rscript -e 'devtools::test(filter = "^spec$")'`
Expected: PASS, `Fail: 0`.

- [ ] **Step 5: Commit**

```bash
git add R/spec.R tests/testthat/test-spec.R
git commit -m "feat(spec): four optional trial design tables with constructor checks"
```

---

### Task 2: Trial design content for spec_synth01

**Files:**
- Modify: `R/zzz-spec-synth01.R` (insert after the `visits` tribble, ~line 56; extend roxygen description ~line 16)

**Interfaces:**
- Consumes: `new_study_spec()` from Task 1; `spec_synth01`'s arms (PBO/SYN50/SYN100) and visits (6, TargetDays -14..84).
- Produces: `spec_synth01$elements` (2 rows), `$ta` (6), `$ie` (6), `$ts` (7). Task 4's tests pin these counts.

- [ ] **Step 1: Insert the four tribbles after the `visits` tribble**

```r
  # Trial design: protocol facts, not collected data. TA carries the
  # transition rule on the element being left (SCRN -> "Randomised") and
  # the branch on the element the branch leads to (TREAT -> "Randomised
  # to <arm>"), per SDTMIG. NARMS / PLANSUB must agree with the arms and
  # study tables - validate_sdtm() recomputes both.
  elements = tribble(
    ~ETCD,   ~ELEMENT,    ~TESTRL,                     ~TEENRL,                            ~TEDUR,
    "SCRN",  "Screening", "Informed consent obtained", "Randomised or screen failure",     "P2W",
    "TREAT", "Treatment", "First dose of study drug",  "End of treatment visit completed", "P12W"
  ),

  ta = tribble(
    ~ARMCD,   ~ETCD,   ~TAETORD, ~EPOCH,      ~TABRANCH,                       ~TATRANS,
    "PBO",    "SCRN",  1L,       "SCREENING", NA,                              "Randomised",
    "PBO",    "TREAT", 2L,       "TREATMENT", "Randomised to Placebo",         NA,
    "SYN50",  "SCRN",  1L,       "SCREENING", NA,                              "Randomised",
    "SYN50",  "TREAT", 2L,       "TREATMENT", "Randomised to SYN-101 50 mg",   NA,
    "SYN100", "SCRN",  1L,       "SCREENING", NA,                              "Randomised",
    "SYN100", "TREAT", 2L,       "TREATMENT", "Randomised to SYN-101 100 mg",  NA
  ),

  ie = tribble(
    ~IETESTCD, ~IETEST,                                                                   ~IECAT,
    "INC1",    "Age 18 to 100 years, inclusive",                                          "INCLUSION",
    "INC2",    "Diagnosis of essential hypertension per protocol section 4.1",            "INCLUSION",
    "INC3",    "Written informed consent obtained before any study procedure",            "INCLUSION",
    "EXC1",    "Treatment with an investigational drug within 30 days before screening",  "EXCLUSION",
    "EXC2",    "Pregnant or breastfeeding",                                               "EXCLUSION",
    "EXC3",    "Clinically significant laboratory abnormality at screening",              "EXCLUSION"
  ),

  ts = tribble(
    ~TSPARMCD, ~TSPARM,                              ~TSVAL,                                                                   ~TSVALNF,
    "TITLE",   "Trial Title",                        "A Phase II randomised, double-blind, placebo-controlled study of SYN-101", NA,
    "PHASE",   "Trial Phase Classification",         "PHASE II",                                                                NA,
    "SPONSOR", "Clinical Study Sponsor",             "SYNTH Pharmaceuticals",                                                   NA,
    "INDIC",   "Trial Indication",                   "Essential Hypertension",                                                  NA,
    "TRT",     "Investigational Therapy or Treatment", "SYN-101; Placebo",                                                      NA,
    "NARMS",   "Planned Number of Arms",             "3",                                                                       NA,
    "PLANSUB", "Planned Number of Subjects",         "24",                                                                      NA
  ),
```

- [ ] **Step 2: Extend the roxygen description**

In the `spec_synth01` docstring paragraph, after "...and the variable-level mapping table for the event/log forms.", add:

```
#' The trial design domains are built from the `elements`, `ta`, `ie` and
#' `ts` tables (two elements, a 3-arm x 2-element plan, six criteria and
#' seven trial summary parameters); TV comes from the `visits` table.
```

- [ ] **Step 3: Verify the spec still constructs with the new content**

Run:
```bash
Rscript -e 'pkgload::load_all("."); stopifnot(nrow(spec_synth01$elements) == 2, nrow(spec_synth01$ta) == 6, nrow(spec_synth01$ie) == 6, nrow(spec_synth01$ts) == 7); cat("ok\n")'
```
Expected: `ok`. (The constructor's new checks from Task 1 validate the shapes; an ARMCD typo or a bad TEDUR stops here.)

- [ ] **Step 4: Run the spec tests**

Run: `Rscript -e 'devtools::test(filter = "^spec$")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/zzz-spec-synth01.R
git commit -m "feat(spec): trial design content for SYNTH01"
```

---

### Task 3: Trial design content for spec_synth02

**Files:**
- Modify: `R/zzz-spec-synth02.R` (insert after the `visits` tribble, ~line 62; extend roxygen description ~line 18)

**Interfaces:**
- Consumes: Task 1 constructor; SYNTH02's arms (PBO/SYN25/SYN50/SYN100 — 4 arms) and visits (6, TargetDays -14..112 → a 16-week treatment element).
- Produces: `spec_synth02$elements` (2), `$ta` (8), `$ie` (6), `$ts` (7). Task 7's cross-study test pins these.

- [ ] **Step 1: Insert the four tribbles after the `visits` tribble**

```r
  # Trial design, SYNTH02 flavour: four arms of SYN-201 in COPD, a 16-week
  # treatment element (EOT sits at day 112), and its own criteria set.
  elements = tribble(
    ~ETCD,   ~ELEMENT,    ~TESTRL,                     ~TEENRL,                            ~TEDUR,
    "SCRN",  "Screening", "Informed consent obtained", "Randomised or screen failure",     "P2W",
    "TREAT", "Treatment", "First dose of study drug",  "End of treatment visit completed", "P16W"
  ),

  ta = tribble(
    ~ARMCD,   ~ETCD,   ~TAETORD, ~EPOCH,      ~TABRANCH,                        ~TATRANS,
    "PBO",    "SCRN",  1L,       "SCREENING", NA,                               "Randomised",
    "PBO",    "TREAT", 2L,       "TREATMENT", "Randomised to Placebo",          NA,
    "SYN25",  "SCRN",  1L,       "SCREENING", NA,                               "Randomised",
    "SYN25",  "TREAT", 2L,       "TREATMENT", "Randomised to SYN-201 25 mg",    NA,
    "SYN50",  "SCRN",  1L,       "SCREENING", NA,                               "Randomised",
    "SYN50",  "TREAT", 2L,       "TREATMENT", "Randomised to SYN-201 50 mg",    NA,
    "SYN100", "SCRN",  1L,       "SCREENING", NA,                               "Randomised",
    "SYN100", "TREAT", 2L,       "TREATMENT", "Randomised to SYN-201 100 mg",   NA
  ),

  ie = tribble(
    ~IETESTCD, ~IETEST,                                                                   ~IECAT,
    "INC1",    "Age 18 to 100 years, inclusive",                                          "INCLUSION",
    "INC2",    "Diagnosis of chronic obstructive pulmonary disease per protocol section 4.1", "INCLUSION",
    "INC3",    "Written informed consent obtained before any study procedure",            "INCLUSION",
    "EXC1",    "Treatment with an investigational drug within 30 days before screening",  "EXCLUSION",
    "EXC2",    "Pregnant or breastfeeding",                                               "EXCLUSION",
    "EXC3",    "Clinically significant laboratory abnormality at screening",              "EXCLUSION"
  ),

  ts = tribble(
    ~TSPARMCD, ~TSPARM,                              ~TSVAL,                                                                   ~TSVALNF,
    "TITLE",   "Trial Title",                        "A Phase II randomised, double-blind, placebo-controlled study of SYN-201", NA,
    "PHASE",   "Trial Phase Classification",         "PHASE II",                                                                NA,
    "SPONSOR", "Clinical Study Sponsor",             "SYNTH Pharmaceuticals",                                                   NA,
    "INDIC",   "Trial Indication",                   "Chronic Obstructive Pulmonary Disease",                                   NA,
    "TRT",     "Investigational Therapy or Treatment", "SYN-201; Placebo",                                                      NA,
    "NARMS",   "Planned Number of Arms",             "4",                                                                       NA,
    "PLANSUB", "Planned Number of Subjects",         "18",                                                                      NA
  ),
```

- [ ] **Step 2: Extend the roxygen description**

Append to the `spec_synth02` description paragraph (after "...anything that differs is a row in one of these tables."):

```
#' The trial design tables follow the same rule: four arms x two elements,
#' two elements, six criteria and seven trial summary parameters, all
#' different from SYNTH01 without a mapper change.
```

- [ ] **Step 3: Verify construction**

Run:
```bash
Rscript -e 'pkgload::load_all("."); stopifnot(nrow(spec_synth02$elements) == 2, nrow(spec_synth02$ta) == 8, nrow(spec_synth02$ie) == 6, nrow(spec_synth02$ts) == 7); cat("ok\n")'
```
Expected: `ok`.

- [ ] **Step 4: Commit**

```bash
git add R/zzz-spec-synth02.R
git commit -m "feat(spec): trial design content for SYNTH02"
```

---

### Task 4: `map_te()` and `map_ta()`

**Files:**
- Create: `R/trial-design.R`
- Modify: `R/globals.R` (add masked names)
- Test: `tests/testthat/test-trial-design.R` (**new**)

**Interfaces:**
- Consumes: `spec$elements`, `spec$ta`, `spec$arms`, `spec$study$STUDYID` (Tasks 1–3); `apply_labels()` from `R/helpers-format.R`.
- Produces (used by Task 7's `build_all()` wiring):
  - `map_te(spec) -> tibble` with columns STUDYID, DOMAIN, ETCD, ELEMENT, TESTRL, TEENRL, TEDUR
  - `map_ta(spec) -> tibble` with columns STUDYID, DOMAIN, ARMCD, ARM, TAETORD, ETCD, ELEMENT, TABRANCH, TATRANS, EPOCH
  (ARM is joined from `spec$arms` — it is an IG-required TA variable that no `ta` table row needs to repeat.)

- [ ] **Step 1: Write the failing builder tests**

Create `tests/testthat/test-trial-design.R`:

```r
# Trial design builders ---------------------------------------------------
# TA/TE/TI/TV/TS are built from spec tables, not CRF forms: the protocol
# lives in the spec, the builders reshape it, and validate_sdtm() recomputes
# what it can instead of trusting them.

test_that("map_te returns the SYNTH01 elements in spec order", {
  te <- map_te(spec_synth01)
  expect_equal(nrow(te), 2)
  expect_equal(te$ETCD, c("SCRN", "TREAT"))
  expect_equal(te$ELEMENT, c("Screening", "Treatment"))
  expect_equal(te$TEDUR, c("P2W", "P12W"))
  expect_true(all(te$STUDYID == "3021"))
  expect_true(all(te$DOMAIN == "TE"))
  expect_equal(attr(te$ETCD, "label"), "Element Code")
})

test_that("map_ta joins the arm decode and element names", {
  ta <- map_ta(spec_synth01)
  expect_equal(nrow(ta), 6)
  # ARMCD/ARM are carried from spec$arms; ELEMENT from spec$elements
  expect_setequal(unique(ta$ARMCD), c("PBO", "SYN50", "SYN100"))
  expect_setequal(unique(ta$ARM), c("Placebo", "SYN-101 50 mg", "SYN-101 100 mg"))
  expect_equal(sum(ta$ELEMENT == "Treatment"), 3)
  # the transition rule sits on SCRN, the branch on TREAT
  expect_equal(unique(ta$TATRANS[ta$ETCD == "SCRN"]), "Randomised")
  expect_true(all(is.na(ta$TATRANS[ta$ETCD == "TREAT"])))
  expect_true(all(!is.na(ta$TABRANCH[ta$ETCD == "TREAT"])))
  # each arm walks its elements in order
  expect_true(all(ta$TAETORD[ta$ETCD == "SCRN"] == 1L))
  expect_true(all(ta$TAETORD[ta$ETCD == "TREAT"] == 2L))
  expect_equal(attr(ta$TAETORD, "label"), "Planned Order of Element within Arm")
})

test_that("trial design builders emit XPT-safe names and labels", {
  for (df in list(map_te(spec_synth01), map_ta(spec_synth01))) {
    expect_true(all(nchar(names(df)) <= 8))
    long <- keep(var_label(df), \(l) !is.null(l) && nchar(l) > 40)
    expect_length(long, 0)
  }
})
```

- [ ] **Step 2: Run to verify failure**

Run: `Rscript -e 'devtools::test(filter = "^trial-design$")'`
Expected: FAIL — `map_te` not found.

- [ ] **Step 3: Create `R/trial-design.R` with both builders**

```r
# ============================================================================
# Title:   Trial design domains - TA (Trial Arms) and TE (Trial Elements)
# Purpose: Trial design is protocol fact, not collected data: it never
#          touches the mapping engine. The builders reshape the spec's
#          `elements` / `ta` tables; validate_sdtm() recomputes what it can
#          instead of trusting them. TV/TI/TS live further down this file.
# ============================================================================

#' Map the Trial Elements domain
#'
#' One record per trial element, straight from `spec$elements`.
#'
#' @param spec A `study_spec` (see [new_study_spec()])
#' @return The labelled SDTM TE tibble.
#' @examples
#' te <- map_te(spec_synth01)
#' te[, c("ETCD", "ELEMENT", "TEDUR")]
#' @export
map_te <- function(spec) {
  spec$elements |>
    mutate(STUDYID = spec$study$STUDYID, DOMAIN = "TE") |>
    transmute(STUDYID, DOMAIN, ETCD, ELEMENT, TESTRL, TEENRL, TEDUR) |>
    apply_labels(c(
      STUDYID = "Study Identifier",
      DOMAIN  = "Domain Abbreviation",
      ETCD    = "Element Code",
      ELEMENT = "Description of Element",
      TESTRL  = "Rule for Start of Element",
      TEENRL  = "Rule for End of Element",
      TEDUR   = "Planned Duration of Element"
    ))
}

#' Map the Trial Arms domain
#'
#' One record per arm per element in planned order. ARMCD and EPOCH come
#' from `spec$ta`; ARM is joined from `spec$arms` and ELEMENT from
#' `spec$elements`, so neither is repeated in a spec table that could
#' drift.
#'
#' @param spec A `study_spec` (see [new_study_spec()])
#' @return The labelled SDTM TA tibble.
#' @examples
#' ta <- map_ta(spec_synth01)
#' ta[, c("ARMCD", "TAETORD", "ETCD", "EPOCH")]
#' @export
map_ta <- function(spec) {
  spec$ta |>
    left_join(spec$arms |> select(ARMCD_DECODE, ARMCD, ARM),
              by = "ARMCD") |>
    left_join(spec$elements |> select(ETCD, ELEMENT), by = "ETCD") |>
    mutate(STUDYID = spec$study$STUDYID, DOMAIN = "TA") |>
    arrange(ARMCD, TAETORD) |>
    transmute(STUDYID, DOMAIN, ARMCD, ARM, TAETORD, ETCD, ELEMENT,
              TABRANCH, TATRANS, EPOCH) |>
    apply_labels(c(
      STUDYID   = "Study Identifier",
      DOMAIN    = "Domain Abbreviation",
      ARMCD     = "Planned Arm Code",
      ARM       = "Description of Planned Arm",
      TAETORD   = "Planned Order of Element within Arm",
      ETCD      = "Element Code",
      ELEMENT   = "Description of Element",
      TABRANCH  = "Branch",
      TATRANS = "Transition Rule",
      EPOCH     = "Epoch"
    ))
}
```

- [ ] **Step 4: Add the masked names to `R/globals.R`**

In the `utils::globalVariables(c(...))` vector (alphabetical spots): add `EPOCH` is already present; add `ETCD`, `TAETORD`, `TABRANCH`, `TATRANS`, `ARMCD_DECODE` (used data-masked in the `left_join` `by` only — `by` is not data-masked, so ARMCD_DECODE is *not* needed; only add names appearing bare in `mutate`/`transmute`/`arrange`/`select` on a data frame: `ETCD`, `ELEMENT`, `TESTRL`, `TEENRL`, `TEDUR`, `TAETORD`, `TABRANCH`, `TATRANS`). Insert them alphabetically.

- [ ] **Step 5: Run the builder tests**

Run: `Rscript -e 'devtools::test(filter = "^trial-design$")'`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add R/trial-design.R R/globals.R tests/testthat/test-trial-design.R
git commit -m "feat(trial-design): map_te() and map_ta()"
```

---

### Task 5: `map_ti()` and `map_ts()`

**Files:**
- Modify: `R/trial-design.R` (append)
- Modify: `R/globals.R`
- Test: `tests/testthat/test-trial-design.R` (append)

**Interfaces:**
- Consumes: `spec$ie`, `spec$ts`, `spec$study$STUDYID`.
- Produces: `map_ti(spec)` (STUDYID, DOMAIN, IETESTCD, IETEST, IECAT) and `map_ts(spec)` (STUDYID, DOMAIN, TSPARMCD, TSPARM, TSVAL, TSVALNF).

- [ ] **Step 1: Write the failing tests (append to `test-trial-design.R`)**

```r
test_that("map_ti returns the SYNTH01 criteria with CT categories", {
  ti <- map_ti(spec_synth01)
  expect_equal(nrow(ti), 6)
  expect_setequal(unique(ti$IECAT), c("INCLUSION", "EXCLUSION"))
  expect_equal(sum(ti$IECAT == "INCLUSION"), 3)
  expect_equal(sum(ti$IECAT == "EXCLUSION"), 3)
  expect_equal(ti$IETESTCD[1], "INC1")
  expect_equal(attr(ti$IETESTCD, "label"), "Incl/Excl Criterion Short Name")
})

test_that("map_ts returns the SYNTH01 parameters with values", {
  ts <- map_ts(spec_synth01)
  expect_equal(nrow(ts), 7)
  expect_setequal(ts$TSPARMCD,
                  c("TITLE", "PHASE", "SPONSOR", "INDIC", "TRT",
                    "NARMS", "PLANSUB"))
  expect_equal(ts$TSVAL[ts$TSPARMCD == "NARMS"], "3")
  expect_equal(ts$TSVAL[ts$TSPARMCD == "PLANSUB"], "24")
  # exactly one of TSVAL / TSVALNF is filled on every row
  expect_true(all(!is.na(ts$TSVAL) & ts$TSVAL != ""))
  expect_true(all(is.na(ts$TSVALNF) | ts$TSVALNF == ""))
})
```

- [ ] **Step 2: Run to verify failure**

Run: `Rscript -e 'devtools::test(filter = "^trial-design$")'`
Expected: FAIL — `map_ti` not found.

- [ ] **Step 3: Append the builders to `R/trial-design.R`**

```r
#' Map the Trial Inclusion/Exclusion Criteria domain
#'
#' One record per criterion, straight from `spec$ie`.
#'
#' @param spec A `study_spec` (see [new_study_spec()])
#' @return The labelled SDTM TI tibble.
#' @examples
#' ti <- map_ti(spec_synth01)
#' ti[, c("IETESTCD", "IECAT")]
#' @export
map_ti <- function(spec) {
  spec$ie |>
    mutate(STUDYID = spec$study$STUDYID, DOMAIN = "TI") |>
    transmute(STUDYID, DOMAIN, IETESTCD, IETEST, IECAT) |>
    apply_labels(c(
      STUDYID   = "Study Identifier",
      DOMAIN    = "Domain Abbreviation",
      IETESTCD  = "Incl/Excl Criterion Short Name",
      IETEST    = "Incl/Excl Criterion Test",
      IECAT     = "Incl/Excl Criterion Category"
    ))
}

#' Map the Trial Summary domain
#'
#' One record per trial summary parameter, straight from `spec$ts`. The
#' value lives in TSVAL and TSVALNF is the null flavor when no value can
#' be provided - exactly one of the two is filled per row (checked at
#' construction). NARMS and PLANSUB are cross-checked against `spec$arms`
#' and `spec$study$n` by `validate_sdtm()`, not recomputed here: the
#' output is the spec, and the validator catches a spec that disagrees
#' with itself.
#'
#' @param spec A `study_spec` (see [new_study_spec()])
#' @return The labelled SDTM TS tibble.
#' @examples
#' ts <- map_ts(spec_synth01)
#' ts[, c("TSPARMCD", "TSVAL")]
#' @export
map_ts <- function(spec) {
  spec$ts |>
    mutate(STUDYID = spec$study$STUDYID, DOMAIN = "TS") |>
    transmute(STUDYID, DOMAIN, TSPARMCD, TSPARM, TSVAL, TSVALNF) |>
    apply_labels(c(
      STUDYID  = "Study Identifier",
      DOMAIN   = "Domain Abbreviation",
      TSPARMCD = "Trial Summary Parameter Short Name",
      TSPARM   = "Trial Summary Parameter",
      TSVAL    = "Parameter Value",
      TSVALNF  = "Null Flavor"
    ))
}
```

- [ ] **Step 4: Add `IETESTCD`, `IETEST`, `IECAT`, `TSPARMCD`, `TSPARM`, `TSVAL`, `TSVALNF` to `R/globals.R`**

Alphabetical insertion into the `globalVariables` vector.

- [ ] **Step 5: Run the tests**

Run: `Rscript -e 'devtools::test(filter = "^trial-design$")'`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add R/trial-design.R R/globals.R tests/testthat/test-trial-design.R
git commit -m "feat(trial-design): map_ti() and map_ts()"
```

---

### Task 6: `map_tv()`

**Files:**
- Modify: `R/trial-design.R` (append)
- Test: `tests/testthat/test-trial-design.R` (append)

**Interfaces:**
- Consumes: `spec$visits` (Folder, VISITNUM, VISIT, EPOCH, TargetDays).
- Produces: `map_tv(spec)` (STUDYID, DOMAIN, VISITNUM, VISIT, VISITDY, EPOCH). **VISITDY uses the package's no-day-0 planned-day rule — the same convention `map_sv()` applies** (`TargetDays >= 0 -> TargetDays + 1`, else `TargetDays`), so a planned day of 0 never appears and TV agrees with SV.

- [ ] **Step 1: Write the failing test (append)**

```r
test_that("map_tv derives the planned visits from spec$visits", {
  tv <- map_tv(spec_synth01)
  expect_equal(nrow(tv), 6)
  expect_equal(tv$VISITNUM, 1:6)
  expect_equal(tv$VISIT[1], "SCREENING")
  expect_equal(tv$EPOCH[tv$VISIT == "BASELINE"], "TREATMENT")
  # VISITDY follows the no-day-0 rule map_sv() uses for planned days:
  # screening (day -14) stays negative, baseline (day 0) becomes day 1
  expect_equal(tv$VISITDY, c(-14L, 1L, 15L, 29L, 57L, 85L))
  expect_equal(attr(tv$VISITDY, "label"), "Planned Study Day of Visit")
})
```

- [ ] **Step 2: Run to verify failure**

Run: `Rscript -e 'devtools::test(filter = "^trial-design$")'`
Expected: FAIL — `map_tv` not found.

- [ ] **Step 3: Append the builder to `R/trial-design.R`**

```r
#' Map the Trial Visits domain
#'
#' One record per planned visit. TV has no spec table of its own: it is a
#' transform of the `visits` table the scheduled-visit domains already
#' read - VISITNUM, VISIT and EPOCH carry over, and VISITDY is TargetDays
#' under the same no-day-0 rule `map_sv()` applies to planned days.
#'
#' @param spec A `study_spec` (see [new_study_spec()])
#' @return The labelled SDTM TV tibble.
#' @examples
#' tv <- map_tv(spec_synth01)
#' tv[, c("VISITNUM", "VISIT", "VISITDY", "EPOCH")]
#' @export
map_tv <- function(spec) {
  spec$visits |>
    mutate(
      STUDYID = spec$study$STUDYID,
      DOMAIN  = "TV",
      # Planned study day: same no-day-0 rule as derive_dy()/map_sv()
      VISITDY = if_else(TargetDays >= 0L, TargetDays + 1L, TargetDays)
    ) |>
    arrange(VISITNUM) |>
    transmute(STUDYID, DOMAIN, VISITNUM, VISIT, VISITDY, EPOCH) |>
    apply_labels(c(
      STUDYID  = "Study Identifier",
      DOMAIN   = "Domain Abbreviation",
      VISITNUM = "Visit Number",
      VISIT    = "Visit Name",
      VISITDY  = "Planned Study Day of Visit",
      EPOCH    = "Epoch"
    ))
}
```

(`VISITNUM`, `VISIT`, `VISITDY`, `EPOCH`, `TargetDays` are all already declared in `globals.R` — nothing to add.)

- [ ] **Step 4: Run the tests**

Run: `Rscript -e 'devtools::test(filter = "^trial-design$")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/trial-design.R tests/testthat/test-trial-design.R
git commit -m "feat(trial-design): map_tv() from the visit schedule"
```

---

### Task 7: Wire the builders into `build_all()`

**Files:**
- Modify: `R/build-all.R` (mapping block ~line 59, list ~line 61, roxygen ~line 25)
- Modify: `tests/testthat/test-build-all.R` (rds set line 13, counts lines 19, 35)
- Test: `tests/testthat/test-trial-design.R` (append cross-study test)

**Interfaces:**
- Consumes: the five builders.
- Produces: `build_all()` returns `$sdtm` with 19 domains — the original 14 plus `TA, TE, TI, TV, TS` appended after `RELREC` in that order.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-trial-design.R`:

```r
test_that("build_all returns the trial design domains for both studies", {
  out <- file.path(tempdir(), "td-build")
  on.exit(unlink(out, recursive = TRUE, force = TRUE), add = TRUE)

  ext1 <- file.path(out, "rave1")
  suppressMessages(generate_rave_extract(out = ext1))
  built1 <- build_all(ext1)
  expect_setequal(names(built1$sdtm),
                  c("DM", "EX", "VS", "AE", "CM", "DS", "SV", "LB", "MH",
                    "SUPPDM", "SUPPAE", "SUPPEX", "CO", "RELREC",
                    "TA", "TE", "TI", "TV", "TS"))
  expect_equal(nrow(built1$sdtm$TA), 6)
  expect_equal(nrow(built1$sdtm$TE), 2)
  expect_equal(nrow(built1$sdtm$TI), 6)
  expect_equal(nrow(built1$sdtm$TV), 6)
  expect_equal(nrow(built1$sdtm$TS), 7)

  ext2 <- file.path(out, "rave2")
  suppressMessages(generate_rave_extract(out = ext2, study = "SYNTH02"))
  built2 <- suppressMessages(build_all(ext2, spec = spec_synth02))
  expect_equal(nrow(built2$sdtm$TA), 8)
  expect_equal(nrow(built2$sdtm$TV), 6)
  expect_equal(nrow(built2$sdtm$TS), 7)
})
```

Update `tests/testthat/test-build-all.R`:

Line 13–16, extend the expected rds set:

```r
  expect_setequal(list.files(file.path(out, "sdtm"), pattern = "[.]rds$"),
                  c("ae.rds", "cm.rds", "co.rds", "dm.rds", "ds.rds",
                    "ex.rds", "lb.rds", "mh.rds", "relrec.rds", "suppae.rds",
                    "suppdm.rds", "suppex.rds", "sv.rds", "ta.rds",
                    "te.rds", "ti.rds", "ts.rds", "tv.rds", "vs.rds"))
```

Line 19: `expect_equal(length(list.files(file.path(out, "sdtm", "xpt"))), 19)`.

**Leave line 35 (the ItemGroupDef count) at 14 for now** — it stays true because the define.xml entries only land in Task 9, which is also where this assertion moves to 19. Every commit stays green.

- [ ] **Step 2: Run to verify state**

Run: `Rscript -e 'devtools::test(filter = "^trial-design$")'`
Expected: FAIL — `build_all` has no trial design domains yet.

- [ ] **Step 3: Wire `build_all()`**

In `R/build-all.R`, after `relrec <- map_relrec(ae, cm, spec)`:

```r
  # Trial design: protocol facts from the spec, no CRF forms involved
  ta <- map_ta(spec)
  te <- map_te(spec)
  ti <- map_ti(spec)
  tv <- map_tv(spec)
  ts <- map_ts(spec)
```

and extend the list:

```r
  sdtm <- list(DM = dm, EX = ex, VS = vs, AE = ae, CM = cm, DS = ds, SV = sv,
               LB = lb, MH = mh, SUPPDM = suppdm, SUPPAE = suppae,
               SUPPEX = suppex, CO = co, RELREC = relrec,
               TA = ta, TE = te, TI = ti, TV = tv, TS = ts)
```

Update the roxygen `@return` line: "A list with `sdtm` (named list of the 19 mapped domains)".

- [ ] **Step 4: Run the affected test files**

Run: `Rscript -e 'devtools::test(filter = "^trial-design$|^build-all$|^synth02$")'`
Expected: PASS everywhere.

- [ ] **Step 5: Commit**

```bash
git add R/build-all.R tests/testthat/test-build-all.R tests/testthat/test-trial-design.R
git commit -m "feat(build): wire the trial design domains into build_all()"
```

---

### Task 8: Validate the trial design domains

**Files:**
- Modify: `R/validate-sdtm.R` (`.sdtm_req_static` ~line 14; new checks before `report <- bind_rows(issues)` ~line 487; roxygen ~line 54)
- Test: `tests/testthat/test-trial-design.R` (append meta-tests)

**Interfaces:**
- Consumes: the 19-domain list from `build_all()`; optional `spec`.
- Produces: new issue `check` codes: `trial-design-empty` (WARN), `ta-dup-key`, `ta-etcd-not-in-te`, `ta-armcd-not-in-arms`, `ta-epoch-not-in-visits`, `te-etcd-not-unique`, `ti-testcd-not-unique`, `iecat-bad-value`, `tv-visit-not-unique`, `tv-vs-spec-visits`, `sv-visit-not-in-tv`, `ts-parmcd-not-unique`, `ts-valnf-xor`, `ts-narms-mismatch`, `ts-plansub-mismatch` (all ERROR).

- [ ] **Step 1: Write the failing meta-tests (append to `test-trial-design.R`)**

```r
# Meta-tests: corrupt a trial design domain, assert the validator trips ---

td_fixture <- function() {
  out <- file.path(tempdir(), "td-meta")
  dir.create(out, showWarnings = FALSE)
  ext <- file.path(out, "rave")
  if (!dir.exists(ext)) suppressMessages(generate_rave_extract(out = ext))
  build_all(ext)
}

test_that("a TA element missing from TE trips ta-etcd-not-in-te", {
  domains <- td_fixture()$sdtm
  domains$TA$ETCD[1] <- "NOPE"
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("ta-etcd-not-in-te" %in% issues$check)
})

test_that("a NARMS row disagreeing with spec$arms trips ts-narms-mismatch", {
  domains <- td_fixture()$sdtm
  domains$TS$TSVAL[domains$TS$TSPARMCD == "NARMS"] <- "9"
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("ts-narms-mismatch" %in% issues$check)
  expect_error(stop_on_error(issues, "meta"), "validation error")
})

test_that("a PLANSUB row disagreeing with study$n trips ts-plansub-mismatch", {
  domains <- td_fixture()$sdtm
  domains$TS$TSVAL[domains$TS$TSPARMCD == "PLANSUB"] <- "999"
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("ts-plansub-mismatch" %in% issues$check)
})

test_that("TV drifting from spec$visits trips tv-vs-spec-visits", {
  domains <- td_fixture()$sdtm
  domains$TV$VISITDY[1] <- 99L
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("tv-vs-spec-visits" %in% issues$check)
})

test_that("an SV visit that TV never planned trips sv-visit-not-in-tv", {
  domains <- td_fixture()$sdtm
  ghost <- domains$SV[1, ] |> mutate(VISITNUM = 99L)
  domains$SV <- bind_rows(domains$SV, ghost)
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("sv-visit-not-in-tv" %in% issues$check)
})

test_that("duplicate TS parameters and valflavor violations trip", {
  domains <- td_fixture()$sdtm
  domains$TS <- bind_rows(domains$TS, domains$TS[domains$TS$TSPARMCD == "NARMS", ])
  expect_true("ts-parmcd-not-unique" %in%
                validate_sdtm(domains, spec_synth01)$check)

  domains <- td_fixture()$sdtm
  domains$TS$TSVALNF[domains$TS$TSPARMCD == "NARMS"] <- "N/A"
  expect_true("ts-valnf-xor" %in% validate_sdtm(domains, spec_synth01)$check)
})

test_that("a bad IECAT trips iecat-bad-value", {
  domains <- td_fixture()$sdtm
  domains$TI$IECAT[1] <- "MAYBE"
  expect_true("iecat-bad-value" %in% validate_sdtm(domains, spec_synth01)$check)
})

test_that("dropping a required trial design variable trips required-vars", {
  domains <- td_fixture()$sdtm
  domains$TA$EPOCH <- NULL
  expect_true(any(validate_sdtm(domains, spec_synth01)$check == "required-vars" &
                    validate_sdtm(domains, spec_synth01)$domain == "TA"))
})

test_that("a clean build has zero trial design findings", {
  domains <- td_fixture()$sdtm
  expect_equal(nrow(validate_sdtm(domains, spec_synth01)), 0)
})
```

- [ ] **Step 2: Run to verify failure**

Run: `Rscript -e 'devtools::test(filter = "^trial-design$")'`
Expected: the meta-tests FAIL (no such checks yet); the last test ("clean build") PASSES.

- [ ] **Step 3: Implement the checks in `R/validate-sdtm.R`**

3a. Extend `.sdtm_req_static`:

```r
  TA = c("STUDYID", "DOMAIN", "ARMCD", "ARM", "TAETORD", "ETCD", "ELEMENT",
         "EPOCH"),
  TE = c("STUDYID", "DOMAIN", "ETCD", "ELEMENT"),
  TI = c("STUDYID", "DOMAIN", "IETESTCD", "IETEST", "IECAT"),
  TV = c("STUDYID", "DOMAIN", "VISITNUM", "VISIT", "VISITDY", "EPOCH"),
  TS = c("STUDYID", "DOMAIN", "TSPARMCD", "TSPARM", "TSVAL")
```

3b. Insert before `report <- bind_rows(issues)`:

```r
  # Trial design --------------------------------------------------------------
  # TA/TE/TI/TV/TS are built from spec tables, so the checks recompute what
  # the spec already knows instead of trusting the builders - the same
  # "read the spec twice" discipline the CT and ADaM checks apply.
  for (d in c("TA", "TE", "TI", "TV", "TS")) {
    df <- domains[[d]]
    if (is.null(df) || nrow(df) == 0) {
      add(d, "WARN", "trial-design-empty", "0 rows")
    }
  }

  ta_df <- domains$TA
  te_df <- domains$TE
  if (!is.null(ta_df) && nrow(ta_df) > 0) {
    dup <- ta_df |> count(ARMCD, TAETORD, name = ".n") |> filter(.n > 1)
    if (nrow(dup) > 0) {
      add("TA", "ERROR", "ta-dup-key",
          sprintf("%d duplicated ARMCD/TAETORD key(s)", nrow(dup)))
    }
    if (!is.null(te_df)) {
      orphan <- setdiff(unique(ta_df$ETCD), unique(te_df$ETCD))
      if (length(orphan) > 0) {
        add("TA", "ERROR", "ta-etcd-not-in-te", str_flatten_comma(orphan))
      }
    }
    if (!is.null(spec)) {
      unknown_arm <- setdiff(unique(ta_df$ARMCD), unique(spec$arms$ARMCD))
      if (length(unknown_arm) > 0) {
        add("TA", "ERROR", "ta-armcd-not-in-arms",
            str_flatten_comma(unknown_arm))
      }
      unknown_ep <- setdiff(unique(ta_df$EPOCH), unique(spec$visits$EPOCH))
      if (length(unknown_ep) > 0) {
        add("TA", "ERROR", "ta-epoch-not-in-visits",
            str_flatten_comma(unknown_ep))
      }
    }
  }
  if (!is.null(te_df) && anyDuplicated(te_df$ETCD) > 0) {
    add("TE", "ERROR", "te-etcd-not-unique", "duplicated ETCD")
  }

  ti_df <- domains$TI
  if (!is.null(ti_df) && nrow(ti_df) > 0) {
    if (anyDuplicated(ti_df$IETESTCD) > 0) {
      add("TI", "ERROR", "ti-testcd-not-unique", "duplicated IETESTCD")
    }
    bad_cat <- setdiff(unique(ti_df$IECAT), c("INCLUSION", "EXCLUSION"))
    if (length(bad_cat) > 0) {
      add("TI", "ERROR", "iecat-bad-value", str_flatten_comma(bad_cat))
    }
  }

  tv_df <- domains$TV
  if (!is.null(tv_df) && nrow(tv_df) > 0) {
    dup <- tv_df |> count(VISITNUM, name = ".n") |> filter(.n > 1)
    if (nrow(dup) > 0) {
      add("TV", "ERROR", "tv-visit-not-unique",
          sprintf("%d duplicated VISITNUM key(s)", nrow(dup)))
    }
    if (!is.null(spec)) {
      planned <- spec$visits |>
        transmute(
          VISITNUM, VISIT,
          # the same no-day-0 rule map_tv() and map_sv() apply
          VISITDY = if_else(TargetDays >= 0L, TargetDays + 1L, TargetDays)
        )
      drift <- planned |>
        anti_join(tv_df, by = c("VISITNUM", "VISIT", "VISITDY")) |>
        bind_rows(tv_df |> anti_join(planned, by = c("VISITNUM", "VISIT", "VISITDY")))
      if (nrow(drift) > 0) {
        add("TV", "ERROR", "tv-vs-spec-visits",
            sprintf("%d TV row(s) disagree with spec$visits", nrow(drift)))
      }
    }
  }
  if (!is.null(domains$SV) && !is.null(tv_df)) {
    unplanned <- domains$SV |> distinct(VISITNUM) |> anti_join(tv_df, by = "VISITNUM")
    if (nrow(unplanned) > 0) {
      add("SV", "ERROR", "sv-visit-not-in-tv",
          str_flatten_comma(as.character(unplanned$VISITNUM)))
    }
  }

  ts_df <- domains$TS
  if (!is.null(ts_df) && nrow(ts_df) > 0) {
    dup <- ts_df |> count(TSPARMCD, name = ".n") |> filter(.n > 1)
    if (nrow(dup) > 0) {
      add("TS", "ERROR", "ts-parmcd-not-unique",
          sprintf("%d duplicated TSPARMCD key(s)", nrow(dup)))
    }
    both_filled <- ts_df |>
      filter(!is.na(TSVAL) & TSVAL != "", !is.na(TSVALNF) & TSVALNF != "")
    both_blank <- ts_df |>
      filter(is.na(TSVAL) | TSVAL == "", is.na(TSVALNF) | TSVALNF == "")
    if (nrow(both_filled) + nrow(both_blank) > 0) {
      add("TS", "ERROR", "ts-valnf-xor",
          "TSVAL and TSVALNF: exactly one must be filled per row")
    }
    if (!is.null(spec)) {
      narms <- ts_df$TSVAL[ts_df$TSPARMCD == "NARMS"]
      if (length(narms) >= 1 && narms[1] != as.character(nrow(spec$arms))) {
        add("TS", "ERROR", "ts-narms-mismatch",
            sprintf("TS NARMS '%s' but spec$arms has %d row(s)",
                    narms[1], nrow(spec$arms)))
      }
      plansub <- ts_df$TSVAL[ts_df$TSPARMCD == "PLANSUB"]
      if (length(plansub) >= 1 && plansub[1] != as.character(spec$study$n)) {
        add("TS", "ERROR", "ts-plansub-mismatch",
            sprintf("TS PLANSUB '%s' but spec$study$n is %d",
                    plansub[1], spec$study$n))
      }
    }
  }
```

3c. Extend the `validate_sdtm()` roxygen sentence of checks with: "trial design integrity (TA→TE, TA against `spec$arms`/`spec$visits`, TV against `spec$visits`, SV visits planned in TV, TS parameter consistency against `spec$arms`/`spec$study$n`)".

- [ ] **Step 4: Run the full trial-design and validator suites**

Run: `Rscript -e 'devtools::test(filter = "^trial-design$|^validators$|^build-all$|^synth02$")'`
Expected: PASS (the synth02 `nrow(validate_sdtm(...)) == 0` assertion must still hold — both shipped specs have full trial design, so `trial-design-empty` never fires for them).

- [ ] **Step 5: Commit**

```bash
git add R/validate-sdtm.R tests/testthat/test-trial-design.R
git commit -m "feat(validate): trial design integrity checks"
```

---

### Task 9: define.xml coverage

**Files:**
- Modify: `R/define-xml.R` (`key_spec` line 30, `structure_spec` line 48, origins lines 67–85, `codelist_vars` line 102, `var_origin` call site line 191)

**Interfaces:**
- Consumes: the 19-domain list.
- Produces: ItemGroupDefs for all 19 domains; `IECAT` curated codelist (CodeList count 9 → 10).

- [ ] **Step 1: Write the failing assertions**

In `tests/testthat/test-build-all.R`, update the define test: line 35 moves from 14 to 19,

```r
  expect_equal(length(xml2::xml_find_all(doc, "//d1:ItemGroupDef", ns)), 19)
```

and extend the same `test_that` with the trial design expectations:

```r
  # the trial design domains carry keys and structures like the rest
  igd_ta <- xml2::xml_find_first(doc, "//d1:ItemGroupDef[@Name='TA']", ns)
  expect_equal(xml2::xml_attr(igd_ta, "Domain"), "TA")
  expect_equal(length(xml2::xml_find_all(doc, "//d1:CodeList", ns)), 10)
```

- [ ] **Step 2: Run to verify failure**

Run: `Rscript -e 'devtools::test(filter = "^build-all$")'`
Expected: FAIL (ItemGroupDef 14, CodeList 9).

- [ ] **Step 3: Implement in `R/define-xml.R`**

3a. `key_spec` additions:

```r
    TA     = c("STUDYID", "ARMCD", "TAETORD"),
    TE     = c("STUDYID", "ETCD"),
    TI     = c("STUDYID", "IETESTCD"),
    TV     = c("STUDYID", "VISITNUM"),
    TS     = c("STUDYID", "TSPARMCD")
```

3b. `structure_spec` additions:

```r
    TA     = "One record per arm per element",
    TE     = "One record per trial element",
    TI     = "One record per inclusion/exclusion criterion",
    TV     = "One record per planned visit",
    TS     = "One record per trial summary parameter"
```

3c. Origins. `VISITDY` already resolves to `Derived` through the existing `DY` suffix rule — leave it there (it is a computed day, the same rule SV's VISITDY gets today). Add to `origin_named` (Protocol origin; all these names are unique to trial design, so a global entry cannot mislabel another domain):

```r
    ETCD = "Protocol", ELEMENT = "Protocol", TESTRL = "Protocol",
    TEENRL = "Protocol", TEDUR = "Protocol", TAETORD = "Protocol",
    TABRANCH = "Protocol", TATRANS = "Protocol", IETESTCD = "Protocol",
    IETEST = "Protocol", IECAT = "Protocol", TSPARMCD = "Protocol",
    TSPARM = "Protocol", TSVAL = "Protocol", TSVALNF = "Protocol"
```

Names shared with other domains (ARMCD, ARM, EPOCH in TA; VISITNUM, VISIT in TV) get a per-domain override so DM/SV declarations are untouched. Above `var_origin`:

```r
  # Names shared with collected domains get a per-domain override: TA's
  # ARMCD is protocol fact, DM's stays CRF/Assigned as declared above.
  origin_by_domain <- list(
    TA = c(ARMCD = "Protocol", ARM = "Protocol", EPOCH = "Protocol"),
    TV = c(VISITNUM = "Protocol", VISIT = "Protocol")
  )
```

and change the function + call site:

```r
  var_origin <- function(var, domain) {
    dom_hit <- origin_by_domain[[domain]][var]
    if (length(dom_hit) == 1 && !is.na(dom_hit)) return(unname(dom_hit))
    hit <- origin_suffix[str_ends(var, names(origin_suffix))]
    if (length(hit) > 0) return(unname(hit[[1]]))
    if (var %in% names(origin_named)) return(unname(origin_named[[var]]))
    "CRF"
  }
```

Call site (inside the domain loop): `xml2::xml_add_child(it, "def:Origin", Type = var_origin(var, d))`.

3d. Add `"IECAT"` to `codelist_vars` (its INCLUSION/EXCLUSION values are observed CT worth declaring).

- [ ] **Step 4: Run the build-all tests**

Run: `Rscript -e 'devtools::test(filter = "^build-all$")'`
Expected: PASS (ItemGroupDef 19, CodeList 10).

- [ ] **Step 5: Commit**

```bash
git add R/define-xml.R tests/testthat/test-build-all.R
git commit -m "feat(define-xml): document the trial design domains"
```

---

### Task 10: Freeze references, full suite, check, lint

**Files:**
- Create: `tests/reference/sdtm/{ta,te,ti,tv,ts}.rds` (via the committed script)

- [ ] **Step 1: Regenerate the references**

Run: `Rscript tests/update_reference.R`
Expected: `update_reference: 19 sdtm dataset(s) written` (and 4 adam).

- [ ] **Step 2: Verify the 14 existing references are byte-identical**

Run: `git status --porcelain tests/reference`
Expected: exactly five new untracked files (`ta.rds`, `te.rds`, `ti.rds`, `tv.rds`, `ts.rds`) and **zero modified** files. If any existing file shows as modified, stop and diff before continuing — a changed reference means an earlier task drifted.

- [ ] **Step 3: Run the full suite**

Run: `Rscript -e 'devtools::test()'`
Expected: `Fail: 0` — including the regression harness now comparing all 19 sdtm references.

- [ ] **Step 4: Document, check, lint**

Run:
```bash
Rscript -e 'devtools::document()'
Rscript -e 'devtools::check(error_on = "note")' 2>&1 | tail -30
Rscript -e 'lintr::lint_package()'
```
Expected: no ERRORs/WARNINGs; no new NOTEs beyond the usual; lint clean (fix any style findings in the new files).

- [ ] **Step 5: Commit**

```bash
git add tests/reference R/man man
git commit -m "test: freeze the trial design reference outputs"
```

---

### Task 11: Docs — README, NEWS, vignettes, design-doc amendments

**Files:**
- Modify: `README.md` (lines 28–30, 68), `NEWS.md` (top), `vignettes/new-study.Rmd`, `vignettes/design.Rmd`, `docs/superpowers/specs/2026-09-05-trial-design-domains-design.md`

- [ ] **Step 1: README**

Line 28–30: replace "**Maps** 14 SDTM domains (DM, EX, VS, AE, CM, DS, SV, LB, MH, SUPPDM, SUPPAE, SUPPEX, CO, RELREC) through a study spec" with:

```markdown
- **Maps** 19 SDTM domains (DM, EX, VS, AE, CM, DS, SV, LB, MH, SUPPDM,
  SUPPAE, SUPPEX, CO, RELREC, TA, TE, TI, TV, TS) through a study spec:
```

Line 68: change the comment to `built$sdtm$DM      # 19 SDTM domains, validated`.

- [ ] **Step 2: NEWS.md**

Prepend:

```markdown
# edc2cdisc 0.3.1.9000

## Added

* The SDTMIG trial design family. Four optional spec tables (`elements`,
  `ta`, `ie`, `ts`) drive `map_te()`, `map_ta()`, `map_ti()` and
  `map_ts()`; `map_tv()` derives TV from the existing `visits` table.
  `build_all()` returns 19 SDTM domains, `build_define_xml()` documents
  them, and `validate_sdtm()` recomputes the derivable facts — NARMS
  against `spec$arms`, PLANSUB against `spec$study$n`, TV against
  `spec$visits`, and every SV visit planned in TV — instead of trusting
  the builders.
```

- [ ] **Step 3: `vignettes/new-study.Rmd`**

Insert a new section immediately before `## Changing a controlled-terminology mapping`:

````markdown
## Trial design (TA, TE, TI, TV, TS)

The trial design domains are protocol facts, so they live in the spec, not
in the mapping table. Four optional tables carry them: `elements` (ETCD,
ELEMENT, start/end rules, planned duration), `ta` (one row per arm per
element, with the branch and transition rules), `ie` (the criteria) and
`ts` (trial summary parameters). TV has no table: it is derived from the
`visits` table you already maintain, VISITDY included. Leave the tables
out and the domains come out empty (a validator warning tells you); fill
them in and `validate_sdtm()` cross-checks what it can — NARMS against
your `arms` table, PLANSUB against `study$n`, TV against `visits`.

```
````

- [ ] **Step 4: `vignettes/design.Rmd`**

Insert a new section immediately before `## The honest limit`:

````markdown
## Trial design is pure spec

TA, TE, TI, TV and TS never touch the mapping engine. Four optional spec
tables hold the protocol; TV is derived from the `visits` table the
scheduled-visit domains already read. The validators re-read the same
tables — NARMS against `spec$arms`, PLANSUB against `spec$study$n`, TV
against `spec$visits`, SV against TV — so a spec that lies about its own
design cannot pass both readers, which is the same discipline the ADaM
checks apply to the SDTM layer.
````

- [ ] **Step 5: Design-doc amendments** (keep the spec honest against three implementation discoveries)

In `docs/superpowers/specs/2026-09-05-trial-design-domains-design.md`:

5a. TV paragraph: replace "VISITNUM, VISIT, EPOCH carried over; VISITDY = TargetDays." with:

```
VISITNUM, VISIT, EPOCH carried over; VISITDY is TargetDays under the
package's no-day-0 planned-day rule (`TargetDays >= 0` → `+1`), the same
convention `map_sv()` already applies, so a planned day of 0 never
appears and TV agrees with SV.
```

5b. Builders paragraph: after "...protocol fact, not collected data.", add:

```
`map_ta()` joins ARM from `spec$arms` (an IG-required TA variable no `ta`
table row should repeat) and ELEMENT from `spec$elements`.
```

5c. TA required-variables list: add `ARM` after `ARMCD`. In the define.xml Origins paragraph: remove `VISITDY` from the Protocol-origin name list and append: "`VISITDY` stays on the existing `DY` suffix rule (`Derived`), which is what SV's VISITDY already gets."

- [ ] **Step 6: Rebuild docs and run the full suite one more time**

```bash
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test()'
```
Expected: `Fail: 0`.

- [ ] **Step 7: Commit**

```bash
git add README.md NEWS.md vignettes docs/superpowers/specs/2026-09-05-trial-design-domains-design.md R/man man
git commit -m "docs: trial design domains in README, NEWS, vignettes"
```

---

## Self-Review (done at plan time)

- **Spec coverage:** constructor tables + checks (Task 1), both shipped specs (2, 3), five builders (4–6), build_all wiring (7), all validator rules from the design (8), define.xml keys/structures/origins/IECAT codelist (9), reference freezing (10), every doc surface named in the design (11).
- **Deviations from the design doc, folded into Task 11's amendments:** VISITDY no-day-0 rule; ARM column on TA (+ added to the required list); VISITDY origin stays Derived via the DY suffix rule.
- **Type consistency:** builder signatures `map_*(spec)` used identically in Tasks 7 and 8; issue check names identical between Task 8's implementation and meta-tests; column sets consistent from Task 1's defaults through Tasks 4–7.

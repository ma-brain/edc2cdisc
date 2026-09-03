# PLAN.md — converting ravesdtm into the `edc2cdisc` R package

Feasibility assessment and phased conversion plan for turning the
scripts-based Rave→SDTM→ADaM training project into an R package.
Working name: **`edc2cdisc`** (verified free on CRAN; generic input side
avoids the Medidata Rave trademark, CDISC destination side covers SDTM,
ADaM and define.xml).

## Verdict

Highly feasible, and unusually low-risk for a scripts-to-package
conversion:

- Roughly half the code is already function-shaped and mostly pure —
  the 973-line generator (`05_generate_rave_extract.R`), all of
  `02_sdtm_helpers.R`, and `01_rave_io.R`.
- The project has accidentally built itself a regression harness: a
  seeded generator (`RAVE_SEED`) plus the two validators mean any
  refactor can be proven behavior-preserving by regenerating outputs
  and waldo-comparing every `.rds` against the last script-based run —
  the same technique used to verify the ADSL/ADAE refactors.

## What is already package-shaped

- **The generator** runs standalone with fallback defaults → becomes
  exported `generate_rave_extract(seed, n)`. Shipped synthetic data
  means examples, tests and vignettes work with no live Rave — the
  same play as pharmaverse's `{pharmaversesdtm}`.
- **The helpers** (`rave_dtc`, `impute_dtc`, `derive_dy_d`,
  `compute_age`, `make_supp`, …) are pure functions → move verbatim
  into `R/`, unit-test trivially.
- **The validators'** check lists are effectively a spec of the
  package's own guarantees → become testthat assertions.

## The real work — three refactors

1. **Kill the global environment threading.** Domain scripts read
   `STUDYID`, `subject_ref`, `visit_map`, `country_map`, `paths` from
   globals, and `check_ct` is defined twice (in `13_sdtm_ae.R` and
   `15_sdtm_ds.R`). Each script becomes `map_dm(extract, spec, refs)`
   → tibble, with refs passed explicitly (DM produces `subject_ref`;
   later domains consume it). `write_sdtm()` stops being baked into
   every script — pure derivation, thin I/O wrapper.
2. **Extract the study spec.** The codelist lookup tables
   (`ae_out_ct`, `ds_decod_ct`, `arm_map`, `visit_map`, `country_map`,
   SUPP qnams specs…) are scattered across scripts and `00_setup.R`.
   They become one `study_spec` object, with SYNTH01 shipped as package
   data — the step that delivers "a new study is a spec change".
3. **Validators become functions.** `22_validate.R` /
   `39_validate_adam.R` read from disk and `stop()`; in the package
   they become `validate_sdtm(x)` returning an issue tibble, with a
   `stop_on_error()` wrapper for the pipeline.

## Phases

| Phase | Content | Effort |
|---|---|---|
| 0 | Scaffold: `edc2cdisc` DESCRIPTION, `R/`, testthat, MIT license, roxygen; keep scripts runnable alongside | hours |
| 1 | Move `02` helpers in verbatim + unit tests per helper | hours |
| 2 | Generator → exported `generate_rave_extract()` + snapshot tests | hours |
| 3 | `01` reader → `read_clinical_view()` | hours |
| 4 | Spec extraction (the meaty one): constants + codelists → `study_spec`, ship `spec_synth01` | 1–2 sessions |
| 5 | Domain mappers 10–21 → functions, DM-first refs threading; file names lose their numbers (`R/map-dm.R`, not `10_sdtm_dm.R`) — run order lives in `build_all()`, not in names | the long tail, mechanical |
| 6 | Validators → `validate_sdtm()` / `validate_adam()` returning issue tibbles | short |
| 7 | `build_all()` pipeline + XPT writers replacing `run_all.R` | short |
| 8 | Regression tests: seeded snapshots per domain + diffdf check + meta-tests (corrupt an input, assert the validator catches it) | 1 session |
| 9 | Docs: "design decisions worth arguing with" → vignette, function docs, a "new study = new spec" vignette | 1 session |
| 10 | Optional CRAN submission (small data, MIT, tidyverse Imports with explicit `@importFrom`) | polish |

Phases 0–3 are a weekend. Phases 4–8 are the actual project, a few
focused sessions. Dependencies are tiny (dplyr, tidyr, stringr, purrr,
tibble, labelled, haven, readr); everything is R-only with no network;
the synthetic data is trivially small — no CRAN red flags.

## Sequencing

Do it on a branch off `main` **after ADVS lands**, so the package
starts from the complete SDTM+ADaM feature set rather than refactoring
twice.

## Main risk

The global-state removal in phases 4–5 is where silent breakage would
hide. Mitigation: run the packaged pipeline against the seeded extract
and waldo-compare every `.rds` against the last script-based outputs
before deleting them. If that passes, the conversion is provably
faithful.

## Design detail: the spec table (what phases 4–5 build)

The codebase is further along this road than it looks: `make_supp()` is
already spec-driven (the `qnams` tribble), and `visit_map`,
`country_map`, `arm_map` and the codelist lookups already exist as
explicit tables. The work is unifying them into one object plus a small
engine that reads it.

### The spec

One `study_spec` object — a list of tibbles, with SYNTH01 shipped as
the reference implementation:

```r
spec$study     # STUDYID, PROJECT, seed/n defaults
spec$sites     # SiteNumber -> site, country        (from 00_setup.R)
spec$arms      # ARMCD_DECODE -> ARMCD, ARM         (currently in 10_sdtm_dm.R)
spec$visits    # Folder -> VISITNUM/VISIT/EPOCH + TargetDays (from 00_setup.R)
spec$codelists # long: ct, rave_decode, cdisc_term  (ae_out_ct, ds_decod_ct, ...)
spec$supp      # rdomain, idvar, qnam, qlabel, src, qorig, qeval  (make_supp() specs)
spec$units     # domain, testcd/analyte, orresu -> factor, std_unit (LB/VS conversions)

spec$variables # THE spec table — one row per SDTM variable per domain:
#   domain    variable   crf_field            transform        ref
#   "DM"      "SUBJID"   "Subject"            "rename"         NA
#   "DM"      "SEX"      "SEX_DECODE"         "decode"         "ct_sex"
#   "DM"      "BRTHDTC"  "BRTHDAT_*"          "dtc"            NA
#   "DM"      "AGE"      NA                   "derivation"     "age_at_first_dose"
#   "AE"      "AETERM"   "AETERM"             "verbatim"       NA
#   "EX"      "EXDOSE"   "EXDOSE"             "numeric"        NA
```

The `transform` column names a small vocabulary the engine knows —
`rename`, `decode`, `dtc`, `yn`, `numeric`, `verbatim` (`str_squish` +
upper) — and `derivation` names a function from a registered derivation
set: `age_at_first_dose()`, `dy()`, `trtemfl()`, the screen-failure DS
reclassification, the SV header-block fold. Form-level structure rides
alongside: each form is typed `event` vs `log` (recordposition key);
VS/LB get a per-test pivot spec instead of per-variable rows.

### Why this works better than it usually does

- The engine is tiny — `map_domain()` is basically `transmute()` driven
  by the table, and every transform type except `derivation` already
  exists as a helper in `02_sdtm_helpers.R`.
- **The validators consume the same spec**: the required-variable lists
  (the `.req` lists in `22`/`39`) and the CT checks fall straight out
  of `spec$variables` and `spec$codelists`, so the spec can't silently
  lie — two things read it.
- The spec table is 90% of a define.xml (variable, origin, codelist,
  label), which collapses README next-step 5 into a by-product.

### The honest limit

"A new study is a spec change" is true for studies in the same CRF
family — same forms, different codelists, arms, visits, sites, SUPP
fields. It is **not** true when the CRF design changes: "which field
means dose administered" or "screen failure arrives on the
protocol-deviation code" are semantics, and semantics live in the
derivation registry as code that the spec *selects*. That boundary is a
feature: new study = new spec + (rarely) one or two new derivation
functions. Over-promising fully metadata-driven SDTM is how these
projects collapse — say the limit in the vignette.

### Proof path

Convert EX first (almost pure rename/decode/dtc rows), then DS (adds
the reclassification derivation), then AE — once those three run
through the engine with waldo-identical outputs, the pattern is proven
and the ugly ones (VS/LB pivots, SV fold) are just more spec rows plus
derivations.


# edc2cdisc <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- logo placeholder: no logo yet; the line above can be dropped until one exists -->

An R package that converts **EDC clinical view exports to CDISC SDTM and
ADaM**, driven by an explicit study specification. It grew out of a
script-based training project (Rave → SDTM → ADaM); the conversion plan and
its rationale live in [PLAN.md](PLAN.md).

The package is **R-only** and needs no live EDC system: a seeded generator
writes a synthetic clinical view extract (one quoted CSV per CRF form, plus
a data dictionary and codelist reference, each ending in an EOF marker row),
and the whole pipeline runs, validates and is tested against frozen
reference outputs without any network access.

## What it does

- **Generates** a synthetic extract with deliberate messiness — mixed units
  (lb/kg, F/C), partial and unknown dates, ongoing records, screen failures,
  a soft-deleted row, a fatal AE — so mappers exercise real-world shapes:
  `generate_rave_extract(out, seed, n)`.
- **Reads** clinical view CSVs defensively: everything as character on
  purpose, EOF truncation guard, inactive-record filtering:
  `read_clinical_view()`, `read_rave_extract()`.
- **Maps** 14 SDTM domains (DM, EX, VS, AE, CM, DS, SV, LB, MH, SUPPDM,
  SUPPAE, SUPPEX, CO, RELREC) through a study spec:
  `map_dm()`, `map_ex()`, … , `map_relrec()`.
- **Derives** the four core ADaM datasets: `derive_adsl()`, `derive_adae()`,
  `derive_advs()`, `derive_adlb()`.
- **Validates** both layers with checks that catch the mistakes this kind of
  project makes — recomputing derivations instead of trusting the build:
  `validate_sdtm()`, `validate_adam()`, `stop_on_error()`.
- **Writes** RDS + SAS v5 XPT and a define.xml stub with value-level
  metadata: `write_sdtm()`, `build_define_xml()`.
- **Orchestrates** all of the above in one call — the run order lives here,
  not in script file names: `build_all()`.

The study-specific configuration (sites, arms, visits, codelists, SUPP
qualifiers, unit conversions, form typing, per-test pivot specs and the
variable-level mapping table) lives in one `study_spec` object; the shipped
`spec_synth01` is the reference implementation. See
`vignette("new-study")` for what it takes to add a study, and
`vignette("design")` for the design decisions worth arguing with —
including the honest limit of "a new study is a spec change".

## Install

```r
# install.packages("remotes")
remotes::install_github("ma-brain/edc2cdisc")
```

## Quick start

```r
library(edc2cdisc)

# generate the synthetic extract, then run the whole pipeline
generate_rave_extract(out = tempdir())
built <- build_all(tempdir())

built$sdtm$DM      # 14 SDTM domains, validated
built$adam$ADSL    # ADSL / ADAE / ADVS / ADLB, validated

# or step by step, mapping only what you need
forms <- read_rave_extract(dir = tempdir())
dm   <- map_dm(forms$DM, forms$EX, forms$DS, spec_synth01)
refs <- subject_ref(dm)                 # feeds every --DY derivation
ex   <- map_ex(forms$EX, spec_synth01, refs)
```

## Regression guarantees

The seeded generator plus the validators make the whole conversion provably
behaviour-preserving: the package test suite regenerates the extract from
the committed seed, rebuilds every domain and waldo-compares each dataset
(values, types, order, variable labels) against frozen script-based
reference outputs. Meta-tests corrupt inputs and assert the validators
catch them.

## Legacy script pipeline

The original numbered scripts (`00_setup.R` … `39_validate_adam.R`,
`run_all.R`, `run_tests.R`) are kept runnable alongside the package during
the conversion; the package supersedes them. `tests/reference/*.rds` are
the script-based ground truth the package regression tests compare against.

# edc2cdisc 0.2.0

Proved the package's central claim - "a new study is a spec change" - with
a real second study, and fixed the hard-coded spots the exercise exposed.

## New

* `generate_rave_extract()` gains a `study` argument backed by per-study
  generator configs. SYNTH01 resolves to the previous constants verbatim,
  so its extract is byte-for-byte unchanged (frozen digests still pass).
* `spec_synth02`: a second, complete study on the same CRF family - other
  sites (FRA/ESP/GBR), four SYN-201 arms, a Week 4/8/12 schedule without
  Week 2, a straight 4-point causality decode, PHYSICIAN DECISION as a
  disposition reason, a respiratory rate vital sign, an AST lab analyte and
  an EX lot-number SUPP qualifier. `build_all(extract,
  spec = spec_synth02)` passes both validators with zero mapper changes;
  the SYNTH02 extract fails loudly under `spec_synth01`.

## Fixed

* `map_suppex()` hard-coded the EXADMBY qualifier in code; SUPPEX qualifier
  columns now come from `spec$supp`, so additional non-standard EX fields
  are a spec row.
* `make_relrec()` crashed on a zero-link extract (a realistic case for
  small cohorts); it now returns a correctly shaped empty tibble.
* `apply_deaths()` in the generator duplicated rows when a death subject's
  AE was the last row of the form (a descending `seq()` edge case); the
  reorder is now guarded.

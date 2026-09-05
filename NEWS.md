# edc2cdisc 0.4.0

## Added

* The SDTMIG trial design family. Four optional spec tables (`elements`,
  `ta`, `ie`, `ts`) drive `map_te()`, `map_ta()`, `map_ti()` and
  `map_ts()`; `map_tv()` derives TV from the existing `visits` table.
  `build_all()` returns 19 SDTM domains, `build_define_xml()` documents
  them, and `validate_sdtm()` recomputes the derivable facts — NARMS
  against `spec$arms`, PLANSUB against `spec$study$n`, TV against
  `spec$visits`, and every SV visit planned in TV — instead of trusting
  the builders.

# edc2cdisc 0.3.1

Closes REVIEW-2026-09-05.

## Fixed

* `map_lb()` no longer scales collected reference ranges by the identity
  factor when the EDC already supplied the standard result. If that
  standard value has a blank standard unit, `LBSTNRLO/HI` stay missing
  and `LBNRIND` stays blank rather than calling a mmol/L glucose LOW
  against mg/dL bounds. A labelled EDC standard unit keeps already-
  standard collected ranges. A blank EDC standard unit on a populated
  standard value is a `lb-std-unit-blank` warning.
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

# edc2cdisc 0.3.0

Closes out the first code review (REVIEW-2026-09-03): all fifteen findings
actioned. Every fix carries a hand-built edge-case test, because the seeded
generator cannot produce any of these inputs. Findings 1-10 are the Fixed
list below, 11-13 are the Breaking list, and 14 plus the two derivation
bugs hiding inside 15 close it out. Finding 7 is documented as a
deliberate SAP boundary (the first-dose date), with the datetime-level
alternative deferred to a future release (#5).

## Breaking

* Derivation functions now take the spec row they run for: the signature
  is `function(data, spec, row)`, and `map_variables()` passes it. The
  contract a derivation must keep is now documented in `derivations.R`:
  a collected CRF source is read through `row$crf_field` (or `row$aux`),
  never a literal column name. The registry entries that hardcoded CRF
  columns (`usubjid`, `country`, `dsdecod`, the ongoing ENRTPT/ENRF
  family) now read the row; the shipped spec rows name their fields
  (`Subject`, `SiteNumber`, `AEONG`/`CMONG`/`MHONG`, `DSREAS_DECODE`).
  Anyone with a custom registry updates the signature; `row` is
  positional, so existing `function(data, spec)` closures must gain the
  parameter even when unused.
* `new_study_spec()` validates references, not just shapes: an unknown
  derivation ref, a decode ref with no codelist, a SUPP transform outside
  the vocabulary, or variables for an unbuildable domain now fail at
  construction. A new optional `derivations` argument declares map-time
  derivation extensions so the check can see them.
* `spec$study` gains required `age_min` / `age_max`: the bounds
  `validate_adam()` applies to AGE were hardcoded 18-100 and made a
  paediatric or elderly protocol impossible without a validator edit.

## Fixed

* RELREC carried `RELTYPE = "ONE"` on record-level links. Per SDTMIG,
  RELTYPE describes a dataset-level relationship; record-level links
  keyed by RELID leave it null, and populating it is what Pinnacle 21
  flags. `make_relrec()` now leaves RELTYPE blank, and the validator
  learned the actual rule - blank for record-level links (IDVAR
  populated), ONE or MANY for dataset-level links - instead of refusing
  the one correct value. The define.xml stub skips codelists with no
  observed values rather than emitting an empty RELTYPE codelist.
* ADVS `ANRIND` read every value above a missing `ANRHI` as `NORMAL`
  (`AVAL > NA` fell through `case_when()`'s default), and the validator's
  own ADVS recompute had the same hole, so it could never catch it.
  ANRIND now needs the value and both bounds - a missing bound means
  "cannot classify", the same rule ADLB already applied - and the rule
  lives once in `adam-rules.R`, used by the deriver and both validator
  recomputes.
* ADVS/ADLB `PARAM` silently became NA when the analysis unit was
  missing (`str_c()` propagates NA into a required ADaM variable); it
  now falls back to the bare test name.
* ADAE `TRTEMFL` silently went blank for every event of a subject whose
  `TRTEDT` is missing (dosing ongoing at the data cut, a blank `EXENDAT`,
  or an unresolvable partial date): `TRUE & NA` fell through the
  `case_when()` and understated the treatment-emergent count with no
  error and no warning. A missing last-dose date now leaves the end of
  the window open - events on or after first dose stay flagged.
* `compute_age()` understated age by one year for a window of
  birth/reference date pairs (days/365.25, e.g. 21 years spanning only 5
  leap days floored to 20). It now counts completed anniversaries.
* `build_define_xml()` never attached `def:ValueListRef` to the parent
  `ItemRef`: a stray `]` inside the XPath string literal matched nothing,
  and `xml_add_child()` on the resulting `xml_missing` is a silent no-op,
  orphaning the value-level metadata. The regression test now counts the
  refs, not just the definitions.
* A blank collected `LBORRESU` voided `LBSTRESN` (NA-condition
  `if_else()`), and a collected unit the spec does not know got the
  converted `LBSTRESU` label on an unconverted value. The unit label now
  follows the conversion actually applied; an unmapped collected unit is
  a `lb-unit-unmapped` WARN, and a numeric `LBORRES` with no `LBSTRESN`
  is a `lb-result-lost` WARN.
* `derive_adsl()` crashed on a partial disposition date
  (`as.Date("2024-04")` in the `LSTALVDT` anchor), and the SV visit-window
  check crashed the same way. Both go through `dtc_date()`: reduced
  precision stays NA instead of erroring.
* `validate_sdtm()` crashed on NA `DOMAIN` / `RDOMAIN`
  (`any(x != y)` with NA) instead of reporting; `map_suppex()` crashed on
  NA `EXOCCUR` in its dosing-count guard. All three now report (WARN rows
  `domain-na` / `rdomain-na`, a warning() for the EXOCCUR case).
* `generate_rave_extract()` reseeded the caller's global RNG and never
  restored it; it now saves and restores `.Random.seed` (the contract
  `apply_mh()` already honoured).

## Changed

* The four derivation rules the ADaM validator had copied verbatim from
  the derivers (TRTEMFL, TRTDURD, CHG/PCHG, study days) now live once, in
  internal `adam-rules.R`, called by both sides. The validator checks the
  build against the rules; the new rule-level tests check the rules
  themselves against hand-built inputs.
* Baseline eligibility in VS/LB documents its boundary: it is the
  first-dose *date* (RFSTDTC carries no time), so a same-day post-dose
  measurement stays baseline-eligible - a recorded SAP choice.

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

# Design: SE (Subject Elements) + SUPPMH + SUPPVS

Date: 2026-09-06
Status: Proposed — awaiting evening skim, then overnight execution

## Decisions made on your behalf

1. **Scope: one spec-driven domain + two SUPP qualifiers.** SE (Subject
   Elements) is the last cheap tier-1 domain — it is derived from DM dates and
   the trial-design `elements` table, no new CRF forms. The qualifiers are
   **SUPPMH** (a new `MHSPEC` field on the MH log form, joined via a new
   `MHSPID` — the AE/AESPID pattern) and **SUPPVS** (a new `VSCOMT` field on
   the VS event form, joined via (USUBJID, VISITNUM, VSTESTCD) → VSSEQ — the
   SUPPPE pattern). Together they exercise both remaining SUPP join shapes.
   SUPPCM was considered and deferred: CM's natural "specify" content already
   maps to CMINDC, and inventing a second one buys little.
2. **Two existing extracts change bytes, deliberately.** Adding `MHSPEC` to
   the MH form and `VSCOMT` to the VS form moves those two digests (additive
   columns, proven by strip-and-compare as usual). One reference moves on
   purpose: **MH.rds gains the MHSPID column** (the AE precedent — AESPID is
   in AE's output precisely so SUPPAE can join). VS.rds does NOT move:
   `VSCOMT` is deliberately unmapped in `spec$variables`, so the SDTM VS
   output is unchanged and only SUPPVS carries the comment. All other 23
   references stay byte-identical.
3. **SE is a DM-derived domain, not an engine domain.** `map_se(dm, spec)`
   joins `spec$elements` (the trial-design table) onto DM's reference dates.
   No new CRF forms, zero generator RNG concerns.
4. **SE element semantics (pragmatic subset).** One record per subject per
   element actually started: SCRN runs from informed consent (RFICDTC) to
   first study treatment (RFXSTDTC); TREAT runs from RFXSTDTC to last dose
   (RFXENDTC). Screen failures get SCRN only, with SEENDTC left blank
   (element not ended — no invented end date); TREAT requires RFXSTDTC, so
   non-dosed subjects produce no TREAT row. No SEUPDEVT, no GETELCD, no
   --DY variables (element dates are visit-frame facts; --DY adds nothing
   the domain needs and the screen-failure rules get simpler).
5. **SE consumes `spec$elements` — "two things read the spec" again.** The
   validator cross-checks SE.ETCD against TE.ETCD (the `ta-etcd` precedent),
   so a drifted elements table cannot satisfy both readers.
6. **Domain count 23 → 25** (SE, SUPPMH, SUPPVS). ItemGroupDef 25;
   ValueListDef stays 3 (none of the three has a --STRESN); CodeList stays
   11.

## Data model

### SE (pragmatic subset)

STUDYID, DOMAIN, USUBJID, SESEQ, ETCD, ELEMENT, SESTDTC, SEENDTC.

- `map_se(dm, spec)`: pivot DM into two element rows per randomized subject
  (one per spec$elements ETCD) with the date mapping above; screen failures
  produce only the SCRN row. ETCD/ELEMENT from `spec$elements` (never
  re-typed in the mapper); SESTDTC/SEENDTC reduced-precision-safe (DM DTCs
  pass through); SESEQ via `derive_seq("SESEQ", ETCD)`.
- Label set: ETCD "Element Code", ELEMENT "Description of Element",
  SESTDTC "Start Date/Time of Subject Element", SEENDTC "End Date/Time of
  Subject Element" — all ≤ 40.

### SUPPMH

- Generator: MH log form gains `MHSPEC` (free text, "If related, specify"),
  populated on exactly one seeded row: `cfg$idx$mh_spec = c(idx, log_pos)`
  (pick indices against the MH row structure at implementation time, verify
  alive, record in-config like the PE/EG seeds).
- Spec: `spec$variables` MH row += `"MH","MHSPID","recordposition","character",…`
  (AE's AESPID row verbatim pattern); `spec$supp` += `"MH","MHSPID","MHSPECD",
  "If Related, Specify","MHSPEC","squish","CRF",NA`.
- `map_suppmh(forms$MH, mh, spec)`: the `map_suppae` clone — raw rows joined
  to built MH by `MHSPID = as.character(recordposition)`, `nrow` guard,
  `make_supp(..., rdomain = "MH", idvar = "MHSEQ", ...)`.

### SUPPVS

- Generator: VS event form gains `VSCOMT` (free text, "Comment"), populated
  on exactly one seeded subject-visit-test: `cfg$idx$vs_comment =
  c(idx, vpos, "TEMP")` (config-driven, zero RNG).
- Spec: `spec$supp` += `"VS","VSSEQ","VSCOMTL","Comment","VSCOMT","squish",
  "CRF",NA`.
- `map_suppvs(forms$VS, vs, spec)`: the `map_supppe` pattern — raw rows with
  a non-blank comment joined to built VS on (USUBJID, VISITNUM, VSTESTCD =
  the seeded testcd) to recover VSSEQ; `nrow` guard; `make_supp(...,
  rdomain = "VS", idvar = "VSSEQ", ...)`. Note: the raw VS row carries all
  five/six tests; the comment belongs to one test, so the join is on the
  seeded testcd only (the pre-join `if_any` narrowing lesson from SUPPPE
  applies).

## Validation

- `.sdtm_req_static`: `SE = c(STUDYID, DOMAIN, USUBJID, SESEQ, ETCD,
  ELEMENT, SESTDTC)`, `SUPPMH`/`SUPPVS` = the standard SUPP set.
- `.sdtm_domains` += "SE", "SUPPMH", "SUPPVS" (with the Task-5 catch-up
  lesson: all three land in the same task as their wiring).
- New checks: `se-etcd-not-in-te` (SE ETCDs ⊆ TE — ERROR, spec-gated not
  needed: TE is a built domain); `se-element-continuity` (one element per
  (USUBJID, ETCD); TREAT start must not precede SCRN end — cheap
  intra-subject coherence); SUPPMH/SUPPVS ride the `.related` loop
  (`SUPPMH = "MH"`, `SUPPVS = "VS"`) plus required-vars; stat-reason and
  friends untouched.
- Screen-failure rules: SE produces no DY variables, so the DY-leak list is
  untouched; SESTDTC on SF SCRN rows is fine (dates, not study days).
- Meta-tests: SE ETCD corruption → `se-etcd-not-in-te`; TREAT-before-SCRN
  date swap → `se-element-continuity`; SUPPMH/SUPPVS orphan corruption →
  `related-parent-orphan` (existing loop checks); clean build zero findings.

## define.xml

Keys: SE (STUDYID, USUBJID, SESEQ), SUPPMH/SUPPVS (SUPP key sets).
Structures: "One record per subject per element" / SUPP patterns. No VLM
(no --STRESN anywhere). Counts: ItemGroupDef 25, ValueListDef 3, CodeList 11.

## Testing

Digest fixture: MH.csv and VS.csv move (additive columns — proven by
strip-and-compare), 11 of 13 stay identical, fixture re-frozen to 13.
References: MH.rds re-frozen (MHSPID column — the one deliberate move),
`se.rds`/`suppmh.rds`/`suppvs.rds` new, all other 22 byte-identical.
Mapper tests: SE row counts from DM arithmetic (randomized × 2 + SF × 1),
element dates per the mapping table, SF single-row shape; SUPPMH/SUPPVS
single-row pins with IDVARVAL derived from the built parents (not magic
numbers); meta-tests above. The QS/PE/EG review discipline applies: pinned
values probed from the actual extract before writing assertions.

## Out of scope

SEUPDEVT/GETELCD/--DY on SE; SUPPCM, SUPPAE extensions; element rules
(TESTRL/TEENRL rendering into SE); ADaM (ADSE is rare; ODS/domains-beyond).

## Run shape

Same contract as the last run: two-phase not needed — one plan, one branch
`se-supp-qualifiers`, one final review, merge locally, never push. Estimated
7 tasks: (1) generator fields + seeds, (2) reader/spec wiring + MHSPID +
digest re-freeze, (3) map_se + tests, (4) map_suppmh + map_suppvs + tests,
(5) build_all wiring + counts, (6) validation + meta-tests, (7) freeze +
define.xml + docs (define folded here since SE/SUPP metadata is small).

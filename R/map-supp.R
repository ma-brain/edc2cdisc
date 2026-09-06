# ============================================================================
# Title:   SDTM SUPP-- mappers
# Purpose: Carry the non-standard CRF fields as name/value qualifiers, with
#          the qualifier specs (qnam / qlabel / src / transform / qeval)
#          coming from spec$supp.
# ============================================================================

# The SUPP transform vocabulary (spec$supp$transform); new_study_spec()
# validates supp$transform against this vector.
.supp_transforms <- c("squish", "yn", "verbatim")

# Apply a spec$supp transform to a raw column. QVAL is required, so a blank
# result becomes NA and yields no SUPP record (make_supp drops it).
supp_transform <- function(x, transform) {
  switch(transform,
    squish   = na_if(str_squish(x), ""),
    yn       = yn(x),
    verbatim = na_if(clean_verbatim(x), ""),
    stop(sprintf("unsupported SUPP transform '%s'", transform), call. = FALSE)
  )
}

# Build the SUPP parent frame: one column per QNAM holding the transformed
# collected value.
supp_parent <- function(raw, spec, rdomain) {
  rows <- filter(spec$supp, rdomain == !!rdomain)
  parent <- tibble(
    STUDYID = spec$study$STUDYID,
    USUBJID = str_c(spec$study$STUDYID, raw$Subject, sep = "-")
  )
  for (i in seq_len(nrow(rows))) {
    parent[[rows$qnam[i]]] <- supp_transform(raw[[rows$src[i]]],
                                             rows$transform[i])
  }
  parent
}

# The make_supp() qnams table straight from the spec rows.
supp_qnams <- function(spec, rdomain) {
  filter(spec$supp, rdomain == !!rdomain) |>
    select(qnam, qlabel, src, qorig, qeval)
}

#' Map the Supplemental Qualifiers for DM
#'
#' DM is one record per subject, so IDVAR / IDVARVAL are left blank and the
#' qualifier is linked to its parent by USUBJID alone. Subject initials are
#' collected for everyone, childbearing potential for women only, the
#' "Other, specify" race text only when RACE is OTHER.
#'
#' @param dm Raw DM clinical view
#' @param spec A `study_spec`
#' @return The labelled SUPPDM tibble.
#' @examples
#' ext <- file.path(tempdir(), "ex-map-suppdm")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' forms <- suppressMessages(read_rave_extract(dir = ext))
#' dm <- map_dm(forms$DM, forms$EX, forms$DS, spec_synth01)
#' suppdm <- map_suppdm(forms$DM, spec_synth01)
#' head(suppdm[, c("USUBJID", "QNAM", "QVAL")])
#' @export
map_suppdm <- function(dm, spec) {
  make_supp(
    parent  = supp_parent(dm, spec, "DM"),
    rdomain = "DM",
    idvar   = NA,
    qnams   = supp_qnams(spec, "DM")
  )
}

#' Map the Supplemental Qualifiers for AE
#'
#' IDVAR = "AESEQ": every SUPP row points at exactly one AE record. AESEQ is
#' a derived key, so the raw CRF fields are joined onto the built AE by
#' AESPID (the log-line number the AE mapper carries for this purpose).
#'
#' @param ae Raw AE clinical view
#' @param ae_built The mapped AE dataset (see [map_ae()])
#' @param spec A `study_spec`
#' @return The labelled SUPPAE tibble.
#' @examples
#' ext <- file.path(tempdir(), "ex-map-suppae")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' forms <- suppressMessages(read_rave_extract(dir = ext))
#' dm <- map_dm(forms$DM, forms$EX, forms$DS, spec_synth01)
#' ae <- map_ae(forms$AE, spec_synth01, subject_ref(dm))
#' suppae <- map_suppae(forms$AE, ae, spec_synth01)
#' head(suppae[, c("USUBJID", "IDVARVAL", "QNAM", "QVAL")])
#' @export
map_suppae <- function(ae, ae_built, spec) {
  parent <- supp_parent(ae, spec, "AE") |>
    mutate(AESPID = as.character(ae$recordposition)) |>
    inner_join(select(ae_built, USUBJID, AESPID, AESEQ),
               by = c("USUBJID", "AESPID"))

  # Every active raw AE row must have matched exactly one AE record.
  if (nrow(parent) != nrow(ae)) {
    stop(sprintf(
      "SUPPAE: %d of %d raw AE row(s) did not map to an AE record via AESPID",
      nrow(ae) - nrow(parent), nrow(ae)
    ), call. = FALSE)
  }

  make_supp(
    parent  = parent,
    rdomain = "AE",
    idvar   = "AESEQ",
    qnams   = supp_qnams(spec, "AE")
  )
}

#' Map the Supplemental Qualifiers for EX
#'
#' EX has one record per dosing interval per visit, so the raw fields are
#' joined onto the built EX by (USUBJID, VISITNUM) to recover the derived
#' EXSEQ - the EX equivalent of the AESPID trick in [map_suppae()]. The
#' qualifier columns come from `spec$supp`, so a study with a different set
#' of non-standard EX fields is a spec change, not a code change.
#'
#' @param ex Raw EX clinical view
#' @param ex_built The mapped EX dataset (see [map_ex()])
#' @param spec A `study_spec`
#' @return The labelled SUPPEX tibble.
#' @examples
#' ext <- file.path(tempdir(), "ex-map-suppx")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' forms <- suppressMessages(read_rave_extract(dir = ext))
#' dm  <- map_dm(forms$DM, forms$EX, forms$DS, spec_synth01)
#' ex  <- map_ex(forms$EX, spec_synth01, subject_ref(dm))
#' suppex <- map_suppex(forms$EX, ex, spec_synth01)
#' head(suppex[, c("USUBJID", "IDVARVAL", "QNAM", "QVAL")])
#' @export
map_suppex <- function(ex, ex_built, spec) {
  rows <- filter(spec$supp, rdomain == "EX")
  visit_map <- spec$visits |> select(Folder, VISITNUM)
  parent <- ex |>
    filter(EXOCCUR == "1") |>
    left_join(visit_map, by = "Folder") |>
    mutate(STUDYID = spec$study$STUDYID,
           USUBJID = str_c(spec$study$STUDYID, Subject, sep = "-"))
  for (i in seq_len(nrow(rows))) {
    parent[[rows$qnam[i]]] <- supp_transform(parent[[rows$src[i]]],
                                             rows$transform[i])
  }
  parent <- inner_join(parent, select(ex_built, USUBJID, VISITNUM, EXSEQ),
                       by = c("USUBJID", "VISITNUM"))

  # na.rm: an NA EXOCCUR must reach the guard as "not dosed", not crash the
  # comparison below with "missing value where TRUE/FALSE needed"
  n_occur <- sum(ex$EXOCCUR == "1", na.rm = TRUE)
  if (anyNA(ex$EXOCCUR)) {
    warning(sprintf("SUPPEX: %d EX record(s) with NA EXOCCUR - treated as not dosed",
                    sum(is.na(ex$EXOCCUR))), call. = FALSE)
  }
  if (nrow(parent) != n_occur) {
    stop(sprintf(
      "SUPPEX: %d of %d dosing record(s) did not map to an EX record",
      n_occur - nrow(parent), n_occur
    ), call. = FALSE)
  }

  make_supp(
    parent  = parent,
    rdomain = "EX",
    idvar   = "EXSEQ",
    qnams   = supp_qnams(spec, "EX")
  )
}

#' Map the Supplemental Qualifiers for PE
#'
#' PE is one record per exam system per visit, so the raw fields are joined
#' onto the built PE by (USUBJID, VISITNUM) plus the system the qualifier
#' hangs off - the "specify abnormality" text is only askable on the CV
#' system's ABNORMAL finding - to recover the derived PESEQ: the [map_suppex()]
#' trick with PETESTCD/PEORRES as the extra key. The qualifier columns come
#' from `spec$supp` only for qualifiers hanging off that CV/ABNORMAL row: a
#' qualifier collected on a different system needs the join itself extended
#' in code, and the row-count guard below fails loudly if a spec row is
#' attempted without it.
#'
#' The transformed value is stored under the `src` name, not `qnam`:
#' [make_supp()] reads QVAL from the parent column named in `qnams$src`, and
#' PE is the first qualifier whose src (CVSPEC) differs from its qnam
#' (PEABNDT).
#'
#' @param pe Raw PE clinical view
#' @param pe_built The mapped PE dataset (see [map_pe()])
#' @param spec A `study_spec`
#' @return The labelled SUPPPE tibble.
#' @examples
#' ext <- file.path(tempdir(), "ex-map-supppe")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' forms <- suppressMessages(read_rave_extract(dir = ext))
#' dm <- map_dm(forms$DM, forms$EX, forms$DS, spec_synth01)
#' pe <- map_pe(forms$PE, spec_synth01, subject_ref(dm))
#' supppe <- map_supppe(forms$PE, pe, spec_synth01)
#' supppe[, c("USUBJID", "IDVARVAL", "QNAM", "QVAL")]
#' @export
map_supppe <- function(pe, pe_built, spec) {
  rows <- filter(spec$supp, rdomain == "PE")
  visit_map <- spec$visits |> select(Folder, VISITNUM)
  parent <- pe |>
    left_join(visit_map, by = "Folder") |>
    mutate(STUDYID = spec$study$STUDYID,
           USUBJID = str_c(spec$study$STUDYID, Subject, sep = "-"))
  for (i in seq_len(nrow(rows))) {
    # under the src name: make_supp() reads QVAL from parent[[qnams$src]]
    parent[[rows$src[i]]] <- supp_transform(parent[[rows$src[i]]],
                                            rows$transform[i])
  }
  # only a row that actually collected a qualifier may look for a parent
  # record - the squish transform turned the blank CVSPEC fields into NA,
  # and they must not reach the join (and its guard) as unmapped noise
  parent <- filter(parent, if_any(all_of(rows$src), ~ !is.na(.x)))
  n_raw <- nrow(parent)

  pe_abnormal <- pe_built |> filter(PETESTCD == "CV", PEORRES == "ABNORMAL")
  parent <- inner_join(parent, select(pe_abnormal, USUBJID, VISITNUM, PESEQ),
                       by = c("USUBJID", "VISITNUM"))

  # Every collected qualifier must have matched exactly one PE record: a
  # CVSPEC with no CV/ABNORMAL finding at that visit is a broken link.
  if (nrow(parent) != n_raw) {
    stop(sprintf(
      "SUPPPE: %d of %d raw PE row(s) with a collected qualifier did not map to a PE record",
      n_raw - nrow(parent), n_raw
    ), call. = FALSE)
  }

  make_supp(
    parent  = parent,
    rdomain = "PE",
    idvar   = "PESEQ",
    qnams   = supp_qnams(spec, "PE")
  )
}

#' Map the Supplemental Qualifiers for MH
#'
#' MH is a log form, so the raw "If Related, Specify" text hangs on one log
#' line: the raw fields are joined onto the built MH by MHSPID (the log-line
#' number [map_mh()] carries as the sponsor identifier - the AESPID trick in
#' [map_suppae()]) to recover the derived MHSEQ. The output IDVAR is
#' "MHSEQ", pointing each SUPP row at its parent record's key variable just
#' like SUPPAE's IDVAR = "AESEQ"; MHSPID is only the join key. Only lines
#' where the investigator actually specified a detail produce a SUPP row.
#'
#' As in [map_supppe()], the transformed value is stored under the `src`
#' name, not `qnam`: [make_supp()] reads QVAL from the parent column named
#' in `qnams$src`, and the qualifier's src (MHSPEC) differs from its qnam
#' (MHSPECD).
#'
#' @param mh Raw MH clinical view
#' @param mh_built The mapped MH dataset (see [map_mh()])
#' @param spec A `study_spec`
#' @return The labelled SUPPMH tibble.
#' @examples
#' ext <- file.path(tempdir(), "ex-map-suppmh")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' forms <- suppressMessages(read_rave_extract(dir = ext))
#' dm <- map_dm(forms$DM, forms$EX, forms$DS, spec_synth01)
#' mh <- map_mh(forms$MH, spec_synth01, subject_ref(dm))
#' suppmh <- map_suppmh(forms$MH, mh, spec_synth01)
#' suppmh[, c("USUBJID", "IDVARVAL", "QNAM", "QVAL")]
#' @export
map_suppmh <- function(mh, mh_built, spec) {
  rows <- filter(spec$supp, rdomain == "MH")
  parent <- tibble(
    STUDYID = spec$study$STUDYID,
    USUBJID = str_c(spec$study$STUDYID, mh$Subject, sep = "-"),
    MHSPID  = as.character(mh$recordposition)
  )
  for (i in seq_len(nrow(rows))) {
    # under the src name: make_supp() reads QVAL from parent[[qnams$src]]
    parent[[rows$src[i]]] <- supp_transform(mh[[rows$src[i]]],
                                            rows$transform[i])
  }
  # only a log line that actually collected a qualifier may look for its
  # parent record: the squish transform turned the blank MHSPEC fields into
  # NA, and they must not reach the join (and its guard) as unmapped noise
  parent <- filter(parent, if_any(all_of(rows$src), ~ !is.na(.x)))
  n_raw <- nrow(parent)

  parent <- inner_join(parent, select(mh_built, USUBJID, MHSPID, MHSEQ),
                       by = c("USUBJID", "MHSPID"))

  # Every collected qualifier must have matched exactly one MH record.
  if (nrow(parent) != n_raw) {
    stop(sprintf(
      "SUPPMH: %d of %d raw MH row(s) with a collected qualifier did not map to an MH record",
      n_raw - nrow(parent), n_raw
    ), call. = FALSE)
  }

  make_supp(
    parent  = parent,
    rdomain = "MH",
    idvar   = "MHSEQ",
    qnams   = supp_qnams(spec, "MH")
  )
}

#' Map the Supplemental Qualifiers for VS
#'
#' VS is one record per test per visit, so the raw fields are joined onto the
#' built VS by (USUBJID, VISITNUM) plus the record the comment hangs on -
#' the [map_supppe()] pattern - to recover the derived VSSEQ. The join
#' hardcodes VSTESTCD == "TEMP": the study's visit comment is collected on
#' the temperature record. VSCOMT is a form-level field on the wide
#' subject-visit row, so the raw data alone cannot say which test a comment
#' belongs to and the mappers never see the config that seeds it - the
#' hardcoded TEMP is the codified assumption. What the raw data CAN police
#' is the link itself: the row-count guard fails loudly if any populated
#' VSCOMT cannot be attached to exactly one built TEMP record, so a comment
#' that drifts off the temperature record surfaces here instead of dropping
#' or mis-attaching silently. A qualifier collected on a different test
#' needs the join extended in code, exactly like SUPPPE's CV/ABNORMAL.
#'
#' @param vs Raw VS clinical view
#' @param vs_built The mapped VS dataset (see [map_vs()])
#' @param spec A `study_spec`
#' @return The labelled SUPPVS tibble.
#' @examples
#' ext <- file.path(tempdir(), "ex-map-suppvs")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' forms <- suppressMessages(read_rave_extract(dir = ext))
#' dm <- map_dm(forms$DM, forms$EX, forms$DS, spec_synth01)
#' vs <- map_vs(forms$VS, spec_synth01, subject_ref(dm))
#' suppvs <- map_suppvs(forms$VS, vs, spec_synth01)
#' suppvs[, c("USUBJID", "IDVARVAL", "QNAM", "QVAL")]
#' @export
map_suppvs <- function(vs, vs_built, spec) {
  rows <- filter(spec$supp, rdomain == "VS")
  visit_map <- spec$visits |> select(Folder, VISITNUM)
  parent <- vs |>
    left_join(visit_map, by = "Folder") |>
    mutate(STUDYID = spec$study$STUDYID,
           USUBJID = str_c(spec$study$STUDYID, Subject, sep = "-"))
  for (i in seq_len(nrow(rows))) {
    # under the src name: make_supp() reads QVAL from parent[[qnams$src]]
    parent[[rows$src[i]]] <- supp_transform(parent[[rows$src[i]]],
                                            rows$transform[i])
  }
  # only a visit that actually collected a comment may look for its parent
  # record - the squish transform turned the blank VSCOMT fields into NA,
  # and they must not reach the join (and its guard) as unmapped noise
  parent <- filter(parent, if_any(all_of(rows$src), ~ !is.na(.x)))
  n_raw <- nrow(parent)

  # TEMP hardcodes the config seed's semantics (its third element names the
  # testcd the comment hangs on; see roxygen). The raw-data cross-check that
  # remains is the guard below: a non-TEMP comment still has to find its
  # visit's TEMP record, and a commented row that cannot fires loudly.
  vs_temp <- vs_built |> filter(VSTESTCD == "TEMP")
  parent <- inner_join(parent, select(vs_temp, USUBJID, VISITNUM, VSSEQ),
                       by = c("USUBJID", "VISITNUM"))

  # Every collected comment must have matched exactly one TEMP record: a
  # VSCOMT with no temperature at that visit is a broken link.
  if (nrow(parent) != n_raw) {
    stop(sprintf(
      "SUPPVS: %d of %d raw VS row(s) with a collected comment did not map to a VS TEMP record",
      n_raw - nrow(parent), n_raw
    ), call. = FALSE)
  }

  make_supp(
    parent  = parent,
    rdomain = "VS",
    idvar   = "VSSEQ",
    qnams   = supp_qnams(spec, "VS")
  )
}

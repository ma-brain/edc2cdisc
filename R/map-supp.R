# ============================================================================
# Title:   SDTM SUPP-- mappers
# Purpose: Carry the non-standard CRF fields as name/value qualifiers, with
#          the qualifier specs (qnam / qlabel / src / transform / qeval)
#          coming from spec$supp.
# ============================================================================

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

  n_occur <- sum(ex$EXOCCUR == "1")
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

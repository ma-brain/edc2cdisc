# ============================================================================
# Title:   SDTM MH - Medical History
# Purpose: Chronic conditions recorded at screening, with partly-known start
#          dates the way real history is reported. History starts before the
#          study, so MHSTDY is legitimately negative - the screen-failure
#          "no study days" rule still holds because those subjects have no
#          RFSTDTC to anchor on at all.
# ============================================================================

#' Map the Medical History domain
#'
#' @param mh Raw MH clinical view
#' @param spec A `study_spec`
#' @param refs Subject reference dates from [subject_ref()]
#' @return The labelled SDTM MH tibble.
#' @examples
#' ext <- file.path(tempdir(), "ex-map-mh")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' forms <- suppressMessages(read_rave_extract(dir = ext))
#' dm <- map_dm(forms$DM, forms$EX, forms$DS, spec_synth01)
#' mh <- map_mh(forms$MH, spec_synth01, subject_ref(dm))
#' head(mh[, c("USUBJID", "MHSEQ", "MHTERM", "MHSTDY")])
#' @export
map_mh <- function(mh, spec, refs) {
  out <- map_variables(mh, spec, filter(spec$variables, domain == "MH"))

  out |>
    left_join(refs, by = "USUBJID") |>
    mutate(
      MHSTDY = derive_dy(MHSTDTC, RFSTDTC),
      MHENDY = derive_dy(MHENDTC, RFSTDTC)
    ) |>
    derive_seq("MHSEQ", MHSTDTC, MHTERM) |>
    select(STUDYID, DOMAIN, USUBJID, MHSEQ, MHTERM, MHDECOD, MHBODSYS,
           MHSTDTC, MHENDTC, MHSTDY, MHENDY, MHENRTPT, MHENRF) |>
    arrange(USUBJID, MHSEQ) |>
    apply_labels(c(
      STUDYID  = "Study Identifier",
      DOMAIN   = "Domain Abbreviation",
      USUBJID  = "Unique Subject Identifier",
      MHSEQ    = "Sequence Number",
      MHTERM   = "Reported Term for the Medical History",
      MHDECOD  = "Dictionary-Derived Term",
      MHBODSYS = "Body System or Organ Class",
      MHSTDTC  = "Start Date/Time of Medical History",
      MHENDTC  = "End Date/Time of Medical History",
      MHSTDY   = "Study Day of Start of Medical History",
      MHENDY   = "Study Day of End of Medical History",
      MHENRTPT = "End Relative to Reference Time Point",
      MHENRF   = "End Relative to Reference Period"
    ))
}

# ============================================================================
# Title:   SDTM CM - Concomitant Medications
# ============================================================================

#' Map the Concomitant Medications domain
#'
#' @param cm Raw CM clinical view
#' @param spec A `study_spec`
#' @param refs Subject reference dates from [subject_ref()]
#' @return The labelled SDTM CM tibble.
#' @examples
#' ext <- file.path(tempdir(), "ex-map-cm")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' forms <- suppressMessages(read_rave_extract(dir = ext))
#' dm <- map_dm(forms$DM, forms$EX, forms$DS, spec_synth01)
#' cm <- map_cm(forms$CM, spec_synth01, subject_ref(dm))
#' head(cm[, c("USUBJID", "CMSEQ", "CMTRT", "CMDOSE", "CMROUTE")])
#' @export
map_cm <- function(cm, spec, refs) {
  out <- map_variables(cm, spec, filter(spec$variables, domain == "CM"))

  out |>
    left_join(refs, by = "USUBJID") |>
    mutate(
      CMSTDY = derive_dy(CMSTDTC, RFSTDTC),
      CMENDY = derive_dy(CMENDTC, RFSTDTC)
    ) |>
    derive_seq("CMSEQ", CMSTDTC, CMDECOD) |>
    select(STUDYID, DOMAIN, USUBJID, CMSEQ, CMSPID, CMTRT, CMDECOD, CMINDC,
           CMDOSE, CMDOSU, CMDOSFRQ, CMROUTE,
           CMSTDTC, CMENDTC, CMSTDY, CMENDY, CMENRTPT, CMENRF) |>
    arrange(USUBJID, CMSEQ) |>
    apply_labels(c(
      STUDYID  = "Study Identifier",
      DOMAIN   = "Domain Abbreviation",
      USUBJID  = "Unique Subject Identifier",
      CMSEQ    = "Sequence Number",
      CMSPID   = "Sponsor-Defined Identifier",
      CMTRT    = "Reported Name of Drug, Med, or Therapy",
      CMDECOD  = "Standardized Medication Name",
      CMINDC   = "Indication",
      CMDOSE   = "Dose per Administration",
      CMDOSU   = "Dose Units",
      CMDOSFRQ = "Dosing Frequency per Interval",
      CMROUTE  = "Route of Administration",
      CMSTDTC  = "Start Date/Time of Medication",
      CMENDTC  = "End Date/Time of Medication",
      CMSTDY   = "Study Day of Start of Medication",
      CMENDY   = "Study Day of End of Medication",
      CMENRTPT = "End Relative to Reference Time Point",
      CMENRF   = "End Relative to Reference Period"
    ))
}

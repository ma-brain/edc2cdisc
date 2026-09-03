# ============================================================================
# Title:   SDTM AE - Adverse Events
# ============================================================================

#' Map the Adverse Events domain
#'
#' Coded terms pass through the spec codelists (severity, causality, action
#' taken, outcome) so an unmapped collected value breaks the run loudly.
#' Treatment-emergent status is deliberately NOT derived here: TRTEMFL is an
#' ADaM variable - SDTM keeps the collected facts, ADAE applies the rule.
#'
#' @param ae Raw AE clinical view
#' @param spec A `study_spec`
#' @param refs Subject reference dates from [subject_ref()]
#' @return The labelled SDTM AE tibble.
#' @examples
#' ext <- file.path(tempdir(), "ex-map-ae")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' forms <- suppressMessages(read_rave_extract(dir = ext))
#' dm <- map_dm(forms$DM, forms$EX, forms$DS, spec_synth01)
#' ae <- map_ae(forms$AE, spec_synth01, subject_ref(dm))
#' head(ae[, c("USUBJID", "AESEQ", "AETERM", "AESEV", "AESTDY")])
#' @export
map_ae <- function(ae, spec, refs) {
  out <- map_variables(ae, spec, filter(spec$variables, domain == "AE"))

  out |>
    left_join(refs, by = "USUBJID") |>
    mutate(
      AESTDY = derive_dy(AESTDTC, RFSTDTC),
      AEENDY = derive_dy(AEENDTC, RFSTDTC)
    ) |>
    derive_seq("AESEQ", AESTDTC, AEDECOD) |>
    select(STUDYID, DOMAIN, USUBJID, AESEQ, AESPID, AETERM, AEDECOD, AEBODSYS,
           AESEV, AESER, AEREL, AEACN, AEOUT,
           AESTDTC, AEENDTC, AESTDY, AEENDY, AEENRTPT, AEENRF) |>
    arrange(USUBJID, AESEQ) |>
    apply_labels(c(
      STUDYID  = "Study Identifier",
      DOMAIN   = "Domain Abbreviation",
      USUBJID  = "Unique Subject Identifier",
      AESEQ    = "Sequence Number",
      AESPID   = "Sponsor-Defined Identifier",
      AETERM   = "Reported Term for the Adverse Event",
      AEDECOD  = "Dictionary-Derived Term",
      AEBODSYS = "Body System or Organ Class",
      AESEV    = "Severity/Intensity",
      AESER    = "Serious Event",
      AEREL    = "Causality",
      AEACN    = "Action Taken with Study Treatment",
      AEOUT    = "Outcome of Adverse Event",
      AESTDTC  = "Start Date/Time of Adverse Event",
      AEENDTC  = "End Date/Time of Adverse Event",
      AESTDY   = "Study Day of Start of Adverse Event",
      AEENDY   = "Study Day of End of Adverse Event",
      AEENRTPT = "End Relative to Reference Time Point",
      AEENRF   = "End Relative to Reference Period"
    ))
}

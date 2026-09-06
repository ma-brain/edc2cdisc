# ============================================================================
# Title:   ADaM ADCM - Concomitant Medication Analysis Dataset
# Purpose: One analysis record per collected CM record, with the analysis
#          dates (imputed, flagged) and the analysis study days anchored on
#          ADSL. The ADAE OCCDS pattern without the SUPP merge-back: CM
#          collects no qualifiers in this study, so there is nothing to
#          pivot - what ADCM adds is the analysis timing, not new content.
# ============================================================================

#' Derive ADCM, the concomitant medication analysis dataset
#'
#' One analysis record per collected CM record; the SDTM sequence is the
#' analysis sequence, so the two cannot silently drift apart. Dates impute
#' per the shared first-of rule (`""`/`"D"`/`"M"`) and the analysis study
#' days anchor on ADSL TRTSDT - unlike SDTM CMSTDY, which stays NA for a
#' partial start. No medication-level treatment-emergent rule exists in the
#' SAP, so no flag is invented; the ADSL anchors (SAFFL, TRTSDT, TRTEDT)
#' are carried per the OCCDS convention so any windowing stays checkable in
#' place. In this extract every dosed subject's medication starts before
#' first dose, so ASTDY is negative on those rows. Ongoing medications keep
#' a missing AENDT - no fabricated end date, same as SDTM.
#'
#' @param cm The mapped SDTM CM dataset
#' @param adsl The ADSL dataset (see [derive_adsl()])
#' @return The labelled ADCM tibble.
#' @examples
#' ext <- file.path(tempdir(), "ex-adcm")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' forms <- suppressMessages(read_rave_extract(dir = ext))
#' built <- build_all(ext)
#' adcm <- derive_adcm(built$sdtm$CM, built$adam$ADSL)
#' head(adcm[, c("USUBJID", "ASEQ", "ASTDT", "ASTDY")])
#' @export
derive_adcm <- function(cm, adsl) {
  adsl_trt <- adsl |> select(USUBJID, TRTSDT, TRTEDT, SAFFL)

  cm |>
    left_join(adsl_trt, by = "USUBJID") |>
    mutate(
      ASTDT  = impute_dtc(CMSTDTC)$date,
      ASTDTF = impute_dtc(CMSTDTC)$flag,
      AENDT  = impute_dtc(CMENDTC)$date,
      AENDTF = impute_dtc(CMENDTC)$flag
    ) |>
    transmute(
      STUDYID, USUBJID,
      # One analysis record per collected record, so the SDTM sequence is
      # the analysis sequence - no renumbering to drift apart.
      ASEQ = CMSEQ,
      CMTRT, CMDECOD, CMINDC, CMDOSE, CMDOSU, CMDOSFRQ, CMROUTE,

      # The ADSL anchors any windowing is defined against, carried on the
      # dataset (OCCDS convention) so the window is checkable in place.
      SAFFL, TRTSDT, TRTEDT,

      # Analysis dates, imputed per the ADSL rule and flagged. Ongoing
      # medications keep a missing AENDT - no fabricated end date, same
      # as SDTM.
      ASTDT, ASTDTF,
      AENDT, AENDTF,

      # Analysis study days, anchored on TRTSDT and computed from the
      # imputed dates. Medications that predate first dose keep their
      # negative day; undosed subjects have no anchor and stay missing.
      ASTDY = derive_dy_d(ASTDT, TRTSDT),
      AENDY = derive_dy_d(AENDT, TRTSDT)
    ) |>
    arrange(USUBJID, ASEQ) |>
    apply_labels(c(
      STUDYID  = "Study Identifier",
      USUBJID  = "Unique Subject Identifier",
      ASEQ     = "Analysis Sequence Number",
      CMTRT    = "Reported Name of Drug, Med, or Therapy",
      CMDECOD  = "Standardized Medication Name",
      CMINDC   = "Indication",
      CMDOSE   = "Dose per Administration",
      CMDOSU   = "Dose Units",
      CMDOSFRQ = "Dosing Frequency per Interval",
      CMROUTE  = "Route of Administration",
      SAFFL    = "Safety Population Flag",
      TRTSDT   = "Treatment Start Date",
      TRTEDT   = "Treatment End Date",
      ASTDT    = "Analysis Start Date",
      ASTDTF   = "Analysis Start Date Imputation Flag",
      AENDT    = "Analysis End Date",
      AENDTF   = "Analysis End Date Imputation Flag",
      ASTDY    = "Analysis Study Day of Start",
      AENDY    = "Analysis Study Day of End"
    ))
}

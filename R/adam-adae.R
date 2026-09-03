# ============================================================================
# Title:   ADaM ADAE - Adverse Event Analysis Dataset
# Purpose: One analysis record per collected AE event, with the analysis
#          dates (imputed, flagged), analysis study days, the treatment-
#          emergent flag the SDTM map deliberately skipped, and the SUPPAE
#          qualifiers merged back onto the record they qualify.
# ============================================================================

#' Derive ADAE, the adverse event analysis dataset
#'
#' Treatment-emergent: onset (imputed start) on or after first dose and up
#' to the last dose. No grace window - no SAP defines one, so the window is
#' stated here where it can be argued with. Events with no classifiable
#' onset, or for undosed subjects, stay blank.
#'
#' @param ae The mapped SDTM AE dataset
#' @param suppae The mapped SDTM SUPPAE dataset
#' @param adsl The ADSL dataset (see [derive_adsl()])
#' @return The labelled ADAE tibble.
#' @examples
#' ext <- file.path(tempdir(), "ex-adae")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' forms <- suppressMessages(read_rave_extract(dir = ext))
#' built <- build_all(ext)
#' adae <- derive_adae(built$sdtm$AE, built$sdtm$SUPPAE, built$adam$ADSL)
#' head(adae[, c("USUBJID", "ASEQ", "ASTDT", "TRTEMFL")])
#' @export
derive_adae <- function(ae, suppae, adsl) {
  adsl_trt <- adsl |> select(USUBJID, TRTSDT, TRTEDT, SAFFL)
  # SUPPAE -> wide: one column per QNAM, keyed to its AE record by AESEQ
  # (IDVARVAL). The validator keeps the long format unique on this key, so
  # the pivot cannot silently drop or duplicate qualifiers.
  supp_labels <- suppae |>
    distinct(QNAM, QLABEL) |>
    deframe()

  supp_wide <- suppae |>
    transmute(USUBJID, AESEQ = as.integer(IDVARVAL), QNAM, QVAL) |>
    pivot_wider(names_from = QNAM, values_from = QVAL)

  # Every AE record must come with its qualifiers - a missing row here
  # means the SUPPAE join or the pivot drifted.
  unqualified <- ae |>
    anti_join(supp_wide, by = c("USUBJID", "AESEQ"))
  if (nrow(unqualified) > 0) {
    stop(sprintf(
      paste("ADAE: %d AE record(s) have no SUPPAE qualifiers",
            "(e.g. %s AESEQ %d)"),
      nrow(unqualified), unqualified$USUBJID[1], unqualified$AESEQ[1]
    ), call. = FALSE)
  }

  ae |>
    left_join(supp_wide, by = c("USUBJID", "AESEQ")) |>
    left_join(adsl_trt, by = "USUBJID") |>
    mutate(
      ASTDT  = impute_dtc(AESTDTC)$date,
      ASTDTF = impute_dtc(AESTDTC)$flag,
      AENDT  = impute_dtc(AEENDTC)$date,
      AENDTF = impute_dtc(AEENDTC)$flag
    ) |>
    transmute(
      STUDYID, USUBJID,
      # One analysis record per collected event, so the SDTM sequence is
      # the analysis sequence - no renumbering to drift apart.
      ASEQ = AESEQ,
      AETERM, AEDECOD, AEBODSYS, AESEV, AESER, AEREL, AEACN, AEOUT,

      # The non-standard CRF fields, back where analysis wants them
      AESI, AEDISCON,

      # The ADSL dates the TRTEMFL window is defined against, carried on
      # the dataset (OCCDS convention) so the flag is checkable in place.
      SAFFL, TRTSDT, TRTEDT,

      # Analysis dates, imputed per the ADSL rule and flagged. Ongoing
      # events keep a missing AENDT - no fabricated end date, same as SDTM.
      ASTDT, ASTDTF,
      AENDT, AENDTF,

      # Analysis study days, anchored on TRTSDT and computed from the
      # imputed dates - unlike SDTM AESTDY, which stays NA for a partial
      # start.
      ASTDY = derive_dy_d(ASTDT, TRTSDT),
      AENDY = derive_dy_d(AENDT, TRTSDT),

      TRTEMFL = case_when(
        is.na(ASTDT) | is.na(TRTSDT)               ~ "",
        ASTDT >= TRTSDT & ASTDT <= TRTEDT          ~ "Y",
        .default                                   = ""
      )
    ) |>
    arrange(USUBJID, ASEQ) |>
    apply_labels(c(
      STUDYID  = "Study Identifier",
      USUBJID  = "Unique Subject Identifier",
      ASEQ     = "Analysis Sequence Number",
      AETERM   = "Reported Term for the Adverse Event",
      AEDECOD  = "Dictionary-Derived Term",
      AEBODSYS = "Body System or Organ Class",
      AESEV    = "Severity/Intensity",
      AESER    = "Serious Event",
      AEREL    = "Causality",
      AEACN    = "Action Taken with Study Treatment",
      AEOUT    = "Outcome of Adverse Event",
      AESI     = unname(supp_labels["AESI"]),
      AEDISCON = unname(supp_labels["AEDISCON"]),
      SAFFL    = "Safety Population Flag",
      TRTSDT   = "Treatment Start Date",
      TRTEDT   = "Treatment End Date",
      ASTDT    = "Analysis Start Date",
      ASTDTF   = "Analysis Start Date Imputation Flag",
      ASTDY    = "Analysis Study Day of Start",
      AENDT    = "Analysis End Date",
      AENDTF   = "Analysis End Date Imputation Flag",
      AENDY    = "Analysis Study Day of End",
      TRTEMFL  = "Treatment Emergent Flag"
    ))
}

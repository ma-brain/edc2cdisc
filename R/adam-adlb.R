# ============================================================================
# Title:   ADaM ADLB - Laboratory Test Results Analysis Dataset (BDS)
# Purpose: One analysis record per subject per visit per analyte. The BDS
#          skeleton shared with ADVS, with the laboratory twist: reference
#          ranges are not declared but arrive with each record - specific to
#          the sex, per the central lab - so ANRLO/ANRHI/ANRIND are sourced
#          row-wise, and BNRIND carries the baseline record's own range
#          indicator.
# ============================================================================

#' Derive ADLB, the laboratory test results analysis dataset (BDS)
#'
#' ANRIND is the SDTM comparison carried forward, checkable against the
#' row's own limits (LBSTNRLO/LBSTNRHI, standardised by the same factor as
#' the result). Screen failures have no baseline record, so their
#' BASE/BNRIND/CHG stay missing while ANRIND still computes - the record's
#' own range needs no study-day anchor.
#'
#' @param lb The mapped SDTM LB dataset
#' @param adsl The ADSL dataset (see [derive_adsl()])
#' @param param_order Deterministic PARAMN ordering (chemistry panel first,
#'   matching the lab). Defaults to the SYNTH01 panel order.
#' @return The labelled ADLB tibble.
#' @export
derive_adlb <- function(lb, adsl,
                        param_order = c("GLUC", "CREAT", "HGB", "K", "ALT")) {
  adsl_trtsdt <- adsl |> select(USUBJID, TRTSDT)
  lb_analysis <- lb |>
    # Analysis records are performed panels. The NOT DONE row documents a
    # missed draw - SDTM keeps it with LBSTAT/LBREASND, BDS drops it.
    filter(!LBSTAT %in% "NOT DONE") |>
    left_join(adsl_trtsdt, by = "USUBJID") |>
    mutate(
      ABLFL = LBBLFL,
      ADT   = dtc_date(LBDTC),
      ADY   = derive_dy_d(ADT, TRTSDT),
      PARAMN = match(LBTESTCD, param_order)
    )

  # Baseline anchor: the one ABLFL='Y' record per subject per analyte,
  # giving both BASE and BNRIND.
  base_ref <- lb_analysis |>
    filter(ABLFL == "Y") |>
    select(USUBJID, PARAMCD = LBTESTCD, BASE = LBSTRESN, BNRIND = LBNRIND)

  lb_analysis |>
    left_join(base_ref, by = c("USUBJID", "LBTESTCD" = "PARAMCD")) |>
    transmute(
      STUDYID, USUBJID,
      PARAMCD = LBTESTCD,
      PARAM   = str_c(LBTEST, " (", LBSTRESU, ")"),
      PARAMN,
      LBCAT,
      AVAL  = LBSTRESN,
      AVALU = LBSTRESU,
      ABLFL,
      BASE, BNRIND,
      # Same rules as ADVS; the %in% forms matter because ABLFL is "Y"/NA
      CHG  = case_when(
        ABLFL %in% "Y" ~ NA_real_,
        is.na(BASE)    ~ NA_real_,
        .default       = AVAL - BASE
      ),
      PCHG = if_else(!is.na(CHG), 100 * CHG / BASE, NA_real_),
      ANRIND = LBNRIND,
      ANRLO  = LBSTNRLO,
      ANRHI  = LBSTNRHI,
      AVISIT  = VISIT,
      AVISITN = VISITNUM,
      ADT, ADY
    ) |>
    arrange(USUBJID, PARAMN, AVISITN) |>
    apply_labels(c(
      STUDYID  = "Study Identifier",
      USUBJID  = "Unique Subject Identifier",
      PARAMCD  = "Parameter Code",
      PARAM    = "Parameter",
      PARAMN   = "Parameter (N)",
      LBCAT    = "Category for Lab Test",
      AVAL     = "Analysis Value",
      AVALU    = "Analysis Value Unit",
      ABLFL    = "Baseline Record Flag",
      BASE     = "Baseline Value",
      BNRIND   = "Baseline Reference Range Indicator",
      CHG      = "Change from Baseline",
      PCHG     = "Percent Change from Baseline",
      ANRIND   = "Analysis Reference Range Indicator",
      ANRLO    = "Analysis Normal Range Lower Limit",
      ANRHI    = "Analysis Normal Range Upper Limit",
      AVISIT   = "Analysis Visit",
      AVISITN  = "Analysis Visit (N)",
      ADT      = "Analysis Date",
      ADY      = "Analysis Study Day"
    ))
}

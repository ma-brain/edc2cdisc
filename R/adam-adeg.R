# ============================================================================
# Title:   ADaM ADEG - ECG Test Results Analysis Dataset (BDS)
# Purpose: One analysis record per subject per visit per ECG interval, in the
#          Basic Data Structure: AVAL, the baseline anchor (ABLFL/BASE), the
#          change derivations (CHG/PCHG), and ANRIND against declared
#          reference ranges. SDTM did the groundwork the BDS stands on:
#          EGSTRESN is already in standard units (milliseconds) and EGBLFL is
#          already the last pre-dose result.
# ============================================================================

#' Derive ADEG, the ECG test results analysis dataset (BDS)
#'
#' Parameter order and reference ranges come from `spec$bds`: EG collects no
#' reference ranges of its own, so ANRIND needs a declared table - the SAP
#' stand-in, the same one the validator checks the build against (the ADVS
#' situation, not the ADLB one where ranges arrive with each record). A
#' date-only EGDTC (a blank EGTIM on the collected form) yields a date-only
#' ADT - collected messiness, not an imputation; the study day still anchors.
#'
#' @param eg The mapped SDTM EG dataset
#' @param adsl The ADSL dataset (see [derive_adsl()])
#' @param spec A `study_spec`; the ADEG rows of `spec$bds` declare the
#'   parameter order and reference ranges
#' @return The labelled ADEG tibble.
#' @examples
#' ext <- file.path(tempdir(), "ex-adeg")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' forms <- suppressMessages(read_rave_extract(dir = ext))
#' built <- build_all(ext)
#' adeg <- derive_adeg(built$sdtm$EG, built$adam$ADSL, spec_synth01)
#' head(adeg[, c("USUBJID", "PARAMCD", "AVAL", "BASE", "CHG", "ANRIND")])
#' @export
derive_adeg <- function(eg, adsl, spec = spec_synth01) {
  param_spec <- filter(spec$bds, domain == "ADEG") |>
    select(PARAMCD = paramcd, PARAMN = paramn,
           ANRLO = anrlo, ANRHI = anrhi)

  adsl_trtsdt <- adsl |> select(USUBJID, TRTSDT)
  eg_analysis <- eg |>
    # Analysis records are performed measurements. The NOT DONE row
    # documents a missed ECG - SDTM keeps it with EGSTAT/EGREASND, but
    # there is no result to analyse, so BDS drops it. (%in% keeps the
    # NA = performed rows.)
    filter(!EGSTAT %in% "NOT DONE") |>
    left_join(param_spec, by = c("EGTESTCD" = "PARAMCD")) |>
    left_join(adsl_trtsdt, by = "USUBJID") |>
    mutate(
      # SDTM's baseline flag carried forward under its ADaM name
      ABLFL = EGBLFL,
      ADT   = dtc_date(EGDTC),
      ADY   = derive_dy_d(ADT, TRTSDT)
    )

  # Baseline value: the one ABLFL='Y' record per subject per parameter.
  # Screen failures have no baseline row, so their BASE stays missing -
  # a fact, not a gap.
  base_ref <- eg_analysis |>
    filter(ABLFL == "Y") |>
    select(USUBJID, PARAMCD = EGTESTCD, BASE = EGSTRESN)

  eg_analysis |>
    left_join(base_ref, by = c("USUBJID", "EGTESTCD" = "PARAMCD")) |>
    transmute(
      STUDYID, USUBJID,
      PARAMCD = EGTESTCD,
      # the unit in parentheses only when there is one: str_c() would
      # propagate an NA unit into a required ADaM variable
      PARAM   = if_else(is.na(EGSTRESU), EGTEST,
                        str_c(EGTEST, " (", EGSTRESU, ")")),
      PARAMN,
      AVAL  = EGSTRESN,
      AVALU = EGSTRESU,
      ABLFL,
      BASE,
      # Change is defined against the baseline record; the baseline row
      # itself carries no CHG/PCHG (nothing to change from), and rows
      # without a baseline (screen failures) stay missing rather than zero.
      CHG  = .rule_chg(AVAL, BASE, ABLFL),
      PCHG = .rule_pchg(CHG, BASE),
      ANRIND = .rule_anrind(AVAL, ANRLO, ANRHI),
      ANRLO, ANRHI,
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
      AVAL     = "Analysis Value",
      AVALU    = "Analysis Value Unit",
      ABLFL    = "Baseline Record Flag",
      BASE     = "Baseline Value",
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

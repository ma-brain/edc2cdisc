# ============================================================================
# Title:   ADaM ADTTE - Time-to-Event Analysis Dataset
# Purpose: One record per subject per parameter. Overall Survival anchors on
#          death - the fact ADSL already carries three ways (DM, DS, AE) -
#          and Time to First Treatment-Emergent Adverse Event anchors on the
#          ADAE the safety story is told with. Everything else is censored
#          at the last known alive date (ADSL EOSDT). AVAL uses the house
#          day rule via derive_dy_d: no day zero. Screen failures have no
#          treatment start and no end-of-study date, so their records carry
#          no dates and no descriptions - an absence, not a zero.
# ============================================================================

#' Derive ADTTE, the time-to-event analysis dataset
#'
#' Two parameters, no spec table - nothing here is study-specific. OS:
#' event at death (`ADSL` `DTHFL` = "Y", `ADT` = `DTHDT`), otherwise
#' censored at `EOSDT`. TTAE: event at the first treatment-emergent AE
#' (`ADAE` `TRTEMFL` = "Y", earliest `ASTDT`, ties broken by lowest
#' `ASEQ`), otherwise censored at `EOSDT`. `STARTDT` is `TRTSDT` and
#' `AVAL` = `ADT` - `STARTDT` through the shared day rule, so a same-day
#' event is day 1.
#'
#' @param adsl The ADSL dataset (see [derive_adsl()])
#' @param adae The ADAE dataset (see [derive_adae()])
#' @return The labelled ADTTE tibble.
#' @examples
#' ext <- file.path(tempdir(), "ex-adtte")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' built <- build_all(ext)
#' adtte <- derive_adtte(built$adam$ADSL, built$adam$ADAE)
#' head(adtte[, c("USUBJID", "PARAMCD", "AVAL", "EVNTDESC", "CNSDTDSC")])
#' @export
derive_adtte <- function(adsl, adae) {
  adsl_ref <- adsl |>
    select(STUDYID, USUBJID, TRTSDT, EOSDT, DTHDT, DTHFL)

  # the first treatment-emergent AE per subject: earliest ASTDT, ties to
  # the lowest ASEQ, so the pick is deterministic under reordering
  # earliest ASTDT per subject, ties to the lowest ASEQ: the pre-sorted
  # frame makes the first occurrence the pick
  first_te_ae <- adae |>
    filter(TRTEMFL %in% "Y") |>
    arrange(USUBJID, ASTDT, ASEQ) |>
    distinct(USUBJID, .keep_all = TRUE) |>
    select(USUBJID, .ae_adt = ASTDT, .ae_seq = ASEQ)

  # one skeleton row per subject per parameter; the event/censor logic is
  # per parameter and lands on the shared columns
  os <- adsl_ref |>
    transmute(
      STUDYID, USUBJID,
      PARAMCD = "OS",
      PARAM   = "Overall Survival (days)",
      PARAMN  = 1,
      STARTDT = TRTSDT,
      event   = DTHFL %in% "Y",
      .dt_event = DTHDT,
      .dt_censor = EOSDT
    )
  ttae <- adsl_ref |>
    left_join(first_te_ae, by = "USUBJID") |>
    transmute(
      STUDYID, USUBJID,
      PARAMCD = "TTAE",
      PARAM   = "Time to First Treatment-Emergent Adverse Event (days)",
      PARAMN  = 2,
      STARTDT = TRTSDT,
      event   = !is.na(.ae_adt),
      .dt_event = .ae_adt,
      .dt_censor = EOSDT,
      .src_seq = .ae_seq
    )

  bind_rows(os, ttae) |>
    transmute(
      STUDYID, USUBJID, PARAMCD, PARAM, PARAMN,
      # AVAL is defined only where both anchors exist: an event or a
      # censoring date without a treatment start (or the reverse) cannot
      # produce a time
      STARTDT,
      ADT    = if_else(event, .dt_event, .dt_censor),
      AVAL   = derive_dy_d(ADT, STARTDT),
      AVALU  = if_else(is.na(AVAL), NA_character_, "DAYS"),
      EVNTDESC  = if_else(event, .evnt_desc(PARAMCD), NA_character_),
      CNSDTDSC  = if_else(!event & !is.na(ADT), "Last known alive date",
                          NA_character_),
      SRCDOM = if_else(is.na(ADT), NA_character_,
                       if_else(PARAMCD == "TTAE" & event, "ADAE", "ADSL")),
      SRCVAR = if_else(is.na(ADT), NA_character_,
                       if_else(PARAMCD == "TTAE" & event, "ASTDT",
                               if_else(event, "DTHDT", "EOSDT"))),
      SRCSEQ = if_else(PARAMCD == "TTAE" & event, .src_seq, NA_real_)
    ) |>
    arrange(USUBJID, PARAMN) |>
    apply_labels(c(
      STUDYID   = "Study Identifier",
      USUBJID   = "Unique Subject Identifier",
      PARAMCD   = "Parameter Code",
      PARAM     = "Parameter",
      PARAMN    = "Parameter (N)",
      AVAL      = "Time to Event",
      AVALU     = "Time to Event Unit",
      STARTDT   = "Analysis Start Date",
      ADT       = "Analysis Date",
      EVNTDESC  = "Event Description",
      CNSDTDSC  = "Reason for Censoring",
      SRCDOM    = "Source Data",
      SRCVAR    = "Source Variable",
      SRCSEQ    = "Source Sequence Number"
    ))
}

.evnt_desc <- function(paramcd) {
  if_else(paramcd == "OS", "Death",
          "First treatment-emergent adverse event")
}

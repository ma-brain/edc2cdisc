# ============================================================================
# Title:   ADaM ADQS - Questionnaire Analysis Dataset (BDS)
# Purpose: One analysis record per subject per visit per questionnaire item,
#          in the Basic Data Structure, plus the package's first derived
#          analysis parameter: the instrument total. SDTM did the groundwork
#          the BDS stands on: QSSTRESN is already standardized (ordinal
#          0-3 item answers) and QSBLFL is already the last pre-dose result.
#          Items carry no reference range - ordinal answers have no absolute
#          norm - so ANRIND stays missing for them (the WEIGHT/HEIGHT
#          precedent); the total classifies against the range its spec$totals
#          row declares. The total's baseline is derived, not carried: QS has
#          no QSBLFL for a parameter that does not exist in SDTM, so the
#          total row at the visit where the items carry QSBLFL = "Y" is the
#          baseline record, and BASE/CHG/PCHG flow from there through the
#          shared rules like any BDS parameter's.
# ============================================================================

#' Derive ADQS, the questionnaire analysis dataset (BDS)
#'
#' The items are one record per subject per visit per QS item (AVAL =
#' QSSTRESN), exactly the ADVS/ADEG pattern on the QS domain. The derived
#' parameters come from `spec$totals`: for each row, the named `src_items`
#' are summed per (USUBJID, VISITNUM) and the total is emitted only where
#' **all** source items are present with a value - the strict,
#' SAP-defensible default; a visit with a missing or unparseable item
#' scores nothing rather than a proration this house does not invent.
#' A not-done visit produces no items and therefore no total.
#'
#' @param qs The mapped SDTM QS dataset
#' @param adsl The ADSL dataset (see [derive_adsl()])
#' @param spec A `study_spec`; the ADQS rows of `spec$bds` declare the item
#'   parameters (codes, order, no ranges) and the `spec$totals` rows declare
#'   the derived totals (code, name, order, reference range, `src_items`)
#' @return The labelled ADQS tibble.
#' @examples
#' ext <- file.path(tempdir(), "ex-adqs")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' forms <- suppressMessages(read_rave_extract(dir = ext))
#' built <- build_all(ext)
#' adqs <- derive_adqs(built$sdtm$QS, built$adam$ADSL, spec_synth01)
#' head(adqs[, c("USUBJID", "PARAMCD", "AVAL", "BASE", "CHG", "ANRIND")])
#' @export
derive_adqs <- function(qs, adsl, spec = spec_synth01) {
  param_spec <- filter(spec$bds, domain == "ADQS") |>
    select(PARAMCD = paramcd, PARAMN = paramn,
           ANRLO = anrlo, ANRHI = anrhi)

  adsl_trtsdt <- adsl |> select(USUBJID, TRTSDT)
  qs_analysis <- qs |>
    # Analysis records are performed questionnaires. The NOT DONE rows
    # document a missed form - SDTM keeps one per item with QSSTAT/QSREASND,
    # but there is no result to analyse, so BDS drops them. (%in% keeps the
    # NA = performed rows.)
    filter(!QSSTAT %in% "NOT DONE") |>
    left_join(param_spec, by = c("QSTESTCD" = "PARAMCD")) |>
    # A QS item the spec does not carry is a spec/data disagreement the
    # validator owns (adqs-item-not-in-spec); here it would silently corrupt
    # the totals' all-required count, so the analysis frame keeps spec'd items.
    filter(!is.na(PARAMN)) |>
    left_join(adsl_trtsdt, by = "USUBJID") |>
    mutate(
      # SDTM's baseline flag carried forward under its ADaM name
      ABLFL = QSBLFL,
      ADT   = dtc_date(QSDTC),
      ADY   = derive_dy_d(ADT, TRTSDT)
    )

  # The one BDS tail for items and totals alike: baseline-anchored change
  # against BASE, classification against the declared range. Items carry NA
  # ranges, so their ANRIND stays missing - by design, not by omission.
  bds_tail <- function(analysis) {
    analysis |>
      transmute(
        STUDYID, USUBJID,
        PARAMCD, PARAM, PARAMN,
        AVAL, AVALU,
        ABLFL,
        BASE,
        # Change is defined against the baseline record; the baseline row
        # itself carries no CHG/PCHG (nothing to change from), and rows
        # without a baseline (screen failures) stay missing, not zero.
        CHG  = .rule_chg(AVAL, BASE, ABLFL),
        PCHG = .rule_pchg(CHG, BASE),
        ANRIND = .rule_anrind(AVAL, ANRLO, ANRHI),
        ANRLO, ANRHI,
        AVISIT, AVISITN,
        ADT, ADY
      )
  }

  # ---- items: the ADVS/ADEG pattern on QS ----------------------------------
  # Baseline value: the one ABLFL='Y' record per subject per item. Screen
  # failures have no baseline row, so their BASE stays missing - a fact,
  # not a gap.
  item_base <- qs_analysis |>
    filter(ABLFL == "Y") |>
    select(USUBJID, PARAMCD = QSTESTCD, BASE = QSSTRESN)

  items_analysis <- qs_analysis |>
    left_join(item_base, by = c("USUBJID", "QSTESTCD" = "PARAMCD")) |>
    transmute(
      STUDYID, USUBJID,
      PARAMCD = QSTESTCD,
      PARAM   = QSTEST,
      PARAMN,
      AVAL  = QSSTRESN,
      AVALU = QSSTRESU,
      ABLFL,
      BASE,
      ANRLO, ANRHI,
      AVISIT  = VISIT,
      AVISITN = VISITNUM,
      ADT, ADY
    )

  # ---- totals: one derived parameter per spec$totals row -------------------
  # Adding a questionnaire total is a spec row, not a code change: sum the
  # named src_items per (USUBJID, VISITNUM), emit only where all of them
  # carry a value, anchor ABLFL at the visit the items flag as baseline.
  totals_spec <- filter(spec$totals, domain == "ADQS")
  scored <- pmap(totals_spec, \(domain, paramcd, param, paramn, anrlo, anrhi, src_items) {
    items <- str_split_1(src_items, ";")
    totals_analysis <- qs_analysis |>
      filter(QSTESTCD %in% items) |>
      summarise(
        .n_items = n(),
        .any_na  = anyNA(QSSTRESN),
        AVAL   = sum(QSSTRESN),
        # the total is anchored where its items are: ABLFL = "Y" at the
        # visit carrying an item QSBLFL = "Y" - and nowhere else, because
        # QS has no baseline flag for a parameter absent from SDTM
        ABLFL  = if_else(any(ABLFL %in% "Y"), "Y", NA_character_),
        # the items of a visit share the form's collected date
        VISIT   = first(VISIT),
        ADT     = first(ADT),
        TRTSDT  = first(TRTSDT),
        STUDYID = first(STUDYID),
        .by = c(USUBJID, VISITNUM)
      ) |>
      # the all-required rule: a visit missing an item (not administered, or
      # administered but unparseable) scores nothing - no proration here
      filter(.n_items == length(items), !.any_na) |>
      transmute(
        STUDYID, USUBJID,
        PARAMCD = paramcd,
        PARAM   = param,
        PARAMN  = paramn,
        AVAL,
        AVALU = NA_character_,
        ABLFL,
        ANRLO = anrlo,
        ANRHI = anrhi,
        AVISIT  = VISIT,
        AVISITN = VISITNUM,
        ADT,
        ADY = derive_dy_d(ADT, TRTSDT)
      )

    # the total's baseline: the one flagged record per subject, joined back
    # so BASE flows through the shared rules - the derived baseline
    # participates in the ordinary BDS tail
    totals_base <- totals_analysis |>
      filter(ABLFL == "Y") |>
      select(USUBJID, BASE = AVAL)

    totals_analysis |>
      left_join(totals_base, by = "USUBJID") |>
      bds_tail()
  })

  bind_rows(c(list(bds_tail(items_analysis)), scored)) |>
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

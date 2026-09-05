# ============================================================================
# Title:   SDTM QS - Questionnaires
# Purpose: Pivot the scheduled questionnaire form into the SDTM findings
#          structure. The per-item pivot spec lives in spec$tests - adding a
#          questionnaire item is a spec row, not a code change.
# ============================================================================

#' Map the Questionnaires domain
#'
#' One questionnaire form row (the item answers side by side) becomes one row
#' per item, via the `spec$tests` QS rows. A form marked not performed
#' (QSPERF "0") becomes one NOT DONE row per item with QSREASND carrying the
#' collected non-administration reason and no result at all - QSDAT still
#' rides on the form, so those rows keep their visit date.
#'
#' Baseline (QSBLFL) mirrors VSBLFL: the last non-missing numeric result on
#' or before first dose, per subject per item. Only QSSTRESN rows are
#' eligible, so NOT DONE records and unparseable answers are never flagged -
#' and screen failures get none because RFSTDTC is the anchor.
#'
#' @param qs Raw QS clinical view
#' @param spec A `study_spec`
#' @param refs Subject reference dates from [subject_ref()]
#' @return The labelled SDTM QS tibble.
#' @examples
#' ext <- file.path(tempdir(), "ex-map-qs")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' forms <- suppressMessages(read_rave_extract(dir = ext))
#' dm   <- map_dm(forms$DM, forms$EX, forms$DS, spec_synth01)
#' qs   <- map_qs(forms$QS, spec_synth01, subject_ref(dm))
#' head(qs[, c("USUBJID", "QSTESTCD", "QSORRES", "QSSTAT", "QSBLFL")])
#' @export
map_qs <- function(qs, spec, refs) {
  usubjid <- str_c(spec$study$STUDYID, qs$Subject, sep = "-")
  qs_prep <- qs |> mutate(usubjid = usubjid)
  qs_spec <- filter(spec$tests, domain == "QS")

  pivoted <- pmap(qs_spec, \(domain, field, testcd, test, cat, specimen) {
    qs_prep |>
      mutate(QSCAT    = cat,
             QSTESTCD = testcd,
             QSTEST   = test,
             QSORRES  = col_or_na(qs_prep, str_c(field, "_RAW")))
  }) |>
    list_rbind()

  # Form not performed: one STAT row per item. The collected reason text
  # (QSNRA) is the REASND; results are blank by definition.
  not_done <- pivoted |>
    filter(QSPERF == "0") |>
    mutate(QSSTAT   = "NOT DONE",
           QSREASND = str_squish(coalesce(QSNRA, "")),
           QSSTRESN = NA_real_)

  # Performed items: keep only actual answers; an item left blank on a
  # performed form is not an SDTM record.
  answered <- pivoted |>
    filter(QSPERF != "0", !is.na(QSORRES), QSORRES != "") |>
    mutate(QSSTRESN = suppressWarnings(as.numeric(QSORRES)))

  bind_rows(answered, not_done) |>
    mutate(
      STUDYID = spec$study$STUDYID,
      DOMAIN  = "QS",
      USUBJID = usubjid,
      QSDTC   = rave_dtc(QSDAT_YYYY, QSDAT_MM, QSDAT_DD)
    ) |>
    left_join(spec$visits |>
                select(Folder, VISITNUM, VISIT),
              by = "Folder") |>
    left_join(refs, by = "USUBJID") |>
    mutate(QSDY = derive_dy(QSDTC, RFSTDTC)) |>
    # Baseline = last non-missing numeric result on or before first dose
    # (date-level boundary by design - RFSTDTC has no time; see map_vs())
    mutate(
      .eligible = !is.na(QSSTRESN) & !is.na(RFSTDTC) &
        dtc_date(QSDTC) <= dtc_date(RFSTDTC),
      # NA key for ineligible rows so they are excluded from the ranking
      .key  = if_else(.eligible, as.numeric(dtc_date(QSDTC)), NA_real_),
      .rank = rank(-.key, ties.method = "first", na.last = "keep"),
      .by = c(USUBJID, QSTESTCD)
    ) |>
    mutate(QSBLFL = if_else(!is.na(.rank) & .rank == 1, "Y", NA_character_)) |>
    select(-.eligible, -.key, -.rank) |>
    derive_seq("QSSEQ", VISITNUM, QSTESTCD) |>
    select(STUDYID, DOMAIN, USUBJID, QSSEQ, QSCAT, QSTESTCD, QSTEST,
           QSORRES, QSSTAT, QSREASND, QSBLFL, VISITNUM, VISIT, QSDTC, QSDY) |>
    arrange(USUBJID, VISITNUM, QSSEQ) |>
    apply_labels(c(
      STUDYID  = "Study Identifier",
      DOMAIN   = "Domain Abbreviation",
      USUBJID  = "Unique Subject Identifier",
      QSSEQ    = "Sequence Number",
      QSCAT    = "Category of Questionnaire",
      QSTESTCD = "Question Short Name",
      QSTEST   = "Question",
      QSORRES  = "Result or Finding in Original Units",
      QSSTAT   = "Completion Status",
      QSREASND = "Reason Not Done",
      QSBLFL   = "Baseline Flag",
      VISITNUM = "Visit Number",
      VISIT    = "Visit Name",
      QSDTC    = "Date/Time of Questionnaire",
      QSDY     = "Study Day of Questionnaire"
    ))
}

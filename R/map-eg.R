# ============================================================================
# Title:   SDTM EG - ECG Test Results
# Purpose: Pivot the ECG form into the SDTM findings structure. The
#          per-interval pivot spec lives in spec$tests - adding an interval
#          is a spec row, not a code change.
# ============================================================================

#' Map the ECG Test Results domain
#'
#' One ECG form row (the interval results side by side) becomes one row per
#' interval, via the `spec$tests` EG rows. The pivot reads the collected
#' `_RAW` values; EGSTRESN parses them (milliseconds) and EGSTRESU marks
#' every parseable result "msec". A form marked not performed (EGPERF "0")
#' becomes one NOT DONE row per interval with EGREASND carrying the collected
#' reason and no result at all - EGDAT/EGTIM still ride on the form, so those
#' rows keep their collected date/time.
#'
#' Baseline (EGBLFL) mirrors VSBLFL: the last non-missing numeric result on
#' or before first dose, per subject per interval. Only EGSTRESN rows are
#' eligible, so NOT DONE records are never flagged - and screen failures get
#' none because RFSTDTC is the anchor.
#'
#' @param eg Raw EG clinical view
#' @param spec A `study_spec`
#' @param refs Subject reference dates from [subject_ref()]
#' @return The labelled SDTM EG tibble.
#' @examples
#' ext <- file.path(tempdir(), "ex-map-eg")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' forms <- suppressMessages(read_rave_extract(dir = ext))
#' dm   <- map_dm(forms$DM, forms$EX, forms$DS, spec_synth01)
#' eg   <- map_eg(forms$EG, spec_synth01, subject_ref(dm))
#' head(eg[, c("USUBJID", "EGTESTCD", "EGORRES", "EGSTRESN", "EGDTC")])
#' @export
map_eg <- function(eg, spec, refs) {
  eg_prep <- eg |> mutate(usubjid = str_c(spec$study$STUDYID, Subject, sep = "-"))
  eg_spec <- filter(spec$tests, domain == "EG")

  pivoted <- pmap(eg_spec, \(domain, field, testcd, test, cat, specimen) {
    eg_prep |>
      mutate(EGTESTCD = testcd,
             EGTEST   = test,
             EGORRES  = col_or_na(eg_prep, str_c(field, "_RAW")))
  }) |>
    list_rbind()

  # Form not performed: one STAT row per interval. The collected reason text
  # (EGNRA) is the REASND; results are blank by definition.
  not_done <- pivoted |>
    filter(EGPERF == "0") |>
    mutate(EGSTAT   = "NOT DONE",
           EGREASND = str_squish(coalesce(EGNRA, "")),
           EGORRES  = NA_character_,
           EGSTRESN = NA_real_,
           EGSTRESU = NA_character_)

  # Performed items: keep only actual answers; an interval left blank on a
  # performed form is not an SDTM record.
  answered <- pivoted |>
    filter(EGPERF != "0", !is.na(EGORRES), EGORRES != "") |>
    mutate(EGSTRESN = suppressWarnings(as.numeric(EGORRES)),
           EGSTRESU = if_else(is.na(EGSTRESN), NA_character_, "msec"))

  bind_rows(answered, not_done) |>
    mutate(
      STUDYID = spec$study$STUDYID,
      DOMAIN  = "EG",
      USUBJID = usubjid,
      # a blank EGTIM keeps EGDTC at date precision - collected messiness,
      # not an imputation
      EGDTC   = rave_dtc(EGDAT_YYYY, EGDAT_MM, EGDAT_DD, time = EGTIM)
    ) |>
    left_join(spec$visits |>
                select(Folder, VISITNUM, VISIT),
              by = "Folder") |>
    left_join(refs, by = "USUBJID") |>
    mutate(EGDY = derive_dy(EGDTC, RFSTDTC)) |>
    # Baseline = last non-missing numeric result on or before first dose
    # (date-level boundary by design - RFSTDTC has no time; see map_vs())
    mutate(
      .eligible = !is.na(EGSTRESN) & !is.na(RFSTDTC) &
        dtc_date(EGDTC) <= dtc_date(RFSTDTC),
      # NA key for ineligible rows so they are excluded from the ranking
      .key  = if_else(.eligible, as.numeric(dtc_date(EGDTC)), NA_real_),
      .rank = rank(-.key, ties.method = "first", na.last = "keep"),
      .by = c(USUBJID, EGTESTCD)
    ) |>
    mutate(EGBLFL = if_else(!is.na(.rank) & .rank == 1, "Y", NA_character_)) |>
    select(-.eligible, -.key, -.rank) |>
    derive_seq("EGSEQ", VISITNUM, EGTESTCD) |>
    select(STUDYID, DOMAIN, USUBJID, EGSEQ, EGTESTCD, EGTEST,
           EGORRES, EGSTRESN, EGSTRESU, EGSTAT, EGREASND, EGBLFL,
           VISITNUM, VISIT, EGDTC, EGDY) |>
    arrange(USUBJID, VISITNUM, EGSEQ) |>
    apply_labels(c(
      STUDYID  = "Study Identifier",
      DOMAIN   = "Domain Abbreviation",
      USUBJID  = "Unique Subject Identifier",
      EGSEQ    = "Sequence Number",
      EGTESTCD = "ECG Test Short Name",
      EGTEST   = "ECG Test Name",
      EGORRES  = "Result or Finding in Original Units",
      EGSTRESN = "Numeric Result/Finding in Std Units",
      EGSTRESU = "Standard Units",
      EGSTAT   = "Completion Status",
      EGREASND = "Reason Not Performed",
      EGBLFL   = "Baseline Flag",
      VISITNUM = "Visit Number",
      VISIT    = "Visit Name",
      EGDTC    = "Date/Time of ECG",
      EGDY     = "Study Day of ECG"
    ))
}

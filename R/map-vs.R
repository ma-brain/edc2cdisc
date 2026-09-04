# ============================================================================
# Title:   SDTM VS - Vital Signs
# Purpose: Pivot the horizontal clinical view into the SDTM findings
#          structure. The per-test pivot spec lives in spec$tests - adding a
#          vital sign to the study is a spec row, not a code change.
# ============================================================================

#' Map the Vital Signs domain
#'
#' One row per CRF field becomes one row per test. Standard units come from
#' the collected _STD columns where the EDC system converted, with
#' [resolve_standard()] as the fallback. Baseline (VSBLFL) is the last
#' non-missing standard result on or before first dose, per subject per
#' test - screen failures get none because RFSTDTC is the anchor.
#'
#' The baseline boundary is the first-dose *date*: RFSTDTC carries no time
#' in this pipeline, so VSDTC's time component is deliberately not compared
#' and a same-day measurement taken after dosing stays baseline-eligible.
#' If a study's SAP wants a pre-dose-time boundary, that is a rule change
#' here, not a data property.
#'
#' @param vs Raw VS clinical view
#' @param spec A `study_spec`
#' @param refs Subject reference dates from [subject_ref()]
#' @return The labelled SDTM VS tibble.
#' @examples
#' ext <- file.path(tempdir(), "ex-map-vs")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' forms <- suppressMessages(read_rave_extract(dir = ext))
#' dm   <- map_dm(forms$DM, forms$EX, forms$DS, spec_synth01)
#' vs   <- map_vs(forms$VS, spec_synth01, subject_ref(dm))
#' head(vs[, c("USUBJID", "VSTESTCD", "VSORRES", "VSSTRESN", "VSBLFL")])
#' @export
map_vs <- function(vs, spec, refs) {
  vs_spec <- filter(spec$tests, domain == "VS")

  vs_long <- pmap(vs_spec, \(domain, field, testcd, test, cat, specimen) {
    vs |>
      mutate(
        VSTESTCD  = testcd,
        VSTEST    = test,
        VSORRES   = col_or_na(vs, str_c(field, "_RAW")),
        VSORRESU  = col_or_na(vs, str_c(field, "_UN")),
        RAVE_STD  = col_or_na(vs, str_c(field, "_STD")),
        RAVE_STDU = col_or_na(vs, str_c(field, "_STD_UN"))
      )
  }) |>
    list_rbind()

  # The EDC already converted the fields that have a standard unit
  # configured (temperature, weight, height at the US site). Anything else
  # falls back to a local conversion, which for mmHg and beats/min is the
  # identity.
  vs_std <- resolve_standard(vs_long$RAVE_STD, vs_long$RAVE_STDU,
                             vs_long$VSORRES, vs_long$VSORRESU)

  vs <- vs_long |>
    mutate(
      VSSTAT   = if_else(VSPERF == "0", "NOT DONE", NA_character_),
      VSREASND = if_else(VSPERF == "0", "VISIT NOT PERFORMED", NA_character_),
      VSSTRESN = vs_std$stresn,
      VSSTRESU = if_else(is.na(vs_std$stresn), NA_character_, vs_std$stresu),
      VSSTRESC = as.character(VSSTRESN)
    ) |>
    # Drop tests not collected at this visit (e.g. height after screening),
    # but keep explicit NOT DONE records.
    filter(!is.na(VSORRES) | VSSTAT == "NOT DONE") |>
    transmute(
      STUDYID  = spec$study$STUDYID,
      DOMAIN   = "VS",
      USUBJID  = str_c(spec$study$STUDYID, Subject, sep = "-"),
      VSTESTCD, VSTEST,
      VSPOS    = str_to_upper(VSPOS_DECODE),
      VSORRES, VSORRESU, VSSTRESC, VSSTRESN, VSSTRESU,
      VSSTAT, VSREASND,
      Folder   = Folder,
      VSDTC    = rave_dtc(VSDAT_YYYY, VSDAT_MM, VSDAT_DD, time = VSTIM)
    ) |>
    left_join(spec$visits |>
                select(Folder, VISITNUM, VISIT, EPOCH),
              by = "Folder") |>
    left_join(refs, by = "USUBJID") |>
    mutate(VSDY = derive_dy(VSDTC, RFSTDTC))

  # Baseline = last non-missing result on or before first dose
  # (date-level boundary by design - RFSTDTC has no time; see roxygen)
  vs <- vs |>
    mutate(
      .eligible = !is.na(VSSTRESN) & !is.na(RFSTDTC) &
        dtc_date(VSDTC) <= dtc_date(RFSTDTC),
      # NA key for ineligible rows so they are excluded from the ranking
      .key  = if_else(.eligible, as.numeric(dtc_date(VSDTC)), NA_real_),
      .rank = rank(-.key, ties.method = "first", na.last = "keep"),
      .by = c(USUBJID, VSTESTCD)
    ) |>
    mutate(VSBLFL = if_else(!is.na(.rank) & .rank == 1, "Y", NA_character_)) |>
    select(-.eligible, -.key, -.rank)

  vs |>
    derive_seq("VSSEQ", VISITNUM, VSTESTCD) |>
    select(STUDYID, DOMAIN, USUBJID, VSSEQ, VSTESTCD, VSTEST, VSPOS,
           VSORRES, VSORRESU, VSSTRESC, VSSTRESN, VSSTRESU, VSSTAT, VSREASND,
           VSBLFL, VISITNUM, VISIT, EPOCH, VSDTC, VSDY) |>
    arrange(USUBJID, VSSEQ) |>
    apply_labels(c(
      STUDYID  = "Study Identifier",
      DOMAIN   = "Domain Abbreviation",
      USUBJID  = "Unique Subject Identifier",
      VSSEQ    = "Sequence Number",
      VSTESTCD = "Vital Signs Test Short Name",
      VSTEST   = "Vital Signs Test Name",
      VSPOS    = "Vital Signs Position of Subject",
      VSORRES  = "Result or Finding in Original Units",
      VSORRESU = "Original Units",
      VSSTRESC = "Character Result/Finding in Std Units",
      VSSTRESN = "Numeric Result/Finding in Std Units",
      VSSTRESU = "Standard Units",
      VSSTAT   = "Completion Status",
      VSREASND = "Reason Not Performed",
      VSBLFL   = "Baseline Flag",
      VISITNUM = "Visit Number",
      VISIT    = "Visit Name",
      EPOCH    = "Epoch",
      VSDTC    = "Date/Time of Measurements",
      VSDY     = "Study Day of Vital Signs"
    ))
}

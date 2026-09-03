# ============================================================================
# Title:   SDTM LB - Laboratory Test Results
# Purpose: Pivot the horizontal clinical view into the findings structure,
#          standardise to SI units, carry the reference range, derive
#          LBNRIND. Unit handling is analyte-aware: "mg/dL" means a different
#          conversion for glucose than for creatinine, so the factor is keyed
#          on the test (spec$units), not the unit alone. A populated EDC
#          _STD column still wins - the local factor is only the fallback.
# ============================================================================

#' Map the Laboratory Test Results domain
#'
#' The per-analyte pivot spec comes from `spec$tests` (domain "LB") and the
#' conventional-to-SI conversion factors from `spec$units`. Baseline
#' (LBBLFL) is the last non-missing standard result on or before first dose.
#'
#' @param lb Raw LB clinical view
#' @param spec A `study_spec`
#' @param refs Subject reference dates from [subject_ref()]
#' @return The labelled SDTM LB tibble.
#' @export
map_lb <- function(lb, spec, refs) {
  lb_spec <- filter(spec$tests, domain == "LB")

  # One row per analyte
  lb_long <- pmap(lb_spec, \(domain, field, testcd, test, cat, specimen) {
    lb |>
      mutate(
        LBTESTCD  = testcd, LBTEST = test, LBCAT = cat, LBSPEC = specimen,
        LBORRES   = col_or_na(lb, str_c(field, "_RAW")),
        LBORRESU  = col_or_na(lb, str_c(field, "_UN")),
        RAVE_STD  = col_or_na(lb, str_c(field, "_STD")),
        RAVE_STDU = col_or_na(lb, str_c(field, "_STD_UN")),
        NRLO_O    = col_or_na(lb, str_c(field, "_NRLO")),
        NRHI_O    = col_or_na(lb, str_c(field, "_NRHI"))
      )
  }) |>
    list_rbind() |>
    left_join(spec$units |>
                rename(LBTESTCD   = testcd,
                       CONV_FROM  = conv_from,
                       CONV_TO    = conv_to,
                       CONV_FACTOR = conv_factor),
              by = "LBTESTCD")

  lb <- lb_long |>
    mutate(
      .orres_n = suppressWarnings(as.numeric(LBORRES)),
      .rave_n  = suppressWarnings(as.numeric(RAVE_STD)),
      # apply the analyte factor only when the collected unit is the one it
      # converts from; otherwise the value is already standard -> identity
      .factor  = if_else(!is.na(CONV_FACTOR) &
                           str_to_upper(str_trim(LBORRESU)) == CONV_FROM,
                         CONV_FACTOR, 1),
      LBSTAT   = if_else(LBPERF == "0", "NOT DONE", NA_character_),
      LBREASND = if_else(LBPERF == "0", "PANEL NOT PERFORMED", NA_character_),
      # the EDC-configured conversion wins; local factor is the fallback
      LBSTRESN = if_else(!is.na(.rave_n), .rave_n, round(.orres_n * .factor, 4)),
      LBSTRESU = case_when(
        !is.na(RAVE_STDU) & RAVE_STDU != "" ~ RAVE_STDU,
        !is.na(CONV_TO)                     ~ CONV_TO,
        .default                            = LBORRESU
      ),
      LBSTRESC = as.character(LBSTRESN),
      # reference range: same factor as the result, so the comparison below
      # is done entirely in standard units
      LBORNRLO = suppressWarnings(as.numeric(NRLO_O)),
      LBORNRHI = suppressWarnings(as.numeric(NRHI_O)),
      LBSTNRLO = round(LBORNRLO * .factor, 4),
      LBSTNRHI = round(LBORNRHI * .factor, 4),
      LBNRIND  = case_when(
        is.na(LBSTRESN) | is.na(LBSTNRLO) | is.na(LBSTNRHI) ~ NA_character_,
        LBSTRESN < LBSTNRLO                                  ~ "LOW",
        LBSTRESN > LBSTNRHI                                  ~ "HIGH",
        .default                                             = "NORMAL"
      )
    ) |>
    # Drop analytes not collected at this visit, keep explicit NOT DONE rows.
    filter(!is.na(LBORRES) | LBSTAT == "NOT DONE") |>
    transmute(
      STUDYID  = spec$study$STUDYID,
      DOMAIN   = "LB",
      USUBJID  = str_c(spec$study$STUDYID, Subject, sep = "-"),
      LBTESTCD, LBTEST, LBCAT, LBSPEC,
      LBORRES, LBORRESU, LBSTRESC, LBSTRESN, LBSTRESU,
      LBORNRLO, LBORNRHI, LBSTNRLO, LBSTNRHI, LBNRIND,
      LBSTAT, LBREASND,
      LBFAST   = yn(LBFAST),
      Folder   = Folder,
      LBDTC    = rave_dtc(LBDAT_YYYY, LBDAT_MM, LBDAT_DD, time = LBTIM)
    ) |>
    left_join(spec$visits |>
                select(Folder, VISITNUM, VISIT, EPOCH),
              by = "Folder") |>
    left_join(refs, by = "USUBJID") |>
    mutate(LBDY = derive_dy(LBDTC, RFSTDTC))

  # Baseline = last non-missing result on or before first dose
  lb <- lb |>
    mutate(
      .eligible = !is.na(LBSTRESN) & !is.na(RFSTDTC) &
        dtc_date(LBDTC) <= dtc_date(RFSTDTC),
      .key  = if_else(.eligible, as.numeric(dtc_date(LBDTC)), NA_real_),
      .rank = rank(-.key, ties.method = "first", na.last = "keep"),
      .by = c(USUBJID, LBTESTCD)
    ) |>
    mutate(LBBLFL = if_else(!is.na(.rank) & .rank == 1, "Y", NA_character_)) |>
    select(-.eligible, -.key, -.rank)

  lb |>
    derive_seq("LBSEQ", VISITNUM, LBTESTCD) |>
    select(STUDYID, DOMAIN, USUBJID, LBSEQ, LBTESTCD, LBTEST, LBCAT, LBSPEC,
           LBORRES, LBORRESU, LBSTRESC, LBSTRESN, LBSTRESU,
           LBORNRLO, LBORNRHI, LBSTNRLO, LBSTNRHI, LBNRIND,
           LBSTAT, LBREASND, LBFAST, LBBLFL,
           VISITNUM, VISIT, EPOCH, LBDTC, LBDY) |>
    arrange(USUBJID, LBSEQ) |>
    apply_labels(c(
      STUDYID  = "Study Identifier",
      DOMAIN   = "Domain Abbreviation",
      USUBJID  = "Unique Subject Identifier",
      LBSEQ    = "Sequence Number",
      LBTESTCD = "Lab Test or Examination Short Name",
      LBTEST   = "Lab Test or Examination Name",
      LBCAT    = "Category for Lab Test",
      LBSPEC   = "Specimen Type",
      LBORRES  = "Result or Finding in Original Units",
      LBORRESU = "Original Units",
      LBSTRESC = "Character Result/Finding in Std Units",
      LBSTRESN = "Numeric Result/Finding in Std Units",
      LBSTRESU = "Standard Units",
      LBORNRLO = "Reference Range Lower Limit-Orig Unit",
      LBORNRHI = "Reference Range Upper Limit-Orig Unit",
      LBSTNRLO = "Reference Range Lower Limit-Std Units",
      LBSTNRHI = "Reference Range Upper Limit-Std Units",
      LBNRIND  = "Reference Range Indicator",
      LBSTAT   = "Completion Status",
      LBREASND = "Reason Not Performed",
      LBFAST   = "Fasting Status",
      LBBLFL   = "Baseline Flag",
      VISITNUM = "Visit Number",
      VISIT    = "Visit Name",
      EPOCH    = "Epoch",
      LBDTC    = "Date/Time of Specimen Collection",
      LBDY     = "Study Day of Specimen Collection"
    ))
}

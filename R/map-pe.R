# ============================================================================
# Title:   SDTM PE - Physical Examination
# Purpose: Pivot the physical exam form into the SDTM findings structure.
#          The per-system pivot spec lives in spec$tests - adding an exam
#          system is a spec row, not a code change.
# ============================================================================

#' Map the Physical Examination domain
#'
#' One physical exam form row (the system findings side by side) becomes one
#' row per system, via the `spec$tests` PE rows. The pivot reads the coded
#' `_DECODE` columns (Normal/Abnormal via the PEFIND codelist) and uppercases
#' them into PEORRES, the VSPOS idiom; PECLSIG is "Y" exactly for the
#' ABNORMAL findings. A form marked not performed (PEPERF "0") becomes one
#' NOT DONE row per system with PEREASND carrying the collected reason and no
#' result at all - PEDAT still rides on the form, so those rows keep their
#' visit date.
#'
#' PE carries no baseline flag by design: a physical exam is a set of
#' normal/abnormal observations, not a rankable measurement, and PECLSIG
#' already carries the clinical significance of each finding at every visit.
#'
#' @param pe Raw PE clinical view
#' @param spec A `study_spec`
#' @param refs Subject reference dates from [subject_ref()]
#' @return The labelled SDTM PE tibble.
#' @examples
#' ext <- file.path(tempdir(), "ex-map-pe")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' forms <- suppressMessages(read_rave_extract(dir = ext))
#' dm   <- map_dm(forms$DM, forms$EX, forms$DS, spec_synth01)
#' pe   <- map_pe(forms$PE, spec_synth01, subject_ref(dm))
#' head(pe[, c("USUBJID", "PETESTCD", "PEORRES", "PECLSIG", "PESTAT")])
#' @export
map_pe <- function(pe, spec, refs) {
  pe_prep <- pe |> mutate(usubjid = str_c(spec$study$STUDYID, Subject, sep = "-"))
  pe_spec <- filter(spec$tests, domain == "PE")

  pivoted <- pmap(pe_spec, \(domain, field, testcd, test, cat, specimen) {
    pe_prep |>
      mutate(PETESTCD = testcd,
             PETEST   = test,
             # the decode is the mapped value; the _RAW twin is the
             # as-entered text and is not mapped
             PEORRES  = str_to_upper(col_or_na(pe_prep, str_c(field, "_DECODE"))))
  }) |>
    list_rbind()

  # Form not performed: one STAT row per system. The collected reason text
  # (PENRA) is the REASND; the per-system stamps the form carries anyway are
  # blanked, not mapped.
  not_done <- pivoted |>
    filter(PEPERF == "0") |>
    mutate(PESTAT   = "NOT DONE",
           PEREASND = str_squish(coalesce(PENRA, "")),
           PEORRES  = NA_character_,
           PECLSIG  = NA_character_)

  # Performed items: keep only actual answers; a system left blank on a
  # performed form is not an SDTM record. The performed stamp must be
  # explicit - blank/NA PERF rows fall through to nothing rather than
  # masquerading as answered.
  answered <- pivoted |>
    filter(PEPERF == "1", !is.na(PEORRES), PEORRES != "") |>
    mutate(PECLSIG = if_else(PEORRES == "ABNORMAL", "Y", "N"))

  bind_rows(answered, not_done) |>
    mutate(
      STUDYID = spec$study$STUDYID,
      DOMAIN  = "PE",
      USUBJID = usubjid,
      PEDTC   = rave_dtc(PEDAT_YYYY, PEDAT_MM, PEDAT_DD)
    ) |>
    left_join(spec$visits |>
                select(Folder, VISITNUM, VISIT),
              by = "Folder") |>
    left_join(refs, by = "USUBJID") |>
    mutate(PEDY = derive_dy(PEDTC, RFSTDTC)) |>
    derive_seq("PESEQ", VISITNUM, PETESTCD) |>
    select(STUDYID, DOMAIN, USUBJID, PESEQ, PETESTCD, PETEST,
           PEORRES, PECLSIG, PESTAT, PEREASND, VISITNUM, VISIT, PEDTC, PEDY) |>
    arrange(USUBJID, VISITNUM, PESEQ) |>
    apply_labels(c(
      STUDYID  = "Study Identifier",
      DOMAIN   = "Domain Abbreviation",
      USUBJID  = "Unique Subject Identifier",
      PESEQ    = "Sequence Number",
      PETESTCD = "Physical Exam Test Short Name",
      PETEST   = "Physical Exam Test Name",
      PEORRES  = "Result or Finding in Original Units",
      PECLSIG  = "Clinically Significant Finding",
      PESTAT   = "Completion Status",
      PEREASND = "Reason Not Performed",
      VISITNUM = "Visit Number",
      VISIT    = "Visit Name",
      PEDTC    = "Date/Time of Examination",
      PEDY     = "Study Day of Examination"
    ))
}

# ============================================================================
# Title:   SDTM SV - Subject Visits
# Purpose: One row per subject per actual visit, built from the CRF header
#          block (Folder / FolderName / TargetDays / RecordDate) the EDC
#          system stamps on every form - no dedicated visit CRF is collected.
# ============================================================================

#' Map the Subject Visits domain
#'
#' Which forms contribute a visit is spec-driven: every form marked
#' `scheduled` in `spec$forms` (the event forms on real folders). Log forms
#' live on Folder = "LOG" and never contribute.
#'
#' @param forms A named list of raw clinical views, as returned by
#'   [read_rave_extract()]
#' @param spec A `study_spec`
#' @param refs Subject reference dates from [subject_ref()]
#' @return The labelled SDTM SV tibble.
#' @export
map_sv <- function(forms, spec, refs) {
  scheduled <- spec$forms$form_oid[spec$forms$scheduled]
  source_forms <- intersect(scheduled, names(forms))

  sv_headers <- source_forms |>
    map(\(f) {
      forms[[f]] |>
        transmute(
          USUBJID    = str_c(spec$study$STUDYID, Subject, sep = "-"),
          Folder,
          FolderName,
          # TargetDays travels in the header block as a character offset
          TargetDays = suppressWarnings(as.integer(TargetDays)),
          RECDT      = str_sub(RecordDate, 1, 10),
          FORMOID    = f
        )
    }) |>
    list_rbind() |>
    # keep only scheduled folders with a real record date
    filter(Folder %in% spec$visits$Folder, !is.na(RECDT), RECDT != "")

  # One visit = the span of record dates for that subject on that folder.
  sv_headers |>
    summarise(
      SVSTDTC    = min_dtc(RECDT),
      SVENDTC    = max_dtc(RECDT),
      TargetDays = first(TargetDays),
      .by = c(USUBJID, Folder)
    ) |>
    left_join(spec$visits |> select(Folder, VISITNUM, VISIT, EPOCH),
              by = "Folder") |>
    left_join(refs, by = "USUBJID") |>
    mutate(
      STUDYID = spec$study$STUDYID,
      DOMAIN  = "SV",
      # Planned study day of the visit: TargetDays is measured from first
      # dose (BASE = TargetDays 0), so apply the same no-day-0 rule as
      # derive_dy().
      VISITDY = if_else(TargetDays >= 0L, TargetDays + 1L, TargetDays),
      SVSTDY  = derive_dy(SVSTDTC, RFSTDTC),
      SVENDY  = derive_dy(SVENDTC, RFSTDTC)
    ) |>
    arrange(USUBJID, VISITNUM) |>
    transmute(
      STUDYID, DOMAIN, USUBJID,
      VISITNUM, VISIT, VISITDY, EPOCH,
      SVSTDTC, SVENDTC, SVSTDY, SVENDY
    ) |>
    apply_labels(c(
      STUDYID  = "Study Identifier",
      DOMAIN   = "Domain Abbreviation",
      USUBJID  = "Unique Subject Identifier",
      VISITNUM = "Visit Number",
      VISIT    = "Visit Name",
      VISITDY  = "Planned Study Day of Visit",
      EPOCH    = "Epoch",
      SVSTDTC  = "Start Date/Time of Visit",
      SVENDTC  = "End Date/Time of Visit",
      SVSTDY   = "Study Day of Start of Visit",
      SVENDY   = "Study Day of End of Visit"
    ))
}

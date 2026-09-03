# ============================================================================
# Title:   SDTM EX - Exposure
# Purpose: One EX record per dosing interval. The proof-path domain for the
#          spec engine: almost pure rename / decode / dtc rows.
# ============================================================================

#' Map the Exposure domain
#'
#' Only administrations that actually occurred (EXOCCUR == "1") become EX
#' records. Visits come from the spec by Folder; study days are anchored on
#' the RFSTDTC threaded through `refs` from the DM build.
#'
#' @param ex Raw EX clinical view
#' @param spec A `study_spec`
#' @param refs Subject reference dates from [subject_ref()]
#' @return The labelled SDTM EX tibble.
#' @examples
#' ext <- file.path(tempdir(), "ex-map-ex")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' forms <- suppressMessages(read_rave_extract(dir = ext))
#' dm   <- map_dm(forms$DM, forms$EX, forms$DS, spec_synth01)
#' ex   <- map_ex(forms$EX, spec_synth01, subject_ref(dm))
#' head(ex[, c("USUBJID", "EXSEQ", "EXTRT", "EXDOSE", "VISIT")])
#' @export
map_ex <- function(ex, spec, refs) {
  ex_f <- filter(ex, EXOCCUR == "1")

  out <- map_variables(ex_f, spec, filter(spec$variables, domain == "EX"))
  # Folder rides along only as the visit-join key, the way the script kept
  # it inside the transmute for the visit_map join.
  out$Folder <- ex_f$Folder

  out |>
    left_join(spec$visits |>
                select(Folder, VISITNUM, VISIT, EPOCH),
              by = "Folder") |>
    left_join(refs, by = "USUBJID") |>
    mutate(
      EXSTDY = derive_dy(EXSTDTC, RFSTDTC),
      EXENDY = derive_dy(EXENDTC, RFSTDTC)
    ) |>
    derive_seq("EXSEQ", VISITNUM, EXSTDTC) |>
    select(STUDYID, DOMAIN, USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU, EXROUTE,
           VISITNUM, VISIT, EPOCH, EXSTDTC, EXENDTC, EXSTDY, EXENDY) |>
    arrange(USUBJID, EXSEQ) |>
    apply_labels(c(
      STUDYID  = "Study Identifier",
      DOMAIN   = "Domain Abbreviation",
      USUBJID  = "Unique Subject Identifier",
      EXSEQ    = "Sequence Number",
      EXTRT    = "Name of Treatment",
      EXDOSE   = "Dose",
      EXDOSU   = "Dose Units",
      EXROUTE  = "Route of Administration",
      VISITNUM = "Visit Number",
      VISIT    = "Visit Name",
      EPOCH    = "Epoch",
      EXSTDTC  = "Start Date/Time of Treatment",
      EXENDTC  = "End Date/Time of Treatment",
      EXSTDY   = "Study Day of Start of Treatment",
      EXENDY   = "Study Day of End of Treatment"
    ))
}

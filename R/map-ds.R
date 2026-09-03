# ============================================================================
# Title:   SDTM DS - Disposition
# ============================================================================

#' Map the Disposition domain
#'
#' DSDECOD runs through the spec codelist (an unmapped reason breaks the
#' run) and then through the screen-failure reclassification derivation:
#' this EDC build records a screen failure on the protocol-deviation code,
#' and DS must carry the fact.
#'
#' @param ds Raw DS clinical view
#' @param spec A `study_spec`
#' @param refs Subject reference dates from [subject_ref()]
#' @return The labelled SDTM DS tibble.
#' @examples
#' ext <- file.path(tempdir(), "ex-map-ds")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' forms <- suppressMessages(read_rave_extract(dir = ext))
#' dm <- map_dm(forms$DM, forms$EX, forms$DS, spec_synth01)
#' ds <- map_ds(forms$DS, spec_synth01, subject_ref(dm))
#' head(ds[, c("USUBJID", "DSSEQ", "DSDECOD", "DSCAT")])
#' @export
map_ds <- function(ds, spec, refs) {
  out <- map_variables(ds, spec, filter(spec$variables, domain == "DS"))

  out |>
    left_join(refs, by = "USUBJID") |>
    mutate(DSSTDY = derive_dy(DSSTDTC, RFSTDTC)) |>
    derive_seq("DSSEQ", DSSTDTC, DSDECOD) |>
    select(STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT,
           DSSTDTC, DSSTDY) |>
    arrange(USUBJID, DSSEQ) |>
    apply_labels(c(
      STUDYID = "Study Identifier",
      DOMAIN  = "Domain Abbreviation",
      USUBJID = "Unique Subject Identifier",
      DSSEQ   = "Sequence Number",
      DSTERM  = "Reported Term for the Disposition Event",
      DSDECOD = "Standardized Disposition Term",
      DSCAT   = "Category for Disposition Event",
      DSSTDTC = "Start Date/Time of Disposition Event",
      DSSTDY  = "Study Day of Start of Disposition Event"
    ))
}

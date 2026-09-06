# ============================================================================
# Title:   SDTM SE - Subject Elements
# Purpose: Pivot the built DM's reference dates into one row per subject per
#          trial element actually started. The element itself (ETCD/ELEMENT)
#          is spec fact read from spec$elements - the same table map_te()
#          reads - so a drifted elements table cannot satisfy both readers;
#          only the date-source mapping lives in the mapper.
# ============================================================================

#' Map the Subject Elements domain
#'
#' One row per subject per element actually started: SCRN runs from informed
#' consent (RFICDTC) to first study treatment (RFXSTDTC); TREAT runs from
#' first dose (RFXSTDTC) to last dose (RFXENDTC). Screen failures are never
#' dosed, so they produce only the SCRN row, with SEENDTC left blank - the
#' element has not ended and no end date is invented.
#'
#' ETCD and ELEMENT come from `spec$elements` (the trial-design table
#' [map_te()] reads), never re-typed here. The start/end date sources are a
#' small ETCD-to-columns map inside the mapper: a study adding an element
#' extends the spec plus one map row, not an if-tree, and an element with no
#' mapping is a loud error rather than a silent empty domain. SESTDTC and
#' SEENDTC pass DM's ISO 8601 strings straight through, so reduced-precision
#' dates stay reduced.
#'
#' @param dm The mapped SDTM DM dataset (see [map_dm()])
#' @param spec A `study_spec`
#' @return The labelled SDTM SE tibble.
#' @examples
#' ext <- file.path(tempdir(), "ex-map-se")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' forms <- suppressMessages(read_rave_extract(dir = ext))
#' dm <- map_dm(forms$DM, forms$EX, forms$DS, spec_synth01)
#' se <- map_se(dm, spec_synth01)
#' head(se[, c("USUBJID", "ETCD", "ELEMENT", "SESTDTC", "SEENDTC")])
#' @export
map_se <- function(dm, spec) {
  # Element date sources: ETCD -> the DM columns holding the element's
  # start and end date.
  date_src <- tribble(
    ~ETCD,   ~start_col, ~end_col,
    "SCRN",  "RFICDTC",  "RFXSTDTC",
    "TREAT", "RFXSTDTC", "RFXENDTC"
  )
  unmapped <- setdiff(spec$elements$ETCD, date_src$ETCD)
  if (length(unmapped) > 0) {
    stop(sprintf("map_se: no DM date-source mapping for ETCD: %s",
                 str_flatten_comma(unmapped)), call. = FALSE)
  }

  # One branch per spec element, driven by the joined ETCD/ELEMENT +
  # date-source row (the map_qs spec-pivot shape; pmap passes the columns
  # as named arguments, so the names and the formals agree)
  spec$elements |>
    select(ETCD, ELEMENT) |>
    inner_join(date_src, by = "ETCD") |>
    rename(etcd = ETCD, element = ELEMENT) |>
    pmap(\(etcd, element, start_col, end_col) {
      dm |>
        transmute(
          STUDYID = spec$study$STUDYID,
          DOMAIN  = "SE",
          USUBJID,
          ETCD    = etcd,
          ELEMENT = element,
          SESTDTC = .data[[start_col]],
          SEENDTC = .data[[end_col]]
        ) |>
        # an element is recorded only where it actually started; the end
        # date may stay blank (screen-failure SCRN)
        filter(!is.na(SESTDTC), SESTDTC != "")
    }) |>
    list_rbind() |>
    # within a subject the elements run in ETCD order (SCRN=1, TREAT=2)
    derive_seq("SESEQ", ETCD) |>
    select(STUDYID, DOMAIN, USUBJID, SESEQ, ETCD, ELEMENT,
           SESTDTC, SEENDTC) |>
    arrange(USUBJID, SESEQ) |>
    apply_labels(c(
      STUDYID  = "Study Identifier",
      DOMAIN   = "Domain Abbreviation",
      USUBJID  = "Unique Subject Identifier",
      SESEQ    = "Sequence Number",
      ETCD     = "Element Code",
      ELEMENT  = "Description of Element",
      SESTDTC  = "Start Date/Time of Subject Element",
      SEENDTC  = "End Date/Time of Subject Element"
    ))
}

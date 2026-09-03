# ============================================================================
# Title:   The full build pipeline
# Purpose: Replace the numbered run_all.R script order with one function:
#          read the extract, map every SDTM domain, validate, derive the
#          ADaM layer, validate again, optionally write RDS + XPT + define.
#          The run order lives here, not in file names.
# ============================================================================

#' Run the full EDC -> SDTM -> ADaM pipeline
#'
#' Reads the clinical view extract from `extract_dir`, maps every SDTM
#' domain through the study spec, validates them, derives the ADaM analysis
#' datasets, validates those too, and optionally writes RDS + SAS v5 XPT
#' files plus a define.xml stub. Any validation ERROR stops the build before
#' anything is written.
#'
#' @param extract_dir Directory holding the clinical view extract (as
#'   written by [generate_rave_extract()])
#' @param spec A `study_spec`; defaults to the shipped [spec_synth01]
#' @param sdtm_dir Optional output directory for the SDTM domains. When
#'   given, each domain is written as `<domain>.rds` plus `xpt/<domain>.xpt`,
#'   and a `define.xml` stub is written alongside.
#' @param adam_dir Optional output directory for the ADaM datasets, laid out
#'   like `sdtm_dir`.
#' @return A list with `sdtm` (named list of the 14 mapped domains) and
#'   `adam` (ADSL, ADAE, ADVS, ADLB), invisibly.
#' @export
#' @examples
#' # regenerate the synthetic extract and run the whole pipeline
#' ext <- file.path(tempdir(), "extract")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' built <- build_all(ext)
#'
#' # or, writing the deliverables as RDS + XPT + define.xml
#' out <- file.path(tempdir(), "deliverables")
#' build_all(ext, sdtm_dir = file.path(out, "sdtm"),
#'           adam_dir = file.path(out, "adam"))
build_all <- function(extract_dir, spec = spec_synth01,
                      sdtm_dir = NULL, adam_dir = NULL) {
  forms <- suppressMessages(read_rave_extract(dir = extract_dir))

  # ---- SDTM --------------------------------------------------------------
  dm <- map_dm(forms$DM, forms$EX, forms$DS, spec)
  refs <- subject_ref(dm)
  ex   <- map_ex(forms$EX, spec, refs)
  vs   <- map_vs(forms$VS, spec, refs)
  ae   <- map_ae(forms$AE, spec, refs)
  cm   <- map_cm(forms$CM, spec, refs)
  ds   <- map_ds(forms$DS, spec, refs)
  sv   <- map_sv(forms, spec, refs)
  lb   <- map_lb(forms$LB, spec, refs)
  mh   <- map_mh(forms$MH, spec, refs)
  suppdm <- map_suppdm(forms$DM, spec)
  suppae <- map_suppae(forms$AE, ae, spec)
  suppex <- map_suppex(forms$EX, ex, spec)
  co     <- map_co(forms$AE, ae, spec)
  relrec <- map_relrec(ae, cm, spec)

  sdtm <- list(DM = dm, EX = ex, VS = vs, AE = ae, CM = cm, DS = ds, SV = sv,
               LB = lb, MH = mh, SUPPDM = suppdm, SUPPAE = suppae,
               SUPPEX = suppex, CO = co, RELREC = relrec)

  issues <- validate_sdtm(sdtm, spec)
  stop_on_error(issues, "SDTM validation")
  if (nrow(issues) > 0) {
    message("build_all: SDTM validation warnings: ", nrow(issues))
  }

  # ---- ADaM --------------------------------------------------------------
  adsl <- derive_adsl(dm, ex, ds)
  adae <- derive_adae(ae, suppae, adsl)
  advs <- derive_advs(vs, adsl)
  adlb <- derive_adlb(lb, adsl)

  adam <- list(ADSL = adsl, ADAE = adae, ADVS = advs, ADLB = adlb)

  issues <- validate_adam(adsl, adae, advs, adlb,
                          dm, ds, ae, vs, lb, suppae)
  stop_on_error(issues, "ADaM validation")
  if (nrow(issues) > 0) {
    message("build_all: ADaM validation warnings: ", nrow(issues))
  }

  # ---- Write ---------------------------------------------------------------
  if (!is.null(sdtm_dir)) {
    for (d in names(sdtm)) {
      write_sdtm(sdtm[[d]], d, rds_dir = sdtm_dir,
                 xpt_dir = file.path(sdtm_dir, "xpt"))
    }
    build_define_xml(sdtm, spec, file.path(sdtm_dir, "define.xml"))
    message("build_all: ", length(sdtm), " SDTM domain(s) written to ",
            sdtm_dir)
  }
  if (!is.null(adam_dir)) {
    for (d in names(adam)) {
      write_sdtm(adam[[d]], d, rds_dir = adam_dir,
                 xpt_dir = file.path(adam_dir, "xpt"))
    }
    message("build_all: ", length(adam), " ADaM dataset(s) written to ",
            adam_dir)
  }

  invisible(list(sdtm = sdtm, adam = adam))
}

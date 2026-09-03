# ============================================================================
# Title:   SDTM CO and RELREC mappers
# ============================================================================

#' Map the Comments domain
#'
#' Carries the free-text investigator comment collected on the AE form. Free
#' text belongs in CO, not SUPP: SUPP is for structured name/value
#' qualifiers, CO is for narrative tied to a record. Structure parallels
#' SUPPAE - RDOMAIN / IDVAR / IDVARVAL point back at one AE record.
#'
#' @param ae Raw AE clinical view
#' @param ae_built The mapped AE dataset (see [map_ae()])
#' @param spec A `study_spec`
#' @return The labelled SDTM CO tibble.
#' @examples
#' ext <- file.path(tempdir(), "ex-map-co")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' forms <- suppressMessages(read_rave_extract(dir = ext))
#' dm <- map_dm(forms$DM, forms$EX, forms$DS, spec_synth01)
#' ae <- map_ae(forms$AE, spec_synth01, subject_ref(dm))
#' co <- map_co(forms$AE, ae, spec_synth01)
#' head(co[, c("USUBJID", "COSEQ", "COVAL")])
#' @export
map_co <- function(ae, ae_built, spec) {
  ae |>
    transmute(
      USUBJID = str_c(spec$study$STUDYID, Subject, sep = "-"),
      AESPID  = as.character(recordposition),
      COVAL   = na_if(str_squish(AECOMNT), "")
    ) |>
    filter(!is.na(COVAL)) |>
    inner_join(select(ae_built, USUBJID, AESPID, AESEQ),
               by = c("USUBJID", "AESPID")) |>
    mutate(
      STUDYID  = spec$study$STUDYID,
      DOMAIN   = "CO",
      RDOMAIN  = "AE",
      IDVAR    = "AESEQ",
      IDVARVAL = as.character(AESEQ)
    ) |>
    derive_seq("COSEQ", as.integer(IDVARVAL)) |>
    select(STUDYID, DOMAIN, RDOMAIN, USUBJID, COSEQ, IDVAR, IDVARVAL, COVAL) |>
    arrange(USUBJID, COSEQ) |>
    apply_labels(c(
      STUDYID  = "Study Identifier",
      DOMAIN   = "Domain Abbreviation",
      RDOMAIN  = "Related Domain Abbreviation",
      USUBJID  = "Unique Subject Identifier",
      COSEQ    = "Sequence Number",
      IDVAR    = "Identifying Variable",
      IDVARVAL = "Identifying Variable Value",
      COVAL    = "Comment"
    ))
}

#' Map the Relrec (Related Records) domain
#'
#' Pairs each concomitant medication with the adverse event it treated. The
#' generator (or a real extract) stamps the AE log line on the CM record
#' (CMSPID) where the indication matches; this mapper resolves the pairs to
#' their --SEQ keys. A link is a claim about the data, so both halves are
#' checked loudly: every stamped CM must resolve to a real AE log line, and
#' the CM's indication must be the event it points at.
#'
#' @param ae_built The mapped AE dataset
#' @param cm_built The mapped CM dataset
#' @param spec A `study_spec`
#' @return The labelled SDTM RELREC tibble.
#' @examples
#' ext <- file.path(tempdir(), "ex-map-relrec")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' forms <- suppressMessages(read_rave_extract(dir = ext))
#' dm <- map_dm(forms$DM, forms$EX, forms$DS, spec_synth01)
#' refs <- subject_ref(dm)
#' ae <- map_ae(forms$AE, spec_synth01, refs)
#' cm <- map_cm(forms$CM, spec_synth01, refs)
#' relrec <- map_relrec(ae, cm, spec_synth01)
#' head(relrec[, c("USUBJID", "RDOMAIN", "IDVARVAL", "RELID")])
#' @export
map_relrec <- function(ae_built, cm_built, spec) {
  linked_cm <- cm_built |>
    filter(!is.na(CMSPID), CMSPID != "")

  orphan <- linked_cm |>
    anti_join(ae_built |> select(USUBJID, AESPID),
              by = c("USUBJID", "CMSPID" = "AESPID"))
  if (nrow(orphan) > 0) {
    stop(sprintf("RELREC: %d CM record(s) with CMSPID not in AE (e.g. %s %s)",
                 nrow(orphan), orphan$USUBJID[1], orphan$CMSPID[1]),
         call. = FALSE)
  }
  mismatch <- linked_cm |>
    inner_join(ae_built |> select(USUBJID, AESPID, AEDECOD),
               by = c("USUBJID", "CMSPID" = "AESPID")) |>
    filter(str_to_upper(CMINDC) != str_to_upper(AEDECOD))
  if (nrow(mismatch) > 0) {
    stop(sprintf(paste("RELREC: %d linked CM record(s) whose CMINDC does not",
                       "match the linked AE's term"), nrow(mismatch)),
         call. = FALSE)
  }

  make_relrec(
    linked_cm |>
      inner_join(ae_built |> select(USUBJID, AESPID, AESEQ),
                 by = c("USUBJID", "CMSPID" = "AESPID")) |>
      transmute(
        USUBJID,
        RDOMAIN_1  = "CM", IDVAR_1 = "CMSEQ", IDVARVAL_1 = CMSEQ,
        RDOMAIN_2  = "AE", IDVAR_2 = "AESEQ", IDVARVAL_2 = AESEQ
      ),
    study_id = spec$study$STUDYID
  )
}

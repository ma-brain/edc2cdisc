# ============================================================================
# Title:   Supplemental qualifier and record-linkage assembly
# ============================================================================

#' Assemble a Supplemental Qualifiers (SUPP--) dataset
#'
#' Pivots one or more non-standard parent-domain columns into the long
#' RDOMAIN / IDVAR / IDVARVAL / QNAM / QVAL structure. A row is emitted only
#' where the value is present: QVAL is required, so a blank qualifier simply
#' produces no SUPP record (a "specify" field appears only for the subjects who
#' triggered it; a Y/N field appears for everyone it was asked of).
#'
#' @param parent  Parent data frame. Must contain STUDYID, USUBJID, every
#'   column named in `qnams$src` (defaulting to `qnams$qnam`), and - unless
#'   `idvar` is NA - the column named by `idvar`.
#' @param rdomain Parent domain code, e.g. "DM" or "AE".
#' @param idvar   Name of the variable that ties a qualifier to one parent
#'   record, e.g. "AESEQ". NA for a one-record-per-subject parent such as DM,
#'   where IDVAR / IDVARVAL are left blank and USUBJID alone is the link.
#' @param qnams   A data frame of qualifiers, one row per QNAM, with columns
#'   `qnam`, `qlabel`, optionally `src` (the column in `parent` holding the
#'   value; defaults to `qnam`), `qorig` (default "CRF") and `qeval`
#'   (default NA).
#' @return The SUPP-- data frame, ordered and labelled.
#' @export
make_supp <- function(parent, rdomain, idvar, qnams) {
  src   <- if ("src"   %in% names(qnams)) qnams$src   else qnams$qnam
  qorig <- if ("qorig" %in% names(qnams)) qnams$qorig else rep("CRF", nrow(qnams))
  qeval <- if ("qeval" %in% names(qnams)) qnams$qeval else rep(NA_character_, nrow(qnams))

  idvarval <- if (is.na(idvar)) NA_character_ else as.character(parent[[idvar]])

  pmap(
    list(qnam = qnams$qnam, qlabel = qnams$qlabel, src = src,
         qorig = qorig, qeval = qeval),
    \(qnam, qlabel, src, qorig, qeval) tibble(
      STUDYID  = as.character(parent$STUDYID),
      RDOMAIN  = rdomain,
      USUBJID  = parent$USUBJID,
      IDVAR    = if (is.na(idvar)) NA_character_ else idvar,
      IDVARVAL = idvarval,
      QNAM     = qnam,
      QLABEL   = qlabel,
      QVAL     = as.character(parent[[src]]),
      QORIG    = qorig,
      QEVAL    = qeval
    )
  ) |>
    list_rbind() |>
    filter(!is.na(QVAL), QVAL != "") |>
    # SUPP-- key order: parent record first (USUBJID, IDVARVAL), then QNAM
    arrange(USUBJID, suppressWarnings(as.numeric(IDVARVAL)), IDVARVAL, QNAM) |>
    apply_labels(c(
      STUDYID  = "Study Identifier",
      RDOMAIN  = "Related Domain Abbreviation",
      USUBJID  = "Unique Subject Identifier",
      IDVAR    = "Identifying Variable",
      IDVARVAL = "Identifying Variable Value",
      QNAM     = "Qualifier Variable Name",
      QLABEL   = "Qualifier Variable Label",
      QVAL     = "Data Value",
      QORIG    = "Origin",
      QEVAL    = "Evaluator"
    ))
}

#' Assemble a RELREC dataset from one-to-one record links
#'
#' RELREC holds one row per linked record: every link in `links` becomes two
#' rows - one per domain - sharing a RELID.
#'
#' @param links A data frame with columns `USUBJID`, `RDOMAIN_1`, `IDVAR_1`,
#'   `IDVARVAL_1`, `RDOMAIN_2`, `IDVAR_2`, `IDVARVAL_2` - one row per link.
#' @param study_id Study identifier, stamped into STUDYID and the RELID key.
#' @return The RELREC data frame, ordered and labelled.
#' @export
make_relrec <- function(links, study_id) {
  req <- c("USUBJID", "RDOMAIN_1", "IDVAR_1", "IDVARVAL_1",
           "RDOMAIN_2", "IDVAR_2", "IDVARVAL_2")
  missing <- setdiff(req, names(links))
  if (length(missing) > 0) {
    stop(sprintf("make_relrec: links missing %s",
                 str_flatten_comma(missing)), call. = FALSE)
  }

  rels <- map(seq_len(nrow(links)), \(i) {
    l   <- links[i, ]
    relid <- sprintf("%s-RL-%04d", study_id, i)
    bind_rows(
      tibble(STUDYID = study_id, RDOMAIN = l$RDOMAIN_1, USUBJID = l$USUBJID,
             IDVAR = l$IDVAR_1, IDVARVAL = as.character(l$IDVARVAL_1),
             RELTYPE = "ONE", RELID = relid),
      tibble(STUDYID = study_id, RDOMAIN = l$RDOMAIN_2, USUBJID = l$USUBJID,
             IDVAR = l$IDVAR_2, IDVARVAL = as.character(l$IDVARVAL_2),
             RELTYPE = "ONE", RELID = relid)
    )
  }) |>
    list_rbind() |>
    arrange(USUBJID, RELID) |>
    apply_labels(c(
      STUDYID  = "Study Identifier",
      RDOMAIN  = "Related Domain Abbreviation",
      USUBJID  = "Unique Subject Identifier",
      IDVAR    = "Identifying Variable",
      IDVARVAL = "Identifying Variable Value",
      RELTYPE  = "Relationship Type",
      RELID    = "Relationship Identifier"
    ))
  rels
}

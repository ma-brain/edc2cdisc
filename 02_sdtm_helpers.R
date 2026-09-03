# ============================================================================
# Title:   Derivation helpers
# Purpose: Partial dates, imputation, study day, age, sequence numbers,
#          unit conversion, SUPP assembly, XPT export
# ============================================================================

#' Build an ISO 8601 --DTC from Rave date component columns
#'
#' Rave splits every date field into integer year/month/day components which
#' are blank when the site entered UNK. That maps directly onto ISO 8601
#' reduced precision, so no imputation is needed or wanted here.
#'
#' @param yyyy,mm,dd Character or numeric component vectors
#' @param time Optional HH:MM character vector, only used on complete dates
rave_dtc <- function(yyyy, mm, dd, time = NULL) {
  y <- str_pad(as.character(yyyy), 4, pad = "0")
  m <- str_pad(as.character(mm), 2, pad = "0")
  d <- str_pad(as.character(dd), 2, pad = "0")

  out <- case_when(
    is.na(y)          ~ NA_character_,
    is.na(m)          ~ y,
    is.na(d)          ~ str_c(y, m, sep = "-"),
    .default          = str_c(y, m, d, sep = "-")
  )

  if (!is.null(time)) {
    tm <- str_sub(as.character(time), 1, 5)
    out <- if_else(
      !is.na(out) & str_length(out) == 10 & !is.na(tm),
      str_c(out, "T", tm),
      out
    )
  }
  out
}

#' Numeric date from a --DTC, NA unless the date is complete
dtc_date <- function(dtc) {
  as.Date(if_else(str_length(dtc) >= 10, str_sub(dtc, 1, 10), NA_character_))
}

#' Impute a partial ISO 8601 date to a full Date, with an imputation flag
#'
#' The analysis-layer counterpart to rave_dtc(): SDTM keeps reduced precision,
#' ADaM resolves it. A missing month and/or day is imputed to 01 (the "first"
#' rule, also admiral's default) and the flag records what happened:
#' "" complete, "D" day imputed, "M" month (and day) imputed.
#'
#' Note the bias this rule carries: imputing a birth date to January 1st can
#' overstate AGE by up to 11 months. A study SAP often prefers mid-year here -
#' the point is to have one explicit, flagged rule rather than silence.
#'
#' @param dtc ISO 8601 character vector (full, reduced precision, or NA)
#' @return list(date = Date vector, flag = c("", "D", "M", NA))
impute_dtc <- function(dtc) {
  full <- str_length(dtc) >= 10
  mth  <- str_length(dtc) >= 7
  y    <- str_sub(dtc, 1, 4)
  m    <- if_else(mth, str_sub(dtc, 6, 7), "01")
  d    <- if_else(full, str_sub(dtc, 9, 10), "01")
  list(
    date = as.Date(if_else(is.na(dtc), NA_character_, str_c(y, m, d, sep = "-"))),
    flag = case_when(
      is.na(dtc) ~ NA_character_,
      full       ~ "",
      mth        ~ "D",
      .default   = "M"
    )
  )
}

#' Age in whole years between two Dates (NA when either is missing)
#'
#' Takes Dates, not --DTC strings, so an analysis dataset can feed it an
#' imputed analysis date from impute_dtc(); SDTM callers wrap the inputs in
#' dtc_date() themselves.
compute_age <- function(brthdt, refdt) {
  age <- as.integer(floor(as.numeric(difftime(refdt, brthdt, units = "days")) / 365.25))
  if_else(is.na(brthdt) | is.na(refdt), NA_integer_, age)
}

#' Study day relative to a reference date, with no day 0 (Date inputs)
#'
#' Analysis datasets call this directly with imputed Dates; derive_dy() wraps
#' it for --DTC strings.
derive_dy_d <- function(dt, refdt) {
  diff <- as.integer(dt - refdt)
  case_when(
    is.na(diff) ~ NA_integer_,
    diff >= 0   ~ diff + 1L,
    .default    = diff
  )
}

#' Study day relative to a reference start date, with no day 0
derive_dy <- function(dtc, rfstdtc) {
  derive_dy_d(dtc_date(dtc), dtc_date(rfstdtc))
}

#' Derive a --SEQ within USUBJID
#'
#' @param data Domain data frame containing USUBJID
#' @param seq_var Name of the sequence variable to create, e.g. "AESEQ"
#' @param ... Ordering expressions passed to arrange()
derive_seq <- function(data, seq_var, ...) {
  data |>
    arrange(USUBJID, ...) |>
    mutate("{seq_var}" := row_number(), .by = USUBJID)
}

#' Convert an original result to standard units
#'
#' Only used as a fallback: if Rave already populated the _STD column for a
#' field with a standard unit configured, that value wins.
to_standard <- function(value, unit) {
  v <- suppressWarnings(as.numeric(value))
  u <- str_to_upper(str_trim(unit))
  case_when(
    is.na(v)     ~ NA_real_,
    u == "F"     ~ round((v - 32) * 5 / 9, 1),
    u == "LB"    ~ round(v * 0.45359237, 1),
    u == "IN"    ~ round(v * 2.54, 1),
    .default     = v
  )
}

standard_unit <- function(unit) {
  u <- str_to_upper(str_trim(unit))
  case_when(
    u == "F"  ~ "C",
    u == "LB" ~ "kg",
    u == "IN" ~ "cm",
    .default  = unit
  )
}

#' Coalesce a Rave standard value with a locally derived one
resolve_standard <- function(std_value, std_unit, orres, orresu) {
  std_num <- suppressWarnings(as.numeric(std_value))
  list(
    stresn = if_else(is.na(std_num), to_standard(orres, orresu), std_num),
    stresu = if_else(is.na(std_unit) | std_unit == "",
                     standard_unit(orresu), std_unit)
  )
}

#' Safe min/max over ISO 8601 character dates (NA when nothing to compare)
min_dtc <- function(x) if (all(is.na(x))) NA_character_ else min(x, na.rm = TRUE)
max_dtc <- function(x) if (all(is.na(x))) NA_character_ else max(x, na.rm = TRUE)

#' Map Rave "1"/"0" coded yes/no to SDTM Y/N
yn <- function(x) {
  recode_values(as.character(x), "1" ~ "Y", "0" ~ "N", default = NA_character_)
}

#' Trim and upper-case a verbatim term without collapsing internal spacing
clean_verbatim <- function(x) {
  x |> str_squish() |> str_to_upper()
}

#' Attach variable labels from a named character vector
apply_labels <- function(data, labels) {
  set_variable_labels(data, .labels = as.list(labels), .strict = FALSE)
}

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
#' rows - one per domain - sharing a RELID. The generator keys the pairs
#' (here: CMSPID on the CM record pointing at the AE log line); by the time
#' they reach this helper the IDVAR/IDVARVAL pairs are the domains' --SEQ
#' keys, which is what RELREC stores.
#'
#' @param links A data frame with columns `USUBJID`, `RDOMAIN_1`, `IDVAR_1`,
#'   `IDVARVAL_1`, `RDOMAIN_2`, `IDVAR_2`, `IDVARVAL_2` - one row per link.
make_relrec <- function(links) {
  req <- c("USUBJID", "RDOMAIN_1", "IDVAR_1", "IDVARVAL_1",
           "RDOMAIN_2", "IDVAR_2", "IDVARVAL_2")
  missing <- setdiff(req, names(links))
  if (length(missing) > 0) {
    stop(sprintf("make_relrec: links missing %s",
                 str_flatten_comma(missing)), call. = FALSE)
  }

  rels <- map(seq_len(nrow(links)), \(i) {
    l   <- links[i, ]
    relid <- sprintf("%s-RL-%04d", STUDYID, i)
    bind_rows(
      tibble(STUDYID = STUDYID, RDOMAIN = l$RDOMAIN_1, USUBJID = l$USUBJID,
             IDVAR = l$IDVAR_1, IDVARVAL = as.character(l$IDVARVAL_1),
             RELTYPE = "ONE", RELID = relid),
      tibble(STUDYID = STUDYID, RDOMAIN = l$RDOMAIN_2, USUBJID = l$USUBJID,
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

#' Write an SDTM domain as RDS and SAS v5 transport
write_sdtm <- function(data, domain, rds_dir = paths$sdtm,
                       xpt_dir = paths$xpt) {
  long_names <- names(data)[str_length(names(data)) > 8]
  if (length(long_names) > 0) {
    stop(sprintf("%s: variable names longer than 8 characters: %s",
                 domain, str_flatten_comma(long_names)), call. = FALSE)
  }
  long_labels <- keep(var_label(data), \(l) !is.null(l) && str_length(l) > 40)
  if (length(long_labels) > 0) {
    warning(sprintf("%s: labels longer than 40 characters: %s", domain,
                    str_flatten_comma(names(long_labels))))
  }

  write_rds(data, file.path(rds_dir, paste0(str_to_lower(domain), ".rds")))
  write_xpt(data, file.path(xpt_dir, paste0(str_to_lower(domain), ".xpt")),
            version = 5, name = str_to_lower(domain))
  invisible(data)
}

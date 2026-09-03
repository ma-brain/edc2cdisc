# ============================================================================
# Title:   Date / partial-date helpers
# Purpose: ISO 8601 --DTC assembly, explicit imputation, safe min/max
# ============================================================================

#' Build an ISO 8601 --DTC from EDC date component columns
#'
#' The source system splits every date field into integer year/month/day
#' components which are blank when the site entered UNK. That maps directly
#' onto ISO 8601 reduced precision, so no imputation is needed or wanted here.
#'
#' @param yyyy,mm,dd Character or numeric component vectors
#' @param time Optional HH:MM character vector, only used on complete dates
#' @return An ISO 8601 date character vector, reduced precision where
#'   components are missing.
#' @export
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
#'
#' @param dtc ISO 8601 character vector
#' @return A Date vector; NA wherever `dtc` has less than full precision.
#' @export
dtc_date <- function(dtc) {
  as.Date(if_else(str_length(dtc) >= 10, str_sub(dtc, 1, 10), NA_character_))
}

#' Impute a partial ISO 8601 date to a full Date, with an imputation flag
#'
#' The analysis-layer counterpart to [rave_dtc()]: SDTM keeps reduced
#' precision, ADaM resolves it. A missing month and/or day is imputed to 01
#' (the "first" rule, also admiral's default) and the flag records what
#' happened: `""` complete, `"D"` day imputed, `"M"` month (and day) imputed.
#'
#' Note the bias this rule carries: imputing a birth date to January 1st can
#' overstate AGE by up to 11 months. A study SAP often prefers mid-year here -
#' the point is to have one explicit, flagged rule rather than silence.
#'
#' @param dtc ISO 8601 character vector (full, reduced precision, or NA)
#' @return A list with `date` (Date vector) and `flag` (`c("", "D", "M", NA)`).
#' @export
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

#' Safe min over ISO 8601 character dates (NA when nothing to compare)
#'
#' @param x ISO 8601 character vector
#' @export
min_dtc <- function(x) if (all(is.na(x))) NA_character_ else min(x, na.rm = TRUE)

#' Safe max over ISO 8601 character dates (NA when nothing to compare)
#'
#' @param x ISO 8601 character vector
#' @export
max_dtc <- function(x) if (all(is.na(x))) NA_character_ else max(x, na.rm = TRUE)

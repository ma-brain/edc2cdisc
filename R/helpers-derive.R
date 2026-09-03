# ============================================================================
# Title:   Derivation helpers
# Purpose: Age, study day, sequence numbers, controlled-terminology lookup
# ============================================================================

#' Age in whole years between two Dates (NA when either is missing)
#'
#' Takes Dates, not --DTC strings, so an analysis dataset can feed it an
#' imputed analysis date from [impute_dtc()]; SDTM callers wrap the inputs in
#' [dtc_date()] themselves.
#'
#' @param brthdt,refdt Date vectors
#' @return Integer age in whole years.
#' @export
#' @examples
#' compute_age(as.Date("1990-06-15"), as.Date("2024-06-15"))
compute_age <- function(brthdt, refdt) {
  age <- as.integer(floor(as.numeric(difftime(refdt, brthdt, units = "days")) / 365.25))
  if_else(is.na(brthdt) | is.na(refdt), NA_integer_, age)
}

#' Study day relative to a reference date, with no day 0 (Date inputs)
#'
#' Analysis datasets call this directly with imputed Dates; [derive_dy()]
#' wraps it for --DTC strings.
#'
#' @param dt,refdt Date vectors
#' @return Integer study day (no day 0: the reference date itself is day 1).
#' @export
#' @examples
#' ref <- as.Date("2024-01-10")
#' derive_dy_d(as.Date(c("2024-01-09", "2024-01-10", "2024-01-11")), ref)
derive_dy_d <- function(dt, refdt) {
  diff <- as.integer(dt - refdt)
  case_when(
    is.na(diff) ~ NA_integer_,
    diff >= 0   ~ diff + 1L,
    .default    = diff
  )
}

#' Study day relative to a reference start date, with no day 0
#'
#' @param dtc,rfstdtc ISO 8601 character vectors
#' @return Integer study day.
#' @export
#' @examples
#' derive_dy("2024-01-10", "2024-01-10")   # the reference date is day 1
#' derive_dy("2024-01-09", "2024-01-10")   # no day 0
derive_dy <- function(dtc, rfstdtc) {
  derive_dy_d(dtc_date(dtc), dtc_date(rfstdtc))
}

#' Derive a --SEQ within USUBJID
#'
#' @param data Domain data frame containing USUBJID
#' @param seq_var Name of the sequence variable to create, e.g. "AESEQ"
#' @param ... Ordering expressions passed to [dplyr::arrange()]
#' @return `data` with the sequence variable added (1..n within USUBJID).
#' @export
#' @examples
#' df <- data.frame(
#'   USUBJID = c("S1", "S1", "S2"),
#'   AESTDTC = c("2024-01-05", "2024-01-02", "2024-02-01")
#' )
#' derive_seq(df, "AESEQ", AESTDTC)
derive_seq <- function(data, seq_var, ...) {
  data |>
    arrange(USUBJID, ...) |>
    mutate("{seq_var}" := row_number(), .by = USUBJID)
}

#' Map collected decodes through a controlled-terminology lookup, loudly
#'
#' @param x Character vector of collected (decoded) values
#' @param lookup A named character vector: collected value -> CDISC term
#' @param varname Variable name used in the error message
#' @return The mapped CDISC terms. Stops if any collected value has no entry:
#'   a new codelist value must be a deliberate mapping change, never a
#'   silent pass-through.
#' @export
#' @examples
#' lookup <- c("Mild" = "MILD", "Moderate" = "MODERATE")
#' check_ct(c("Mild", "Moderate"), lookup, "AESEV")
#' tryCatch(check_ct("Severe", lookup, "AESEV"), error = conditionMessage)
check_ct <- function(x, lookup, varname) {
  unmapped <- setdiff(unique(na.omit(x)), names(lookup))
  if (length(unmapped) > 0) {
    stop(sprintf("%s: unmapped codelist value(s): %s", varname,
                 str_flatten_comma(unmapped)), call. = FALSE)
  }
  unname(lookup[x])
}

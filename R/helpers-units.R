# ============================================================================
# Title:   Unit-conversion helpers
# Purpose: Conventional -> standard unit conversion for findings domains
# ============================================================================

#' Convert an original result to standard units
#'
#' Only used as a fallback: if the EDC system already populated the _STD
#' column for a field with a standard unit configured, that value wins.
#'
#' @param value Numeric (or numeric-string) result in original units
#' @param unit Original unit
#' @return Numeric result converted to the standard unit (identity when the
#'   unit is already standard).
#' @export
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

#' Standard unit for a collected unit
#'
#' @param unit Collected unit
#' @return The standard unit string (identity when already standard).
#' @export
standard_unit <- function(unit) {
  u <- str_to_upper(str_trim(unit))
  case_when(
    u == "F"  ~ "C",
    u == "LB" ~ "kg",
    u == "IN" ~ "cm",
    .default  = unit
  )
}

#' Coalesce an EDC-provided standard value with a locally derived one
#'
#' @param std_value,std_unit EDC-configured converted value/unit (may be blank)
#' @param orres,orresu Original result/unit, used for the local fallback
#' @return A list with `stresn` (numeric) and `stresu` (character).
#' @export
resolve_standard <- function(std_value, std_unit, orres, orresu) {
  std_num <- suppressWarnings(as.numeric(std_value))
  list(
    stresn = if_else(is.na(std_num), to_standard(orres, orresu), std_num),
    stresu = if_else(is.na(std_unit) | std_unit == "",
                     standard_unit(orresu), std_unit)
  )
}

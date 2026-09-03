# ============================================================================
# Title:   Formatting helpers
# Purpose: Yes/no recode, verbatim cleaning, variable labels
# ============================================================================

#' Map coded "1"/"0" yes/no values to SDTM Y/N
#'
#' @param x Character or factor vector of "1"/"0" coded values
#' @return "Y"/"N" character vector, NA where the input was not coded.
#' @export
yn <- function(x) {
  recode_values(as.character(x), "1" ~ "Y", "0" ~ "N", default = NA_character_)
}

#' Trim and upper-case a verbatim term without collapsing internal spacing
#'
#' @param x Verbatim term character vector
#' @return Cleaned character vector.
#' @export
clean_verbatim <- function(x) {
  x |> str_squish() |> str_to_upper()
}

#' Attach variable labels from a named character vector
#'
#' @param data A data frame
#' @param labels Named character vector: variable name -> label. Variables
#'   not present in `data` are ignored.
#' @return `data` with variable labels set.
#' @export
apply_labels <- function(data, labels) {
  set_variable_labels(data, .labels = as.list(labels), .strict = FALSE)
}

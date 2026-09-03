#' edc2cdisc: convert EDC clinical view exports to CDISC SDTM and ADaM
#'
#' Reads EDC clinical view CSV extracts and maps them to CDISC SDTM domains,
#' ADaM analysis datasets and a define.xml stub, driven by an explicit study
#' specification (see [spec_synth01] for the reference implementation). A
#' seeded synthetic extract generator ([generate_rave_extract()]) ships with
#' the package so the whole pipeline runs without a live EDC system.
#'
#' @keywords internal
"_PACKAGE"

#' @rawNamespace importFrom(rlang, ":=", .data)
#' @rawNamespace importFrom(dplyr, across, all_of, anti_join, any_of,
#'   arrange, bind_rows, case_when, coalesce, count, distinct, filter, first,
#'   full_join, if_any, if_else, inner_join, left_join, mutate, na_if, pull,
#'   recode_values, rename, row_number, select, slice_max, summarise,
#'   transmute)
#' @importFrom tidyr pivot_wider unite
#' @importFrom stringr str_c str_detect str_ends str_flatten_comma
#'   str_length str_pad str_remove str_squish str_sub str_to_lower
#'   str_to_sentence str_to_title str_to_upper str_trim
#' @importFrom purrr imap keep list_rbind map map_dfr pmap set_names
#' @importFrom tibble as_tibble_row deframe tibble tribble
#' @importFrom labelled set_variable_labels var_label
#' @importFrom haven write_xpt
#' @importFrom readr col_character cols read_csv read_lines write_csv
#'   write_rds
#' @importFrom stats na.omit rnorm runif
#' @importFrom utils modifyList
NULL

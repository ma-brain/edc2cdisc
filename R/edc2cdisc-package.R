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

#' @import dplyr tidyr stringr purrr tibble labelled haven readr
#' @importFrom stats na.omit
NULL

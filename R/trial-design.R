# ============================================================================
# Title:   Trial design domains - TA (Trial Arms) and TE (Trial Elements)
# Purpose: Trial design is protocol fact, not collected data: it never
#          touches the mapping engine. The builders reshape the spec's
#          `elements` / `ta` tables; validate_sdtm() recomputes what it can
#          instead of trusting them. TV/TI/TS live further down this file.
# ============================================================================

#' Map the Trial Elements domain
#'
#' One record per trial element, straight from `spec$elements`.
#'
#' @param spec A `study_spec` (see [new_study_spec()])
#' @return The labelled SDTM TE tibble.
#' @examples
#' te <- map_te(spec_synth01)
#' te[, c("ETCD", "ELEMENT", "TEDUR")]
#' @export
map_te <- function(spec) {
  spec$elements |>
    mutate(STUDYID = spec$study$STUDYID, DOMAIN = "TE") |>
    transmute(STUDYID, DOMAIN, ETCD, ELEMENT, TESTRL, TEENRL, TEDUR) |>
    apply_labels(c(
      STUDYID = "Study Identifier",
      DOMAIN  = "Domain Abbreviation",
      ETCD    = "Element Code",
      ELEMENT = "Description of Element",
      TESTRL  = "Rule for Start of Element",
      TEENRL  = "Rule for End of Element",
      TEDUR   = "Planned Duration of Element"
    ))
}

#' Map the Trial Arms domain
#'
#' One record per arm per element in planned order. ARMCD and EPOCH come
#' from `spec$ta`; ARM is joined from `spec$arms` and ELEMENT from
#' `spec$elements`, so neither is repeated in a spec table that could
#' drift.
#'
#' @param spec A `study_spec` (see [new_study_spec()])
#' @return The labelled SDTM TA tibble.
#' @examples
#' ta <- map_ta(spec_synth01)
#' ta[, c("ARMCD", "TAETORD", "ETCD", "EPOCH")]
#' @export
map_ta <- function(spec) {
  spec$ta |>
    left_join(spec$arms |> select(ARMCD, ARM),
              by = "ARMCD") |>
    left_join(spec$elements |> select(ETCD, ELEMENT), by = "ETCD") |>
    mutate(STUDYID = spec$study$STUDYID, DOMAIN = "TA") |>
    arrange(ARMCD, TAETORD) |>
    transmute(STUDYID, DOMAIN, ARMCD, ARM, TAETORD, ETCD, ELEMENT,
              TABRANCH, TATRANS, EPOCH) |>
    apply_labels(c(
      STUDYID   = "Study Identifier",
      DOMAIN    = "Domain Abbreviation",
      ARMCD     = "Planned Arm Code",
      ARM       = "Description of Planned Arm",
      TAETORD   = "Planned Order of Element within Arm",
      ETCD      = "Element Code",
      ELEMENT   = "Description of Element",
      TABRANCH  = "Branch",
      TATRANS   = "Transition Rule",
      EPOCH     = "Epoch"
    ))
}

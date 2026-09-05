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
    left_join(select(spec$arms, ARMCD, ARM), by = "ARMCD") |>
    left_join(select(spec$elements, ETCD, ELEMENT), by = "ETCD") |>
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

#' Map the Trial Inclusion/Exclusion Criteria domain
#'
#' One record per criterion, straight from `spec$ie`.
#'
#' @param spec A `study_spec` (see [new_study_spec()])
#' @return The labelled SDTM TI tibble.
#' @examples
#' ti <- map_ti(spec_synth01)
#' ti[, c("IETESTCD", "IECAT")]
#' @export
map_ti <- function(spec) {
  spec$ie |>
    mutate(STUDYID = spec$study$STUDYID, DOMAIN = "TI") |>
    transmute(STUDYID, DOMAIN, IETESTCD, IETEST, IECAT) |>
    apply_labels(c(
      STUDYID   = "Study Identifier",
      DOMAIN    = "Domain Abbreviation",
      IETESTCD  = "Incl/Excl Criterion Short Name",
      IETEST    = "Incl/Excl Criterion Test",
      IECAT     = "Incl/Excl Criterion Category"
    ))
}

#' Map the Trial Summary domain
#'
#' One record per trial summary parameter, straight from `spec$ts`. The
#' value lives in TSVAL and TSVALNF is the null flavor when no value can
#' be provided - exactly one of the two is filled per row (checked at
#' construction). NARMS and PLANSUB are cross-checked against `spec$arms`
#' and `spec$study$n` by `validate_sdtm()`, not recomputed here: the
#' output is the spec, and the validator catches a spec that disagrees
#' with itself.
#'
#' @param spec A `study_spec` (see [new_study_spec()])
#' @return The labelled SDTM TS tibble.
#' @examples
#' ts <- map_ts(spec_synth01)
#' ts[, c("TSPARMCD", "TSVAL")]
#' @export
map_ts <- function(spec) {
  spec$ts |>
    mutate(STUDYID = spec$study$STUDYID, DOMAIN = "TS") |>
    transmute(STUDYID, DOMAIN, TSPARMCD, TSPARM, TSVAL, TSVALNF) |>
    apply_labels(c(
      STUDYID  = "Study Identifier",
      DOMAIN   = "Domain Abbreviation",
      TSPARMCD = "Trial Summary Parameter Short Name",
      TSPARM   = "Trial Summary Parameter",
      TSVAL    = "Parameter Value",
      TSVALNF  = "Null Flavor"
    ))
}

#' Map the Trial Visits domain
#'
#' One record per planned visit. TV has no spec table of its own: it is a
#' transform of the `visits` table the scheduled-visit domains already
#' read - VISITNUM, VISIT and EPOCH carry over, and VISITDY is TargetDays
#' under the same no-day-0 rule `map_sv()` applies to planned days.
#'
#' @param spec A `study_spec` (see [new_study_spec()])
#' @return The labelled SDTM TV tibble.
#' @examples
#' tv <- map_tv(spec_synth01)
#' tv[, c("VISITNUM", "VISIT", "VISITDY", "EPOCH")]
#' @export
map_tv <- function(spec) {
  spec$visits |>
    mutate(
      STUDYID = spec$study$STUDYID,
      DOMAIN  = "TV",
      # Planned study day: same no-day-0 rule as derive_dy()/map_sv()
      VISITDY = if_else(TargetDays >= 0L, TargetDays + 1L, TargetDays)
    ) |>
    arrange(VISITNUM) |>
    transmute(STUDYID, DOMAIN, VISITNUM, VISIT, VISITDY, EPOCH) |>
    apply_labels(c(
      STUDYID  = "Study Identifier",
      DOMAIN   = "Domain Abbreviation",
      VISITNUM = "Visit Number",
      VISIT    = "Visit Name",
      VISITDY  = "Planned Study Day of Visit",
      EPOCH    = "Epoch"
    ))
}

# ============================================================================
# Title:   Shared ADaM derivation rules
# Purpose: One implementation per derivation rule, called by both the
#          deriver (derive_adsl/adae/advs/adlb) and validate_adam(). The
#          validator comparing the built data against these functions can
#          only detect post-hoc corruption - a wrong rule is invisible to it
#          by construction - so each rule here is pinned by hand-built
#          input tests in test-adam-rules.R.
# ============================================================================

# Treatment-emergent: onset (imputed) on or after first dose and up to the
# last dose. No grace window - no SAP defines one. A missing TRTEDT (dosing
# ongoing at the data cut, or a last EX record with no end date) leaves the
# end of the window open: `astdt <= trtedt` must not NULL the whole
# comparison out via `TRUE & NA`, which would unflag every event the
# subject has.
.rule_trtemfl <- function(astdt, trtsdt, trtedt) {
  case_when(
    is.na(astdt) | is.na(trtsdt)                        ~ "",
    astdt >= trtsdt & (is.na(trtedt) | astdt <= trtedt) ~ "Y",
    .default                                            = ""
  )
}

# Treatment duration in days, both ends inclusive.
.rule_trtdurd <- function(trtsdt, trtedt) {
  as.integer(trtedt - trtsdt) + 1L
}

# Change from baseline: the baseline row itself carries none, and rows
# without a baseline stay missing rather than zero. ABLFL is "Y" or NA, so
# the %in% form matters: NA != "Y" would be NA and the NA-armed case_when
# would nullify every post-baseline change.
.rule_chg <- function(aval, base, ablfl) {
  case_when(
    ablfl %in% "Y" ~ NA_real_,
    is.na(base)    ~ NA_real_,
    .default       = aval - base
  )
}

# Percent change from baseline, defined only where CHG is.
.rule_pchg <- function(chg, base) {
  if_else(!is.na(chg), 100 * chg / base, NA_real_)
}

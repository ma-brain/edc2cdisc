# ============================================================================
# Title:   Derivation registry
# Purpose: Named derivation functions the spec's variables table selects.
#          Each takes (data, spec) - `data` is the in-flight frame including
#          columns derived earlier in the same map_variables() pass - and
#          returns a vector. A new study reuses these; a new SEMANTIC needs
#          a new entry here. That boundary is the honest limit of
#          "a new study is a spec change" (see the design vignette).
# ============================================================================

# Shared derivations. Domain-local ones (e.g. DMDY) are appended by their
# mapper as local closures over the same signature.
.derivations <- list(

  study_id = function(data, spec) {
    rep(spec$study$STUDYID, nrow(data))
  },

  usubjid = function(data, spec) {
    str_c(spec$study$STUDYID, data$Subject, sep = "-")
  },

  # Age at first dose; screen failures (no RFSTDTC) are aged at informed
  # consent. Partial birth dates stay missing here rather than being
  # silently imputed - that rule belongs to the analysis layer (ADSL).
  age_at_first_dose = function(data, spec) {
    compute_age(dtc_date(data$BRTHDTC),
                dtc_date(coalesce(data$RFSTDTC, data$RFICDTC)))
  },

  ageu = function(data, spec) {
    if_else(is.na(data$AGE), NA_character_, "YEARS")
  },

  # ARMCD / ARM from the joined spec$arms lookup (raw ARMCD_ codes are
  # dropped by the mapper before the join); unrandomised subjects fall to
  # the screen-failure placeholder.
  armcd = function(data, spec) {
    coalesce(data$ARMCD, "SCRNFAIL")
  },

  arm = function(data, spec) {
    coalesce(data$ARM, "Screen Failure")
  },

  country = function(data, spec) {
    lookup <- set_names(spec$sites$COUNTRY, spec$sites$SiteNumber)
    unname(lookup[data$SiteNumber])
  },

  # DSDECOD: Rave decode -> CDISC disposition CT via the spec codelist,
  # then the screen-failure reclassification. This EDC build records a
  # screen failure on the protocol-deviation reason code; the verbatim
  # carries the fact, so DS reflects it.
  dsdecod = function(data, spec) {
    decod <- check_ct(str_to_upper(data$DSREAS_DECODE),
                      ct_lookup(spec, "DSDECOD"), "DSDECOD")
    if_else(str_detect(str_to_lower(data$DSTERM), "screen failure"),
            "SCREEN FAILURE", decod)
  },

  dscat = function(data, spec) {
    if_else(data$DSDECOD == "SCREEN FAILURE",
            "PROTOCOL MILESTONE", "DISPOSITION EVENT")
  },

  # Ongoing (AE) records carry no end date; the relative timing goes in
  # ENRTPT / ENRF.
  enrtpt_ongoing = function(data, spec) {
    if_else(data$AEONG == "1", "ONGOING", NA_character_)
  },

  enrf_ongoing = function(data, spec) {
    if_else(data$AEONG == "1", "AFTER", NA_character_)
  },

  enrtpt_ongoing_cm = function(data, spec) {
    if_else(data$CMONG == "1", "ONGOING", NA_character_)
  },

  enrf_ongoing_cm = function(data, spec) {
    if_else(data$CMONG == "1", "AFTER", NA_character_)
  },

  enrtpt_ongoing_mh = function(data, spec) {
    if_else(data$MHONG == "1", "ONGOING", NA_character_)
  },

  enrf_ongoing_mh = function(data, spec) {
    if_else(data$MHONG == "1", "AFTER", NA_character_)
  },

  # DM's study-day column, anchored on the joined RFSTDTC.
  dmdy = function(data, spec) {
    derive_dy(data$DMDTC, data$RFSTDTC)
  }
)

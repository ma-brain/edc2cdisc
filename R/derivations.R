# ============================================================================
# Title:   Derivation registry
# Purpose: Named derivation functions the spec's variables table selects.
#          Each takes (data, spec, row) - `data` is the in-flight frame
#          including columns derived earlier in the same map_variables()
#          pass, `row` is the spec row the derivation runs for.
#
#          The contract a registry entry must keep: a collected CRF source
#          is read through row$crf_field (or row$aux), never a literal
#          column name - a study whose ongoing flag or disposition decode
#          lives in another column is a spec change, not a code change.
#          Columns created by earlier rows of the same pass are outputs of
#          the mapping, not collected data, and are addressed by their
#          SDTM names (RFSTDTC, DSTERM, ...). A new study reuses these; a
#          new SEMANTIC needs a new entry here. That boundary is the
#          honest limit of "a new study is a spec change" (see the design
#          vignette).
# ============================================================================

.derivations_need_field <- c(
  "usubjid", "country", "dsdecod",
  "enrtpt_ongoing", "enrf_ongoing",
  "enrtpt_ongoing_cm", "enrf_ongoing_cm",
  "enrtpt_ongoing_mh", "enrf_ongoing_mh"
)

.derivations <- list(

  study_id = function(data, spec, row) {
    rep(spec$study$STUDYID, nrow(data))
  },

  usubjid = function(data, spec, row) {
    str_c(spec$study$STUDYID, data[[row$crf_field]], sep = "-")
  },

  # Age at first dose; screen failures (no RFSTDTC) are aged at informed
  # consent. BRTHDTC / RFSTDTC / RFICDTC are earlier rows' outputs, not
  # collected columns. Partial birth dates stay missing here rather than
  # being silently imputed - that rule belongs to the analysis layer (ADSL).
  age_at_first_dose = function(data, spec, row) {
    compute_age(dtc_date(data$BRTHDTC),
                dtc_date(coalesce(data$RFSTDTC, data$RFICDTC)))
  },

  ageu = function(data, spec, row) {
    if_else(is.na(data$AGE), NA_character_, "YEARS")
  },

  # ARMCD / ARM from the joined spec$arms lookup (raw ARMCD_ codes are
  # dropped by the mapper before the join); unrandomised subjects fall to
  # the screen-failure placeholder.
  armcd = function(data, spec, row) {
    coalesce(data$ARMCD, "SCRNFAIL")
  },

  arm = function(data, spec, row) {
    coalesce(data$ARM, "Screen Failure")
  },

  country = function(data, spec, row) {
    lookup <- set_names(spec$sites$COUNTRY, spec$sites$SiteNumber)
    unname(lookup[data[[row$crf_field]]])
  },

  # DSDECOD: Rave decode (the row's collected field) -> CDISC disposition
  # CT via the spec codelist, then the screen-failure reclassification
  # against DSTERM - the squished verbatim an earlier spec row produced.
  # This EDC build records a screen failure on the protocol-deviation
  # reason code; the verbatim carries the fact, so DS reflects it.
  dsdecod = function(data, spec, row) {
    decod <- check_ct(str_to_upper(data[[row$crf_field]]),
                      ct_lookup(spec, "DSDECOD"), "DSDECOD")
    if_else(str_detect(str_to_lower(data$DSTERM), "screen failure"),
            "SCREEN FAILURE", decod)
  },

  dscat = function(data, spec, row) {
    if_else(data$DSDECOD == "SCREEN FAILURE",
            "PROTOCOL MILESTONE", "DISPOSITION EVENT")
  },

  # Ongoing records carry no end date; the relative timing goes in the
  # ENRTPT and ENRF flags. The ongoing marker is the row's collected field
  # (AEONG / CMONG / MHONG - a different name is a spec edit, see the
  # *_ongoing suffix each domain uses).
  enrtpt_ongoing = function(data, spec, row) {
    if_else(data[[row$crf_field]] == "1", "ONGOING", NA_character_)
  },

  enrf_ongoing = function(data, spec, row) {
    if_else(data[[row$crf_field]] == "1", "AFTER", NA_character_)
  },

  enrtpt_ongoing_cm = function(data, spec, row) {
    if_else(data[[row$crf_field]] == "1", "ONGOING", NA_character_)
  },

  enrf_ongoing_cm = function(data, spec, row) {
    if_else(data[[row$crf_field]] == "1", "AFTER", NA_character_)
  },

  enrtpt_ongoing_mh = function(data, spec, row) {
    if_else(data[[row$crf_field]] == "1", "ONGOING", NA_character_)
  },

  enrf_ongoing_mh = function(data, spec, row) {
    if_else(data[[row$crf_field]] == "1", "AFTER", NA_character_)
  },

  # DM's study-day column, anchored on the joined RFSTDTC - both earlier
  # rows' outputs.
  dmdy = function(data, spec, row) {
    derive_dy(data$DMDTC, data$RFSTDTC)
  }
)

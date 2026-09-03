# ============================================================================
# Title:   SDTM AE - Adverse Events
# Purpose: Derive AE from the Rave AE log form
# Input:   data/rave/AE.csv, subject_ref from 10_sdtm_dm.R
# Output:  data/sdtm/ae.rds, data/sdtm/xpt/ae.xpt
# ============================================================================

ae_raw <- read_rave_form("AE")

# Rave decodes -> CDISC controlled terminology. Keeping these as explicit
# lookup tables (rather than toupper() on the decode) makes the mapping
# reviewable and fails loudly when a new codelist value appears.
ae_out_ct <- c(
  "Recovered/Resolved"            = "RECOVERED/RESOLVED",
  "Recovering/Resolving"          = "RECOVERING/RESOLVING",
  "Not recovered/Not resolved"    = "NOT RECOVERED/NOT RESOLVED",
  "Fatal"                         = "FATAL",
  "Unknown"                       = "UNKNOWN"
)

ae_acn_ct <- c(
  "Dose not changed"  = "DOSE NOT CHANGED",
  "Dose reduced"      = "DOSE REDUCED",
  "Drug interrupted"  = "DRUG INTERRUPTED",
  "Drug withdrawn"    = "DRUG WITHDRAWN"
)

ae_sev_ct <- c("Mild" = "MILD", "Moderate" = "MODERATE", "Severe" = "SEVERE")

# Sponsor convention: collapse the 4-point causality scale to related / not
ae_rel_ct <- c(
  "Not related"      = "NOT RELATED",
  "Possibly related" = "RELATED",
  "Probably related" = "RELATED",
  "Related"          = "RELATED"
)

check_ct <- function(x, lookup, varname) {
  unmapped <- setdiff(unique(na.omit(x)), names(lookup))
  if (length(unmapped) > 0) {
    stop(sprintf("%s: unmapped codelist value(s): %s", varname,
                 str_flatten_comma(unmapped)), call. = FALSE)
  }
  unname(lookup[x])
}

ae <- ae_raw |>
  transmute(
    STUDYID  = .env$STUDYID,
    DOMAIN   = "AE",
    USUBJID  = str_c(STUDYID, Subject, sep = "-"),
    # Rave log-line number. Kept as AESPID so SUPPAE / CO can pin each related
    # record to an AE row without re-deriving AESEQ
    # (see 19_sdtm_suppae.R, 21_sdtm_co.R).
    AESPID   = as.character(recordposition),
    AETERM   = clean_verbatim(AETERM),
    AEDECOD  = AECOD_PT,
    AEBODSYS = AECOD_SOC,
    AESEV    = check_ct(AESEV_DECODE, ae_sev_ct, "AESEV"),
    AESER    = yn(AESER),
    AEREL    = check_ct(AEREL_DECODE, ae_rel_ct, "AEREL"),
    AEACN    = check_ct(AEACN_DECODE, ae_acn_ct, "AEACN"),
    AEOUT    = check_ct(AEOUT_DECODE, ae_out_ct, "AEOUT"),
    AESTDTC  = rave_dtc(AESTDAT_YYYY, AESTDAT_MM, AESTDAT_DD),
    AEENDTC  = rave_dtc(AEENDAT_YYYY, AEENDAT_MM, AEENDAT_DD),
    ONGOING  = AEONG
  ) |>
  left_join(subject_ref, by = "USUBJID") |>
  mutate(
    # Ongoing events carry no end date; the relative timing goes in ENRTPT
    AEENRTPT = if_else(ONGOING == "1", "ONGOING", NA_character_),
    AEENRF   = if_else(ONGOING == "1", "AFTER", NA_character_),
    AESTDY   = derive_dy(AESTDTC, RFSTDTC),
    AEENDY   = derive_dy(AEENDTC, RFSTDTC)
    # Treatment-emergent status is deliberately NOT derived here: TRTEMFL is
    # an ADaM variable. SDTM keeps the collected facts, ADAE applies the rule.
  ) |>
  derive_seq("AESEQ", AESTDTC, AEDECOD) |>
  select(STUDYID, DOMAIN, USUBJID, AESEQ, AESPID, AETERM, AEDECOD, AEBODSYS,
         AESEV, AESER, AEREL, AEACN, AEOUT,
         AESTDTC, AEENDTC, AESTDY, AEENDY, AEENRTPT, AEENRF) |>
  arrange(USUBJID, AESEQ) |>
  apply_labels(c(
    STUDYID  = "Study Identifier",
    DOMAIN   = "Domain Abbreviation",
    USUBJID  = "Unique Subject Identifier",
    AESEQ    = "Sequence Number",
    AESPID   = "Sponsor-Defined Identifier",
    AETERM   = "Reported Term for the Adverse Event",
    AEDECOD  = "Dictionary-Derived Term",
    AEBODSYS = "Body System or Organ Class",
    AESEV    = "Severity/Intensity",
    AESER    = "Serious Event",
    AEREL    = "Causality",
    AEACN    = "Action Taken with Study Treatment",
    AEOUT    = "Outcome of Adverse Event",
    AESTDTC  = "Start Date/Time of Adverse Event",
    AEENDTC  = "End Date/Time of Adverse Event",
    AESTDY   = "Study Day of Start of Adverse Event",
    AEENDY   = "Study Day of End of Adverse Event",
    AEENRTPT = "End Relative to Reference Time Point",
    AEENRF   = "End Relative to Reference Period"
  ))

write_sdtm(ae, "AE")

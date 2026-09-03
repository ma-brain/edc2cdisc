# ============================================================================
# Title:   SDTM SUPPAE - Supplemental Qualifiers for AE
# Purpose: Carry the non-standard adverse-event CRF fields - AE of special
#          interest, and whether the event led to study discontinuation -
#          keyed to each AE record by AESEQ.
# Input:   data/rave/AE.csv, ae (built in 13_sdtm_ae.R)
# Output:  data/sdtm/suppae.rds, data/sdtm/xpt/suppae.xpt
#
# IDVAR = "AESEQ": every SUPP row points at exactly one AE record. AESEQ is a
# derived key, so the raw CRF fields are joined onto the built AE by AESPID
# (the Rave log-line number, carried through 13_sdtm_ae.R for this purpose).
# ============================================================================

ae_raw <- read_rave_form("AE")

suppae_src <- ae_raw |>
  transmute(
    STUDYID  = .env$STUDYID,
    USUBJID  = str_c(STUDYID, Subject, sep = "-"),
    AESPID   = as.character(recordposition),
    AESI     = yn(AESI),
    AEDISCON = yn(AEDISCON)
  ) |>
  inner_join(select(ae, USUBJID, AESPID, AESEQ), by = c("USUBJID", "AESPID"))

# Every active raw AE row must have matched exactly one AE record.
if (nrow(suppae_src) != nrow(ae_raw)) {
  stop(sprintf(
    "SUPPAE: %d of %d raw AE row(s) did not map to an AE record via AESPID",
    nrow(ae_raw) - nrow(suppae_src), nrow(ae_raw)
  ), call. = FALSE)
}

suppae <- make_supp(
  parent  = suppae_src,
  rdomain = "AE",
  idvar   = "AESEQ",
  # QEVAL names who assessed a subjective value. "AE of special interest" is the
  # investigator's call, so it carries QEVAL = "INVESTIGATOR"; "led to
  # discontinuation" is a matter of record and stays blank.
  qnams   = tribble(
    ~qnam,      ~qlabel,                              ~qeval,
    "AESI",     "Adverse Event of Special Interest",  "INVESTIGATOR",
    "AEDISCON", "AE Led to Study Discontinuation",    NA_character_
  )
)

write_sdtm(suppae, "SUPPAE")

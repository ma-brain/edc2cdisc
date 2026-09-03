# ============================================================================
# Title:   SDTM CO - Comments
# Purpose: Carry the free-text investigator comment collected on the AE form.
#          Free text belongs in CO, not SUPP: SUPP is for structured
#          name/value qualifiers, CO is for narrative tied to a record.
# Input:   data/rave/AE.csv, ae (built in 13_sdtm_ae.R)
# Output:  data/sdtm/co.rds, data/sdtm/xpt/co.xpt
#
# Structure parallels SUPPAE - RDOMAIN / IDVAR / IDVARVAL point back at one AE
# record - but CO has its own COSEQ and a COVAL string instead of QNAM/QVAL.
# COVAL is capped at 200 characters; a longer comment would spill into
# COVAL1, COVAL2, ... Here every comment fits, so a single COVAL is enough.
# ============================================================================

ae_raw <- read_rave_form("AE")

co <- ae_raw |>
  transmute(
    STUDYID = .env$STUDYID,
    USUBJID = str_c(STUDYID, Subject, sep = "-"),
    AESPID  = as.character(recordposition),
    COVAL   = na_if(str_squish(AECOMNT), "")
  ) |>
  filter(!is.na(COVAL)) |>
  inner_join(select(ae, USUBJID, AESPID, AESEQ), by = c("USUBJID", "AESPID")) |>
  mutate(
    DOMAIN   = "CO",
    RDOMAIN  = "AE",
    IDVAR    = "AESEQ",
    IDVARVAL = as.character(AESEQ)
  ) |>
  derive_seq("COSEQ", as.integer(IDVARVAL)) |>
  select(STUDYID, DOMAIN, RDOMAIN, USUBJID, COSEQ, IDVAR, IDVARVAL, COVAL) |>
  arrange(USUBJID, COSEQ) |>
  apply_labels(c(
    STUDYID  = "Study Identifier",
    DOMAIN   = "Domain Abbreviation",
    RDOMAIN  = "Related Domain Abbreviation",
    USUBJID  = "Unique Subject Identifier",
    COSEQ    = "Sequence Number",
    IDVAR    = "Identifying Variable",
    IDVARVAL = "Identifying Variable Value",
    COVAL    = "Comment"
  ))

write_sdtm(co, "CO")

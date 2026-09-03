# ============================================================================
# Title:   SDTM SUPPDM - Supplemental Qualifiers for DM
# Purpose: Carry the non-standard demographics CRF fields that have no home in
#          DM - subject initials, childbearing potential, the "Other, specify"
#          race text - as name/value pairs tied back to each DM record.
# Input:   data/rave/DM.csv
# Output:  data/sdtm/suppdm.rds, data/sdtm/xpt/suppdm.xpt
#
# DM is one record per subject, so IDVAR / IDVARVAL are left blank and the
# qualifier is linked to its parent by USUBJID alone. Contrast
# 19_sdtm_suppae.R, where IDVAR = "AESEQ" pins each value to one AE record.
# ============================================================================

dm_raw <- read_rave_form("DM")

# One column per QNAM. A blank value yields no SUPP row (make_supp drops it):
# SUBJINIT is collected for everyone, CHILDPOT for women only, RACEOTH only
# when RACE is OTHER.
suppdm_src <- dm_raw |>
  transmute(
    STUDYID  = .env$STUDYID,
    USUBJID  = str_c(STUDYID, Subject, sep = "-"),
    SUBJINIT = na_if(str_squish(SUBJINIT), ""),
    CHILDPOT = yn(CHILDPOT),
    RACEOTH  = na_if(clean_verbatim(RACEOTH), "")
  )

suppdm <- make_supp(
  parent  = suppdm_src,
  rdomain = "DM",
  idvar   = NA,
  qnams   = tribble(
    ~qnam,      ~qlabel,
    "SUBJINIT", "Subject Initials",
    "CHILDPOT", "Childbearing Potential",
    "RACEOTH",  "Race, Other Specify"
  )
)

write_sdtm(suppdm, "SUPPDM")

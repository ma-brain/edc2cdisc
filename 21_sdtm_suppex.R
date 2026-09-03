# ============================================================================
# Title:   SDTM SUPPEX - Supplemental Qualifiers for EX
# Purpose: Carry the non-standard exposure CRF field - who administered the
#          dose - keyed to each EX record by EXSEQ.
# Input:   data/rave/EX.csv, ex (built in 11_sdtm_ex.R), visit_map
# Output:  data/sdtm/suppex.rds, data/sdtm/xpt/suppex.xpt
#
# A third SUPP parent, to show the same make_supp() call against a different
# domain and IDVAR. EX has one record per dosing interval per visit, so the
# raw field is joined onto the built EX by (USUBJID, VISITNUM) to recover the
# derived EXSEQ - the EX equivalent of the AESPID trick in 19_sdtm_suppae.R.
# ============================================================================

ex_raw <- read_rave_form("EX")

suppex_src <- ex_raw |>
  filter(EXOCCUR == "1") |>
  left_join(visit_map, by = "Folder") |>
  transmute(
    STUDYID = .env$STUDYID,
    USUBJID = str_c(STUDYID, Subject, sep = "-"),
    VISITNUM,
    EXADMBY = na_if(str_squish(EXADMBY), "")
  ) |>
  inner_join(select(ex, USUBJID, VISITNUM, EXSEQ), by = c("USUBJID", "VISITNUM"))

n_occur <- sum(ex_raw$EXOCCUR == "1")
if (nrow(suppex_src) != n_occur) {
  stop(sprintf(
    "SUPPEX: %d of %d dosing record(s) did not map to an EX record",
    n_occur - nrow(suppex_src), n_occur
  ), call. = FALSE)
}

suppex <- make_supp(
  parent  = suppex_src,
  rdomain = "EX",
  idvar   = "EXSEQ",
  qnams   = tribble(
    ~qnam,     ~qlabel,
    "EXADMBY", "Dose Administered By"
  )
)

write_sdtm(suppex, "SUPPEX")

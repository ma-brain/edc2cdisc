# ============================================================================
# Title:   SDTM DS - Disposition
# Purpose: Derive DS from the Rave End of Study form
# Input:   data/rave/DS.csv, subject_ref from 10_sdtm_dm.R
# Output:  data/sdtm/ds.rds, data/sdtm/xpt/ds.xpt
# ============================================================================

ds_raw <- read_rave_form("DS")

# Rave decode -> CDISC NCOMPLT / disposition CT. Explicit lookup so a new
# reason value breaks the run rather than passing through silently.
ds_decod_ct <- c(
  "COMPLETED"             = "COMPLETED",
  "ADVERSE EVENT"         = "ADVERSE EVENT",
  "WITHDRAWAL BY SUBJECT" = "WITHDRAWAL BY SUBJECT",
  "LOST TO FOLLOW-UP"     = "LOST TO FOLLOW-UP",
  "PROTOCOL DEVIATION"    = "PROTOCOL DEVIATION",
  "DEATH"                 = "DEATH"
)

check_ct <- function(x, lookup, varname) {
  unmapped <- setdiff(unique(na.omit(x)), names(lookup))
  if (length(unmapped) > 0) {
    stop(sprintf("%s: unmapped codelist value(s): %s", varname,
                 str_flatten_comma(unmapped)), call. = FALSE)
  }
  unname(lookup[x])
}

ds <- ds_raw |>
  transmute(
    STUDYID = .env$STUDYID,
    DOMAIN  = "DS",
    USUBJID = str_c(STUDYID, Subject, sep = "-"),
    DSTERM  = str_squish(DSTERM),
    DSDECOD = check_ct(str_to_upper(DSREAS_DECODE), ds_decod_ct, "DSDECOD"),
    DSSTDTC = rave_dtc(DSSTDAT_YYYY, DSSTDAT_MM, DSSTDAT_DD)
  ) |>
  mutate(
    # This EDC build records a screen failure on the protocol-deviation
    # reason code. Reclassify it from the verbatim so DS carries the fact.
    DSDECOD = if_else(str_detect(str_to_lower(DSTERM), "screen failure"),
                      "SCREEN FAILURE", DSDECOD),
    DSCAT   = if_else(DSDECOD == "SCREEN FAILURE",
                      "PROTOCOL MILESTONE", "DISPOSITION EVENT")
  ) |>
  left_join(subject_ref, by = "USUBJID") |>
  mutate(DSSTDY = derive_dy(DSSTDTC, RFSTDTC)) |>
  derive_seq("DSSEQ", DSSTDTC, DSDECOD) |>
  select(STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT,
         DSSTDTC, DSSTDY) |>
  arrange(USUBJID, DSSEQ) |>
  apply_labels(c(
    STUDYID = "Study Identifier",
    DOMAIN  = "Domain Abbreviation",
    USUBJID = "Unique Subject Identifier",
    DSSEQ   = "Sequence Number",
    DSTERM  = "Reported Term for the Disposition Event",
    DSDECOD = "Standardized Disposition Term",
    DSCAT   = "Category for Disposition Event",
    DSSTDTC = "Start Date/Time of Disposition Event",
    DSSTDY  = "Study Day of Start of Disposition Event"
  ))

write_sdtm(ds, "DS")

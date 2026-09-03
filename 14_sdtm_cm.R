# ============================================================================
# Title:   SDTM CM - Concomitant Medications
# Purpose: Derive CM from the Rave CM log form
# Input:   data/rave/CM.csv, subject_ref from 10_sdtm_dm.R
# Output:  data/sdtm/cm.rds, data/sdtm/xpt/cm.xpt
# ============================================================================

cm_raw <- read_rave_form("CM")

cm <- cm_raw |>
  transmute(
    STUDYID  = .env$STUDYID,
    DOMAIN   = "CM",
    USUBJID  = str_c(STUDYID, Subject, sep = "-"),
    # Rave log-line number. Kept as CMSPID so RELREC can pin each linked
    # record to a CM row without re-deriving CMSEQ (see 23_sdtm_relrec.R).
    CMSPID   = as.character(CMSPID),
    CMTRT    = clean_verbatim(CMTRT),
    CMDECOD  = CMCOD,
    CMINDC   = str_squish(CMINDC),
    CMDOSE   = suppressWarnings(as.numeric(CMDOSE)),
    CMDOSU   = CMDOSU,
    # Rave frequency / route decodes are already CDISC CT abbreviations.
    CMDOSFRQ = str_to_upper(CMFRQ_DECODE),
    CMROUTE  = str_to_upper(CMROUTE_DECODE),
    CMSTDTC  = rave_dtc(CMSTDAT_YYYY, CMSTDAT_MM, CMSTDAT_DD),
    CMENDTC  = rave_dtc(CMENDAT_YYYY, CMENDAT_MM, CMENDAT_DD),
    ONGOING  = CMONG
  ) |>
  left_join(subject_ref, by = "USUBJID") |>
  mutate(
    # Ongoing medications carry no end date; relative timing goes in ENRTPT.
    CMENRTPT = if_else(ONGOING == "1", "ONGOING", NA_character_),
    CMENRF   = if_else(ONGOING == "1", "AFTER", NA_character_),
    CMSTDY   = derive_dy(CMSTDTC, RFSTDTC),
    CMENDY   = derive_dy(CMENDTC, RFSTDTC)
  ) |>
  derive_seq("CMSEQ", CMSTDTC, CMDECOD) |>
  select(STUDYID, DOMAIN, USUBJID, CMSEQ, CMSPID, CMTRT, CMDECOD, CMINDC,
         CMDOSE, CMDOSU, CMDOSFRQ, CMROUTE,
         CMSTDTC, CMENDTC, CMSTDY, CMENDY, CMENRTPT, CMENRF) |>
  arrange(USUBJID, CMSEQ) |>
  apply_labels(c(
    STUDYID  = "Study Identifier",
    DOMAIN   = "Domain Abbreviation",
    USUBJID  = "Unique Subject Identifier",
    CMSEQ    = "Sequence Number",
    CMSPID   = "Sponsor-Defined Identifier",
    CMTRT    = "Reported Name of Drug, Med, or Therapy",
    CMDECOD  = "Standardized Medication Name",
    CMINDC   = "Indication",
    CMDOSE   = "Dose per Administration",
    CMDOSU   = "Dose Units",
    CMDOSFRQ = "Dosing Frequency per Interval",
    CMROUTE  = "Route of Administration",
    CMSTDTC  = "Start Date/Time of Medication",
    CMENDTC  = "End Date/Time of Medication",
    CMSTDY   = "Study Day of Start of Medication",
    CMENDY   = "Study Day of End of Medication",
    CMENRTPT = "End Relative to Reference Time Point",
    CMENRF   = "End Relative to Reference Period"
  ))

write_sdtm(cm, "CM")

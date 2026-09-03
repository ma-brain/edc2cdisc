# ============================================================================
# Title:   SDTM MH - Medical History
# Purpose: Derive MH from the Rave MH log form - chronic conditions recorded
#          at screening, with partly-known start dates the way real history
#          is reported.
# Input:   data/rave/MH.csv, subject_ref from 10_sdtm_dm.R
# Output:  data/sdtm/mh.rds, data/sdtm/xpt/mh.xpt
# Note:    History starts before the study, so MHSTDY is legitimately
#          negative - the screen-failure "no study days" rule still holds
#          because those subjects have no RFSTDTC to anchor on at all.
# ============================================================================

mh_raw <- read_rave_form("MH")

mh <- mh_raw |>
  transmute(
    STUDYID  = .env$STUDYID,
    DOMAIN   = "MH",
    USUBJID  = str_c(STUDYID, Subject, sep = "-"),
    MHTERM   = str_squish(MHTERM),
    MHDECOD  = MHCOD_PT,
    MHBODSYS = MHCOD_SOC,
    MHSTDTC  = rave_dtc(MHSTDAT_YYYY, MHSTDAT_MM, MHSTDAT_DD),
    MHENDTC  = rave_dtc(MHENDAT_YYYY, MHENDAT_MM, MHENDAT_DD),
    ONGOING  = MHONG
  ) |>
  left_join(subject_ref, by = "USUBJID") |>
  mutate(
    # Ongoing conditions carry no end date; relative timing goes in ENRTPT
    MHENRTPT = if_else(ONGOING == "1", "ONGOING", NA_character_),
    MHENRF   = if_else(ONGOING == "1", "AFTER", NA_character_),
    MHSTDY   = derive_dy(MHSTDTC, RFSTDTC),
    MHENDY   = derive_dy(MHENDTC, RFSTDTC)
  ) |>
  derive_seq("MHSEQ", MHSTDTC, MHTERM) |>
  select(STUDYID, DOMAIN, USUBJID, MHSEQ, MHTERM, MHDECOD, MHBODSYS,
         MHSTDTC, MHENDTC, MHSTDY, MHENDY, MHENRTPT, MHENRF) |>
  arrange(USUBJID, MHSEQ) |>
  apply_labels(c(
    STUDYID  = "Study Identifier",
    DOMAIN   = "Domain Abbreviation",
    USUBJID  = "Unique Subject Identifier",
    MHSEQ    = "Sequence Number",
    MHTERM   = "Reported Term for the Medical History",
    MHDECOD  = "Dictionary-Derived Term",
    MHBODSYS = "Body System or Organ Class",
    MHSTDTC  = "Start Date/Time of Medical History",
    MHENDTC  = "End Date/Time of Medical History",
    MHSTDY   = "Study Day of Start of Medical History",
    MHENDY   = "Study Day of End of Medical History",
    MHENRTPT = "End Relative to Reference Time Point",
    MHENRF   = "End Relative to Reference Period"
  ))

write_sdtm(mh, "MH")

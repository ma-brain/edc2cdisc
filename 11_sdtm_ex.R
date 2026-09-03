# ============================================================================
# Title:   SDTM EX - Exposure
# Purpose: Derive EX from the Rave EX view (one record per dosing interval)
# Input:   data/rave/EX.csv, subject_ref + visit_map
# Output:  data/sdtm/ex.rds, data/sdtm/xpt/ex.xpt
# ============================================================================

ex_raw <- read_rave_form("EX")

ex <- ex_raw |>
  # Only administrations that actually occurred become EX records.
  filter(EXOCCUR == "1") |>
  transmute(
    STUDYID = .env$STUDYID,
    DOMAIN  = "EX",
    USUBJID = str_c(STUDYID, Subject, sep = "-"),
    EXTRT   = str_to_upper(str_squish(EXTRT)),
    EXDOSE  = suppressWarnings(as.numeric(EXDOSE)),
    EXDOSU  = EXDOSU,
    # Rave stores the route decode ("ORAL"); it is already CDISC ROUTE CT.
    EXROUTE = str_to_upper(EXROUTE_DECODE),
    Folder  = Folder,
    EXSTDTC = rave_dtc(EXSTDAT_YYYY, EXSTDAT_MM, EXSTDAT_DD),
    EXENDTC = rave_dtc(EXENDAT_YYYY, EXENDAT_MM, EXENDAT_DD)
  ) |>
  left_join(visit_map, by = "Folder") |>
  left_join(subject_ref, by = "USUBJID") |>
  mutate(
    EXSTDY = derive_dy(EXSTDTC, RFSTDTC),
    EXENDY = derive_dy(EXENDTC, RFSTDTC)
  ) |>
  derive_seq("EXSEQ", VISITNUM, EXSTDTC) |>
  select(STUDYID, DOMAIN, USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU, EXROUTE,
         VISITNUM, VISIT, EPOCH, EXSTDTC, EXENDTC, EXSTDY, EXENDY) |>
  arrange(USUBJID, EXSEQ) |>
  apply_labels(c(
    STUDYID  = "Study Identifier",
    DOMAIN   = "Domain Abbreviation",
    USUBJID  = "Unique Subject Identifier",
    EXSEQ    = "Sequence Number",
    EXTRT    = "Name of Treatment",
    EXDOSE   = "Dose",
    EXDOSU   = "Dose Units",
    EXROUTE  = "Route of Administration",
    VISITNUM = "Visit Number",
    VISIT    = "Visit Name",
    EPOCH    = "Epoch",
    EXSTDTC  = "Start Date/Time of Treatment",
    EXENDTC  = "End Date/Time of Treatment",
    EXSTDY   = "Study Day of Start of Treatment",
    EXENDY   = "Study Day of End of Treatment"
  ))

write_sdtm(ex, "EX")

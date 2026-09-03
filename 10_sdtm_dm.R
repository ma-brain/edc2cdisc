# ============================================================================
# Title:   SDTM DM - Demographics
# Purpose: Derive DM from the Rave DM view, with reference dates from EX/DS
# Input:   data/rave/DM.csv, EX.csv, DS.csv
# Output:  data/sdtm/dm.rds, data/sdtm/xpt/dm.xpt
# Note:    DM is built first because every other domain needs RFSTDTC for --DY
# ============================================================================

dm_raw <- read_rave_form("DM")
ex_raw <- read_rave_form("EX")
ds_raw <- read_rave_form("DS")

# Reference dates ------------------------------------------------------------

ref_exposure <- ex_raw |>
  filter(EXOCCUR == "1") |>
  mutate(
    usubjid  = str_c(STUDYID, Subject, sep = "-"),
    exstdtc  = rave_dtc(EXSTDAT_YYYY, EXSTDAT_MM, EXSTDAT_DD),
    exendtc  = rave_dtc(EXENDAT_YYYY, EXENDAT_MM, EXENDAT_DD)
  ) |>
  summarise(
    RFXSTDTC = min_dtc(exstdtc),
    RFXENDTC = max_dtc(exendtc),
    .by = usubjid
  )

ref_disposition <- ds_raw |>
  mutate(
    usubjid  = str_c(STUDYID, Subject, sep = "-"),
    RFPENDTC = rave_dtc(DSSTDAT_YYYY, DSSTDAT_MM, DSSTDAT_DD)
  ) |>
  slice_max(RFPENDTC, by = usubjid, with_ties = FALSE) |>
  select(usubjid, RFPENDTC)

# Age: only computable when the birth date is complete. Partial birth dates
# stay missing here rather than being silently imputed - if the study needs
# an age for a partial date, that rule belongs in the SAP, not in the map.
# (ADSL applies that rule: see impute_dtc() and 30_adam_adsl.R.)

arm_map <- tribble(
  ~ARMCD_DECODE,   ~ARMCD,     ~ARM,
  "Placebo",       "PBO",      "Placebo",
  "SYN-101 50 mg", "SYN50",    "SYN-101 50 mg",
  "SYN-101 100 mg","SYN100",   "SYN-101 100 mg"
)

dm <- dm_raw |>
  mutate(usubjid = str_c(STUDYID, Subject, sep = "-")) |>
  # Drop the raw coded arm field ("1"/"2"/"3"); ARMCD/ARM are derived from
  # ARMCD_DECODE below and the bare name would otherwise clash on the join.
  select(-any_of(c("ARMCD", "ARMCD_RAW"))) |>
  left_join(ref_exposure, by = "usubjid") |>
  left_join(ref_disposition, by = "usubjid") |>
  left_join(arm_map, by = "ARMCD_DECODE") |>
  transmute(
    STUDYID  = .env$STUDYID,
    DOMAIN   = "DM",
    USUBJID  = usubjid,
    SUBJID   = Subject,
    RFSTDTC  = RFXSTDTC,
    RFENDTC  = RFXENDTC,
    RFXSTDTC = RFXSTDTC,
    RFXENDTC = RFXENDTC,
    RFICDTC  = rave_dtc(ICDAT_YYYY, ICDAT_MM, ICDAT_DD),
    RFPENDTC = RFPENDTC,
    DTHFL    = yn(DTHFL),
    DTHDTC   = rave_dtc(DTHDAT_YYYY, DTHDAT_MM, DTHDAT_DD),
    SITEID   = SiteNumber,
    BRTHDTC  = rave_dtc(BRTHDAT_YYYY, BRTHDAT_MM, BRTHDAT_DD),
    # Age at first dose; screen failures are aged at informed consent
    AGE      = compute_age(dtc_date(BRTHDTC), dtc_date(coalesce(RFSTDTC, RFICDTC))),
    AGEU     = if_else(is.na(AGE), NA_character_, "YEARS"),
    SEX      = recode_values(SEX_DECODE, "Male" ~ "M", "Female" ~ "F",
                             default = "U"),
    RACE     = RACE_DECODE,
    ETHNIC   = ETHNIC_DECODE,
    ARMCD    = coalesce(ARMCD, "SCRNFAIL"),
    ARM      = coalesce(ARM, "Screen Failure"),
    ACTARMCD = ARMCD,
    ACTARM   = ARM,
    COUNTRY  = unname(country_map[SiteNumber]),
    DMDTC    = rave_dtc(ICDAT_YYYY, ICDAT_MM, ICDAT_DD)
  ) |>
  mutate(DMDY = derive_dy(DMDTC, RFSTDTC)) |>
  arrange(USUBJID) |>
  apply_labels(c(
    STUDYID  = "Study Identifier",
    DOMAIN   = "Domain Abbreviation",
    USUBJID  = "Unique Subject Identifier",
    SUBJID   = "Subject Identifier for the Study",
    RFSTDTC  = "Subject Reference Start Date/Time",
    RFENDTC  = "Subject Reference End Date/Time",
    RFXSTDTC = "Date/Time of First Study Treatment",
    RFXENDTC = "Date/Time of Last Study Treatment",
    RFICDTC  = "Date/Time of Informed Consent",
    RFPENDTC = "Date/Time of End of Participation",
    DTHFL    = "Subject Death Flag",
    DTHDTC   = "Date/Time of Death",
    SITEID   = "Study Site Identifier",
    BRTHDTC  = "Date/Time of Birth",
    AGE      = "Age",
    AGEU     = "Age Units",
    SEX      = "Sex",
    RACE     = "Race",
    ETHNIC   = "Ethnicity",
    ARMCD    = "Planned Arm Code",
    ARM      = "Description of Planned Arm",
    ACTARMCD = "Actual Arm Code",
    ACTARM   = "Description of Actual Arm",
    COUNTRY  = "Country",
    DMDTC    = "Date/Time of Collection",
    DMDY     = "Study Day of Collection"
  ))

write_sdtm(dm, "DM")

# Reference table reused by every downstream domain
subject_ref <- dm |> select(USUBJID, RFSTDTC, RFENDTC)

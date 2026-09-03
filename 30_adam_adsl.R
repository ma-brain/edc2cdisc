# ============================================================================
# Title:   ADaM ADSL - Subject-Level Analysis Dataset
# Purpose: One row per subject: demographics, planned/actual treatment,
#          analysis dates, population flags, disposition. This is where the
#          partial-date imputation the SDTM map deliberately refused finally
#          happens - explicitly, and flagged.
# Input:   data/sdtm/{dm,ex,ds}.rds
# Output:  data/adam/adsl.rds, data/adam/xpt/adsl.xpt
# Note:    Analysis dates are proper Date columns (XPT v5 writes them as
#          numeric DATE values), unlike the all-character SDTM layer.
# ============================================================================

dm <- read_rds(file.path(paths$sdtm, "dm.rds"))
ex <- read_rds(file.path(paths$sdtm, "ex.rds"))
ds <- read_rds(file.path(paths$sdtm, "ds.rds"))

# Treatment dates: first dose start / last dose end off the EX records ------
dosing <- ex |>
  summarise(
    trtsdtc = min_dtc(EXSTDTC),
    trtedtc = max_dtc(EXENDTC),
    .by = USUBJID
  )

# Disposition: the DISPOSITION EVENT record carries end-of-study; the screen
# failures' PROTOCOL MILESTONE record is their last contact instead ---------
disposition <- ds |>
  filter(DSCAT == "DISPOSITION EVENT") |>
  select(USUBJID, dsstdtc = DSSTDTC, DSDECOD)

last_contact <- ds |>
  transmute(USUBJID, contactdtc = DSSTDTC) |>
  summarise(contactdtc = max_dtc(contactdtc), .by = USUBJID)

adsl <- dm |>
  left_join(dosing,      by = "USUBJID") |>
  left_join(disposition, by = "USUBJID") |>
  left_join(last_contact, by = "USUBJID") |>
  mutate(
    BRTHDT   = impute_dtc(BRTHDTC)$date,
    BRTHDTF  = impute_dtc(BRTHDTC)$flag,
    ENRLSTDT = impute_dtc(RFICDTC)$date,
    TRTSDT   = impute_dtc(trtsdtc)$date,
    TRTSDTF  = impute_dtc(trtsdtc)$flag,
    TRTEDT   = impute_dtc(trtedtc)$date,
    TRTEDTF  = impute_dtc(trtedtc)$flag,
    EOSDT    = impute_dtc(dsstdtc)$date,
    EOSDTF   = impute_dtc(dsstdtc)$flag,
    DTHDT    = impute_dtc(DTHDTC)$date,
    DTHDTF   = impute_dtc(DTHDTC)$flag
  ) |>
  transmute(
    STUDYID, USUBJID, SUBJID, SITEID, COUNTRY,

    # Birth date: SDTM kept it partial; ADSL resolves it per the imputation
    # rule in impute_dtc() and flags what was filled. AGE follows the
    # imputed date, so a birth year alone is enough.
    BRTHDTC,
    BRTHDT,
    BRTHDTF,
    AGE  = compute_age(BRTHDT, coalesce(TRTSDT, ENRLSTDT)),
    AGEU = if_else(is.na(AGE), NA_character_, "YEARS"),
    SEX, RACE, ETHNIC,

    # Population flags. The generator collects no randomization record, so
    # ITTFL is "randomized" (has a planned arm) and SAFFL is "dosed"
    # (has a treatment start). Safety requires randomization in this design,
    # which 39_validate_adam.R asserts.
    ITTFL = if_else(ARMCD != "SCRNFAIL", "Y", "N"),
    SAFFL = if_else(!is.na(TRTSDT), "Y", "N"),

    # Planned/actual treatment, populated only for randomized subjects.
    # Unlike DM's "Screen Failure" placeholder, ADaM carries the absence of
    # an arm as an absent value - that is what treatment comparison needs.
    TRT01P   = if_else(ITTFL == "Y", ARM, NA_character_),
    TRT01PCD = if_else(ITTFL == "Y", ARMCD, NA_character_),
    TRT01A   = TRT01P,
    TRT01ACD = TRT01PCD,

    # Analysis dates with their imputation flags (blank in the current
    # extract - EX, DS and consent dates are all complete - but the rule
    # would light up flagged if the generator starts emitting partials).
    ENRLSTDT,
    TRTSDT, TRTSDTF,
    TRTEDT, TRTEDTF,
    TRTDURD = as.integer(TRTEDT - TRTSDT) + 1L,

    # End of study: screen failures have no disposition record, so their
    # EOS* fields stay empty - they never entered the treated population.
    EOSDT, EOSDTF,
    EOSSTT = case_when(
      is.na(EOSDT)           ~ NA_character_,
      DSDECOD == "COMPLETED" ~ "COMPLETED",
      .default               = "DISCONTINUED"
    ),
    DCSREAS = if_else(EOSSTT == "DISCONTINUED", DSDECOD, NA_character_),

    # Death: the collected DM fact carried forward, with the death date
    # imputed and flagged like every other analysis date. Dying subjects
    # are discontinued with reason DEATH (their DS record), which the
    # EOS* logic above already handles.
    DTHDT  = DTHDT,
    DTHDTF = DTHDTF,

    # Last known alive: death is the anchor when it exists
    LSTALVDT = pmax(TRTEDT, as.Date(contactdtc), DTHDT, na.rm = TRUE),
    DTHFL    = if_else(DTHFL %in% "Y", "Y", "")
  ) |>
  arrange(USUBJID) |>
  select(STUDYID, USUBJID, SUBJID, SITEID, COUNTRY,
         AGE, AGEU, SEX, RACE, ETHNIC, BRTHDTC, BRTHDT, BRTHDTF,
         TRT01P, TRT01PCD, TRT01A, TRT01ACD,
         ENRLSTDT, TRTSDT, TRTSDTF, TRTEDT, TRTEDTF, TRTDURD,
         SAFFL, ITTFL, EOSDT, EOSDTF, EOSSTT, DCSREAS,
         DTHDT, DTHDTF, LSTALVDT, DTHFL) |>
  apply_labels(c(
    STUDYID   = "Study Identifier",
    USUBJID   = "Unique Subject Identifier",
    SUBJID    = "Subject Identifier for the Study",
    SITEID    = "Study Site Identifier",
    COUNTRY   = "Country",
    AGE       = "Age",
    AGEU      = "Age Units",
    SEX       = "Sex",
    RACE      = "Race",
    ETHNIC    = "Ethnicity",
    BRTHDTC   = "Date/Time of Birth",
    BRTHDT    = "Date of Birth (Imputed)",
    BRTHDTF   = "Date of Birth Imputation Flag",
    TRT01P    = "Planned Treatment for Period 01",
    TRT01PCD  = "Planned Treatment for Period 01, Code",
    TRT01A    = "Actual Treatment for Period 01",
    TRT01ACD  = "Actual Treatment for Period 01, Code",
    ENRLSTDT  = "Enrollment Start Date",
    TRTSDT    = "Treatment Start Date",
    TRTSDTF   = "Treatment Start Date Imputation Flag",
    TRTEDT    = "Treatment End Date",
    TRTEDTF   = "Treatment End Date Imputation Flag",
    TRTDURD   = "Treatment Duration (Days)",
    SAFFL     = "Safety Population Flag",
    ITTFL     = "Intent-To-Treat Population Flag",
    EOSDT     = "End of Study Date",
    EOSDTF    = "End of Study Date Imputation Flag",
    EOSSTT    = "End of Study Status",
    DCSREAS   = "Reason for Discontinuation from Study",
    DTHDT     = "Date of Death",
    DTHDTF    = "Date of Death Imputation Flag",
    LSTALVDT  = "Date of Last Known Alive",
    DTHFL     = "Subject Death Flag"
  ))

write_sdtm(adsl, "ADSL", rds_dir = paths$adam, xpt_dir = paths$xpt_adam)

# ============================================================================
# Title:   SDTM VS - Vital Signs
# Purpose: Pivot the horizontal Rave VS view into the SDTM findings structure
# Input:   data/rave/VS.csv, subject_ref from 10_sdtm_dm.R
# Output:  data/sdtm/vs.rds, data/sdtm/xpt/vs.xpt
# ============================================================================

vs_raw <- read_rave_form("VS")

# One row per CRF field -> one row per test. The spec table is the only thing
# that changes when a new vital sign is added to the CRF.
vs_spec <- tribble(
  ~field,   ~VSTESTCD, ~VSTEST,
  "SYSBP",  "SYSBP",   "Systolic Blood Pressure",
  "DIABP",  "DIABP",   "Diastolic Blood Pressure",
  "PULSE",  "PULSE",   "Pulse Rate",
  "TEMP",   "TEMP",    "Temperature",
  "WEIGHT", "WEIGHT",  "Weight",
  "HEIGHT", "HEIGHT",  "Height"
)

vs_long <- vs_spec |>
  pmap(\(field, VSTESTCD, VSTEST) {
    vs_raw |>
      mutate(
        VSTESTCD  = VSTESTCD,
        VSTEST    = VSTEST,
        VSORRES   = col_or_na(vs_raw, str_c(field, "_RAW")),
        VSORRESU  = col_or_na(vs_raw, str_c(field, "_UN")),
        RAVE_STD  = col_or_na(vs_raw, str_c(field, "_STD")),
        RAVE_STDU = col_or_na(vs_raw, str_c(field, "_STD_UN"))
      )
  }) |>
  list_rbind()

# Rave already converted the fields that have a standard unit configured
# (temperature, weight, height at the US site). Anything else falls back to
# a local conversion, which for mmHg and beats/min is the identity.
vs_std <- resolve_standard(vs_long$RAVE_STD, vs_long$RAVE_STDU,
                           vs_long$VSORRES, vs_long$VSORRESU)

vs <- vs_long |>
  mutate(
    VSSTAT   = if_else(VSPERF == "0", "NOT DONE", NA_character_),
    VSREASND = if_else(VSPERF == "0", "VISIT NOT PERFORMED", NA_character_),
    VSSTRESN = vs_std$stresn,
    VSSTRESU = if_else(is.na(vs_std$stresn), NA_character_, vs_std$stresu),
    VSSTRESC = as.character(VSSTRESN)
  ) |>
  # Drop tests not collected at this visit (e.g. height after screening),
  # but keep explicit NOT DONE records.
  filter(!is.na(VSORRES) | VSSTAT == "NOT DONE") |>
  transmute(
    STUDYID  = .env$STUDYID,
    DOMAIN   = "VS",
    USUBJID  = str_c(STUDYID, Subject, sep = "-"),
    VSTESTCD, VSTEST,
    VSPOS    = str_to_upper(VSPOS_DECODE),
    VSORRES, VSORRESU, VSSTRESC, VSSTRESN, VSSTRESU,
    VSSTAT, VSREASND,
    Folder   = Folder,
    VSDTC    = rave_dtc(VSDAT_YYYY, VSDAT_MM, VSDAT_DD, time = VSTIM)
  ) |>
  left_join(visit_map, by = "Folder") |>
  left_join(subject_ref, by = "USUBJID") |>
  mutate(VSDY = derive_dy(VSDTC, RFSTDTC))

# Baseline = last non-missing result on or before first dose ------------------

vs <- vs |>
  mutate(
    .eligible = !is.na(VSSTRESN) & !is.na(RFSTDTC) &
      dtc_date(VSDTC) <= dtc_date(RFSTDTC),
    # NA key for ineligible rows so they are excluded from the ranking
    .key  = if_else(.eligible, as.numeric(dtc_date(VSDTC)), NA_real_),
    .rank = rank(-.key, ties.method = "first", na.last = "keep"),
    .by = c(USUBJID, VSTESTCD)
  ) |>
  mutate(VSBLFL = if_else(!is.na(.rank) & .rank == 1, "Y", NA_character_)) |>
  select(-.eligible, -.key, -.rank)

vs <- vs |>
  derive_seq("VSSEQ", VISITNUM, VSTESTCD) |>
  select(STUDYID, DOMAIN, USUBJID, VSSEQ, VSTESTCD, VSTEST, VSPOS,
         VSORRES, VSORRESU, VSSTRESC, VSSTRESN, VSSTRESU, VSSTAT, VSREASND,
         VSBLFL, VISITNUM, VISIT, EPOCH, VSDTC, VSDY) |>
  arrange(USUBJID, VSSEQ) |>
  apply_labels(c(
    STUDYID  = "Study Identifier",
    DOMAIN   = "Domain Abbreviation",
    USUBJID  = "Unique Subject Identifier",
    VSSEQ    = "Sequence Number",
    VSTESTCD = "Vital Signs Test Short Name",
    VSTEST   = "Vital Signs Test Name",
    VSPOS    = "Vital Signs Position of Subject",
    VSORRES  = "Result or Finding in Original Units",
    VSORRESU = "Original Units",
    VSSTRESC = "Character Result/Finding in Std Units",
    VSSTRESN = "Numeric Result/Finding in Std Units",
    VSSTRESU = "Standard Units",
    VSSTAT   = "Completion Status",
    VSREASND = "Reason Not Performed",
    VSBLFL   = "Baseline Flag",
    VISITNUM = "Visit Number",
    VISIT    = "Visit Name",
    EPOCH    = "Epoch",
    VSDTC    = "Date/Time of Measurements",
    VSDY     = "Study Day of Vital Signs"
  ))

write_sdtm(vs, "VS")

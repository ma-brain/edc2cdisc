# ============================================================================
# Title:   ADaM ADVS - Vital Signs Analysis Dataset (BDS)
# Purpose: One analysis record per subject per visit per vital sign, in the
#          Basic Data Structure: AVAL, the baseline anchor (ABLFL/BASE), the
#          change derivations (CHG/PCHG), and ANRIND against declared
#          reference ranges.
# Input:   data/sdtm/vs.rds, data/adam/adsl.rds
# Output:  data/adam/advs.rds, data/adam/xpt/advs.xpt
# Note:    SDTM did the groundwork the BDS stands on: VSSTRESN is already in
#          standard units (Rave _STD or the local conversion fallback) and
#          VSBLFL is already the last pre-dose result - screen failures get
#          no baseline because RFSTDTC is the anchor.
# ============================================================================

vs   <- read_rds(file.path(paths$sdtm, "vs.rds"))
adsl <- read_rds(file.path(paths$adam, "adsl.rds"))

# Parameter order and reference ranges. VS carries no reference ranges at
# all, so ANRIND needs a declared table - this is the SAP stand-in, in one
# place, arguable by construction. Weight and height have no absolute adult
# range: their ANRIND stays missing by design, not by oversight.
param_spec <- tribble(
  ~PARAMCD, ~PARAMN, ~ANRLO, ~ANRHI,
  "SYSBP",  1,        90,     140,
  "DIABP",  2,        50,     90,
  "PULSE",  3,        50,     120,
  "TEMP",   4,        35,     37.5,
  "WEIGHT", 5,        NA,     NA,
  "HEIGHT", 6,        NA,     NA
)

vs_analysis <- vs |>
  # Analysis records are performed measurements. The NOT DONE row documents
  # a missed visit - SDTM keeps it with VSSTAT/VSREASND, but there is no
  # result to analyse, so BDS drops it. (%in% keeps the NA = performed rows.)
  filter(!VSSTAT %in% "NOT DONE") |>
  left_join(param_spec, by = c("VSTESTCD" = "PARAMCD")) |>
  left_join(adsl |> select(USUBJID, TRTSDT), by = "USUBJID") |>
  mutate(
    # SDTM's baseline flag carried forward under its ADaM name
    ABLFL = VSBLFL,
    ADT   = dtc_date(VSDTC),
    ADY   = derive_dy_d(ADT, TRTSDT)
  )

# Baseline value: the one ABLFL='Y' record per subject per parameter. Screen
# failures have no baseline row, so their BASE stays missing - a fact, not
# a gap.
base_ref <- vs_analysis |>
  filter(ABLFL == "Y") |>
  select(USUBJID, PARAMCD = VSTESTCD, BASE = VSSTRESN)

advs <- vs_analysis |>
  left_join(base_ref, by = c("USUBJID", "VSTESTCD" = "PARAMCD")) |>
  transmute(
    STUDYID, USUBJID,
    PARAMCD = VSTESTCD,
    PARAM   = str_c(VSTEST, " (", VSSTRESU, ")"),
    PARAMN,
    AVAL  = VSSTRESN,
    AVALU = VSSTRESU,
    ABLFL,
    BASE,
    # Change is defined against the baseline record; the baseline row itself
    # carries no CHG/PCHG (nothing to change from), and rows without a
    # baseline (screen failures) stay missing rather than zero. ABLFL is
    # "Y" or NA, so the %in% form matters: NA != "Y" would be NA and the
    # NA-armed if_else would nullify every post-baseline change.
    CHG  = case_when(
      ABLFL %in% "Y" ~ NA_real_,
      is.na(BASE)    ~ NA_real_,
      .default       = AVAL - BASE
    ),
    PCHG = if_else(!is.na(CHG), 100 * CHG / BASE, NA_real_),
    ANRIND = case_when(
      is.na(AVAL) | is.na(ANRLO) ~ NA_character_,
      AVAL < ANRLO               ~ "LOW",
      AVAL > ANRHI               ~ "HIGH",
      .default                   = "NORMAL"
    ),
    ANRLO, ANRHI,
    AVISIT  = VISIT,
    AVISITN = VISITNUM,
    ADT, ADY,
    # Position rides along: a handful of visits measured supine instead of
    # sitting - same analysis parameter (never both at one visit), but the
    # traceability matters.
    VSPOS
  ) |>
  arrange(USUBJID, PARAMN, AVISITN) |>
  apply_labels(c(
    STUDYID  = "Study Identifier",
    USUBJID  = "Unique Subject Identifier",
    PARAMCD  = "Parameter Code",
    PARAM    = "Parameter",
    PARAMN   = "Parameter (N)",
    AVAL     = "Analysis Value",
    AVALU    = "Analysis Value Unit",
    ABLFL    = "Baseline Record Flag",
    BASE     = "Baseline Value",
    CHG      = "Change from Baseline",
    PCHG     = "Percent Change from Baseline",
    ANRIND   = "Analysis Reference Range Indicator",
    ANRLO    = "Analysis Normal Range Lower Limit",
    ANRHI    = "Analysis Normal Range Upper Limit",
    AVISIT   = "Analysis Visit",
    AVISITN  = "Analysis Visit (N)",
    ADT      = "Analysis Date",
    ADY      = "Analysis Study Day",
    VSPOS    = "Position of Subject"
  ))

write_sdtm(advs, "ADVS", rds_dir = paths$adam, xpt_dir = paths$xpt_adam)

# ============================================================================
# Title:   ADaM ADLB - Laboratory Test Results Analysis Dataset (BDS)
# Purpose: One analysis record per subject per visit per analyte. The BDS
#          skeleton shared with ADVS (ABLFL/BASE/CHG/PCHG, ANRIND), with the
#          laboratory twist: reference ranges are not a declared table but
#          arrive with each record - sex-specific, per the central lab -
#          so ANRLO/ANRHI/ANRIND are sourced row-wise, and BNRIND carries
#          the baseline record's own range indicator.
# Input:   data/sdtm/lb.rds, data/adam/adsl.rds
# Output:  data/adam/adlb.rds, data/adam/xpt/adlb.xpt
# ============================================================================

lb   <- read_rds(file.path(paths$sdtm, "lb.rds"))
adsl <- read_rds(file.path(paths$adam, "adsl.rds"))

# Deterministic parameter order (chemistry panel first, matching the lab)
param_order <- c("GLUC", "CREAT", "HGB", "K", "ALT")

lb_analysis <- lb |>
  # Analysis records are performed panels. The NOT DONE row documents a
  # missed draw - SDTM keeps it with LBSTAT/LBREASND, BDS drops it.
  # (%in% keeps the NA = performed rows.)
  filter(!LBSTAT %in% "NOT DONE") |>
  left_join(adsl |> select(USUBJID, TRTSDT), by = "USUBJID") |>
  mutate(
    ABLFL = LBBLFL,
    ADT   = dtc_date(LBDTC),
    ADY   = derive_dy_d(ADT, TRTSDT),
    PARAMN = match(LBTESTCD, param_order)
  )

# Baseline anchor: the one ABLFL='Y' record per subject per analyte, giving
# both BASE and BNRIND. Screen failures have no baseline record, so their
# BASE/BNRIND/CHG stay missing while ANRIND still computes - the record's
# own range needs no study-day anchor.
base_ref <- lb_analysis |>
  filter(ABLFL == "Y") |>
  select(USUBJID, PARAMCD = LBTESTCD, BASE = LBSTRESN, BNRIND = LBNRIND)

adlb <- lb_analysis |>
  left_join(base_ref, by = c("USUBJID", "LBTESTCD" = "PARAMCD")) |>
  transmute(
    STUDYID, USUBJID,
    PARAMCD = LBTESTCD,
    PARAM   = str_c(LBTEST, " (", LBSTRESU, ")"),
    PARAMN,
    LBCAT,
    AVAL  = LBSTRESN,
    AVALU = LBSTRESU,
    ABLFL,
    BASE, BNRIND,
    # Same rules as ADVS; the %in% forms matter because ABLFL is "Y"/NA
    CHG  = case_when(
      ABLFL %in% "Y" ~ NA_real_,
      is.na(BASE)    ~ NA_real_,
      .default       = AVAL - BASE
    ),
    PCHG = if_else(!is.na(CHG), 100 * CHG / BASE, NA_real_),
    # The range arrives with the record (sex-specific), already standardised
    # by the same factor as the result in 17_sdtm_lb.R - so ANRIND is the
    # SDTM comparison carried forward, checkable against the row's own
    # limits.
    ANRIND = LBNRIND,
    ANRLO  = LBSTNRLO,
    ANRHI  = LBSTNRHI,
    AVISIT  = VISIT,
    AVISITN = VISITNUM,
    ADT, ADY
  ) |>
  arrange(USUBJID, PARAMN, AVISITN) |>
  apply_labels(c(
    STUDYID  = "Study Identifier",
    USUBJID  = "Unique Subject Identifier",
    PARAMCD  = "Parameter Code",
    PARAM    = "Parameter",
    PARAMN   = "Parameter (N)",
    LBCAT    = "Category for Lab Test",
    AVAL     = "Analysis Value",
    AVALU    = "Analysis Value Unit",
    ABLFL    = "Baseline Record Flag",
    BASE     = "Baseline Value",
    BNRIND   = "Baseline Reference Range Indicator",
    CHG      = "Change from Baseline",
    PCHG     = "Percent Change from Baseline",
    ANRIND   = "Analysis Reference Range Indicator",
    ANRLO    = "Analysis Normal Range Lower Limit",
    ANRHI    = "Analysis Normal Range Upper Limit",
    AVISIT   = "Analysis Visit",
    AVISITN  = "Analysis Visit (N)",
    ADT      = "Analysis Date",
    ADY      = "Analysis Study Day"
  ))

write_sdtm(adlb, "ADLB", rds_dir = paths$adam, xpt_dir = paths$xpt_adam)

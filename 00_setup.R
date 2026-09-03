# ============================================================================
# Title:   Project setup
# Purpose: Packages, paths, study constants and the visit / country maps that
#          every downstream script depends on
# Note:    source() this first. run_all.R does that for you.
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)   # dplyr, tidyr, readr, stringr, purrr, tibble, ...
  library(labelled)    # set_variable_labels(), var_label()
  library(haven)       # write_xpt()
})

# ---------------------------------------------------------------------------
# Paths - everything is relative to the project root
# ---------------------------------------------------------------------------

paths <- local({
  root <- tryCatch(here::here(), error = function(e) getwd())
  list(
    root     = root,
    rave     = file.path(root, "data", "rave"),       # synthetic source extract
    sdtm     = file.path(root, "data", "sdtm"),       # derived SDTM .rds
    xpt      = file.path(root, "data", "sdtm", "xpt"),    # SDTM SAS v5 XPT
    adam     = file.path(root, "data", "adam"),       # derived ADaM .rds
    xpt_adam = file.path(root, "data", "adam", "xpt")     # ADaM SAS v5 XPT
  )
})

for (p in c(paths$rave, paths$sdtm, paths$xpt, paths$adam, paths$xpt_adam)) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

# ---------------------------------------------------------------------------
# Study-level constants (must match 05_generate_rave_extract.R)
# ---------------------------------------------------------------------------

PROJECT     <- "SYNTH01"
PROJECT_ID  <- "1487"
STUDYID     <- "3021"
ENVIRONMENT <- "Prod"
USER_ID     <- "9911"

# Reproducibility: the generator is stochastic, so pin one seed here and let
# 05_generate_rave_extract.R pick it up.
RAVE_SEED <- 20260903
RAVE_N    <- 24

# ---------------------------------------------------------------------------
# Site table  (siteid, SiteNumber, Site name, SiteGroup, StudySiteId, country)
# ---------------------------------------------------------------------------

sites <- tribble(
  ~siteid, ~SiteNumber, ~Site,                         ~SiteGroup,       ~StudySiteId, ~COUNTRY,
  "4101",  "101",       "Spitalul Clinic Timisoara",   "Europe",         "7701",       "ROU",
  "4102",  "102",       "Charite Berlin",              "Europe",         "7702",       "DEU",
  "4103",  "103",       "Mercy General Hospital",      "North America",  "7703",       "USA"
)

# SiteNumber -> ISO 3166-1 alpha-3, consumed by 10_sdtm_dm.R
country_map <- set_names(sites$COUNTRY, sites$SiteNumber)

# ---------------------------------------------------------------------------
# Visit map  (Folder OID -> VISITNUM / VISIT / EPOCH)
# ---------------------------------------------------------------------------
# Joined onto scheduled-visit domains (VS, EX) by Folder. Log forms (AE, CM)
# never join this - they have no folder.

visit_map <- tribble(
  ~Folder, ~VISITNUM, ~VISIT,             ~EPOCH,
  "SCRN",  1,         "SCREENING",        "SCREENING",
  "BASE",  2,         "BASELINE",         "TREATMENT",
  "WK02",  3,         "WEEK 2",           "TREATMENT",
  "WK04",  4,         "WEEK 4",           "TREATMENT",
  "WK08",  5,         "WEEK 8",           "TREATMENT",
  "EOT",   6,         "END OF TREATMENT", "TREATMENT"
)

# Target study day per folder, for reference / SV work later
visit_targets <- c(SCRN = -14, BASE = 0, WK02 = 14, WK04 = 28, WK08 = 56, EOT = 84)

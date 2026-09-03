# ============================================================================
# Title:   The SYNTH01 study specification
# Purpose: Shipped reference implementation of a study_spec: the complete
#          mapping configuration for the synthetic study the package
#          generates. A new study in the same CRF family is a new object
#          of this shape - not a code change.
# ============================================================================

#' The SYNTH01 study specification
#'
#' A [study_spec][new_study_spec()] for the synthetic study shipped by
#' [generate_rave_extract()]: study constants, three sites (ROU / DEU / USA),
#' three arms, a six-visit schedule, the controlled-terminology mappings for
#' AE outcome / action / severity / causality and disposition reason, the
#' SUPP-- qualifiers, the LB conventional-to-SI conversions, form typing,
#' the VS/LB per-test pivot specs, and the variable-level mapping table for
#' the event/log forms.
#'
#' @format A `study_spec` object (a validated named list of tibbles).
#' @source Assembled from the SYNTH01 CRF design; see `?new_study_spec`
#'   for the table structure.
#' @export
spec_synth01 <- new_study_spec(
  study = tibble(
    STUDYID = "3021",
    PROJECT = "SYNTH01",
    seed    = 20260903,
    n       = 24L
  ),

  sites = tribble(
    ~siteid, ~SiteNumber, ~Site,                         ~SiteGroup,       ~StudySiteId, ~COUNTRY,
    "4101",  "101",       "Spitalul Clinic Timisoara",   "Europe",         "7701",       "ROU",
    "4102",  "102",       "Charite Berlin",              "Europe",         "7702",       "DEU",
    "4103",  "103",       "Mercy General Hospital",      "North America",  "7703",       "USA"
  ),

  arms = tribble(
    ~ARMCD_DECODE,    ~ARMCD,   ~ARM,
    "Placebo",        "PBO",    "Placebo",
    "SYN-101 50 mg",  "SYN50",  "SYN-101 50 mg",
    "SYN-101 100 mg", "SYN100", "SYN-101 100 mg"
  ),

  visits = tribble(
    ~Folder, ~VISITNUM, ~VISIT,             ~EPOCH,        ~TargetDays,
    "SCRN",  1,         "SCREENING",        "SCREENING",   -14L,
    "BASE",  2,         "BASELINE",         "TREATMENT",   0L,
    "WK02",  3,         "WEEK 2",           "TREATMENT",   14L,
    "WK04",  4,         "WEEK 4",           "TREATMENT",   28L,
    "WK08",  5,         "WEEK 8",           "TREATMENT",   56L,
    "EOT",   6,         "END OF TREATMENT", "TREATMENT",   84L
  ),

  codelists = tribble(
    ~ct,      ~rave_decode,                    ~cdisc_term,
    # AE outcome (AEOUT)
    "AEOUT",  "Recovered/Resolved",            "RECOVERED/RESOLVED",
    "AEOUT",  "Recovering/Resolving",          "RECOVERING/RESOLVING",
    "AEOUT",  "Not recovered/Not resolved",    "NOT RECOVERED/NOT RESOLVED",
    "AEOUT",  "Fatal",                         "FATAL",
    "AEOUT",  "Unknown",                       "UNKNOWN",
    # AE action taken with study drug (AEACN)
    "AEACN",  "Dose not changed",              "DOSE NOT CHANGED",
    "AEACN",  "Dose reduced",                  "DOSE REDUCED",
    "AEACN",  "Drug interrupted",              "DRUG INTERRUPTED",
    "AEACN",  "Drug withdrawn",                "DRUG WITHDRAWN",
    # AE severity (AESEV)
    "AESEV",  "Mild",                          "MILD",
    "AESEV",  "Moderate",                      "MODERATE",
    "AESEV",  "Severe",                        "SEVERE",
    # AE causality (AEREL): sponsor convention collapses the 4-point
    # scale to related / not related
    "AEREL",  "Not related",                   "NOT RELATED",
    "AEREL",  "Possibly related",              "RELATED",
    "AEREL",  "Probably related",              "RELATED",
    "AEREL",  "Related",                       "RELATED",
    # Disposition reason (DSDECOD): Rave decode -> CDISC NCOMPLT-style CT.
    # Explicit lookup so a new reason value breaks the run rather than
    # passing through silently.
    "DSDECOD", "COMPLETED",                    "COMPLETED",
    "DSDECOD", "ADVERSE EVENT",                "ADVERSE EVENT",
    "DSDECOD", "WITHDRAWAL BY SUBJECT",        "WITHDRAWAL BY SUBJECT",
    "DSDECOD", "LOST TO FOLLOW-UP",            "LOST TO FOLLOW-UP",
    "DSDECOD", "PROTOCOL DEVIATION",           "PROTOCOL DEVIATION",
    "DSDECOD", "DEATH",                        "DEATH",
    # Sex (SEX): collected decode -> SDTM sex code; anything else is "U"
    # (handled by the decode default in the variables table)
    "SEX",    "Male",                          "M",
    "SEX",    "Female",                        "F"
  ),

  supp = tribble(
    ~rdomain, ~idvar,   ~qnam,      ~qlabel,                              ~src,       ~transform, ~qorig,         ~qeval,
    "DM",     NA,       "SUBJINIT", "Subject Initials",                   "SUBJINIT", "squish",   "CRF",          NA,
    "DM",     NA,       "CHILDPOT", "Childbearing Potential",             "CHILDPOT", "yn",       "CRF",          NA,
    "DM",     NA,       "RACEOTH",  "Race, Other Specify",                "RACEOTH",  "verbatim", "CRF",          NA,
    "AE",     "AESEQ",  "AESI",     "Adverse Event of Special Interest",  "AESI",     "yn",       "CRF",          "INVESTIGATOR",
    "AE",     "AESEQ",  "AEDISCON", "AE Led to Study Discontinuation",    "AEDISCON", "yn",       "CRF",          NA,
    "EX",     "EXSEQ",  "EXADMBY",  "Dose Administered By",               "EXADMBY",  "squish",   "CRF",          NA
  ),

  units = tribble(
    ~testcd,  ~conv_from, ~conv_to,  ~conv_factor,
    "GLUC",   "MG/DL",    "mmol/L",  1 / 18.0156,
    "CREAT",  "MG/DL",    "umol/L",  88.42,
    "HGB",    "G/DL",     "g/L",     10
  ),

  forms = tribble(
    ~form_oid, ~type,    ~scheduled,
    "DM",      "event",  TRUE,
    "VS",      "event",  TRUE,
    "EX",      "event",  TRUE,
    "DS",      "event",  TRUE,
    "LB",      "event",  TRUE,
    "AE",      "log",    FALSE,
    "CM",      "log",    FALSE,
    "MH",      "log",    FALSE
  ),

  tests = tribble(
    ~domain,  ~field,   ~testcd,  ~test,                        ~cat,         ~specimen,
    "VS",     "SYSBP",  "SYSBP",  "Systolic Blood Pressure",    NA,           NA,
    "VS",     "DIABP",  "DIABP",  "Diastolic Blood Pressure",   NA,           NA,
    "VS",     "PULSE",  "PULSE",  "Pulse Rate",                 NA,           NA,
    "VS",     "TEMP",   "TEMP",   "Temperature",                NA,           NA,
    "VS",     "WEIGHT", "WEIGHT", "Weight",                     NA,           NA,
    "VS",     "HEIGHT", "HEIGHT", "Height",                     NA,           NA,
    "LB",     "GLUC",   "GLUC",   "Glucose",                    "CHEMISTRY",  "SERUM",
    "LB",     "CREAT",  "CREAT",  "Creatinine",                 "CHEMISTRY",  "SERUM",
    "LB",     "HGB",    "HGB",    "Hemoglobin",                 "HEMATOLOGY", "BLOOD",
    "LB",     "POT",    "K",      "Potassium",                  "CHEMISTRY",  "SERUM",
    "LB",     "ALT",    "ALT",    "Alanine Aminotransferase",   "CHEMISTRY",  "SERUM"
  ),

  bds = tribble(
    ~domain, ~paramcd, ~paramn, ~anrlo, ~anrhi,
    # ADVS: the SAP stand-in ranges. Weight and height have no absolute
    # adult range - their ANRIND stays missing by design, not oversight.
    "ADVS", "SYSBP",  1,  90,   140,
    "ADVS", "DIABP",  2,  50,   90,
    "ADVS", "PULSE",  3,  50,   120,
    "ADVS", "TEMP",   4,  35,   37.5,
    "ADVS", "WEIGHT", 5,  NA,   NA,
    "ADVS", "HEIGHT", 6,  NA,   NA,
    # ADLB: ranges arrive with each record (sex-specific, central lab), so
    # the spec carries only the deterministic PARAMN ordering.
    "ADLB", "GLUC",   1,  NA,   NA,
    "ADLB", "CREAT",  2,  NA,   NA,
    "ADLB", "HGB",    3,  NA,   NA,
    "ADLB", "K",      4,  NA,   NA,
    "ADLB", "ALT",    5,  NA,   NA
  ),

  variables = tribble(
    ~domain, ~variable, ~crf_field,        ~transform,    ~ref,      ~value,     ~aux,     ~default,
    # ---- DM (output order matters: DM has no trailing select) ----
    "DM",    "STUDYID",  NA,                "derivation",  "study_id", NA,        NA,       NA,
    "DM",    "DOMAIN",   NA,                "constant",    NA,         "DM",      NA,       NA,
    "DM",    "USUBJID",  NA,                "derivation",  "usubjid",  NA,        NA,       NA,
    "DM",    "SUBJID",   "Subject",         "rename",      NA,         NA,        NA,       NA,
    "DM",    "RFSTDTC",  "RFXSTDTC",        "rename",      NA,         NA,        NA,       NA,
    "DM",    "RFENDTC",  "RFXENDTC",        "rename",      NA,         NA,        NA,       NA,
    "DM",    "RFXSTDTC", "RFXSTDTC",        "rename",      NA,         NA,        NA,       NA,
    "DM",    "RFXENDTC", "RFXENDTC",        "rename",      NA,         NA,        NA,       NA,
    "DM",    "RFICDTC",  "ICDAT",           "dtc",         NA,         NA,        NA,       NA,
    "DM",    "RFPENDTC", "RFPENDTC",        "rename",      NA,         NA,        NA,       NA,
    "DM",    "DTHFL",    "DTHFL",           "yn",          NA,         NA,        NA,       NA,
    "DM",    "DTHDTC",   "DTHDAT",          "dtc",         NA,         NA,        NA,       NA,
    "DM",    "SITEID",   "SiteNumber",      "rename",      NA,         NA,        NA,       NA,
    "DM",    "BRTHDTC",  "BRTHDAT",         "dtc",         NA,         NA,        NA,       NA,
    "DM",    "AGE",      NA,                "derivation",  "age_at_first_dose", NA, NA,     NA,
    "DM",    "AGEU",     NA,                "derivation",  "ageu",     NA,        NA,       NA,
    "DM",    "SEX",      "SEX_DECODE",      "decode",      "SEX",      NA,        NA,       "U",
    "DM",    "RACE",     "RACE_DECODE",     "rename",      NA,         NA,        NA,       NA,
    "DM",    "ETHNIC",   "ETHNIC_DECODE",   "rename",      NA,         NA,        NA,       NA,
    "DM",    "ARMCD",    NA,                "derivation",  "armcd",    NA,        NA,       NA,
    "DM",    "ARM",      NA,                "derivation",  "arm",      NA,        NA,       NA,
    "DM",    "ACTARMCD", "ARMCD",           "rename",      NA,         NA,        NA,       NA,
    "DM",    "ACTARM",   "ARM",             "rename",      NA,         NA,        NA,       NA,
    "DM",    "COUNTRY",  NA,                "derivation",  "country",  NA,        NA,       NA,
    "DM",    "DMDTC",    "ICDAT",           "dtc",         NA,         NA,        NA,       NA,
    "DM",    "DMDY",     NA,                "derivation",  "dmdy",     NA,        NA,       NA,
    # ---- EX ----
    "EX",    "STUDYID",  NA,                "derivation",  "study_id", NA,        NA,       NA,
    "EX",    "DOMAIN",   NA,                "constant",    NA,         "EX",      NA,       NA,
    "EX",    "USUBJID",  NA,                "derivation",  "usubjid",  NA,        NA,       NA,
    "EX",    "EXTRT",    "EXTRT",           "upper_squish", NA,        NA,        NA,       NA,
    "EX",    "EXDOSE",   "EXDOSE",          "numeric",     NA,         NA,        NA,       NA,
    "EX",    "EXDOSU",   "EXDOSU",          "rename",      NA,         NA,        NA,       NA,
    # The collected route decode is already CDISC ROUTE CT.
    "EX",    "EXROUTE",  "EXROUTE_DECODE",  "upper",       NA,         NA,        NA,       NA,
    "EX",    "EXSTDTC",  "EXSTDAT",         "dtc",         NA,         NA,        NA,       NA,
    "EX",    "EXENDTC",  "EXENDAT",         "dtc",         NA,         NA,        NA,       NA,
    # ---- DS ----
    "DS",    "STUDYID",  NA,                "derivation",  "study_id", NA,        NA,       NA,
    "DS",    "DOMAIN",   NA,                "constant",    NA,         "DS",      NA,       NA,
    "DS",    "USUBJID",  NA,                "derivation",  "usubjid",  NA,        NA,       NA,
    "DS",    "DSTERM",   "DSTERM",          "squish",      NA,         NA,        NA,       NA,
    "DS",    "DSDECOD",  "DSREAS_DECODE",   "derivation",  "dsdecod",  NA,        NA,       NA,
    "DS",    "DSSTDTC",  "DSSTDAT",         "dtc",         NA,         NA,        NA,       NA,
    "DS",    "DSCAT",    NA,                "derivation",  "dscat",    NA,        NA,       NA,
    # ---- AE ----
    "AE",    "STUDYID",  NA,                "derivation",  "study_id", NA,        NA,       NA,
    "AE",    "DOMAIN",   NA,                "constant",    NA,         "AE",      NA,       NA,
    "AE",    "USUBJID",  NA,                "derivation",  "usubjid",  NA,        NA,       NA,
    # Rave log-line number, kept as AESPID so SUPPAE / CO can pin each
    # related record to an AE row without re-deriving AESEQ.
    "AE",    "AESPID",   "recordposition",  "character",   NA,         NA,        NA,       NA,
    "AE",    "AETERM",   "AETERM",          "verbatim",    NA,         NA,        NA,       NA,
    "AE",    "AEDECOD",  "AECOD_PT",        "rename",      NA,         NA,        NA,       NA,
    "AE",    "AEBODSYS", "AECOD_SOC",       "rename",      NA,         NA,        NA,       NA,
    "AE",    "AESEV",    "AESEV_DECODE",    "decode",      "AESEV",    NA,        NA,       NA,
    "AE",    "AESER",    "AESER",           "yn",          NA,         NA,        NA,       NA,
    "AE",    "AEREL",    "AEREL_DECODE",    "decode",      "AEREL",    NA,        NA,       NA,
    "AE",    "AEACN",    "AEACN_DECODE",    "decode",      "AEACN",    NA,        NA,       NA,
    "AE",    "AEOUT",    "AEOUT_DECODE",    "decode",      "AEOUT",    NA,        NA,       NA,
    "AE",    "AESTDTC",  "AESTDAT",         "dtc",         NA,         NA,        NA,       NA,
    "AE",    "AEENDTC",  "AEENDAT",         "dtc",         NA,         NA,        NA,       NA,
    "AE",    "AEENRTPT", NA,                "derivation",  "enrtpt_ongoing", NA,  NA,       NA,
    "AE",    "AEENRF",   NA,                "derivation",  "enrf_ongoing",   NA,  NA,       NA,
    # ---- CM ----
    "CM",    "STUDYID",  NA,                "derivation",  "study_id", NA,        NA,       NA,
    "CM",    "DOMAIN",   NA,                "constant",    NA,         "CM",      NA,       NA,
    "CM",    "USUBJID",  NA,                "derivation",  "usubjid",  NA,        NA,       NA,
    # Rave log-line number, kept as CMSPID so RELREC can pin linked records.
    "CM",    "CMSPID",   "CMSPID",          "character",   NA,         NA,        NA,       NA,
    "CM",    "CMTRT",    "CMTRT",           "verbatim",    NA,         NA,        NA,       NA,
    "CM",    "CMDECOD",  "CMCOD",           "rename",      NA,         NA,        NA,       NA,
    "CM",    "CMINDC",   "CMINDC",          "squish",      NA,         NA,        NA,       NA,
    "CM",    "CMDOSE",   "CMDOSE",          "numeric",     NA,         NA,        NA,       NA,
    "CM",    "CMDOSU",   "CMDOSU",          "rename",      NA,         NA,        NA,       NA,
    # Rave frequency / route decodes are already CDISC CT abbreviations.
    "CM",    "CMDOSFRQ", "CMFRQ_DECODE",    "upper",       NA,         NA,        NA,       NA,
    "CM",    "CMROUTE",  "CMROUTE_DECODE",  "upper",       NA,         NA,        NA,       NA,
    "CM",    "CMSTDTC",  "CMSTDAT",         "dtc",         NA,         NA,        NA,       NA,
    "CM",    "CMENDTC",  "CMENDAT",         "dtc",         NA,         NA,        NA,       NA,
    "CM",    "CMENRTPT", NA,                "derivation",  "enrtpt_ongoing_cm", NA, NA,      NA,
    "CM",    "CMENRF",   NA,                "derivation",  "enrf_ongoing_cm",  NA,  NA,       NA,
    # ---- MH ----
    "MH",    "STUDYID",  NA,                "derivation",  "study_id", NA,        NA,       NA,
    "MH",    "DOMAIN",   NA,                "constant",    NA,         "MH",      NA,       NA,
    "MH",    "USUBJID",  NA,                "derivation",  "usubjid",  NA,        NA,       NA,
    "MH",    "MHTERM",   "MHTERM",          "squish",      NA,         NA,        NA,       NA,
    "MH",    "MHDECOD",  "MHCOD_PT",        "rename",      NA,         NA,        NA,       NA,
    "MH",    "MHBODSYS", "MHCOD_SOC",       "rename",      NA,         NA,        NA,       NA,
    "MH",    "MHSTDTC",  "MHSTDAT",         "dtc",         NA,         NA,        NA,       NA,
    "MH",    "MHENDTC",  "MHENDAT",         "dtc",         NA,         NA,        NA,       NA,
    "MH",    "MHENRTPT", NA,                "derivation",  "enrtpt_ongoing_mh", NA, NA,      NA,
    "MH",    "MHENRF",   NA,                "derivation",  "enrf_ongoing_mh",  NA,  NA,       NA
  )
)

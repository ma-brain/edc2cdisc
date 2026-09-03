# ============================================================================
# Title:   Synthetic EDC clinical-view generator
# Purpose: Emit a fake "Regular" clinical view extract - one CSV per CRF form
#          plus the data dictionary and codelist reference - so the SDTM
#          mappers have source data without a live EDC system.
#
# Ported verbatim from the script-based 05_generate_rave_extract.R. The RNG
# draw order is load-bearing: with the committed seed the generator must
# reproduce the shipped extract byte-for-byte, and every downstream domain's
# regression baseline depends on it. Do not reorder draws.
#
# This is a straight R re-implementation of the retired python generator. It
# reproduces:
#   * the fixed 30-column CRF header block in front of every form
#   * field columns named from the field OID, with suffix families
#     (_RAW _DECODE _UN _STD _STD_UN _YYYY _MM _DD)
#   * every value written as a quoted character string
#   * a trailing EOF marker row
#
# Deliberate messiness kept from the original (this is the point of the data):
#   - mixed units (lb/kg, F/C) with populated _STD columns at the US site
#   - blood pressure / pulse have NO _STD column
#   - partial / unknown dates (UNK day, UNK day+month)
#   - ongoing AEs and CMs with blank end dates
#   - one inactive (soft-deleted) vitals record, RecordActive = "0"
#   - two screen failures with no post-screening data
#   - three early terminations with truncated visit schedules
#   - one "visit not performed" vitals record
#   - height collected at screening only
#   - trailing whitespace + case noise in verbatim terms
# ============================================================================

# Study-level constants of the synthetic study. These describe the CRF the
# generator emits - they are generator internals, not mapping configuration
# (the mapping side lives in the study spec).
.gen_project      <- "SYNTH01"
.gen_project_id   <- "1487"
.gen_studyid      <- "3021"
.gen_environment  <- "Prod"
.gen_user_id      <- "9911"

# Private RNG stream for the medical-history pass
.gen_mh_seed      <- 20260903L

# ---------------------------------------------------------------------------
# Reference tables
# ---------------------------------------------------------------------------

# siteid, SiteNumber, Site name, SiteGroup, StudySiteId
.SITES <- list(
  c("4101", "101", "Spitalul Clinic Timisoara", "Europe",        "7701"),
  c("4102", "102", "Charite Berlin",             "Europe",        "7702"),
  c("4103", "103", "Mercy General Hospital",     "North America", "7703")
)

# folderid, Folder OID, FolderName, InstanceName, FolderSeq, TargetDays
.FOLDERS <- list(
  SCRN = c("5001", "SCRN", "Screening",        "Screening",        "1.0",  "-14"),
  BASE = c("5002", "BASE", "Baseline",         "Baseline",         "2.0",  "0"),
  WK02 = c("5003", "WK02", "Week 2",           "Week 2",           "3.0",  "14"),
  WK04 = c("5004", "WK04", "Week 4",           "Week 4",           "4.0",  "28"),
  WK08 = c("5005", "WK08", "Week 8",           "Week 8",           "5.0",  "56"),
  EOT  = c("5006", "EOT",  "End of Treatment", "End of Treatment", "6.0",  "84"),
  LOG  = c("5900", "LOG",  "Log Forms",        "Log Forms",        "99.0", "")
)

.VISIT_FOLDERS <- c("SCRN", "BASE", "WK02", "WK04", "WK08", "EOT")

.HEADER_COLS <- c(
  "userid", "projectid", "project", "studyid", "environmentName",
  "subjectId", "StudySiteId", "Subject", "siteid", "Site", "SiteNumber",
  "SiteGroup", "instanceId", "InstanceName", "InstanceRepeatNumber",
  "folderid", "Folder", "FolderName", "FolderSeq", "TargetDays",
  "DataPageId", "DataPageName", "PageRepeatNumber", "RecordDate",
  "RecordId", "recordposition", "RecordActive", "SaveTs", "MinCreated",
  "MaxUpdated"
)

.MONTH_ABBR <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                 "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

# verbatim, coded PT, coded SOC
.AE_DICTIONARY <- list(
  c("Headache", "Headache", "Nervous system disorders"),
  c("Head ache", "Headache", "Nervous system disorders"),
  c("Nausea", "Nausea", "Gastrointestinal disorders"),
  c("Feeling sick", "Nausea", "Gastrointestinal disorders"),
  c("Fatigue", "Fatigue", "General disorders and administration site conditions"),
  c("Injection site pain", "Injection site pain",
    "General disorders and administration site conditions"),
  c("Injection site erythema", "Injection site erythema",
    "General disorders and administration site conditions"),
  c("Pyrexia", "Pyrexia", "General disorders and administration site conditions"),
  c("Dizziness", "Dizziness", "Nervous system disorders"),
  c("Upper respiratory tract infection", "Upper respiratory tract infection",
    "Infections and infestations"),
  c("Diarrhoea", "Diarrhoea", "Gastrointestinal disorders"),
  c("Back pain", "Back pain", "Musculoskeletal and connective tissue disorders")
)

# verbatim, coded, indication, dose, unit, route code, frequency code
.CM_DICTIONARY <- list(
  c("Paracetamol", "PARACETAMOL", "Headache", "500", "mg", "1", "3"),
  c("Ibuprofen", "IBUPROFEN", "Back pain", "400", "mg", "1", "2"),
  c("Lisinopril", "LISINOPRIL", "Hypertension", "10", "mg", "1", "1"),
  c("Metformin", "METFORMIN", "Type 2 diabetes mellitus", "850", "mg", "1", "2"),
  c("Atorvastatin", "ATORVASTATIN", "Hypercholesterolaemia", "20", "mg", "1", "1"),
  c("Vitamin D", "COLECALCIFEROL", "Vitamin D deficiency", "1000", "IU", "1", "1")
)

# verbatim, coded PT, coded SOC - medical history is chronic conditions, so
# the start dates reach back months or years and are often only partly known
.MH_DICTIONARY <- list(
  c("High blood pressure", "Hypertension", "Vascular disorders"),
  c("Hypertension", "Hypertension", "Vascular disorders"),
  c("Type 2 diabetes", "Type 2 diabetes mellitus", "Endocrine disorders"),
  c("Diabetes mellitus", "Type 2 diabetes mellitus", "Endocrine disorders"),
  c("High cholesterol", "Hypercholesterolaemia",
    "Metabolism and nutrition disorders"),
  c("Asthma", "Asthma", "Respiratory, thoracic and mediastinal disorders"),
  c("Underactive thyroid", "Hypothyroidism", "Endocrine disorders"),
  c("Migraine", "Migraine", "Nervous system disorders")
)

# codelist OID -> named vector of coded value -> decode
.CODELISTS <- list(
  SEX    = c("1" = "Male", "2" = "Female"),
  RACE   = c("1" = "WHITE",
             "2" = "BLACK OR AFRICAN AMERICAN",
             "3" = "ASIAN",
             "4" = "AMERICAN INDIAN OR ALASKA NATIVE",
             "5" = "OTHER"),
  ETHNIC = c("1" = "HISPANIC OR LATINO",
             "2" = "NOT HISPANIC OR LATINO",
             "3" = "NOT REPORTED"),
  YN     = c("1" = "Yes", "0" = "No"),
  SEV    = c("1" = "Mild", "2" = "Moderate", "3" = "Severe"),
  REL    = c("1" = "Not related", "2" = "Possibly related",
             "3" = "Probably related", "4" = "Related"),
  ACN    = c("1" = "Dose not changed", "2" = "Dose reduced",
             "3" = "Drug interrupted", "4" = "Drug withdrawn"),
  OUT    = c("1" = "Recovered/Resolved", "2" = "Recovering/Resolving",
             "3" = "Not recovered/Not resolved", "4" = "Fatal", "5" = "Unknown"),
  POS    = c("1" = "Sitting", "2" = "Supine", "3" = "Standing"),
  ROUTE  = c("1" = "ORAL", "2" = "SUBCUTANEOUS", "3" = "INTRAVENOUS", "4" = "TOPICAL"),
  FRQ    = c("1" = "QD", "2" = "BID", "3" = "TID", "4" = "PRN"),
  DSREAS = c("1" = "COMPLETED",
             "2" = "ADVERSE EVENT",
             "3" = "WITHDRAWAL BY SUBJECT",
             "4" = "LOST TO FOLLOW-UP",
             "5" = "PROTOCOL DEVIATION",
             "6" = "DEATH"),
  ARM    = c("1" = "Placebo", "2" = "SYN-101 50 mg", "3" = "SYN-101 100 mg")
)

# ---------------------------------------------------------------------------
# Small helpers  (thin wrappers so the port reads like the original)
# ---------------------------------------------------------------------------

 randint <- function(a, b) if (a == b) as.integer(a) else sample(a:b, 1L)
choice  <- function(x) x[[sample.int(length(x), 1L)]]
choices <- function(x, w) x[[sample.int(length(x), 1L, prob = w)]]

id_gen <- function(start) {
  v <- start
  function() {
    v <<- v + randint(1L, 4L)
    as.character(v)
  }
}

# fixed per-form id offsets (the python version hashed the form OID, which is
# not portable; a lookup keeps the surrogate keys stable run to run)
.FORM_ID_OFFSET <- c(DM = 1000L, VS = 12000L, AE = 23000L,
                     CM = 34000L, EX = 45000L, DS = 56000L, LB = 67000L,
                     MH = 78000L)

iso_dt <- function(d, hh = 9L, mm = 30L, ss = 0L) {
  sprintf("%sT%02d:%02d:%02d", format(as.Date(d), "%Y-%m-%d"), hh, mm, ss)
}

# EDC date column family for one date field:
#   missing = "none"     full date
#             "day"      UNK day
#             "monthday" UNK day and month
#             "all"      nothing entered
date_parts <- function(d, missing = "none") {
  if (length(d) == 0 || is.na(d) || missing == "all") {
    return(list(value = "", raw = "", yyyy = "", mm = "", dd = ""))
  }
  y <- as.integer(format(d, "%Y"))
  m <- as.integer(format(d, "%m"))
  dd <- as.integer(format(d, "%d"))
  mon <- .MONTH_ABBR[m]
  if (missing == "day") {
    v <- sprintf("UNK %s %d", mon, y)
    return(list(value = v, raw = v, yyyy = as.character(y),
                mm = as.character(m), dd = ""))
  }
  if (missing == "monthday") {
    v <- sprintf("UNK UNK %d", y)
    return(list(value = v, raw = v, yyyy = as.character(y), mm = "", dd = ""))
  }
  v <- sprintf("%02d %s %d", dd, mon, y)
  list(value = v, raw = v, yyyy = as.character(y),
       mm = as.character(m), dd = as.character(dd))
}

date_cols <- function(prefix, d, missing = "none") {
  p <- date_parts(d, missing)
  set_names(list(p$value, p$raw, p$yyyy, p$mm, p$dd),
            c(prefix, paste0(prefix, c("_RAW", "_YYYY", "_MM", "_DD"))))
}

coded_cols <- function(prefix, codelist, value) {
  if (length(value) == 0 || is.na(value) || identical(value, "")) {
    out <- list("", "", "")
  } else {
    out <- list(value, value, unname(.CODELISTS[[codelist]][[value]]))
  }
  set_names(out, c(prefix, paste0(prefix, c("_RAW", "_DECODE"))))
}

# one-decimal string, matching how the site would have keyed a measurement
d1 <- function(x) formatC(x, format = "f", digits = 1)

# fixed-decimal string with an explicit place count (lab results per analyte)
dfmt <- function(x, dp) formatC(x, format = "f", digits = dp)

# ---------------------------------------------------------------------------
# Form builder  (was the FormWriter class)
# ---------------------------------------------------------------------------

new_form <- function(form_oid, page_name, field_cols) {
  e <- new.env(parent = emptyenv())
  e$form_oid     <- form_oid
  e$page_name    <- page_name
  e$field_cols   <- field_cols
  e$rows         <- list()
  off <- .FORM_ID_OFFSET[[form_oid]]
  e$datapage_id  <- id_gen(600000L + off)
  e$record_id    <- id_gen(1300000L + off)
  e$instance_id  <- id_gen(190000L)
  e
}

form_add <- function(e, sub, folder_oid, rec_date, fields,
                     record_position = 0L, active = "1", page_repeat = 0L) {
  folder <- .FOLDERS[[folder_oid]]
  site   <- sub$site
  has_rec <- length(rec_date) == 1L && !is.na(rec_date)

  base_dt <- if (has_rec) as.Date(rec_date) else sub$scrn
  save_dt <- base_dt + randint(0L, 5L)
  created <- iso_dt(save_dt, randint(8L, 17L), randint(0L, 59L))
  updated <- iso_dt(save_dt + randint(0L, 20L), randint(8L, 17L), randint(0L, 59L))

  header <- list(
    userid = .gen_user_id, projectid = .gen_project_id, project = .gen_project,
    studyid = .gen_studyid, environmentName = .gen_environment,
    subjectId = sub$subjectId, StudySiteId = site[5], Subject = sub$Subject,
    siteid = site[1], Site = site[3], SiteNumber = site[2], SiteGroup = site[4],
    instanceId = e$instance_id(), InstanceName = folder[4],
    InstanceRepeatNumber = "0",
    folderid = folder[1], Folder = folder[2], FolderName = folder[3],
    FolderSeq = folder[5], TargetDays = folder[6],
    DataPageId = e$datapage_id(), DataPageName = e$page_name,
    PageRepeatNumber = as.character(page_repeat),
    RecordDate = if (has_rec) iso_dt(rec_date) else "",
    RecordId = e$record_id(), recordposition = as.character(record_position),
    RecordActive = active, SaveTs = created,
    MinCreated = created, MaxUpdated = updated
  )
  blanks <- set_names(as.list(rep("", length(e$field_cols))), e$field_cols)
  row <- modifyList(c(header, blanks), as.list(fields))
  e$rows[[length(e$rows) + 1L]] <- row
  invisible(NULL)
}

form_write <- function(e, outdir) {
  cols <- c(.HEADER_COLS, e$field_cols)
  # Rows may not carry every field (a CRF page that was never filled):
  # missing names align as blank, never as zero-length surprises.
  df <- map_dfr(e$rows, \(r) {
    vals <- lapply(cols, \(nm) if (is.null(r[[nm]])) NA_character_ else r[[nm]])
    as_tibble_row(set_names(vals, cols), .name_repair = "minimal")
  })
  df[] <- lapply(df, \(x) ifelse(is.na(x), "", as.character(x)))
  path <- file.path(outdir, paste0(e$form_oid, ".csv"))
  write_csv(df, path, quote = "all", eol = "\n", na = "")
  cat("EOF\n", file = path, append = TRUE)   # EDC extracts end with an EOF row
  invisible(path)
}

# ---------------------------------------------------------------------------
# Field column definitions  (order matters - it is the extract column order)
# ---------------------------------------------------------------------------

.DM_FIELDS <- c(
  "ICDAT", "ICDAT_RAW", "ICDAT_YYYY", "ICDAT_MM", "ICDAT_DD",
  "BRTHDAT", "BRTHDAT_RAW", "BRTHDAT_YYYY", "BRTHDAT_MM", "BRTHDAT_DD",
  "SUBJINIT",                                   # non-standard -> SUPPDM
  "SEX", "SEX_RAW", "SEX_DECODE",
  "RACE", "RACE_RAW", "RACE_DECODE",
  "RACEOTH",                                    # non-standard -> SUPPDM (RACE = OTHER only)
  "ETHNIC", "ETHNIC_RAW", "ETHNIC_DECODE",
  "CHILDPOT", "CHILDPOT_RAW", "CHILDPOT_DECODE", # non-standard -> SUPPDM (females only)
  "ARMCD", "ARMCD_RAW", "ARMCD_DECODE",
  "RANDDAT", "RANDDAT_RAW", "RANDDAT_YYYY", "RANDDAT_MM", "RANDDAT_DD"
)

.VS_FIELDS <- c(
  "VSPERF", "VSPERF_RAW", "VSPERF_DECODE",
  "VSDAT", "VSDAT_RAW", "VSDAT_YYYY", "VSDAT_MM", "VSDAT_DD",
  "VSTIM", "VSTIM_RAW",
  "VSPOS", "VSPOS_RAW", "VSPOS_DECODE",
  "SYSBP", "SYSBP_RAW", "SYSBP_UN",
  "DIABP", "DIABP_RAW", "DIABP_UN",
  "PULSE", "PULSE_RAW", "PULSE_UN",
  "TEMP", "TEMP_RAW", "TEMP_UN", "TEMP_STD", "TEMP_STD_UN",
  "WEIGHT", "WEIGHT_RAW", "WEIGHT_UN", "WEIGHT_STD", "WEIGHT_STD_UN",
  "HEIGHT", "HEIGHT_RAW", "HEIGHT_UN", "HEIGHT_STD", "HEIGHT_STD_UN"
)

.AE_FIELDS <- c(
  "AETERM", "AETERM_RAW",
  "AECOD_PT", "AECOD_SOC",
  "AESTDAT", "AESTDAT_RAW", "AESTDAT_YYYY", "AESTDAT_MM", "AESTDAT_DD",
  "AEENDAT", "AEENDAT_RAW", "AEENDAT_YYYY", "AEENDAT_MM", "AEENDAT_DD",
  "AEONG", "AEONG_RAW", "AEONG_DECODE",
  "AESEV", "AESEV_RAW", "AESEV_DECODE",
  "AESER", "AESER_RAW", "AESER_DECODE",
  "AEREL", "AEREL_RAW", "AEREL_DECODE",
  "AEACN", "AEACN_RAW", "AEACN_DECODE",
  "AEOUT", "AEOUT_RAW", "AEOUT_DECODE",
  "AESI", "AESI_RAW", "AESI_DECODE",             # non-standard -> SUPPAE
  "AEDISCON", "AEDISCON_RAW", "AEDISCON_DECODE", # non-standard -> SUPPAE
  "AECOMNT"                                      # free text -> CO domain
)

.CM_FIELDS <- c(
  "CMTRT", "CMTRT_RAW",
  "CMCOD", "CMINDC",
  "CMSTDAT", "CMSTDAT_RAW", "CMSTDAT_YYYY", "CMSTDAT_MM", "CMSTDAT_DD",
  "CMENDAT", "CMENDAT_RAW", "CMENDAT_YYYY", "CMENDAT_MM", "CMENDAT_DD",
  "CMONG", "CMONG_RAW", "CMONG_DECODE",
  "CMDOSE", "CMDOSE_RAW", "CMDOSU",
  "CMROUTE", "CMROUTE_RAW", "CMROUTE_DECODE",
  "CMFRQ", "CMFRQ_RAW", "CMFRQ_DECODE"
)

.MH_FIELDS <- c(
  "MHTERM", "MHTERM_RAW",
  "MHCOD_PT", "MHCOD_SOC",
  "MHSTDAT", "MHSTDAT_RAW", "MHSTDAT_YYYY", "MHSTDAT_MM", "MHSTDAT_DD",
  "MHENDAT", "MHENDAT_RAW", "MHENDAT_YYYY", "MHENDAT_MM", "MHENDAT_DD",
  "MHONG", "MHONG_RAW", "MHONG_DECODE"
)

.EX_FIELDS <- c(
  "EXOCCUR", "EXOCCUR_RAW", "EXOCCUR_DECODE",
  "EXTRT",
  "EXDOSE", "EXDOSE_RAW", "EXDOSU",
  "EXROUTE", "EXROUTE_RAW", "EXROUTE_DECODE",
  "EXSTDAT", "EXSTDAT_RAW", "EXSTDAT_YYYY", "EXSTDAT_MM", "EXSTDAT_DD",
  "EXENDAT", "EXENDAT_RAW", "EXENDAT_YYYY", "EXENDAT_MM", "EXENDAT_DD",
  "EXADMBY"                                      # non-standard -> SUPPEX
)

.DS_FIELDS <- c(
  "DSCOMP", "DSCOMP_RAW", "DSCOMP_DECODE",
  "DSSTDAT", "DSSTDAT_RAW", "DSSTDAT_YYYY", "DSSTDAT_MM", "DSSTDAT_DD",
  "DSREAS", "DSREAS_RAW", "DSREAS_DECODE",
  "DSTERM", "DSTERM_RAW"
)

# ---------------------------------------------------------------------------
# Central-lab panel
# ---------------------------------------------------------------------------
# Labs are drawn at a subset of the visit schedule (no Week 2 sample).
.LB_VISIT_FOLDERS <- c("SCRN", "BASE", "WK04", "WK08", "EOT")

# One entry per analyte:
#   oid    - EDC field OID (the CDISC LBTESTCD is assigned in the LB mapper)
#   std    - does the extract populate a converted _STD / _STD_UN column?
#   f      - factor to multiply the US-site conventional value by to reach SI
#   un_us  - unit the US site (site 103) keys the result in
#   un_si  - the SI / standard unit the EU sites key directly
#   dp_*   - decimal places the value carries in that unit
#   nr     - sex-specific reference range, expressed in the SI unit
.LB_PANEL <- list(
  list(oid = "GLUC",  std = TRUE,  f = 1 / 18.0156, un_us = "mg/dL",  un_si = "mmol/L",
       dp_us = 0, dp_si = 1, nr = list(M = c(3.9, 6.1),  "F" = c(3.9, 6.1))),
  list(oid = "CREAT", std = TRUE,  f = 88.42,       un_us = "mg/dL",  un_si = "umol/L",
       dp_us = 2, dp_si = 0, nr = list(M = c(62, 106),  "F" = c(44, 80))),
  list(oid = "HGB",   std = TRUE,  f = 10,          un_us = "g/dL",   un_si = "g/L",
       dp_us = 1, dp_si = 0, nr = list(M = c(130, 170), "F" = c(120, 155))),
  list(oid = "POT",   std = FALSE, f = 1,           un_us = "mmol/L", un_si = "mmol/L",
       dp_us = 1, dp_si = 1, nr = list(M = c(3.5, 5.1),  "F" = c(3.5, 5.1))),
  list(oid = "ALT",   std = FALSE, f = 1,           un_us = "U/L",    un_si = "U/L",
       dp_us = 0, dp_si = 0, nr = list(M = c(10, 40),   "F" = c(7, 35)))
)

.LB_FIELDS <- c(
  "LBPERF", "LBPERF_RAW", "LBPERF_DECODE",
  "LBDAT", "LBDAT_RAW", "LBDAT_YYYY", "LBDAT_MM", "LBDAT_DD",
  "LBTIM", "LBTIM_RAW",
  "LBFAST", "LBFAST_RAW", "LBFAST_DECODE",
  unlist(lapply(.LB_PANEL, function(a) {
    cols <- paste0(a$oid, c("", "_RAW", "_UN"))
    if (a$std) cols <- c(cols, paste0(a$oid, c("_STD", "_STD_UN")))
    c(cols, paste0(a$oid, c("_NRLO", "_NRHI")))
  }))
)

# ---------------------------------------------------------------------------
# Subject construction
# ---------------------------------------------------------------------------

build_subjects <- function(n) {
  sid <- id_gen(310000L)
  enrol <- as.Date("2024-01-15")

  map(seq_len(n) - 1L, function(i) {   # i is 0-based, as in the original
    site <- .SITES[[i %% length(.SITES) + 1L]]
    seq  <- i + 1L
    scrn <- enrol + (as.integer(i * 3.5) + randint(0L, 3L))
    base <- scrn + randint(7L, 14L)

    status <- if (i %in% c(5L, 17L)) "SF"
              else if (i %in% c(3L, 11L, 20L)) "ET"
              else "COMPLETED"

    list(
      subjectId = sid(),
      Subject   = sprintf("%s-%03d", site[2], seq),
      site      = site,
      status    = status,
      sex       = choice(c("1", "2")),
      race      = choices(c("1", "2", "3", "5"), c(0.7, 0.12, 0.12, 0.06)),
      ethnic    = choices(c("1", "2", "3"), c(0.15, 0.8, 0.05)),
      arm       = c("1", "2", "3")[i %% 3L + 1L],
      birth     = as.Date(sprintf("%04d-%02d-%02d", randint(1948L, 1996L),
                                  randint(1L, 12L), randint(1L, 28L))),
      scrn      = scrn,
      base      = base,
      height    = round(runif(1, 152, 191), 1),
      weight_kg = round(runif(1, 54, 108), 1),
      imperial  = site[2] == "103"   # US site keys imperial units
    )
  })
}

# Actual visit dates per folder, honouring discontinuation
visit_dates <- function(sub) {
  if (sub$status == "SF") return(list(SCRN = sub$scrn))
  out <- list(SCRN = sub$scrn, BASE = sub$base)

  offsets <- c(WK02 = 14L, WK04 = 28L, WK08 = 56L, EOT = 84L)
  ord <- names(offsets)
  stop_after <- if (sub$status == "ET") choice(c("WK02", "WK04")) else NULL

  for (folder in ord) {
    if (folder == "EOT") {
      # EOT always happens, at the point of discontinuation for ET subjects
      anchor <- if (!is.null(stop_after)) offsets[[stop_after]] + randint(1L, 6L)
                else 84L + randint(-3L, 3L)
      out[["EOT"]] <- sub$base + anchor
      next
    }
    if (!is.null(stop_after) && match(folder, ord) > match(stop_after, ord)) next
    out[[folder]] <- sub$base + (offsets[[folder]] + randint(-3L, 3L))
  }
  out
}

# ---------------------------------------------------------------------------
# Population
# ---------------------------------------------------------------------------

populate <- function(subjects) {
  dm <- new_form("DM", "Demographics", .DM_FIELDS)
  vs <- new_form("VS", "Vital Signs", .VS_FIELDS)
  ae <- new_form("AE", "Adverse Events", .AE_FIELDS)
  cm <- new_form("CM", "Concomitant Medications", .CM_FIELDS)
  ex <- new_form("EX", "Study Drug Administration", .EX_FIELDS)
  ds <- new_form("DS", "End of Study", .DS_FIELDS)
  lb <- new_form("LB", "Laboratory", .LB_FIELDS)

  for (idx in seq_along(subjects) - 1L) {
    sub <- subjects[[idx + 1L]]
    vdates <- visit_dates(sub)

    # ---- DM ---------------------------------------------------------------
    ic_date <- sub$scrn - randint(0L, 3L)
    birth_missing <- if (idx == 2L) "day" else if (idx == 9L) "monthday" else "none"

    # Non-standard DM fields -> SUPPDM. Derived deterministically so the RNG
    # stream (and every downstream domain) is unchanged by adding them.
    #   SUBJINIT  three letters, keyed off the internal subject id
    #   RACEOTH   free text, only when RACE is OTHER (idx 6 is forced to OTHER
    #             so there is always at least one to map)
    #   CHILDPOT  collected for women only; "N" once past childbearing age
    init_n   <- sum(utf8ToInt(sub$subjectId))
    subjinit <- paste0(LETTERS[(init_n * c(3L, 7L, 13L)) %% 26L + 1L], collapse = "")

    race_code <- if (idx == 6L) "5" else sub$race
    raceoth <- if (race_code == "5") {
      c("MIDDLE EASTERN", "MIXED WHITE AND ASIAN",
        "NORTH AFRICAN")[idx %% 3L + 1L]
    } else ""

    age_scrn <- as.integer(format(sub$scrn, "%Y")) -
      as.integer(format(sub$birth, "%Y"))
    childpot <- if (sub$sex == "2") if (age_scrn < 51L) "1" else "0" else ""

    f <- c(
      date_cols("ICDAT", ic_date),
      date_cols("BRTHDAT", sub$birth, birth_missing),
      list(SUBJINIT = subjinit),
      coded_cols("SEX", "SEX", sub$sex),
      coded_cols("RACE", "RACE", race_code),
      list(RACEOTH = raceoth),
      coded_cols("ETHNIC", "ETHNIC", sub$ethnic),
      coded_cols("CHILDPOT", "YN", childpot)
    )
    if (sub$status == "SF") {
      f <- c(f, coded_cols("ARMCD", "ARM", ""),
             date_cols("RANDDAT", as.Date(NA), "all"))
    } else {
      f <- c(f, coded_cols("ARMCD", "ARM", sub$arm),
             date_cols("RANDDAT", sub$base))
    }
    form_add(dm, sub, "SCRN", sub$scrn, f)

    # ---- VS -------------------------------------------------------------
    for (folder in .VISIT_FOLDERS) {
      if (is.null(vdates[[folder]])) next
      vdate <- vdates[[folder]]

      # one visit not performed
      if (idx == 7L && folder == "WK04") {
        form_add(vs, sub, folder, vdate,
                 c(coded_cols("VSPERF", "YN", "0"), date_cols("VSDAT", vdate)))
        next
      }

      drift     <- rnorm(1, 0, 1)
      sys_bp    <- round(120 + rnorm(1, 0, 11) + drift)
      dia_bp    <- round(76 + rnorm(1, 0, 8) + drift)
      pulse     <- round(70 + rnorm(1, 0, 9))
      temp_c    <- round(36.6 + rnorm(1, 0, 0.35), 1)
      weight_kg <- round(sub$weight_kg + rnorm(1, 0, 1.2), 1)

      f <- c(coded_cols("VSPERF", "YN", "1"), date_cols("VSDAT", vdate))
      vstim <- sprintf("%02d:%s", randint(8L, 15L), choice(c("00", "15", "30", "45")))
      f[["VSTIM"]] <- vstim
      f[["VSTIM_RAW"]] <- vstim
      f <- c(f, coded_cols("VSPOS", "POS", choices(c("1", "2"), c(0.85, 0.15))))

      f[["SYSBP"]] <- as.character(sys_bp)
      f[["SYSBP_RAW"]] <- as.character(sys_bp)
      f[["SYSBP_UN"]] <- "mmHg"
      f[["DIABP"]] <- as.character(dia_bp)
      f[["DIABP_RAW"]] <- as.character(dia_bp)
      f[["DIABP_UN"]] <- "mmHg"
      f[["PULSE"]] <- as.character(pulse)
      f[["PULSE_RAW"]] <- as.character(pulse)
      f[["PULSE_UN"]] <- "beats/min"

      if (sub$imperial) {
        temp_f    <- round(temp_c * 9 / 5 + 32, 1)
        weight_lb <- round(weight_kg * 2.20462, 1)
        f[["TEMP"]] <- d1(temp_f); f[["TEMP_RAW"]] <- d1(temp_f)
        f[["TEMP_UN"]] <- "F"
        f[["TEMP_STD"]] <- d1(temp_c); f[["TEMP_STD_UN"]] <- "C"
        f[["WEIGHT"]] <- d1(weight_lb); f[["WEIGHT_RAW"]] <- d1(weight_lb)
        f[["WEIGHT_UN"]] <- "lb"
        f[["WEIGHT_STD"]] <- d1(weight_kg); f[["WEIGHT_STD_UN"]] <- "kg"
      } else {
        f[["TEMP"]] <- d1(temp_c); f[["TEMP_RAW"]] <- d1(temp_c)
        f[["TEMP_UN"]] <- "C"
        f[["TEMP_STD"]] <- d1(temp_c); f[["TEMP_STD_UN"]] <- "C"
        f[["WEIGHT"]] <- d1(weight_kg); f[["WEIGHT_RAW"]] <- d1(weight_kg)
        f[["WEIGHT_UN"]] <- "kg"
        f[["WEIGHT_STD"]] <- d1(weight_kg); f[["WEIGHT_STD_UN"]] <- "kg"
      }

      # height collected at screening only
      if (folder == "SCRN") {
        if (sub$imperial) {
          height_in <- round(sub$height / 2.54, 1)
          f[["HEIGHT"]] <- d1(height_in); f[["HEIGHT_RAW"]] <- d1(height_in)
          f[["HEIGHT_UN"]] <- "in"
        } else {
          f[["HEIGHT"]] <- d1(sub$height); f[["HEIGHT_RAW"]] <- d1(sub$height)
          f[["HEIGHT_UN"]] <- "cm"
        }
        f[["HEIGHT_STD"]] <- d1(sub$height); f[["HEIGHT_STD_UN"]] <- "cm"
      }

      form_add(vs, sub, folder, vdate, f)

      # one duplicated-then-deleted vitals record with an impossible SYSBP
      if (idx == 1L && folder == "BASE") {
        bad <- f
        bad[["SYSBP"]] <- "1200"
        bad[["SYSBP_RAW"]] <- "1200"
        form_add(vs, sub, folder, vdate, bad, active = "0", page_repeat = 1L)
      }
    }

    # ---- EX -----------------------------------------------------------
    if (sub$status != "SF") {
      dm2  <- list("1" = c("PLACEBO", "0"), "2" = c("SYN-101", "50"),
                   "3" = c("SYN-101", "100"))[[sub$arm]]
      trt  <- dm2[1]
      dose <- dm2[2]
      nxt_of <- c(BASE = "WK02", WK02 = "WK04", WK04 = "WK08", WK08 = "EOT")

      for (folder in c("BASE", "WK02", "WK04", "WK08")) {
        if (is.null(vdates[[folder]])) next
        start <- vdates[[folder]]
        nxt <- nxt_of[[folder]]
        end <- (if (!is.null(vdates[[nxt]])) vdates[[nxt]] else start + 13L) - 1L
        # Non-standard EX field -> SUPPEX. First dose is given in clinic under
        # observation; the subject self-administers the rest at home.
        admby <- if (folder == "BASE") "STUDY STAFF" else "SUBJECT"
        f <- c(
          coded_cols("EXOCCUR", "YN", "1"),
          list(EXTRT = trt, EXDOSE = dose, EXDOSE_RAW = dose, EXDOSU = "mg"),
          coded_cols("EXROUTE", "ROUTE", "1"),
          date_cols("EXSTDAT", start),
          date_cols("EXENDAT", end),
          list(EXADMBY = admby)
        )
        form_add(ex, sub, folder, start, f)
      }
    }

    # ---- AE (log form) ------------------------------------------------
    if (sub$status != "SF") {
      n_ae <- choices(0:4, c(0.15, 0.3, 0.3, 0.17, 0.08))
      for (pos in seq_len(n_ae)) {
        trip <- choice(.AE_DICTIONARY)
        verbatim <- trip[1]; pt <- trip[2]; soc <- trip[3]
        st <- sub$base + randint(0L, 80L)
        ongoing <- runif(1) < 0.2
        serious <- runif(1) < 0.07
        en <- if (ongoing) as.Date(NA) else st + randint(1L, 20L)
        st_missing <- if (runif(1) < 0.08) "day" else "none"
        sev <- choices(c("1", "2", "3"), c(0.6, 0.3, 0.1))

        # Non-standard AE fields -> SUPPAE, derived from the draws already made
        # so the RNG stream is unchanged: injection-site reactions are the
        # protocol's events of special interest; a severe or serious event is
        # taken to have led to discontinuation.
        aesi     <- if (pt %in% c("Injection site pain",
                                  "Injection site erythema")) "1" else "0"
        aediscon <- if (sev == "3" || serious) "1" else "0"

        # Free-text investigator note -> CO domain (not SUPP). Recorded only for
        # the events that warranted a comment; blank otherwise.
        aecomnt <- if (aediscon == "1") {
          sprintf(paste("%s led to study drug discontinuation; investigator",
                        "judged continuation unsafe and followed the subject",
                        "to outcome."), str_to_sentence(pt))
        } else if (aesi == "1") {
          sprintf(paste("%s flagged as an event of special interest and",
                        "reported on an expedited basis per protocol."),
                  str_to_sentence(pt))
        } else ""

        f <- c(
          # trailing whitespace + case noise, as in real EDC free text
          list(AETERM = paste0(toupper(verbatim), if (pos %% 3L == 0L) "  " else ""),
               AETERM_RAW = verbatim, AECOD_PT = pt, AECOD_SOC = soc),
          date_cols("AESTDAT", st, st_missing),
          date_cols("AEENDAT", en, if (ongoing) "all" else "none"),
          coded_cols("AEONG", "YN", if (ongoing) "1" else "0"),
          coded_cols("AESEV", "SEV", sev),
          coded_cols("AESER", "YN", if (serious) "1" else "0"),
          coded_cols("AEREL", "REL",
                     choices(c("1", "2", "3", "4"), c(0.5, 0.3, 0.15, 0.05))),
          coded_cols("AEACN", "ACN",
                     choices(c("1", "2", "3", "4"), c(0.75, 0.1, 0.1, 0.05))),
          coded_cols("AEOUT", "OUT",
                     if (ongoing) "3" else choices(c("1", "2"), c(0.85, 0.15))),
          coded_cols("AESI", "YN", aesi),
          coded_cols("AEDISCON", "YN", aediscon),
          list(AECOMNT = aecomnt)
        )
        form_add(ae, sub, "LOG", st, f, record_position = pos, page_repeat = 0L)
      }
    }

    # ---- CM (log form) ----------------------------------------------
    n_cm <- choices(0:3, c(0.25, 0.35, 0.25, 0.15))
    for (pos in seq_len(n_cm)) {
      r <- choice(.CM_DICTIONARY)
      trt <- r[1]; coded <- r[2]; indc <- r[3]
      dose <- r[4]; unit <- r[5]; route_idx <- r[6]; frq <- r[7]
      st <- sub$scrn - randint(0L, 900L)
      ongoing <- runif(1) < 0.55
      en <- if (ongoing) as.Date(NA) else st + randint(5L, 400L)
      st_missing <- choices(c("none", "day", "monthday"), c(0.6, 0.25, 0.15))

      f <- c(
        list(CMTRT = toupper(trt), CMTRT_RAW = trt, CMCOD = coded, CMINDC = indc),
        date_cols("CMSTDAT", st, st_missing),
        date_cols("CMENDAT", en, if (ongoing) "all" else "none"),
        coded_cols("CMONG", "YN", if (ongoing) "1" else "0"),
        list(CMDOSE = dose, CMDOSE_RAW = dose, CMDOSU = unit),
        coded_cols("CMROUTE", "ROUTE", route_idx),
        coded_cols("CMFRQ", "FRQ", frq)
      )
      form_add(cm, sub, "LOG", NA, f, record_position = pos)
    }

    # ---- DS ---------------------------------------------------------
    if (sub$status == "SF") {
      term <- "Screen failure - inclusion criterion 4 not met"
      f <- c(
        coded_cols("DSCOMP", "YN", "0"),
        date_cols("DSSTDAT", sub$scrn + 2L),
        coded_cols("DSREAS", "DSREAS", "5"),
        list(DSTERM = term, DSTERM_RAW = term)
      )
      form_add(ds, sub, "SCRN", sub$scrn + 2L, f)
    } else {
      completed <- sub$status == "COMPLETED"
      reason <- if (completed) "1" else choice(c("2", "3", "4"))
      term <- str_to_title(.CODELISTS[["DSREAS"]][[reason]])
      f <- c(
        coded_cols("DSCOMP", "YN", if (completed) "1" else "0"),
        date_cols("DSSTDAT", vdates[["EOT"]]),
        coded_cols("DSREAS", "DSREAS", reason),
        list(DSTERM = term, DSTERM_RAW = term)
      )
      form_add(ds, sub, "EOT", vdates[["EOT"]], f)
    }

    # ---- LB (central lab; a screen failure still has a screening draw) ---
    # Placed last in the subject loop so the other domains' random draws are
    # unaffected by adding this panel.
    sex_l <- if (sub$sex == "1") "M" else "F"
    for (folder in .LB_VISIT_FOLDERS) {
      if (is.null(vdates[[folder]])) next
      ldate <- vdates[[folder]]

      # one lab panel not performed
      if (idx == 10L && folder == "WK08") {
        form_add(lb, sub, folder, ldate,
                 c(coded_cols("LBPERF", "YN", "0"), date_cols("LBDAT", ldate)))
        next
      }

      fasting <- folder %in% c("SCRN", "BASE") || runif(1) < 0.9
      f <- c(coded_cols("LBPERF", "YN", "1"), date_cols("LBDAT", ldate))
      ltim <- sprintf("%02d:%s", randint(7L, 10L), choice(c("00", "15", "30", "45")))
      f[["LBTIM"]] <- ltim
      f[["LBTIM_RAW"]] <- ltim
      f <- c(f, coded_cols("LBFAST", "YN", if (fasting) "1" else "0"))

      for (a in .LB_PANEL) {
        rng <- a$nr[[sex_l]]
        val_si <- rnorm(1, mean(rng), diff(rng) / 5)
        # one forced grade-3 transaminase rise so LBNRIND has a sure HIGH
        if (a$oid == "ALT" && idx == 4L && folder == "WK04") val_si <- 40 * 3.1

        if (a$std && sub$imperial) {          # US site keys conventional units
          orr  <- val_si / a$f; un_o <- a$un_us; dp_o <- a$dp_us
          lo_o <- rng[1] / a$f; hi_o <- rng[2] / a$f
        } else {                              # EU sites key SI directly
          orr  <- val_si;       un_o <- a$un_si; dp_o <- a$dp_si
          lo_o <- rng[1];       hi_o <- rng[2]
        }
        f[[a$oid]]                  <- dfmt(orr, dp_o)
        f[[paste0(a$oid, "_RAW")]]  <- dfmt(orr, dp_o)
        f[[paste0(a$oid, "_UN")]]   <- un_o
        f[[paste0(a$oid, "_NRLO")]] <- dfmt(lo_o, dp_o)
        f[[paste0(a$oid, "_NRHI")]] <- dfmt(hi_o, dp_o)
        if (a$std) {                          # the EDC-configured conversion
          f[[paste0(a$oid, "_STD")]]    <- dfmt(val_si, a$dp_si)
          f[[paste0(a$oid, "_STD_UN")]] <- a$un_si
        }
      }
      form_add(lb, sub, folder, ldate, f)
    }
  }

  list(DM = dm, VS = vs, LB = lb, AE = ae, CM = cm, EX = ex, DS = ds)
}

# ---------------------------------------------------------------------------
# Clinical View Metadata + CodeLists
# ---------------------------------------------------------------------------

.HEADER_META <- list(
  userid = c("User ID", "text", "12"),
  projectid = c("Project ID", "text", "12"),
  project = c("Project Name", "text", "64"),
  studyid = c("Study ID", "text", "12"),
  environmentName = c("Environment Name", "text", "32"),
  subjectId = c("Subject ID (internal)", "text", "12"),
  StudySiteId = c("Study Site ID", "text", "12"),
  Subject = c("Subject Name", "text", "32"),
  siteid = c("Site ID", "text", "12"),
  Site = c("Site Name", "text", "128"),
  SiteNumber = c("Site Number", "text", "32"),
  SiteGroup = c("Site Group", "text", "64"),
  instanceId = c("Folder Instance ID", "text", "12"),
  InstanceName = c("Folder Instance Name", "text", "128"),
  InstanceRepeatNumber = c("Instance Repeat Number", "integer", "8"),
  folderid = c("Folder ID", "text", "12"),
  Folder = c("Folder OID", "text", "32"),
  FolderName = c("Folder Name", "text", "128"),
  FolderSeq = c("Folder Sequence", "float", "8"),
  TargetDays = c("Target Days", "integer", "8"),
  DataPageId = c("Data Page ID", "text", "12"),
  DataPageName = c("Data Page Name", "text", "128"),
  PageRepeatNumber = c("Page Repeat Number", "integer", "8"),
  RecordDate = c("Record Date", "datetime", "20"),
  RecordId = c("Record ID", "text", "12"),
  recordposition = c("Record Position (log line)", "integer", "8"),
  RecordActive = c("Record Active Flag", "integer", "8"),
  SaveTs = c("Save Timestamp", "datetime", "20"),
  MinCreated = c("Minimum Created Timestamp", "datetime", "20"),
  MaxUpdated = c("Maximum Updated Timestamp", "datetime", "20")
)

.FIELD_LABELS <- list(
  ICDAT = "Date of Informed Consent",
  BRTHDAT = "Date of Birth",
  SUBJINIT = "Subject Initials",
  SEX = "Sex", RACE = "Race", ETHNIC = "Ethnicity",
  RACEOTH = "Race, Other Specify",
  CHILDPOT = "Childbearing Potential",
  ARMCD = "Randomised Treatment Arm",
  RANDDAT = "Date of Randomisation",
  DTHFL = "Death",
  DTHDAT = "Date of Death",
  VSPERF = "Vital Signs Performed",
  VSDAT = "Date of Vital Signs",
  VSTIM = "Time of Vital Signs",
  VSPOS = "Position of Subject",
  SYSBP = "Systolic Blood Pressure",
  DIABP = "Diastolic Blood Pressure",
  PULSE = "Pulse Rate", TEMP = "Temperature",
  WEIGHT = "Weight", HEIGHT = "Height",
  AETERM = "Adverse Event Verbatim Term",
  AECOD_PT = "AE Coded Preferred Term",
  AECOD_SOC = "AE Coded System Organ Class",
  AESTDAT = "AE Start Date", AEENDAT = "AE End Date",
  AEONG = "AE Ongoing", AESEV = "AE Severity",
  AESER = "AE Serious", AEREL = "AE Relationship to Study Drug",
  AEACN = "Action Taken with Study Drug", AEOUT = "AE Outcome",
  AESI = "Adverse Event of Special Interest",
  AEDISCON = "AE Led to Study Discontinuation",
  AECOMNT = "Investigator Comment on Adverse Event",
  CMTRT = "Medication Verbatim Term",
  CMCOD = "Medication Coded Term", CMINDC = "Indication",
  CMSTDAT = "Medication Start Date", CMENDAT = "Medication End Date",
  CMONG = "Medication Ongoing", CMDOSE = "Dose", CMDOSU = "Dose Unit",
  CMROUTE = "Route of Administration", CMFRQ = "Frequency",
  CMSPID = "Linked Adverse Event Log Line",
  MHTERM = "Medical History Verbatim Term",
  MHCOD_PT = "Medical History Coded Preferred Term",
  MHCOD_SOC = "Medical History Coded System Organ Class",
  MHSTDAT = "Medical History Start Date", MHENDAT = "Medical History End Date",
  MHONG = "Medical History Ongoing",
  EXOCCUR = "Study Drug Administered", EXTRT = "Study Drug Name",
  EXDOSE = "Dose Administered", EXDOSU = "Dose Unit",
  EXROUTE = "Route of Administration",
  EXSTDAT = "Dosing Start Date", EXENDAT = "Dosing End Date",
  EXADMBY = "Dose Administered By",
  DSCOMP = "Study Completed", DSSTDAT = "Date of Disposition Event",
  DSREAS = "Reason for Discontinuation", DSTERM = "Disposition Term",
  LBPERF = "Laboratory Panel Performed",
  LBDAT = "Date of Sample Collection", LBTIM = "Time of Sample Collection",
  LBFAST = "Fasting Status",
  GLUC = "Glucose", CREAT = "Creatinine", HGB = "Hemoglobin",
  POT = "Potassium", ALT = "Alanine Aminotransferase"
)

.CODELIST_OF <- list(
  SEX = "SEX", RACE = "RACE", ETHNIC = "ETHNIC", ARMCD = "ARM",
  CHILDPOT = "YN", DTHFL = "YN",
  VSPERF = "YN", VSPOS = "POS", AEONG = "YN", AESEV = "SEV",
  AESER = "YN", AEREL = "REL", AEACN = "ACN", AEOUT = "OUT",
  AESI = "YN", AEDISCON = "YN",
  CMONG = "YN", CMROUTE = "ROUTE", CMFRQ = "FRQ", EXOCCUR = "YN",
  EXROUTE = "ROUTE", DSCOMP = "YN", DSREAS = "DSREAS",
  LBPERF = "YN", LBFAST = "YN", MHONG = "YN"
)

.SUFFIX_META <- list(
  "_RAW"    = c("(as entered)", "text", "64"),
  "_DECODE" = c("(decode)", "text", "200"),
  "_UN"     = c("(units)", "text", "32"),
  "_STD"    = c("(standard value)", "float", "8"),
  "_STD_UN" = c("(standard units)", "text", "32"),
  "_NRLO"   = c("(reference range low)", "float", "8"),
  "_NRHI"   = c("(reference range high)", "float", "8"),
  "_YYYY"   = c("(year component)", "integer", "8"),
  "_MM"     = c("(month component)", "integer", "8"),
  "_DD"     = c("(day component)", "integer", "8")
)

.FLOAT_FIELDS <- c("SYSBP", "DIABP", "PULSE", "TEMP", "WEIGHT", "HEIGHT",
                   "CMDOSE", "EXDOSE", "GLUC", "CREAT", "HGB", "POT", "ALT")

.SASFORMAT_OF <- c(date = "DATE9.", datetime = "E8601DT19.",
                   float = "BEST8.", integer = "BEST8.")

write_metadata <- function(forms, outdir) {
  suffixes <- names(.SUFFIX_META)[order(-nchar(names(.SUFFIX_META)))]
  rows <- list()

  for (nm in names(forms)) {
    e <- forms[[nm]]
    all_cols <- c(.HEADER_COLS, e$field_cols)
    for (i in seq_along(all_cols)) {
      col <- all_cols[i]

      if (!is.null(.HEADER_META[[col]])) {
        hm <- .HEADER_META[[col]]
        label <- hm[1]; vtype <- hm[2]; len <- hm[3]; codelist <- ""
      } else {
        base <- col; suffix <- ""
        for (suf in suffixes) {
          if (endsWith(col, suf) && is.null(.FIELD_LABELS[[col]])) {
            base <- substr(col, 1, nchar(col) - nchar(suf)); suffix <- suf; break
          }
        }
        base_label <- if (!is.null(.FIELD_LABELS[[base]])) .FIELD_LABELS[[base]] else base
        if (nzchar(suffix)) {
          sm <- .SUFFIX_META[[suffix]]
          label <- paste(base_label, sm[1]); vtype <- sm[2]; len <- sm[3]
        } else {
          label <- base_label
          if (endsWith(base, "DAT")) { vtype <- "date"; len <- "20" }
          else if (base %in% .FLOAT_FIELDS) { vtype <- "float"; len <- "8" }
          else if (!is.null(.CODELIST_OF[[base]])) { vtype <- "text"; len <- "8" }
          else { vtype <- "text"; len <- "200" }
        }
        codelist <- if (!is.null(.CODELIST_OF[[base]])) .CODELIST_OF[[base]] else ""
      }

      rows[[length(rows) + 1L]] <- tibble(
        ProjectName = .gen_project, FormOID = nm, FormName = e$page_name,
        VariableName = col, VariableLabel = label, VariableOrdinal = i,
        DataType = vtype, Length = as.integer(len),
        CodeListOID = codelist,
        SASFormat = unname(ifelse(is.na(.SASFORMAT_OF[vtype]), "", .SASFORMAT_OF[vtype]))
      )
    }
  }

  path <- file.path(outdir, "ClinicalViewMetadata.csv")
  write_csv(bind_rows(rows), path, quote = "all", eol = "\n", na = "")
  cat("EOF\n", file = path, append = TRUE)
  invisible(path)
}

write_codelists <- function(outdir) {
  rows <- imap(.CODELISTS, \(vals, cl) tibble(
    CodeListOID = cl,
    CodedValue  = names(vals),
    Decode      = unname(vals),
    Ordinal     = seq_along(vals)
  )) |> list_rbind()

  path <- file.path(outdir, "CodeLists.csv")
  write_csv(rows, path, quote = "all", eol = "\n", na = "")
  cat("EOF\n", file = path, append = TRUE)
  invisible(path)
}

# ---------------------------------------------------------------------------
# Deaths  (post-processing: zero draws from the main RNG stream)
# ---------------------------------------------------------------------------

# Late deaths: two completed-schedule subjects die shortly after EOT. A
# fatal AE with onset during the last dosing days, drug withdrawn, EOT
# visit done, death a couple of days later. Chosen by index like the other
# deliberate messiness (SF = 5/17 and ET = 3/11/20 must not be touched),
# and applied AFTER populate() so the main RNG stream - and every existing
# row - is untouched. Deaths after a completed schedule are also the case
# a strict treatment-emergent window keeps: onset before the last dose,
# outcome after it.
.DEATH_IDX <- c(7L, 15L)

apply_deaths <- function(forms, subjects) {
  ds_death_date <- function(row) {
    as.Date(sprintf("%s-%s-%s", row$DSSTDAT_YYYY, row$DSSTDAT_MM,
                    row$DSSTDAT_DD))
  }
  rows_of <- function(form, sub) {
    which(vapply(form$rows, \(r) identical(r$subjectId, sub$subjectId),
                 logical(1)))
  }

  for (idx in .DEATH_IDX) {
    sub <- subjects[[idx + 1L]]
    if (sub$status != "COMPLETED") {
      stop(sprintf("apply_deaths: subject %s is '%s', not COMPLETED",
                   sub$Subject, sub$status), call. = FALSE)
    }

    # Death date anchors on the subject's EOT disposition record
    ds_pos     <- rows_of(forms$DS, sub)
    death_date <- ds_death_date(forms$DS$rows[[ds_pos]]) + 2L

    # DS: the disposition event becomes the death
    forms$DS$rows[[ds_pos]] <- modifyList(forms$DS$rows[[ds_pos]], as.list(c(
      coded_cols("DSCOMP", "YN", "0"),
      date_cols("DSSTDAT", death_date),
      coded_cols("DSREAS", "DSREAS", "6"),
      list(DSTERM = "Death", DSTERM_RAW = "Death")
    )))

    # DM: collected death fields. Only these rows carry the names - every
    # other row comes back blank from form_write's alignment, the way a
    # CRF page that was never filled looks in a real extract.
    if (!"DTHFL" %in% forms$DM$field_cols) {
      forms$DM$field_cols <- c(forms$DM$field_cols,
                               "DTHFL", "DTHFL_RAW", "DTHFL_DECODE",
                               "DTHDAT", "DTHDAT_RAW",
                               "DTHDAT_YYYY", "DTHDAT_MM", "DTHDAT_DD")
    }
    dm_pos <- rows_of(forms$DM, sub)
    forms$DM$rows[[dm_pos]] <- modifyList(forms$DM$rows[[dm_pos]], as.list(c(
      coded_cols("DTHFL", "YN", "1"),
      date_cols("DTHDAT", death_date)
    )))

    # AE: the fatal event, inserted after the subject's existing log lines.
    # Relatedness tracks the arm - a death on placebo is not drug-related.
    own_ae <- rows_of(forms$AE, sub)
    pos <- if (length(own_ae) == 0L) 1L else
      max(as.integer(vapply(forms$AE$rows[own_ae], \(r) r$recordposition,
                            character(1)))) + 1L
    aerel <- if (sub$arm == "1") "1" else "3"
    onset <- death_date - 4L
    form_add(forms$AE, sub, "LOG", onset, as.list(c(
      list(AETERM = "CARDIAC ARREST", AETERM_RAW = "Cardiac arrest",
           AECOD_PT = "Cardiac arrest", AECOD_SOC = "Cardiac disorders"),
      date_cols("AESTDAT", onset),
      date_cols("AEENDAT", death_date),
      coded_cols("AEONG", "YN", "0"),
      coded_cols("AESEV", "SEV", "3"),
      coded_cols("AESER", "YN", "1"),
      coded_cols("AEREL", "REL", aerel),
      coded_cols("AEACN", "ACN", "4"),
      coded_cols("AEOUT", "OUT", "4"),
      coded_cols("AESI", "YN", "0"),
      coded_cols("AEDISCON", "YN", "1"),
      list(AECOMNT = sprintf(paste("Cardiac arrest with study drug withdrawn;",
                                   "subject died on %s despite resuscitation."),
                             format(death_date, "%d %b %Y")))
    )), record_position = as.integer(pos), page_repeat = 0L)
    if (length(own_ae) > 0L) {
      n <- length(forms$AE$rows)
      forms$AE$rows <- forms$AE$rows[
        c(seq_len(own_ae[length(own_ae)]), n, seq(own_ae[length(own_ae)] + 1L, n - 1L))]
    }
  }
  invisible(forms)
}

# ---------------------------------------------------------------------------
# RELREC link key  (post-processing: deterministic, zero RNG draws)
# ---------------------------------------------------------------------------

# A CM whose indication matches one of the subject's adverse events treated
# that event: stamp the AE's log line on the CM record (CMSPID), which the
# RELREC mapper turns into a RELREC pair. First match wins, in row order,
# and each AE is linked at most once - all deterministic.
.RELREC_INDICATIONS <- c("HEADACHE", "BACK PAIN")

apply_relrec <- function(forms) {
  if (!"CMSPID" %in% forms$CM$field_cols) {
    forms$CM$field_cols <- c(forms$CM$field_cols, "CMSPID")
  }

  linked_ae <- character(0)   # recordposition values already used
  cm_rows <- seq_along(forms$CM$rows)
  for (i in cm_rows) {
    row <- forms$CM$rows[[i]]
    indc <- toupper(str_squish(if (is.null(row$CMINDC)) "" else row$CMINDC))
    if (!indc %in% .RELREC_INDICATIONS) next
    hit <- vapply(forms$AE$rows, \(r) {
      identical(r$subjectId, row$subjectId) &&
        identical(toupper(r$AECOD_PT), indc) &&
        !(r$recordposition %in% linked_ae)
    }, logical(1))
    if (!any(hit)) next
    ae_row <- forms$AE$rows[[which(hit)[[1L]]]]
    forms$CM$rows[[i]] <- modifyList(row, list(CMSPID = ae_row$recordposition))
    linked_ae <- c(linked_ae, ae_row$recordposition)
  }
  invisible(forms)
}

# ---------------------------------------------------------------------------
# Medical History  (own form, own RNG stream)
# ---------------------------------------------------------------------------

# MH is drawn in its own pass, not inside populate()'s subject loop: a new
# block mid-loop would consume main-stream draws and silently move every
# later subject's data. Instead each subject gets a private stream seeded
# from the MH seed + index; .Random.seed is restored afterwards, so the main
# stream - and every pre-existing row - is byte-identical.
apply_mh <- function(forms, subjects) {
  mh <- new_form("MH", "Medical History", .MH_FIELDS)

  for (idx in seq_along(subjects) - 1L) {
    sub <- subjects[[idx + 1L]]
    seed_hold <- .Random.seed
    set.seed(.gen_mh_seed + idx)

    # Medical history is collected at screening for everyone, screen
    # failures included; start dates reach back months or years and are
    # often only partly known.
    n_mh <- choices(0:3, c(0.2, 0.4, 0.25, 0.15))
    for (pos in seq_len(n_mh)) {
      trip <- choice(.MH_DICTIONARY)
      verbatim <- trip[1]; pt <- trip[2]; soc <- trip[3]
      st <- sub$scrn - randint(200L, 3600L)
      ongoing <- runif(1) < 0.45
      en <- if (ongoing) as.Date(NA) else st + randint(100L, 1500L)
      st_missing <- choices(c("none", "day", "monthday"), c(0.5, 0.3, 0.2))

      f <- c(
        list(MHTERM = toupper(verbatim), MHTERM_RAW = verbatim,
             MHCOD_PT = pt, MHCOD_SOC = soc),
        date_cols("MHSTDAT", st, st_missing),
        date_cols("MHENDAT", en, if (ongoing) "all" else "none"),
        coded_cols("MHONG", "YN", if (ongoing) "1" else "0")
      )
      form_add(mh, sub, "LOG", NA, f, record_position = pos)
    }

    assign(".Random.seed", seed_hold, envir = globalenv())
  }
  forms$MH <- mh
  invisible(forms)
}

#' Generate a synthetic EDC clinical view extract
#'
#' Writes a fake "Regular" clinical view extract: one quoted-CSV file per
#' CRF form (DM, VS, LB, AE, CM, EX, DS, MH) plus `ClinicalViewMetadata.csv`
#' (the data dictionary) and `CodeLists.csv`. Every file ends with an EOF
#' marker row, the way a real export guards against truncation.
#'
#' The output is stochastic but seeded: with the same `seed` and `n` the
#' bytes are identical, which is what the package's regression tests stand on.
#' The data is deliberately messy in specific, documented ways (mixed units,
#' partial dates, ongoing records, screen failures, a soft-deleted row, a
#' fatal AE) so the SDTM/ADaM mappers exercise real-world shapes.
#'
#' @param out Directory to write the extract into; created if missing.
#' @param seed Random seed. The default regenerates the SYNTH01 extract the
#'   package's reference outputs were built from.
#' @param n Number of subjects to generate.
#' @return The form environments built, invisibly.
#' @export
generate_rave_extract <- function(out, seed = 20260903, n = 24) {
  set.seed(seed)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)

  subjects <- build_subjects(n)
  forms <- populate(subjects)
  # apply_deaths/apply_relrec mutate form environments (references); MH is
  # a new form, so its pass returns the list and the assignment is required.
  apply_deaths(forms, subjects)
  apply_relrec(forms)
  forms <- apply_mh(forms, subjects)

  for (e in forms) form_write(e, out)
  write_metadata(forms, out)
  write_codelists(out)

  for (nm in names(forms)) {
    message(sprintf("  %s.csv: %d rows, %d cols", nm, length(forms[[nm]]$rows),
                    length(.HEADER_COLS) + length(forms[[nm]]$field_cols)))
  }
  message(sprintf("Rave extract written to %s (seed %d, n %d)", out, seed, n))
  invisible(forms)
}

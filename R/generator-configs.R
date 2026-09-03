# ============================================================================
# Title:   Per-study generator configs
# Purpose: Everything about the synthetic studies the generator needs beyond
#          the shared CRF family: study constants, sites, folders, schedule,
#          arms, dosing, codelist decodes, medical dictionaries, lab panel,
#          per-study CRF extras, and where the deliberate messiness lands.
#          The generator is CRF-family-shaped; these configs are the
#          study-shaped part. SYNTH01 resolves to the module constants in
#          generate-rave-extract.R verbatim, so its RNG stream - and the
#          committed reference outputs - are untouched by this layer.
# ============================================================================

#' (internal) Select a study's generator config
#'
#' @param study Study name, e.g. "SYNTH01"
#' @return The config list consumed by the generator internals.
#' @keywords internal
.gen_cfg <- function(study = "SYNTH01") {
  switch(study,
    SYNTH01 = .cfg_synth01(),
    SYNTH02 = .cfg_synth02(),
    stop(sprintf(paste("generate_rave_extract: unknown study '%s'",
                       "(known: SYNTH01, SYNTH02)"), study), call. = FALSE)
  )
}

# The shipped reference study. Every element points at the module constants,
# so refactoring the generator cannot move SYNTH01's RNG stream.
.cfg_synth01 <- function() {
  list(
    seed = 20260903, n = 24L,
    project = .gen_project, project_id = .gen_project_id,
    studyid = .gen_studyid, environment = .gen_environment,
    user_id = .gen_user_id, mh_seed = .gen_mh_seed,

    sites = .SITES,
    folders = .FOLDERS,
    visit_folders = .VISIT_FOLDERS,
    visit_offsets = c(WK02 = 14L, WK04 = 28L, WK08 = 56L, EOT = 84L),
    et_stop = c("WK02", "WK04"),
    dosing_folders = c("BASE", "WK02", "WK04", "WK08"),
    lab_folders = .LB_VISIT_FOLDERS,

    codelists = .CODELISTS,
    doses = list("1" = c("PLACEBO", "0"), "2" = c("SYN-101", "50"),
                 "3" = c("SYN-101", "100")),
    ae_dict = .AE_DICTIONARY,
    cm_dict = .CM_DICTIONARY,
    mh_dict = .MH_DICTIONARY,
    aesi_terms = c("Injection site pain", "Injection site erythema"),

    lb_panel = .LB_PANEL,
    fields = list(DM = .DM_FIELDS, VS = .VS_FIELDS, AE = .AE_FIELDS,
                  CM = .CM_FIELDS, MH = .MH_FIELDS, EX = .EX_FIELDS,
                  DS = .DS_FIELDS, LB = .LB_FIELDS),
    field_labels = .FIELD_LABELS,
    codelist_of = .CODELIST_OF,
    float_fields = .FLOAT_FIELDS,

    imperial_site = "103",
    enrol = as.Date("2024-01-15"),
    birth_years = c(1948L, 1996L),
    subject_id_base = 310000L,
    sf_term = "Screen failure - inclusion criterion 4 not met",
    disc_reasons = c("2", "3", "4"),
    relrec_indications = .RELREC_INDICATIONS,
    vs_extra = NULL,
    ex_lot = NULL,

    idx = list(
      birth_day = 2L, birth_monthday = 9L,
      sf = c(5L, 17L), et = c(3L, 11L, 20L),
      race_other = 6L,
      vs_np = 7L, vs_np_folder = "WK04",
      vs_dup = 1L,
      lb_np = 10L, lb_np_folder = "WK08",
      force_high = list(analyte = "ALT", idx = 4L, folder = "WK04",
                        value = 40 * 3.1),
      deaths = c(7L, 15L)
    )
  )
}

# The second study: same CRF family, different everything else. Different
# sponsor ids, three European sites (the UK one keys imperial units), four
# arms of SYN-201, a longer schedule without the Week 2 visit (Week 12
# instead), a different discontinuation reason list (adds PHYSICIAN
# DECISION), a causality decode kept on the 4-point scale in the spec, a
# respiratory rate collected on the VS form, an AST analyte in the central
# lab panel, a lot-number SUPP qualifier on EX, its own dictionaries and its
# own messiness indexes.
.cfg_synth02 <- function() {
  # The CRF family's field metadata is shared; this study adds two fields.
  field_labels <- utils::modifyList(.FIELD_LABELS, list(
    RESP  = "Respiration Rate",
    EXLOT = "Lot Number"
  ))
  float_fields <- union(.FLOAT_FIELDS, "RESP")

  lb_panel <- list(
    list(oid = "GLUC",  std = TRUE,  f = 1 / 18.0156, un_us = "mg/dL",
         un_si = "mmol/L", dp_us = 0, dp_si = 1,
         nr = list(M = c(3.9, 6.1), "F" = c(3.9, 6.1))),
    list(oid = "CREAT", std = TRUE,  f = 88.42, un_us = "mg/dL",
         un_si = "umol/L", dp_us = 2, dp_si = 0,
         nr = list(M = c(62, 106), "F" = c(44, 80))),
    list(oid = "HGB",   std = TRUE,  f = 10, un_us = "g/dL", un_si = "g/L",
         dp_us = 1, dp_si = 0, nr = list(M = c(130, 170), "F" = c(120, 155))),
    list(oid = "POT",   std = FALSE, f = 1, un_us = "mmol/L", un_si = "mmol/L",
         dp_us = 1, dp_si = 1, nr = list(M = c(3.5, 5.1), "F" = c(3.5, 5.1))),
    list(oid = "ALT",   std = FALSE, f = 1, un_us = "U/L", un_si = "U/L",
         dp_us = 0, dp_si = 0, nr = list(M = c(10, 40), "F" = c(7, 35))),
    list(oid = "AST",   std = FALSE, f = 1, un_us = "U/L", un_si = "U/L",
         dp_us = 0, dp_si = 0, nr = list(M = c(10, 45), "F" = c(7, 38)))
  )

  # the family's VS field block, with RESP (no _STD, like the BPs) after pulse
  vs_fields <- local({
    i <- match("PULSE_UN", .VS_FIELDS)
    append(.VS_FIELDS, c("RESP", "RESP_RAW", "RESP_UN"), after = i)
  })

  list(
    seed = 20260904, n = 18L,
    project = "SYNTH02", project_id = "1593",
    studyid = "4033", environment = "Prod",
    user_id = "9935", mh_seed = 20260904L,

    sites = list(
      c("4201", "201", "Hopital Saint-Louis",                 "Europe", "8801"),
      c("4202", "202", "Hospital Universitari Vall d'Hebron", "Europe", "8802"),
      c("4203", "203", "Guy's Hospital",                      "Europe", "8803")
    ),
    folders = list(
      SCRN = c("6001", "SCRN", "Screening",        "Screening",        "1.0",  "-14"),
      BASE = c("6002", "BASE", "Baseline",         "Baseline",         "2.0",  "0"),
      WK04 = c("6003", "WK04", "Week 4",           "Week 4",           "3.0",  "28"),
      WK08 = c("6004", "WK08", "Week 8",           "Week 8",           "4.0",  "56"),
      WK12 = c("6005", "WK12", "Week 12",          "Week 12",          "5.0",  "84"),
      EOT  = c("6006", "EOT",  "End of Treatment", "End of Treatment", "6.0",  "112"),
      LOG  = c("6900", "LOG",  "Log Forms",        "Log Forms",        "99.0", "")
    ),
    visit_folders = c("SCRN", "BASE", "WK04", "WK08", "WK12", "EOT"),
    visit_offsets = c(WK04 = 28L, WK08 = 56L, WK12 = 84L, EOT = 112L),
    et_stop = c("WK04", "WK08"),
    dosing_folders = c("BASE", "WK04", "WK08", "WK12"),
    lab_folders = c("SCRN", "BASE", "WK08", "EOT"),

    codelists = utils::modifyList(.CODELISTS, list(
      DSREAS = c(.CODELISTS$DSREAS, "7" = "PHYSICIAN DECISION"),
      ARM = c("1" = "Placebo", "2" = "SYN-201 25 mg",
              "3" = "SYN-201 50 mg", "4" = "SYN-201 100 mg")
    )),
    doses = list("1" = c("PLACEBO", "0"), "2" = c("SYN-201", "25"),
                 "3" = c("SYN-201", "50"), "4" = c("SYN-201", "100")),
    ae_dict = list(
      c("Headache", "Headache", "Nervous system disorders"),
      c("Head ache", "Headache", "Nervous system disorders"),
      c("Nausea", "Nausea", "Gastrointestinal disorders"),
      c("Fatigue", "Fatigue",
        "General disorders and administration site conditions"),
      c("Injection site pain", "Injection site pain",
        "General disorders and administration site conditions"),
      c("Injection site erythema", "Injection site erythema",
        "General disorders and administration site conditions"),
      c("Pyrexia", "Pyrexia",
        "General disorders and administration site conditions"),
      c("Dizziness", "Dizziness", "Nervous system disorders"),
      c("Arthralgia", "Arthralgia",
        "Musculoskeletal and connective tissue disorders"),
      c("Upper respiratory tract infection", "Upper respiratory tract infection",
        "Infections and infestations"),
      c("Nasopharyngitis", "Nasopharyngitis", "Infections and infestations"),
      c("Oropharyngeal pain", "Oropharyngeal pain", "Infections and infestations")
    ),
    cm_dict = list(
      c("Paracetamol", "PARACETAMOL", "Headache", "500", "mg", "1", "3"),
      c("Naproxen", "NAPROXEN", "Arthralgia", "250", "mg", "1", "2"),
      c("Amlodipine", "AMLODIPINE", "Hypertension", "5", "mg", "1", "1"),
      c("Empagliflozin", "EMPAGLIFLOZIN", "Type 2 diabetes mellitus", "10", "mg", "1", "1"),
      c("Esomeprazole", "ESOMEPRAZOLE", "Gastro-oesophageal reflux disease", "20", "mg", "1", "1"),
      c("Vitamin D", "COLECALCIFEROL", "Vitamin D deficiency", "1000", "IU", "1", "1")
    ),
    mh_dict = list(
      c("High blood pressure", "Hypertension", "Vascular disorders"),
      c("Hypertension", "Hypertension", "Vascular disorders"),
      c("Type 2 diabetes", "Type 2 diabetes mellitus", "Endocrine disorders"),
      c("High cholesterol", "Hypercholesterolaemia",
        "Metabolism and nutrition disorders"),
      c("Osteoarthritis", "Osteoarthritis",
        "Musculoskeletal and connective tissue disorders"),
      c("Asthma", "Asthma", "Respiratory, thoracic and mediastinal disorders"),
      c("Underactive thyroid", "Hypothyroidism", "Endocrine disorders"),
      c("Migraine", "Migraine", "Nervous system disorders")
    ),
    aesi_terms = c("Injection site pain", "Injection site erythema"),

    lb_panel = lb_panel,
    fields = list(DM = .DM_FIELDS, VS = vs_fields, AE = .AE_FIELDS,
                  CM = .CM_FIELDS, MH = .MH_FIELDS,
                  EX = c(.EX_FIELDS, "EXLOT"),
                  DS = .DS_FIELDS, LB = .lb_fields(lb_panel)),
    field_labels = field_labels,
    codelist_of = .CODELIST_OF,
    float_fields = float_fields,

    imperial_site = "203",
    enrol = as.Date("2025-06-02"),
    birth_years = c(1952L, 2000L),
    subject_id_base = 520000L,
    sf_term = "Screen failure - exclusion criterion 2 met",
    disc_reasons = c("2", "4", "7"),
    relrec_indications = c("HEADACHE", "ARTHRALGIA"),
    vs_extra = list(field = "RESP", base = 16, sd = 2, unit = "breaths/min"),
    ex_lot = list(field = "EXLOT", prefix = "SY2-"),

    idx = list(
      birth_day = 4L, birth_monthday = 11L,
      sf = c(6L, 14L), et = c(2L, 9L),
      race_other = 5L,
      vs_np = 8L, vs_np_folder = "WK08",
      vs_dup = 3L,
      lb_np = 12L, lb_np_folder = "WK12",
      force_high = list(analyte = "AST", idx = 4L, folder = "WK08",
                        value = 45 * 3.1),
      deaths = c(10L, 16L)
    )
  )
}

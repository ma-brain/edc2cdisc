# ============================================================================
# Title:   Structural validation of the derived SDTM domains
# Purpose: Cheap, dependency-free checks that catch the mistakes this project
#          is designed to make you think about. Not a Pinnacle 21 replacement.
# Input:   data/sdtm/*.rds
# Output:  a printed issue report; stop() on any ERROR
# ============================================================================

.val_domains <- c("DM", "EX", "VS", "AE", "CM", "DS", "SV", "LB", "MH",
                  "SUPPDM", "SUPPAE", "SUPPEX", "CO", "RELREC")
sdtm <- .val_domains |>
  set_names() |>
  map(\(d) read_rds(file.path(paths$sdtm, str_c(str_to_lower(d), ".rds"))))

# Required-variable lists (a pragmatic subset of the IG, not exhaustive) -----
.req <- list(
  DM = c("STUDYID", "DOMAIN", "USUBJID", "SUBJID", "RFSTDTC", "SITEID",
         "AGE", "SEX", "RACE", "ETHNIC", "ARMCD", "ARM", "COUNTRY"),
  EX = c("STUDYID", "DOMAIN", "USUBJID", "EXSEQ", "EXTRT", "EXDOSE",
         "EXDOSU", "EXSTDTC"),
  VS = c("STUDYID", "DOMAIN", "USUBJID", "VSSEQ", "VSTESTCD", "VSTEST",
         "VSORRES", "VISITNUM", "VSDTC"),
  AE = c("STUDYID", "DOMAIN", "USUBJID", "AESEQ", "AETERM", "AEDECOD",
         "AESTDTC"),
  CM = c("STUDYID", "DOMAIN", "USUBJID", "CMSEQ", "CMTRT", "CMSTDTC"),
  DS = c("STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD",
         "DSSTDTC"),
  SV = c("STUDYID", "DOMAIN", "USUBJID", "VISITNUM", "VISIT", "SVSTDTC"),
  LB = c("STUDYID", "DOMAIN", "USUBJID", "LBSEQ", "LBTESTCD", "LBTEST",
         "LBORRES", "LBSTRESN", "LBSTRESU", "LBNRIND", "VISITNUM", "LBDTC"),
  MH = c("STUDYID", "DOMAIN", "USUBJID", "MHSEQ", "MHTERM", "MHDECOD",
         "MHSTDTC"),
  RELREC = c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL",
             "RELTYPE", "RELID"),
  SUPPDM = c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL",
             "QNAM", "QLABEL", "QVAL", "QORIG", "QEVAL"),
  SUPPAE = c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL",
             "QNAM", "QLABEL", "QVAL", "QORIG", "QEVAL"),
  SUPPEX = c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL",
             "QNAM", "QLABEL", "QVAL", "QORIG", "QEVAL"),
  CO = c("STUDYID", "DOMAIN", "RDOMAIN", "USUBJID", "COSEQ",
         "IDVAR", "IDVARVAL", "COVAL")
)

# ISO 8601: full or reduced precision, optional time; NA is allowed ----------
.iso_ok <- function(x) {
  is.na(x) |
    str_detect(x, "^\\d{4}(-\\d{2}(-\\d{2}(T\\d{2}:\\d{2}(:\\d{2})?)?)?)?$")
}

.issues <- list()
.add <- function(domain, severity, check, detail) {
  .issues[[length(.issues) + 1L]] <<-
    tibble(domain = domain, severity = severity, check = check, detail = detail)
}

.dm_ids <- sdtm$DM$USUBJID

for (d in names(sdtm)) {
  df <- sdtm[[d]]

  miss <- setdiff(.req[[d]], names(df))
  if (length(miss) > 0) .add(d, "ERROR", "required-vars", str_flatten_comma(miss))

  if ("DOMAIN" %in% names(df) && any(df$DOMAIN != d)) {
    .add(d, "ERROR", "domain-constant", str_c("DOMAIN != '", d, "'"))
  }

  if ("USUBJID" %in% names(df)) {
    if (anyNA(df$USUBJID)) .add(d, "ERROR", "usubjid-missing", "NA USUBJID present")
    orphan <- setdiff(df$USUBJID, .dm_ids)
    if (length(orphan) > 0) {
      .add(d, "ERROR", "usubjid-not-in-dm", str_flatten_comma(orphan))
    }
  }

  seq_var <- str_c(d, "SEQ")
  if (seq_var %in% names(df)) {
    dup <- df |>
      count(across(all_of(c("USUBJID", seq_var))), name = ".n") |>
      filter(.n > 1)
    if (nrow(dup) > 0) {
      .add(d, "ERROR", "seq-not-unique",
           sprintf("%d duplicated USUBJID/%s key(s)", nrow(dup), seq_var))
    }
  }

  for (v in names(df)[str_ends(names(df), "DTC")]) {
    bad <- unique(df[[v]][!.iso_ok(df[[v]])])
    if (length(bad) > 0) {
      .add(d, "ERROR", "dtc-not-iso8601",
           sprintf("%s: %s", v, str_flatten_comma(bad)))
    }
  }

  for (v in names(df)[str_ends(names(df), "DY")]) {
    if (any(df[[v]] == 0, na.rm = TRUE)) .add(d, "ERROR", "study-day-zero", v)
  }
}

# Domain-specific -----------------------------------------------------------
if (anyDuplicated(sdtm$DM$USUBJID) > 0) {
  .add("DM", "ERROR", "dm-one-row-per-subject", "USUBJID duplicated")
}

# At most one baseline flag per subject / test (VS and LB) ----------------
for (bl in list(c("VS", "VSBLFL", "VSTESTCD"), c("LB", "LBBLFL", "LBTESTCD"))) {
  df <- sdtm[[bl[1]]]
  if (!all(bl[2:3] %in% names(df))) next
  multi_bl <- df |>
    filter(.data[[bl[2]]] == "Y") |>
    count(across(all_of(c("USUBJID", bl[3]))), name = ".n") |>
    filter(.n > 1)
  if (nrow(multi_bl) > 0) {
    .add(bl[1], "ERROR", "baseline-flag-multi",
         sprintf("%d subject/test with >1 %s='Y'", nrow(multi_bl), bl[2]))
  }
}

# LB: LBNRIND must be populated and match the standard-unit comparison ----
lb_ind_vals <- setdiff(unique(na.omit(sdtm$LB$LBNRIND)), c("LOW", "NORMAL", "HIGH"))
if (length(lb_ind_vals) > 0) {
  .add("LB", "ERROR", "lbnrind-bad-value", str_flatten_comma(lb_ind_vals))
}
lb_ind_bad <- sdtm$LB |>
  filter(!is.na(LBSTRESN), !is.na(LBSTNRLO), !is.na(LBSTNRHI)) |>
  mutate(expect = case_when(
    LBSTRESN < LBSTNRLO ~ "LOW",
    LBSTRESN > LBSTNRHI ~ "HIGH",
    .default            = "NORMAL"
  )) |>
  filter(is.na(LBNRIND) | LBNRIND != expect)
if (nrow(lb_ind_bad) > 0) {
  .add("LB", "ERROR", "lbnrind-inconsistent",
       sprintf("%d row(s) where LBNRIND disagrees with the range comparison",
               nrow(lb_ind_bad)))
}

# LB: standardised result and range must share a unit --------------------
lb_unit_gap <- sdtm$LB |>
  filter(!is.na(LBSTRESN), LBSTRESU == "" | is.na(LBSTRESU))
if (nrow(lb_unit_gap) > 0) {
  .add("LB", "ERROR", "lbstresu-missing",
       sprintf("%d numeric result(s) with no LBSTRESU", nrow(lb_unit_gap)))
}

# SV: one row per subject per visit -------------------------------------
sv_dup <- sdtm$SV |> count(USUBJID, VISITNUM, name = ".n") |> filter(.n > 1)
if (nrow(sv_dup) > 0) {
  .add("SV", "ERROR", "sv-visit-not-unique",
       sprintf("%d duplicated USUBJID/VISITNUM key(s)", nrow(sv_dup)))
}

# VISITNUM reconciliation: the scheduled-visit domains vs SV ---------------
# SV is the reference list of visits that actually happened. Every collected
# (USUBJID, VISITNUM) must map to an SV record, and the VISITNUM -> VISIT
# decode must agree with SV.
sv_keys      <- sdtm$SV |> distinct(USUBJID, VISITNUM)
sv_visit_map <- sdtm$SV |> distinct(VISITNUM, VISIT)

for (d in c("VS", "EX", "LB")) {
  df <- sdtm[[d]]
  if (!all(c("USUBJID", "VISITNUM", "VISIT") %in% names(df))) next

  orphan <- df |>
    distinct(USUBJID, VISITNUM, VISIT) |>
    anti_join(sv_keys, by = c("USUBJID", "VISITNUM"))
  if (nrow(orphan) > 0) {
    .add(d, "ERROR", "visit-not-in-sv",
         sprintf("%d USUBJID/VISITNUM not in SV (e.g. %s VISITNUM %s)",
                 nrow(orphan), orphan$USUBJID[1], orphan$VISITNUM[1]))
  }

  clash <- df |>
    distinct(VISITNUM, VISIT) |>
    inner_join(sv_visit_map, by = "VISITNUM", suffix = c("", "_SV")) |>
    filter(VISIT != VISIT_SV)
  if (nrow(clash) > 0) {
    .add(d, "ERROR", "visitnum-decode-mismatch",
         str_flatten_comma(sprintf("%s '%s' vs SV '%s'",
                                   clash$VISITNUM, clash$VISIT, clash$VISIT_SV)))
  }
}

# Date coherence: a VS measurement should fall inside its SV visit window ---
vs_outside <- sdtm$VS |>
  filter(!is.na(VSDTC)) |>
  inner_join(select(sdtm$SV, USUBJID, VISITNUM, SVSTDTC, SVENDTC),
             by = c("USUBJID", "VISITNUM")) |>
  mutate(vd = as.Date(str_sub(VSDTC, 1, 10))) |>
  filter(vd < as.Date(SVSTDTC) | vd > as.Date(SVENDTC))
if (nrow(vs_outside) > 0) {
  .add("VS", "WARN", "vsdtc-outside-sv-window",
       sprintf("%d VS record(s) dated outside their SV visit window",
               nrow(vs_outside)))
}

# Screen failures must have no study days anywhere (no RFSTDTC) -------------
sf_ids <- sdtm$DM |> filter(ARMCD == "SCRNFAIL") |> pull(USUBJID)
for (d in c("VS", "AE", "CM", "DS", "SV", "LB", "MH")) {
  df <- sdtm[[d]]
  # VISITDY is a protocol-planned day, not anchored to RFSTDTC - exclude it.
  dy_vars <- setdiff(names(df)[str_ends(names(df), "DY")], "VISITDY")
  leaked <- df |>
    filter(USUBJID %in% sf_ids) |>
    filter(if_any(all_of(dy_vars), \(x) !is.na(x)))
  if (nrow(leaked) > 0) {
    .add(d, "ERROR", "screenfail-has-study-day",
         sprintf("%d row(s) for screen-failure subjects", nrow(leaked)))
  }
}

# SUPP-- / CO: every related record must tie back to a real parent --------
# The generic loop above already covers required vars and USUBJID in DM.
# These checks are the related-record structure: constant RDOMAIN, the
# IDVAR / IDVARVAL link back to the parent, one value per QNAM (SUPP only),
# and QORIG controlled terms (SUPP only). CO carries a COVAL string and its
# own COSEQ instead, and its comments may legitimately repeat per record.
.related <- c(SUPPDM = "DM", SUPPAE = "AE", SUPPEX = "EX", CO = "AE")
for (rel in names(.related)) {
  df <- sdtm[[rel]]
  if (is.null(df)) next
  rd      <- unname(.related[[rel]])
  parent  <- sdtm[[rd]]
  val_var <- if ("QVAL" %in% names(df)) "QVAL" else "COVAL"

  if (any(df$RDOMAIN != rd)) {
    .add(rel, "ERROR", "rdomain-constant", str_c("RDOMAIN != '", rd, "'"))
  }

  n_blank <- sum(is.na(df[[val_var]]) | df[[val_var]] == "")
  if (n_blank > 0) {
    .add(rel, "ERROR", "value-missing",
         sprintf("%d row(s) with blank %s", n_blank, val_var))
  }

  # IDVAR is either blank throughout or one consistent parent key
  idvars <- unique(df$IDVAR[!is.na(df$IDVAR) & df$IDVAR != ""])
  if (length(idvars) > 1) {
    .add(rel, "ERROR", "idvar-inconsistent", str_flatten_comma(idvars))
  }

  # SUPP: one qualifier value per parent record
  if ("QNAM" %in% names(df)) {
    dup <- df |>
      count(USUBJID, IDVAR, IDVARVAL, QNAM, name = ".n") |>
      filter(.n > 1)
    if (nrow(dup) > 0) {
      .add(rel, "ERROR", "qnam-not-unique",
           sprintf("%d duplicated USUBJID/IDVARVAL/QNAM key(s)", nrow(dup)))
    }
  }

  # Referential integrity to the parent domain
  if (length(idvars) == 0) {
    orphan <- setdiff(df$USUBJID, parent$USUBJID)
    if (length(orphan) > 0) {
      .add(rel, "ERROR", "related-parent-orphan",
           sprintf("USUBJID not in %s: %s", rd, str_flatten_comma(orphan)))
    }
  } else {
    idv <- idvars[[1]]
    parent_keys <- parent |>
      transmute(USUBJID, IDVARVAL = as.character(.data[[idv]]))
    orphan <- df |>
      distinct(USUBJID, IDVARVAL) |>
      anti_join(parent_keys, by = c("USUBJID", "IDVARVAL"))
    if (nrow(orphan) > 0) {
      .add(rel, "ERROR", "related-parent-orphan",
           sprintf("%d %s value(s) of %s with no matching %s record",
                   nrow(orphan), rel, idv, rd))
    }
  }

  # SUPP: QORIG controlled terminology
  if ("QORIG" %in% names(df)) {
    bad_orig <- setdiff(
      unique(na.omit(df$QORIG)),
      c("CRF", "DERIVED", "ASSIGNED", "PROTOCOL", "PREDECESSOR", "eDT")
    )
    if (length(bad_orig) > 0) {
      .add(rel, "ERROR", "qorig-bad-value", str_flatten_comma(bad_orig))
    }
  }
}

# Death coherence: the three layers that record a death - a fatal AE
# outcome, the DS disposition record, the collected DM death fields - must
# name exactly the same subjects, and the death dates must agree.
death_sets <- list(
  ae = sdtm$AE |> filter(AEOUT == "FATAL") |> pull(USUBJID) |> unique(),
  ds = sdtm$DS |> filter(DSDECOD == "DEATH") |> pull(USUBJID) |> unique(),
  dm = sdtm$DM |> filter(DTHFL %in% "Y") |> pull(USUBJID) |> unique()
)
death_inconsistent <- setdiff(Reduce(union, death_sets),
                              Reduce(intersect, death_sets))
if (length(death_inconsistent) > 0) {
  .add("AE", "ERROR", "death-coherence",
       sprintf(paste("%d subject(s) where fatal AE / DS DEATH / DM DTHFL",
                     "disagree: %s"), length(death_inconsistent),
               str_flatten_comma(death_inconsistent)))
}
death_dates <- sdtm$DM |>
  filter(DTHFL %in% "Y") |>
  select(USUBJID, DTHDTC) |>
  inner_join(sdtm$DS |> filter(DSDECOD == "DEATH") |> select(USUBJID, DSSTDTC),
             by = "USUBJID") |>
  filter(is.na(DTHDTC) | DTHDTC != DSSTDTC)
if (nrow(death_dates) > 0) {
  .add("DM", "ERROR", "death-date-mismatch",
       "DTHDTC disagrees with the DS DEATH record date")
}

# MH: history precedes the study - no condition's recorded onset may fall
# after first dose. ISO 8601 reduced precision compares correctly as a
# string ("2023" < "2024-03-17"), which is exactly why partial dates stay
# partial: the comparison works without imputing.
mh_after <- sdtm$MH |>
  left_join(select(sdtm$DM, USUBJID, RFSTDTC), by = "USUBJID") |>
  filter(!is.na(MHSTDTC), !is.na(RFSTDTC), MHSTDTC > RFSTDTC)
if (nrow(mh_after) > 0) {
  .add("MH", "ERROR", "mh-after-first-dose",
       sprintf("%d history record(s) starting after RFSTDTC", nrow(mh_after)))
}

# RELREC: a link is a claim about two records, so both halves must resolve --
# each RELID pairs exactly one CM record with one AE record, the IDVARs name
# the domains' sequence keys, and every IDVARVAL hits a real parent row.
relrec <- sdtm$RELREC
if (!is.null(relrec)) {
  bad_reltype <- relrec |> filter(!RELTYPE %in% c("ONE", "MANY"))
  if (nrow(bad_reltype) > 0) {
    .add("RELREC", "ERROR", "reltype-bad-value", "RELTYPE must be ONE or MANY")
  }

  rel_size <- relrec |> count(RELID, name = ".n") |> filter(.n != 2)
  if (nrow(rel_size) > 0) {
    .add("RELREC", "ERROR", "relid-not-a-pair",
         sprintf("%d RELID(s) that are not a two-record pair", nrow(rel_size)))
  }
  rel_mixed <- relrec |>
    count(RELID, RDOMAIN) |>
    count(RELID, name = ".n") |>
    filter(.n != 2)
  if (nrow(rel_mixed) > 0) {
    .add("RELREC", "ERROR", "relid-not-cm-ae",
         "every RELID must pair a CM record with an AE record")
  }
  rel_idvars <- relrec |> distinct(RDOMAIN, IDVAR)
  if (!setequal(rel_idvars$IDVAR, c("CMSEQ", "AESEQ")) ||
      nrow(rel_idvars) != 2) {
    .add("RELREC", "ERROR", "idvar-not-seq",
         "IDVAR must be CMSEQ for the CM side and AESEQ for the AE side")
  }

  for (side in list(c("CM", "CMSEQ"), c("AE", "AESEQ"))) {
    rd <- side[1]; idv <- side[2]
    parent_keys <- sdtm[[rd]] |>
      transmute(USUBJID, IDVARVAL = as.character(.data[[idv]]))
    orphans <- relrec |>
      filter(RDOMAIN == rd) |>
      anti_join(parent_keys, by = c("USUBJID", "IDVARVAL"))
    if (nrow(orphans) > 0) {
      .add("RELREC", "ERROR", "relrec-parent-orphan",
           sprintf("%d %s-side link(s) with no matching %s record",
                   nrow(orphans), rd, rd))
    }
  }
}

# Report ------------------------------------------------------------------
report <- bind_rows(.issues)
if (nrow(report) == 0) {
  message("24_validate.R: all structural checks passed (",
          length(sdtm), " domains)")
} else {
  message("24_validate.R: ", nrow(report), " issue(s)")
  print(report, n = Inf)
  n_err <- sum(report$severity == "ERROR")
  if (n_err > 0) {
    stop(sprintf("24_validate.R: %d structural error(s)", n_err), call. = FALSE)
  }
}

rm(.val_domains, .req, .iso_ok, .issues, .add, .dm_ids, sf_ids, .related)

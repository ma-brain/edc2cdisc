# ============================================================================
# Title:   Structural validation of the mapped SDTM domains
# Purpose: Cheap checks that catch the mistakes this project is designed to
#          make you think about. Not a Pinnacle 21 replacement. Ported from
#          the script-based 24_validate.R: instead of reading data/sdtm and
#          stopping, validate_sdtm() takes the mapped domains and returns an
#          issue tibble; stop_on_error() turns that into the pipeline guard.
# ============================================================================

# Required-variable lists for the domains the mapping engine does not drive
# (a pragmatic subset of the IG, not exhaustive). For DM/EX/DS/AE/CM/MH the
# required list comes from spec$variables - two things read the spec, so it
# cannot silently lie.
.sdtm_req_static <- list(
  VS = c("STUDYID", "DOMAIN", "USUBJID", "VSSEQ", "VSTESTCD", "VSTEST",
         "VSORRES", "VISITNUM", "VSDTC"),
  SV = c("STUDYID", "DOMAIN", "USUBJID", "VISITNUM", "VISIT", "SVSTDTC"),
  LB = c("STUDYID", "DOMAIN", "USUBJID", "LBSEQ", "LBTESTCD", "LBTEST",
         "LBORRES", "LBSTRESN", "LBSTRESU", "LBNRIND", "VISITNUM", "LBDTC"),
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

# ISO 8601: full or reduced precision, optional time; NA is allowed
.iso_ok <- function(x) {
  is.na(x) |
    str_detect(x, "^\\d{4}(-\\d{2}(-\\d{2}(T\\d{2}:\\d{2}(:\\d{2})?)?)?)?$")
}

.new_issue <- function(domain = character(0), severity = character(0),
                       check = character(0), detail = character(0)) {
  tibble(domain = domain, severity = severity, check = check, detail = detail)
}

.sdtm_required_vars <- function(spec, domain) {
  if (!is.null(spec) && any(spec$variables$domain == domain)) {
    out <- spec$variables$variable[spec$variables$domain == domain]
    if (domain != "DM") out <- c(out, paste0(domain, "SEQ"))
    return(out)
  }
  .sdtm_req_static[[domain]]
}

#' Validate the mapped SDTM domains
#'
#' Runs the structural checks: required variables, DOMAIN constants, USUBJID
#' referential integrity against DM, unique --SEQ keys, ISO 8601 --DTC
#' formats, no study day 0, baseline-flag uniqueness, LBNRIND coherence, SV
#' visit uniqueness, VISITNUM reconciliation against SV, screen-failure
#' study-day leaks, SUPP/CO related-record structure, death coherence across
#' AE/DS/DM, MH onset before first dose, RELREC pair integrity and - when a
#' spec is supplied - controlled-terminology values against `spec$codelists`.
#'
#' @param domains A named list of mapped SDTM datasets, as built by
#'   [build_all()] (DM, EX, VS, AE, CM, DS, SV, LB, MH, SUPPDM, SUPPAE,
#'   SUPPEX, CO, RELREC)
#' @param spec Optional `study_spec`; when given, required-variable lists
#'   for the engine-mapped domains come from `spec$variables`.
#' @return An issue tibble: domain, severity ("ERROR" / "WARN"), check,
#'   detail. Empty when everything passes.
#' @export
#' @examples
#' ext <- file.path(tempdir(), "extract-val")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' built <- build_all(ext)
#' issues <- validate_sdtm(built$sdtm, spec_synth01)
#' issues                                  # empty: the build is clean
#'
#' # the pipeline wrapper stops on any ERROR row
#' stop_on_error(issues, "SDTM validation")
validate_sdtm <- function(domains, spec = NULL) {
  issues <- list()
  add <- function(domain, severity, check, detail) {
    issues[[length(issues) + 1L]] <<- .new_issue(domain, severity, check, detail) # nolint: assignment_linter. (issue collector)
  }

  dm_ids <- domains$DM$USUBJID

  for (d in names(domains)) {
    df <- domains[[d]]

    req <- .sdtm_required_vars(spec, d)
    miss <- setdiff(req, names(df))
    if (length(miss) > 0) add(d, "ERROR", "required-vars", str_flatten_comma(miss))

    if ("DOMAIN" %in% names(df) && any(df$DOMAIN != d)) {
      add(d, "ERROR", "domain-constant", str_c("DOMAIN != '", d, "'"))
    }

    if ("USUBJID" %in% names(df)) {
      if (anyNA(df$USUBJID)) add(d, "ERROR", "usubjid-missing", "NA USUBJID present")
      orphan <- setdiff(df$USUBJID, dm_ids)
      if (length(orphan) > 0) {
        add(d, "ERROR", "usubjid-not-in-dm", str_flatten_comma(orphan))
      }
    }

    seq_var <- str_c(d, "SEQ")
    if (seq_var %in% names(df)) {
      dup <- df |>
        count(across(all_of(c("USUBJID", seq_var))), name = ".n") |>
        filter(.n > 1)
      if (nrow(dup) > 0) {
        add(d, "ERROR", "seq-not-unique",
            sprintf("%d duplicated USUBJID/%s key(s)", nrow(dup), seq_var))
      }
    }

    for (v in names(df)[str_ends(names(df), "DTC")]) {
      bad <- unique(df[[v]][!.iso_ok(df[[v]])])
      if (length(bad) > 0) {
        add(d, "ERROR", "dtc-not-iso8601",
            sprintf("%s: %s", v, str_flatten_comma(bad)))
      }
    }

    for (v in names(df)[str_ends(names(df), "DY")]) {
      if (any(df[[v]] == 0, na.rm = TRUE)) add(d, "ERROR", "study-day-zero", v)
    }
  }

  # Domain-specific -----------------------------------------------------------
  if (anyDuplicated(domains$DM$USUBJID) > 0) {
    add("DM", "ERROR", "dm-one-row-per-subject", "USUBJID duplicated")
  }

  # At most one baseline flag per subject / test (VS and LB)
  for (bl in list(c("VS", "VSBLFL", "VSTESTCD"), c("LB", "LBBLFL", "LBTESTCD"))) {
    df <- domains[[bl[1]]]
    if (is.null(df) || !all(bl[2:3] %in% names(df))) next
    multi_bl <- df |>
      filter(.data[[bl[2]]] == "Y") |>
      count(across(all_of(c("USUBJID", bl[3]))), name = ".n") |>
      filter(.n > 1)
    if (nrow(multi_bl) > 0) {
      add(bl[1], "ERROR", "baseline-flag-multi",
          sprintf("%d subject/test with >1 %s='Y'", nrow(multi_bl), bl[2]))
    }
  }

  # LB: LBNRIND must be populated and match the standard-unit comparison
  lb_ind_vals <- setdiff(unique(na.omit(domains$LB$LBNRIND)),
                         c("LOW", "NORMAL", "HIGH"))
  if (length(lb_ind_vals) > 0) {
    add("LB", "ERROR", "lbnrind-bad-value", str_flatten_comma(lb_ind_vals))
  }
  lb_ind_bad <- domains$LB |>
    filter(!is.na(LBSTRESN), !is.na(LBSTNRLO), !is.na(LBSTNRHI)) |>
    mutate(expect = case_when(
      LBSTRESN < LBSTNRLO ~ "LOW",
      LBSTRESN > LBSTNRHI ~ "HIGH",
      .default            = "NORMAL"
    )) |>
    filter(is.na(LBNRIND) | LBNRIND != expect)
  if (nrow(lb_ind_bad) > 0) {
    add("LB", "ERROR", "lbnrind-inconsistent",
        sprintf("%d row(s) where LBNRIND disagrees with the range comparison",
                nrow(lb_ind_bad)))
  }

  # LB: standardised result and range must share a unit
  lb_unit_gap <- domains$LB |>
    filter(!is.na(LBSTRESN), LBSTRESU == "" | is.na(LBSTRESU))
  if (nrow(lb_unit_gap) > 0) {
    add("LB", "ERROR", "lbstresu-missing",
        sprintf("%d numeric result(s) with no LBSTRESU", nrow(lb_unit_gap)))
  }

  # SV: one row per subject per visit
  sv_dup <- domains$SV |> count(USUBJID, VISITNUM, name = ".n") |> filter(.n > 1)
  if (nrow(sv_dup) > 0) {
    add("SV", "ERROR", "sv-visit-not-unique",
        sprintf("%d duplicated USUBJID/VISITNUM key(s)", nrow(sv_dup)))
  }

  # VISITNUM reconciliation: SV is the reference list of visits that
  # actually happened; every collected (USUBJID, VISITNUM) must map to an
  # SV record, and the VISITNUM -> VISIT decode must agree with SV.
  sv_keys      <- domains$SV |> distinct(USUBJID, VISITNUM)
  sv_visit_map <- domains$SV |> distinct(VISITNUM, VISIT)

  for (d in c("VS", "EX", "LB")) {
    df <- domains[[d]]
    if (is.null(df) || !all(c("USUBJID", "VISITNUM", "VISIT") %in% names(df))) next

    orphan <- df |>
      distinct(USUBJID, VISITNUM, VISIT) |>
      anti_join(sv_keys, by = c("USUBJID", "VISITNUM"))
    if (nrow(orphan) > 0) {
      add(d, "ERROR", "visit-not-in-sv",
          sprintf("%d USUBJID/VISITNUM not in SV (e.g. %s VISITNUM %s)",
                  nrow(orphan), orphan$USUBJID[1], orphan$VISITNUM[1]))
    }

    clash <- df |>
      distinct(VISITNUM, VISIT) |>
      inner_join(sv_visit_map, by = "VISITNUM", suffix = c("", "_SV")) |>
      filter(VISIT != VISIT_SV)
    if (nrow(clash) > 0) {
      add(d, "ERROR", "visitnum-decode-mismatch",
          str_flatten_comma(sprintf("%s '%s' vs SV '%s'",
                                    clash$VISITNUM, clash$VISIT, clash$VISIT_SV)))
    }
  }

  # Date coherence: a VS measurement should fall inside its SV visit window
  vs_outside <- domains$VS |>
    filter(!is.na(VSDTC)) |>
    inner_join(select(domains$SV, USUBJID, VISITNUM, SVSTDTC, SVENDTC),
               by = c("USUBJID", "VISITNUM")) |>
    mutate(vd = as.Date(str_sub(VSDTC, 1, 10))) |>
    filter(vd < as.Date(SVSTDTC) | vd > as.Date(SVENDTC))
  if (nrow(vs_outside) > 0) {
    add("VS", "WARN", "vsdtc-outside-sv-window",
        sprintf("%d VS record(s) dated outside their SV visit window",
                nrow(vs_outside)))
  }

  # Screen failures must have no study days anywhere (no RFSTDTC)
  sf_ids <- domains$DM |> filter(ARMCD == "SCRNFAIL") |> pull(USUBJID)
  for (d in c("VS", "AE", "CM", "DS", "SV", "LB", "MH")) {
    df <- domains[[d]]
    # VISITDY is a protocol-planned day, not anchored to RFSTDTC - exclude it.
    dy_vars <- setdiff(names(df)[str_ends(names(df), "DY")], "VISITDY")
    leaked <- df |>
      filter(USUBJID %in% sf_ids) |>
      filter(if_any(all_of(dy_vars), \(x) !is.na(x)))
    if (nrow(leaked) > 0) {
      add(d, "ERROR", "screenfail-has-study-day",
          sprintf("%d row(s) for screen-failure subjects", nrow(leaked)))
    }
  }

  # SUPP-- / CO: related-record structure
  .related <- c(SUPPDM = "DM", SUPPAE = "AE", SUPPEX = "EX", CO = "AE")
  for (rel in names(.related)) {
    df <- domains[[rel]]
    if (is.null(df)) next
    rd     <- unname(.related[[rel]])
    parent <- domains[[rd]]
    val_var <- if ("QVAL" %in% names(df)) "QVAL" else "COVAL"

    if (any(df$RDOMAIN != rd)) {
      add(rel, "ERROR", "rdomain-constant", str_c("RDOMAIN != '", rd, "'"))
    }

    n_blank <- sum(is.na(df[[val_var]]) | df[[val_var]] == "")
    if (n_blank > 0) {
      add(rel, "ERROR", "value-missing",
          sprintf("%d row(s) with blank %s", n_blank, val_var))
    }

    # IDVAR is either blank throughout or one consistent parent key
    idvars <- unique(df$IDVAR[!is.na(df$IDVAR) & df$IDVAR != ""])
    if (length(idvars) > 1) {
      add(rel, "ERROR", "idvar-inconsistent", str_flatten_comma(idvars))
    }

    # SUPP: one qualifier value per parent record
    if ("QNAM" %in% names(df)) {
      dup <- df |>
        count(USUBJID, IDVAR, IDVARVAL, QNAM, name = ".n") |>
        filter(.n > 1)
      if (nrow(dup) > 0) {
        add(rel, "ERROR", "qnam-not-unique",
            sprintf("%d duplicated USUBJID/IDVARVAL/QNAM key(s)", nrow(dup)))
      }
    }

    # Referential integrity to the parent domain
    if (length(idvars) == 0) {
      orphan <- setdiff(df$USUBJID, parent$USUBJID)
      if (length(orphan) > 0) {
        add(rel, "ERROR", "related-parent-orphan",
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
        add(rel, "ERROR", "related-parent-orphan",
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
        add(rel, "ERROR", "qorig-bad-value", str_flatten_comma(bad_orig))
      }
    }
  }

  # Controlled terminology consistency: for every codelist the spec maps
  # through a decode transform, the built data must contain only
  # spec-declared CDISC terms (plus any decode default, e.g. SEX's "U").
  # The mappers already enforce this at map time via check_ct(); reading
  # the spec a second time here means a drifted spec cannot satisfy both
  # readers. Derivation-mapped variables (DSDECOD's reclassification) are
  # skipped: their full output vocabulary lives in the derivation, not the
  # codelist.
  if (!is.null(spec)) {
    decode_cts <- spec$variables |>
      filter(transform == "decode", !is.na(ref)) |>
      pull(ref) |>
      unique()
    for (ct in intersect(decode_cts, unique(spec$codelists$ct))) {
      allowed <- spec$codelists$cdisc_term[spec$codelists$ct == ct]
      defaults <- spec$variables$default[spec$variables$transform == "decode" &
                                           spec$variables$ref == ct]
      allowed <- c(allowed, defaults)
      for (d in names(domains)) {
        df <- domains[[d]]
        if (!ct %in% names(df)) next
        bad <- setdiff(unique(na.omit(df[[ct]])), allowed)
        if (length(bad) > 0) {
          add(d, "ERROR", "ct-value-not-in-spec",
              sprintf("%s: value(s) not declared in the spec: %s", ct,
                      str_flatten_comma(bad)))
        }
      }
    }
  }

  # Death coherence: the three layers that record a death must name exactly
  # the same subjects, and the death dates must agree.
  death_sets <- list(
    ae = domains$AE |> filter(AEOUT == "FATAL") |> pull(USUBJID) |> unique(),
    ds = domains$DS |> filter(DSDECOD == "DEATH") |> pull(USUBJID) |> unique(),
    dm = domains$DM |> filter(DTHFL %in% "Y") |> pull(USUBJID) |> unique()
  )
  death_inconsistent <- setdiff(Reduce(union, death_sets),
                                Reduce(intersect, death_sets))
  if (length(death_inconsistent) > 0) {
    add("AE", "ERROR", "death-coherence",
        sprintf(paste("%d subject(s) where fatal AE / DS DEATH / DM DTHFL",
                      "disagree: %s"), length(death_inconsistent),
                str_flatten_comma(death_inconsistent)))
  }
  death_dates <- domains$DM |>
    filter(DTHFL %in% "Y") |>
    select(USUBJID, DTHDTC) |>
    inner_join(domains$DS |> filter(DSDECOD == "DEATH") |> select(USUBJID, DSSTDTC),
               by = "USUBJID") |>
    filter(is.na(DTHDTC) | DTHDTC != DSSTDTC)
  if (nrow(death_dates) > 0) {
    add("DM", "ERROR", "death-date-mismatch",
        "DTHDTC disagrees with the DS DEATH record date")
  }

  # MH: history precedes the study. ISO 8601 reduced precision compares
  # correctly as a string, which is exactly why partial dates stay partial:
  # the comparison works without imputing.
  mh_after <- domains$MH |>
    left_join(select(domains$DM, USUBJID, RFSTDTC), by = "USUBJID") |>
    filter(!is.na(MHSTDTC), !is.na(RFSTDTC), MHSTDTC > RFSTDTC)
  if (nrow(mh_after) > 0) {
    add("MH", "ERROR", "mh-after-first-dose",
        sprintf("%d history record(s) starting after RFSTDTC", nrow(mh_after)))
  }

  # RELREC: a link is a claim about two records, so both halves must resolve
  relrec <- domains$RELREC
  if (!is.null(relrec) && nrow(relrec) > 0) {
    bad_reltype <- relrec |> filter(!RELTYPE %in% c("ONE", "MANY"))
    if (nrow(bad_reltype) > 0) {
      add("RELREC", "ERROR", "reltype-bad-value", "RELTYPE must be ONE or MANY")
    }

    rel_size <- relrec |> count(RELID, name = ".n") |> filter(.n != 2)
    if (nrow(rel_size) > 0) {
      add("RELREC", "ERROR", "relid-not-a-pair",
          sprintf("%d RELID(s) that are not a two-record pair", nrow(rel_size)))
    }
    rel_mixed <- relrec |>
      count(RELID, RDOMAIN) |>
      count(RELID, name = ".n") |>
      filter(.n != 2)
    if (nrow(rel_mixed) > 0) {
      add("RELREC", "ERROR", "relid-not-cm-ae",
          "every RELID must pair a CM record with an AE record")
    }
    rel_idvars <- relrec |> distinct(RDOMAIN, IDVAR)
    if (!setequal(rel_idvars$IDVAR, c("CMSEQ", "AESEQ")) ||
          nrow(rel_idvars) != 2) {
      add("RELREC", "ERROR", "idvar-not-seq",
          "IDVAR must be CMSEQ for the CM side and AESEQ for the AE side")
    }

    for (side in list(c("CM", "CMSEQ"), c("AE", "AESEQ"))) {
      rd <- side[1]; idv <- side[2]
      parent_keys <- domains[[rd]] |>
        transmute(USUBJID, IDVARVAL = as.character(.data[[idv]]))
      orphans <- relrec |>
        filter(RDOMAIN == rd) |>
        anti_join(parent_keys, by = c("USUBJID", "IDVARVAL"))
      if (nrow(orphans) > 0) {
        add("RELREC", "ERROR", "relrec-parent-orphan",
            sprintf("%d %s-side link(s) with no matching %s record",
                    nrow(orphans), rd, rd))
      }
    }
  }

  report <- bind_rows(issues)
  if (nrow(report) == 0) report <- .new_issue()
  report
}

#' Stop when a validation issue tibble carries ERRORs
#'
#' The pipeline wrapper: `validate_sdtm()` / `validate_adam()` return issues;
#' the pipeline calls this to turn ERRORs into a hard stop. WARNs pass.
#'
#' @param issues An issue tibble from [validate_sdtm()] or [validate_adam()]
#' @param context Label used in the error message
#' @return Invisibly, the issue tibble (when there are no ERRORs).
#' @export
stop_on_error <- function(issues, context = "validation") {
  n_err <- sum(issues$severity == "ERROR")
  if (n_err > 0) {
    stop(sprintf("%s: %d validation error(s)", context, n_err), call. = FALSE)
  }
  invisible(issues)
}

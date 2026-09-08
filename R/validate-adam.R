# ============================================================================
# Title:   Content validation of the derived ADaM datasets
# Purpose: Same contract as validate_sdtm() - cheap checks that catch the
#          mistakes this project is designed to make you think about - but
#          for ADaM, where the checks are about derivations and coherence,
#          not SDTM structure. Ported from the script-based
#          39_validate_adam.R.
# ============================================================================

#' Validate the derived ADaM datasets
#'
#' Recomputes the derivations from their inputs through the same shared
#' rule functions the derivers call (adam-rules.R), so the validator and
#' the build cannot drift apart: it detects post-hoc corruption of the
#' built dataset. A wrong rule would satisfy both sides - the rules
#' themselves are pinned by the rule-level tests against hand-built
#' inputs. Checks population-flag and treatment coherence on ADSL, the
#' TRTEMFL window and SUPP merge-back on ADAE, record coverage and
#' analysis-timing coherence on ADCM, and for the BDS datasets
#' (ADVS / ADEG / ADLB) the baseline anchors, BASE/CHG/PCHG arithmetic,
#' ANRIND against the row's own range, and coverage against the SDTM
#' source records. ADEG is the ADVS situation exactly: EG collects no
#' reference ranges of its own, so the ADEG rows of `spec$bds` are the
#' SAP stand-in the built ANRIND is checked against. ADQS joins the same
#' contract with the package's first derived analysis parameter: the
#' instrument total is recomputed from the SDTM QS items through the
#' same `spec$totals` rows and the same all-required rule the builder
#' applies (`adqs-total-wrong`), and a total may exist only where the
#' complete item set exists (`adqs-total-coverage`). The built ranges
#' join the drift check like any BDS parameter's - the `spec$bds` NA/NA
#' for the items, the `spec$totals` range for the total
#' (`adqs-range-spec-drift`) - and a QS item performed in SDTM but
#' missing from `spec$bds`, which the builder silently drops, trips
#' `adqs-item-not-in-spec`.
#'
#' @param adsl,adae,adcm,advs,adeg,adlb,adqs,adtte The mapped ADaM datasets
#' @param dm,ds,ae,cm,vs,qs,eg,lb,suppae The SDTM source datasets the ADaM layer
#'   was built from
#' @param spec A `study_spec`; the ADVS rows of `spec$bds` declare the
#'   reference ranges the built ADVS is checked against and the ADEG rows
#'   the ECG interval ranges the built ADEG is checked against - a silent
#'   change on either side (in `spec$bds` or in [derive_advs()] /
#'   [derive_adeg()]) trips the range-drift check, and a parameter missing
#'   from `spec$bds` trips the coverage check. The `spec$totals` rows
#'   declare the derived totals the built ADQS is recomputed against - a
#'   silent change there or in [derive_adqs()] trips the total-recompute
#'   check; with the ADQS `spec$bds` rows they are also the range source
#'   the built ADQS is drift-checked against, and a QS item missing from
#'   `spec$bds` trips `adqs-item-not-in-spec`. The study's `age_min` /
#'   `age_max` bound the AGE check.
#' @return An issue tibble: domain, severity ("ERROR" / "WARN"), check,
#'   detail. Empty when everything passes.
#' @export
#' @examples
#' ext <- file.path(tempdir(), "extract-adam")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' built <- build_all(ext)
#' issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM,
#'                         built$adam$ADVS, built$adam$ADEG, built$adam$ADLB,
#'                         built$adam$ADQS, built$adam$ADTTE,
#'                         built$sdtm$DM, built$sdtm$DS, built$sdtm$AE,
#'                         built$sdtm$CM, built$sdtm$VS, built$sdtm$QS,
#'                         built$sdtm$EG, built$sdtm$LB, built$sdtm$SUPPAE,
#'                         spec_synth01)
#' issues                                  # empty: the build is clean
validate_adam <- function(adsl, adae, adcm, advs, adeg, adlb, adqs, adtte,
                          dm, ds, ae, cm, vs, qs, eg, lb, suppae,
                          spec = spec_synth01) {
  issues <- list()
  add <- function(domain, severity, check, detail) {
    issues[[length(issues) + 1L]] <<- # nolint: assignment_linter. (issue collector)
      .new_issue(domain, severity, check, detail)
  }

  .req_adsl <- c(
    "STUDYID", "USUBJID", "SUBJID", "SITEID", "COUNTRY", "AGE", "AGEU",
    "SEX", "RACE", "ETHNIC", "BRTHDTC", "BRTHDT", "BRTHDTF",
    "TRT01P", "TRT01PCD", "TRT01A", "TRT01ACD",
    "ENRLSTDT", "TRTSDT", "TRTSDTF", "TRTEDT", "TRTEDTF", "TRTDURD",
    "SAFFL", "ITTFL", "EOSDT", "EOSDTF", "EOSSTT", "DCSREAS",
    "DTHDT", "DTHDTF", "LSTALVDT", "DTHFL"
  )

  # ADSL: structure -------------------------------------------------------------
  miss <- setdiff(.req_adsl, names(adsl))
  if (length(miss) > 0) {
    add("ADSL", "ERROR", "required-vars", str_flatten_comma(miss))
  }

  if (anyDuplicated(adsl$USUBJID) > 0) {
    add("ADSL", "ERROR", "adsl-one-row-per-subject", "USUBJID duplicated")
  }
  orphan <- setdiff(adsl$USUBJID, dm$USUBJID)
  if (length(orphan) > 0) {
    add("ADSL", "ERROR", "adsl-not-in-dm", str_flatten_comma(orphan))
  }
  missing_subj <- setdiff(dm$USUBJID, adsl$USUBJID)
  if (length(missing_subj) > 0) {
    add("ADSL", "ERROR", "dm-not-in-adsl", str_flatten_comma(missing_subj))
  }

  # Population flags
  bad_fl <- adsl |> filter(!SAFFL %in% c("Y", "N") | !ITTFL %in% c("Y", "N"))
  if (nrow(bad_fl) > 0) {
    add("ADSL", "ERROR", "pop-flag-ct",
        sprintf("%d row(s) with SAFFL/ITTFL not Y/N", nrow(bad_fl)))
  }

  # ITTFL='Y' is "randomized", and the arm comes with it; ITTFL='N' must
  # carry no arm at all.
  bad_itt <- adsl |>
    filter(ITTFL == "Y" &
             (is.na(TRT01P) | is.na(TRT01PCD) | is.na(TRT01A) | is.na(TRT01ACD))) |>
    bind_rows(adsl |> filter(ITTFL == "N" & (!is.na(TRT01P) | !is.na(TRT01A))))
  if (nrow(bad_itt) > 0) {
    add("ADSL", "ERROR", "itt-trt-coherence",
        "ITTFL='Y' must carry TRT01P/PCD/A/ACD; ITTFL='N' must have none")
  }

  # SAFFL is "has a treatment start", no more, no less
  bad_saf <- adsl |> filter((SAFFL == "Y") != (!is.na(TRTSDT)))
  if (nrow(bad_saf) > 0) {
    add("ADSL", "ERROR", "saffl-trtsdt-coherence",
        "SAFFL='Y' iff TRTSDT is populated")
  }

  bad_si <- adsl |> filter(SAFFL == "Y", ITTFL != "Y")
  if (nrow(bad_si) > 0) {
    add("ADSL", "ERROR", "saffl-without-itt", "dosed subject without a planned arm")
  }

  # Treatment dates
  bad_ord <- adsl |> filter(!is.na(TRTSDT), !is.na(TRTEDT), TRTEDT < TRTSDT)
  if (nrow(bad_ord) > 0) {
    add("ADSL", "ERROR", "trtedt-before-trtsdt", sprintf("%d row(s)", nrow(bad_ord)))
  }

  bad_dur <- adsl |>
    filter(!is.na(TRTSDT), !is.na(TRTEDT)) |>
    mutate(.expect = .rule_trtdurd(TRTSDT, TRTEDT)) |>
    filter(xor(is.na(TRTDURD), is.na(.expect)) |
             (!is.na(TRTDURD) & !is.na(.expect) & TRTDURD != .expect))
  if (nrow(bad_dur) > 0) {
    add("ADSL", "ERROR", "trtdurd-wrong", sprintf("%d row(s)", nrow(bad_dur)))
  }

  # End of study
  bad_stt <- adsl |> filter(!EOSSTT %in% c("COMPLETED", "DISCONTINUED", NA))
  if (nrow(bad_stt) > 0) {
    add("ADSL", "ERROR", "eosstt-ct", sprintf("%d row(s) with bad EOSSTT",
                                              nrow(bad_stt)))
  }

  bad_stt2 <- adsl |> filter(is.na(EOSSTT) != is.na(EOSDT))
  if (nrow(bad_stt2) > 0) {
    add("ADSL", "ERROR", "eosstt-without-eosdt",
        "EOSSTT and EOSDT must be populated together")
  }

  bad_dc <- adsl |>
    filter(EOSSTT == "COMPLETED" & !is.na(DCSREAS)) |>
    bind_rows(adsl |> filter(EOSSTT == "DISCONTINUED" & is.na(DCSREAS)))
  if (nrow(bad_dc) > 0) {
    add("ADSL", "ERROR", "dcsreas-coherence",
        "DCSREAS populated iff EOSSTT='DISCONTINUED'")
  }

  # DCSREAS must be the DS disposition decode, not a free retyping of it
  ds_disp <- ds |>
    filter(DSCAT == "DISPOSITION EVENT") |>
    select(USUBJID, DSDECOD)
  bad_ds <- adsl |>
    filter(!is.na(EOSSTT)) |>
    left_join(ds_disp, by = "USUBJID") |>
    filter(is.na(DSDECOD) | (!is.na(DCSREAS) & DCSREAS != DSDECOD))
  if (nrow(bad_ds) > 0) {
    add("ADSL", "ERROR", "dcsreas-not-from-ds",
        "DCSREAS disagrees with the subject's DS DISPOSITION EVENT decode")
  }

  # Screen-failure coherence (DM is the source of the fact)
  sf_ids <- dm |> filter(ARMCD == "SCRNFAIL") |> pull(USUBJID)
  bad_sf <- adsl |>
    filter(USUBJID %in% sf_ids) |>
    filter(ITTFL != "N" | SAFFL != "N" | !is.na(TRTSDT) | !is.na(EOSSTT))
  if (nrow(bad_sf) > 0) {
    add("ADSL", "ERROR", "screenfail-leak",
        sprintf(paste("%d screen-failure subject(s) with a population flag,",
                      "dosing or disposition"), nrow(bad_sf)))
  }

  bad_nsf <- adsl |>
    filter(!USUBJID %in% sf_ids, ITTFL != "Y")
  if (nrow(bad_nsf) > 0) {
    add("ADSL", "ERROR", "randomized-without-itt",
        sprintf("%d randomized subject(s) with ITTFL != 'Y'", nrow(bad_nsf)))
  }

  # Last known alive
  bad_lal <- adsl |>
    filter(SAFFL == "Y" | !is.na(EOSDT), is.na(LSTALVDT))
  if (nrow(bad_lal) > 0) {
    add("ADSL", "ERROR", "lstalvdt-missing",
        "dosed or disposed subject without LSTALVDT")
  }

  bad_lal2 <- adsl |> filter(!is.na(TRTSDT), !is.na(LSTALVDT), LSTALVDT < TRTSDT)
  if (nrow(bad_lal2) > 0) {
    add("ADSL", "ERROR", "lstalvdt-before-trtsdt", sprintf("%d row(s)", nrow(bad_lal2)))
  }

  bad_lal3 <- adsl |> filter(!is.na(EOSDT), !is.na(LSTALVDT), LSTALVDT < EOSDT)
  if (nrow(bad_lal3) > 0) {
    add("ADSL", "ERROR", "lstalvdt-before-eosdt", sprintf("%d row(s)", nrow(bad_lal3)))
  }

  # Age: a missing AGE signals a missing birth date, which nothing
  # downstream expects. The bounds are the study's own - spec$study's
  # age_min / age_max - so a paediatric or elderly protocol is a spec
  # change, not a validator edit.
  bad_age <- adsl |>
    filter(is.na(AGE) | AGE < spec$study$age_min | AGE > spec$study$age_max)
  if (nrow(bad_age) > 0) {
    add("ADSL", "ERROR", "age-out-of-range",
        sprintf("%d row(s) with AGE missing or outside %s-%s", nrow(bad_age),
                spec$study$age_min, spec$study$age_max))
  }

  # Imputation flags: controlled values, and a flag implies a date
  for (fl in c("BRTHDTF", "TRTSDTF", "TRTEDTF", "EOSDTF", "DTHDTF")) {
    dtv <- str_remove(fl, "F$")
    bad <- adsl |>
      filter(!.data[[fl]] %in% c("", "D", "M", NA)) |>
      bind_rows(adsl |> filter(.data[[fl]] %in% c("D", "M"), is.na(.data[[dtv]])))
    if (nrow(bad) > 0) {
      add("ADSL", "ERROR", "imputation-flag-bad",
          sprintf("%d row(s) with a bad %s / %s pair", nrow(bad), fl, dtv))
    }
  }

  # Date ordering: consent before first dose, end of study after it
  bad_cons <- adsl |>
    filter(!is.na(ENRLSTDT), !is.na(TRTSDT), TRTSDT < ENRLSTDT)
  if (nrow(bad_cons) > 0) {
    add("ADSL", "ERROR", "dose-before-consent", sprintf("%d row(s)", nrow(bad_cons)))
  }

  bad_eos <- adsl |>
    filter(!is.na(TRTSDT), !is.na(EOSDT), EOSDT < TRTSDT)
  if (nrow(bad_eos) > 0) {
    add("ADSL", "ERROR", "eosdt-before-trtsdt", sprintf("%d row(s)", nrow(bad_eos)))
  }

  # Death coherence
  death_ids <- dm |> filter(DTHFL %in% "Y") |> pull(USUBJID)
  fatal_ids <- ae |> filter(AEOUT == "FATAL") |> distinct(USUBJID) |> pull(USUBJID)
  ds_death  <- ds |> filter(DSDECOD == "DEATH") |> distinct(USUBJID) |> pull(USUBJID)

  bad_dth <- adsl |>
    filter(DTHFL == "Y") |>
    filter(is.na(DTHDT) | !USUBJID %in% fatal_ids | !USUBJID %in% ds_death |
             !USUBJID %in% death_ids)
  if (nrow(bad_dth) > 0) {
    add("ADSL", "ERROR", "dthfl-unbacked",
        "DTHFL='Y' without a fatal AE, DS DEATH record, DM flag or DTHDT")
  }

  bad_dth2 <- adsl |>
    filter(USUBJID %in% setdiff(fatal_ids, death_ids))
  if (nrow(bad_dth2) > 0) {
    add("ADSL", "ERROR", "fatal-ae-without-dthfl",
        "fatal-outcome AE whose subject has no DTHFL='Y'")
  }

  bad_dth3 <- adsl |>
    filter(DTHFL == "Y") |>
    filter(!EOSSTT %in% "DISCONTINUED" | !DCSREAS %in% "DEATH" |
             is.na(TRTSDT) | is.na(DTHDT) | DTHDT < TRTSDT |
             is.na(LSTALVDT) | LSTALVDT != DTHDT)
  if (nrow(bad_dth3) > 0) {
    add("ADSL", "ERROR", "dth-derivation-inconsistent",
        paste("death subject whose EOSSTT/DCSREAS/LSTALVDT/DTHDT disagree",
              "with the death"))
  }

  bad_dthct <- adsl |> filter(!DTHFL %in% c("Y", ""))
  if (nrow(bad_dthct) > 0) {
    add("ADSL", "ERROR", "dthfl-ct", "DTHFL must be 'Y' or blank")
  }

  # ADAE: structure -------------------------------------------------------------
  .req_adae <- c(
    "STUDYID", "USUBJID", "ASEQ", "AETERM", "AEDECOD", "AEBODSYS", "AESEV",
    "AESER", "AEREL", "AEACN", "AEOUT", "AESI", "AEDISCON",
    "ASTDT", "ASTDTF", "ASTDY", "AENDT", "AENDTF", "AENDY", "TRTEMFL"
  )
  miss <- setdiff(.req_adae, names(adae))
  if (length(miss) > 0) {
    add("ADAE", "ERROR", "adae-required-vars", str_flatten_comma(miss))
  }

  # One analysis record per collected event
  adae_dup <- adae |> count(USUBJID, ASEQ, name = ".n") |> filter(.n > 1)
  if (nrow(adae_dup) > 0) {
    add("ADAE", "ERROR", "adae-key-not-unique",
        sprintf("%d duplicated USUBJID/ASEQ key(s)", nrow(adae_dup)))
  }
  ae_keys   <- ae   |> distinct(USUBJID, AESEQ)
  adae_keys <- adae |> distinct(USUBJID, ASEQ)
  lost <- anti_join(ae_keys, adae_keys, by = c("USUBJID", AESEQ = "ASEQ"))
  if (nrow(lost) > 0) {
    add("ADAE", "ERROR", "adae-lost-event",
        sprintf("%d SDTM AE record(s) with no ADAE row", nrow(lost)))
  }
  extra <- anti_join(adae_keys, ae_keys, by = c("USUBJID", ASEQ = "AESEQ"))
  if (nrow(extra) > 0) {
    add("ADAE", "ERROR", "adae-extra-event",
        sprintf("%d ADAE row(s) with no SDTM AE record", nrow(extra)))
  }

  # TRTEMFL: recompute the rule from ADSL instead of trusting the build
  te_expected <- adae |>
    left_join(
      adsl |>
        transmute(USUBJID, .ref_trtsdt = TRTSDT, .ref_trtedt = TRTEDT),
      by = "USUBJID"
    ) |>
    mutate(.expect = .rule_trtemfl(ASTDT, .ref_trtsdt, .ref_trtedt))
  bad_te <- te_expected |>
    filter(xor(is.na(TRTEMFL), is.na(.expect)) |
             (!is.na(TRTEMFL) & !is.na(.expect) & TRTEMFL != .expect))
  if (nrow(bad_te) > 0) {
    add("ADAE", "ERROR", "trtemfl-not-derivable",
        sprintf(paste("%d row(s) where TRTEMFL disagrees with the",
                      "onset-within-[TRTSDT,TRTEDT] rule"), nrow(bad_te)))
  }

  # ADAE imputation flags
  for (fl in c("ASTDTF", "AENDTF")) {
    dtv <- str_remove(fl, "F$")
    bad <- adae |>
      filter(!.data[[fl]] %in% c("", "D", "M", NA)) |>
      bind_rows(adae |> filter(.data[[fl]] %in% c("D", "M"), is.na(.data[[dtv]])))
    if (nrow(bad) > 0) {
      add("ADAE", "ERROR", "adae-imputation-flag-bad",
          sprintf("%d row(s) with a bad %s / %s pair", nrow(bad), fl, dtv))
    }
  }

  # Analysis study days: anchored on TRTSDT with the no-day-0 rule
  bad_dy <- adae |>
    filter(!is.na(ASTDT), !is.na(TRTSDT)) |>
    mutate(.expect = derive_dy_d(ASTDT, TRTSDT)) |>
    filter(xor(is.na(ASTDY), is.na(.expect)) |
             (!is.na(ASTDY) & !is.na(.expect) & ASTDY != .expect))
  if (nrow(bad_dy) > 0) {
    add("ADAE", "ERROR", "astdy-wrong-anchor",
        sprintf("%d row(s) where ASTDY disagrees with ASTDT vs TRTSDT",
                nrow(bad_dy)))
  }
  if (any(adae$ASTDY == 0, na.rm = TRUE) || any(adae$AENDY == 0, na.rm = TRUE)) {
    add("ADAE", "ERROR", "adae-study-day-zero", "ASTDY or AENDY equals zero")
  }

  # A full-precision start yields the same study day SDTM computed
  bad_dy2 <- adae |>
    inner_join(select(ae, USUBJID, AESEQ, AESTDTC, AESTDY),
               by = c("USUBJID", ASEQ = "AESEQ")) |>
    filter(str_length(AESTDTC) == 10, !is.na(ASTDY), !is.na(AESTDY),
           ASTDY != AESTDY)
  if (nrow(bad_dy2) > 0) {
    add("ADAE", "ERROR", "astdy-vs-sdtm-aestdy",
        "ASTDY disagrees with SDTM AESTDY for a full-precision start")
  }

  bad_ord <- adae |> filter(!is.na(ASTDT), !is.na(AENDT), AENDT < ASTDT)
  if (nrow(bad_ord) > 0) {
    add("ADAE", "ERROR", "aendt-before-astdt", sprintf("%d row(s)", nrow(bad_ord)))
  }

  # SUPP merge-back: values must match SUPPAE, and every AE record must
  # have its qualifiers
  supp_check <- suppae |>
    transmute(USUBJID, ASEQ = as.integer(IDVARVAL), QNAM, QVAL) |>
    pivot_wider(names_from = QNAM, values_from = QVAL)
  bad_supp <- adae |>
    select(USUBJID, ASEQ, AESI, AEDISCON) |>
    full_join(supp_check, by = c("USUBJID", "ASEQ")) |>
    filter(is.na(AESI.x) | is.na(AEDISCON.x) |
             xor(is.na(AESI.x), is.na(AESI.y)) |
             xor(is.na(AEDISCON.x), is.na(AEDISCON.y)) |
             AESI.x != AESI.y | AEDISCON.x != AEDISCON.y)
  if (nrow(bad_supp) > 0) {
    add("ADAE", "ERROR", "adae-supp-merge-bad",
        sprintf(paste("%d row(s) where AESI/AEDISCON disagree with SUPPAE",
                      "or are missing"), nrow(bad_supp)))
  }

  # ADCM: structure -------------------------------------------------------------
  .req_adcm <- c(
    "STUDYID", "USUBJID", "ASEQ", "CMTRT", "CMDECOD", "CMINDC", "CMDOSE",
    "CMDOSU", "CMDOSFRQ", "CMROUTE",
    "ASTDT", "ASTDTF", "ASTDY", "AENDT", "AENDTF", "AENDY"
  )
  miss <- setdiff(.req_adcm, names(adcm))
  if (length(miss) > 0) {
    add("ADCM", "ERROR", "adcm-required-vars", str_flatten_comma(miss))
  }

  # One analysis record per collected medication
  adcm_dup <- adcm |> count(USUBJID, ASEQ, name = ".n") |> filter(.n > 1)
  if (nrow(adcm_dup) > 0) {
    add("ADCM", "ERROR", "adcm-key-not-unique",
        sprintf("%d duplicated USUBJID/ASEQ key(s)", nrow(adcm_dup)))
  }
  cm_keys   <- cm |> distinct(USUBJID, CMSEQ)
  adcm_keys <- adcm |> distinct(USUBJID, ASEQ)
  lost <- anti_join(cm_keys, adcm_keys, by = c("USUBJID", CMSEQ = "ASEQ"))
  if (nrow(lost) > 0) {
    add("ADCM", "ERROR", "adcm-lost-record",
        sprintf("%d SDTM CM record(s) with no ADCM row", nrow(lost)))
  }
  extra <- anti_join(adcm_keys, cm_keys, by = c("USUBJID", ASEQ = "CMSEQ"))
  if (nrow(extra) > 0) {
    add("ADCM", "ERROR", "adcm-extra-record",
        sprintf("%d ADCM row(s) with no SDTM CM record", nrow(extra)))
  }

  # ADCM imputation flags
  for (fl in c("ASTDTF", "AENDTF")) {
    dtv <- str_remove(fl, "F$")
    bad <- adcm |>
      filter(!.data[[fl]] %in% c("", "D", "M", NA)) |>
      bind_rows(adcm |> filter(.data[[fl]] %in% c("D", "M"), is.na(.data[[dtv]])))
    if (nrow(bad) > 0) {
      add("ADCM", "ERROR", "adcm-imputation-flag-bad",
          sprintf("%d row(s) with a bad %s / %s pair", nrow(bad), fl, dtv))
    }
  }

  # Analysis study days: anchored on TRTSDT with the no-day-0 rule
  bad_dy <- adcm |>
    left_join(
      adsl |> transmute(USUBJID, .ref_trtsdt = TRTSDT),
      by = "USUBJID"
    ) |>
    mutate(.expect = derive_dy_d(ASTDT, .ref_trtsdt)) |>
    filter(xor(is.na(ASTDY), is.na(.expect)) |
             (!is.na(ASTDY) & !is.na(.expect) & ASTDY != .expect))
  if (nrow(bad_dy) > 0) {
    add("ADCM", "ERROR", "adcm-astdy-wrong-anchor",
        sprintf("%d row(s) where ASTDY disagrees with ASTDT vs TRTSDT",
                nrow(bad_dy)))
  }
  if (any(adcm$ASTDY == 0, na.rm = TRUE) || any(adcm$AENDY == 0, na.rm = TRUE)) {
    add("ADCM", "ERROR", "adcm-study-day-zero", "ASTDY or AENDY equals zero")
  }

  # A full-precision start yields the same study day SDTM computed
  bad_dy2 <- adcm |>
    inner_join(select(cm, USUBJID, CMSEQ, CMSTDTC, CMSTDY),
               by = c("USUBJID", ASEQ = "CMSEQ")) |>
    filter(str_length(CMSTDTC) == 10, !is.na(ASTDY), !is.na(CMSTDY),
           ASTDY != CMSTDY)
  if (nrow(bad_dy2) > 0) {
    add("ADCM", "ERROR", "adcm-astdy-vs-sdtm-cmstdy",
        "ASTDY disagrees with SDTM CMSTDY for a full-precision start")
  }

  bad_ord <- adcm |> filter(!is.na(ASTDT), !is.na(AENDT), AENDT < ASTDT)
  if (nrow(bad_ord) > 0) {
    add("ADCM", "ERROR", "adcm-aendt-before-astdt", sprintf("%d row(s)", nrow(bad_ord)))
  }

  # ADVS: structure -------------------------------------------------------------
  .req_advs <- c(
    "STUDYID", "USUBJID", "PARAMCD", "PARAM", "PARAMN", "AVAL", "AVALU",
    "ABLFL", "BASE", "CHG", "PCHG", "ANRIND", "ANRLO", "ANRHI",
    "AVISIT", "AVISITN", "ADT", "ADY"
  )
  miss <- setdiff(.req_advs, names(advs))
  if (length(miss) > 0) {
    add("ADVS", "ERROR", "advs-required-vars", str_flatten_comma(miss))
  }

  # One analysis record per subject / parameter / visit
  advs_dup <- advs |>
    count(USUBJID, PARAMCD, AVISITN, name = ".n") |>
    filter(.n > 1)
  if (nrow(advs_dup) > 0) {
    add("ADVS", "ERROR", "advs-key-not-unique",
        sprintf(paste("%d duplicated USUBJID/PARAMCD/AVISITN key(s) - a visit",
                      "with two positions for one test?"), nrow(advs_dup)))
  }

  # Coverage: exactly the SDTM VS records that carry a result
  vs_results <- vs |> filter(!is.na(VSSTRESN))
  if (nrow(advs) != nrow(vs_results)) {
    add("ADVS", "ERROR", "advs-coverage",
        sprintf("ADVS has %d row(s) but VS carries %d result(s)",
                nrow(advs), nrow(vs_results)))
  }
  advs_orphan <- advs |>
    anti_join(vs_results,
              by = c("USUBJID", PARAMCD = "VSTESTCD", AVISITN = "VISITNUM",
                     AVAL = "VSSTRESN"))
  if (nrow(advs_orphan) > 0) {
    add("ADVS", "ERROR", "advs-orphan-record",
        sprintf("%d ADVS row(s) with no matching VS result", nrow(advs_orphan)))
  }

  # Baseline: exactly one per randomized subject per parameter, none for
  # screen failures, and it must be the value SDTM flagged
  itt_ids <- adsl |> filter(ITTFL == "Y") |> pull(USUBJID)
  adsl_ref <- adsl |> select(USUBJID, .ref_trtsdt = TRTSDT)
  bl_multi <- advs |>
    filter(ABLFL == "Y") |>
    count(USUBJID, PARAMCD, name = ".n") |>
    filter(.n > 1)
  if (nrow(bl_multi) > 0) {
    add("ADVS", "ERROR", "advs-ablfl-multi",
        sprintf("%d subject/parameter(s) with >1 ABLFL='Y'", nrow(bl_multi)))
  }
  bl_missing <- advs |>
    filter(USUBJID %in% itt_ids, !is.na(AVAL)) |>
    count(USUBJID, PARAMCD, ABLFL) |>
    filter(!any(ABLFL %in% "Y"), .by = c("USUBJID", "PARAMCD"))
  if (nrow(bl_missing) > 0) {
    add("ADVS", "ERROR", "advs-ablfl-missing",
        sprintf(paste("%d randomized subject/parameter(s) with no baseline",
                      "record"), nrow(bl_missing)))
  }
  bl_sf <- advs |> filter(USUBJID %in% sf_ids, ABLFL == "Y")
  if (nrow(bl_sf) > 0) {
    add("ADVS", "ERROR", "advs-ablfl-screenfail",
        "screen-failure subject with a baseline flag")
  }
  vs_bl <- vs_results |>
    filter(VSBLFL == "Y") |>
    select(USUBJID, PARAMCD = VSTESTCD, .vs_bl = VSSTRESN)
  bad_bl <- advs |>
    filter(ABLFL == "Y") |>
    left_join(vs_bl, by = c("USUBJID", "PARAMCD")) |>
    filter(is.na(.vs_bl) | AVAL != .vs_bl)
  if (nrow(bad_bl) > 0) {
    add("ADVS", "ERROR", "advs-ablfl-not-from-vs",
        "ABLFL='Y' AVAL disagrees with the SDTM VSBLFL record")
  }

  # BASE/CHG/PCHG arithmetic, recomputed from the analysis values
  base_chk <- advs |>
    filter(ABLFL == "Y") |>
    select(USUBJID, PARAMCD, .base = AVAL)
  bad_base <- advs |>
    left_join(base_chk, by = c("USUBJID", "PARAMCD")) |>
    filter(xor(is.na(BASE), is.na(.base)) |
             (!is.na(BASE) & !is.na(.base) & BASE != .base))
  if (nrow(bad_base) > 0) {
    add("ADVS", "ERROR", "advs-base-wrong",
        "BASE disagrees with the ABLFL='Y' AVAL")
  }

  bad_base_sf <- advs |> filter(USUBJID %in% sf_ids, !is.na(BASE))
  if (nrow(bad_base_sf) > 0) {
    add("ADVS", "ERROR", "advs-base-screenfail",
        "screen-failure row with a BASE value")
  }
  bad_base_itt <- advs |>
    filter(USUBJID %in% itt_ids, !is.na(AVAL), is.na(BASE))
  if (nrow(bad_base_itt) > 0) {
    add("ADVS", "ERROR", "advs-base-missing",
        sprintf("%d randomized row(s) with no BASE", nrow(bad_base_itt)))
  }

  bad_chg <- advs |>
    filter(!is.na(BASE)) |>
    mutate(.chg  = .rule_chg(AVAL, BASE, ABLFL),
           .pchg = .rule_pchg(.chg, BASE)) |>
    filter(xor(is.na(CHG), is.na(.chg)) | xor(is.na(PCHG), is.na(.pchg)) |
             (!is.na(CHG) & CHG != .chg) | (!is.na(PCHG) & PCHG != .pchg))
  if (nrow(bad_chg) > 0) {
    add("ADVS", "ERROR", "advs-chg-wrong",
        "CHG/PCHG disagree with AVAL - BASE (or populated on the baseline row)")
  }

  # ANRIND: recomputed from the row's own range, and the ranges themselves
  # must equal the declared spec
  advs_spec <- filter(spec$bds, domain == "ADVS") |>
    select(PARAMCD = paramcd, ANRLO = anrlo, ANRHI = anrhi)
  bad_range <- advs |>
    select(PARAMCD, ANRLO, ANRHI) |>
    distinct() |>
    full_join(advs_spec, by = "PARAMCD") |>
    filter(xor(is.na(ANRLO.x), is.na(ANRLO.y)) |
             (!is.na(ANRLO.x) & !is.na(ANRLO.y) & ANRLO.x != ANRLO.y) |
             (!is.na(ANRHI.x) & !is.na(ANRHI.y) & ANRHI.x != ANRHI.y))
  if (nrow(bad_range) > 0) {
    add("ADVS", "ERROR", "advs-range-spec-drift",
        "ANRLO/ANRHI on ADVS disagree with the declared reference ranges")
  }

  bad_anrind <- advs |>
    mutate(.expect = .rule_anrind(AVAL, ANRLO, ANRHI)) |>
    filter(xor(is.na(ANRIND), is.na(.expect)) |
             (!is.na(ANRIND) & !is.na(.expect) & ANRIND != .expect))
  if (nrow(bad_anrind) > 0) {
    add("ADVS", "ERROR", "advs-anrind-wrong",
        "ANRIND disagrees with the AVAL vs ANRLO/ANRHI comparison")
  }

  # Analysis day: anchored on ADSL TRTSDT
  bad_ady <- advs |>
    left_join(adsl_ref, by = "USUBJID") |>
    mutate(.expect = derive_dy_d(ADT, .ref_trtsdt)) |>
    filter(xor(is.na(ADY), is.na(.expect)) |
             (!is.na(ADY) & !is.na(.expect) & ADY != .expect))
  if (nrow(bad_ady) > 0) {
    add("ADVS", "ERROR", "advs-ady-wrong",
        "ADY disagrees with ADT vs ADSL TRTSDT")
  }
  if (any(advs$ADY == 0, na.rm = TRUE)) {
    add("ADVS", "ERROR", "advs-study-day-zero", "ADY equals zero")
  }

  # Spec coverage: every built parameter must be declared in spec$bds - a
  # new vital sign collected on the CRF without an ADaM spec row shows up
  # here instead of as a silently unconfigured analysis parameter
  advs_undeclared <- setdiff(unique(advs$PARAMCD),
                             spec$bds$paramcd[spec$bds$domain == "ADVS"])
  if (length(advs_undeclared) > 0) {
    add("ADVS", "ERROR", "advs-param-not-in-spec",
        str_flatten_comma(advs_undeclared))
  }

  # ADEG: structure -------------------------------------------------------------
  .req_adeg <- c(
    "STUDYID", "USUBJID", "PARAMCD", "PARAM", "PARAMN", "AVAL", "AVALU",
    "ABLFL", "BASE", "CHG", "PCHG", "ANRIND", "ANRLO", "ANRHI",
    "AVISIT", "AVISITN", "ADT", "ADY"
  )
  miss <- setdiff(.req_adeg, names(adeg))
  if (length(miss) > 0) {
    add("ADEG", "ERROR", "adeg-required-vars", str_flatten_comma(miss))
  }

  # One analysis record per subject / parameter / visit
  adeg_dup <- adeg |>
    count(USUBJID, PARAMCD, AVISITN, name = ".n") |>
    filter(.n > 1)
  if (nrow(adeg_dup) > 0) {
    add("ADEG", "ERROR", "adeg-key-not-unique",
        sprintf(paste("%d duplicated USUBJID/PARAMCD/AVISITN key(s) - a visit",
                      "with two positions for one interval?"), nrow(adeg_dup)))
  }

  # Coverage: exactly the SDTM EG records that carry a result - the NOT
  # DONE row documents a missed ECG and stays an SDTM-only fact
  eg_results <- eg |> filter(!EGSTAT %in% "NOT DONE")
  if (nrow(adeg) != nrow(eg_results)) {
    add("ADEG", "ERROR", "adeg-coverage",
        sprintf("ADEG has %d row(s) but EG carries %d result(s)",
                nrow(adeg), nrow(eg_results)))
  }
  adeg_orphan <- adeg |>
    anti_join(eg_results,
              by = c("USUBJID", PARAMCD = "EGTESTCD", AVISITN = "VISITNUM",
                     AVAL = "EGSTRESN"))
  if (nrow(adeg_orphan) > 0) {
    add("ADEG", "ERROR", "adeg-orphan-record",
        sprintf("%d ADEG row(s) with no matching EG result", nrow(adeg_orphan)))
  }

  # Baseline: exactly one per randomized subject per interval, none for
  # screen failures, and it must be the value SDTM flagged
  bl_multi <- adeg |>
    filter(ABLFL == "Y") |>
    count(USUBJID, PARAMCD, name = ".n") |>
    filter(.n > 1)
  if (nrow(bl_multi) > 0) {
    add("ADEG", "ERROR", "adeg-ablfl-multi",
        sprintf("%d subject/interval(s) with >1 ABLFL='Y'", nrow(bl_multi)))
  }
  bl_missing <- adeg |>
    filter(USUBJID %in% itt_ids, !is.na(AVAL)) |>
    count(USUBJID, PARAMCD, ABLFL) |>
    filter(!any(ABLFL %in% "Y"), .by = c("USUBJID", "PARAMCD"))
  if (nrow(bl_missing) > 0) {
    add("ADEG", "ERROR", "adeg-ablfl-missing",
        sprintf(paste("%d randomized subject/interval(s) with no baseline",
                      "record"), nrow(bl_missing)))
  }
  bl_sf <- adeg |> filter(USUBJID %in% sf_ids, ABLFL == "Y")
  if (nrow(bl_sf) > 0) {
    add("ADEG", "ERROR", "adeg-ablfl-screenfail",
        "screen-failure subject with a baseline flag")
  }
  eg_bl <- eg_results |>
    filter(EGBLFL == "Y") |>
    select(USUBJID, PARAMCD = EGTESTCD, .eg_bl = EGSTRESN)
  bad_bl <- adeg |>
    filter(ABLFL == "Y") |>
    left_join(eg_bl, by = c("USUBJID", "PARAMCD")) |>
    filter(is.na(.eg_bl) | AVAL != .eg_bl)
  if (nrow(bad_bl) > 0) {
    add("ADEG", "ERROR", "adeg-ablfl-not-from-eg",
        "ABLFL='Y' AVAL disagrees with the SDTM EGBLFL record")
  }

  # BASE/CHG/PCHG arithmetic, recomputed from the analysis values
  base_chk <- adeg |>
    filter(ABLFL == "Y") |>
    select(USUBJID, PARAMCD, .base = AVAL)
  bad_base <- adeg |>
    left_join(base_chk, by = c("USUBJID", "PARAMCD")) |>
    filter(xor(is.na(BASE), is.na(.base)) |
             (!is.na(BASE) & !is.na(.base) & BASE != .base))
  if (nrow(bad_base) > 0) {
    add("ADEG", "ERROR", "adeg-base-wrong",
        "BASE disagrees with the ABLFL='Y' AVAL")
  }

  bad_base_sf <- adeg |> filter(USUBJID %in% sf_ids, !is.na(BASE))
  if (nrow(bad_base_sf) > 0) {
    add("ADEG", "ERROR", "adeg-base-screenfail",
        "screen-failure row with a BASE value")
  }
  bad_base_itt <- adeg |>
    filter(USUBJID %in% itt_ids, !is.na(AVAL), is.na(BASE))
  if (nrow(bad_base_itt) > 0) {
    add("ADEG", "ERROR", "adeg-base-missing",
        sprintf("%d randomized row(s) with no BASE", nrow(bad_base_itt)))
  }

  bad_chg <- adeg |>
    filter(!is.na(BASE)) |>
    mutate(.chg  = .rule_chg(AVAL, BASE, ABLFL),
           .pchg = .rule_pchg(.chg, BASE)) |>
    filter(xor(is.na(CHG), is.na(.chg)) | xor(is.na(PCHG), is.na(.pchg)) |
             (!is.na(CHG) & CHG != .chg) | (!is.na(PCHG) & PCHG != .pchg))
  if (nrow(bad_chg) > 0) {
    add("ADEG", "ERROR", "adeg-chg-wrong",
        "CHG/PCHG disagree with AVAL - BASE (or populated on the baseline row)")
  }

  # ANRIND: recomputed from the row's own range, and the ranges themselves
  # must equal the declared spec - EG collects no ranges of its own, the
  # spec$bds ADEG rows are the SAP stand-in
  adeg_spec <- filter(spec$bds, domain == "ADEG") |>
    select(PARAMCD = paramcd, ANRLO = anrlo, ANRHI = anrhi)
  bad_range <- adeg |>
    select(PARAMCD, ANRLO, ANRHI) |>
    distinct() |>
    full_join(adeg_spec, by = "PARAMCD") |>
    filter(xor(is.na(ANRLO.x), is.na(ANRLO.y)) |
             (!is.na(ANRLO.x) & !is.na(ANRLO.y) & ANRLO.x != ANRLO.y) |
             (!is.na(ANRHI.x) & !is.na(ANRHI.y) & ANRHI.x != ANRHI.y))
  if (nrow(bad_range) > 0) {
    add("ADEG", "ERROR", "adeg-range-spec-drift",
        "ANRLO/ANRHI on ADEG disagree with the declared reference ranges")
  }

  bad_anrind <- adeg |>
    mutate(.expect = .rule_anrind(AVAL, ANRLO, ANRHI)) |>
    filter(xor(is.na(ANRIND), is.na(.expect)) |
             (!is.na(ANRIND) & !is.na(.expect) & ANRIND != .expect))
  if (nrow(bad_anrind) > 0) {
    add("ADEG", "ERROR", "adeg-anrind-wrong",
        "ANRIND disagrees with the AVAL vs ANRLO/ANRHI comparison")
  }

  # Analysis day: anchored on ADSL TRTSDT
  bad_ady <- adeg |>
    left_join(adsl_ref, by = "USUBJID") |>
    mutate(.expect = derive_dy_d(ADT, .ref_trtsdt)) |>
    filter(xor(is.na(ADY), is.na(.expect)) |
             (!is.na(ADY) & !is.na(.expect) & ADY != .expect))
  if (nrow(bad_ady) > 0) {
    add("ADEG", "ERROR", "adeg-ady-wrong",
        "ADY disagrees with ADT vs ADSL TRTSDT")
  }
  if (any(adeg$ADY == 0, na.rm = TRUE)) {
    add("ADEG", "ERROR", "adeg-study-day-zero", "ADY equals zero")
  }

  # Spec coverage: every built parameter must be declared in spec$bds - a
  # new ECG interval collected on the CRF without an ADaM spec row shows up
  # here instead of as a silently unconfigured analysis parameter
  adeg_undeclared <- setdiff(unique(adeg$PARAMCD),
                             spec$bds$paramcd[spec$bds$domain == "ADEG"])
  if (length(adeg_undeclared) > 0) {
    add("ADEG", "ERROR", "adeg-param-not-in-spec",
        str_flatten_comma(adeg_undeclared))
  }

  # ADLB: structure -------------------------------------------------------------
  .req_adlb <- c(
    "STUDYID", "USUBJID", "PARAMCD", "PARAM", "PARAMN", "LBCAT",
    "AVAL", "AVALU", "ABLFL", "BASE", "BNRIND", "CHG", "PCHG",
    "ANRIND", "ANRLO", "ANRHI", "AVISIT", "AVISITN", "ADT", "ADY"
  )
  miss <- setdiff(.req_adlb, names(adlb))
  if (length(miss) > 0) {
    add("ADLB", "ERROR", "adlb-required-vars", str_flatten_comma(miss))
  }

  adlb_dup <- adlb |>
    count(USUBJID, PARAMCD, AVISITN, name = ".n") |>
    filter(.n > 1)
  if (nrow(adlb_dup) > 0) {
    add("ADLB", "ERROR", "adlb-key-not-unique",
        sprintf("%d duplicated USUBJID/PARAMCD/AVISITN key(s)", nrow(adlb_dup)))
  }

  # Coverage: exactly the SDTM LB records that carry a result
  lb_results <- lb |> filter(!is.na(LBSTRESN))
  if (nrow(adlb) != nrow(lb_results)) {
    add("ADLB", "ERROR", "adlb-coverage",
        sprintf("ADLB has %d row(s) but LB carries %d result(s)",
                nrow(adlb), nrow(lb_results)))
  }
  adlb_orphan <- adlb |>
    anti_join(lb_results,
              by = c("USUBJID", PARAMCD = "LBTESTCD", AVISITN = "VISITNUM",
                     AVAL = "LBSTRESN"))
  if (nrow(adlb_orphan) > 0) {
    add("ADLB", "ERROR", "adlb-orphan-record",
        sprintf("%d ADLB row(s) with no matching LB result", nrow(adlb_orphan)))
  }

  # Baseline: one per randomized subject per analyte, none for screen
  # failures, and it must be the record SDTM flagged
  bl_multi <- adlb |>
    filter(ABLFL == "Y") |>
    count(USUBJID, PARAMCD, name = ".n") |>
    filter(.n > 1)
  if (nrow(bl_multi) > 0) {
    add("ADLB", "ERROR", "adlb-ablfl-multi",
        sprintf("%d subject/analyte(s) with >1 ABLFL='Y'", nrow(bl_multi)))
  }
  bl_missing <- adlb |>
    filter(USUBJID %in% itt_ids, !is.na(AVAL)) |>
    count(USUBJID, PARAMCD, ABLFL) |>
    filter(!any(ABLFL %in% "Y"), .by = c("USUBJID", "PARAMCD"))
  if (nrow(bl_missing) > 0) {
    add("ADLB", "ERROR", "adlb-ablfl-missing",
        sprintf("%d randomized subject/analyte(s) with no baseline record",
                nrow(bl_missing)))
  }
  bl_sf <- adlb |> filter(USUBJID %in% sf_ids, ABLFL == "Y")
  if (nrow(bl_sf) > 0) {
    add("ADLB", "ERROR", "adlb-ablfl-screenfail",
        "screen-failure subject with a baseline flag")
  }
  lb_bl <- lb_results |>
    filter(LBBLFL == "Y") |>
    select(USUBJID, PARAMCD = LBTESTCD, .lb_bl = LBSTRESN, .lb_blind = LBNRIND)
  bad_bl <- adlb |>
    filter(ABLFL == "Y") |>
    left_join(lb_bl, by = c("USUBJID", "PARAMCD")) |>
    filter(is.na(.lb_bl) | AVAL != .lb_bl)
  if (nrow(bad_bl) > 0) {
    add("ADLB", "ERROR", "adlb-ablfl-not-from-lb",
        "ABLFL='Y' AVAL disagrees with the SDTM LBBLFL record")
  }

  # BASE/BNRIND/CHG/PCHG recomputed from the analysis values
  base_chk <- adlb |>
    filter(ABLFL == "Y") |>
    select(USUBJID, PARAMCD, .base = AVAL, .bnrind = ANRIND)
  bad_base <- adlb |>
    left_join(base_chk, by = c("USUBJID", "PARAMCD")) |>
    filter(xor(is.na(BASE), is.na(.base)) |
             (!is.na(BASE) & !is.na(.base) & BASE != .base) |
             xor(is.na(BNRIND), is.na(.bnrind)) |
             (!is.na(BNRIND) & !is.na(.bnrind) & BNRIND != .bnrind))
  if (nrow(bad_base) > 0) {
    add("ADLB", "ERROR", "adlb-base-wrong",
        "BASE/BNRIND disagree with the ABLFL='Y' record")
  }
  bad_base_sf <- adlb |> filter(USUBJID %in% sf_ids, !is.na(BASE) | !is.na(BNRIND))
  if (nrow(bad_base_sf) > 0) {
    add("ADLB", "ERROR", "adlb-base-screenfail",
        "screen-failure row with a BASE or BNRIND value")
  }
  bad_base_itt <- adlb |>
    filter(USUBJID %in% itt_ids, !is.na(AVAL), is.na(BASE))
  if (nrow(bad_base_itt) > 0) {
    add("ADLB", "ERROR", "adlb-base-missing",
        sprintf("%d randomized row(s) with no BASE", nrow(bad_base_itt)))
  }
  bad_chg <- adlb |>
    filter(!is.na(BASE)) |>
    mutate(.chg  = .rule_chg(AVAL, BASE, ABLFL),
           .pchg = .rule_pchg(.chg, BASE)) |>
    filter(xor(is.na(CHG), is.na(.chg)) | xor(is.na(PCHG), is.na(.pchg)) |
             (!is.na(CHG) & CHG != .chg) | (!is.na(PCHG) & PCHG != .pchg))
  if (nrow(bad_chg) > 0) {
    add("ADLB", "ERROR", "adlb-chg-wrong",
        "CHG/PCHG disagree with AVAL - BASE (or populated on the baseline row)")
  }

  # ANRIND: recomputed from the row's own limits, and the whole range triple
  # must match the SDTM record - the ranges are collected data here
  bad_anrind <- adlb |>
    mutate(.expect = .rule_anrind(AVAL, ANRLO, ANRHI)) |>
    filter(xor(is.na(ANRIND), is.na(.expect)) |
             (!is.na(ANRIND) & !is.na(.expect) & ANRIND != .expect))
  if (nrow(bad_anrind) > 0) {
    add("ADLB", "ERROR", "adlb-anrind-wrong",
        "ANRIND disagrees with the AVAL vs ANRLO/ANRHI comparison")
  }
  lb_rng <- lb_results |>
    transmute(USUBJID, PARAMCD = LBTESTCD, AVISITN = VISITNUM,
              .lb_lo = LBSTNRLO, .lb_hi = LBSTNRHI, .lb_ind = LBNRIND)
  bad_rng <- adlb |>
    left_join(lb_rng, by = c("USUBJID", "PARAMCD", "AVISITN")) |>
    filter(is.na(.lb_lo) |
             (!is.na(ANRLO) & ANRLO != .lb_lo) |
             (!is.na(ANRHI) & ANRHI != .lb_hi) |
             xor(is.na(ANRIND), is.na(.lb_ind)) |
             (!is.na(ANRIND) & !is.na(.lb_ind) & ANRIND != .lb_ind))
  if (nrow(bad_rng) > 0) {
    add("ADLB", "ERROR", "adlb-range-not-from-lb",
        "ANRLO/ANRHI/ANRIND disagree with the SDTM record's own range")
  }

  bad_ady <- adlb |>
    left_join(adsl_ref, by = "USUBJID") |>
    mutate(.expect = derive_dy_d(ADT, .ref_trtsdt)) |>
    filter(xor(is.na(ADY), is.na(.expect)) |
             (!is.na(ADY) & !is.na(.expect) & ADY != .expect))
  if (nrow(bad_ady) > 0) {
    add("ADLB", "ERROR", "adlb-ady-wrong",
        "ADY disagrees with ADT vs ADSL TRTSDT")
  }
  if (any(adlb$ADY == 0, na.rm = TRUE)) {
    add("ADLB", "ERROR", "adlb-study-day-zero", "ADY equals zero")
  }

  adlb_undeclared <- setdiff(unique(adlb$PARAMCD),
                             spec$bds$paramcd[spec$bds$domain == "ADLB"])
  if (length(adlb_undeclared) > 0) {
    add("ADLB", "ERROR", "adlb-param-not-in-spec",
        str_flatten_comma(adlb_undeclared))
  }

  # ADQS: structure -------------------------------------------------------------
  .req_adqs <- c(
    "STUDYID", "USUBJID", "PARAMCD", "PARAM", "PARAMN", "AVAL", "AVALU",
    "ABLFL", "BASE", "CHG", "PCHG", "ANRIND", "ANRLO", "ANRHI",
    "AVISIT", "AVISITN", "ADT", "ADY"
  )
  miss <- setdiff(.req_adqs, names(adqs))
  if (length(miss) > 0) {
    add("ADQS", "ERROR", "adqs-required-vars", str_flatten_comma(miss))
  }

  # One analysis record per subject / parameter / visit
  adqs_dup <- adqs |>
    count(USUBJID, PARAMCD, AVISITN, name = ".n") |>
    filter(.n > 1)
  if (nrow(adqs_dup) > 0) {
    add("ADQS", "ERROR", "adqs-key-not-unique",
        sprintf(paste("%d duplicated USUBJID/PARAMCD/AVISITN key(s) - a visit",
                      "with two answers for one item?"), nrow(adqs_dup)))
  }

  # The spec declares both sides: the item parameters (spec$bds) and the
  # derived totals (spec$totals). The record-level checks against SDTM
  # are items only - a derived parameter has no QS record to be orphaned
  # against; the totals are owned by the recomputes that close the block.
  qs_items_spec <- spec$bds$paramcd[spec$bds$domain == "ADQS"]
  tot_spec <- filter(spec$totals, domain == "ADQS")
  adqs_items <- adqs |> filter(PARAMCD %in% qs_items_spec)
  adqs_tot <- adqs |> filter(PARAMCD %in% tot_spec$paramcd)

  # Coverage: exactly the performed QS records the spec carries - NOT DONE
  # rows document a missed form and stay an SDTM-only fact (the ADEG rule)
  qs_results <- qs |>
    filter(!QSSTAT %in% "NOT DONE", QSTESTCD %in% qs_items_spec)
  if (nrow(adqs_items) != nrow(qs_results)) {
    add("ADQS", "ERROR", "adqs-coverage",
        sprintf("ADQS has %d item row(s) but QS carries %d performed result(s)",
                nrow(adqs_items), nrow(qs_results)))
  }
  adqs_orphan <- adqs_items |>
    anti_join(qs_results,
              by = c("USUBJID", PARAMCD = "QSTESTCD", AVISITN = "VISITNUM",
                     AVAL = "QSSTRESN"))
  if (nrow(adqs_orphan) > 0) {
    add("ADQS", "ERROR", "adqs-orphan-record",
        sprintf("%d ADQS item row(s) with no matching QS result",
                nrow(adqs_orphan)))
  }

  # Baseline: exactly one per randomized subject per parameter, none for
  # screen failures; for the items it must be the record SDTM flagged. The
  # total's baseline is derived (QS has no QSBLFL for a parameter absent
  # from SDTM), so it is anchored by the builder and held to account
  # through BASE/CHG below, not against SDTM.
  bl_multi <- adqs |>
    filter(ABLFL == "Y") |>
    count(USUBJID, PARAMCD, name = ".n") |>
    filter(.n > 1)
  if (nrow(bl_multi) > 0) {
    add("ADQS", "ERROR", "adqs-ablfl-multi",
        sprintf("%d subject/parameter(s) with >1 ABLFL='Y'", nrow(bl_multi)))
  }
  bl_missing <- adqs |>
    filter(USUBJID %in% itt_ids, !is.na(AVAL)) |>
    count(USUBJID, PARAMCD, ABLFL) |>
    filter(!any(ABLFL %in% "Y"), .by = c("USUBJID", "PARAMCD"))
  if (nrow(bl_missing) > 0) {
    add("ADQS", "ERROR", "adqs-ablfl-missing",
        sprintf(paste("%d randomized subject/parameter(s) with no baseline",
                      "record"), nrow(bl_missing)))
  }
  bl_sf <- adqs |> filter(USUBJID %in% sf_ids, ABLFL == "Y")
  if (nrow(bl_sf) > 0) {
    add("ADQS", "ERROR", "adqs-ablfl-screenfail",
        "screen-failure subject with a baseline flag")
  }
  qs_bl <- qs_results |>
    filter(QSBLFL == "Y") |>
    select(USUBJID, PARAMCD = QSTESTCD, .qs_bl = QSSTRESN)
  bad_bl <- adqs_items |>
    filter(ABLFL == "Y") |>
    left_join(qs_bl, by = c("USUBJID", "PARAMCD")) |>
    filter(is.na(.qs_bl) | AVAL != .qs_bl)
  if (nrow(bad_bl) > 0) {
    add("ADQS", "ERROR", "adqs-ablfl-not-from-qs",
        "ABLFL='Y' AVAL disagrees with the SDTM QSBLFL record")
  }

  # BASE/CHG/PCHG arithmetic, recomputed from the analysis values
  base_chk <- adqs |>
    filter(ABLFL == "Y") |>
    select(USUBJID, PARAMCD, .base = AVAL)
  bad_base <- adqs |>
    left_join(base_chk, by = c("USUBJID", "PARAMCD")) |>
    filter(xor(is.na(BASE), is.na(.base)) |
             (!is.na(BASE) & !is.na(.base) & BASE != .base))
  if (nrow(bad_base) > 0) {
    add("ADQS", "ERROR", "adqs-base-wrong",
        "BASE disagrees with the ABLFL='Y' AVAL")
  }
  bad_base_sf <- adqs |> filter(USUBJID %in% sf_ids, !is.na(BASE))
  if (nrow(bad_base_sf) > 0) {
    add("ADQS", "ERROR", "adqs-base-screenfail",
        "screen-failure row with a BASE value")
  }
  bad_base_itt <- adqs |>
    filter(USUBJID %in% itt_ids, !is.na(AVAL), is.na(BASE))
  if (nrow(bad_base_itt) > 0) {
    add("ADQS", "ERROR", "adqs-base-missing",
        sprintf("%d randomized row(s) with no BASE", nrow(bad_base_itt)))
  }
  bad_chg <- adqs |>
    filter(!is.na(BASE)) |>
    mutate(.chg  = .rule_chg(AVAL, BASE, ABLFL),
           .pchg = .rule_pchg(.chg, BASE)) |>
    filter(xor(is.na(CHG), is.na(.chg)) | xor(is.na(PCHG), is.na(.pchg)) |
             (!is.na(CHG) & CHG != .chg) | (!is.na(PCHG) & PCHG != .pchg))
  if (nrow(bad_chg) > 0) {
    add("ADQS", "ERROR", "adqs-chg-wrong",
        "CHG/PCHG disagree with AVAL - BASE (or populated on the baseline row)")
  }

  # ANRIND: recomputed from the row's own range - the spec-declared range
  # for a total, none for the ordinal items (ANRIND stays missing there by
  # design, the WEIGHT/HEIGHT precedent)
  bad_anrind <- adqs |>
    mutate(.expect = .rule_anrind(AVAL, ANRLO, ANRHI)) |>
    filter(xor(is.na(ANRIND), is.na(.expect)) |
             (!is.na(ANRIND) & !is.na(.expect) & ANRIND != .expect))
  if (nrow(bad_anrind) > 0) {
    add("ADQS", "ERROR", "adqs-anrind-wrong",
        "ANRIND disagrees with the AVAL vs ANRLO/ANRHI comparison")
  }

  # The ranges themselves must equal the declared spec - the ADVS/ADEG
  # drift check on ADQS's two range sources: the spec$bds range for an
  # item (NA/NA, ordinal answers have no absolute norm) and the spec$totals
  # range for a total - so a drifted range cannot hide behind a
  # classification that still holds
  adqs_rng_spec <- bind_rows(
    filter(spec$bds, domain == "ADQS") |>
      select(PARAMCD = paramcd, ANRLO = anrlo, ANRHI = anrhi),
    filter(spec$totals, domain == "ADQS") |>
      select(PARAMCD = paramcd, ANRLO = anrlo, ANRHI = anrhi)
  )
  bad_range <- adqs |>
    select(PARAMCD, ANRLO, ANRHI) |>
    distinct() |>
    full_join(adqs_rng_spec, by = "PARAMCD") |>
    filter(xor(is.na(ANRLO.x), is.na(ANRLO.y)) |
             (!is.na(ANRLO.x) & !is.na(ANRLO.y) & ANRLO.x != ANRLO.y) |
             (!is.na(ANRHI.x) & !is.na(ANRHI.y) & ANRHI.x != ANRHI.y))
  if (nrow(bad_range) > 0) {
    add("ADQS", "ERROR", "adqs-range-spec-drift",
        "ANRLO/ANRHI on ADQS disagree with the declared reference ranges")
  }

  # Analysis day: anchored on ADSL TRTSDT
  bad_ady <- adqs |>
    left_join(adsl_ref, by = "USUBJID") |>
    mutate(.expect = derive_dy_d(ADT, .ref_trtsdt)) |>
    filter(xor(is.na(ADY), is.na(.expect)) |
             (!is.na(ADY) & !is.na(.expect) & ADY != .expect))
  if (nrow(bad_ady) > 0) {
    add("ADQS", "ERROR", "adqs-ady-wrong",
        "ADY disagrees with ADT vs ADSL TRTSDT")
  }
  if (any(adqs$ADY == 0, na.rm = TRUE)) {
    add("ADQS", "ERROR", "adqs-study-day-zero", "ADY equals zero")
  }

  # Spec coverage: every built parameter must be declared - the items in
  # spec$bds, the totals in spec$totals - so a questionnaire item collected
  # on the CRF without an ADaM spec row shows up here instead of silently
  # corrupting the totals' all-required count
  adqs_undeclared <- setdiff(unique(adqs$PARAMCD),
                             c(qs_items_spec, tot_spec$paramcd))
  if (length(adqs_undeclared) > 0) {
    add("ADQS", "ERROR", "adqs-param-not-in-spec",
        str_flatten_comma(adqs_undeclared))
  }

  # The built-side sweep cannot see what the builder dropped before it ever
  # reached ADQS: derive_adqs() keeps spec'd items only, so an item
  # performed in QS but missing from spec$bds vanishes from the items and
  # from the totals' all-required count in silence - the disagreement is
  # checked straight off the SDTM QS, with the coverage check's NOT DONE
  # filtering
  qs_undeclared <- setdiff(
    unique(qs$QSTESTCD[!qs$QSSTAT %in% "NOT DONE"]),
    qs_items_spec
  )
  if (length(qs_undeclared) > 0) {
    add("ADQS", "ERROR", "adqs-item-not-in-spec",
        str_flatten_comma(qs_undeclared))
  }

  # The recompute-don't-trust centrepiece: the total is derived, so the
  # validator derives it too - the same spec$totals rows, the same
  # performed-items frame and the same all-required rule the builder
  # applies, straight off the SDTM QS the build started from. The nrow
  # guard is load-bearing, not redundant: a totals-less spec makes pmap
  # return an empty list whose bind_rows() carries no join columns, and
  # the joins below would error on a spec that declares no totals at all.
  if (nrow(tot_spec) > 0) {
    expected_totals <- pmap(tot_spec, \(domain, paramcd, param, paramn, anrlo, anrhi, src_items) {
      items <- str_split_1(src_items, ";")
      qs |>
        filter(!QSSTAT %in% "NOT DONE", QSTESTCD %in% items) |>
        summarise(
          .n_items = n(),
          .any_na  = anyNA(QSSTRESN),
          AVAL     = sum(QSSTRESN),
          .by = c(USUBJID, VISITNUM)
        ) |>
        filter(.n_items == length(items), !.any_na) |>
        transmute(USUBJID, PARAMCD = paramcd, AVISITN = VISITNUM,
                  .exp_aval = AVAL)
    }) |>
      bind_rows()

    bad_tot <- adqs_tot |>
      select(USUBJID, PARAMCD, AVISITN, AVAL) |>
      inner_join(expected_totals, by = c("USUBJID", "PARAMCD", "AVISITN")) |>
      filter(xor(is.na(AVAL), is.na(.exp_aval)) |
               (!is.na(AVAL) & !is.na(.exp_aval) & AVAL != .exp_aval))
    if (nrow(bad_tot) > 0) {
      add("ADQS", "ERROR", "adqs-total-wrong",
          sprintf(paste("%d total row(s) whose AVAL is not the sum of their",
                        "SDTM QS items"), nrow(bad_tot)))
    }

    # Coverage (totals), both directions: a visit with every item present
    # must have its total, and a total may not exist where an item is
    # missing or unparseable - no proration to hide behind
    lost_tot <- anti_join(expected_totals,
                          select(adqs_tot, USUBJID, PARAMCD, AVISITN),
                          by = c("USUBJID", "PARAMCD", "AVISITN"))
    extra_tot <- anti_join(select(adqs_tot, USUBJID, PARAMCD, AVISITN),
                           expected_totals,
                           by = c("USUBJID", "PARAMCD", "AVISITN"))
    if (nrow(lost_tot) + nrow(extra_tot) > 0) {
      add("ADQS", "ERROR", "adqs-total-coverage",
          sprintf(paste("%d subject/visit(s) where the total and the complete",
                        "set of QS items disagree about existing"),
                  nrow(lost_tot) + nrow(extra_tot)))
    }
  }

  # ADTTE: structure -------------------------------------------------------------
  .req_adtte <- c(
    "STUDYID", "USUBJID", "PARAMCD", "PARAM", "PARAMN", "AVAL", "AVALU",
    "STARTDT", "ADT", "EVNTDESC", "CNSDTDSC", "SRCDOM", "SRCVAR", "SRCSEQ"
  )
  miss <- setdiff(.req_adtte, names(adtte))
  if (length(miss) > 0) {
    add("ADTTE", "ERROR", "adtte-required-vars", str_flatten_comma(miss))
  }
  adtte_dup <- adtte |>
    count(USUBJID, PARAMCD, name = ".n") |>
    filter(.n > 1)
  if (nrow(adtte_dup) > 0) {
    add("ADTTE", "ERROR", "adtte-key-not-unique",
        sprintf("%d duplicated USUBJID/PARAMCD key(s)", nrow(adtte_dup)))
  }
  bad_param <- setdiff(unique(adtte$PARAMCD), c("OS", "TTAE"))
  if (length(bad_param) > 0) {
    add("ADTTE", "ERROR", "adtte-param-unexpected",
        str_flatten_comma(bad_param))
  }

  # Coverage: exactly one record per ADSL subject per parameter - a
  # subject the TTE drops is a safety population that vanished
  expect_pairs <- merge(adsl["USUBJID"], data.frame(PARAMCD = c("OS", "TTAE")))
  missing_pairs <- anti_join(expect_pairs, adtte,
                             by = c("USUBJID", "PARAMCD"))
  if (nrow(missing_pairs) > 0) {
    add("ADTTE", "ERROR", "adtte-coverage",
        sprintf("%d subject/parameter pair(s) missing", nrow(missing_pairs)))
  }

  # AVAL recomputes through the shared day rule; the traceability pair
  # (SRCDOM/SRCVAR) must be present wherever a date is
  bad_aval <- adtte |>
    mutate(.expect = derive_dy_d(ADT, STARTDT)) |>
    filter(xor(is.na(AVAL), is.na(.expect)) |
             (!is.na(AVAL) & !is.na(.expect) & AVAL != .expect))
  if (nrow(bad_aval) > 0) {
    add("ADTTE", "ERROR", "adtte-aval-wrong",
        "AVAL disagrees with ADT vs STARTDT through the shared day rule")
  }
  bad_trace <- adtte |> filter(!is.na(ADT) & (is.na(SRCDOM) | is.na(SRCVAR)))
  if (nrow(bad_trace) > 0) {
    add("ADTTE", "ERROR", "adtte-source-missing",
        sprintf("%d dated row(s) without SRCDOM/SRCVAR", nrow(bad_trace)))
  }

  # OS is the ADSL death story read back: the deaths are events dated at
  # DTHDT, everyone treated and alive is censored at EOSDT
  os <- adtte |> filter(PARAMCD == "OS")
  os_bad_death <- os |>
    left_join(select(adsl, USUBJID, .dthdt = DTHDT, .dthfl = DTHFL,
                     .eosdt = EOSDT, .trtsdt = TRTSDT), by = "USUBJID") |>
    filter((.dthfl %in% "Y") != (EVNTDESC %in% "Death") |
             (EVNTDESC %in% "Death" & !is.na(ADT) & ADT != .dthdt) |
             (!EVNTDESC %in% "Death" & !is.na(ADT) & ADT != .eosdt))
  if (nrow(os_bad_death) > 0) {
    add("ADTTE", "ERROR", "adtte-os-not-from-adsl",
        "OS dates/events disagree with ADSL DTHFL/DTHDT/EOSDT")
  }

  # TTAE is the first treatment-emergent AE recomputed from ADAE, both
  # directions: an event row must match it, a censored row must have none
  ttae <- adtte |> filter(PARAMCD == "TTAE")
  first_ae <- adae |>
    filter(TRTEMFL %in% "Y") |>
    arrange(USUBJID, ASTDT, ASEQ) |>
    distinct(USUBJID, .keep_all = TRUE) |>
    select(USUBJID, .fae_adt = ASTDT, .fae_seq = ASEQ)
  ttae_chk <- ttae |>
    left_join(first_ae, by = "USUBJID") |>
    mutate(.is_event = EVNTDESC %in% "First treatment-emergent adverse event")
  ttae_bad_event <- ttae_chk |>
    filter(.is_event & (is.na(.fae_adt) |
                          (!is.na(ADT) & ADT != .fae_adt) |
                          (!is.na(SRCSEQ) & SRCSEQ != .fae_seq)))
  ttae_bad_censor <- ttae_chk |>
    filter(!.is_event & !is.na(.fae_adt))
  if (nrow(ttae_bad_event) + nrow(ttae_bad_censor) > 0) {
    add("ADTTE", "ERROR", "adtte-ttae-not-first-te-ae",
        "TTAE events/censoring disagree with the first TE AE in ADAE")
  }

  # event/censor description coherence: exactly one description per row,
  # and only where a date exists
  # unanchored screen-failure rows legitimately carry neither description;
  # any dated row must carry exactly one, matching event vs censor
  desc_bad <- adtte |>
    filter((!is.na(EVNTDESC) & !is.na(CNSDTDSC)) |
             (!is.na(EVNTDESC) & is.na(ADT)) |
             (!is.na(CNSDTDSC) & is.na(ADT)) |
             (is.na(EVNTDESC) & is.na(CNSDTDSC) & !is.na(ADT)))
  if (nrow(desc_bad) > 0) {
    add("ADTTE", "ERROR", "adtte-eventdesc-coherence",
        "EVNTDESC/CNSDTDSC disagree with each other or with ADT")
  }

  report <- bind_rows(issues)
  if (nrow(report) == 0) report <- .new_issue()
  report
}

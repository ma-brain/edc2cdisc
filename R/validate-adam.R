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
#' TRTEMFL window and SUPP merge-back on ADAE, and for the BDS datasets
#' (ADVS / ADLB) the baseline anchors, BASE/CHG/PCHG arithmetic, ANRIND
#' against the row's own range, and coverage against the SDTM source
#' records.
#'
#' @param adsl,adae,advs,adlb The mapped ADaM datasets
#' @param dm,ds,ae,vs,lb,suppae The SDTM source datasets the ADaM layer was
#'   built from
#' @param spec A `study_spec`; the ADVS rows of `spec$bds` declare the
#'   reference ranges the built ADVS is checked against - a silent change
#'   on either side (in `spec$bds` or in [derive_advs()]) trips the
#'   range-drift check, and a parameter missing from `spec$bds` trips the
#'   coverage check
#' @return An issue tibble: domain, severity ("ERROR" / "WARN"), check,
#'   detail. Empty when everything passes.
#' @export
#' @examples
#' ext <- file.path(tempdir(), "extract-adam")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' built <- build_all(ext)
#' issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADVS,
#'                         built$adam$ADLB, built$sdtm$DM, built$sdtm$DS,
#'                         built$sdtm$AE, built$sdtm$VS, built$sdtm$LB,
#'                         built$sdtm$SUPPAE, spec_synth01)
#' issues                                  # empty: the build is clean
validate_adam <- function(adsl, adae, advs, adlb,
                          dm, ds, ae, vs, lb, suppae,
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
    filter(!is.na(TRTSDT), !is.na(TRTEDT),
           TRTDURD != .rule_trtdurd(TRTSDT, TRTEDT))
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
  # downstream expects. Range check matches the protocol's adult population.
  bad_age <- adsl |> filter(is.na(AGE) | AGE < 18 | AGE > 100)
  if (nrow(bad_age) > 0) {
    add("ADSL", "ERROR", "age-out-of-range",
        sprintf("%d row(s) with AGE missing or outside 18-100", nrow(bad_age)))
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
    filter(EOSSTT != "DISCONTINUED" | DCSREAS != "DEATH" |
             is.na(TRTSDT) | DTHDT < TRTSDT |
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
  bad_te <- te_expected |> filter(TRTEMFL != .expect)
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
    filter(ASTDY != .expect)
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
    filter(is.na(PARAMCD) |
             xor(is.na(ANRLO.x), is.na(ANRLO.y)) |
             (!is.na(ANRLO.x) & !is.na(ANRLO.y) & ANRLO.x != ANRLO.y) |
             (!is.na(ANRHI.x) & !is.na(ANRHI.y) & ANRHI.x != ANRHI.y))
  if (nrow(bad_range) > 0) {
    add("ADVS", "ERROR", "advs-range-spec-drift",
        "ANRLO/ANRHI on ADVS disagree with the declared reference ranges")
  }

  bad_anrind <- advs |>
    mutate(.expect = case_when(
      is.na(ANRLO) ~ NA_character_,
      AVAL < ANRLO ~ "LOW",
      AVAL > ANRHI ~ "HIGH",
      .default     = "NORMAL"
    )) |>
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
    filter(ADY != .expect)
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
    mutate(.expect = case_when(
      is.na(ANRLO) | is.na(ANRHI) ~ NA_character_,
      AVAL < ANRLO                ~ "LOW",
      AVAL > ANRHI                ~ "HIGH",
      .default                    = "NORMAL"
    )) |>
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

  report <- bind_rows(issues)
  if (nrow(report) == 0) report <- .new_issue()
  report
}

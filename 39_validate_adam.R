# ============================================================================
# Title:   Content validation of the derived ADaM datasets
# Purpose: Same contract as 22_validate.R - cheap checks that catch the
#          mistakes this project is designed to make you think about - but
#          for ADaM, where the checks are about derivations and coherence,
#          not SDTM structure. stop() on any ERROR.
# Input:   data/adam/{adsl,adae}.rds + the SDTM domains they were built from
# Output:  a printed issue report
# ============================================================================

adsl   <- read_rds(file.path(paths$adam, "adsl.rds"))
adae   <- read_rds(file.path(paths$adam, "adae.rds"))
dm     <- read_rds(file.path(paths$sdtm, "dm.rds"))
ds     <- read_rds(file.path(paths$sdtm, "ds.rds"))
ae     <- read_rds(file.path(paths$sdtm, "ae.rds"))
suppae <- read_rds(file.path(paths$sdtm, "suppae.rds"))

.req_adsl <- c(
  "STUDYID", "USUBJID", "SUBJID", "SITEID", "COUNTRY", "AGE", "AGEU",
  "SEX", "RACE", "ETHNIC", "BRTHDTC", "BRTHDT", "BRTHDTF",
  "TRT01P", "TRT01PCD", "TRT01A", "TRT01ACD",
  "ENRLSTDT", "TRTSDT", "TRTSDTF", "TRTEDT", "TRTEDTF", "TRTDURD",
  "SAFFL", "ITTFL", "EOSDT", "EOSDTF", "EOSSTT", "DCSREAS",
  "DTHDT", "DTHDTF", "LSTALVDT", "DTHFL"
)

.issues <- list()
.add <- function(severity, check, detail) {
  .issues[[length(.issues) + 1L]] <<-
    tibble(severity = severity, check = check, detail = detail)
}

# Structure -------------------------------------------------------------------
miss <- setdiff(.req_adsl, names(adsl))
if (length(miss) > 0) .add("ERROR", "required-vars", str_flatten_comma(miss))

if (anyDuplicated(adsl$USUBJID) > 0) {
  .add("ERROR", "adsl-one-row-per-subject", "USUBJID duplicated")
}
orphan <- setdiff(adsl$USUBJID, dm$USUBJID)
if (length(orphan) > 0) .add("ERROR", "adsl-not-in-dm", str_flatten_comma(orphan))
missing_subj <- setdiff(dm$USUBJID, adsl$USUBJID)
if (length(missing_subj) > 0) {
  .add("ERROR", "dm-not-in-adsl", str_flatten_comma(missing_subj))
}

# Population flags ------------------------------------------------------------
bad_fl <- adsl |> filter(!SAFFL %in% c("Y", "N") | !ITTFL %in% c("Y", "N"))
if (nrow(bad_fl) > 0) {
  .add("ERROR", "pop-flag-ct", sprintf("%d row(s) with SAFFL/ITTFL not Y/N",
                                       nrow(bad_fl)))
}

# ITTFL='Y' is "randomized", and the arm comes with it; ITTFL='N' must
# carry no arm at all.
bad_itt <- adsl |>
  filter(ITTFL == "Y" &
           (is.na(TRT01P) | is.na(TRT01PCD) | is.na(TRT01A) | is.na(TRT01ACD))) |>
  bind_rows(adsl |> filter(ITTFL == "N" & (!is.na(TRT01P) | !is.na(TRT01A))))
if (nrow(bad_itt) > 0) {
  .add("ERROR", "itt-trt-coherence",
       "ITTFL='Y' must carry TRT01P/PCD/A/ACD; ITTFL='N' must have none")
}

# SAFFL is "has a treatment start", no more, no less
bad_saf <- adsl |> filter((SAFFL == "Y") != (!is.na(TRTSDT)))
if (nrow(bad_saf) > 0) {
  .add("ERROR", "saffl-trtsdt-coherence",
       "SAFFL='Y' iff TRTSDT is populated")
}

bad_si <- adsl |> filter(SAFFL == "Y", ITTFL != "Y")
if (nrow(bad_si) > 0) {
  .add("ERROR", "saffl-without-itt", "dosed subject without a planned arm")
}

# Treatment dates -------------------------------------------------------------
bad_ord <- adsl |> filter(!is.na(TRTSDT), !is.na(TRTEDT), TRTEDT < TRTSDT)
if (nrow(bad_ord) > 0) {
  .add("ERROR", "trtedt-before-trtsdt", sprintf("%d row(s)", nrow(bad_ord)))
}

bad_dur <- adsl |>
  filter(!is.na(TRTSDT), !is.na(TRTEDT),
         TRTDURD != as.integer(TRTEDT - TRTSDT) + 1L)
if (nrow(bad_dur) > 0) {
  .add("ERROR", "trtdurd-wrong", sprintf("%d row(s)", nrow(bad_dur)))
}

# End of study ----------------------------------------------------------------
bad_stt <- adsl |> filter(!EOSSTT %in% c("COMPLETED", "DISCONTINUED", NA))
if (nrow(bad_stt) > 0) {
  .add("ERROR", "eosstt-ct", sprintf("%d row(s) with bad EOSSTT", nrow(bad_stt)))
}

bad_stt2 <- adsl |> filter(is.na(EOSSTT) != is.na(EOSDT))
if (nrow(bad_stt2) > 0) {
  .add("ERROR", "eosstt-without-eosdt",
       "EOSSTT and EOSDT must be populated together")
}

bad_dc <- adsl |>
  filter(EOSSTT == "COMPLETED" & !is.na(DCSREAS)) |>
  bind_rows(adsl |> filter(EOSSTT == "DISCONTINUED" & is.na(DCSREAS)))
if (nrow(bad_dc) > 0) {
  .add("ERROR", "dcsreas-coherence",
       "DCSREAS populated iff EOSSTT='DISCONTINUED'")
}

# DCSREAS must be the DS disposition decode, not a free retyping of it --------
ds_disp <- ds |>
  filter(DSCAT == "DISPOSITION EVENT") |>
  select(USUBJID, DSDECOD)
bad_ds <- adsl |>
  filter(!is.na(EOSSTT)) |>
  left_join(ds_disp, by = "USUBJID") |>
  filter(is.na(DSDECOD) | (!is.na(DCSREAS) & DCSREAS != DSDECOD))
if (nrow(bad_ds) > 0) {
  .add("ERROR", "dcsreas-not-from-ds",
       "DCSREAS disagrees with the subject's DS DISPOSITION EVENT decode")
}

# Screen-failure coherence (DM is the source of the fact) ---------------------
sf_ids <- dm |> filter(ARMCD == "SCRNFAIL") |> pull(USUBJID)
bad_sf <- adsl |>
  filter(USUBJID %in% sf_ids) |>
  filter(ITTFL != "N" | SAFFL != "N" | !is.na(TRTSDT) | !is.na(EOSSTT))
if (nrow(bad_sf) > 0) {
  .add("ERROR", "screenfail-leak",
       sprintf(paste("%d screen-failure subject(s) with a population flag,",
                     "dosing or disposition"), nrow(bad_sf)))
}

bad_nsf <- adsl |>
  filter(!USUBJID %in% sf_ids, ITTFL != "Y")
if (nrow(bad_nsf) > 0) {
  .add("ERROR", "randomized-without-itt",
       sprintf("%d randomized subject(s) with ITTFL != 'Y'", nrow(bad_nsf)))
}

# Last known alive ------------------------------------------------------------
bad_lal <- adsl |>
  filter(SAFFL == "Y" | !is.na(EOSDT), is.na(LSTALVDT))
if (nrow(bad_lal) > 0) {
  .add("ERROR", "lstalvdt-missing",
       "dosed or disposed subject without LSTALVDT")
}

bad_lal2 <- adsl |> filter(!is.na(TRTSDT), !is.na(LSTALVDT), LSTALVDT < TRTSDT)
if (nrow(bad_lal2) > 0) {
  .add("ERROR", "lstalvdt-before-trtsdt", sprintf("%d row(s)", nrow(bad_lal2)))
}

bad_lal3 <- adsl |> filter(!is.na(EOSDT), !is.na(LSTALVDT), LSTALVDT < EOSDT)
if (nrow(bad_lal3) > 0) {
  .add("ERROR", "lstalvdt-before-eosdt", sprintf("%d row(s)", nrow(bad_lal3)))
}

# Age: the imputation rule means a birth year alone yields an AGE, so a
# missing AGE signals a missing birth date - which nothing downstream
# expects. Range check matches the protocol's adult population.
bad_age <- adsl |> filter(is.na(AGE) | AGE < 18 | AGE > 100)
if (nrow(bad_age) > 0) {
  .add("ERROR", "age-out-of-range",
       sprintf("%d row(s) with AGE missing or outside 18-100", nrow(bad_age)))
}

# Imputation flags: controlled values, and a flag implies a date --------------
for (fl in c("BRTHDTF", "TRTSDTF", "TRTEDTF", "EOSDTF", "DTHDTF")) {
  dtv <- str_remove(fl, "F$")
  bad <- adsl |>
    filter(!.data[[fl]] %in% c("", "D", "M", NA)) |>
    bind_rows(adsl |> filter(.data[[fl]] %in% c("D", "M"), is.na(.data[[dtv]])))
  if (nrow(bad) > 0) {
    .add("ERROR", "imputation-flag-bad",
         sprintf("%d row(s) with a bad %s / %s pair", nrow(bad), fl, dtv))
  }
}

# Date ordering: consent before first dose, end of study after it -------------
bad_cons <- adsl |>
  filter(!is.na(ENRLSTDT), !is.na(TRTSDT), TRTSDT < ENRLSTDT)
if (nrow(bad_cons) > 0) {
  .add("ERROR", "dose-before-consent", sprintf("%d row(s)", nrow(bad_cons)))
}

bad_eos <- adsl |>
  filter(!is.na(TRTSDT), !is.na(EOSDT), EOSDT < TRTSDT)
if (nrow(bad_eos) > 0) {
  .add("ERROR", "eosdt-before-trtsdt", sprintf("%d row(s)", nrow(bad_eos)))
}

# Death coherence: ADSL.DTHFL, the fatal AE, the DS DEATH record and the
# collected DM fields must all tell the same story, and death must anchor
# the analysis dates.
death_ids <- dm |> filter(DTHFL %in% "Y") |> pull(USUBJID)
fatal_ids <- ae |> filter(AEOUT == "FATAL") |> distinct(USUBJID) |> pull(USUBJID)
ds_death  <- ds |> filter(DSDECOD == "DEATH") |> distinct(USUBJID) |> pull(USUBJID)

bad_dth <- adsl |>
  filter(DTHFL == "Y") |>
  filter(is.na(DTHDT) | !USUBJID %in% fatal_ids | !USUBJID %in% ds_death |
           !USUBJID %in% death_ids)
if (nrow(bad_dth) > 0) {
  .add("ERROR", "dthfl-unbacked",
       "DTHFL='Y' without a fatal AE, DS DEATH record, DM flag or DTHDT")
}

bad_dth2 <- adsl |>
  filter(USUBJID %in% setdiff(fatal_ids, death_ids))
if (nrow(bad_dth2) > 0) {
  .add("ERROR", "fatal-ae-without-dthfl",
       "fatal-outcome AE whose subject has no DTHFL='Y'")
}

# Death is a discontinuation with reason DEATH, and it anchors last known
# alive; a death date before first dose is nonsense.
bad_dth3 <- adsl |>
  filter(DTHFL == "Y") |>
  filter(EOSSTT != "DISCONTINUED" | DCSREAS != "DEATH" |
           is.na(TRTSDT) | DTHDT < TRTSDT |
           is.na(LSTALVDT) | LSTALVDT != DTHDT)
if (nrow(bad_dth3) > 0) {
  .add("ERROR", "dth-derivation-inconsistent",
       paste("death subject whose EOSSTT/DCSREAS/LSTALVDT/DTHDT disagree",
             "with the death"))
}

bad_dthct <- adsl |> filter(!DTHFL %in% c("Y", ""))
if (nrow(bad_dthct) > 0) {
  .add("ERROR", "dthfl-ct", "DTHFL must be 'Y' or blank")
}

# ADAE: structure -------------------------------------------------------------
.req_adae <- c(
  "STUDYID", "USUBJID", "ASEQ", "AETERM", "AEDECOD", "AEBODSYS", "AESEV",
  "AESER", "AEREL", "AEACN", "AEOUT", "AESI", "AEDISCON",
  "ASTDT", "ASTDTF", "ASTDY", "AENDT", "AENDTF", "AENDY", "TRTEMFL"
)
miss <- setdiff(.req_adae, names(adae))
if (length(miss) > 0) .add("ERROR", "adae-required-vars", str_flatten_comma(miss))

# One analysis record per collected event: the (USUBJID, ASEQ) key set must
# equal the SDTM AE (USUBJID, AESEQ) key set, exactly.
adae_dup <- adae |> count(USUBJID, ASEQ, name = ".n") |> filter(.n > 1)
if (nrow(adae_dup) > 0) {
  .add("ERROR", "adae-key-not-unique",
       sprintf("%d duplicated USUBJID/ASEQ key(s)", nrow(adae_dup)))
}
ae_keys   <- ae   |> distinct(USUBJID, AESEQ)
adae_keys <- adae |> distinct(USUBJID, ASEQ)
lost <- anti_join(ae_keys, adae_keys, by = c("USUBJID", AESEQ = "ASEQ"))
if (nrow(lost) > 0) {
  .add("ERROR", "adae-lost-event",
       sprintf("%d SDTM AE record(s) with no ADAE row", nrow(lost)))
}
extra <- anti_join(adae_keys, ae_keys, by = c("USUBJID", ASEQ = "AESEQ"))
if (nrow(extra) > 0) {
  .add("ERROR", "adae-extra-event",
       sprintf("%d ADAE row(s) with no SDTM AE record", nrow(extra)))
}

# TRTEMFL: recompute the rule from ADSL instead of trusting the build. The
# window is onset within [TRTSDT, TRTEDT], no grace period - the same rule
# 31_adam_adae.R states, so a change on either side shows up here. The
# ADSL-side dates are renamed to keep them independent of the copies ADAE
# carries.
te_expected <- adae |>
  left_join(
    adsl |>
      transmute(USUBJID, .ref_trtsdt = TRTSDT, .ref_trtedt = TRTEDT),
    by = "USUBJID"
  ) |>
  mutate(.expect = case_when(
    is.na(ASTDT) | is.na(.ref_trtsdt)          ~ "",
    ASTDT >= .ref_trtsdt & ASTDT <= .ref_trtedt ~ "Y",
    .default                                    = ""
  ))
bad_te <- te_expected |> filter(TRTEMFL != .expect)
if (nrow(bad_te) > 0) {
  .add("ERROR", "trtemfl-not-derivable",
       sprintf(paste("%d row(s) where TRTEMFL disagrees with the",
                     "onset-within-[TRTSDT,TRTEDT] rule"), nrow(bad_te)))
}

# ADAE imputation flags: controlled values, and a flag implies a date -------
for (fl in c("ASTDTF", "AENDTF")) {
  dtv <- str_remove(fl, "F$")
  bad <- adae |>
    filter(!.data[[fl]] %in% c("", "D", "M", NA)) |>
    bind_rows(adae |> filter(.data[[fl]] %in% c("D", "M"), is.na(.data[[dtv]])))
  if (nrow(bad) > 0) {
    .add("ERROR", "adae-imputation-flag-bad",
         sprintf("%d row(s) with a bad %s / %s pair", nrow(bad), fl, dtv))
  }
}

# Analysis study days: anchored on TRTSDT with the no-day-0 rule, computed
# from the imputed dates
bad_dy <- adae |>
  filter(!is.na(ASTDT), !is.na(TRTSDT)) |>
  mutate(.diff = as.integer(ASTDT - TRTSDT),
         .expect = if_else(.diff >= 0, .diff + 1L, .diff)) |>
  filter(ASTDY != .expect)
if (nrow(bad_dy) > 0) {
  .add("ERROR", "astdy-wrong-anchor",
       sprintf("%d row(s) where ASTDY disagrees with ASTDT vs TRTSDT",
               nrow(bad_dy)))
}
if (any(adae$ASTDY == 0, na.rm = TRUE) || any(adae$AENDY == 0, na.rm = TRUE)) {
  .add("ERROR", "adae-study-day-zero", "ASTDY or AENDY equals zero")
}

# A full-precision start yields the same study day SDTM computed: ADAE
# anchors on TRTSDT, SDTM on RFSTDTC, and for dosed subjects those are the
# same date by construction.
bad_dy2 <- adae |>
  inner_join(select(ae, USUBJID, AESEQ, AESTDTC, AESTDY),
             by = c("USUBJID", ASEQ = "AESEQ")) |>
  filter(str_length(AESTDTC) == 10, !is.na(ASTDY), !is.na(AESTDY),
         ASTDY != AESTDY)
if (nrow(bad_dy2) > 0) {
  .add("ERROR", "astdy-vs-sdtm-aestdy",
       "ASTDY disagrees with SDTM AESTDY for a full-precision start")
}

bad_ord <- adae |> filter(!is.na(ASTDT), !is.na(AENDT), AENDT < ASTDT)
if (nrow(bad_ord) > 0) {
  .add("ERROR", "aendt-before-astdt", sprintf("%d row(s)", nrow(bad_ord)))
}

# SUPP merge-back: values must match SUPPAE, and every AE record must have
# its qualifiers - the generator asks both of every event, so a NA here
# means the pivot or the join drifted.
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
  .add("ERROR", "adae-supp-merge-bad",
       sprintf(paste("%d row(s) where AESI/AEDISCON disagree with SUPPAE",
                     "or are missing"), nrow(bad_supp)))
}

# ADVS: structure -------------------------------------------------------------
advs <- read_rds(file.path(paths$adam, "advs.rds"))
vs   <- read_rds(file.path(paths$sdtm, "vs.rds"))

.req_advs <- c(
  "STUDYID", "USUBJID", "PARAMCD", "PARAM", "PARAMN", "AVAL", "AVALU",
  "ABLFL", "BASE", "CHG", "PCHG", "ANRIND", "ANRLO", "ANRHI",
  "AVISIT", "AVISITN", "ADT", "ADY"
)
miss <- setdiff(.req_advs, names(advs))
if (length(miss) > 0) .add("ERROR", "advs-required-vars", str_flatten_comma(miss))

# One analysis record per subject / parameter / visit: if the generator ever
# produces both positions at one visit, this key breaks loudly here.
advs_dup <- advs |>
  count(USUBJID, PARAMCD, AVISITN, name = ".n") |>
  filter(.n > 1)
if (nrow(advs_dup) > 0) {
  .add("ERROR", "advs-key-not-unique",
       sprintf(paste("%d duplicated USUBJID/PARAMCD/AVISITN key(s) - a visit",
                     "with two positions for one test?"), nrow(advs_dup)))
}

# Coverage: exactly the SDTM VS records that carry a result. The NOT DONE
# rows are the deliberate exclusion.
vs_results <- vs |> filter(!is.na(VSSTRESN))
if (nrow(advs) != nrow(vs_results)) {
  .add("ERROR", "advs-coverage",
       sprintf("ADVS has %d row(s) but VS carries %d result(s)",
               nrow(advs), nrow(vs_results)))
}
advs_orphan <- advs |>
  anti_join(vs_results,
            by = c("USUBJID", PARAMCD = "VSTESTCD", AVISITN = "VISITNUM",
                   AVAL = "VSSTRESN"))
if (nrow(advs_orphan) > 0) {
  .add("ERROR", "advs-orphan-record",
       sprintf("%d ADVS row(s) with no matching VS result", nrow(advs_orphan)))
}

# Baseline: exactly one per randomized subject per parameter, none for
# screen failures, and it must be the value SDTM flagged.
itt_ids <- adsl |> filter(ITTFL == "Y") |> pull(USUBJID)
sf_ids  <- adsl |> filter(ITTFL == "N") |> pull(USUBJID)
bl_multi <- advs |>
  filter(ABLFL == "Y") |>
  count(USUBJID, PARAMCD, name = ".n") |>
  filter(.n > 1)
if (nrow(bl_multi) > 0) {
  .add("ERROR", "advs-ablfl-multi",
       sprintf("%d subject/parameter(s) with >1 ABLFL='Y'", nrow(bl_multi)))
}
bl_missing <- advs |>
  filter(USUBJID %in% itt_ids, !is.na(AVAL)) |>
  count(USUBJID, PARAMCD, ABLFL) |>
  filter(!any(ABLFL %in% "Y"), .by = c("USUBJID", "PARAMCD"))
if (nrow(bl_missing) > 0) {
  .add("ERROR", "advs-ablfl-missing",
       sprintf(paste("%d randomized subject/parameter(s) with no baseline",
                     "record"), nrow(bl_missing)))
}
bl_sf <- advs |> filter(USUBJID %in% sf_ids, ABLFL == "Y")
if (nrow(bl_sf) > 0) {
  .add("ERROR", "advs-ablfl-screenfail",
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
  .add("ERROR", "advs-ablfl-not-from-vs",
       "ABLFL='Y' AVAL disagrees with the SDTM VSBLFL record")
}

# BASE/CHG/PCHG arithmetic, recomputed from the analysis values --------------
base_chk <- advs |>
  filter(ABLFL == "Y") |>
  select(USUBJID, PARAMCD, .base = AVAL)
bad_base <- advs |>
  left_join(base_chk, by = c("USUBJID", "PARAMCD")) |>
  filter(xor(is.na(BASE), is.na(.base)) |
           (!is.na(BASE) & !is.na(.base) & BASE != .base))
if (nrow(bad_base) > 0) {
  .add("ERROR", "advs-base-wrong",
       "BASE disagrees with the ABLFL='Y' AVAL")
}

# BASE presence: populated for randomized subjects' rows, missing for
# screen failures
bad_base_sf <- advs |> filter(USUBJID %in% sf_ids, !is.na(BASE))
if (nrow(bad_base_sf) > 0) {
  .add("ERROR", "advs-base-screenfail",
       "screen-failure row with a BASE value")
}
bad_base_itt <- advs |>
  filter(USUBJID %in% itt_ids, !is.na(AVAL), is.na(BASE))
if (nrow(bad_base_itt) > 0) {
  .add("ERROR", "advs-base-missing",
       sprintf("%d randomized row(s) with no BASE", nrow(bad_base_itt)))
}

bad_chg <- advs |>
  filter(!is.na(BASE)) |>
  mutate(.chg  = if_else(ABLFL %in% "Y", NA_real_, AVAL - BASE),
         .pchg = if_else(is.na(.chg), NA_real_, 100 * .chg / BASE)) |>
  filter(xor(is.na(CHG), is.na(.chg)) | xor(is.na(PCHG), is.na(.pchg)) |
           (!is.na(CHG) & CHG != .chg) | (!is.na(PCHG) & PCHG != .pchg))
if (nrow(bad_chg) > 0) {
  .add("ERROR", "advs-chg-wrong",
       "CHG/PCHG disagree with AVAL - BASE (or populated on the baseline row)")
}

# ANRIND: recomputed from the row's own range, and the ranges themselves
# must equal the declared spec (re-declared here on purpose: a silent
# change of either copy is exactly the drift to catch)
advs_spec <- tribble(
  ~PARAMCD, ~ANRLO, ~ANRHI,
  "SYSBP",  90,      140,
  "DIABP",  50,      90,
  "PULSE",  50,      120,
  "TEMP",   35,      37.5,
  "WEIGHT", NA,      NA,
  "HEIGHT", NA,      NA
)
bad_range <- advs |>
  select(PARAMCD, ANRLO, ANRHI) |>
  distinct() |>
  full_join(advs_spec, by = "PARAMCD") |>
  filter(is.na(PARAMCD) |
           xor(is.na(ANRLO.x), is.na(ANRLO.y)) |
           (!is.na(ANRLO.x) & !is.na(ANRLO.y) & ANRLO.x != ANRLO.y) |
           (!is.na(ANRHI.x) & !is.na(ANRHI.y) & ANRHI.x != ANRHI.y))
if (nrow(bad_range) > 0) {
  .add("ERROR", "advs-range-spec-drift",
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
  .add("ERROR", "advs-anrind-wrong",
       "ANRIND disagrees with the AVAL vs ANRLO/ANRHI comparison")
}

# Analysis day: anchored on ADSL TRTSDT, so missing exactly when it is ------
bad_ady <- advs |>
  left_join(adsl |> select(USUBJID, .ref_trtsdt = TRTSDT), by = "USUBJID") |>
  mutate(.expect = derive_dy_d(ADT, .ref_trtsdt)) |>
  filter(ADY != .expect)
if (nrow(bad_ady) > 0) {
  .add("ERROR", "advs-ady-wrong",
       "ADY disagrees with ADT vs ADSL TRTSDT")
}
if (any(advs$ADY == 0, na.rm = TRUE)) {
  .add("ERROR", "advs-study-day-zero", "ADY equals zero")
}

# ADLB: structure -------------------------------------------------------------
adlb <- read_rds(file.path(paths$adam, "adlb.rds"))
lb   <- read_rds(file.path(paths$sdtm, "lb.rds"))

.req_adlb <- c(
  "STUDYID", "USUBJID", "PARAMCD", "PARAM", "PARAMN", "LBCAT",
  "AVAL", "AVALU", "ABLFL", "BASE", "BNRIND", "CHG", "PCHG",
  "ANRIND", "ANRLO", "ANRHI", "AVISIT", "AVISITN", "ADT", "ADY"
)
miss <- setdiff(.req_adlb, names(adlb))
if (length(miss) > 0) .add("ERROR", "adlb-required-vars", str_flatten_comma(miss))

adlb_dup <- adlb |>
  count(USUBJID, PARAMCD, AVISITN, name = ".n") |>
  filter(.n > 1)
if (nrow(adlb_dup) > 0) {
  .add("ERROR", "adlb-key-not-unique",
       sprintf("%d duplicated USUBJID/PARAMCD/AVISITN key(s)", nrow(adlb_dup)))
}

# Coverage: exactly the SDTM LB records that carry a result
lb_results <- lb |> filter(!is.na(LBSTRESN))
if (nrow(adlb) != nrow(lb_results)) {
  .add("ERROR", "adlb-coverage",
       sprintf("ADLB has %d row(s) but LB carries %d result(s)",
               nrow(adlb), nrow(lb_results)))
}
adlb_orphan <- adlb |>
  anti_join(lb_results,
            by = c("USUBJID", PARAMCD = "LBTESTCD", AVISITN = "VISITNUM",
                   AVAL = "LBSTRESN"))
if (nrow(adlb_orphan) > 0) {
  .add("ERROR", "adlb-orphan-record",
       sprintf("%d ADLB row(s) with no matching LB result", nrow(adlb_orphan)))
}

# Baseline: one per randomized subject per analyte, none for screen failures,
# and it must be the record SDTM flagged
bl_multi <- adlb |>
  filter(ABLFL == "Y") |>
  count(USUBJID, PARAMCD, name = ".n") |>
  filter(.n > 1)
if (nrow(bl_multi) > 0) {
  .add("ERROR", "adlb-ablfl-multi",
       sprintf("%d subject/analyte(s) with >1 ABLFL='Y'", nrow(bl_multi)))
}
bl_missing <- adlb |>
  filter(USUBJID %in% itt_ids, !is.na(AVAL)) |>
  count(USUBJID, PARAMCD, ABLFL) |>
  filter(!any(ABLFL %in% "Y"), .by = c("USUBJID", "PARAMCD"))
if (nrow(bl_missing) > 0) {
  .add("ERROR", "adlb-ablfl-missing",
       sprintf("%d randomized subject/analyte(s) with no baseline record",
               nrow(bl_missing)))
}
bl_sf <- adlb |> filter(USUBJID %in% sf_ids, ABLFL == "Y")
if (nrow(bl_sf) > 0) {
  .add("ERROR", "adlb-ablfl-screenfail",
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
  .add("ERROR", "adlb-ablfl-not-from-lb",
       "ABLFL='Y' AVAL disagrees with the SDTM LBBLFL record")
}

# BASE/BNRIND/CHG/PCHG recomputed from the analysis values -------------------
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
  .add("ERROR", "adlb-base-wrong",
       "BASE/BNRIND disagree with the ABLFL='Y' record")
}
bad_base_sf <- adlb |> filter(USUBJID %in% sf_ids, !is.na(BASE) | !is.na(BNRIND))
if (nrow(bad_base_sf) > 0) {
  .add("ERROR", "adlb-base-screenfail",
       "screen-failure row with a BASE or BNRIND value")
}
bad_base_itt <- adlb |>
  filter(USUBJID %in% itt_ids, !is.na(AVAL), is.na(BASE))
if (nrow(bad_base_itt) > 0) {
  .add("ERROR", "adlb-base-missing",
       sprintf("%d randomized row(s) with no BASE", nrow(bad_base_itt)))
}
bad_chg <- adlb |>
  filter(!is.na(BASE)) |>
  mutate(.chg  = if_else(ABLFL %in% "Y", NA_real_, AVAL - BASE),
         .pchg = if_else(is.na(.chg), NA_real_, 100 * .chg / BASE)) |>
  filter(xor(is.na(CHG), is.na(.chg)) | xor(is.na(PCHG), is.na(.pchg)) |
           (!is.na(CHG) & CHG != .chg) | (!is.na(PCHG) & PCHG != .pchg))
if (nrow(bad_chg) > 0) {
  .add("ERROR", "adlb-chg-wrong",
       "CHG/PCHG disagree with AVAL - BASE (or populated on the baseline row)")
}

# ANRIND: recomputed from the row's own limits, and the whole range triple
# must match the SDTM record - the ranges are collected data here, so a
# mismatch means the merge, not a rule, drifted
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
  .add("ERROR", "adlb-anrind-wrong",
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
  .add("ERROR", "adlb-range-not-from-lb",
       "ANRLO/ANRHI/ANRIND disagree with the SDTM record's own range")
}

bad_ady <- adlb |>
  left_join(adsl |> select(USUBJID, .ref_trtsdt = TRTSDT), by = "USUBJID") |>
  mutate(.expect = derive_dy_d(ADT, .ref_trtsdt)) |>
  filter(xor(is.na(ADY), is.na(.expect)) |
           (!is.na(ADY) & !is.na(.expect) & ADY != .expect))
if (nrow(bad_ady) > 0) {
  .add("ERROR", "adlb-ady-wrong",
       "ADY disagrees with ADT vs ADSL TRTSDT")
}
if (any(adlb$ADY == 0, na.rm = TRUE)) {
  .add("ERROR", "adlb-study-day-zero", "ADY equals zero")
}

# Report ------------------------------------------------------------------
report <- bind_rows(.issues)
if (nrow(report) == 0) {
  message("39_validate_adam.R: all ADaM checks passed (",
          nrow(adsl), " ADSL subjects, ", nrow(adae), " ADAE records, ",
          nrow(advs), " ADVS records, ", nrow(adlb), " ADLB records)")
} else {
  message("39_validate_adam.R: ", nrow(report), " issue(s)")
  print(report, n = Inf)
  n_err <- sum(report$severity == "ERROR")
  if (n_err > 0) {
    stop(sprintf("39_validate_adam.R: %d ADaM error(s)", n_err), call. = FALSE)
  }
}

rm(.req_adsl, .req_adae, .req_advs, .req_adlb, .issues, .add, sf_ids)

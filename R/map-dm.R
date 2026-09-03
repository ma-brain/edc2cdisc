# ============================================================================
# Title:   SDTM DM - Demographics
# Purpose: Derive DM from the Rave DM view, with reference dates from EX/DS.
#          DM runs first: its subject_ref (USUBJID + RFSTDTC/RFENDTC) feeds
#          every other domain's --DY derivations.
# ============================================================================

#' Reference dates for one subject, from the DM build
#'
#' @param dm The mapped DM dataset (see [map_dm()])
#' @return A tibble: USUBJID, RFSTDTC, RFENDTC.
#' @export
subject_ref <- function(dm) {
  dm |> select(USUBJID, RFSTDTC, RFENDTC)
}

#' Map the Demographics domain
#'
#' Reference dates come from the raw EX and DS views: RFXSTDTC/RFXENDTC from
#' the dosing records, RFPENDTC from the latest disposition record. Age is
#' computed at first dose (screen failures are aged at informed consent);
#' partial birth dates stay missing rather than being silently imputed -
#' that rule belongs to the analysis layer (see [derive_adsl()]).
#'
#' @param dm Raw DM clinical view
#' @param ex Raw EX clinical view (reference start/end dates)
#' @param ds Raw DS clinical view (end of participation)
#' @param spec A `study_spec`
#' @return The labelled SDTM DM tibble, one row per subject.
#' @export
map_dm <- function(dm, ex, ds, spec) {
  # Reference dates -----------------------------------------------------------
  ref_exposure <- ex |>
    filter(EXOCCUR == "1") |>
    mutate(
      usubjid = str_c(spec$study$STUDYID, Subject, sep = "-"),
      exstdtc = rave_dtc(EXSTDAT_YYYY, EXSTDAT_MM, EXSTDAT_DD),
      exendtc = rave_dtc(EXENDAT_YYYY, EXENDAT_MM, EXENDAT_DD)
    ) |>
    summarise(
      RFXSTDTC = min_dtc(exstdtc),
      RFXENDTC = max_dtc(exendtc),
      .by = usubjid
    )

  ref_disposition <- ds |>
    mutate(
      usubjid  = str_c(spec$study$STUDYID, Subject, sep = "-"),
      RFPENDTC = rave_dtc(DSSTDAT_YYYY, DSSTDAT_MM, DSSTDAT_DD)
    ) |>
    slice_max(RFPENDTC, by = usubjid, with_ties = FALSE) |>
    select(usubjid, RFPENDTC)

  # Drop the raw coded arm field ("1"/"2"/"3"); ARMCD/ARM are derived from
  # ARMCD_DECODE via spec$arms and the bare name would otherwise clash on
  # the join.
  dm_prep <- dm |>
    mutate(usubjid = str_c(spec$study$STUDYID, Subject, sep = "-")) |>
    select(-any_of(c("ARMCD", "ARMCD_RAW"))) |>
    left_join(ref_exposure, by = "usubjid") |>
    left_join(ref_disposition, by = "usubjid") |>
    left_join(spec$arms, by = "ARMCD_DECODE")

  out <- map_variables(dm_prep, spec,
                       filter(spec$variables, domain == "DM"))

  out |>
    arrange(USUBJID) |>
    apply_labels(c(
      STUDYID  = "Study Identifier",
      DOMAIN   = "Domain Abbreviation",
      USUBJID  = "Unique Subject Identifier",
      SUBJID   = "Subject Identifier for the Study",
      RFSTDTC  = "Subject Reference Start Date/Time",
      RFENDTC  = "Subject Reference End Date/Time",
      RFXSTDTC = "Date/Time of First Study Treatment",
      RFXENDTC = "Date/Time of Last Study Treatment",
      RFICDTC  = "Date/Time of Informed Consent",
      RFPENDTC = "Date/Time of End of Participation",
      DTHFL    = "Subject Death Flag",
      DTHDTC   = "Date/Time of Death",
      SITEID   = "Study Site Identifier",
      BRTHDTC  = "Date/Time of Birth",
      AGE      = "Age",
      AGEU     = "Age Units",
      SEX      = "Sex",
      RACE     = "Race",
      ETHNIC   = "Ethnicity",
      ARMCD    = "Planned Arm Code",
      ARM      = "Description of Planned Arm",
      ACTARMCD = "Actual Arm Code",
      ACTARM   = "Description of Actual Arm",
      COUNTRY  = "Country",
      DMDTC    = "Date/Time of Collection",
      DMDY     = "Study Day of Collection"
    ))
}

# ADEG: the ECG analysis dataset (BDS) ---------------------------------------
# derive_adeg() is the ADVS pattern on the EG domain: the same BDS tail
# (ABLFL carried from SDTM's EGBLFL, BASE from the baseline record, CHG/PCHG
# via the shared rules, ANRIND against the spec-declared interval ranges)
# with the NOT-DONE drop. Pins below were probed from the actual extract
# before being asserted; expected study days are recomputed from the pinned
# dates via the stated day rule, never from the implementation.

adeg_fixture <- function() {
  out <- file.path(tempdir(), "adeg-fix")
  dir.create(out, showWarnings = FALSE)
  ext <- file.path(out, "rave")
  if (!dir.exists(ext)) suppressMessages(generate_rave_extract(out = ext))
  built <- suppressMessages(build_all(ext))
  list(eg = built$sdtm$EG, adsl = built$adam$ADSL)
}

test_that("derive_adeg keeps one analysis record per performed EG record", {
  f <- adeg_fixture()
  adeg <- derive_adeg(f$eg, f$adsl)
  performed <- f$eg |> filter(!EGSTAT %in% "NOT DONE")

  expect_equal(nrow(adeg), nrow(performed))
  expect_setequal(paste(adeg$USUBJID, adeg$PARAMCD, adeg$AVISITN),
                  paste(performed$USUBJID, performed$EGTESTCD,
                        performed$VISITNUM))
  # the NOT-DONE WEEK 8 visit of 3021-101-010 documents a missed ECG - there
  # is no result to analyse, so BDS drops it
  expect_false(any(adeg$USUBJID == "3021-101-010" & adeg$AVISITN == 5))
  # one record per subject per parameter per visit, over all subjects
  expect_equal(nrow(distinct(adeg, USUBJID, PARAMCD, AVISITN)), nrow(adeg))
  # sorted like ADVS: USUBJID, then the analysis parameter, then the visit
  expect_equal(as.data.frame(adeg),
               as.data.frame(arrange(adeg, USUBJID, PARAMN, AVISITN)))
})

test_that("derive_adeg returns the BDS interface columns in order", {
  f <- adeg_fixture()
  adeg <- derive_adeg(f$eg, f$adsl)

  expect_equal(names(adeg), c(
    "STUDYID", "USUBJID", "PARAMCD", "PARAM", "PARAMN",
    "AVAL", "AVALU", "ABLFL", "BASE", "CHG", "PCHG",
    "ANRIND", "ANRLO", "ANRHI", "AVISIT", "AVISITN", "ADT", "ADY"
  ))
})

test_that("parameters follow the spec: codes, PARAM strings and unit", {
  f <- adeg_fixture()
  adeg <- derive_adeg(f$eg, f$adsl)
  spec_rows <- filter(spec_synth01$bds, domain == "ADEG")

  expect_setequal(unique(adeg$PARAMCD), spec_rows$paramcd)
  expect_setequal(unique(adeg$PARAM),
                  c("PR Interval (msec)", "QRS Duration (msec)",
                    "QT Interval (msec)", "QTcF Interval (msec)",
                    "RR Interval (msec)"))
  # PARAMN comes from the spec (QTcF fourth, per the ADEG rows)
  pn <- distinct(adeg, PARAMCD, PARAMN)
  expect_setequal(pn$PARAMN[pn$PARAMCD == "QTCF"], 4)
  # every parseable EG result is msec
  expect_true(all(!is.na(adeg$AVALU) & adeg$AVALU == "msec"))
})

test_that("the seeded WEEK 4 QT/QTCF classify HIGH against declared ranges", {
  f <- adeg_fixture()
  adeg <- derive_adeg(f$eg, f$adsl)
  w4 <- adeg |> filter(USUBJID == "3021-102-014", AVISIT == "WEEK 4")
  expect_equal(nrow(w4), 5)

  qt   <- w4[w4$PARAMCD == "QT", ]
  qtcf <- w4[w4$PARAMCD == "QTCF", ]
  expect_equal(as.vector(qt$AVAL), 520)
  expect_equal(as.vector(qtcf$AVAL), 537)
  expect_equal(as.vector(qt$ANRIND), "HIGH")
  expect_equal(as.vector(qtcf$ANRIND), "HIGH")
  # the SAP stand-in QT range from spec$bds
  expect_equal(as.vector(qt$ANRLO), 350)
  expect_equal(as.vector(qt$ANRHI), 450)
  # the same visit's other intervals stay inside their ranges
  expect_true(all(w4$ANRIND[!w4$PARAMCD %in% c("QT", "QTCF")] == "NORMAL"))
})

test_that("ABLFL is SDTM's EGBLFL carried forward", {
  f <- adeg_fixture()
  adeg <- derive_adeg(f$eg, f$adsl)
  cmp <- f$eg |>
    filter(!EGSTAT %in% "NOT DONE") |>
    select(USUBJID, PARAMCD = EGTESTCD, AVISITN = VISITNUM, EGBLFL) |>
    left_join(adeg, by = c("USUBJID", "PARAMCD", "AVISITN"))
  expect_equal(as.vector(cmp$ABLFL), as.vector(cmp$EGBLFL))
  # probed: one baseline per subject per interval (22 subjects x 5 params)
  expect_equal(sum(cmp$ABLFL %in% "Y", na.rm = TRUE), 110)
})

test_that("BASE/CHG/PCHG follow the shared BDS rules", {
  f <- adeg_fixture()
  adeg <- derive_adeg(f$eg, f$adsl)

  # the baseline row itself carries no change (probed: BASELINE QT 398)
  bl <- adeg |> filter(USUBJID == "3021-102-014", AVISIT == "BASELINE",
                       PARAMCD == "QT")
  expect_equal(nrow(bl), 1)
  expect_equal(as.vector(bl$ABLFL), "Y")
  expect_equal(as.vector(bl$BASE), 398)
  expect_true(is.na(bl$CHG))
  expect_true(is.na(bl$PCHG))

  # the seeded WEEK 4 QT changes from its baseline (probed: 520 vs 398)
  qt <- adeg |> filter(USUBJID == "3021-102-014", AVISIT == "WEEK 4",
                       PARAMCD == "QT")
  expect_equal(nrow(qt), 1)
  expect_equal(as.vector(qt$BASE), 398)
  expect_equal(as.vector(qt$CHG), 122)
  expect_equal(as.vector(qt$PCHG), 100 * 122 / 398)

  # screen failures have no baseline row - a fact, not a gap: BASE/CHG stay
  # missing while ANRIND still computes against the declared ranges
  sf <- adeg |> filter(USUBJID == "3021-103-006")
  expect_equal(nrow(sf), 5)
  expect_true(all(is.na(sf$BASE) & is.na(sf$CHG) & is.na(sf$PCHG)))
  expect_equal(unique(sf$ANRIND), "NORMAL")
})

test_that("ADT handles reduced-precision EGDTC; ADY recomputes from dates", {
  f <- adeg_fixture()
  adeg <- derive_adeg(f$eg, f$adsl)

  # 3021-103-003 WEEK 2 collected a date-only EGDTC - dtc_date() keeps the
  # date, and the study day anchors on TRTSDT 2024-02-03
  rp <- adeg |> filter(USUBJID == "3021-103-003", AVISIT == "WEEK 2")
  expect_equal(nrow(rp), 5)
  expect_equal(as.vector(unique(rp$ADT)), as.Date("2024-02-18"))
  expect_true(all(rp$ADY == derive_dy_d(as.Date("2024-02-18"),
                                        as.Date("2024-02-03"))))

  # the seeded HIGH row: full-precision EGDTC, TRTSDT 2024-03-15
  qt <- adeg |> filter(USUBJID == "3021-102-014", AVISIT == "WEEK 4",
                       PARAMCD == "QT")
  expect_equal(as.vector(qt$ADT), as.Date("2024-04-11"))
  expect_equal(as.vector(qt$ADY),
               derive_dy_d(as.Date("2024-04-11"), as.Date("2024-03-15")))

  # screen failures have no anchor: the collected date survives, ADY does not
  sf <- adeg |> filter(USUBJID == "3021-103-006")
  expect_true(all(!is.na(sf$ADT) & is.na(sf$ADY)))
})

test_that("derive_adeg labels the analysis columns", {
  f <- adeg_fixture()
  adeg <- derive_adeg(f$eg, f$adsl)

  # the ADVS label set minus VSPOS (EG has no positional variable)
  expect_equal(
    unlist(var_label(adeg)),
    c(STUDYID  = "Study Identifier",
      USUBJID  = "Unique Subject Identifier",
      PARAMCD  = "Parameter Code",
      PARAM    = "Parameter",
      PARAMN   = "Parameter (N)",
      AVAL     = "Analysis Value",
      AVALU    = "Analysis Value Unit",
      ABLFL    = "Baseline Record Flag",
      BASE     = "Baseline Value",
      CHG      = "Change from Baseline",
      PCHG     = "Percent Change from Baseline",
      ANRIND   = "Analysis Reference Range Indicator",
      ANRLO    = "Analysis Normal Range Lower Limit",
      ANRHI    = "Analysis Normal Range Upper Limit",
      AVISIT   = "Analysis Visit",
      AVISITN  = "Analysis Visit (N)",
      ADT      = "Analysis Date",
      ADY      = "Analysis Study Day")
  )
  # the <=40 sweep guards future columns; the pins above guard these
  long <- keep(var_label(adeg), \(l) !is.null(l) && nchar(l) > 40)
  expect_length(long, 0)
})

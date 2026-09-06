# ADCM: the OCCDS concomitant-medication analysis dataset --------------------
# derive_adcm() is the ADAE pattern without the SUPP pivot (there is no
# SUPPCM to merge back): one analysis record per collected CM record with
# ASEQ = CMSEQ, imputed analysis dates on the shared first-of rule, study
# days anchored on ADSL TRTSDT, and the OCCDS anchors (SAFFL/TRTSDT/TRTEDT)
# carried so any windowing is checkable in place. Pins below were probed
# from the actual extract before being asserted; expected study days are
# computed from the pinned dates via the stated day rule, never from the
# implementation.

adcm_fixture <- function() {
  built <- built_suite()
  list(cm = built$sdtm$CM, adsl = built$adam$ADSL)
}

test_that("derive_adcm keeps one analysis record per collected CM record", {
  f <- adcm_fixture()
  adcm <- derive_adcm(f$cm, f$adsl)

  expect_equal(nrow(adcm), nrow(f$cm))
  expect_setequal(paste(adcm$USUBJID, adcm$ASEQ),
                  paste(f$cm$USUBJID, f$cm$CMSEQ))
  # sorted like ADAE: USUBJID, then the analysis sequence
  expect_equal(as.data.frame(adcm), as.data.frame(arrange(adcm, USUBJID, ASEQ)))
})

test_that("derive_adcm returns the OCCDS interface columns in order", {
  f <- adcm_fixture()
  adcm <- derive_adcm(f$cm, f$adsl)

  expect_equal(names(adcm), c(
    "STUDYID", "USUBJID", "ASEQ",
    "CMTRT", "CMDECOD", "CMINDC", "CMDOSE", "CMDOSU", "CMDOSFRQ", "CMROUTE",
    "SAFFL", "TRTSDT", "TRTEDT",
    "ASTDT", "ASTDTF", "AENDT", "AENDTF", "ASTDY", "AENDY"
  ))
})

test_that("a full-precision CM record carries its collected dates, unflagged", {
  f <- adcm_fixture()
  adcm <- derive_adcm(f$cm, f$adsl)
  # 3021-101-007 CMSEQ 1: METFORMIN 850 mg BID ORAL, 2021-08-29..2022-09-10,
  # first dose 2024-02-15 - the medication predates the study window
  r <- adcm[adcm$USUBJID == "3021-101-007" & adcm$ASEQ == 1, ]

  expect_equal(as.vector(r$CMTRT), "METFORMIN")
  expect_equal(as.vector(r$CMDECOD), "METFORMIN")
  expect_equal(as.vector(r$CMINDC), "Type 2 diabetes mellitus")
  expect_equal(as.vector(r$CMDOSE), 850)
  expect_equal(as.vector(r$CMDOSU), "mg")
  expect_equal(as.vector(r$CMDOSFRQ), "BID")
  expect_equal(as.vector(r$CMROUTE), "ORAL")

  expect_equal(as.vector(r$ASTDT), as.Date("2021-08-29"))
  expect_equal(as.vector(r$ASTDTF), "")
  expect_equal(as.vector(r$AENDT), as.Date("2022-09-10"))
  expect_equal(as.vector(r$AENDTF), "")

  # negative study days pass through the no-day-0 rule unchanged
  expect_equal(as.vector(r$ASTDY),
               as.integer(as.Date("2021-08-29") - as.Date("2024-02-15")))
  expect_equal(as.vector(r$AENDY),
               as.integer(as.Date("2022-09-10") - as.Date("2024-02-15")))

  # the OCCDS anchors come from ADSL, not retyped
  expect_equal(as.vector(r$SAFFL), "Y")
  expect_equal(as.vector(r$TRTSDT), as.Date("2024-02-15"))
  expect_equal(as.vector(r$TRTEDT), as.Date("2024-05-09"))
})

test_that("reduced-precision starts impute to the first of the period, flagged", {
  f <- adcm_fixture()
  adcm <- derive_adcm(f$cm, f$adsl)

  # 3021-101-010 CMSEQ 1, CMSTDTC "2024-02": day imputed, month kept
  d <- adcm[adcm$USUBJID == "3021-101-010" & adcm$ASEQ == 1, ]
  expect_equal(as.vector(d$ASTDT), as.Date("2024-02-01"))
  expect_equal(as.vector(d$ASTDTF), "D")

  # 3021-102-005 CMSEQ 1, CMSTDTC "2021": month and day imputed
  y <- adcm[adcm$USUBJID == "3021-102-005" & adcm$ASEQ == 1, ]
  expect_equal(as.vector(y$ASTDT), as.Date("2021-01-01"))
  expect_equal(as.vector(y$ASTDTF), "M")
  # the end date was collected in full and stays unflagged
  expect_equal(as.vector(y$AENDT), as.Date("2021-12-24"))
  expect_equal(as.vector(y$AENDTF), "")
})

test_that("an ongoing medication keeps a missing analysis end", {
  f <- adcm_fixture()
  adcm <- derive_adcm(f$cm, f$adsl)
  # 3021-101-022 CMSEQ 2: CMENRTPT ONGOING, no CMENDTC collected
  r <- adcm[adcm$USUBJID == "3021-101-022" & adcm$ASEQ == 2, ]

  expect_true(is.na(r$AENDT))
  expect_true(is.na(r$AENDTF))
  expect_true(is.na(r$AENDY))
})

test_that("a medication of an undosed subject has no anchored study day", {
  f <- adcm_fixture()
  adcm <- derive_adcm(f$cm, f$adsl)
  # 3021-103-006 is a screen failure (SAFFL N, no TRTSDT) with full
  # precision on CMSEQ 2 - the start date exists, the anchor does not
  r <- adcm[adcm$USUBJID == "3021-103-006" & adcm$ASEQ == 2, ]

  expect_equal(as.vector(r$SAFFL), "N")
  expect_true(is.na(r$TRTSDT))
  expect_equal(as.vector(r$ASTDT), as.Date("2022-10-02"))
  expect_true(is.na(r$ASTDY))
})

test_that("derive_adcm labels the analysis columns", {
  f <- adcm_fixture()
  adcm <- derive_adcm(f$cm, f$adsl)

  lbl <- var_label(adcm)
  expect_equal(lbl$ASEQ, "Analysis Sequence Number")
  expect_equal(lbl$CMTRT, "Reported Name of Drug, Med, or Therapy")
  expect_equal(lbl$ASTDT, "Analysis Start Date")
  expect_equal(lbl$ASTDTF, "Analysis Start Date Imputation Flag")
  expect_equal(lbl$ASTDY, "Analysis Study Day of Start")
  expect_equal(lbl$AENDT, "Analysis End Date")
  expect_equal(lbl$AENDY, "Analysis Study Day of End")
  # the <=40 sweep guards future columns; the pins above guard these
  long <- keep(lbl, \(l) !is.null(l) && nchar(l) > 40)
  expect_length(long, 0)
})

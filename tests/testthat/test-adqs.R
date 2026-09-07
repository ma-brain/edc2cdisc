# ADQS: the questionnaire analysis dataset (BDS) ------------------------------
# derive_adqs() is the ADEG pattern on the QS domain plus the package's first
# derived analysis parameter: the items clone the BDS tail (ABLFL carried from
# QSBLFL, BASE from the baseline record, CHG/PCHG via the shared rules), the
# MOSTOT total is summed per visit from the spec$totals src_items and anchored
# at the item-baseline visit. Pins below were probed from the actual build
# before being asserted; expected study days are recomputed from the pinned
# dates via the stated day rule, never from the implementation.

adqs_fixture <- function(study = c("SYNTH01", "SYNTH02")) {
  study <- match.arg(study)
  built <- built_suite(study)
  list(qs = built$sdtm$QS, adsl = built$adam$ADSL)
}

test_that("derive_adqs keeps one analysis record per performed QS item", {
  f <- adqs_fixture()
  adqs <- derive_adqs(f$qs, f$adsl)
  performed <- f$qs |> filter(!QSSTAT %in% "NOT DONE")
  items <- adqs |> filter(PARAMCD != "MOSTOT")

  # probed: 520 performed item rows + 130 complete-visit totals = 650
  expect_equal(nrow(items), nrow(performed))
  expect_equal(nrow(items), 520)
  expect_setequal(paste(items$USUBJID, items$PARAMCD, items$AVISITN),
                  paste(performed$USUBJID, performed$QSTESTCD,
                        performed$VISITNUM))
  # the NOT-DONE WEEK 4 visit of 3021-102-008 documents a missed form - there
  # is nothing to analyse, so no items and no total
  expect_false(any(adqs$USUBJID == "3021-102-008" & adqs$AVISITN == 4))
  # one record per subject per parameter per visit, over all subjects
  expect_equal(nrow(distinct(adqs, USUBJID, PARAMCD, AVISITN)), nrow(adqs))
  # sorted like ADEG: USUBJID, then the analysis parameter, then the visit
  expect_equal(as.data.frame(adqs),
               as.data.frame(arrange(adqs, USUBJID, PARAMN, AVISITN)))
})

test_that("derive_adqs returns the BDS interface columns in order", {
  f <- adqs_fixture()
  adqs <- derive_adqs(f$qs, f$adsl)

  expect_equal(names(adqs), c(
    "STUDYID", "USUBJID", "PARAMCD", "PARAM", "PARAMN",
    "AVAL", "AVALU", "ABLFL", "BASE", "CHG", "PCHG",
    "ANRIND", "ANRLO", "ANRHI", "AVISIT", "AVISITN", "ADT", "ADY"
  ))
})

test_that("parameters follow the spec: items from QSTEST, total from the table", {
  f <- adqs_fixture()
  adqs <- derive_adqs(f$qs, f$adsl)

  expect_setequal(unique(adqs$PARAMCD),
                  c("MOS01", "MOS02", "MOS03", "MOS04", "MOSTOT"))
  expect_setequal(unique(adqs$PARAM),
                  c("Mood: Cheerful", "Mood: Down", "Sleep Quality",
                    "Energy Level", "MOOD SCALE Total"))
  # PARAMN comes from the spec: items 1-4, the total fifth
  pn <- distinct(adqs, PARAMCD, PARAMN)
  expect_equal(pn$PARAMN[pn$PARAMCD == "MOS01"], 1)
  expect_equal(pn$PARAMN[pn$PARAMCD == "MOS02"], 2)
  expect_equal(pn$PARAMN[pn$PARAMCD == "MOS03"], 3)
  expect_equal(pn$PARAMN[pn$PARAMCD == "MOS04"], 4)
  expect_equal(pn$PARAMN[pn$PARAMCD == "MOSTOT"], 5)
})

test_that("items carry AVAL = QSSTRESN, ABLFL = QSBLFL and no reference range", {
  f <- adqs_fixture()
  adqs <- derive_adqs(f$qs, f$adsl)
  items <- adqs |> filter(PARAMCD != "MOSTOT")

  cmp <- f$qs |>
    filter(!QSSTAT %in% "NOT DONE") |>
    select(USUBJID, PARAMCD = QSTESTCD, AVISITN = VISITNUM,
           QSSTRESN, QSBLFL) |>
    left_join(items, by = c("USUBJID", "PARAMCD", "AVISITN"))
  expect_equal(as.vector(cmp$AVAL), as.vector(cmp$QSSTRESN))
  expect_equal(as.vector(cmp$ABLFL), as.vector(cmp$QSBLFL))
  # probed: one baseline per subject per item (22 treated subjects x 4 items)
  expect_equal(sum(items$ABLFL %in% "Y", na.rm = TRUE), 88)
  # ordinal 0-3 items have no absolute range: ANRIND stays missing by design
  # (the WEIGHT/HEIGHT precedent), and the declared bounds ride through as NA
  expect_true(all(is.na(items$ANRIND)))
  expect_true(all(is.na(items$ANRLO)))
  expect_true(all(is.na(items$ANRHI)))
})

test_that("item BASE/CHG/PCHG follow the shared BDS rules", {
  f <- adqs_fixture()
  adqs <- derive_adqs(f$qs, f$adsl)

  # the baseline row itself carries no change (probed: BASELINE MOS01 = 3)
  bl <- adqs |> filter(USUBJID == "3021-102-014", AVISIT == "BASELINE",
                       PARAMCD == "MOS01")
  expect_equal(nrow(bl), 1)
  expect_equal(as.vector(bl$ABLFL), "Y")
  expect_equal(as.vector(bl$BASE), 3)
  expect_true(is.na(bl$CHG))
  expect_true(is.na(bl$PCHG))

  # the WEEK 2 item changes from its baseline (probed: MOS01 0 vs 3, MOS03 2 vs 1)
  m01 <- adqs |> filter(USUBJID == "3021-102-014", AVISIT == "WEEK 2",
                        PARAMCD == "MOS01")
  expect_equal(as.vector(m01$BASE), 3)
  expect_equal(as.vector(m01$CHG), -3)
  expect_equal(as.vector(m01$PCHG), -100)
  m03 <- adqs |> filter(USUBJID == "3021-102-014", AVISIT == "WEEK 2",
                        PARAMCD == "MOS03")
  expect_equal(as.vector(m03$BASE), 1)
  expect_equal(as.vector(m03$CHG), 1)
  expect_equal(as.vector(m03$PCHG), 100)

  # every baseline item row carries no change, by the shared rule
  expect_true(all(is.na(adqs$CHG[adqs$ABLFL %in% "Y"])))

  # screen failures have no baseline row - a fact, not a gap: BASE/CHG stay
  # missing while the records themselves survive (ABLFL carried from QSBLFL,
  # which is NA for the undosed)
  sf <- adqs |> filter(USUBJID == "3021-103-006")
  expect_equal(nrow(sf), 5) # 4 items + 1 total at SCREENING
  expect_true(all(is.na(sf$BASE) & is.na(sf$CHG) & is.na(sf$PCHG)))
})

test_that("MOSTOT sums the visit's items and exists only where all items do", {
  f <- adqs_fixture()
  adqs <- derive_adqs(f$qs, f$adsl)
  totals <- adqs |> filter(PARAMCD == "MOSTOT")

  # probed: every performed visit carries all four items, so 130 totals -
  # one per (subject, performed visit) except the NOT-DONE WEEK 4
  expect_equal(nrow(totals), 130)
  complete <- f$qs |>
    filter(!QSSTAT %in% "NOT DONE") |>
    summarise(
      n_items = n(),
      total = sum(QSSTRESN),
      .by = c(USUBJID, VISITNUM)
    ) |>
    filter(n_items == 4)
  expect_setequal(paste(totals$USUBJID, totals$AVISITN),
                  paste(complete$USUBJID, complete$VISITNUM))
  # the total is the sum of that visit's items, recomputed here from QS
  cmp <- totals |>
    select(USUBJID, AVISITN, AVAL) |>
    left_join(complete, by = c("USUBJID", "AVISITN" = "VISITNUM"))
  expect_equal(as.vector(cmp$AVAL), as.vector(cmp$total))
  # probed pins: the (idx + visit_pos + item) %% 4 arithmetic over four
  # consecutive items is a complete residue system - every total is exactly 6
  expect_equal(unique(totals$AVAL), 6)
  expect_equal(as.vector(totals$AVAL[totals$USUBJID == "3021-102-014" &
                                       totals$AVISITN == 2]), 6)
  expect_equal(as.vector(totals$AVAL[totals$USUBJID == "3021-102-014" &
                                       totals$AVISITN == 3]), 6)
})

test_that("the total's baseline is derived at the item-baseline visit", {
  f <- adqs_fixture()
  adqs <- derive_adqs(f$qs, f$adsl)
  totals <- adqs |> filter(PARAMCD == "MOSTOT")

  # QS has no QSBLFL for a parameter that does not exist in SDTM: the total
  # takes ABLFL = "Y" at the visit where the items carry QSBLFL = "Y" - and
  # nowhere else (probed: the BASELINE visit, VISITNUM 2, one per subject)
  item_bl <- adqs |>
    filter(PARAMCD != "MOSTOT", ABLFL %in% "Y") |>
    distinct(USUBJID, AVISITN)
  tot_bl <- totals |> filter(ABLFL %in% "Y") |> distinct(USUBJID, AVISITN)
  expect_equal(as.data.frame(tot_bl), as.data.frame(item_bl))
  expect_equal(nrow(tot_bl), 22)
  expect_true(all(tot_bl$AVISITN == 2))
  # exactly one flagged row per treated subject
  expect_equal(sum(totals$ABLFL %in% "Y", na.rm = TRUE), 22)

  # the baseline total row itself carries no change; later visits change
  # against the derived baseline (probed: every total is 6, so CHG is 0)
  bl <- totals |> filter(USUBJID == "3021-102-014", AVISITN == 2)
  expect_equal(as.vector(bl$ABLFL), "Y")
  expect_equal(as.vector(bl$BASE), 6)
  expect_true(is.na(bl$CHG))
  expect_true(is.na(bl$PCHG))
  wk2 <- totals |> filter(USUBJID == "3021-102-014", AVISITN == 3)
  expect_equal(as.vector(wk2$BASE), 6)
  expect_equal(as.vector(wk2$CHG), 0)
  expect_equal(as.vector(wk2$PCHG), 0)

  # screen failures: no item baseline exists, so the total is never flagged
  # and its BASE/CHG/PCHG stay missing
  sf <- totals |> filter(USUBJID %in% c("3021-103-006", "3021-103-018"))
  expect_equal(nrow(sf), 2)
  expect_true(all(is.na(sf$ABLFL)))
  expect_true(all(is.na(sf$BASE) & is.na(sf$CHG) & is.na(sf$PCHG)))
})

test_that("the total classifies against its declared range; items do not", {
  f <- adqs_fixture()
  adqs <- derive_adqs(f$qs, f$adsl)
  totals <- adqs |> filter(PARAMCD == "MOSTOT")

  # the declared SYNTH01 range from spec$totals
  expect_true(all(totals$ANRLO == 4))
  expect_true(all(totals$ANRHI == 12))
  # probed: the deterministic arithmetic makes every total exactly 6, so the
  # built data classifies NORMAL only - the LOW branch is pinned against
  # hand-built input below, input the seeded generator never produces
  expect_equal(unique(totals$ANRIND), "NORMAL")
  # the items never classify: their spec rows declare no range
  expect_equal(unique(adqs$ANRIND[adqs$PARAMCD != "MOSTOT"]), NA_character_)
})

test_that("ADT is the collected QS date; ADY recomputes from pinned dates", {
  f <- adqs_fixture()
  adqs <- derive_adqs(f$qs, f$adsl)

  # 3021-102-014: TRTSDT 2024-03-15 (probed); SCREENING collected 2024-03-02,
  # WEEK 4 collected 2024-04-11 - the same date the total row carries
  w4 <- adqs |> filter(USUBJID == "3021-102-014", AVISITN == 4)
  expect_true(all(w4$ADT == as.Date("2024-04-11")))
  expect_true(all(w4$ADY == derive_dy_d(as.Date("2024-04-11"),
                                        as.Date("2024-03-15"))))
  scr <- adqs |> filter(USUBJID == "3021-102-014", AVISITN == 1)
  expect_true(all(scr$ADT == as.Date("2024-03-02")))
  expect_true(all(scr$ADY == derive_dy_d(as.Date("2024-03-02"),
                                         as.Date("2024-03-15"))))

  # screen failures have no anchor: the collected date survives, ADY does not
  sf <- adqs |> filter(USUBJID == "3021-103-006")
  expect_true(all(!is.na(sf$ADT) & is.na(sf$ADY)))
  expect_true(all(sf$ADT == as.Date("2024-02-03")))
})

test_that("derive_adqs labels the analysis columns", {
  f <- adqs_fixture()
  adqs <- derive_adqs(f$qs, f$adsl)

  # the ADVS/ADEG label set
  expect_equal(
    unlist(var_label(adqs)),
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
  long <- keep(var_label(adqs), \(l) !is.null(l) && nchar(l) > 40)
  expect_length(long, 0)
})

test_that("SYNTH02 scores five items into a sixth-parameter total", {
  f <- adqs_fixture("SYNTH02")
  adqs <- derive_adqs(f$qs, f$adsl, spec_synth02)

  expect_setequal(unique(adqs$PARAMCD),
                  c("MOS01", "MOS02", "MOS03", "MOS04", "MOS05", "MOSTOT"))
  expect_setequal(unique(adqs$PARAM),
                  c("Mood: Cheerful", "Mood: Down", "Sleep Quality",
                    "Energy Level", "Concentration", "MOOD SCALE Total"))
  pn <- distinct(adqs, PARAMCD, PARAMN)
  expect_equal(pn$PARAMN[pn$PARAMCD == "MOS05"], 5)
  expect_equal(pn$PARAMN[pn$PARAMCD == "MOSTOT"], 6)
  # probed: 94 performed visits, all complete, so 94 totals; the fifth item
  # breaks the residue system - totals span 6-9, all inside 4-15
  totals <- adqs |> filter(PARAMCD == "MOSTOT")
  expect_equal(nrow(totals), 94)
  expect_setequal(unique(totals$AVAL), c(6, 7, 8, 9))
  expect_true(all(totals$ANRLO == 4 & totals$ANRHI == 15))
  expect_equal(unique(totals$ANRIND), "NORMAL")
  # the NOT-DONE WEEK 2 visit of 4033-202-005 leaves no records at all
  expect_false(any(adqs$USUBJID == "4033-202-005" & adqs$AVISITN == 3))
})

test_that("hand-built input: partial and NA-item visits produce no total", {
  # the seeded generator always administers complete forms - these are the
  # inputs it never produces, pinning the all-required rule and the LOW
  # branch the built data never reaches (the test-adam-rules.R idiom)
  qs_hand <- tribble(
    ~STUDYID, ~USUBJID, ~QSTESTCD, ~QSTEST,          ~QSSTAT, ~QSBLFL, ~VISITNUM, ~VISIT,      ~QSDTC,       ~QSSTRESN, ~QSSTRESU,
    "S",      "S1",     "MOS01",  "Mood: Cheerful",  NA,      "Y",     2,         "BASELINE",  "2024-01-10", 1,         NA,
    "S",      "S1",     "MOS02",  "Mood: Down",      NA,      "Y",     2,         "BASELINE",  "2024-01-10", 0,         NA,
    "S",      "S1",     "MOS03",  "Sleep Quality",   NA,      "Y",     2,         "BASELINE",  "2024-01-10", 1,         NA,
    "S",      "S1",     "MOS04",  "Energy Level",    NA,      "Y",     2,         "BASELINE",  "2024-01-10", 0,         NA,
    "S",      "S1",     "MOS01",  "Mood: Cheerful",  NA,      NA,      3,         "WEEK 2",    "2024-01-24", 0,         NA,
    "S",      "S1",     "MOS02",  "Mood: Down",      NA,      NA,      3,         "WEEK 2",    "2024-01-24", 1,         NA,
    "S",      "S1",     "MOS03",  "Sleep Quality",   NA,      NA,      3,         "WEEK 2",    "2024-01-24", 1,         NA,
    "S",      "S1",     "MOS04",  "Energy Level",    NA,      NA,      3,         "WEEK 2",    "2024-01-24", 1,         NA,
    # WEEK 4: the form was started but only three items answered
    "S",      "S1",     "MOS01",  "Mood: Cheerful",  NA,      NA,      4,         "WEEK 4",    "2024-02-07", 3,         NA,
    "S",      "S1",     "MOS02",  "Mood: Down",      NA,      NA,      4,         "WEEK 4",    "2024-02-07", 3,         NA,
    "S",      "S1",     "MOS03",  "Sleep Quality",   NA,      NA,      4,         "WEEK 4",    "2024-02-07", 3,         NA,
    # WEEK 8: all four items present but one unparseable - no QSSTRESN
    "S",      "S1",     "MOS01",  "Mood: Cheerful",  NA,      NA,      5,         "WEEK 8",    "2024-02-21", 1,         NA,
    "S",      "S1",     "MOS02",  "Mood: Down",      NA,      NA,      5,         "WEEK 8",    "2024-02-21", 1,         NA,
    "S",      "S1",     "MOS03",  "Sleep Quality",   NA,      NA,      5,         "WEEK 8",    "2024-02-21", 1,         NA,
    "S",      "S1",     "MOS04",  "Energy Level",    NA,      NA,      5,         "WEEK 8",    "2024-02-21", NA,        NA,
    # a screen-failure-like subject: records, no baseline, no TRTSDT
    "S",      "S2",     "MOS01",  "Mood: Cheerful",  NA,      NA,      1,         "SCREENING", "2024-01-05", 0,         NA,
    "S",      "S2",     "MOS02",  "Mood: Down",      NA,      NA,      1,         "SCREENING", "2024-01-05", 1,         NA,
    "S",      "S2",     "MOS03",  "Sleep Quality",   NA,      NA,      1,         "SCREENING", "2024-01-05", 0,         NA,
    "S",      "S2",     "MOS04",  "Energy Level",    NA,      NA,      1,         "SCREENING", "2024-01-05", 1,         NA,
    # S3: the all-zero form - the total sums to 0 and is a real total, the
    # zero-base case the shared PCHG rule leaves unguarded on purpose
    "S",      "S3",     "MOS01",  "Mood: Cheerful",  NA,      "Y",     2,         "BASELINE",  "2024-01-10", 0,         NA,
    "S",      "S3",     "MOS02",  "Mood: Down",      NA,      "Y",     2,         "BASELINE",  "2024-01-10", 0,         NA,
    "S",      "S3",     "MOS03",  "Sleep Quality",   NA,      "Y",     2,         "BASELINE",  "2024-01-10", 0,         NA,
    "S",      "S3",     "MOS04",  "Energy Level",    NA,      "Y",     2,         "BASELINE",  "2024-01-10", 0,         NA,
    "S",      "S3",     "MOS01",  "Mood: Cheerful",  NA,      NA,      3,         "WEEK 2",    "2024-01-24", 0,         NA,
    "S",      "S3",     "MOS02",  "Mood: Down",      NA,      NA,      3,         "WEEK 2",    "2024-01-24", 0,         NA,
    "S",      "S3",     "MOS03",  "Sleep Quality",   NA,      NA,      3,         "WEEK 2",    "2024-01-24", 0,         NA,
    "S",      "S3",     "MOS04",  "Energy Level",    NA,      NA,      3,         "WEEK 2",    "2024-01-24", 0,         NA
  )
  adsl_hand <- tribble(
    ~STUDYID, ~USUBJID, ~TRTSDT,
    "S",      "S1",     as.Date("2024-01-10"),
    "S",      "S2",     as.Date(NA)
  )
  adqs <- derive_adqs(qs_hand, adsl_hand, spec_synth01)

  totals <- adqs |> filter(PARAMCD == "MOSTOT")
  # totals only where all four items carry a value: visits 2 and 3 for S1,
  # the screening visit for S2, both S3 visits - never the 3-item or
  # NA-item visit
  expect_setequal(paste(totals$USUBJID, totals$AVISITN),
                  c("S1 2", "S1 3", "S2 1", "S3 2", "S3 3"))
  # S1 baseline total 2 (1+0+1+0): below the declared 4 - LOW, no change
  bl <- totals |> filter(USUBJID == "S1", AVISITN == 2)
  expect_equal(as.vector(bl$AVAL), 2)
  expect_equal(as.vector(bl$ABLFL), "Y")
  expect_equal(as.vector(bl$ANRIND), "LOW")
  expect_true(is.na(bl$CHG) && is.na(bl$PCHG))
  # S1 week 2 total 3 (0+1+1+1): still LOW, change +1 off the derived baseline
  wk2 <- totals |> filter(USUBJID == "S1", AVISITN == 3)
  expect_equal(as.vector(wk2$AVAL), 3)
  expect_equal(as.vector(wk2$ANRIND), "LOW")
  expect_equal(as.vector(wk2$BASE), 2)
  expect_equal(as.vector(wk2$CHG), 1)
  expect_equal(as.vector(wk2$PCHG), 50)
  # S2: no item baseline anywhere - the total is unanchored but classified
  s2 <- totals |> filter(USUBJID == "S2")
  expect_true(is.na(s2$ABLFL))
  expect_true(all(is.na(s2$BASE) & is.na(s2$CHG) & is.na(s2$PCHG)))
  expect_equal(as.vector(s2$ANRIND), "LOW")
  expect_true(is.na(s2$ADY))

  # S3: an all-zero total is emitted like any other - flagged at its visit
  # and classified LOW (0 < the declared 4) - and its zero baseline makes
  # PCHG NaN by the shared rule's deliberate no-guard, the dataset-level
  # face of the .rule_pchg comment's contract (test-adam-rules.R pins the
  # rule itself)
  s3 <- totals |> filter(USUBJID == "S3")
  expect_setequal(s3$AVISITN, c(2, 3))
  expect_true(all(s3$AVAL == 0))
  expect_equal(as.vector(s3$ANRIND), c("LOW", "LOW"))
  expect_equal(as.vector(s3$ABLFL[s3$AVISITN == 2]), "Y")
  expect_equal(as.vector(s3$BASE[s3$AVISITN == 3]), 0)
  expect_equal(as.vector(s3$CHG[s3$AVISITN == 3]), 0)
  expect_true(is.nan(s3$PCHG[s3$AVISITN == 3]))
  # the same zero-base NaN through the shared rules on the items
  item_w2 <- adqs |> filter(USUBJID == "S3", AVISITN == 3, PARAMCD == "MOS01")
  expect_equal(as.vector(item_w2$CHG), 0)
  expect_true(is.nan(item_w2$PCHG))
})

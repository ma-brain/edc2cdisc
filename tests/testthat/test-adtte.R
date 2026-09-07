# ADTTE: the time-to-event analysis dataset -----------------------------------
# One record per subject per parameter. OS anchors on death (ADSL DTHFL/
# DTHDT) with censoring at the last known alive date; TTAE anchors on the
# first treatment-emergent AE with the same censoring. AVAL uses the house
# day rule (derive_dy_d): no day zero. Pins below were probed from the
# actual build before being asserted.

adtte_fixture <- function() {
  built <- built_suite()
  list(adsl = built$adam$ADSL, adae = built$adam$ADAE, dm = built$sdtm$DM)
}

test_that("derive_adtte keeps one record per subject per parameter", {
  f <- adtte_fixture()
  adtte <- derive_adtte(f$adsl, f$adae)

  expect_equal(nrow(adtte), nrow(f$adsl) * 2) # probed: 24 subjects x 2 params
  expect_equal(nrow(distinct(adtte, USUBJID, PARAMCD)), nrow(adtte))
  expect_setequal(unique(adtte$PARAMCD), c("OS", "TTAE"))
  expect_setequal(
    paste(adtte$USUBJID, adtte$PARAMCD),
    as.vector(t(outer(unique(f$adsl$USUBJID), c("OS", "TTAE"), paste)))
  )
  # sorted like the other ADaM sets: USUBJID, then the parameter
  expect_equal(as.data.frame(adtte),
               as.data.frame(arrange(adtte, USUBJID, PARAMN)))
})

test_that("OS: the deaths are events, everyone else is censored alive", {
  f <- adtte_fixture()
  adtte <- derive_adtte(f$adsl, f$adae)
  os <- adtte |> filter(PARAMCD == "OS")

  deaths <- os |> filter(EVNTDESC == "Death")
  expect_equal(nrow(deaths), 2) # probed: the seeded fatal AE subjects
  expect_setequal(deaths$USUBJID,
                  f$adsl$USUBJID[f$adsl$DTHFL %in% "Y"])
  d1 <- deaths |> filter(USUBJID == "3021-101-016")
  expect_equal(as.vector(d1$ADT), as.Date("2024-06-13"))
  expect_equal(as.vector(d1$STARTDT), as.Date("2024-03-17"))
  # house day rule: event day minus start day, no zero day
  expect_equal(as.vector(d1$AVAL),
               as.numeric(as.Date("2024-06-13") - as.Date("2024-03-17")) + 1)
  expect_equal(as.vector(d1$AVALU), "DAYS")
  expect_equal(as.vector(d1$SRCDOM), "ADSL")
  expect_equal(as.vector(d1$SRCVAR), "DTHDT")

  # censored: the treated survivors carry the last known alive date
  censored <- os |> filter(!is.na(CNSDTDSC))
  expect_equal(nrow(censored), 20) # 22 treated - 2 deaths
  expect_true(all(censored$EVNTDESC %in% NA))
  expect_true(all(censored$ADT == f$adsl$EOSDT[match(
    censored$USUBJID, f$adsl$USUBJID)]))
  c1 <- censored |> filter(USUBJID == "3021-102-014")
  expect_equal(as.vector(c1$CNSDTDSC), "Last known alive date")
  expect_equal(as.vector(c1$SRCVAR), "EOSDT")
})

test_that("TTAE: first treatment-emergent AE or censoring, with traceability", {
  f <- adtte_fixture()
  adtte <- derive_adtte(f$adsl, f$adae)
  ttae <- adtte |> filter(PARAMCD == "TTAE")

  te <- f$adae |> filter(TRTEMFL %in% "Y")
  first_ae <- te |>
    arrange(USUBJID, ASTDT, ASEQ) |>
    distinct(USUBJID, .keep_all = TRUE)

  events <- ttae |> filter(!is.na(EVNTDESC))
  expect_equal(nrow(events), nrow(first_ae)) # probed: 15
  expect_setequal(events$USUBJID, first_ae$USUBJID)
  expect_equal(as.vector(events$ADT[match(events$USUBJID, first_ae$USUBJID)]),
               as.vector(first_ae$ASTDT))
  expect_true(all(events$EVNTDESC == "First treatment-emergent adverse event"))
  expect_true(all(events$SRCDOM == "ADAE" & events$SRCVAR == "ASTDT"))
  expect_equal(as.vector(events$SRCSEQ[match(events$USUBJID, first_ae$USUBJID)]),
               as.vector(first_ae$ASEQ))

  # treated subjects without a TE AE are censored at the same alive date
  no_ae <- ttae |> filter(!is.na(CNSDTDSC))
  expect_equal(nrow(no_ae), 22 - nrow(first_ae)) # treated minus events
  expect_true(all(no_ae$EVNTDESC %in% NA))
})

test_that("screen failures carry records without anchors, not zeros", {
  f <- adtte_fixture()
  adtte <- derive_adtte(f$adsl, f$adae)
  sf <- adtte |> filter(USUBJID %in% c("3021-103-006", "3021-103-018"))

  expect_equal(nrow(sf), 4)
  expect_true(all(is.na(sf$STARTDT) & is.na(sf$ADT) & is.na(sf$AVAL)))
  expect_true(all(is.na(sf$EVNTDESC) & is.na(sf$CNSDTDSC)))
})

test_that("derive_adtte labels the analysis columns", {
  f <- adtte_fixture()
  adtte <- derive_adtte(f$adsl, f$adae)
  expect_equal(unlist(var_label(adtte))[c("AVAL", "CNSDTDSC", "SRCSEQ")],
               c(AVAL = "Time to Event",
                 CNSDTDSC = "Reason for Censoring",
                 SRCSEQ = "Source Sequence Number"))
})

test_that("hand-built input: ties, non-TE AEs and untreated subjects", {
  adsl_hand <- tribble(
    ~STUDYID, ~USUBJID, ~TRTSDT,            ~EOSDT,             ~DTHDT,             ~DTHFL,
    "S",      "S1",     as.Date("2024-01-10"), as.Date("2024-02-01"), as.Date(NA),     NA,
    "S",      "S2",     as.Date("2024-01-12"), as.Date("2024-03-01"), as.Date("2024-02-20"), "Y",
    "S",      "S3",     as.Date("2024-01-14"), as.Date(NA),        as.Date(NA),        NA
  )
  adae_hand <- tribble(
    ~STUDYID, ~USUBJID, ~ASEQ, ~ASTDT,             ~TRTEMFL,
    "S",      "S1",     1,     as.Date("2024-01-05"), "N", # pre-treatment: ignored
    "S",      "S1",     2,     as.Date("2024-01-20"), "Y",
    "S",      "S1",     3,     as.Date("2024-01-20"), "Y", # same-day tie: lowest ASEQ wins
    "S",      "S2",     1,     as.Date("2024-01-25"), "Y"  # death beats nothing: TTAE still fires
  )
  adtte <- derive_adtte(adsl_hand, adae_hand)

  # S1 TTAE: the first TE AE, not the pre-treatment one; tie at 2024-01-20
  # resolves to the lowest ASEQ
  s1 <- adtte |> filter(USUBJID == "S1", PARAMCD == "TTAE")
  expect_equal(as.vector(s1$ADT), as.Date("2024-01-20"))
  expect_equal(as.vector(s1$SRCSEQ), 2)
  expect_equal(as.vector(s1$AVAL), 11) # Jan 20 vs Jan 10 start, no zero day
  # S2: OS is a death event even though a TE AE happened earlier
  s2os <- adtte |> filter(USUBJID == "S2", PARAMCD == "OS")
  expect_equal(as.vector(s2os$EVNTDESC), "Death")
  expect_equal(as.vector(s2os$AVAL), 40) # Feb 20 vs Jan 12
  # S3: no EOSDT, no AE - censored in description but without a date
  s3 <- adtte |> filter(USUBJID == "S3")
  expect_true(all(is.na(s3$ADT) & is.na(s3$AVAL)))
  expect_true(all(is.na(s3$CNSDTDSC) & is.na(s3$EVNTDESC)))
})

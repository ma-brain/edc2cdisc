# Meta-tests: corrupt an input, assert the validator catches it ------------
# The validators are only useful if they actually fire. Rather than trusting
# a green run, break the data on purpose and check the right check trips.

build_fixtures <- function() {
  # the build is the session-cached one (helper-edc2cdisc.R); the extract
  # directory it read stays in the return shape because one meta-test
  # re-reads a raw AE form from it
  list(built = built_suite(),
       ext = file.path(tempdir(), "edc2cdisc-suite", "SYNTH01"))
}

test_that("a flipped SAFFL trips the SAFFL/TRTSDT coherence check", {
  built <- build_fixtures()$built
  adsl <- built$adam$ADSL
  adsl$SAFFL[3] <- ifelse(adsl$SAFFL[3] == "Y", "N", "Y")

  issues <- validate_adam(adsl, built$adam$ADAE, built$adam$ADCM, built$adam$ADVS, built$adam$ADEG,
                          built$adam$ADLB, built$adam$ADQS, built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS,
                          built$sdtm$AE, built$sdtm$CM, built$sdtm$VS, built$sdtm$QS, built$sdtm$EG,
                          built$sdtm$LB,
                          built$sdtm$SUPPAE, spec_synth01)
  expect_true("saffl-trtsdt-coherence" %in% issues$check)
  expect_error(stop_on_error(issues, "meta"), "1 validation error")
})

test_that("a blanked EOSSTT or DCSREAS on a death subject is still flagged", {
  built <- build_fixtures()$built
  adsl <- built$adam$ADSL
  death <- which(adsl$DTHFL == "Y")[1]
  expect_gte(length(which(adsl$DTHFL == "Y")), 1)

  adsl$EOSSTT[death] <- NA_character_
  issues <- validate_adam(adsl, built$adam$ADAE, built$adam$ADCM, built$adam$ADVS,
                          built$adam$ADEG, built$adam$ADLB, built$adam$ADQS, built$adam$ADTTE,
                          built$sdtm$DM, built$sdtm$DS, built$sdtm$AE, built$sdtm$CM,
                          built$sdtm$VS, built$sdtm$QS, built$sdtm$EG, built$sdtm$LB,
                          built$sdtm$SUPPAE, spec_synth01)
  expect_true("dth-derivation-inconsistent" %in% issues$check)

  adsl2 <- built$adam$ADSL
  adsl2$DCSREAS[death] <- NA_character_
  issues2 <- validate_adam(adsl2, built$adam$ADAE, built$adam$ADCM, built$adam$ADVS,
                           built$adam$ADEG, built$adam$ADLB, built$adam$ADQS, built$adam$ADTTE,
                           built$sdtm$DM, built$sdtm$DS, built$sdtm$AE, built$sdtm$CM,
                           built$sdtm$VS, built$sdtm$QS, built$sdtm$EG, built$sdtm$LB,
                           built$sdtm$SUPPAE, spec_synth01)
  expect_true("dth-derivation-inconsistent" %in% issues2$check)
})

test_that("a flipped TRTEMFL is recomputed and flagged", {
  built <- build_fixtures()$built
  adae <- built$adam$ADAE
  treated <- adae$TRTEMFL == "Y"
  adae$TRTEMFL[which(treated)[1]] <- ""

  issues <- validate_adam(built$adam$ADSL, adae, built$adam$ADCM, built$adam$ADVS, built$adam$ADEG,
                          built$adam$ADLB, built$adam$ADQS, built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS,
                          built$sdtm$AE, built$sdtm$CM, built$sdtm$VS, built$sdtm$QS, built$sdtm$EG,
                          built$sdtm$LB,
                          built$sdtm$SUPPAE, spec_synth01)
  expect_true("trtemfl-not-derivable" %in% issues$check)
})

test_that("a blanked TRTEMFL is recomputed and flagged", {
  built <- build_fixtures()$built
  adae <- built$adam$ADAE
  treated <- which(adae$TRTEMFL == "Y")
  adae$TRTEMFL[treated[1]] <- NA_character_

  issues <- validate_adam(built$adam$ADSL, adae, built$adam$ADCM, built$adam$ADVS, built$adam$ADEG,
                          built$adam$ADLB, built$adam$ADQS, built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS,
                          built$sdtm$AE, built$sdtm$CM, built$sdtm$VS, built$sdtm$QS, built$sdtm$EG,
                          built$sdtm$LB,
                          built$sdtm$SUPPAE, spec_synth01)
  expect_true("trtemfl-not-derivable" %in% issues$check)
})

test_that("a blanked TRTDURD is flagged", {
  built <- build_fixtures()$built
  adsl <- built$adam$ADSL
  i <- which(!is.na(adsl$TRTSDT) & !is.na(adsl$TRTEDT) & !is.na(adsl$TRTDURD))[1]
  adsl$TRTDURD[i] <- NA_integer_

  issues <- validate_adam(adsl, built$adam$ADAE, built$adam$ADCM, built$adam$ADVS, built$adam$ADEG,
                          built$adam$ADLB, built$adam$ADQS, built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS,
                          built$sdtm$AE, built$sdtm$CM, built$sdtm$VS, built$sdtm$QS, built$sdtm$EG,
                          built$sdtm$LB,
                          built$sdtm$SUPPAE, spec_synth01)
  expect_true("trtdurd-wrong" %in% issues$check)
})

test_that("an ADCM study day that disagrees with its anchor is caught", {
  built <- build_fixtures()$built
  adcm <- built$adam$ADCM
  i <- which(!is.na(adcm$ASTDY))[1]
  adcm$ASTDY[i] <- adcm$ASTDY[i] + 1L

  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, adcm,
                          built$adam$ADVS, built$adam$ADEG, built$adam$ADLB, built$adam$ADQS,
                          built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS, built$sdtm$AE,
                          built$sdtm$CM, built$sdtm$VS, built$sdtm$QS, built$sdtm$EG,
                          built$sdtm$LB, built$sdtm$SUPPAE, spec_synth01)
  expect_true("adcm-astdy-wrong-anchor" %in% issues$check)
})

test_that("a dropped CM record breaks ADCM coverage loudly", {
  built <- build_fixtures()$built
  adcm <- built$adam$ADCM[-1, ]       # a CM record vanishes from ADCM

  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, adcm,
                          built$adam$ADVS, built$adam$ADEG, built$adam$ADLB, built$adam$ADQS,
                          built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS, built$sdtm$AE,
                          built$sdtm$CM, built$sdtm$VS, built$sdtm$QS, built$sdtm$EG,
                          built$sdtm$LB, built$sdtm$SUPPAE, spec_synth01)
  expect_true("adcm-lost-record" %in% issues$check)
})

test_that("an ADCM row with no CM record and a bad flag are caught", {
  built <- build_fixtures()$built

  extra <- built$adam$ADCM[1, ]
  extra$ASEQ <- 99L                   # no SDTM CM record behind it
  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE,
                          bind_rows(built$adam$ADCM, extra),
                          built$adam$ADVS, built$adam$ADEG, built$adam$ADLB, built$adam$ADQS,
                          built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS, built$sdtm$AE,
                          built$sdtm$CM, built$sdtm$VS, built$sdtm$QS, built$sdtm$EG,
                          built$sdtm$LB, built$sdtm$SUPPAE, spec_synth01)
  expect_true("adcm-extra-record" %in% issues$check)

  adcm <- built$adam$ADCM
  adcm$ASTDTF[1] <- "X"               # imputing nothing is not a flag value
  issues2 <- validate_adam(built$adam$ADSL, built$adam$ADAE, adcm,
                           built$adam$ADVS, built$adam$ADEG, built$adam$ADLB, built$adam$ADQS,
                           built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS, built$sdtm$AE,
                           built$sdtm$CM, built$sdtm$VS, built$sdtm$QS, built$sdtm$EG,
                           built$sdtm$LB, built$sdtm$SUPPAE, spec_synth01)
  expect_true("adcm-imputation-flag-bad" %in% issues2$check)
})

test_that("an ADCM end date moved before the start is caught", {
  built <- build_fixtures()$built
  adcm <- built$adam$ADCM
  i <- which(!is.na(adcm$ASTDT) & !is.na(adcm$AENDT))[1]
  adcm$AENDT[i] <- adcm$ASTDT[i] - 1

  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, adcm,
                          built$adam$ADVS, built$adam$ADEG, built$adam$ADLB, built$adam$ADQS,
                          built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS, built$sdtm$AE,
                          built$sdtm$CM, built$sdtm$VS, built$sdtm$QS, built$sdtm$EG,
                          built$sdtm$LB, built$sdtm$SUPPAE, spec_synth01)
  expect_true("adcm-aendt-before-astdt" %in% issues$check)
})

test_that("a blanked ADVS ADY is flagged", {
  built <- build_fixtures()$built
  advs <- built$adam$ADVS
  i <- which(!is.na(advs$ADY))[1]
  advs$ADY[i] <- NA_integer_

  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM, advs,
                          built$adam$ADEG, built$adam$ADLB, built$adam$ADQS, built$adam$ADTTE, built$sdtm$DM,
                          built$sdtm$DS, built$sdtm$AE, built$sdtm$CM,
                          built$sdtm$VS, built$sdtm$QS, built$sdtm$EG, built$sdtm$LB,
                          built$sdtm$SUPPAE, spec_synth01)
  expect_true("advs-ady-wrong" %in% issues$check)
})

test_that("an ADVS row whose upper bound went missing cannot stay NORMAL", {
  built <- build_fixtures()$built
  advs <- built$adam$ADVS
  i <- which(!is.na(advs$ANRHI) & advs$ANRIND == "NORMAL")[1]
  advs$ANRHI[i] <- NA # the range is now one-sided; NORMAL is unsupported

  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM, advs,
                          built$adam$ADEG, built$adam$ADLB, built$adam$ADQS, built$adam$ADTTE, built$sdtm$DM,
                          built$sdtm$DS, built$sdtm$AE, built$sdtm$CM,
                          built$sdtm$VS, built$sdtm$QS, built$sdtm$EG, built$sdtm$LB,
                          built$sdtm$SUPPAE, spec_synth01)
  expect_true("advs-anrind-wrong" %in% issues$check)
})

test_that("a blanked ADEG ADY is flagged", {
  built <- build_fixtures()$built
  adeg <- built$adam$ADEG
  i <- which(!is.na(adeg$ADY))[1]
  adeg$ADY[i] <- NA_integer_

  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM,
                          built$adam$ADVS, adeg, built$adam$ADLB, built$adam$ADQS,
                          built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS, built$sdtm$AE,
                          built$sdtm$CM, built$sdtm$VS, built$sdtm$QS, built$sdtm$EG,
                          built$sdtm$LB, built$sdtm$SUPPAE, spec_synth01)
  expect_true("adeg-ady-wrong" %in% issues$check)
})

test_that("an ADEG ANRIND flipped without a reason is caught", {
  built <- build_fixtures()$built
  adeg <- built$adam$ADEG
  i <- which(adeg$ANRIND == "NORMAL")[1]
  adeg$ANRIND[i] <- "HIGH"            # an in-range interval is not HIGH

  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM,
                          built$adam$ADVS, adeg, built$adam$ADLB, built$adam$ADQS,
                          built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS, built$sdtm$AE,
                          built$sdtm$CM, built$sdtm$VS, built$sdtm$QS, built$sdtm$EG,
                          built$sdtm$LB, built$sdtm$SUPPAE, spec_synth01)
  expect_true("adeg-anrind-wrong" %in% issues$check)
})

test_that("a dropped EG record breaks ADEG coverage loudly", {
  built <- build_fixtures()$built
  adeg <- built$adam$ADEG
  adeg <- adeg[-which(is.na(adeg$ABLFL))[1], ]   # a non-baseline row

  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM,
                          built$adam$ADVS, adeg, built$adam$ADLB, built$adam$ADQS,
                          built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS, built$sdtm$AE,
                          built$sdtm$CM, built$sdtm$VS, built$sdtm$QS, built$sdtm$EG,
                          built$sdtm$LB, built$sdtm$SUPPAE, spec_synth01)
  expect_true("adeg-coverage" %in% issues$check)
})

test_that("an ABLFL flag flipped to N loses ADEG its baseline anchor", {
  built <- build_fixtures()$built
  adeg <- built$adam$ADEG
  i <- which(adeg$ABLFL == "Y")[1]
  adeg$ABLFL[i] <- "N"

  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM,
                          built$adam$ADVS, adeg, built$adam$ADLB, built$adam$ADQS,
                          built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS, built$sdtm$AE,
                          built$sdtm$CM, built$sdtm$VS, built$sdtm$QS, built$sdtm$EG,
                          built$sdtm$LB, built$sdtm$SUPPAE, spec_synth01)
  expect_true("adeg-ablfl-missing" %in% issues$check)
})

test_that("an ADEG value moved outside its range cannot stay NORMAL", {
  built <- build_fixtures()$built
  adeg <- built$adam$ADEG
  i <- which(adeg$ANRIND == "NORMAL" & is.na(adeg$ABLFL))[1]
  adeg$AVAL[i] <- 9999                # msec - outside every declared interval
  # ANRIND left "NORMAL": the value and its classification now disagree

  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM,
                          built$adam$ADVS, adeg, built$adam$ADLB, built$adam$ADQS,
                          built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS, built$sdtm$AE,
                          built$sdtm$CM, built$sdtm$VS, built$sdtm$QS, built$sdtm$EG,
                          built$sdtm$LB, built$sdtm$SUPPAE, spec_synth01)
  expect_true("adeg-anrind-wrong" %in% issues$check)
})

# ADQS: the derived total is recomputed from SDTM QS, never trusted ------

test_that("a MOSTOT AVAL that is not the sum of its QS items is flagged, alone", {
  built <- build_fixtures()$built
  adqs <- built$adam$ADQS
  # fabricate on a screen-failure total row: no BASE, so the CHG recompute
  # has nothing to co-fire on and the fabrication is owned by the total
  # recompute and nothing else
  i <- which(adqs$PARAMCD == "MOSTOT" & is.na(adqs$BASE))[1]
  adqs$AVAL[i] <- adqs$AVAL[i] + 1    # the total no longer sums its items

  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM,
                          built$adam$ADVS, built$adam$ADEG, built$adam$ADLB,
                          adqs, built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS, built$sdtm$AE,
                          built$sdtm$CM, built$sdtm$VS, built$sdtm$QS,
                          built$sdtm$EG, built$sdtm$LB, built$sdtm$SUPPAE,
                          spec_synth01)
  expect_setequal(issues$check, "adqs-total-wrong")
})

test_that("a duplicated SDTM baseline flag fans the join out and is caught", {
  # the CRF never flags two baselines, but a hand-edited QS can: the
  # item_base join fans every flagged item's rows out and the total inherits
  # a second anchor. The builder does not defend against it (that would be
  # a cross-dataset redesign); the validator owns it - five checks trip.
  built <- build_fixtures()$built
  qs2 <- built$sdtm$QS
  hit <- qs2$USUBJID == "3021-102-014" & qs2$QSTESTCD == "MOS01" &
    qs2$VISITNUM == 3
  qs2$QSBLFL[hit] <- "Y"
  # dplyr announces the fan-out as many-to-many warnings at the baseline
  # joins - the announcement IS the corruption, so it is silenced here and
  # the row count and the fired checks below are the loud part
  adqs <- suppressWarnings(derive_adqs(qs2, built$adam$ADSL, spec_synth01))
  expect_equal(nrow(adqs), 662)       # probed: 650 clean rows fan out by 12

  issues <- suppressWarnings(validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM,
                                           built$adam$ADVS, built$adam$ADEG, built$adam$ADLB,
                                           adqs, built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS, built$sdtm$AE,
                                           built$sdtm$CM, built$sdtm$VS, built$sdtm$QS,
                                           built$sdtm$EG, built$sdtm$LB, built$sdtm$SUPPAE,
                                           spec_synth01))
  expect_setequal(unique(issues$check),
                  c("adqs-key-not-unique", "adqs-coverage", "adqs-ablfl-multi",
                    "adqs-ablfl-not-from-qs", "adqs-base-wrong"))
})

test_that("total coverage is checked in both directions", {
  built <- build_fixtures()$built
  adqs <- built$adam$ADQS
  i <- which(adqs$PARAMCD == "MOSTOT")[1]

  # a visit whose items are all present must keep its total row
  lost <- adqs[-i, ]
  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM,
                          built$adam$ADVS, built$adam$ADEG, built$adam$ADLB,
                          lost, built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS, built$sdtm$AE,
                          built$sdtm$CM, built$sdtm$VS, built$sdtm$QS,
                          built$sdtm$EG, built$sdtm$LB, built$sdtm$SUPPAE,
                          spec_synth01)
  expect_true("adqs-total-coverage" %in% issues$check)

  # ...and a total may not exist for a visit whose items are absent
  extra <- adqs[i, ]
  extra$AVISITN <- 99                 # no QS collection at visit 99
  issues2 <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM,
                           built$adam$ADVS, built$adam$ADEG, built$adam$ADLB,
                           bind_rows(adqs, extra),
                           built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS, built$sdtm$AE,
                           built$sdtm$CM, built$sdtm$VS, built$sdtm$QS,
                           built$sdtm$EG, built$sdtm$LB, built$sdtm$SUPPAE,
                           spec_synth01)
  expect_true("adqs-total-coverage" %in% issues2$check)
})

test_that("a blanked ADQS ADY is flagged", {
  built <- build_fixtures()$built
  adqs <- built$adam$ADQS
  i <- which(!is.na(adqs$ADY))[1]
  adqs$ADY[i] <- NA_integer_

  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM,
                          built$adam$ADVS, built$adam$ADEG, built$adam$ADLB,
                          adqs, built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS, built$sdtm$AE,
                          built$sdtm$CM, built$sdtm$VS, built$sdtm$QS,
                          built$sdtm$EG, built$sdtm$LB, built$sdtm$SUPPAE,
                          spec_synth01)
  expect_true("adqs-ady-wrong" %in% issues$check)
})

test_that("a flipped total ABLFL loses ADQS its derived baseline anchor", {
  built <- build_fixtures()$built
  adqs <- built$adam$ADQS
  i <- which(adqs$PARAMCD == "MOSTOT" & adqs$ABLFL %in% "Y")[1]
  adqs$ABLFL[i] <- "N"

  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM,
                          built$adam$ADVS, built$adam$ADEG, built$adam$ADLB,
                          adqs, built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS, built$sdtm$AE,
                          built$sdtm$CM, built$sdtm$VS, built$sdtm$QS,
                          built$sdtm$EG, built$sdtm$LB, built$sdtm$SUPPAE,
                          spec_synth01)
  expect_true("adqs-ablfl-missing" %in% issues$check)
})

test_that("a MOSTOT ANRIND flipped without a reason is caught", {
  built <- build_fixtures()$built
  adqs <- built$adam$ADQS
  i <- which(adqs$PARAMCD == "MOSTOT" & adqs$ANRIND == "NORMAL")[1]
  adqs$ANRIND[i] <- "HIGH"            # an in-range total is not HIGH

  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM,
                          built$adam$ADVS, built$adam$ADEG, built$adam$ADLB,
                          adqs, built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS, built$sdtm$AE,
                          built$sdtm$CM, built$sdtm$VS, built$sdtm$QS,
                          built$sdtm$EG, built$sdtm$LB, built$sdtm$SUPPAE,
                          spec_synth01)
  expect_true("adqs-anrind-wrong" %in% issues$check)
})

test_that("a MOSTOT ANRLO drift the classification survives is caught", {
  built <- build_fixtures()$built
  adqs <- built$adam$ADQS
  i <- which(adqs$PARAMCD == "MOSTOT")[1]
  adqs$ANRLO[i] <- adqs$ANRLO[i] + 1  # 4 -> 5: the 6-point total stays NORMAL

  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM,
                          built$adam$ADVS, built$adam$ADEG, built$adam$ADLB,
                          adqs, built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS, built$sdtm$AE,
                          built$sdtm$CM, built$sdtm$VS, built$sdtm$QS,
                          built$sdtm$EG, built$sdtm$LB, built$sdtm$SUPPAE,
                          spec_synth01)
  expect_false("adqs-anrind-wrong" %in% issues$check) # the class still holds ...
  expect_true("adqs-range-spec-drift" %in% issues$check) # ... the drift shows
  expect_setequal(issues$check, "adqs-range-spec-drift")
})

test_that("an item ANRLO drift the classification survives is caught", {
  built <- build_fixtures()$built
  adqs <- built$adam$ADQS
  i <- which(adqs$PARAMCD == "MOS01")[1]
  adqs$ANRLO[i] <- 1  # the spec declares NA/NA for ordinal items: any value drifts

  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM,
                          built$adam$ADVS, built$adam$ADEG, built$adam$ADLB,
                          adqs, built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS, built$sdtm$AE,
                          built$sdtm$CM, built$sdtm$VS, built$sdtm$QS,
                          built$sdtm$EG, built$sdtm$LB, built$sdtm$SUPPAE,
                          spec_synth01)
  expect_false("adqs-anrind-wrong" %in% issues$check) # one bound still cannot classify
  expect_true("adqs-range-spec-drift" %in% issues$check)
  expect_setequal(issues$check, "adqs-range-spec-drift")
})

test_that("a QS item missing from spec$bds trips the spec check, not silence", {
  built <- build_fixtures()$built

  # a spec whose ADQS block forgot MOS03: the builder keeps spec'd items
  # only, so the disagreement lives between QS and the spec, not in ADQS
  spec2 <- spec_synth01
  spec2$bds <- spec2$bds[!(spec2$bds$domain == "ADQS" &
                             spec2$bds$paramcd == "MOS03"), ]

  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM,
                          built$adam$ADVS, built$adam$ADEG, built$adam$ADLB,
                          built$adam$ADQS, built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS,
                          built$sdtm$AE, built$sdtm$CM, built$sdtm$VS,
                          built$sdtm$QS, built$sdtm$EG, built$sdtm$LB,
                          built$sdtm$SUPPAE, spec2)
  expect_true("adqs-item-not-in-spec" %in% issues$check)
  expect_true("MOS03" %in% issues$detail[issues$check == "adqs-item-not-in-spec"])

  # against the clean spec every performed item is declared
  issues2 <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM,
                           built$adam$ADVS, built$adam$ADEG, built$adam$ADLB,
                           built$adam$ADQS, built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS,
                           built$sdtm$AE, built$sdtm$CM, built$sdtm$VS,
                           built$sdtm$QS, built$sdtm$EG, built$sdtm$LB,
                           built$sdtm$SUPPAE, spec_synth01)
  expect_false("adqs-item-not-in-spec" %in% issues2$check)
})

# ADTTE: the time-to-event is recomputed and read back against ADSL/ADAE --

test_that("a fabricated OS AVAL on a censored row is flagged, alone", {
  built <- build_fixtures()$built
  adtte <- built$adam$ADTTE
  i <- which(adtte$PARAMCD == "OS" & !is.na(adtte$CNSDTDSC))[1]
  adtte$AVAL[i] <- adtte$AVAL[i] + 5
  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM,
                          built$adam$ADVS, built$adam$ADEG, built$adam$ADLB,
                          built$adam$ADQS, adtte, built$sdtm$DM, built$sdtm$DS,
                          built$sdtm$AE, built$sdtm$CM, built$sdtm$VS,
                          built$sdtm$QS, built$sdtm$EG, built$sdtm$LB,
                          built$sdtm$SUPPAE, spec_synth01)
  expect_setequal(issues$check, "adtte-aval-wrong")
})

test_that("an OS death moved off the ADSL DTHDT is caught", {
  built <- build_fixtures()$built
  adtte <- built$adam$ADTTE
  i <- which(adtte$PARAMCD == "OS" & adtte$EVNTDESC %in% "Death")[1]
  adtte$ADT[i] <- adtte$ADT[i] + 1
  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM,
                          built$adam$ADVS, built$adam$ADEG, built$adam$ADLB,
                          built$adam$ADQS, adtte, built$sdtm$DM, built$sdtm$DS,
                          built$sdtm$AE, built$sdtm$CM, built$sdtm$VS,
                          built$sdtm$QS, built$sdtm$EG, built$sdtm$LB,
                          built$sdtm$SUPPAE, spec_synth01)
  expect_setequal(unique(issues$check),
                  c("adtte-aval-wrong", "adtte-os-not-from-adsl"))
})

test_that("a dropped subject breaks ADTTE coverage loudly", {
  built <- build_fixtures()$built
  adtte <- built$adam$ADTTE
  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM,
                          built$adam$ADVS, built$adam$ADEG, built$adam$ADLB,
                          built$adam$ADQS, adtte[adtte$USUBJID != "3021-102-014", ],
                          built$sdtm$DM, built$sdtm$DS,
                          built$sdtm$AE, built$sdtm$CM, built$sdtm$VS,
                          built$sdtm$QS, built$sdtm$EG, built$sdtm$LB,
                          built$sdtm$SUPPAE, spec_synth01)
  expect_setequal(issues$check, "adtte-coverage")
})

test_that("a TTAE event fabricated on a subject with no TE AE is caught", {
  built <- build_fixtures()$built
  adtte <- built$adam$ADTTE
  i <- which(adtte$PARAMCD == "TTAE" & !is.na(adtte$CNSDTDSC))[1]
  adtte$CNSDTDSC[i] <- NA_character_
  adtte$EVNTDESC[i] <- "First treatment-emergent adverse event"
  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM,
                          built$adam$ADVS, built$adam$ADEG, built$adam$ADLB,
                          built$adam$ADQS, adtte, built$sdtm$DM, built$sdtm$DS,
                          built$sdtm$AE, built$sdtm$CM, built$sdtm$VS,
                          built$sdtm$QS, built$sdtm$EG, built$sdtm$LB,
                          built$sdtm$SUPPAE, spec_synth01)
  expect_setequal(issues$check, "adtte-ttae-not-first-te-ae")
})

test_that("a clean build passes the ADaM validator with zero findings", {
  built <- build_fixtures()$built
  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM,
                          built$adam$ADVS, built$adam$ADEG, built$adam$ADLB, built$adam$ADQS,
                          built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS, built$sdtm$AE,
                          built$sdtm$CM, built$sdtm$VS, built$sdtm$QS, built$sdtm$EG,
                          built$sdtm$LB, built$sdtm$SUPPAE, spec_synth01)
  expect_equal(nrow(issues), 0)
  expect_error(stop_on_error(issues, "meta"), NA)
})

test_that("a zero study day is caught in SDTM and ADaM", {
  built <- build_fixtures()$built
  ae <- built$sdtm$AE
  ae$AESTDY[1] <- 0L
  domains <- built$sdtm
  domains$AE <- ae
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("study-day-zero" %in% issues$check)

  adae <- built$adam$ADAE
  adae$AENDY[1] <- 0L
  issues <- validate_adam(built$adam$ADSL, adae, built$adam$ADCM,
                          built$adam$ADVS, built$adam$ADEG, built$adam$ADLB, built$adam$ADQS,
                          built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS, built$sdtm$AE,
                          built$sdtm$CM, built$sdtm$VS, built$sdtm$QS, built$sdtm$EG,
                          built$sdtm$LB, built$sdtm$SUPPAE, spec_synth01)
  expect_true("adae-study-day-zero" %in% issues$check)
})

test_that("dropping a required column fires required-vars", {
  built <- build_fixtures()$built
  vs <- built$sdtm$VS
  vs$VSTESTCD <- NULL
  domains <- built$sdtm
  domains$VS <- vs
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("required-vars" %in% issues$check[issues$domain == "VS"])
})

test_that("a DM without ARMCD defers the screen-failure sweep to required-vars", {
  # the sweep reads DM's ARMCD to find the screen-failure subjects - a
  # hand-crafted DM without it must report required-vars, not crash
  domains <- build_fixtures()$built$sdtm
  domains$DM$ARMCD <- NULL
  issues <- validate_sdtm(domains, spec_synth01)
  expect_false("screenfail-has-study-day" %in% issues$check)
  expect_true("required-vars" %in% issues$check[issues$domain == "DM"])
})

test_that("the screen-failure sweep skips a domain the caller did not supply", {
  domains <- build_fixtures()$built$sdtm
  domains$EG <- NULL
  issues <- validate_sdtm(domains, spec_synth01)
  expect_false("screenfail-has-study-day" %in% issues$check)
  expect_false(any(issues$domain == "EG"))
})


test_that("a dropped dataset row breaks referential integrity loudly", {
  built <- build_fixtures()$built
  domains <- built$sdtm
  domains$AE <- built$sdtm$AE[-1, ]    # an AE record vanishes
  issues <- validate_sdtm(domains, spec_synth01)
  # SUPPAE / CO still carry the deleted AESEQ -> orphaned related records
  expect_true("related-parent-orphan" %in% issues$check)

  # a lost SUPPAE qualifier row means an AE record without its qualifiers:
  # ADAE refuses to build rather than drifting silently
  suppae <- built$sdtm$SUPPAE
  suppae <- suppae[1:3, ]
  expect_error(derive_adae(built$sdtm$AE, suppae, built$adam$ADSL),
               "no SUPPAE qualifiers")
})

test_that("RELREC record-level links carry a blank RELTYPE, and only those", {
  built <- build_fixtures()$built

  # the built record-level RELREC (IDVAR populated) passes with blank
  # RELTYPE - the one value the old validator refused
  issues <- validate_sdtm(built$sdtm, spec_synth01)
  expect_false("reltype-bad-value" %in% issues$check)

  # ...and populating it on a record-level link is the P21-flagged
  # mistake the validator must catch
  domains <- built$sdtm
  domains$RELREC$RELTYPE[1] <- "ONE"
  issues2 <- validate_sdtm(domains, spec_synth01)
  expect_true("reltype-bad-value" %in% issues2$check)
  expect_equal(unique(issues2$severity[issues2$check == "reltype-bad-value"]),
               "ERROR")
})

test_that("an unmapped codelist value breaks the mapper, not the report", {
  built <- build_fixtures()$built
  fx <- build_fixtures()
  ae_raw <- read_clinical_view("AE", fx$ext)
  ae_raw$AESEV_DECODE[1] <- "CATASTROPHIC"
  refs <- subject_ref(built$sdtm$DM)
  expect_error(map_ae(ae_raw, spec_synth01, refs),
               "unmapped codelist value")
})

test_that("clean data produces an empty issue tibble", {
  built <- build_fixtures()$built
  issues <- validate_sdtm(built$sdtm, spec_synth01)
  expect_equal(nrow(issues), 0)
  expect_s3_class(issues, "tbl_df")
  expect_error(stop_on_error(issues, "meta"), NA)
})

test_that("the AGE bounds come from spec$study and can be widened there", {
  built <- build_fixtures()$built
  adsl <- built$adam$ADSL
  adsl$AGE[1] <- 17

  issues <- validate_adam(adsl, built$adam$ADAE, built$adam$ADCM,
                          built$adam$ADVS, built$adam$ADEG, built$adam$ADLB, built$adam$ADQS,
                          built$adam$ADTTE, built$sdtm$DM, built$sdtm$DS, built$sdtm$AE,
                          built$sdtm$CM, built$sdtm$VS, built$sdtm$QS, built$sdtm$EG,
                          built$sdtm$LB, built$sdtm$SUPPAE, spec_synth01)
  expect_true("age-out-of-range" %in% issues$check)

  # a paediatric protocol is a spec row, not a validator edit
  spec_paed <- new_study_spec(
    study     = dplyr::mutate(spec_synth01$study, age_min = 12),
    sites     = spec_synth01$sites,
    arms      = spec_synth01$arms,
    visits    = spec_synth01$visits,
    codelists = spec_synth01$codelists,
    supp      = spec_synth01$supp,
    units     = spec_synth01$units,
    forms     = spec_synth01$forms,
    tests     = spec_synth01$tests,
    bds       = spec_synth01$bds,
    variables = spec_synth01$variables
  )
  issues2 <- validate_adam(adsl, built$adam$ADAE, built$adam$ADCM, built$adam$ADVS,
                           built$adam$ADEG, built$adam$ADLB, built$adam$ADQS, built$adam$ADTTE, built$sdtm$DM,
                           built$sdtm$DS, built$sdtm$AE, built$sdtm$CM,
                           built$sdtm$VS, built$sdtm$QS, built$sdtm$EG, built$sdtm$LB,
                           built$sdtm$SUPPAE, spec_paed)
  expect_false("age-out-of-range" %in% issues2$check)
})


# Exact-name battery + spec-driven checks -------------------------------
# Combined corruptions in one build; the union of fired checks must equal
# the expected set exactly - a check renaming itself shows up here.

test_that("the corruption battery fires exactly the expected check names", {
  fx <- build_fixtures()
  built <- fx$built

  domains <- built$sdtm
  domains$AE$AESTDY[2] <- 0L                       # study-day-zero
  domains$VS <- domains$VS[, setdiff(names(domains$VS), "VSTESTCD")]  # required-vars
  domains$AE <- domains$AE[-1, ]                   # related-parent-orphan

  sdtm_issues <- validate_sdtm(domains, spec_synth01)
  expect_setequal(
    sdtm_issues$check,
    c("study-day-zero", "required-vars", "related-parent-orphan")
  )
  expect_true(all(sdtm_issues$severity == "ERROR"))
})

test_that("built CT values that are not spec-declared are caught", {
  fx <- build_fixtures()
  built <- fx$built

  domains <- built$sdtm
  domains$AE$AEOUT[domains$AE$AEOUT == "FATAL"][1] <- "UNRESOLVED"
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("ct-value-not-in-spec" %in% issues$check)
  expect_true(all(issues$domain[issues$check == "ct-value-not-in-spec"] == "AE"))

  # SEX's decode default ("U") is allowed without a codelist row
  expect_false("ct-value-not-in-spec" %in%
                 issues$check[issues$domain == "DM"])
})

test_that("an ADaM parameter missing from spec$bds trips the coverage check", {
  fx <- build_fixtures()
  built <- fx$built

  # a spec whose ADVS block forgot TEMP
  spec2 <- spec_synth01
  spec2$bds <- spec2$bds[!(spec2$bds$domain == "ADVS" &
                             spec2$bds$paramcd == "TEMP"), ]

  # derive against the trimmed spec: TEMP loses its ranges/order
  advs <- derive_advs(built$sdtm$VS, built$adam$ADSL, spec2)
  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, built$adam$ADCM, advs,
                          built$adam$ADEG, built$adam$ADLB, built$adam$ADQS, built$adam$ADTTE, built$sdtm$DM,
                          built$sdtm$DS, built$sdtm$AE, built$sdtm$CM,
                          built$sdtm$VS, built$sdtm$QS, built$sdtm$EG, built$sdtm$LB,
                          built$sdtm$SUPPAE, spec2)
  expect_true("advs-param-not-in-spec" %in% issues$check)
  expect_true("TEMP" %in% issues$detail[issues$check == "advs-param-not-in-spec"])
})

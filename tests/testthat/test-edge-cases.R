# Hand-built edge-case inputs the seeded generator never produces - the
# shared blind spot that let the REVIEW-2026-09-03 findings sit behind a
# green suite. Direct mapper/derivation/validator calls; build_all() only
# where the validator over a full domain set is the unit under test.

test_that("map_lb: a blank collected unit no longer voids the result", {
  refs <- tibble(USUBJID = "3021-101-001", RFSTDTC = "2024-04-01",
                 RFENDTC = "2024-06-01")
  lb_raw <- tibble(
    Subject = "101-001", Folder = spec_synth01$visits$Folder[1],
    LBPERF = "1", LBFAST = "0",
    LBDAT_YYYY = "2024", LBDAT_MM = "03", LBDAT_DD = "20", LBTIM = NA,
    GLUC_RAW = "90", GLUC_UN = NA, GLUC_STD = NA, GLUC_STD_UN = NA,
    GLUC_NRLO = "70", GLUC_NRHI = "100"
  )
  lb <- map_lb(lb_raw, spec_synth01, refs)
  row <- lb[lb$LBORRES == "90", ]
  expect_equal(nrow(row), 1)
  # the value survives, unconverted and honestly labelled: the missing
  # unit surfaces as the loud lbstresu-missing ERROR, never as a result
  # voided by an NA-condition if_else
  expect_equal(as.vector(row$LBSTRESN), 90)
  expect_true(is.na(as.vector(row$LBSTRESU)) | as.vector(row$LBSTRESU) == "")
  expect_equal(as.vector(row$LBNRIND), "NORMAL") # compared in collected units
})

test_that("map_lb: a unit the spec does not know labels the value as collected", {
  refs <- tibble(USUBJID = "3021-101-001", RFSTDTC = "2024-04-01",
                 RFENDTC = "2024-06-01")
  lb_raw <- tibble(
    Subject = "101-001", Folder = spec_synth01$visits$Folder[1],
    LBPERF = "1", LBFAST = "0",
    LBDAT_YYYY = "2024", LBDAT_MM = "03", LBDAT_DD = "20", LBTIM = NA,
    GLUC_RAW = "90", GLUC_UN = "umol/L", GLUC_STD = NA, GLUC_STD_UN = NA,
    GLUC_NRLO = "70", GLUC_NRHI = "100"
  )
  lb <- map_lb(lb_raw, spec_synth01, refs)
  row <- lb[lb$LBORRES == "90", ]
  # the mg/dL factor is NOT applied, and the label follows: a converted
  # label on an unconverted value is the one outcome this must never give
  expect_equal(as.vector(row$LBSTRESN), 90)
  expect_equal(as.vector(row$LBSTRESU), "umol/L")
})

test_that("validate_sdtm: an unconfigured collected unit is a WARN", {
  out <- file.path(tempdir(), "edc2cdisc-lb-unit")
  on.exit(unlink(out, recursive = TRUE, force = TRUE), add = TRUE)
  ext <- file.path(out, "rave")
  suppressMessages(generate_rave_extract(out = ext))
  built <- suppressMessages(build_all(ext))

  forms <- suppressMessages(read_rave_extract(dir = ext))
  forms$LB$GLUC_RAW[1] <- "90"
  forms$LB$GLUC_STD[1] <- NA # no EDC conversion: the local factor decides
  forms$LB$GLUC_UN[1] <- "umol/L"
  dm <- map_dm(forms$DM, forms$EX, forms$DS, spec_synth01)
  lb <- map_lb(forms$LB, spec_synth01, subject_ref(dm))

  domains <- built$sdtm
  domains$LB <- lb
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("lb-unit-unmapped" %in% issues$check)
  expect_equal(unique(issues$severity[issues$check == "lb-unit-unmapped"]),
               "WARN")
})

test_that("validate_sdtm: NA DOMAIN is a WARN, not a crash", {
  out <- file.path(tempdir(), "edc2cdisc-dom-na")
  on.exit(unlink(out, recursive = TRUE, force = TRUE), add = TRUE)
  ext <- file.path(out, "rave")
  suppressMessages(generate_rave_extract(out = ext))
  built <- suppressMessages(build_all(ext))

  domains <- built$sdtm
  domains$AE$DOMAIN[1] <- NA
  expect_no_error(issues <- validate_sdtm(domains, spec_synth01))
  expect_true("domain-na" %in% issues$check)
  expect_false("domain-constant" %in% issues$check[issues$domain == "AE"])
})

test_that("validate_sdtm: NA RDOMAIN is a WARN, not a crash", {
  out <- file.path(tempdir(), "edc2cdisc-rdom-na")
  on.exit(unlink(out, recursive = TRUE, force = TRUE), add = TRUE)
  ext <- file.path(out, "rave")
  suppressMessages(generate_rave_extract(out = ext))
  built <- suppressMessages(build_all(ext))

  domains <- built$sdtm
  domains$SUPPAE$RDOMAIN[1] <- NA
  expect_no_error(issues <- validate_sdtm(domains, spec_synth01))
  expect_true("rdomain-na" %in% issues$check)
})

test_that("validate_sdtm: a reduced-precision VSDTC cannot crash the window check", {
  out <- file.path(tempdir(), "edc2cdisc-partial-vs")
  on.exit(unlink(out, recursive = TRUE, force = TRUE), add = TRUE)
  ext <- file.path(out, "rave")
  suppressMessages(generate_rave_extract(out = ext))
  built <- suppressMessages(build_all(ext))

  domains <- built$sdtm
  domains$VS$VSDTC[1] <- "2024-03" # partial: not as.Date()-able
  expect_no_error(issues <- validate_sdtm(domains, spec_synth01))
  expect_true(all(issues$severity[issues$check == "vsdtc-outside-sv-window"] %in%
                    "WARN"))
})

test_that("derive_adsl: a partial DSSTDTC is dropped from LSTALVDT, not fatal", {
  out <- file.path(tempdir(), "edc2cdisc-partial-ds")
  on.exit(unlink(out, recursive = TRUE, force = TRUE), add = TRUE)
  ext <- file.path(out, "rave")
  suppressMessages(generate_rave_extract(out = ext))

  forms <- suppressMessages(read_rave_extract(dir = ext))
  forms$DS$DSSTDAT_DD[1] <- NA # UNK day: DSSTDTC "2024-04", not as.Date()-able
  dm <- map_dm(forms$DM, forms$EX, forms$DS, spec_synth01)
  refs <- subject_ref(dm)
  ds <- map_ds(forms$DS, spec_synth01, refs)
  ex <- map_ex(forms$EX, spec_synth01, refs)
  adsl <- derive_adsl(dm, ex, ds) # was: charToDate() hard error

  u <- adsl$USUBJID == "3021-101-001"
  expect_true(any(u))
  # the partial contact date cannot anchor LSTALVDT; the other anchors win
  expect_equal(adsl$LSTALVDT[u], adsl$TRTEDT[u])
})

test_that("map_suppex: NA EXOCCUR warns and counts as not dosed", {
  out <- file.path(tempdir(), "edc2cdisc-exoccur-na")
  on.exit(unlink(out, recursive = TRUE, force = TRUE), add = TRUE)
  ext <- file.path(out, "rave")
  suppressMessages(generate_rave_extract(out = ext))

  forms <- suppressMessages(read_rave_extract(dir = ext))
  forms$EX$EXOCCUR[1] <- NA
  dm <- map_dm(forms$DM, forms$EX, forms$DS, spec_synth01)
  refs <- subject_ref(dm)
  ex_built <- map_ex(forms$EX, spec_synth01, refs)
  # was: sum(NA) -> guard if(NA) -> "missing value where TRUE/FALSE needed"
  expect_warning(suppex <- map_suppex(forms$EX, ex_built, spec_synth01),
                 "NA EXOCCUR")
  expect_gt(nrow(suppex), 0)
})

test_that("generate_rave_extract leaves the caller's RNG stream alone", {
  out <- file.path(tempdir(), "edc2cdisc-rng")
  on.exit(unlink(out, recursive = TRUE, force = TRUE), add = TRUE)
  set.seed(7)
  runif(1)
  seed_before <- .Random.seed # nolint: object_name_linter. (.Random.seed is a required base name)
  suppressMessages(generate_rave_extract(out = file.path(out, "rave")))
  expect_identical(.Random.seed, seed_before) # nolint: object_name_linter. (.Random.seed is a required base name)
})

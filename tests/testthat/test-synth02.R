# The SYNTH02 proof: a second study that is only a spec change ------------
#
# SYNTH02 shares SYNTH01's CRF family but differs in everything a real study
# changes: ids, sites, arms, visit schedule, codelist decodes, lab panel, and
# two extra collected fields (a respiratory rate, a lot number). The
# generator is parameterized per study; the mapping pipeline is not
# parameterized at all - it reads the spec. Everything here goes through the
# same build_all() the SYNTH01 tests use, with spec_synth02.

test_that("SYNTH02 generates a reproducible, distinct extract", {
  a <- file.path(tempdir(), "synth02-repro-a")
  b <- file.path(tempdir(), "synth02-repro-b")
  on.exit(unlink(c(a, b), recursive = TRUE, force = TRUE), add = TRUE)

  suppressMessages(generate_rave_extract(out = a, study = "SYNTH02"))
  suppressMessages(generate_rave_extract(out = b, study = "SYNTH02"))
  expect_setequal(list.files(a), list.files(b))
  for (f in list.files(a)) {
    expect_identical(
      readLines(file.path(a, f), warn = FALSE),
      readLines(file.path(b, f), warn = FALSE),
      label = sprintf("%s identical across runs", f)
    )
  }
})

test_that("study = \"SYNTH01\" regenerates the committed default study", {
  fixture <- readRDS(testthat::test_path("fixtures", "generator-digests.rds"))
  scratch <- file.path(tempdir(), "synth01-explicit")
  on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)

  suppressMessages(generate_rave_extract(out = scratch, study = "SYNTH01"))
  for (f in names(fixture)) {
    expect_identical(unname(tools::md5sum(file.path(scratch, f))),
                     fixture[[f]], label = sprintf("digest of %s", f))
  }
})

test_that("an unknown study name is rejected", {
  expect_error(generate_rave_extract(out = file.path(tempdir(), "nope"),
                                     study = "SYNTH99"),
               "unknown study")
})

test_that("SYNTH02 runs end-to-end through build_all with its own spec", {
  out <- file.path(tempdir(), "synth02-e2e")
  on.exit(unlink(out, recursive = TRUE, force = TRUE), add = TRUE)
  ext <- file.path(out, "rave")
  suppressMessages(generate_rave_extract(out = ext, study = "SYNTH02"))

  built <- suppressMessages(build_all(ext, spec = spec_synth02,
                                      sdtm_dir = file.path(out, "sdtm"),
                                      adam_dir = file.path(out, "adam")))

  # the whole point: both validators are clean, with no mapper changes
  expect_equal(nrow(validate_sdtm(built$sdtm, spec_synth02)), 0)
  expect_equal(nrow(validate_adam(built$adam$ADSL, built$adam$ADAE,
                                  built$adam$ADVS, built$adam$ADLB,
                                  built$sdtm$DM, built$sdtm$DS,
                                  built$sdtm$AE, built$sdtm$VS,
                                  built$sdtm$LB, built$sdtm$SUPPAE,
                                  spec_synth02)), 0)

  # study constants and sites
  dm <- built$sdtm$DM
  expect_equal(nrow(dm), 18L)
  expect_true(all(dm$STUDYID == "4033"))
  expect_true(all(startsWith(dm$USUBJID, "4033-")))
  expect_setequal(unique(dm$SITEID), c("201", "202", "203"))
  expect_setequal(unique(dm$COUNTRY), c("ESP", "FRA", "GBR"))

  # four SYN-201 arms, two screen failures, two late deaths
  expect_setequal(setdiff(unique(dm$ARM), "Screen Failure"),
                  c("Placebo", "SYN-201 25 mg", "SYN-201 50 mg",
                    "SYN-201 100 mg"))
  expect_equal(sum(dm$ARMCD == "SCRNFAIL"), 2L)
  expect_equal(sum(dm$DTHFL %in% "Y"), 2L)

  # visit schedule: Week 12, no Week 2
  sv_visits <- unique(built$sdtm$SV$VISIT)
  expect_true("WEEK 12" %in% sv_visits)
  expect_false("WEEK 2" %in% sv_visits)

  # the extra vital sign reaches SDTM and ADVS with its declared range
  expect_true("RESP" %in% unique(built$sdtm$VS$VSTESTCD))
  resp <- built$adam$ADVS |> dplyr::filter(PARAMCD == "RESP")
  expect_gt(nrow(resp), 0)
  expect_equal(unique(resp$ANRLO), 12)
  expect_equal(unique(resp$ANRHI), 20)

  # the extra analyte reaches SDTM and ADLB
  expect_true("AST" %in% unique(built$sdtm$LB$LBTESTCD))
  expect_setequal(unique(built$adam$ADLB$PARAMCD),
                  c("GLUC", "CREAT", "HGB", "K", "ALT", "AST"))

  # the extra SUPP qualifier on EX, including the lot number
  expect_setequal(unique(built$sdtm$SUPPEX$QNAM), c("EXADMBY", "EXLOT"))
  lot <- built$sdtm$SUPPEX |> dplyr::filter(QNAM == "EXLOT")
  expect_gt(nrow(lot), 0)
  expect_true(all(str_detect(lot$QVAL, "^SY2-\\d{4}$")))

  # this sponsor keeps the 4-point causality scale
  expect_setequal(unique(built$sdtm$AE$AEREL),
                  c("NOT RELATED", "POSSIBLY RELATED", "PROBABLY RELATED",
                    "RELATED"))

  # dosing is SYN-201
  expect_setequal(unique(built$sdtm$EX$EXTRT), c("PLACEBO", "SYN-201"))

  # deliverables written like any study's
  expect_true(file.exists(file.path(out, "sdtm", "define.xml")))
  expect_equal(length(list.files(file.path(out, "sdtm", "xpt"))), 14L)
  expect_equal(length(list.files(file.path(out, "adam", "xpt"))), 4L)
})

test_that("the SYNTH02 extract fails loudly under the SYNTH01 spec", {
  out <- file.path(tempdir(), "synth02-cross")
  on.exit(unlink(out, recursive = TRUE, force = TRUE), add = TRUE)
  ext <- file.path(out, "rave")
  suppressMessages(generate_rave_extract(out = ext, study = "SYNTH02"))

  # the arm decodes, extra test and extra qualifier are not in spec_synth01,
  # so validation must refuse the build rather than drift silently
  expect_error(suppressMessages(build_all(ext, spec = spec_synth01)))
})

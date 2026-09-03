# Study spec + mapping engine tests ------------------------------------

test_that("spec_synth01 is a valid study_spec", {
  expect_s3_class(spec_synth01, "study_spec")
  expect_output(print(spec_synth01), "3021")
  expect_equal(spec_synth01$study$STUDYID, "3021")
  expect_equal(nrow(spec_synth01$visits), 6)
  expect_equal(nrow(spec_synth01$sites), 3)
})

test_that("new_study_spec validates its tables", {
  good <- list(
    study = tibble(STUDYID = "S", PROJECT = "P", seed = 1, n = 1L),
    sites = tibble(SiteNumber = "1", siteid = "1", Site = "s",
                   SiteGroup = "g", StudySiteId = "1", COUNTRY = "ROU"),
    arms = tibble(ARMCD_DECODE = "d", ARMCD = "A", ARM = "a"),
    visits = tibble(Folder = "F", VISITNUM = 1L, VISIT = "V", EPOCH = "E",
                    TargetDays = 0L),
    codelists = tibble(ct = "X", rave_decode = "r", cdisc_term = "c"),
    supp = tibble(rdomain = "DM", idvar = NA, qnam = "Q", qlabel = "ql",
                  src = "Q", transform = "yn", qorig = "CRF", qeval = NA),
    units = tibble(testcd = "T", conv_from = "A", conv_to = "B",
                   conv_factor = 1),
    forms = tibble(form_oid = "DM", type = "event", scheduled = TRUE),
    tests = tibble(domain = "VS", field = "F", testcd = "T", test = "t",
                   cat = NA, specimen = NA),
    variables = tibble(domain = "DM", variable = "DOMAIN", crf_field = NA,
                       transform = "constant", ref = NA, value = "DM",
                       aux = NA, default = NA)
  )
  expect_s3_class(do.call(new_study_spec, good), "study_spec")

  bad_col <- good
  bad_col$study$STUDYID <- NULL
  expect_error(do.call(new_study_spec, bad_col), "missing column")

  bad_tr <- good
  bad_tr$variables$transform <- "magic"
  expect_error(do.call(new_study_spec, bad_tr), "unknown transform")

  no_ref <- good
  no_ref$variables$transform <- "derivation"
  no_ref$variables$ref <- NA
  expect_error(do.call(new_study_spec, no_ref), "without a ref")
})

test_that("ct_lookup returns named vectors and fails on unknown ct", {
  out <- ct_lookup(spec_synth01, "AESEV")
  expect_equal(out[["Severe"]], "SEVERE")
  expect_error(ct_lookup(spec_synth01, "NOPE"), "no codelist")
})

test_that("the spec codelists cover every collected decode in the extract", {
  extract_dir <- file.path(tempdir(), "edc2cdisc-spec-ct")
  on.exit(unlink(extract_dir, recursive = TRUE, force = TRUE), add = TRUE)
  suppressMessages(generate_rave_extract(out = extract_dir))

  ae <- read_clinical_view("AE", extract_dir)
  for (pair in list(c("AESEV", "AESEV_DECODE"), c("AEREL", "AEREL_DECODE"),
                    c("AEACN", "AEACN_DECODE"), c("AEOUT", "AEOUT_DECODE"))) {
    collected <- unique(stats::na.omit(ae[[pair[2]]]))
    # every collected value must be mapped; the spec may legitimately
    # cover values this small extract never shows
    expect_true(all(collected %in% names(ct_lookup(spec_synth01, pair[1]))))
  }

  ds <- read_clinical_view("DS", extract_dir)
  expect_true(all(unique(stats::na.omit(ds$DSREAS_DECODE)) %in%
                    names(ct_lookup(spec_synth01, "DSDECOD"))))
})

test_that("spec visit targets match the extract header block", {
  extract_dir <- file.path(tempdir(), "edc2cdisc-spec-visits")
  on.exit(unlink(extract_dir, recursive = TRUE, force = TRUE), add = TRUE)
  suppressMessages(generate_rave_extract(out = extract_dir))

  vs <- read_clinical_view("VS", extract_dir)
  got <- vs |>
    distinct(Folder, TargetDays) |>
    mutate(TargetDays = as.integer(TargetDays)) |>
    arrange(Folder)
  expected <- spec_synth01$visits |>
    select(Folder, TargetDays) |>
    arrange(Folder)
  expect_equal(as.data.frame(got), as.data.frame(expected))
})

test_that("map_variables drives EX columns off the spec table", {
  extract_dir <- file.path(tempdir(), "edc2cdisc-spec-engine")
  on.exit(unlink(extract_dir, recursive = TRUE, force = TRUE), add = TRUE)
  suppressMessages(generate_rave_extract(out = extract_dir))

  ex <- read_clinical_view("EX", extract_dir) |>
    filter(EXOCCUR == "1")
  out <- map_variables(ex, spec_synth01,
                       filter(spec_synth01$variables, domain == "EX"))

  expect_equal(names(out), c("STUDYID", "DOMAIN", "USUBJID", "EXTRT",
                             "EXDOSE", "EXDOSU", "EXROUTE", "EXSTDTC",
                             "EXENDTC"))
  expect_true(all(out$DOMAIN == "EX"))
  expect_equal(unique(out$STUDYID), "3021")
  expect_setequal(unique(out$EXTRT), c("SYN-101", "PLACEBO"))
  expect_setequal(unique(out$EXROUTE), "ORAL")
  expect_type(out$EXDOSE, "double")
  expect_match(unique(out$EXSTDTC), "^\\d{4}-\\d{2}-\\d{2}$")
  # USUBJID is built from the spec study id + collected subject
  expect_match(out$USUBJID[1], "^3021-\\d{3}-\\d{3}$")
})

test_that("the DS screen-failure reclassification derivation fires", {
  extract_dir <- file.path(tempdir(), "edc2cdisc-spec-ds")
  on.exit(unlink(extract_dir, recursive = TRUE, force = TRUE), add = TRUE)
  suppressMessages(generate_rave_extract(out = extract_dir))

  ds <- read_clinical_view("DS", extract_dir)
  out <- map_variables(ds, spec_synth01,
                       filter(spec_synth01$variables, domain == "DS"))

  # the two screen failures carry protocol-deviation as collected reason
  sf_rows <- out[out$DSTERM != "" &
                   grepl("screen failure", out$DSTERM, ignore.case = TRUE), ]
  expect_equal(nrow(sf_rows), 2)
  expect_true(all(sf_rows$DSDECOD == "SCREEN FAILURE"))
  expect_true(all(sf_rows$DSCAT == "PROTOCOL MILESTONE"))
  # everyone else is a plain disposition event
  expect_true(all(out$DSCAT[out$DSDECOD != "SCREEN FAILURE"] ==
                    "DISPOSITION EVENT"))
})

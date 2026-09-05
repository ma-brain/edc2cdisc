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
    study = tibble(STUDYID = "S", PROJECT = "P", seed = 1, n = 1L,
                   age_min = 18, age_max = 100),
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
    bds = tibble(domain = "ADVS", paramcd = "SYSBP", paramn = 1,
                 anrlo = 90, anrhi = 140),
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

test_that("new_study_spec validates references, not just shapes", {
  base <- list(
    study = tibble(STUDYID = "S", PROJECT = "P", seed = 1, n = 1L,
                   age_min = 18, age_max = 100),
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
    bds = tibble(domain = "ADVS", paramcd = "SYSBP", paramn = 1,
                 anrlo = 90, anrhi = 140),
    variables = tibble(
      domain    = c("DM", "DM"),
      variable  = c("DOMAIN", "ARM"),
      crf_field = c(NA, NA),
      transform = c("constant", "derivation"),
      ref       = c(NA, "study_id"),
      value     = c("DM", NA),
      aux       = NA,
      default   = NA
    )
  )

  # a derivation ref nothing in the registry answers: construction time,
  # not halfway through a build
  bad_der <- base
  bad_der$variables$ref[2] <- "no_such_derivation"
  expect_error(do.call(new_study_spec, bad_der), "unknown derivation ref")

  # an extra derivation registered at map time must be declared to the
  # constructor to pass the reference check
  local_ok <- base
  local_ok$variables$ref[2] <- "dmdy_local"
  expect_error(do.call(new_study_spec, local_ok), "unknown derivation ref")
  local_fns <- list(dmdy_local = function(data, spec, row) {
    rep(1L, nrow(data))
  })
  expect_s3_class(
    do.call(new_study_spec, c(local_ok, list(derivations = local_fns))),
    "study_spec"
  )
  spec_local <- do.call(new_study_spec, c(local_ok, list(derivations = local_fns)))
  expect_true("dmdy_local" %in% names(spec_local$derivations))
  mapped <- map_variables(
    tibble(Subject = "101-001"),
    spec_local,
    local_ok$variables[2, ]
  )
  expect_equal(mapped$ARM, 1L)

  # a decode row naming a codelist the spec does not carry
  bad_dec <- base
  bad_dec$variables$transform[2] <- "decode"
  bad_dec$variables$ref[2] <- "NOPE"
  bad_dec$variables$crf_field[2] <- "X"
  expect_error(do.call(new_study_spec, bad_dec), "no matching codelist")

  # a SUPP transform outside the vocabulary supp_transform() knows
  bad_supp <- base
  bad_supp$supp$transform <- "rot13"
  expect_error(do.call(new_study_spec, bad_supp), "unsupported SUPP transform")

  # variables for a domain no mapper can build
  bad_dom <- base
  bad_dom$variables$domain <- "ZZ"
  expect_error(do.call(new_study_spec, bad_dom), "unbuildable domain")
})

test_that("new_study_spec requires crf_field on field-dependent derivations", {
  base <- list(
    study = tibble(STUDYID = "S", PROJECT = "P", seed = 1, n = 1L,
                   age_min = 18, age_max = 100),
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
    bds = tibble(domain = "ADVS", paramcd = "SYSBP", paramn = 1,
                 anrlo = 90, anrhi = 140),
    variables = tibble(
      domain    = "DM",
      variable  = "USUBJID",
      crf_field = NA_character_,
      transform = "derivation",
      ref       = "usubjid",
      value     = NA, aux = NA, default = NA
    )
  )
  expect_error(do.call(new_study_spec, base), "missing crf_field")
  base$variables$crf_field <- "Subject"
  expect_s3_class(do.call(new_study_spec, base), "study_spec")
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

test_that("map_variables passes the spec row to derivations", {
  df <- tibble(Subject = "101-001", MARKER = c("a", "b"))
  rows <- tibble(
    domain = "ZZ", variable = "MARKER_OUT", crf_field = "MARKER",
    transform = "derivation", ref = "row_reader", value = NA, aux = NA,
    default = NA
  )
  out <- map_variables(
    df, spec_synth01, rows,
    derivations = list(row_reader = function(data, spec, row) {
      paste0(row$variable, ":", data[[row$crf_field]])
    })
  )
  # the derivation saw its row: the CRF source came from row$crf_field,
  # not from a name baked into the function
  expect_equal(out$MARKER_OUT, c("MARKER_OUT:a", "MARKER_OUT:b"))
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

# Trial design tables ----------------------------------------------------
# Optional at construction, always present on the spec: a spec without
# trial design still builds, one with trial design is checked here rather
# than halfway through a build.

td_spec <- function(...) {
  new_study_spec(
    study     = spec_synth01$study,
    sites     = spec_synth01$sites,
    arms      = spec_synth01$arms,
    visits    = spec_synth01$visits,
    codelists = spec_synth01$codelists,
    supp      = spec_synth01$supp,
    units     = spec_synth01$units,
    forms     = spec_synth01$forms,
    tests     = spec_synth01$tests,
    bds       = spec_synth01$bds,
    variables = spec_synth01$variables,
    ...
  )
}

test_that("trial design tables default to empty typed tibbles", {
  s <- td_spec()
  expect_equal(nrow(s$elements), 0)
  expect_setequal(names(s$elements),
                  c("ETCD", "ELEMENT", "TESTRL", "TEENRL", "TEDUR"))
  expect_setequal(names(s$ta),
                  c("ARMCD", "ETCD", "TAETORD", "EPOCH", "TABRANCH", "TATRANSAC"))
  expect_setequal(names(s$ie), c("IETESTCD", "IETEST", "IECAT"))
  expect_setequal(names(s$ts), c("TSPARMCD", "TSPARM", "TSVAL", "TSVALNF"))
})

test_that("a well-formed trial design spec constructs", {
  s <- td_spec(
    elements = tibble(ETCD = "A", ELEMENT = "Element A", TESTRL = "consent",
                      TEENRL = "randomised", TEDUR = "P2W"),
    ta = tibble(ARMCD = "PBO", ETCD = "A", TAETORD = 1L, EPOCH = "SCREENING",
                TABRANCH = NA_character_, TATRANSAC = "Randomised"),
    ie = tibble(IETESTCD = "INC1", IETEST = "Age 18-100", IECAT = "INCLUSION"),
    ts = tibble(TSPARMCD = "PHASE", TSPARM = "Trial Phase",
                TSVAL = "PHASE II", TSVALNF = NA_character_)
  )
  expect_equal(nrow(s$ta), 1)
})

test_that("trial design shape errors fire at construction", {
  expect_error(td_spec(ta = tibble(ARMCD = "NOPE", ETCD = "A", TAETORD = 1L,
                                   EPOCH = "SCREENING", TABRANCH = NA,
                                   TATRANSAC = NA)),
               "ta: ARMCD not in spec\\$arms")
  expect_error(td_spec(elements = tibble(ETCD = "A", ELEMENT = "Element A",
                                         TESTRL = "r", TEENRL = "r",
                                         TEDUR = "P2W"),
                       ta = tibble(ARMCD = "PBO", ETCD = "B", TAETORD = 1L,
                                   EPOCH = "SCREENING", TABRANCH = NA,
                                   TATRANSAC = NA)),
               "ta: ETCD not in spec\\$elements")
  expect_error(td_spec(elements = tibble(ETCD = c("A", "A"), ELEMENT = c("x", "y"),
                                         TESTRL = "r", TEENRL = "r",
                                         TEDUR = "P2W")),
               "duplicate ETCD")
  expect_error(td_spec(elements = tibble(ETCD = "TOOLONGCD", ELEMENT = "x",
                                         TESTRL = "r", TEENRL = "r",
                                         TEDUR = "P2W")),
               "ETCD longer than 8")
  expect_error(td_spec(elements = tibble(ETCD = "A", ELEMENT = "x",
                                         TESTRL = "r", TEENRL = "r",
                                         TEDUR = "2 weeks")),
               "TEDUR is not an ISO 8601 duration")
  expect_error(td_spec(ta = tibble(ARMCD = c("PBO", "PBO"), ETCD = c("A", "A"),
                                   TAETORD = c(1L, 1L), EPOCH = "SCREENING",
                                   TABRANCH = NA, TATRANSAC = NA)),
               "duplicated ARMCD/TAETORD")
  expect_error(td_spec(ie = tibble(IETESTCD = "INC1", IETEST = "x",
                                   IECAT = "MAYBE")),
               "ie: IECAT not INCLUSION/EXCLUSION")
  expect_error(td_spec(ts = tibble(TSPARMCD = "X", TSPARM = "x", TSVAL = "v",
                                   TSVALNF = "N/A")),
               "both TSVAL and TSVALNF")
  expect_error(td_spec(ts = tibble(TSPARMCD = "X", TSPARM = "x", TSVAL = NA,
                                   TSVALNF = NA)),
               "neither TSVAL nor TSVALNF")
})

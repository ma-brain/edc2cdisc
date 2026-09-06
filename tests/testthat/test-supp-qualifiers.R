# SUPP qualifiers: SUPPMH (MHSPECD) and SUPPVS (VSCOMTL) ---------------------
# The seeded qualifiers: one MH log line carries MHSPEC = "Autoimmune
# thyroiditis", one VS TEMP record carries VSCOMT = "Repeated after arm
# reposition" - exactly one SUPP row per study each. build_all() does not
# wire map_suppmh()/map_suppvs() yet (Task 5 does), so the fixture calls
# the mappers directly on the built parents - the intended usage.

supp_q_fixture <- function(study = c("SYNTH01", "SYNTH02")) {
  study <- match.arg(study)
  out <- file.path(tempdir(), paste0("supp-q-", study))
  dir.create(out, showWarnings = FALSE)
  ext <- file.path(out, "rave")
  if (!dir.exists(ext)) {
    suppressMessages(generate_rave_extract(out = ext, study = study))
  }
  spec  <- switch(study, SYNTH01 = spec_synth01, SYNTH02 = spec_synth02)
  forms <- suppressMessages(read_rave_extract(dir = ext))
  dm   <- map_dm(forms$DM, forms$EX, forms$DS, spec)
  refs <- subject_ref(dm)
  mh   <- map_mh(forms$MH, spec, refs)
  vs   <- map_vs(forms$VS, spec, refs)
  list(forms    = forms,
       spec     = spec,
       mh_built = mh,
       vs_built = vs,
       suppmh   = map_suppmh(forms$MH, mh, spec),
       suppvs   = map_suppvs(forms$VS, vs, spec))
}

# the one raw row the generator seeded, as (USUBJID, key) on the built parent
supp_q_seeded_mh <- function(f) {
  seeded <- f$forms$MH |>
    filter(!is.na(MHSPEC), str_squish(MHSPEC) != "")
  expect_equal(nrow(seeded), 1L, label = "seeded MHSPEC raw rows")
  f$mh_built |>
    filter(USUBJID == str_c(f$spec$study$STUDYID, seeded$Subject, sep = "-"),
           MHSPID  == as.character(seeded$recordposition))
}

supp_q_seeded_vs <- function(f) {
  seeded <- f$forms$VS |>
    filter(!is.na(VSCOMT), str_squish(VSCOMT) != "")
  expect_equal(nrow(seeded), 1L, label = "seeded VSCOMT raw rows")
  vnum <- f$spec$visits$VISITNUM[match(seeded$Folder, f$spec$visits$Folder)]
  f$vs_built |>
    filter(USUBJID == str_c(f$spec$study$STUDYID, seeded$Subject, sep = "-"),
           VISITNUM == vnum, VSTESTCD == "TEMP")
}

test_that("SUPPMH carries exactly the seeded related-history detail, linked to its MH record", {
  for (study in c("SYNTH01", "SYNTH02")) {
    f <- supp_q_fixture(study)
    parent <- supp_q_seeded_mh(f)
    expect_equal(nrow(parent), 1L,
                 label = sprintf("seeded MH parent (%s)", study))

    supp <- f$suppmh
    expect_equal(nrow(supp), 1L,
                 label = sprintf("SUPPMH rows (%s)", study))
    expect_equal(as.vector(supp$QNAM), "MHSPECD")
    expect_equal(as.vector(supp$QVAL), "Autoimmune thyroiditis")
    expect_equal(as.vector(supp$QLABEL), "If Related, Specify")
    expect_equal(as.vector(supp$IDVAR), "MHSEQ")
    expect_equal(as.vector(supp$IDVARVAL), as.character(parent$MHSEQ))
    expect_equal(supp$USUBJID, parent$USUBJID)
    expect_equal(as.vector(supp$RDOMAIN), "MH")
    expect_equal(as.vector(supp$QORIG), "CRF")
    expect_true(is.na(supp$QEVAL))
  }
})

test_that("SUPPVS carries exactly the seeded visit comment, linked to its TEMP record", {
  for (study in c("SYNTH01", "SYNTH02")) {
    f <- supp_q_fixture(study)
    parent <- supp_q_seeded_vs(f)
    expect_equal(nrow(parent), 1L,
                 label = sprintf("seeded VS TEMP parent (%s)", study))

    supp <- f$suppvs
    expect_equal(nrow(supp), 1L,
                 label = sprintf("SUPPVS rows (%s)", study))
    expect_equal(as.vector(supp$QNAM), "VSCOMTL")
    expect_equal(as.vector(supp$QVAL), "Repeated after arm reposition")
    expect_equal(as.vector(supp$QLABEL), "Comment")
    expect_equal(as.vector(supp$IDVAR), "VSSEQ")
    expect_equal(as.vector(supp$IDVARVAL), as.character(parent$VSSEQ))
    expect_equal(supp$USUBJID, parent$USUBJID)
    expect_equal(as.vector(supp$RDOMAIN), "VS")
    expect_equal(as.vector(supp$QORIG), "CRF")
    expect_true(is.na(supp$QEVAL))
  }
})

test_that("map_suppmh/map_suppvs return the SUPP interface columns in order, labelled", {
  f <- supp_q_fixture("SYNTH01")
  expected_names <- c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL",
                      "QNAM", "QLABEL", "QVAL", "QORIG", "QEVAL")
  expect_equal(names(f$suppmh), expected_names)
  expect_equal(names(f$suppvs), expected_names)
  expect_equal(
    unlist(var_label(f$suppmh))[c("IDVAR", "QNAM", "QVAL")],
    c(IDVAR = "Identifying Variable",
      QNAM  = "Qualifier Variable Name",
      QVAL  = "Data Value")
  )
})

test_that("a collected qualifier with no parent record stops the mapper loudly", {
  f <- supp_q_fixture("SYNTH01")

  # drop the seeded MH parent: the qualifier's join key no longer resolves
  orphan_mh <- supp_q_seeded_mh(f)
  mh_broken <- f$mh_built |>
    anti_join(orphan_mh, by = c("USUBJID", "MHSEQ"))
  expect_error(map_suppmh(f$forms$MH, mh_broken, f$spec),
               "did not map to an MH record")

  # drop the seeded visit's TEMP record
  orphan_vs <- supp_q_seeded_vs(f)
  vs_broken <- f$vs_built |>
    anti_join(orphan_vs, by = c("USUBJID", "VSSEQ"))
  expect_error(map_suppvs(f$forms$VS, vs_broken, f$spec),
               "did not map to a VS TEMP record")
})

# Orphan SUPPMH/SUPPVS rows tripping related-parent-orphan in validate_sdtm()
# land with Task 6's validator wiring: the .related lookup in
# R/validate-sdtm.R does not know the two domains until then.

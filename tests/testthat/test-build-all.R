# build_all output-writing and the define.xml stub ----------------------

test_that("build_all writes the full output tree", {
  out <- file.path(tempdir(), "edc2cdisc-write")
  on.exit(unlink(out, recursive = TRUE, force = TRUE), add = TRUE)
  ext <- file.path(out, "rave")
  suppressMessages(generate_rave_extract(out = ext))

  built <- build_all(ext,
                     sdtm_dir = file.path(out, "sdtm"),
                     adam_dir = file.path(out, "adam"))

  expect_setequal(list.files(file.path(out, "sdtm"), pattern = "[.]rds$"),
                  c("ae.rds", "cm.rds", "co.rds", "dm.rds", "ds.rds",
                    "ex.rds", "lb.rds", "mh.rds", "relrec.rds", "suppae.rds",
                    "suppdm.rds", "suppex.rds", "sv.rds", "vs.rds"))
  expect_setequal(list.files(file.path(out, "adam"), pattern = "[.]rds$"),
                  c("adae.rds", "adlb.rds", "adsl.rds", "advs.rds"))
  expect_equal(length(list.files(file.path(out, "sdtm", "xpt"))), 14)
  expect_equal(length(list.files(file.path(out, "adam", "xpt"))), 4)
  expect_true(file.exists(file.path(out, "sdtm", "define.xml")))
})

test_that("the define.xml stub is well-formed and complete", {
  out <- file.path(tempdir(), "edc2cdisc-define")
  on.exit(unlink(out, recursive = TRUE, force = TRUE), add = TRUE)
  ext <- file.path(out, "rave")
  suppressMessages(generate_rave_extract(out = ext))
  built <- build_all(ext)

  path <- build_define_xml(built$sdtm, spec_synth01,
                           file.path(out, "define.xml"))
  doc <- xml2::read_xml(path)
  ns <- xml2::xml_ns(doc)
  expect_equal(length(xml2::xml_find_all(doc, "//d1:ItemGroupDef", ns)), 14)
  # 10 curated value codelists, minus RELTYPE: its values are all blank on
  # record-level links, and an empty codelist is not emitted
  expect_equal(length(xml2::xml_find_all(doc, "//d1:CodeList", ns)), 9)
  # value-level metadata hooked onto VSSTRESN / LBSTRESN
  expect_equal(length(xml2::xml_find_all(doc, "//d1:ValueListDef", ns)), 2)
  # ...and hooked ONTO the parent ItemRefs: a ValueListDef that no
  # ItemRef references is metadata emitted and then orphaned
  expect_equal(length(xml2::xml_find_all(doc, "//def:ValueListRef", ns)), 2)
})

test_that("XPT output round-trips", {
  out <- file.path(tempdir(), "edc2cdisc-xpt")
  on.exit(unlink(out, recursive = TRUE, force = TRUE), add = TRUE)
  ext <- file.path(out, "rave")
  suppressMessages(generate_rave_extract(out = ext))
  built <- build_all(ext, sdtm_dir = file.path(out, "sdtm"))

  xpt <- haven::read_xpt(file.path(out, "sdtm", "xpt", "dm.xpt"))
  expect_equal(nrow(xpt), nrow(built$sdtm$DM))
  expect_setequal(names(xpt), names(built$sdtm$DM))
  expect_equal(xpt$USUBJID, built$sdtm$DM$USUBJID)
})

test_that("build_define_xml errors when the ValueList parent ItemRef is missing", {
  out <- file.path(tempdir(), "edc2cdisc-define-missing-ref")
  on.exit(unlink(out, recursive = TRUE, force = TRUE), add = TRUE)
  ext <- file.path(out, "rave")
  suppressMessages(generate_rave_extract(out = ext))
  built <- suppressMessages(build_all(ext))

  domains <- built$sdtm
  domains$VS$VSSTRESN <- NULL
  expect_error(
    build_define_xml(domains, spec_synth01, file.path(out, "define.xml")),
    "IT.VS.VSSTRESN"
  )
})

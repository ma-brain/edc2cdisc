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
                    "eg.rds", "ex.rds", "lb.rds", "mh.rds", "pe.rds",
                    "qs.rds", "relrec.rds", "se.rds", "suppae.rds",
                    "suppdm.rds", "suppex.rds", "suppmh.rds", "supppe.rds",
                    "suppvs.rds", "sv.rds", "ta.rds", "te.rds", "ti.rds",
                    "ts.rds", "tv.rds", "vs.rds"))
  expect_setequal(list.files(file.path(out, "adam"), pattern = "[.]rds$"),
                  c("adcm.rds", "adae.rds", "adlb.rds", "adsl.rds", "advs.rds"))
  expect_equal(length(list.files(file.path(out, "sdtm", "xpt"))), 26)
  expect_equal(length(list.files(file.path(out, "adam", "xpt"))), 5)

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
  expect_equal(length(xml2::xml_find_all(doc, "//d1:ItemGroupDef", ns)), 26)
  # 12 curated value codelists, minus RELTYPE: its values are all blank on
  # record-level links, and an empty codelist is not emitted
  expect_equal(length(xml2::xml_find_all(doc, "//d1:CodeList", ns)), 11)
  # the trial design domains carry keys and structures like the rest
  igd_ta <- xml2::xml_find_first(doc, "//d1:ItemGroupDef[@Name='TA']", ns)
  expect_equal(xml2::xml_attr(igd_ta, "Domain"), "TA")
  # ARMCD carries KeySequence 2 now that key_spec knows TA
  ta_ref <- xml2::xml_find_first(doc, "//d1:ItemGroupDef[@Name='TA']/d1:ItemRef[@ItemOID='IT.TA.ARMCD']", ns)
  expect_equal(xml2::xml_attr(ta_ref, "KeySequence"), "2")
  # QS is documented like the other tabulations: structure string on the
  # ItemGroupDef, and QSSEQ keyed third
  igd_qs <- xml2::xml_find_first(doc, "//d1:ItemGroupDef[@Name='QS']", ns)
  expect_equal(xml2::xml_attr(igd_qs, "def:Structure", ns = ns),
               "One record per subject per questionnaire item per visit")
  qs_ref <- xml2::xml_find_first(doc, "//d1:ItemGroupDef[@Name='QS']/d1:ItemRef[@ItemOID='IT.QS.QSSEQ']", ns)
  expect_equal(xml2::xml_attr(qs_ref, "KeySequence"), "3")
  # PE/EG/SUPPPE are documented like the other tabulations. The structure
  # attribute is namespace-prefixed: unprefixed xml_attr() returns NA here.
  igd_pe <- xml2::xml_find_first(doc, "//d1:ItemGroupDef[@Name='PE']", ns)
  expect_equal(xml2::xml_attr(igd_pe, "def:Structure", ns = ns),
               "One record per subject per body system per visit")
  igd_eg <- xml2::xml_find_first(doc, "//d1:ItemGroupDef[@Name='EG']", ns)
  expect_equal(xml2::xml_attr(igd_eg, "def:Structure", ns = ns),
               "One record per subject per ECG test per visit")
  eg_ref <- xml2::xml_find_first(doc, "//d1:ItemGroupDef[@Name='EG']/d1:ItemRef[@ItemOID='IT.EG.EGSEQ']", ns)
  expect_equal(xml2::xml_attr(eg_ref, "KeySequence"), "3")
  expect_equal(length(xml2::xml_find_all(doc, "//d1:ItemGroupDef[@Name='SUPPPE']", ns)), 1)
  # SE and the remaining SUPP qualifiers are documented like the rest: SE is
  # keyed on the subject + SESEQ, the SUPP sets on the six SUPP columns
  igd_se <- xml2::xml_find_first(doc, "//d1:ItemGroupDef[@Name='SE']", ns)
  expect_equal(xml2::xml_attr(igd_se, "def:Structure", ns = ns),
               "One record per subject per element")
  se_ref <- xml2::xml_find_first(doc, "//d1:ItemGroupDef[@Name='SE']/d1:ItemRef[@ItemOID='IT.SE.SESEQ']", ns)
  expect_equal(xml2::xml_attr(se_ref, "KeySequence"), "3")
  expect_equal(length(xml2::xml_find_all(doc, "//d1:ItemGroupDef[@Name='SUPPMH']", ns)), 1)
  expect_equal(length(xml2::xml_find_all(doc, "//d1:ItemGroupDef[@Name='SUPPVS']", ns)), 1)
  # PE's observed NORMAL/ABNORMAL values become a curated codelist
  pe_items <- xml2::xml_find_all(doc, "//d1:CodeList[@OID='CL.PEORRES']/d1:EnumeratedItem", ns)
  expect_equal(xml2::xml_attr(pe_items, "CodedValue"), c("ABNORMAL", "NORMAL"))
  # value-level metadata hooked onto VSSTRESN / LBSTRESN / EGSTRESN: only
  # VS, LB and EG expose a --STRESN column - PE reports character PEORRES,
  # and QS's numeric result never leaves the mapper
  expect_equal(length(xml2::xml_find_all(doc, "//d1:ValueListDef", ns)), 3)
  # ...and hooked ONTO the parent ItemRefs: a ValueListDef that no
  # ItemRef references is metadata emitted and then orphaned
  expect_equal(length(xml2::xml_find_all(doc, "//def:ValueListRef", ns)), 3)
  # every EG value-level description carries the msec unit: map_eg() binds
  # answered rows before NOT DONE, and the VLM's first-occurrence-per-test
  # EGSTRESU depends on that ordering
  eg_vlm_desc <- xml2::xml_text(xml2::xml_find_all(
    doc, "//d1:ItemDef[starts-with(@OID, 'IT.EG.EGSTRESN.')]/d1:Description/d1:TranslatedText",
    ns
  ))
  expect_length(eg_vlm_desc, 5)
  expect_true(all(str_detect(eg_vlm_desc, "\\(msec\\)$")))
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

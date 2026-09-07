# Define-XML 2.0: the schema is the oracle ------------------------------------
# The canonical CDISC 2.0 schema set ships under inst/schema/, and the built
# define.xml is validated against it offline (xml2::xml_validate / libxml2).

describe_schema_errors <- function(res) {
  errs <- attr(res, "errors")
  errs <- errs[!grepl("^Element '\\{http://www.w3.org/2001/XMLSchema\\}import'",
                      errs)] # schema-internal duplicate-import notice
  unique(sub("\\s+", " ", trimws(errs)))
}

test_that("the vendored 2.0 schema set loads offline", {
  expect_true(file.exists(edc2cdisc:::.define_schema_path()))
  expect_s3_class(xml2::read_xml(edc2cdisc:::.define_schema_path()),
                  "xml_document")
})

test_that("the built define.xml validates against Define-XML 2.0", {
  built <- built_suite()
  path <- build_define_xml(built$sdtm, spec_synth01,
                           file.path(tempdir(), "define-2.0.xml"))
  doc <- xml2::read_xml(path)
  res <- xml2::xml_validate(doc, xml2::read_xml(edc2cdisc:::.define_schema_path()))
  expect_true(res, paste(collapse = "\n", describe_schema_errors(res)))
})

test_that("the document carries the 2.0 submission contract", {
  built <- built_suite()
  doc <- xml2::read_xml(build_define_xml(built$sdtm, spec_synth01,
                                         file.path(tempdir(), "define-c.xml")))
  ns <- xml2::xml_ns(doc)
  mdv <- xml2::xml_find_first(doc, "//d1:MetaDataVersion", ns)
  expect_equal(xml2::xml_attr(mdv, "def:StandardName", ns = ns), "SDTMIG")
  expect_equal(xml2::xml_attr(mdv, "def:StandardVersion", ns = ns), "3.1.2")
  expect_equal(xml2::xml_attr(mdv, "def:DefineVersion", ns = ns), "2.0.0")
  # every dataset is classed, and the class is the SDTM class
  classes <- xml2::xml_attr(xml2::xml_find_all(doc, "//d1:ItemGroupDef", ns),
                            "def:Class", ns = ns)
  expect_true(all(!is.na(classes)))
  expect_equal(unique(classes[xml2::xml_attr(
    xml2::xml_find_all(doc, "//d1:ItemGroupDef", ns), "Name") == "AE"]),
    "EVENTS")
  expect_equal(unique(classes[xml2::xml_attr(
    xml2::xml_find_all(doc, "//d1:ItemGroupDef", ns), "Name") == "VS"]),
    "FINDINGS")
  # -DTC columns collect a time part where the data carries one and are
  # dates otherwise; VSSTRESN is a float with observed SignificantDigits
  dtc <- xml2::xml_find_first(doc, "//d1:ItemDef[@Name='VSDTC']", ns)
  expect_equal(xml2::xml_attr(dtc, "DataType"), "datetime")
  mh_dtc <- xml2::xml_find_first(doc, "//d1:ItemDef[@Name='MHSTDTC']", ns)
  expect_equal(xml2::xml_attr(mh_dtc, "DataType"), "date")
  expect_equal(xml2::xml_attr(dtc, "Length"), NA_character_)
  vsn <- xml2::xml_find_first(doc, "//d1:ItemDef[@OID='IT.VS.VSSTRESN']", ns)
  expect_equal(xml2::xml_attr(vsn, "DataType"), "float")
  expect_true(!is.na(xml2::xml_attr(vsn, "SignificantDigits")))
  # the value list hangs off the parameter variable's ItemDef - that is
  # where the 2.0 schema puts it, not on the ItemGroupDef's ItemRef
  expect_equal(length(xml2::xml_find_all(
    doc, "//d1:ItemDef[@OID='IT.VS.VSSTRESN']/def:ValueListRef", ns)), 1)
  expect_equal(length(xml2::xml_find_all(
    doc, "//d1:ItemRef[@ItemOID='IT.VS.VSSTRESN']/def:ValueListRef", ns)), 0)
  # the archive leaf follows the ItemRefs inside its ItemGroupDef
  igd_vs <- xml2::xml_find_first(doc, "//d1:ItemGroupDef[@Name='VS']", ns)
  kids <- xml2::xml_name(xml2::xml_children(igd_vs))
  expect_equal(tail(kids, 1), "leaf")
})

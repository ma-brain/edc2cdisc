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

test_that("spec codelists decode and carry their NCI codelist aliases", {
  built <- built_suite()
  doc <- xml2::read_xml(build_define_xml(built$sdtm, spec_synth01,
                                         file.path(tempdir(), "define-ct.xml")))
  ns <- xml2::xml_ns(doc)
  aeout <- xml2::xml_find_first(doc, "//d1:CodeList[@OID='CL.AEOUT']", ns)
  expect_equal(xml2::xml_attr(aeout, "Name"), "AEOUT")
  alias <- xml2::xml_find_first(aeout, "d1:Alias", ns)
  expect_equal(xml2::xml_attr(alias, "Context"), "ncim:CodeList")
  expect_equal(xml2::xml_attr(alias, "Name"), "C66768")
  items <- xml2::xml_find_all(aeout, "d1:CodeListItem", ns)
  aeout_spec <- spec_synth01$codelists[spec_synth01$codelists$ct == "AEOUT", ]
  observed <- unique(built$sdtm$AE$AEOUT)
  kept <- aeout_spec[aeout_spec$cdisc_term %in% observed, ]
  expect_equal(xml2::xml_attr(items, "CodedValue"), kept$cdisc_term)
  expect_equal(xml2::xml_text(xml2::xml_find_all(items, "d1:Decode/d1:TranslatedText",
                                                 ns)), kept$rave_decode)

  sex <- xml2::xml_find_first(doc, "//d1:CodeList[@OID='CL.SEX']", ns)
  expect_equal(xml2::xml_attr(xml2::xml_find_first(sex, "d1:Alias", ns),
                              "Name"), "C66731")

  # a curated observed-value codelist with no spec backing stays
  # enumerated and unaliased
  peorres <- xml2::xml_find_first(doc, "//d1:CodeList[@OID='CL.PEORRES']", ns)
  expect_equal(length(xml2::xml_find_all(peorres, "d1:CodeListItem", ns)), 0)
  expect_equal(length(xml2::xml_find_all(peorres, "d1:Alias", ns)), 0)
  expect_true(length(xml2::xml_find_all(peorres, "d1:EnumeratedItem", ns)) > 0)
})

test_that("value-level ItemDefs type their own code's data", {
  built <- built_suite()
  doc <- xml2::read_xml(build_define_xml(built$sdtm, spec_synth01,
                                         file.path(tempdir(), "define-vlm.xml")))
  ns <- xml2::xml_ns(doc)
  vs <- built$sdtm$VS
  for (cd in c("HEIGHT", "TEMP")) {
    oid <- sprintf("//d1:ItemDef[@OID='IT.VS.VSSTRESN.%s']", cd)
    it <- xml2::xml_find_first(doc, oid, ns)
    x <- vs$VSSTRESN[vs$VSTESTCD == cd & !is.na(vs$VSSTRESN)]
    expect_equal(xml2::xml_attr(it, "Length"),
                 as.character(max(nchar(as.character(x)))),
                 label = paste("length of", cd))
    dec <- max(nchar(sub("^[^.]*\\.?", "", as.character(x))))
    expect_equal(xml2::xml_attr(it, "SignificantDigits"),
                 if (dec > 0) as.character(dec) else NA_character_,
                 label = paste("sig digits of", cd))
  }
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
  igds <- xml2::xml_find_all(doc, "//d1:ItemGroupDef", ns)
  igd_names <- xml2::xml_attr(igds, "Name")
  classes <- xml2::xml_attr(igds, "def:Class", ns = ns)
  expect_true(all(!is.na(classes)))
  expect_equal(unique(classes[igd_names == "AE"]), "EVENTS")
  expect_equal(unique(classes[igd_names == "VS"]), "FINDINGS")
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
  vlr <- "//d1:ItemDef[@OID='IT.VS.VSSTRESN']/def:ValueListRef"
  expect_equal(length(xml2::xml_find_all(doc, vlr, ns)), 1)
  expect_equal(length(xml2::xml_find_all(doc, gsub("ItemDef", "ItemRef", vlr),
                                         ns)), 0)
  # derived variables carry MethodOIDs that the emitted MethodDefs resolve,
  # and the SUPP family shares one CommentDef explaining the linkage
  expect_true(length(xml2::xml_find_all(doc, "//d1:MethodDef", ns)) > 20)
  lb_ref <- xml2::xml_find_first(doc, "//d1:ItemRef[@ItemOID='IT.LB.LBNRIND']", ns)
  expect_equal(xml2::xml_attr(lb_ref, "MethodOID"), "MT.LB.LBNRIND")
  expect_equal(length(xml2::xml_find_all(doc, "//def:CommentDef", ns)), 3)
  suppdm <- xml2::xml_find_first(doc, "//d1:ItemGroupDef[@Name='SUPPDM']", ns)
  expect_equal(xml2::xml_attr(suppdm, "def:CommentOID", ns = ns), "COM.SP")
  # the archive leaf follows the ItemRefs inside its ItemGroupDef
  igd_vs <- xml2::xml_find_first(doc, "//d1:ItemGroupDef[@Name='VS']", ns)
  kids <- xml2::xml_name(xml2::xml_children(igd_vs))
  expect_equal(tail(kids, 1), "leaf")
})

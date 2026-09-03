# ============================================================================
# Title:   define.xml guard
# Purpose: The define stub must stay in step with the data it describes:
#          well-formed XML, one ItemGroupDef per SDTM domain, leaves that
#          point at real XPT files, variable counts matching the built
#          datasets, and the findings value-level metadata present.
# ============================================================================

ns <- c(odm = "http://www.cdisc.org/ns/odm/v1.3",
        def = "http://www.cdisc.org/ns/def/v2.0",
        xlink = "http://www.w3.org/1999/xlink")

test_that("define.xml is present and well-formed", {
  expect_true(file.exists(file.path(paths$sdtm, "define.xml")))
  expect_s3_class(read_xml(file.path(paths$sdtm, "define.xml")), "xml_document")
})

doc <- read_xml(file.path(paths$sdtm, "define.xml"))

test_that("define.xml describes every SDTM domain", {
  igds <- xml_find_all(doc, "//odm:ItemGroupDef", ns)
  expect_setequal(xml_attr(igds, "Name"),
                  c("DM", "EX", "VS", "AE", "CM", "DS", "SV", "LB", "MH",
                    "SUPPDM", "SUPPAE", "SUPPEX", "CO", "RELREC"))
})

test_that("every domain leaf points at a real XPT file", {
  leaves <- xml_find_all(doc, "//odm:ItemGroupDef/def:leaf", ns)
  # the href parses as a plain attribute on some xml2 builds; xml_attrs
  # sidesteps the namespace lookup either way
  hrefs <- map_chr(leaves, \(l) xml_attrs(l)[["href"]])
  expect_gt(length(hrefs), 0)
  expect_true(all(file.exists(file.path(paths$sdtm, hrefs))))
})

test_that("ItemDef counts match the built datasets", {
  for (d in c("DM", "AE", "VS", "LB", "MH", "RELREC")) {
    df <- readRDS(file.path(paths$sdtm, str_c(str_to_lower(d), ".rds")))
    n <- xml_find_all(
      doc, str_c("//odm:ItemGroupDef[@Name='", d, "']/odm:ItemRef"), ns
    ) |> length()
    expect_equal(n, ncol(df))
  }
})

test_that("findings value-level metadata is present and complete", {
  vs <- readRDS(file.path(paths$sdtm, "vs.rds"))
  lb <- readRDS(file.path(paths$sdtm, "lb.rds"))
  vl_vs <- xml_find_first(doc, "//odm:ValueListDef[@OID='VL.VS.VSSTRESN']", ns)
  vl_lb <- xml_find_first(doc, "//odm:ValueListDef[@OID='VL.LB.LBSTRESN']", ns)
  expect_equal(xml_length(vl_vs), n_distinct(vs$VSTESTCD))
  expect_equal(xml_length(vl_lb), n_distinct(lb$LBTESTCD))

  # the LB analyte with sex-specific ranges carries them in its description
  hgb <- xml_text(xml_find_first(
    doc, "//odm:ItemDef[@OID='IT.LB.LBSTRESN.HGB']/odm:Description/odm:TranslatedText",
    ns))
  expect_match(hgb, "Hemoglobin \\(g/L\\)")
  expect_match(hgb, "120 to 155")
  expect_match(hgb, "130 to 170")
})

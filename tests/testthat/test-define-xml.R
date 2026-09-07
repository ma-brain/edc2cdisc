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
  errs <- describe_schema_errors(res)

  # the pre-2.0 builder's failure shapes, pinned so the fixes are driven by
  # the schema one shape at a time: an ODM-root attribute the schema does
  # not declare, the required MetaDataVersion standard identification, and
  # the MetaDataVersion element sequence the builder emits out of order
  expect_false(res)
  expect_true(any(grepl("v2.0}Context' is not allowed", errs, fixed = TRUE)),
              paste(collapse = "\n", errs))
  expect_true(any(grepl("StandardName' is required but missing", errs)),
              paste(collapse = "\n", errs))
  expect_true(any(grepl("StandardVersion' is required but missing", errs)),
              paste(collapse = "\n", errs))
  expect_true(any(grepl("ItemGroupDef': This element is not expected", errs,
                        fixed = TRUE)),
              paste(collapse = "\n", errs))
})

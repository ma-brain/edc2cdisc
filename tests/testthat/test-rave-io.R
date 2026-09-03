# Reader tests -----------------------------------------------------------
# Self-contained: generate a fresh extract into a scratch directory, then
# exercise the defensive-reader contracts against it.

extract_dir <- file.path(tempdir(), "edc2cdisc-io-test")
on.exit(unlink(extract_dir, recursive = TRUE, force = TRUE), add = TRUE)
suppressMessages(generate_rave_extract(out = extract_dir))

test_that("read_clinical_view types the header bookkeeping columns", {
  vs <- read_clinical_view("VS", extract_dir)
  expect_s3_class(vs, "tbl_df")
  expect_setequal(rave_header_cols, intersect(rave_header_cols, names(vs)))
  expect_type(vs$RecordActive, "integer")
  expect_type(vs$recordposition, "integer")
  expect_s3_class(vs$SaveTs, c("POSIXct", "POSIXt"))
  expect_equal(unique(vs$FORMOID), "VS")
  expect_gt(nrow(vs), 0)
})

test_that("read_clinical_view drops the soft-deleted record when active_only", {
  vs_all    <- read_clinical_view("VS", extract_dir, active_only = FALSE)
  vs_active <- read_clinical_view("VS", extract_dir, active_only = TRUE)
  expect_gt(nrow(vs_all) - nrow(vs_active), 0)   # the generator ships one
  expect_true(all(vs_active$RecordActive == 1))
})

test_that("read_clinical_view fails on a missing form", {
  expect_error(read_clinical_view("ZZ", extract_dir),
               "Clinical view not found")
})

test_that("read_clinical_view refuses a truncated extract", {
  bad_dir <- file.path(tempdir(), "edc2cdisc-io-truncated")
  on.exit(unlink(bad_dir, recursive = TRUE, force = TRUE), add = TRUE)
  dir.create(bad_dir, recursive = TRUE)
  # copy DM without its EOF marker row
  lines <- readLines(file.path(extract_dir, "DM.csv"))
  writeLines(lines[str_detect(lines, "^\"?EOF\"?", negate = TRUE)],
             file.path(bad_dir, "DM.csv"))
  expect_error(read_clinical_view("DM", bad_dir), "No EOF marker")
})

test_that("labels arrive from the CV metadata", {
  dm <- read_clinical_view("DM", extract_dir)
  meta <- read_cv_metadata(extract_dir)
  dm <- label_from_cv_metadata(dm, "DM", meta)
  expect_equal(attr(dm$BRTHDAT, "label"), "Date of Birth")
})

test_that("col_or_na fills absent columns", {
  dm <- read_clinical_view("DM", extract_dir)
  expect_equal(col_or_na(dm, "SEX"), dm$SEX)
  filled <- col_or_na(dm, "NO_SUCH_FIELD")
  expect_length(filled, nrow(dm))
  expect_true(all(is.na(filled)))
})

test_that("read_codelists returns the long reference", {
  cl <- read_codelists(extract_dir)
  expect_setequal(names(cl), c("CodeListOID", "CodedValue", "Decode", "Ordinal"))
  expect_true("SEX" %in% cl$CodeListOID)
  expect_equal(cl$Decode[cl$CodeListOID == "SEX" & cl$CodedValue == "1"], "Male")
})

test_that("read_rave_extract loads every form labelled", {
  forms <- suppressMessages(read_rave_extract(dir = extract_dir))
  expect_setequal(names(forms),
                  c("DM", "VS", "LB", "AE", "CM", "EX", "DS", "MH"))
  for (f in forms) expect_gt(nrow(f), 0)
  expect_equal(attr(forms$AE$AETERM, "label"), "Adverse Event Verbatim Term")
})

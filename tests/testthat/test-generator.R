# Generator reproducibility guard ---------------------------------------
#
# The package's core claim: the extract is stochastic but seeded - with the
# committed seed and n, the bytes are identical. The expected file digests
# were taken from the script-based generator's output (tests/testthat/
# fixtures/generator-digests.rds) and are the frozen ground truth; a change
# here means the synthetic source data changed, which moves every downstream
# regression baseline and must be a deliberate, reviewed act.
#
# The fixture keeps the package tests self-contained: they do not depend on
# the (gitignored) data/rave extract being present.

test_that("generate_rave_extract reproduces the frozen extract byte-for-byte", {
  scratch <- file.path(tempdir(), "edc2cdisc-generator-test")
  on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)

  suppressMessages(generate_rave_extract(out = scratch))
  generated <- sort(list.files(scratch))
  expect_gt(length(generated), 0)

  fixture <- readRDS(testthat::test_path("fixtures",
                                         "generator-digests.rds"))
  expect_setequal(generated, names(fixture))

  for (f in generated) {
    got <- unname(tools::md5sum(file.path(scratch, f)))
    expect_identical(got, fixture[[f]],
                     label = sprintf("digest of %s", f))
  }
})

test_that("different seeds give different extracts", {
  a <- file.path(tempdir(), "edc2cdisc-seed-a")
  b <- file.path(tempdir(), "edc2cdisc-seed-b")
  on.exit(unlink(c(a, b), recursive = TRUE, force = TRUE), add = TRUE)

  suppressMessages(generate_rave_extract(out = a, seed = 1))
  suppressMessages(generate_rave_extract(out = b, seed = 2))
  expect_false(identical(
    readLines(file.path(a, "DM.csv"), warn = FALSE),
    readLines(file.path(b, "DM.csv"), warn = FALSE)
  ))
})

test_that("n controls the subject count on the DM form", {
  a <- file.path(tempdir(), "edc2cdisc-n30")
  on.exit(unlink(a, recursive = TRUE, force = TRUE), add = TRUE)

  suppressMessages(generate_rave_extract(out = a, seed = 1, n = 30))
  # the trailing unquoted EOF marker row is short by design -> parse warning
  dm <- suppressWarnings(readr::read_csv(
    file.path(a, "DM.csv"),
    col_types = readr::cols(.default = readr::col_character())
  ))
  dm <- dm[!is.na(dm$Subject) & dm$Subject != "EOF", ]  # drop EOF marker row
  expect_equal(nrow(dm), 30)
})

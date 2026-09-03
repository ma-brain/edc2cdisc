# Regression harness -----------------------------------------------------
# The package's core guarantee: the whole pipeline - seeded extract, reader,
# spec engine, mappers - reproduces the committed script-based reference
# outputs exactly (values, types, order and variable labels). These .rds
# files were frozen from the last script-based run; a difference means
# something drifted and must be a deliberate, reviewed reference update.

# locate the committed references from wherever the tests run
reference_dir <- local({
  start <- normalizePath(testthat::test_path(".."), mustWork = TRUE)
  p <- start
  repeat {
    if (dir.exists(file.path(p, "reference", "sdtm"))) break
    parent <- dirname(p)
    if (parent == p) {
      stop("tests: reference directory not found from ", start, call. = FALSE)
    }
    p <- parent
  }
  file.path(p, "reference")
})

test_that("the package pipeline reproduces every reference dataset", {
  out <- file.path(tempdir(), "edc2cdisc-regression")
  on.exit(unlink(out, recursive = TRUE, force = TRUE), add = TRUE)
  ext <- file.path(out, "rave")
  suppressMessages(generate_rave_extract(out = ext))

  built <- build_all(ext)

  for (layer in c("sdtm", "adam")) {
    refs <- list.files(file.path(reference_dir, layer), pattern = "[.]rds$")
    expect_gt(length(refs), 0)
    for (f in refs) {
      domain <- toupper(tools::file_path_sans_ext(f))
      got <- if (layer == "sdtm") built$sdtm[[domain]] else built$adam[[domain]]
      ref <- readRDS(file.path(reference_dir, layer, f))
      info <- waldo::compare(got, ref, tolerance = 1e-8)
      expect_length(info, 0)
      expect_gt(nrow(got), 0)
    }
  }
})

test_that("diffdf agrees for the subject-level anchor domain", {
  out <- file.path(tempdir(), "edc2cdisc-diffdf")
  on.exit(unlink(out, recursive = TRUE, force = TRUE), add = TRUE)
  ext <- file.path(out, "rave")
  suppressMessages(generate_rave_extract(out = ext))
  built <- build_all(ext)

  ref <- readRDS(file.path(reference_dir, "sdtm", "dm.rds"))
  d <- diffdf::diffdf(built$sdtm$DM, ref, suppress_warnings = TRUE)
  expect_false(diffdf::diffdf_has_issues(d))
})

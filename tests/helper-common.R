# ============================================================================
# Title:   Test helpers
# Purpose: Sourced by testthat before the test files. Locates the project
#          root (00_setup.R) from wherever test_dir() was launched, loads
#          the study setup, and defines the reference-comparison expectation.
# ============================================================================

.root <- local({
  p <- normalizePath(getwd(), mustWork = TRUE)
  repeat {
    if (file.exists(file.path(p, "00_setup.R"))) break
    parent <- dirname(p)
    if (parent == p) {
      stop("tests: project root (00_setup.R) not found from ", getwd(),
           call. = FALSE)
    }
    p <- parent
  }
  p
})

source(file.path(.root, "00_setup.R"))

# define.xml guard needs XPath access
suppressPackageStartupMessages(library(xml2))

#' Expect a dataset to equal its committed reference
#'
#' The core regression guard: every domain build must reproduce the stored
#' baseline byte-for-byte in value (floating tolerance 1e-8) - including
#' variable labels, which expect_equal compares as attributes.
expect_matches_reference <- function(data, layer, domain) {
  ref_file <- file.path(.root, "tests", "reference", layer,
                        paste0(tolower(domain), ".rds"))
  if (!file.exists(ref_file)) {
    fail(sprintf("reference file missing: %s", ref_file))
    return(invisible(NULL))
  }
  ref <- readRDS(ref_file)
  expect_equal(data, ref, tolerance = 1e-8,
               label = sprintf("%s/%s", layer, tolower(domain)),
               expected.label = "reference")
  invisible(NULL)
}

#' Expect a dataset to match its committed reference exactly (no tolerance)
expect_identical_to_reference <- function(data, layer, domain) {
  ref_file <- file.path(.root, "tests", "reference", layer,
                        paste0(tolower(domain), ".rds"))
  if (!file.exists(ref_file)) {
    fail(sprintf("reference file missing: %s", ref_file))
    return(invisible(NULL))
  }
  expect_identical(data, readRDS(ref_file),
                   label = sprintf("%s/%s", layer, tolower(domain)),
                   expected.label = "reference")
  invisible(NULL)
}

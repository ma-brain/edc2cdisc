# Unit tests: unit-conversion and formatting helpers -------------------

test_that("to_standard converts the conventional units", {
  expect_equal(to_standard("98.6", "F"), 37.0)
  expect_equal(to_standard("100", "lb"), 45.4)
  expect_equal(to_standard("70", "in"), 177.8)
  expect_equal(to_standard("120", "mmHg"), 120)      # identity
  expect_equal(to_standard(NA, "kg"), NA_real_)
})

test_that("standard_unit names the target unit", {
  expect_equal(standard_unit("F"), "C")
  expect_equal(standard_unit("LB"), "kg")
  expect_equal(standard_unit("IN"), "cm")
  expect_equal(standard_unit("mmol/L"), "mmol/L")    # identity
})

test_that("resolve_standard prefers the EDC value, falls back locally", {
  got <- resolve_standard("36.8", "C", "98.6", "F")
  expect_equal(got$stresn, 36.8)
  expect_equal(got$stresu, "C")

  got <- resolve_standard("", "", "98.6", "F")
  expect_equal(got$stresn, 37.0)
  expect_equal(got$stresu, "C")
})

test_that("yn recodes coded yes/no and NAs the rest", {
  expect_equal(yn(c("1", "0", "", NA)), c("Y", "N", NA, NA))
})

test_that("clean_verbatim trims and upper-cases, keeping inner spacing", {
  expect_equal(clean_verbatim("  head ache  "), "HEAD ACHE")
  expect_equal(clean_verbatim(NA), NA_character_)
})

test_that("apply_labels sets labels and ignores unknown variables", {
  df <- tibble::tibble(A = 1, B = 2)
  out <- apply_labels(df, c(A = "Alpha", Z = "Missing"))
  expect_equal(attr(out$A, "label"), "Alpha")
  expect_null(attr(out$B, "label"))
})

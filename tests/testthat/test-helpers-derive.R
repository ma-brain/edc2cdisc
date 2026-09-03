# Unit tests: derivation helpers ---------------------------------------

test_that("compute_age is whole years, NA-safe", {
  expect_equal(compute_age(as.Date("1990-06-15"), as.Date("2024-06-14")), 33)
  expect_equal(compute_age(as.Date("1990-06-15"), as.Date("2024-06-15")), 34)
  expect_equal(compute_age(as.Date(NA), as.Date("2024-01-01")), NA_integer_)
  expect_equal(compute_age(as.Date("1990-06-15"), as.Date(NA)), NA_integer_)
})

test_that("derive_dy_d has no day 0", {
  ref <- as.Date("2024-01-10")
  expect_equal(derive_dy_d(as.Date("2024-01-10"), ref), 1L)   # same day
  expect_equal(derive_dy_d(as.Date("2024-01-09"), ref), -1L)  # day before
  expect_equal(derive_dy_d(as.Date("2024-01-11"), ref), 2L)
  expect_equal(derive_dy_d(as.Date(NA), ref), NA_integer_)
})

test_that("derive_dy wraps dtc strings", {
  expect_equal(derive_dy("2024-01-10", "2024-01-10"), 1L)
  expect_equal(derive_dy("2024-01", "2024-01-10"), NA_integer_)  # partial
})

test_that("derive_seq numbers within USUBJID after ordering", {
  df <- tibble::tibble(
    USUBJID = c("S2", "S1", "S2", "S1"),
    DT      = c("2024-01-03", "2024-01-02", "2024-01-01", "2024-01-05")
  )
  out <- derive_seq(df, "XXSEQ", DT)
  expect_equal(out$XXSEQ, c(1L, 2L, 1L, 2L))
  expect_equal(out$USUBJID, c("S1", "S1", "S2", "S2"))
})

test_that("check_ct maps values and fails loudly on unmapped ones", {
  lookup <- c("Mild" = "MILD", "Moderate" = "MODERATE")
  expect_equal(check_ct(c("Mild", "Moderate", NA), lookup, "XXSEV"),
               c("MILD", "MODERATE", NA))
  expect_error(check_ct("Severe", lookup, "XXSEV"),
               "XXSEV: unmapped codelist value\\(s\\): Severe")
})

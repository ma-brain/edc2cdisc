# Unit tests: date / partial-date helpers ------------------------------

test_that("rave_dtc builds full and reduced-precision ISO dates", {
  expect_equal(rave_dtc("2024", "01", "15"), "2024-01-15")
  expect_equal(rave_dtc("2024", "01", NA), "2024-01")      # UNK day
  expect_equal(rave_dtc("2024", NA, NA), "2024")           # UNK day+month
  expect_equal(rave_dtc(NA, "01", "15"), NA_character_)    # no year, no date
  expect_equal(rave_dtc(2024, 1, 15), "2024-01-15")        # numeric components
})

test_that("rave_dtc pads short components", {
  expect_equal(rave_dtc("24", "1", "5"), "0024-01-05")
})

test_that("rave_dtc attaches time only to complete dates", {
  expect_equal(rave_dtc("2024", "01", "15", time = "09:30"),
               "2024-01-15T09:30")
  expect_equal(rave_dtc("2024", "01", NA, time = "09:30"), "2024-01")
  expect_equal(rave_dtc("2024", "01", "15", time = NA), "2024-01-15")
  expect_equal(rave_dtc("2024", "01", "15", time = "09:30:45"),
               "2024-01-15T09:30")                          # HH:MM only
})

test_that("dtc_date keeps complete dates only", {
  expect_equal(dtc_date("2024-01-15"), as.Date("2024-01-15"))
  expect_equal(dtc_date("2024-01-15T09:30"), as.Date("2024-01-15"))
  expect_equal(dtc_date(c("2024-01", "2024", NA)),
               as.Date(c(NA, NA, NA)))
})

test_that("impute_dtc flags what it filled", {
  x <- c("2024-01-15", "2024-01", "2024", NA)
  imp <- impute_dtc(x)
  expect_equal(imp$date, as.Date(c("2024-01-15", "2024-01-01",
                                   "2024-01-01", NA)))
  expect_equal(imp$flag, c("", "D", "M", NA))
})

test_that("min_dtc / max_dtc return NA on all-NA input", {
  expect_equal(min_dtc(c(NA, NA)), NA_character_)
  expect_equal(max_dtc(c(NA, NA)), NA_character_)
  expect_equal(min_dtc(c("2024-01-15", "2023-12-31", NA)), "2023-12-31")
  expect_equal(max_dtc(c("2024-01-15", "2023-12-31", NA)), "2024-01-15")
})

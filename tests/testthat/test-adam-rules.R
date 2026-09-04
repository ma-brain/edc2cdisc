# Rule-level tests for the shared ADaM derivation rules (R/adam-rules.R).
# validate_adam() recomputes with these same functions, so it can only ever
# detect post-hoc corruption of the built data - a wrong rule is invisible
# to it by construction. These tests are the only thing pinning the rules
# themselves, against hand-built inputs the seeded generator never produces.

test_that("trtemfl: a missing TRTEDT leaves the window open at the end", {
  trtsdt <- as.Date("2024-01-10")
  # dosing ongoing at the data cut: everything on/after first dose is
  # treatment-emergent - `astdt <= trtedt` must not NULL the rule out
  expect_equal(.rule_trtemfl(as.Date("2024-02-01"), trtsdt, as.Date(NA)), "Y")
  expect_equal(.rule_trtemfl(as.Date("2024-01-10"), trtsdt, as.Date(NA)), "Y")
  # onset before first dose stays unflagged either way
  expect_equal(.rule_trtemfl(as.Date("2024-01-09"), trtsdt, as.Date(NA)), "")
})

test_that("trtemfl: the window, and the unclassifiable cases", {
  trtsdt <- as.Date("2024-01-10")
  trtedt <- as.Date("2024-01-31")
  expect_equal(.rule_trtemfl(as.Date("2024-01-31"), trtsdt, trtedt), "Y")
  expect_equal(.rule_trtemfl(as.Date("2024-02-01"), trtsdt, trtedt), "")
  # no classifiable onset, or an undosed subject
  expect_equal(.rule_trtemfl(as.Date(NA), trtsdt, trtedt), "")
  expect_equal(.rule_trtemfl(as.Date("2024-01-15"), as.Date(NA), trtedt), "")
})

test_that("trtdurd: inclusive of both ends, NA-safe", {
  expect_equal(.rule_trtdurd(as.Date("2024-01-10"), as.Date("2024-01-10")), 1L)
  expect_equal(.rule_trtdurd(as.Date("2024-01-10"), as.Date("2024-01-31")), 22L)
  expect_equal(.rule_trtdurd(as.Date("2024-01-10"), as.Date(NA)), NA_integer_)
  expect_equal(.rule_trtdurd(as.Date(NA), as.Date("2024-01-31")), NA_integer_)
})

test_that("chg: baseline row and missing baseline stay missing", {
  expect_equal(.rule_chg(5, 10, "Y"), NA_real_)
  expect_equal(.rule_chg(5, NA, NA), NA_real_)
  expect_equal(.rule_chg(12, 10, NA), 2)
})

test_that("pchg: defined only where CHG is", {
  expect_equal(.rule_pchg(NA_real_, 10), NA_real_)
  expect_equal(.rule_pchg(2, 10), 20)
  expect_equal(.rule_pchg(2, NA_real_), NA_real_)
})

test_that("anrind: needs the value and both bounds", {
  expect_equal(.rule_anrind(36, 35, 37.5), "NORMAL")
  expect_equal(.rule_anrind(30, 35, 37.5), "LOW")
  expect_equal(.rule_anrind(40, 35, 37.5), "HIGH")
  # a missing bound means "cannot classify" - never a default NORMAL,
  # which is how a value far above a missing upper limit used to read
  expect_equal(.rule_anrind(40, 35, NA), NA_character_)
  expect_equal(.rule_anrind(30, NA, 37.5), NA_character_)
  expect_equal(.rule_anrind(NA_real_, 35, 37.5), NA_character_)
})

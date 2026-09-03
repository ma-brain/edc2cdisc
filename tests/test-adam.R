# ============================================================================
# Title:   ADaM output guard
# Purpose: Every ADaM dataset in data/adam/ must reproduce the committed
#          reference, and the analysis keys must stay unique. (The deep
#          derivation checks live in 39_validate_adam.R - this file freezes
#          the outputs, it does not re-audit them.)
# ============================================================================

for (d in c("ADSL", "ADAE", "ADVS", "ADLB")) {
  test_that(sprintf("ADaM %s matches the committed reference", d), {
    df <- readRDS(file.path(paths$adam, paste0(tolower(d), ".rds")))
    expect_gt(nrow(df), 0)
    expect_matches_reference(df, "adam", d)
  })
}

test_that("ADSL keeps one row per subject", {
  adsl <- readRDS(file.path(paths$adam, "adsl.rds"))
  expect_equal(anyDuplicated(adsl$USUBJID), 0L)
})

test_that("ADAE keys stay unique per subject and event", {
  adae <- readRDS(file.path(paths$adam, "adae.rds"))
  expect_true(!anyDuplicated(adae[c("USUBJID", "ASEQ")]))
})

test_that("BDS keys stay unique per subject, parameter and visit", {
  for (d in c("ADVS", "ADLB")) {
    df <- readRDS(file.path(paths$adam, paste0(tolower(d), ".rds")))
    expect_true(!anyDuplicated(df[c("USUBJID", "PARAMCD", "AVISITN")]),
                label = sprintf("%s key", d))
  }
})

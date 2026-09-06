# The shared fixture cache (helper-edc2cdisc.R) -----------------------------
# Every meta-test corrupts a fetched copy of the built pipeline output. That
# style is only safe while R copies a data frame before mutating it when
# another reference exists: if R ever mutated the shared build in place, the
# corruption would leak into every later test and fail the suite at random.
# This file pins the contract so a future R change fails here, loudly.

test_that("corrupting a fetched copy never poisons the shared build", {
  built1 <- built_suite()
  built1$sdtm$TA$ETCD[1] <- "POISON"
  built1$adam$ADSL$AGE[1] <- 999L
  built2 <- built_suite()
  expect_equal(built2$sdtm$TA$ETCD[1], "SCRN")
  expect_false(999L %in% built2$adam$ADSL$AGE)
})

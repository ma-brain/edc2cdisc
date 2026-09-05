# Trial design builders ---------------------------------------------------
# TA/TE/TI/TV/TS are built from spec tables, not CRF forms: the protocol
# lives in the spec, the builders reshape it, and validate_sdtm() recomputes
# what it can instead of trusting them.

test_that("map_te returns the SYNTH01 elements in spec order", {
  te <- map_te(spec_synth01)
  expect_equal(nrow(te), 2)
  # ignore_attr = "label": the builders label their columns (checked below),
  # so the bare-value comparisons must not trip over that attribute
  expect_equal(te$ETCD, c("SCRN", "TREAT"), ignore_attr = "label")
  expect_equal(te$ELEMENT, c("Screening", "Treatment"), ignore_attr = "label")
  expect_equal(te$TEDUR, c("P2W", "P12W"), ignore_attr = "label")
  expect_true(all(te$STUDYID == "3021"))
  expect_true(all(te$DOMAIN == "TE"))
  expect_equal(attr(te$ETCD, "label"), "Element Code")
})

test_that("map_ta joins the arm decode and element names", {
  ta <- map_ta(spec_synth01)
  expect_equal(nrow(ta), 6)
  # ARMCD/ARM are carried from spec$arms; ELEMENT from spec$elements
  expect_setequal(unique(ta$ARMCD), c("PBO", "SYN50", "SYN100"))
  expect_setequal(unique(ta$ARM), c("Placebo", "SYN-101 50 mg", "SYN-101 100 mg"))
  expect_equal(sum(ta$ELEMENT == "Treatment"), 3)
  # the transition rule sits on SCRN, the branch on TREAT
  expect_equal(unique(ta$TATRANS[ta$ETCD == "SCRN"]), "Randomised")
  expect_true(all(is.na(ta$TATRANS[ta$ETCD == "TREAT"])))
  expect_true(all(!is.na(ta$TABRANCH[ta$ETCD == "TREAT"])))
  # each arm walks its elements in order
  expect_true(all(ta$TAETORD[ta$ETCD == "SCRN"] == 1L))
  expect_true(all(ta$TAETORD[ta$ETCD == "TREAT"] == 2L))
  expect_equal(attr(ta$TAETORD, "label"), "Planned Order of Element within Arm")
})

test_that("trial design builders emit XPT-safe names and labels", {
  for (df in list(map_te(spec_synth01), map_ta(spec_synth01))) {
    expect_true(all(nchar(names(df)) <= 8))
    long <- keep(var_label(df), \(l) !is.null(l) && nchar(l) > 40)
    expect_length(long, 0)
  }
})

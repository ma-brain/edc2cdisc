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
  for (df in list(map_te(spec_synth01), map_ta(spec_synth01),
                  map_ti(spec_synth01), map_ts(spec_synth01),
                  map_tv(spec_synth01))) {
    expect_true(all(nchar(names(df)) <= 8))
    long <- keep(var_label(df), \(l) !is.null(l) && nchar(l) > 40)
    expect_length(long, 0)
  }
})

test_that("map_ti returns the SYNTH01 criteria with CT categories", {
  ti <- map_ti(spec_synth01)
  expect_equal(nrow(ti), 6)
  expect_setequal(unique(ti$IECAT), c("INCLUSION", "EXCLUSION"))
  expect_equal(sum(ti$IECAT == "INCLUSION"), 3)
  expect_equal(sum(ti$IECAT == "EXCLUSION"), 3)
  expect_equal(ti$IETESTCD[1], "INC1")
  expect_equal(attr(ti$IETESTCD, "label"), "Incl/Excl Criterion Short Name")
})

test_that("map_ts returns the SYNTH01 parameters with values", {
  ts <- map_ts(spec_synth01)
  expect_equal(nrow(ts), 7)
  expect_setequal(ts$TSPARMCD,
                  c("TITLE", "PHASE", "SPONSOR", "INDIC", "TRT",
                    "NARMS", "PLANSUB"))
  expect_equal(ts$TSVAL[ts$TSPARMCD == "NARMS"], "3")
  expect_equal(ts$TSVAL[ts$TSPARMCD == "PLANSUB"], "24")
  # exactly one of TSVAL / TSVALNF is filled on every row
  expect_true(all(!is.na(ts$TSVAL) & ts$TSVAL != ""))
  expect_true(all(is.na(ts$TSVALNF) | ts$TSVALNF == ""))
})

test_that("map_tv derives the planned visits from spec$visits", {
  tv <- map_tv(spec_synth01)
  expect_equal(nrow(tv), 6)
  expect_equal(tv$VISITNUM, 1:6, ignore_attr = "label")
  expect_equal(tv$VISIT[1], "SCREENING")
  expect_equal(tv$EPOCH[tv$VISIT == "BASELINE"], "TREATMENT")
  # VISITDY follows the no-day-0 rule map_sv() uses for planned days:
  # screening (day -14) stays negative, baseline (day 0) becomes day 1
  expect_equal(tv$VISITDY, c(-14L, 1L, 15L, 29L, 57L, 85L),
               ignore_attr = "label")
  expect_equal(attr(tv$VISITDY, "label"), "Planned Study Day of Visit")
})

test_that("build_all returns the trial design domains for both studies", {
  out <- file.path(tempdir(), "td-build")
  on.exit(unlink(out, recursive = TRUE, force = TRUE), add = TRUE)

  ext1 <- file.path(out, "rave1")
  suppressMessages(generate_rave_extract(out = ext1))
  built1 <- build_all(ext1)
  expect_setequal(names(built1$sdtm),
                  c("DM", "EX", "VS", "AE", "CM", "DS", "SV", "LB", "MH",
                    "SUPPDM", "SUPPAE", "SUPPEX", "CO", "RELREC",
                    "TA", "TE", "TI", "TV", "TS"))
  expect_equal(nrow(built1$sdtm$TA), 6)
  expect_equal(nrow(built1$sdtm$TE), 2)
  expect_equal(nrow(built1$sdtm$TI), 6)
  expect_equal(nrow(built1$sdtm$TV), 6)
  expect_equal(nrow(built1$sdtm$TS), 7)

  ext2 <- file.path(out, "rave2")
  suppressMessages(generate_rave_extract(out = ext2, study = "SYNTH02"))
  built2 <- suppressMessages(build_all(ext2, spec = spec_synth02))
  expect_equal(nrow(built2$sdtm$TA), 8)
  expect_equal(nrow(built2$sdtm$TV), 6)
  expect_equal(nrow(built2$sdtm$TS), 7)
})

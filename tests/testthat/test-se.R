# SE: Subject Elements -------------------------------------------------------
# SE is derived, not collected: map_se() pivots the built DM's reference
# dates into one row per subject per element actually started, with
# ETCD/ELEMENT read from spec$elements. build_all() does not wire SE yet
# (Task 5 does), so the fixture calls map_se() directly on build_all()'s DM -
# the intended usage.

se_fixture <- function() {
  out <- file.path(tempdir(), "se-fix")
  dir.create(out, showWarnings = FALSE)
  ext <- file.path(out, "rave")
  if (!dir.exists(ext)) suppressMessages(generate_rave_extract(out = ext))
  built <- suppressMessages(build_all(ext))
  list(se = map_se(built$sdtm$DM, spec_synth01),
       dm = built$sdtm$DM)
}

test_that("map_se yields two rows per randomized subject, one per screen failure", {
  f <- se_fixture()
  sf    <- f$dm$USUBJID[f$dm$ARMCD == "SCRNFAIL"]
  dosed <- setdiff(f$dm$USUBJID, sf)

  counts <- count(f$se, USUBJID)
  # every DM subject produces at least the SCRN element, and nothing else
  expect_setequal(counts$USUBJID, f$dm$USUBJID)
  expect_true(all(counts$n[counts$USUBJID %in% dosed] == 2L))
  expect_true(all(counts$n[counts$USUBJID %in% sf] == 1L))
  expect_equal(nrow(f$se), 2L * length(dosed) + length(sf))
})

test_that("element/date mappings follow the design table", {
  f <- se_fixture()
  j <- f$se |> left_join(f$dm, by = "USUBJID")

  scrn  <- j |> filter(ETCD == "SCRN")
  treat <- j |> filter(ETCD == "TREAT")

  # SCRN runs from informed consent to first dose (value equality: the SE
  # labels differ from the DM ones they came from)
  expect_equal(scrn$SESTDTC, scrn$RFICDTC, ignore_attr = "label")
  expect_equal(scrn$SEENDTC, scrn$RFXSTDTC, ignore_attr = "label")
  # TREAT runs from first dose to last dose; DM DTC strings pass through
  expect_equal(treat$SESTDTC, treat$RFXSTDTC, ignore_attr = "label")
  expect_equal(treat$SEENDTC, treat$RFXENDTC, ignore_attr = "label")
})

test_that("screen failures get only SCRN, with a blank end date", {
  f <- se_fixture()
  sf <- f$dm$USUBJID[f$dm$ARMCD == "SCRNFAIL"]
  sf_se <- f$se |> filter(USUBJID %in% sf)
  expect_length(unique(sf_se$USUBJID), length(sf))
  expect_true(all(sf_se$ETCD == "SCRN"))
  # element not ended: no invented end date
  expect_true(all(is.na(sf_se$SEENDTC)))
  expect_false(any(f$se$USUBJID %in% sf & f$se$ETCD == "TREAT"))
})

test_that("SESEQ runs 1..n within every subject, SCRN first", {
  f <- se_fixture()
  seq_ok <- f$se |>
    arrange(USUBJID, SESEQ) |>
    summarise(ok = all(SESEQ == seq_len(dplyr::n())), .by = USUBJID)
  expect_true(all(seq_ok$ok))
  expect_true(all(f$se$ETCD[f$se$SESEQ == 1L] == "SCRN"))
  expect_true(all(f$se$SESEQ[f$se$ETCD == "TREAT"] == 2L))
})

test_that("map_se returns the interface columns in order, labelled", {
  f <- se_fixture()
  expect_equal(names(f$se), c("STUDYID", "DOMAIN", "USUBJID", "SESEQ", "ETCD",
                              "ELEMENT", "SESTDTC", "SEENDTC"))
  # pin the exact label text; the <=40 sweep only guards future columns
  expect_equal(
    unlist(var_label(f$se)),
    c(STUDYID  = "Study Identifier",
      DOMAIN   = "Domain Abbreviation",
      USUBJID  = "Unique Subject Identifier",
      SESEQ    = "Sequence Number",
      ETCD     = "Element Code",
      ELEMENT  = "Description of Element",
      SESTDTC  = "Start Date/Time of Subject Element",
      SEENDTC  = "End Date/Time of Subject Element")
  )
  long <- keep(var_label(f$se), \(l) !is.null(l) && nchar(l) > 40)
  expect_length(long, 0)
})

test_that("ETCD/ELEMENT values come from spec$elements", {
  f <- se_fixture()
  expect_setequal(unique(f$se$ETCD), spec_synth01$elements$ETCD)
  joined <- f$se |>
    left_join(select(spec_synth01$elements, ETCD, ELEMENT_SPEC = ELEMENT),
              by = "ETCD")
  expect_equal(joined$ELEMENT, joined$ELEMENT_SPEC, ignore_attr = "label")
})

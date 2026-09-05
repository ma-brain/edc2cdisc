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

# Meta-tests: corrupt a trial design domain, assert the validator trips ---

td_fixture <- function() {
  out <- file.path(tempdir(), "td-meta")
  dir.create(out, showWarnings = FALSE)
  ext <- file.path(out, "rave")
  if (!dir.exists(ext)) suppressMessages(generate_rave_extract(out = ext))
  build_all(ext)
}

test_that("a TA element missing from TE trips ta-etcd-not-in-te", {
  domains <- td_fixture()$sdtm
  domains$TA$ETCD[1] <- "NOPE"
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("ta-etcd-not-in-te" %in% issues$check)
})

test_that("a NARMS row disagreeing with spec$arms trips ts-narms-mismatch", {
  domains <- td_fixture()$sdtm
  domains$TS$TSVAL[domains$TS$TSPARMCD == "NARMS"] <- "9"
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("ts-narms-mismatch" %in% issues$check)
  expect_error(stop_on_error(issues, "meta"), "validation error")
})

test_that("a PLANSUB row disagreeing with study$n trips ts-plansub-mismatch", {
  domains <- td_fixture()$sdtm
  domains$TS$TSVAL[domains$TS$TSPARMCD == "PLANSUB"] <- "999"
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("ts-plansub-mismatch" %in% issues$check)
})

test_that("a null-flavor NARMS row does not crash the TS mismatch check", {
  domains <- td_fixture()$sdtm
  hit <- domains$TS$TSPARMCD == "NARMS"
  domains$TS$TSVAL[hit] <- NA_character_
  domains$TS$TSVALNF[hit] <- "N/A"
  issues <- validate_sdtm(domains, spec_synth01)
  expect_false("ts-narms-mismatch" %in% issues$check)
})

test_that("a TS row with both TSVAL and TSVALNF blank trips ts-valnf-xor", {
  domains <- td_fixture()$sdtm
  hit <- domains$TS$TSPARMCD == "PLANSUB"
  domains$TS$TSVAL[hit] <- NA_character_
  expect_true("ts-valnf-xor" %in% validate_sdtm(domains, spec_synth01)$check)
})

test_that("TV drifting from spec$visits trips tv-vs-spec-visits", {
  domains <- td_fixture()$sdtm
  domains$TV$VISITDY[1] <- 99L
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("tv-vs-spec-visits" %in% issues$check)
})

test_that("an SV visit that TV never planned trips sv-visit-not-in-tv", {
  domains <- td_fixture()$sdtm
  ghost <- domains$SV[1, ] |> mutate(VISITNUM = 99L)
  domains$SV <- bind_rows(domains$SV, ghost)
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("sv-visit-not-in-tv" %in% issues$check)
})

test_that("duplicate TS parameters and valflavor violations trip", {
  domains <- td_fixture()$sdtm
  domains$TS <- bind_rows(domains$TS, domains$TS[domains$TS$TSPARMCD == "NARMS", ])
  expect_true("ts-parmcd-not-unique" %in%
                validate_sdtm(domains, spec_synth01)$check)

  domains <- td_fixture()$sdtm
  domains$TS$TSVALNF[domains$TS$TSPARMCD == "NARMS"] <- "N/A"
  expect_true("ts-valnf-xor" %in% validate_sdtm(domains, spec_synth01)$check)
})

test_that("a bad IECAT trips iecat-bad-value", {
  domains <- td_fixture()$sdtm
  domains$TI$IECAT[1] <- "MAYBE"
  expect_true("iecat-bad-value" %in% validate_sdtm(domains, spec_synth01)$check)
})

test_that("dropping a required trial design variable trips required-vars", {
  domains <- td_fixture()$sdtm
  domains$TA$EPOCH <- NULL
  expect_true(any(validate_sdtm(domains, spec_synth01)$check == "required-vars" &
                    validate_sdtm(domains, spec_synth01)$domain == "TA"))
})

test_that("a clean build has zero trial design findings", {
  domains <- td_fixture()$sdtm
  expect_equal(nrow(validate_sdtm(domains, spec_synth01)), 0)
})

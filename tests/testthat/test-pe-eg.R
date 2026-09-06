# PE/EG: physical exam and ECG ----------------------------------------------
# Both forms are one row per subject-visit with the findings side by side
# (CARDIO_DECODE..., PR_RAW...). map_pe()/map_eg() pivot each to one row per
# item; the fixture generates both studies once and reuses them, like
# qs_fixture().

pe_eg_fixture <- function(study = c("SYNTH01", "SYNTH02")) {
  study <- match.arg(study)
  out <- file.path(tempdir(), paste0("pe-eg-", study))
  dir.create(out, showWarnings = FALSE)
  ext <- file.path(out, "rave")
  if (!dir.exists(ext)) {
    suppressMessages(generate_rave_extract(out = ext, study = study))
  }
  spec <- switch(study, SYNTH01 = spec_synth01, SYNTH02 = spec_synth02)
  forms <- suppressMessages(read_rave_extract(dir = ext))
  dm   <- map_dm(forms$DM, forms$EX, forms$DS, spec)
  refs <- subject_ref(dm)
  list(pe   = map_pe(forms$PE, spec, refs),
       eg   = map_eg(forms$EG, spec, refs),
       vs   = map_vs(forms$VS, spec, refs),
       dm   = dm,
       spec = spec)
}

test_that("map_pe and map_eg pivot one row per spec item for every VS subject-visit", {
  for (study in c("SYNTH01", "SYNTH02")) {
    f <- pe_eg_fixture(study)
    keys <- distinct(f$vs[, c("USUBJID", "VISITNUM")])
    for (dom_name in c("pe", "eg")) {
      dom <- f[[dom_name]]
      n_items <- nrow(filter(f$spec$tests, domain == toupper(dom_name)))
      counts <- count(dom, USUBJID, VISITNUM)
      # every VS key is covered exactly n_items times, and the domain has no
      # extras
      expect_setequal(paste(counts$USUBJID, counts$VISITNUM),
                      paste(keys$USUBJID, keys$VISITNUM))
      expect_true(all(counts$n == n_items),
                  label = sprintf("%s rows per key (%s)", dom_name, study))
      expect_equal(nrow(dom), n_items * nrow(keys))
    }
    # the spec drives the panel: 4 PE systems in SYNTH01, 5 in SYNTH02
    # (SYNTH02 adds the skin exam), 5 ECG intervals in both
    expect_equal(nrow(filter(f$spec$tests, domain == "PE")),
                 if (study == "SYNTH01") 4L else 5L)
    expect_equal(nrow(filter(f$spec$tests, domain == "EG")), 5L)
  }
})

test_that("sequence numbers run 1..n within every subject, visits in order", {
  f <- pe_eg_fixture("SYNTH01")
  for (dom_name in c("pe", "eg")) {
    dom <- f[[dom_name]]
    seq_var <- paste0(toupper(dom_name), "SEQ")
    seq_ok <- dom |>
      arrange(USUBJID, .data[[seq_var]]) |>
      summarise(ok = all(.data[[seq_var]] == seq_len(dplyr::n())), .by = USUBJID)
    expect_true(all(seq_ok$ok),
                label = sprintf("%s: 1..n in every subject", seq_var))
  }
})

test_that("map_pe/map_eg return the interface columns in order, labelled", {
  f <- pe_eg_fixture("SYNTH01")
  expect_equal(names(f$pe), c("STUDYID", "DOMAIN", "USUBJID", "PESEQ",
                              "PETESTCD", "PETEST", "PEORRES", "PECLSIG",
                              "PESTAT", "PEREASND", "VISITNUM", "VISIT",
                              "PEDTC", "PEDY"))
  expect_equal(names(f$eg), c("STUDYID", "DOMAIN", "USUBJID", "EGSEQ",
                              "EGTESTCD", "EGTEST", "EGORRES", "EGSTRESN",
                              "EGSTRESU", "EGSTAT", "EGREASND", "EGBLFL",
                              "VISITNUM", "VISIT", "EGDTC", "EGDY"))
  expect_setequal(unique(f$pe$PETESTCD),
                  filter(f$spec$tests, domain == "PE")$testcd)
  expect_setequal(unique(f$eg$EGTESTCD),
                  filter(f$spec$tests, domain == "EG")$testcd)
  # PE carries no baseline flag by design; EG does
  expect_false("PEBLFL" %in% names(f$pe))
  expect_true("EGBLFL" %in% names(f$eg))
  for (dom in list(f$pe, f$eg)) {
    long <- keep(var_label(dom), \(l) !is.null(l) && nchar(l) > 40)
    expect_length(long, 0)
  }
})

test_that("the seeded abnormal PE finding is CV ABNORMAL with PECLSIG Y, exactly one", {
  for (study in c("SYNTH01", "SYNTH02")) {
    f <- pe_eg_fixture(study)
    abnormal_subj <- switch(study, SYNTH01 = "3021-101-004",
                            SYNTH02 = "4033-201-004")
    sig <- f$pe |> filter(PECLSIG == "Y")
    expect_equal(nrow(sig), 1L,
                 label = sprintf("PECLSIG Y rows (%s)", study))
    expect_equal(unique(sig$PETESTCD), "CV")
    expect_equal(unique(sig$PEORRES), "ABNORMAL")
    expect_equal(unique(sig$USUBJID), abnormal_subj)
    expect_equal(unique(sig$VISITNUM), 2) # BASE

    # every other answered system is NORMAL with PECLSIG N
    answered <- f$pe |> filter(is.na(PESTAT))
    expect_setequal(unique(answered$PEORRES), c("NORMAL", "ABNORMAL"))
    expect_true(all(answered$PECLSIG[answered$PEORRES == "NORMAL"] == "N"))
  }
})

test_that("SUPPPE carries exactly the seeded abnormality detail, linked to its PE record", {
  out <- file.path(tempdir(), "pe-eg-supppe")
  on.exit(unlink(out, recursive = TRUE, force = TRUE), add = TRUE)
  ext <- file.path(out, "rave")
  suppressMessages(generate_rave_extract(out = ext))

  built <- suppressMessages(build_all(ext))
  supppe <- built$sdtm$SUPPPE
  parent <- built$sdtm$PE |> filter(PECLSIG == "Y")

  # the qualifier exists only where the exam collected one: the CV/ABNORMAL
  # row's "specify abnormality" text, pointing back at that PE record
  expect_equal(nrow(supppe), 1L)
  expect_equal(as.vector(supppe$QNAM), "PEABNDT")
  expect_equal(as.vector(supppe$QVAL), "Systolic murmur")
  expect_equal(as.vector(supppe$QLABEL), "Abnormality Details")
  expect_equal(as.vector(supppe$IDVAR), "PESEQ")
  expect_equal(as.vector(supppe$IDVARVAL), as.character(parent$PESEQ))
  expect_equal(supppe$USUBJID, parent$USUBJID)
  expect_equal(unique(as.vector(supppe$USUBJID)), "3021-101-004")
  expect_equal(as.vector(supppe$RDOMAIN), "PE")
  expect_equal(as.vector(supppe$QORIG), "CRF")
  expect_true(is.na(supppe$QEVAL))
})

test_that("the seeded not-done visits yield STAT rows with reasons and blank results", {
  for (study in c("SYNTH01", "SYNTH02")) {
    f <- pe_eg_fixture(study)
    pe_items <- nrow(filter(f$spec$tests, domain == "PE"))
    eg_items <- nrow(filter(f$spec$tests, domain == "EG"))
    # generator seeds: PE not-done idx 12 at BASE in both studies; EG not-done
    # idx 9 in SYNTH01 but idx 7 in SYNTH02 (its idx 9 is an early-termination
    # subject whose Week 12 visit does not exist), both at visit 5
    pe_subj <- switch(study, SYNTH01 = "3021-101-013", SYNTH02 = "4033-201-013")
    eg_subj <- switch(study, SYNTH01 = "3021-101-010", SYNTH02 = "4033-202-008")

    # PE: one STAT row per system
    pe_stat <- f$pe |> filter(PESTAT == "NOT DONE")
    expect_equal(nrow(pe_stat), pe_items,
                 label = sprintf("PE STAT rows (%s)", study))
    expect_true(all(pe_stat$USUBJID == pe_subj))
    expect_equal(unique(pe_stat$VISITNUM), 2) # BASE
    expect_equal(unique(pe_stat$PEREASND), "Subject unwell")
    expect_true(all(is.na(pe_stat$PEORRES)))
    expect_true(all(is.na(pe_stat$PECLSIG)))
    # the visit date is still collected on a not-done form
    expect_true(all(str_length(pe_stat$PEDTC) == 10))

    # EG: one STAT row per interval
    eg_stat <- f$eg |> filter(EGSTAT == "NOT DONE")
    expect_equal(nrow(eg_stat), eg_items,
                 label = sprintf("EG STAT rows (%s)", study))
    expect_true(all(eg_stat$USUBJID == eg_subj))
    expect_equal(unique(eg_stat$VISITNUM), 5)
    expect_equal(unique(eg_stat$EGREASND), "Equipment failure")
    expect_true(all(is.na(eg_stat$EGORRES)))
    expect_true(all(is.na(eg_stat$EGSTRESN)))
    expect_true(all(is.na(eg_stat$EGSTRESU)))
    expect_true(all(is.na(eg_stat$EGBLFL)))
  }
})

test_that("EG values parse numerically in msec; the blank-time row is date-only", {
  for (study in c("SYNTH01", "SYNTH02")) {
    f <- pe_eg_fixture(study)
    answered <- f$eg |> filter(is.na(EGSTAT))
    # the generator's interval arithmetic only ever produces plain integers,
    # so every answered EGORRES parses
    expect_true(all(!is.na(answered$EGSTRESN)),
                label = sprintf("answered EGSTRESN (%s)", study))
    expect_true(all(answered$EGSTRESU == "msec"))
    expect_true(all(is.na(f$eg$EGSTRESU[is.na(f$eg$EGSTRESN)])))

    # the generator leaves EGTIM blank for subject idx 2 at visit 3 (WK02 in
    # SYNTH01, WK04 in SYNTH02): the intervals are still there (120/98/386/
    # 405/1000, the base=5 arithmetic) but EGDTC carries no time component
    late_subj <- switch(study, SYNTH01 = "3021-103-003", SYNTH02 = "4033-203-003")
    late <- f$eg |> filter(USUBJID == late_subj, VISITNUM == 3)
    expect_equal(nrow(late), 5L, label = sprintf("late rows (%s)", study))
    expect_setequal(late$EGSTRESN, c(120, 98, 386, 405, 1000))
    expect_true(all(str_length(late$EGDTC) == 10))
    expect_false(any(str_detect(late$EGDTC, "T")))
  }
})

test_that("EGBLFL flags only numeric results on or before first dose", {
  f <- pe_eg_fixture("SYNTH01")
  expect_true(all(is.na(f$eg$EGBLFL[!is.na(f$eg$EGSTAT)])))
  # dosing starts at BASE (visit 2); later visits are never baseline
  expect_true(all(f$eg$VISITNUM[f$eg$EGBLFL %in% "Y"] <= 2))
  expect_true(all(!is.na(f$eg$EGSTRESN[f$eg$EGBLFL %in% "Y"])))
})

test_that("screen failures appear only at screening, with no study day", {
  for (study in c("SYNTH01", "SYNTH02")) {
    f <- pe_eg_fixture(study)
    sf <- f$dm$USUBJID[f$dm$ARMCD == "SCRNFAIL"]
    for (dom_name in c("pe", "eg")) {
      dom <- f[[dom_name]]
      dy_var <- paste0(toupper(dom_name), "DY")
      sf_dom <- dom |> filter(USUBJID %in% sf)
      expect_length(unique(sf_dom$USUBJID), length(sf))
      expect_true(all(sf_dom$VISITNUM == 1))
      # RFSTDTC is the DY anchor; screen failures are never dosed
      expect_true(all(is.na(sf_dom[[dy_var]])))
    }
  }
})

# Meta-tests: corrupt PE/EG/SUPPPE, assert the validator trips --------------

pe_eg_built <- function() {
  built_suite()
}

test_that("a PEORRES outside NORMAL/ABNORMAL trips peorres-bad-value", {
  domains <- pe_eg_built()$sdtm
  idx <- which(domains$PE$PEORRES == "NORMAL")[1]
  domains$PE$PEORRES[idx] <- "MAYBE"
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("peorres-bad-value" %in% issues$check[issues$domain == "PE"])
})

test_that("a flipped PECLSIG on a NORMAL row trips peclsig-coherence", {
  domains <- pe_eg_built()$sdtm
  idx <- which(domains$PE$PEORRES == "NORMAL" & domains$PE$PECLSIG == "N")[1]
  domains$PE$PECLSIG[idx] <- "Y"
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("peclsig-coherence" %in% issues$check[issues$domain == "PE"])
})

test_that("EGSTRESU usec on a numeric result row trips egstresu-fixed", {
  domains <- pe_eg_built()$sdtm
  idx <- which(!is.na(domains$EG$EGSTRESN))[1]
  domains$EG$EGSTRESU[idx] <- "usec"
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("egstresu-fixed" %in% issues$check[issues$domain == "EG"])
})

test_that("a NOT DONE PE row with a blank reason trips stat-reason", {
  domains <- pe_eg_built()$sdtm
  idx <- which(domains$PE$PESTAT == "NOT DONE")[1]
  domains$PE$PEREASND[idx] <- NA_character_
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("stat-reason" %in% issues$check[issues$domain == "PE"])
})

test_that("a NOT DONE EG row with a blank reason trips stat-reason", {
  domains <- pe_eg_built()$sdtm
  idx <- which(domains$EG$EGSTAT == "NOT DONE")[1]
  domains$EG$EGREASND[idx] <- NA_character_
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("stat-reason" %in% issues$check[issues$domain == "EG"])
})

test_that("a status on a collected result trips stat-reason even without NOT DONE", {
  # SDTM --STAT CT only allows NOT DONE, but arm 2 of the check (a populated
  # result may not declare a status) must not depend on the value: an exotic
  # STAT string on an answered row still fires
  domains <- pe_eg_built()$sdtm
  idx <- which(is.na(domains$PE$PESTAT))[1]
  domains$PE$PESTAT[idx] <- "PARTIAL"
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("stat-reason" %in% issues$check[issues$domain == "PE"])
})

test_that("a SUPPPE row for a subject with no PE record trips related-parent-orphan", {
  domains <- pe_eg_built()$sdtm
  domains$SUPPPE$USUBJID[1] <- "3021-999-999" # in neither PE nor DM
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("related-parent-orphan" %in% issues$check[issues$domain == "SUPPPE"])
})

test_that("a clean build still validates with zero findings", {
  built <- pe_eg_built()
  expect_equal(nrow(validate_sdtm(built$sdtm, spec_synth01)), 0)
})

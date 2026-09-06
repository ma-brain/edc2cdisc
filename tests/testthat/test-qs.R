# QS: the scheduled event questionnaire -------------------------------------
# The QS form is one row per subject-visit with the item answers side by side
# (MOS01_RAW...). map_qs() pivots it to one row per item; the fixture
# generates the extract once and reuses it, like td_fixture().

qs_fixture <- function() {
  out <- file.path(tempdir(), "qs-fix")
  dir.create(out, showWarnings = FALSE)
  ext <- file.path(out, "rave")
  if (!dir.exists(ext)) suppressMessages(generate_rave_extract(out = ext))
  built <- suppressMessages(build_all(ext))
  forms <- suppressMessages(read_rave_extract(dir = ext))
  qs <- map_qs(forms$QS, spec_synth01, subject_ref(built$sdtm$DM))
  list(qs = qs, dm = built$sdtm$DM, vs = built$sdtm$VS)
}

test_that("map_qs pivots one row per spec item for every VS subject-visit", {
  f <- qs_fixture()
  qs_n_items <- filter(spec_synth01$tests, domain == "QS") |> nrow()

  keys <- distinct(f$vs[, c("USUBJID", "VISITNUM")])
  counts <- count(f$qs, USUBJID, VISITNUM)
  # every VS key is covered exactly qs_n_items times, and QS has no extras
  expect_setequal(paste(counts$USUBJID, counts$VISITNUM),
                  paste(keys$USUBJID, keys$VISITNUM))
  expect_true(all(counts$n == qs_n_items))
  expect_equal(nrow(f$qs), qs_n_items * nrow(keys))

  # QSSEQ numbers 1..n within every subject across visits, and within one
  # subject-visit the sequence follows QSTESTCD ascending
  seq_ok <- f$qs |>
    arrange(USUBJID, QSSEQ) |>
    summarise(ok = all(QSSEQ == seq_len(dplyr::n())), .by = USUBJID)
  expect_true(all(seq_ok$ok))
  item_order <- f$qs |>
    arrange(USUBJID, VISITNUM, QSSEQ) |>
    summarise(ok = all(QSTESTCD == sort(QSTESTCD)),
              .by = c(USUBJID, VISITNUM))
  expect_true(all(item_order$ok))
})

test_that("map_qs returns the interface columns in order, labelled", {
  f <- qs_fixture()
  expect_equal(names(f$qs), c("STUDYID", "DOMAIN", "USUBJID", "QSSEQ", "QSCAT",
                              "QSTESTCD", "QSTEST", "QSORRES", "QSSTAT",
                              "QSREASND", "QSSTRESC", "QSSTRESN", "QSSTRESU",
                              "QSBLFL", "VISITNUM", "VISIT", "QSDTC", "QSDY"))
  expect_true(all(f$qs$QSCAT == "MOOD SCALE"))
  expect_setequal(unique(f$qs$QSTESTCD),
                  filter(spec_synth01$tests, domain == "QS")$testcd)
  # pin the exact label text; the <=40 sweep only guards future columns
  expect_equal(
    unlist(var_label(f$qs)),
    c(STUDYID  = "Study Identifier",
      DOMAIN   = "Domain Abbreviation",
      USUBJID  = "Unique Subject Identifier",
      QSSEQ    = "Sequence Number",
      QSCAT    = "Category of Questionnaire",
      QSTESTCD = "Question Short Name",
      QSTEST   = "Question",
      QSORRES  = "Result or Finding in Original Units",
      QSSTAT   = "Completion Status",
      QSREASND = "Reason Not Done",
      QSSTRESC = "Character Result/Finding in Std Units",
      QSSTRESN = "Numeric Result/Finding in Std Units",
      QSSTRESU = "Standard Units",
      QSBLFL   = "Baseline Flag",
      VISITNUM = "Visit Number",
      VISIT    = "Visit Name",
      QSDTC    = "Date/Time of Questionnaire",
      QSDY     = "Study Day of Questionnaire")
  )
  long <- keep(var_label(f$qs), \(l) !is.null(l) && nchar(l) > 40)
  expect_length(long, 0)
})

test_that("answered items parse numerically; QSBLFL needs a numeric result", {
  f <- qs_fixture()
  answered <- f$qs |> filter(is.na(QSSTAT))
  # the generator's item arithmetic only ever produces 0..3, so every
  # answered QSORRES parses (the QSBLFL rank runs on the parsed value)
  expect_true(all(!is.na(suppressWarnings(as.numeric(answered$QSORRES)))))
  expect_setequal(unique(as.numeric(answered$QSORRES)), 0:3)
  expect_true(all(answered$QSORRES != ""))

  # flags only on answered rows, and the deterministic data puts every flag
  # on BASE (visit 2): the last pre-dose numeric result is always collected
  expect_true(all(is.na(f$qs$QSBLFL[!is.na(f$qs$QSSTAT)])))
  expect_true(all(f$qs$VISITNUM[f$qs$QSBLFL %in% "Y"] == 2))
})

test_that("every answered row standardizes: QSSTRESN numeric, QSSTRESC its character", {
  f <- qs_fixture()
  answered <- f$qs |> filter(is.na(QSSTAT), !is.na(QSORRES), QSORRES != "")
  # the numeric result leaves the mapper: non-NA on every answered row, and
  # QSSTRESC is its character rendering (the VSSTRESC idiom)
  expect_true(all(!is.na(answered$QSSTRESN)))
  expect_equal(answered$QSSTRESC, as.character(answered$QSSTRESN),
               ignore_attr = "label")
  # ordinal items carry no unit: present-but-empty is the honest form
  expect_true(all(is.na(f$qs$QSSTRESU)))
})

test_that("the seeded not-done visit yields NOT DONE rows with blank results", {
  f <- qs_fixture()
  stat <- f$qs |> filter(QSSTAT == "NOT DONE")
  # subject idx 7 (102-008) at WK04, one row per questionnaire item
  expect_equal(nrow(stat), 4L)
  expect_true(all(stat$USUBJID == "3021-102-008"))
  expect_equal(unique(stat$VISITNUM), 4)
  expect_true(all(is.na(stat$QSORRES) | stat$QSORRES == ""))
  expect_true(all(is.na(stat$QSBLFL)))
  # a blank result standardizes to nothing: all three standardized variables NA
  expect_true(all(is.na(stat$QSSTRESC)))
  expect_true(all(is.na(stat$QSSTRESN)))
  expect_true(all(is.na(stat$QSSTRESU)))
  expect_equal(unique(stat$QSREASND), "Subject refused")
  # the visit date is still collected on a not-done form
  expect_true(all(str_length(stat$QSDTC) == 10))
})

test_that("screen failures appear only at screening, with no study day", {
  f <- qs_fixture()
  sf <- f$dm$USUBJID[f$dm$ARMCD == "SCRNFAIL"]
  sf_qs <- f$qs |> filter(USUBJID %in% sf)
  expect_length(unique(sf_qs$USUBJID), length(sf))
  expect_true(all(sf_qs$VISITNUM == 1))
  # RFSTDTC is the DY anchor; screen failures are never dosed
  expect_true(all(is.na(sf_qs$QSDY)))
  expect_true(all(is.na(sf_qs$QSBLFL)))
})

# Meta-tests: corrupt QS, assert the validator trips -----------------------

qs_built <- function() {
  out <- file.path(tempdir(), "qs-meta")
  dir.create(out, showWarnings = FALSE)
  ext <- file.path(out, "rave")
  if (!dir.exists(ext)) suppressMessages(generate_rave_extract(out = ext))
  suppressMessages(build_all(ext))
}

test_that("a QSCAT not in spec$tests trips qscat-not-in-spec", {
  domains <- qs_built()$sdtm
  domains$QS$QSCAT[1] <- "BOGUS"
  expect_true("qscat-not-in-spec" %in%
                validate_sdtm(domains, spec_synth01)$check)
})

test_that("a NOT DONE row with a blank reason trips stat-reason", {
  domains <- qs_built()$sdtm
  idx <- which(domains$QS$QSSTAT == "NOT DONE")[1]
  domains$QS$QSREASND[idx] <- NA_character_
  expect_true("stat-reason" %in%
                validate_sdtm(domains, spec_synth01)$check)
})

test_that("a result injected onto a NOT DONE row trips stat-reason", {
  domains <- qs_built()$sdtm
  idx <- which(domains$QS$QSSTAT == "NOT DONE")[1]
  domains$QS$QSORRES[idx] <- "2"
  expect_true("stat-reason" %in%
                validate_sdtm(domains, spec_synth01)$check)
})

test_that("a clean build validates QS with zero findings", {
  built <- qs_built()
  expect_equal(nrow(validate_sdtm(built$sdtm, spec_synth01)), 0)
})

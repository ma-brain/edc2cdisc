# Unit tests: SUPP and RELREC assembly ---------------------------------

test_that("make_supp drops blank qualifiers and pins records by idvar", {
  parent <- tibble::tibble(
    STUDYID = "S1", USUBJID = c("S1-101", "S1-102"),
    XXSEQ   = c(1L, 2L),
    XXSI    = c("Y", ""),      # blank -> no SUPP row for S1-102
    XXNOTE  = c("", "detail")
  )
  out <- make_supp(
    parent, "XX", "XXSEQ",
    qnams = tibble::tribble(
      ~qnam,   ~qlabel,
      "XXSI",  "Special Interest",
      "XXNOTE", "Note"
    )
  )
  expect_setequal(out$QNAM, c("XXSI", "XXNOTE"))
  expect_equal(nrow(out), 2)
  expect_equal(as.vector(out$QVAL[out$QNAM == "XXSI"]), "Y")
  expect_equal(as.vector(out$IDVARVAL), c("1", "2"))
  expect_equal(unique(as.vector(out$IDVAR)), "XXSEQ")
  expect_equal(unique(as.vector(out$QORIG)), "CRF")
})

test_that("make_supp with idvar = NA leaves the link to USUBJID", {
  parent <- tibble::tibble(STUDYID = "S1", USUBJID = "S1-101", INIT = "ABC")
  out <- make_supp(parent, "DM", NA,
                   qnams = tibble::tibble(qnam = "INIT", qlabel = "Initials"))
  expect_equal(nrow(out), 1)
  expect_true(is.na(out$IDVAR))
  expect_true(is.na(out$IDVARVAL))
})

test_that("make_supp honours src, qorig and qeval columns", {
  parent <- tibble::tibble(STUDYID = "S1", USUBJID = "S1-101",
                           AESEQ = c(1L, 2L),
                           FLAG = c("Y", "N"), NOTE = c("n", ""))
  out <- make_supp(
    parent, "AE", "AESEQ",
    qnams = tibble::tribble(
      ~qnam,   ~qlabel, ~src,    ~qorig,        ~qeval,
      "AESI",  "SI",    "FLAG",  "CRF",         "INVESTIGATOR",
      "AENOT", "Note",  "NOTE",  "OTHER",       NA_character_
    )
  )
  expect_setequal(out$QNAM, c("AESI", "AENOT"))
  expect_equal(as.vector(out$QVAL[out$QNAM == "AESI"]), c("Y", "N"))
  expect_equal(as.vector(out$QORIG[out$QNAM == "AENOT"]), "OTHER")
  expect_equal(as.vector(out$QEVAL[out$QNAM == "AESI"]),
               c("INVESTIGATOR", "INVESTIGATOR"))
  expect_length(unique(out$IDVARVAL), 2)                # one per AE record
})

test_that("make_relrec emits one row per side per link with a shared RELID", {
  links <- tibble::tibble(
    USUBJID    = "S1-101",
    RDOMAIN_1  = "CM", IDVAR_1 = "CMSEQ", IDVARVAL_1 = 1L,
    RDOMAIN_2  = "AE", IDVAR_2 = "AESEQ", IDVARVAL_2 = 3L
  )
  out <- make_relrec(links, study_id = "3021")
  expect_equal(nrow(out), 2)
  expect_setequal(out$RDOMAIN, c("CM", "AE"))
  expect_length(unique(out$RELID), 1)
  expect_match(unique(out$RELID), "^3021-RL-0001$")
  expect_equal(unique(out$STUDYID), "3021")
  # RELTYPE describes a dataset-level relationship; these are record-level
  # links (IDVAR populated), which SDTMIG leaves null - populating it is
  # what Pinnacle 21 flags
  expect_true(all(is.na(out$RELTYPE)))
})

test_that("make_relrec validates the link structure", {
  expect_error(make_relrec(tibble::tibble(USUBJID = "x"), study_id = "S"),
               "links missing")
})

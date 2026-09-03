# Meta-tests: corrupt an input, assert the validator catches it ------------
# The validators are only useful if they actually fire. Rather than trusting
# a green run, break the data on purpose and check the right check trips.

build_fixtures <- function() {
  out <- file.path(tempdir(), "edc2cdisc-meta")
  dir.create(out, showWarnings = FALSE)
  ext <- file.path(out, "rave")
  if (!dir.exists(ext)) suppressMessages(generate_rave_extract(out = ext))
  built <- build_all(ext)
  list(built = built, ext = ext)
}

test_that("a flipped SAFFL trips the SAFFL/TRTSDT coherence check", {
  built <- build_fixtures()$built
  adsl <- built$adam$ADSL
  adsl$SAFFL[3] <- ifelse(adsl$SAFFL[3] == "Y", "N", "Y")

  issues <- validate_adam(adsl, built$adam$ADAE, built$adam$ADVS,
                          built$adam$ADLB, built$sdtm$DM, built$sdtm$DS,
                          built$sdtm$AE, built$sdtm$VS, built$sdtm$LB,
                          built$sdtm$SUPPAE, spec_synth01)
  expect_true("saffl-trtsdt-coherence" %in% issues$check)
  expect_error(stop_on_error(issues, "meta"), "1 validation error")
})

test_that("a flipped TRTEMFL is recomputed and flagged", {
  built <- build_fixtures()$built
  adae <- built$adam$ADAE
  treated <- adae$TRTEMFL == "Y"
  adae$TRTEMFL[which(treated)[1]] <- ""

  issues <- validate_adam(built$adam$ADSL, adae, built$adam$ADVS,
                          built$adam$ADLB, built$sdtm$DM, built$sdtm$DS,
                          built$sdtm$AE, built$sdtm$VS, built$sdtm$LB,
                          built$sdtm$SUPPAE, spec_synth01)
  expect_true("trtemfl-not-derivable" %in% issues$check)
})

test_that("a zero study day is caught in SDTM and ADaM", {
  built <- build_fixtures()$built
  ae <- built$sdtm$AE
  ae$AESTDY[1] <- 0L
  domains <- built$sdtm
  domains$AE <- ae
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("study-day-zero" %in% issues$check)

  adae <- built$adam$ADAE
  adae$AENDY[1] <- 0L
  issues <- validate_adam(built$adam$ADSL, adae, built$adam$ADVS,
                          built$adam$ADLB, built$sdtm$DM, built$sdtm$DS,
                          built$sdtm$AE, built$sdtm$VS, built$sdtm$LB,
                          built$sdtm$SUPPAE, spec_synth01)
  expect_true("adae-study-day-zero" %in% issues$check)
})

test_that("dropping a required column fires required-vars", {
  built <- build_fixtures()$built
  vs <- built$sdtm$VS
  vs$VSTESTCD <- NULL
  domains <- built$sdtm
  domains$VS <- vs
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("required-vars" %in% issues$check[issues$domain == "VS"])
})

test_that("a dropped dataset row breaks referential integrity loudly", {
  built <- build_fixtures()$built
  domains <- built$sdtm
  domains$AE <- built$sdtm$AE[-1, ]    # an AE record vanishes
  issues <- validate_sdtm(domains, spec_synth01)
  # SUPPAE / CO still carry the deleted AESEQ -> orphaned related records
  expect_true("related-parent-orphan" %in% issues$check)

  # a lost SUPPAE qualifier row means an AE record without its qualifiers:
  # ADAE refuses to build rather than drifting silently
  suppae <- built$sdtm$SUPPAE
  suppae <- suppae[1:3, ]
  expect_error(derive_adae(built$sdtm$AE, suppae, built$adam$ADSL),
               "no SUPPAE qualifiers")
})

test_that("an unmapped codelist value breaks the mapper, not the report", {
  built <- build_fixtures()$built
  fx <- build_fixtures()
  ae_raw <- read_clinical_view("AE", fx$ext)
  ae_raw$AESEV_DECODE[1] <- "CATASTROPHIC"
  refs <- subject_ref(built$sdtm$DM)
  expect_error(map_ae(ae_raw, spec_synth01, refs),
               "unmapped codelist value")
})

test_that("clean data produces an empty issue tibble", {
  built <- build_fixtures()$built
  issues <- validate_sdtm(built$sdtm, spec_synth01)
  expect_equal(nrow(issues), 0)
  expect_s3_class(issues, "tbl_df")
  expect_error(stop_on_error(issues, "meta"), NA)
})


# Exact-name battery + spec-driven checks -------------------------------
# Combined corruptions in one build; the union of fired checks must equal
# the expected set exactly - a check renaming itself shows up here.

test_that("the corruption battery fires exactly the expected check names", {
  fx <- build_fixtures()
  built <- fx$built

  domains <- built$sdtm
  domains$AE$AESTDY[2] <- 0L                       # study-day-zero
  domains$VS <- domains$VS[, setdiff(names(domains$VS), "VSTESTCD")]  # required-vars
  domains$AE <- domains$AE[-1, ]                   # related-parent-orphan

  sdtm_issues <- validate_sdtm(domains, spec_synth01)
  expect_setequal(
    sdtm_issues$check,
    c("study-day-zero", "required-vars", "related-parent-orphan")
  )
  expect_true(all(sdtm_issues$severity == "ERROR"))
})

test_that("built CT values that are not spec-declared are caught", {
  fx <- build_fixtures()
  built <- fx$built

  domains <- built$sdtm
  domains$AE$AEOUT[domains$AE$AEOUT == "FATAL"][1] <- "UNRESOLVED"
  issues <- validate_sdtm(domains, spec_synth01)
  expect_true("ct-value-not-in-spec" %in% issues$check)
  expect_true(all(issues$domain[issues$check == "ct-value-not-in-spec"] == "AE"))

  # SEX's decode default ("U") is allowed without a codelist row
  expect_false("ct-value-not-in-spec" %in%
                 issues$check[issues$domain == "DM"])
})

test_that("an ADaM parameter missing from spec$bds trips the coverage check", {
  fx <- build_fixtures()
  built <- fx$built

  # a spec whose ADVS block forgot TEMP
  spec2 <- spec_synth01
  spec2$bds <- spec2$bds[!(spec2$bds$domain == "ADVS" &
                             spec2$bds$paramcd == "TEMP"), ]

  # derive against the trimmed spec: TEMP loses its ranges/order
  advs <- derive_advs(built$sdtm$VS, built$adam$ADSL, spec2)
  issues <- validate_adam(built$adam$ADSL, built$adam$ADAE, advs,
                          built$adam$ADLB, built$sdtm$DM, built$sdtm$DS,
                          built$sdtm$AE, built$sdtm$VS, built$sdtm$LB,
                          built$sdtm$SUPPAE, spec2)
  expect_true("advs-param-not-in-spec" %in% issues$check)
  expect_true("TEMP" %in% issues$detail[issues$check == "advs-param-not-in-spec"])
})

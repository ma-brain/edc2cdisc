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
                          built$sdtm$SUPPAE)
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
                          built$sdtm$SUPPAE)
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
                          built$sdtm$SUPPAE)
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

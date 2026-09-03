# ============================================================================
# Title:   Generator reproducibility guard
# Purpose: The project's core claim - the extract is stochastic but seeded -
#          as a test. Regenerating with the committed seed must reproduce
#          data/rave/ byte-for-byte.
#
#          Sourcing 05_generate_rave_extract.R REGENERATES the extract (it
#          ends in a bare generate_rave_extract() call, mirroring how the
#          SDTM scripts run on source()). So the script is sourced into a
#          sandbox environment whose paths$rave points at a scratch
#          directory: the on-source regeneration lands there, and it is
#          compared against the untouched data/rave/ on disk. The real
#          extract is never written by a test.
# ============================================================================

test_that("the generator reproduces data/rave byte-for-byte from the seed", {
  scratch <- file.path(tempdir(), "ravesdtm-generator-test")
  on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)

  gen_env <- new.env(parent = globalenv())
  gen_env$paths     <- list(rave = scratch)   # redirect the on-source write
  gen_env$RAVE_SEED <- RAVE_SEED
  gen_env$RAVE_N    <- RAVE_N
  suppressMessages(
    eval(parse(file.path(.root, "05_generate_rave_extract.R")), gen_env)
  )

  generated <- list.files(scratch)
  expect_setequal(generated, list.files(paths$rave))
  expect_gt(length(generated), 0)

  for (f in generated) {
    expect_identical(
      readLines(file.path(scratch, f), warn = FALSE),
      readLines(file.path(paths$rave, f), warn = FALSE),
      label = sprintf("data/rave/%s", f)
    )
  }
})

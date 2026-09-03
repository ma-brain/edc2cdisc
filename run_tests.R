# ============================================================================
# Title:   Run the output guard tests
# Purpose: Verify data/ against the committed references in tests/reference.
#          Builds nothing - run `Rscript run_all.R` first if data/ is stale.
# Run:     Rscript run_tests.R
# Note:    Exits non-zero on any failure, so CI or a pre-commit hook can
#          call it directly.
# ============================================================================

results <- testthat::test_dir("tests", reporter = "summary")

n_bad <- sum(c(results$failed, results$error) > 0)
if (n_bad > 0) {
  message("run_tests.R: ", n_bad, " failing test file result(s)")
  quit(status = 1L)
}
message("run_tests.R: all output guards passed")

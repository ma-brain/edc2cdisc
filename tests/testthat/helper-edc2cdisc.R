# ============================================================================
# Title:   Shared test fixture - one deterministic build per session
# Purpose: The meta-tests and mapper tests all need the same built pipeline
#          output. Building per test was the suite's dominant wall-clock
#          cost, so the build runs once per study and is shared. R's
#          copy-on-modify keeps the cache pristine under the suite's
#          corrupt-a-copy test style (a $<- or [<- on a data frame with
#          more than one reference copies first) - test-helper-cache.R
#          pins that contract so a future R change fails loudly, not
#          silently.
# ============================================================================

.suite_cache <- new.env(parent = emptyenv())

built_suite <- function(study = c("SYNTH01", "SYNTH02")) {
  study <- match.arg(study)
  if (is.null(.suite_cache[[study]])) {
    ext <- file.path(tempdir(), "edc2cdisc-suite", study)
    dir.create(ext, recursive = TRUE, showWarnings = FALSE)
    suppressMessages(generate_rave_extract(out = ext, study = study))
    spec <- switch(study, SYNTH01 = spec_synth01, SYNTH02 = spec_synth02)
    .suite_cache[[study]] <- suppressMessages(build_all(ext, spec = spec))
  }
  .suite_cache[[study]]
}

# ============================================================================
# Title:   Refresh the committed reference outputs
# Purpose: Overwrite tests/reference/ with the current package pipeline's
#          output. This is a deliberate, reviewed act - NOT a fix for
#          failing tests. If the regression tests fail, first decide whether
#          the change is intended: read the diff (build_all() then compare
#          in R with waldo::compare), fix if it is a bug, and only then
#          refresh the baseline with this script.
# Run:     Rscript tests/update_reference.R   (from the project root)
# ============================================================================

# work from the source tree or an installed copy, whichever is available
root <- local({
  p <- normalizePath(getwd(), mustWork = TRUE)
  repeat {
    if (file.exists(file.path(p, "DESCRIPTION"))) break
    parent <- dirname(p)
    if (parent == p) stop("project root (DESCRIPTION) not found", call. = FALSE)
    p <- parent
  }
  p
})
if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(root, quiet = TRUE)
} else {
  library(edc2cdisc)
}

scratch <- file.path(tempdir(), "edc2cdisc-ref-update")
on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)

suppressMessages(generate_rave_extract(out = file.path(scratch, "rave")))
built <- build_all(file.path(scratch, "rave"))

for (layer in c("sdtm", "adam")) {
  datasets <- if (layer == "sdtm") built$sdtm else built$adam
  to <- file.path("tests", "reference", layer)
  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  for (nm in names(datasets)) {
    # write_rds (not saveRDS) so the refresh is byte-stable against the
    # committed baselines: only real value changes show up as a diff
    readr::write_rds(datasets[[nm]], file.path(to, paste0(tolower(nm), ".rds")))
  }
  message(sprintf("update_reference: %d %s dataset(s) written",
                  length(datasets), layer))
}

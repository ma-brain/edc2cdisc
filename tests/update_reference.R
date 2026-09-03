# ============================================================================
# Title:   Refresh the committed reference outputs
# Purpose: Overwrite tests/reference/ with the current data/ outputs. This is
#          a deliberate, reviewed act - NOT a fix for failing tests. If the
#          guards fail, first decide whether the change is intended: read the
#          diff (run_all.R then compare in R with waldo::compare), fix if it
#          is a bug, and only then refresh the baseline with this script.
# Run:     Rscript tests/update_reference.R
# ============================================================================

root <- local({
  p <- normalizePath(getwd(), mustWork = TRUE)
  repeat {
    if (file.exists(file.path(p, "00_setup.R"))) break
    parent <- dirname(p)
    if (parent == p) stop("project root (00_setup.R) not found from ", getwd(),
                          call. = FALSE)
    p <- parent
  }
  p
})

for (layer in c("sdtm", "adam")) {
  from <- file.path(root, "data", layer)
  to   <- file.path(root, "tests", "reference", layer)
  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(list.files(from, pattern = "\\.rds$", full.names = TRUE), to,
                  overwrite = TRUE)
  message(sprintf("update_reference: %d %s dataset(s) copied", sum(ok), layer))
}

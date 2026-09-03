# ============================================================================
# Title:   SDTM output guard
# Purpose: Every SDTM domain in data/sdtm/ must reproduce the committed
#          reference. Any value, column, type or label change trips this -
#          which is the point: such a change must be a deliberate reference
#          update (tests/update_reference.R), reviewed as a diff.
# ============================================================================

sdtm_domains <- c("DM", "EX", "VS", "AE", "CM", "DS", "SV", "LB", "MH",
                  "SUPPDM", "SUPPAE", "SUPPEX", "CO", "RELREC")

for (d in sdtm_domains) {
  test_that(sprintf("SDTM %s matches the committed reference", d), {
    df <- readRDS(file.path(paths$sdtm, paste0(tolower(d), ".rds")))
    expect_gt(nrow(df), 0)
    expect_matches_reference(df, "sdtm", d)
  })
}

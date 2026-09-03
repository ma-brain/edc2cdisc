# ============================================================================
# Title:   SDTM RELREC - Related Records
# Purpose: Pair each concomitant medication with the adverse event it
#          treated. The generator stamps the AE log line on the CM record
#          (CMSPID) where the indication matches; this script resolves the
#          pairs to their --SEQ keys and hands them to make_relrec().
# Input:   data/sdtm/{ae,cm}.rds
# Output:  data/sdtm/relrec.rds, data/sdtm/xpt/relrec.xpt
# ============================================================================

ae <- read_rds(file.path(paths$sdtm, "ae.rds"))
cm <- read_rds(file.path(paths$sdtm, "cm.rds"))

linked_cm <- cm |>
  filter(!is.na(CMSPID), CMSPID != "")

# Every stamped CM must resolve to a real AE log line, and the CM's
# indication must be the event it points at - a link is a claim about the
# data, so both halves are checked here, loudly.
orphan <- linked_cm |>
  anti_join(ae |> select(USUBJID, AESPID), by = c("USUBJID", "CMSPID" = "AESPID"))
if (nrow(orphan) > 0) {
  stop(sprintf("RELREC: %d CM record(s) with CMSPID not in AE (e.g. %s %s)",
               nrow(orphan), orphan$USUBJID[1], orphan$CMSPID[1]),
       call. = FALSE)
}
mismatch <- linked_cm |>
  inner_join(ae |> select(USUBJID, AESPID, AEDECOD),
             by = c("USUBJID", "CMSPID" = "AESPID")) |>
  filter(str_to_upper(CMINDC) != str_to_upper(AEDECOD))
if (nrow(mismatch) > 0) {
  stop(sprintf(paste("RELREC: %d linked CM record(s) whose CMINDC does not",
                     "match the linked AE's term"), nrow(mismatch)),
       call. = FALSE)
}

relrec <- make_relrec(
  linked_cm |>
    inner_join(ae |> select(USUBJID, AESPID, AESEQ),
               by = c("USUBJID", "CMSPID" = "AESPID")) |>
    transmute(
      USUBJID,
      RDOMAIN_1  = "CM", IDVAR_1 = "CMSEQ", IDVARVAL_1 = CMSEQ,
      RDOMAIN_2  = "AE", IDVAR_2 = "AESEQ", IDVARVAL_2 = AESEQ
    )
)

write_sdtm(relrec, "RELREC")

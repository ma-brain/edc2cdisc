# ============================================================================
# Title:   SDTM SV - Subject Visits
# Purpose: One row per subject per actual visit, built from the CRF header
#          block (Folder / FolderName / TargetDays / RecordDate) that Rave
#          stamps on every form - no dedicated visit CRF is collected.
# Input:   data/rave/{DM,VS,EX,DS}.csv, subject_ref + visit_map + visit_targets
# Output:  data/sdtm/sv.rds, data/sdtm/xpt/sv.xpt
# Note:    VISITNUM here is the reference the other scheduled-visit domains are
#          reconciled against in 22_validate.R.
# ============================================================================

# Forms that sit on a real scheduled folder. Log forms (AE, CM) live on
# Folder = "LOG" and never contribute a visit.
sv_source_forms <- c("DM", "VS", "EX", "DS", "LB")

sv_headers <- sv_source_forms |>
  map(\(f) read_rave_form(f) |>
        transmute(
          USUBJID    = str_c(STUDYID, Subject, sep = "-"),
          Folder,
          FolderName,
          # TargetDays travels in the header block as a character offset
          TargetDays = suppressWarnings(as.integer(TargetDays)),
          RECDT      = str_sub(RecordDate, 1, 10),
          FORMOID    = f
        )) |>
  list_rbind() |>
  # keep only scheduled folders with a real record date
  filter(Folder %in% visit_map$Folder, !is.na(RECDT), RECDT != "")

# One visit = the span of record dates for that subject on that folder.
sv <- sv_headers |>
  summarise(
    SVSTDTC    = min_dtc(RECDT),
    SVENDTC    = max_dtc(RECDT),
    TargetDays = first(TargetDays),
    .by = c(USUBJID, Folder)
  ) |>
  left_join(visit_map, by = "Folder") |>
  left_join(subject_ref, by = "USUBJID") |>
  mutate(
    STUDYID = .env$STUDYID,
    DOMAIN  = "SV",
    # Planned study day of the visit: TargetDays is measured from first dose
    # (BASE = TargetDays 0), so apply the same no-day-0 rule as derive_dy().
    VISITDY = if_else(TargetDays >= 0L, TargetDays + 1L, TargetDays),
    SVSTDY  = derive_dy(SVSTDTC, RFSTDTC),
    SVENDY  = derive_dy(SVENDTC, RFSTDTC)
  ) |>
  arrange(USUBJID, VISITNUM) |>
  transmute(
    STUDYID, DOMAIN, USUBJID,
    VISITNUM, VISIT, VISITDY, EPOCH,
    SVSTDTC, SVENDTC, SVSTDY, SVENDY
  ) |>
  apply_labels(c(
    STUDYID  = "Study Identifier",
    DOMAIN   = "Domain Abbreviation",
    USUBJID  = "Unique Subject Identifier",
    VISITNUM = "Visit Number",
    VISIT    = "Visit Name",
    VISITDY  = "Planned Study Day of Visit",
    EPOCH    = "Epoch",
    SVSTDTC  = "Start Date/Time of Visit",
    SVENDTC  = "End Date/Time of Visit",
    SVSTDY   = "Study Day of Start of Visit",
    SVENDY   = "Study Day of End of Visit"
  ))

write_sdtm(sv, "SV")

# ============================================================================
# Title:   Rave clinical view ingestion layer
# Purpose: Read Rave CSV clinical views defensively and attach CV metadata
# ============================================================================

#' The fixed CRF header block present in every Rave clinical view
rave_header_cols <- c(
  "userid", "projectid", "project", "studyid", "environmentName",
  "subjectId", "StudySiteId", "Subject", "siteid", "Site", "SiteNumber",
  "SiteGroup", "instanceId", "InstanceName", "InstanceRepeatNumber",
  "folderid", "Folder", "FolderName", "FolderSeq", "TargetDays",
  "DataPageId", "DataPageName", "PageRepeatNumber", "RecordDate",
  "RecordId", "recordposition", "RecordActive", "SaveTs", "MinCreated",
  "MaxUpdated"
  )

#' Read one Rave clinical view CSV
#'
#' Everything is read as character on purpose: Rave CSV exports quote all
#' values, mix numeric and text across CRF versions, and use partial dates
#' that no date parser should be allowed to guess at. Typing happens later,
#' explicitly, per variable.
#'
#' @param form_oid Form OID, e.g. "VS"
#' @param dir Directory holding the extract
#' @param active_only Drop soft-deleted records (RecordActive == "0")
read_rave_form <- function(form_oid, dir = paths$rave, active_only = TRUE) {
  path <- file.path(dir, paste0(form_oid, ".csv"))
  if (!file.exists(path)) {
    stop(sprintf("Clinical view not found: %s", path), call. = FALSE)
  }

  lines <- read_lines(path)

  # The EOF marker proves the stream was not truncated by an RWS timeout.
  # Its absence is a hard error, not a warning.
  if (!any(str_detect(lines, "^\"?EOF\"?\\s*$"))) {
    stop(sprintf("No EOF marker in %s - extract may be truncated", path),
         call. = FALSE)
  }
  lines <- lines[!str_detect(lines, "^\"?EOF\"?\\s*$")]

  raw <- read_csv(
    I(lines),
    col_types = cols(.default = col_character()),
    na = ""
  )

  missing_header <- setdiff(rave_header_cols, names(raw))
  if (length(missing_header) > 0) {
    warning(sprintf("%s: missing header columns: %s", form_oid,
                    str_flatten_comma(missing_header)))
  }

  out <- raw |>
    mutate(
      FORMOID = form_oid,
      RecordActive = as.integer(RecordActive),
      recordposition = as.integer(recordposition),
      PageRepeatNumber = as.integer(PageRepeatNumber),
      InstanceRepeatNumber = as.integer(InstanceRepeatNumber),
      FolderSeq = as.numeric(FolderSeq),
      across(c(SaveTs, MinCreated, MaxUpdated),
             \(x) as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
    )

  n_inactive <- sum(out$RecordActive == 0, na.rm = TRUE)
  if (active_only && n_inactive > 0) {
    message(sprintf("%s: dropping %d inactive record(s)", form_oid,
                    n_inactive))
    out <- filter(out, RecordActive == 1)
  }

  out
}

#' Read the Clinical View Metadata data dictionary
read_cv_metadata <- function(dir = paths$rave) {
  lines <- read_lines(file.path(dir, "ClinicalViewMetadata.csv"))
  lines <- lines[!str_detect(lines, "^\"?EOF\"?\\s*$")]
  read_csv(I(lines), col_types = cols(.default = col_character()), na = "") |>
    mutate(
      VariableOrdinal = as.integer(VariableOrdinal),
      Length = as.integer(Length)
    )
}

#' Read the codelist reference
read_codelists <- function(dir = paths$rave) {
  lines <- read_lines(file.path(dir, "CodeLists.csv"))
  lines <- lines[!str_detect(lines, "^\"?EOF\"?\\s*$")]
  read_csv(I(lines), col_types = cols(.default = col_character()), na = "")
  }

#' Attach CV metadata labels to a clinical view
label_from_cv_metadata <- function(data, form_oid, meta) {
  lbl <- meta |>
    filter(FormOID == form_oid, VariableName %in% names(data)) |>
    select(VariableName, VariableLabel) |>
    deframe() |>
    as.list()
  set_variable_labels(data, .labels = lbl, .strict = FALSE)
  }

#' Safely pull a column that may not exist in this form
col_or_na <- function(data, name) {
  if (name %in% names(data)) data[[name]] else rep(NA_character_, nrow(data))
  }

#' Load every form in the extract at once
read_rave_extract <- function(form_oids = c("DM", "VS", "LB", "AE", "CM", "EX", "DS"),
                              dir = paths$rave) {
  meta <- read_cv_metadata(dir)
  form_oids |>
    set_names() |>
    map(\(f) label_from_cv_metadata(read_rave_form(f, dir), f, meta))
  }

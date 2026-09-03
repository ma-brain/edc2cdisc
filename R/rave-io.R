# ============================================================================
# Title:   Clinical view ingestion layer
# Purpose: Read EDC clinical view CSVs defensively and attach CV metadata
# ============================================================================

#' The fixed CRF header block present in every clinical view extract
#'
#' @format A character vector of the 30 header column names.
#' @export
rave_header_cols <- c(
  "userid", "projectid", "project", "studyid", "environmentName",
  "subjectId", "StudySiteId", "Subject", "siteid", "Site", "SiteNumber",
  "SiteGroup", "instanceId", "InstanceName", "InstanceRepeatNumber",
  "folderid", "Folder", "FolderName", "FolderSeq", "TargetDays",
  "DataPageId", "DataPageName", "PageRepeatNumber", "RecordDate",
  "RecordId", "recordposition", "RecordActive", "SaveTs", "MinCreated",
  "MaxUpdated"
  )

#' Read one clinical view CSV
#'
#' Everything is read as character on purpose: EDC CSV exports quote all
#' values, mix numeric and text across CRF versions, and use partial dates
#' that no date parser should be allowed to guess at. Typing happens later,
#' explicitly, per variable.
#'
#' The file must end with an `EOF` marker row: its absence means the stream
#' was truncated by an API timeout, which is a hard error, not a warning.
#'
#' @param form_oid Form OID, e.g. "VS"
#' @param dir Directory holding the extract
#' @param active_only Drop soft-deleted records (RecordActive == "0")
#' @return The form as a tibble: all columns character on read, with the
#'   header bookkeeping columns typed (flags integer, timestamps POSIXct)
#'   and FORMOID attached.
#' @export
#' @examples
#' ext <- file.path(tempdir(), "extract-io")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' vs <- read_clinical_view("VS", ext)
#' nrow(vs)
#' table(vs$RecordActive)
read_clinical_view <- function(form_oid, dir, active_only = TRUE) {
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
#'
#' @param dir Directory holding the extract
#' @return The metadata as a tibble (one row per form variable).
#' @export
read_cv_metadata <- function(dir) {
  lines <- read_lines(file.path(dir, "ClinicalViewMetadata.csv"))
  lines <- lines[!str_detect(lines, "^\"?EOF\"?\\s*$")]
  read_csv(I(lines), col_types = cols(.default = col_character()), na = "") |>
    mutate(
      VariableOrdinal = as.integer(VariableOrdinal),
      Length = as.integer(Length)
    )
}

#' Read the codelist reference
#'
#' @param dir Directory holding the extract
#' @return The codelists as a long tibble (CodeListOID, CodedValue, Decode,
#'   Ordinal).
#' @export
read_codelists <- function(dir) {
  lines <- read_lines(file.path(dir, "CodeLists.csv"))
  lines <- lines[!str_detect(lines, "^\"?EOF\"?\\s*$")]
  read_csv(I(lines), col_types = cols(.default = col_character()), na = "")
  }

#' Attach CV metadata labels to a clinical view
#'
#' @param data A clinical view tibble
#' @param form_oid Form OID the view was read for
#' @param meta Metadata tibble from [read_cv_metadata()]
#' @return `data` with variable labels set from the metadata.
#' @export
label_from_cv_metadata <- function(data, form_oid, meta) {
  lbl <- meta |>
    filter(FormOID == form_oid, VariableName %in% names(data)) |>
    select(VariableName, VariableLabel) |>
    deframe() |>
    as.list()
  set_variable_labels(data, .labels = lbl, .strict = FALSE)
  }

#' Safely pull a column that may not exist in this form
#'
#' @param data A data frame
#' @param name Column name
#' @return The column, or a vector of NA_character_ when absent.
#' @export
col_or_na <- function(data, name) {
  if (name %in% names(data)) data[[name]] else rep(NA_character_, nrow(data))
  }

#' Load every form in the extract at once
#'
#' @param form_oids Form OIDs to read
#' @param dir Directory holding the extract
#' @return A named list of labelled clinical view tibbles.
#' @export
#' @examples
#' ext <- file.path(tempdir(), "extract-io2")
#' \dontshow{
#' suppressMessages(generate_rave_extract(out = ext))
#' }
#' forms <- suppressMessages(read_rave_extract(dir = ext))
#' names(forms)
read_rave_extract <- function(form_oids = c("DM", "VS", "LB", "AE", "CM",
                                            "EX", "DS", "MH"),
                              dir) {
  meta <- read_cv_metadata(dir)
  form_oids |>
    set_names() |>
    map(\(f) label_from_cv_metadata(read_clinical_view(f, dir), f, meta))
  }

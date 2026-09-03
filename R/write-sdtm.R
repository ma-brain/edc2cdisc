# ============================================================================
# Title:   SDTM dataset writer
# ============================================================================

#' Write an SDTM/ADaM dataset as RDS and SAS v5 transport
#'
#' Guards the XPT v5 constraints (variable names <= 8 characters, labels
#' <= 40 characters) before writing, then writes both formats.
#'
#' @param data A data frame with variable labels attached
#' @param domain Domain name, used for the file name and XPT member name
#' @param rds_dir,xpt_dir Output directories; both are created if missing
#' @return `data`, invisibly.
#' @export
#' @examples
#' out <- file.path(tempdir(), "sdtm")
#' dm <- data.frame(STUDYID = "3021", USUBJID = "3021-101-001")
#' write_sdtm(dm, "DM", rds_dir = out, xpt_dir = file.path(out, "xpt"))
#' list.files(out, recursive = TRUE)
write_sdtm <- function(data, domain, rds_dir, xpt_dir) {
  long_names <- names(data)[str_length(names(data)) > 8]
  if (length(long_names) > 0) {
    stop(sprintf("%s: variable names longer than 8 characters: %s",
                 domain, str_flatten_comma(long_names)), call. = FALSE)
  }
  long_labels <- keep(var_label(data), \(l) !is.null(l) && str_length(l) > 40)
  if (length(long_labels) > 0) {
    warning(sprintf("%s: labels longer than 40 characters: %s", domain,
                    str_flatten_comma(names(long_labels))))
  }

  dir.create(rds_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(xpt_dir, recursive = TRUE, showWarnings = FALSE)

  write_rds(data, file.path(rds_dir, paste0(str_to_lower(domain), ".rds")))
  write_xpt(data, file.path(xpt_dir, paste0(str_to_lower(domain), ".xpt")),
            version = 5, name = str_to_lower(domain))
  invisible(data)
}

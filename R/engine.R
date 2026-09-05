# ============================================================================
# Title:   The mapping engine
# Purpose: Turn a spec's variables rows into columns. Tiny on purpose: every
#          transform except `derivation` is a packaged helper; a derivation
#          is a named function from the registry (see derivations.R).
# ============================================================================

#' Map one domain's variables through the spec table
#'
#' Applies the `transform` vocabulary row by row, in table order, so later
#' rows see columns created by earlier ones (DM's ACTARMCD mirrors the
#' derived ARMCD, DMDY uses the derived DMDTC, and so on).
#'
#' @param data The in-flight source frame: raw clinical view columns plus
#'   whatever glue columns the mapper joined on before calling this.
#' @param spec A `study_spec`
#' @param rows The variables-table rows for one domain, in order (usually
#'   `filter(spec$variables, domain == "XX")`)
#' @param derivations Additional named derivation functions with signature
#'   `function(data, spec, row)`. Omit to use `spec$derivations`; pass a
#'   list to override. They extend the shared registry.
#' @details Derivations receive the spec row they run for: a study's
#'   collected source for a derivation-mapped variable must be read
#'   through `row$crf_field` (or `row$aux`), never a literal column name.
#'   That is what keeps "a new study is a spec change" true for the
#'   derivation layer.
#' @return A tibble with exactly the rows' variables, in table order.
#' @keywords internal
map_variables <- function(data, spec, rows, derivations = NULL) {
  if (is.null(derivations)) {
    derivations <- spec$derivations
    if (is.null(derivations)) {
      derivations <- list()
    }
  }
  registry <- c(.derivations, derivations)

  out <- data[, 0]           # zero-column frame carrying row.count
  work <- data               # accumulates derived columns as rows apply
  for (i in seq_len(nrow(rows))) {
    row <- rows[i, ]
    x   <- row$crf_field
    val <- switch(row$transform,
      rename       = work[[x]],
      character    = as.character(work[[x]]),
      decode       = {
        mapped <- check_ct(work[[x]], ct_lookup(spec, row$ref), row$variable)
        if (is.na(row$default)) mapped else coalesce(mapped, row$default)
      },
      dtc          = rave_dtc(
        work[[str_c(x, "_YYYY")]],
        work[[str_c(x, "_MM")]],
        work[[str_c(x, "_DD")]],
        time = if (is.na(row$aux)) NULL else work[[row$aux]]
      ),
      yn           = yn(work[[x]]),
      numeric      = suppressWarnings(as.numeric(work[[x]])),
      verbatim     = clean_verbatim(work[[x]]),
      squish       = str_squish(work[[x]]),
      upper        = str_to_upper(work[[x]]),
      upper_squish = str_to_upper(str_squish(work[[x]])),
      constant     = rep(row$value, nrow(work)),
      derivation   = {
        fn <- registry[[row$ref]]
        if (is.null(fn)) {
          stop(sprintf("map_variables: unknown derivation '%s'", row$ref),
               call. = FALSE)
        }
        fn(work, spec, row)
      },
      stop(sprintf("map_variables: unhandled transform '%s'",
                   row$transform), call. = FALSE)
    )
    out[[row$variable]] <- val
    work[[row$variable]] <- val
  }
  out
}

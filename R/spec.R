# ============================================================================
# Title:   Study specification object
# Purpose: One object holding everything that changes between studies:
#          study constants, sites, arms, visits, codelists, SUPP qualifiers,
#          unit conversions, form typing and the variable-level mapping table.
#          "A new study is a spec change" - within one CRF family.
# ============================================================================

#' The transform vocabulary the mapping engine knows
#'
#' `derivation` names a function from the derivation registry; everything
#' else names a direct column transform. See [new_study_spec()].
.transforms <- c(
  "rename", "decode", "dtc", "yn", "numeric", "verbatim", "squish",
  "upper", "upper_squish", "character", "constant", "derivation"
)

# The domains build_all() can produce; variables rows must name one of these
.sdtm_domains <- c("DM", "EX", "VS", "AE", "CM", "DS", "SV", "LB", "MH",
                   "SUPPDM", "SUPPAE", "SUPPEX", "CO", "RELREC")

# Trial design tables: optional constructor arguments, always present on
# the built spec so the builders can rely on their columns. Protocol fact,
# not collected data - see map_te()/map_ta()/map_ti()/map_ts()/map_tv().
.trial_defaults <- list(
  elements = tibble(ETCD = character(), ELEMENT = character(),
                    TESTRL = character(), TEENRL = character(),
                    TEDUR = character()),
  ta = tibble(ARMCD = character(), ETCD = character(), TAETORD = integer(),
              EPOCH = character(), TABRANCH = character(),
              TATRANS = character()),
  ie = tibble(IETESTCD = character(), IETEST = character(),
              IECAT = character()),
  ts = tibble(TSPARMCD = character(), TSPARM = character(),
              TSVAL = character(), TSVALNF = character())
)

#' Build and validate a study specification
#'
#' A `study_spec` is a list of tibbles. Each table carries one kind of
#' study-specific mapping configuration:
#'
#' * `study`     - study constants: `STUDYID`, `PROJECT`, `seed`, `n`,
#'   `age_min` / `age_max` (the bounds [validate_adam()] applies to AGE)
#' * `sites`     - site table: `SiteNumber`, `siteid`, `Site`, `SiteGroup`,
#'   `StudySiteId`, `COUNTRY`
#' * `arms`      - planned arm decode -> `ARMCD` / `ARM` (screen failures are
#'   not randomised and are handled by a derivation, not a row here)
#' * `visits`    - folder OID -> `VISITNUM` / `VISIT` / `EPOCH` /
#'   `TargetDays` (joined onto scheduled-visit domains)
#' * `codelists` - long controlled-terminology table: `ct`, `rave_decode`,
#'   `cdisc_term`. One `ct` group per mapped variable; unmapped collected
#'   values break the run loudly (see [check_ct()]).
#' * `supp`      - SUPP-- qualifier specs: `rdomain`, `idvar`, `qnam`,
#'   `qlabel`, `src` (raw column holding the value), `transform` (how to
#'   derive QVAL from `src`), `qorig`, `qeval`
#' * `units`     - analyte-aware conventional -> SI conversions for LB:
#'   `testcd`, `conv_from`, `conv_to`, `conv_factor`
#' * `forms`     - form typing: `form_oid`, `type` ("event" / "log"),
#'   `scheduled` (does the form sit on a real scheduled folder and thus
#'   contribute an SV visit?)
#' * `tests`     - per-test pivot specs for the findings domains VS/LB:
#'   `domain`, `field` (raw field OID), `testcd`, `test`, `cat`, `specimen`
#' * `bds`       - ADaM BDS parameter configuration: `domain` (ADVS / ADLB),
#'   `paramcd`, `paramn` (order), `anrlo`, `anrhi` (declared reference
#'   ranges; NA where a parameter has no absolute range)
#' * `variables` - the mapping table proper, one row per SDTM variable per
#'   domain: `domain`, `variable`, `crf_field`, `transform`, `ref`,
#'   `value` (for constants), `aux` (auxiliary raw column, e.g. the time
#'   companion of a --DTC), `default` (decode fallback). `transform`
#'   names a member of the fixed vocabulary; `derivation` rows name a
#'   registered derivation in `ref`.
#' * `elements`  - trial elements: `ETCD` (<=8 chars, unique), `ELEMENT`
#'   (<=40 chars), `TESTRL` / `TEENRL` (start/end rules), `TEDUR` (ISO 8601
#'   duration, e.g. `P2W`; NA allowed)
#' * `ta`        - planned arm x element order: `ARMCD`, `ETCD`, `TAETORD`
#'   (integer), `EPOCH`, `TABRANCH`, `TATRANS`
#' * `ie`        - inclusion/exclusion criteria: `IETESTCD`, `IETEST`,
#'   `IECAT` (INCLUSION / EXCLUSION)
#' * `ts`        - trial summary parameters: `TSPARMCD` (unique),
#'   `TSPARM`, `TSVAL`, `TSVALNF` - exactly one of TSVAL/TSVALNF filled
#'
#' The constructor validates references as well as shapes: every
#' `derivation` row's `ref` must resolve against the derivation registry
#' (the shared one plus `derivations`), every `decode` row's `ref` must
#' name a codelist the spec carries, every `supp$transform` must be one
#' `supp_transform()` knows, and every `variables$domain` must be a domain
#' the package can build. A spec that cannot be honoured fails here, not
#' halfway through a build.
#'
#' @param study,sites,arms,visits,codelists,supp,units,forms,tests,bds,variables
#'   Tibbles as described above.
#' @param elements,ta,ie,ts Optional trial design tibbles: `elements`
#'   (ETCD, ELEMENT, TESTRL, TEENRL, TEDUR), `ta` (ARMCD, ETCD, TAETORD,
#'   EPOCH, TABRANCH, TATRANS), `ie` (IETESTCD, IETEST, IECAT) and `ts`
#'   (TSPARMCD, TSPARM, TSVAL, TSVALNF). Missing ones default to zero-row
#'   tables; the trial design domains built from them (see
#'   [map_te()]) then come out empty.
#' @param derivations Optional named list of extra derivation functions
#'   (signature `function(data, spec, row)`). Stored on `spec$derivations`
#'   and read by [map_variables()] by default; an explicit
#'   `map_variables(derivations = )` argument still overrides. Declared
#'   here so the reference check can see them.
#' @return A `study_spec` object (a validated named list).
#' @export
new_study_spec <- function(study, sites, arms, visits, codelists,
                           supp, units, forms, tests, bds, variables,
                           elements = NULL, ta = NULL, ie = NULL, ts = NULL,
                           derivations = list()) {
  spec <- list(
    study     = study,
    sites     = sites,
    arms      = arms,
    visits    = visits,
    codelists = codelists,
    supp      = supp,
    units     = units,
    forms     = forms,
    tests     = tests,
    bds       = bds,
    variables = variables,
    elements  = elements,
    ta        = ta,
    ie        = ie,
    ts        = ts,
    derivations = derivations
  )

  # optional tables get their typed defaults before the required-column
  # loop below runs, so a user-supplied wrong shape still fails there
  for (nm in names(.trial_defaults)) {
    if (is.null(spec[[nm]])) spec[[nm]] <- .trial_defaults[[nm]]
  }

  required <- list(
    study     = c("STUDYID", "PROJECT", "seed", "n", "age_min", "age_max"),
    sites     = c("SiteNumber", "siteid", "Site", "SiteGroup", "StudySiteId",
                  "COUNTRY"),
    arms      = c("ARMCD_DECODE", "ARMCD", "ARM"),
    visits    = c("Folder", "VISITNUM", "VISIT", "EPOCH", "TargetDays"),
    codelists = c("ct", "rave_decode", "cdisc_term"),
    supp      = c("rdomain", "idvar", "qnam", "qlabel", "src", "transform",
                  "qorig", "qeval"),
    units     = c("testcd", "conv_from", "conv_to", "conv_factor"),
    forms     = c("form_oid", "type", "scheduled"),
    tests     = c("domain", "field", "testcd", "test", "cat", "specimen"),
    bds       = c("domain", "paramcd", "paramn", "anrlo", "anrhi"),
    variables = c("domain", "variable", "crf_field", "transform", "ref",
                  "value", "aux", "default"),
    elements  = c("ETCD", "ELEMENT", "TESTRL", "TEENRL", "TEDUR"),
    ta        = c("ARMCD", "ETCD", "TAETORD", "EPOCH", "TABRANCH", "TATRANS"),
    ie        = c("IETESTCD", "IETEST", "IECAT"),
    ts        = c("TSPARMCD", "TSPARM", "TSVAL", "TSVALNF")
  )
  for (nm in names(required)) {
    if (is.null(spec[[nm]]) || !is.data.frame(spec[[nm]])) {
      stop(sprintf("study spec: '%s' must be a data frame", nm), call. = FALSE)
    }
    missing_cols <- setdiff(required[[nm]], names(spec[[nm]]))
    if (length(missing_cols) > 0) {
      stop(sprintf("study spec: '%s' missing column(s): %s", nm,
                   str_flatten_comma(missing_cols)), call. = FALSE)
    }
  }

  bad_transform <- setdiff(unique(spec$variables$transform), .transforms)
  if (length(bad_transform) > 0) {
    stop(sprintf("study spec: unknown transform(s): %s",
                 str_flatten_comma(bad_transform)), call. = FALSE)
  }
  needs_ref <- spec$variables$transform == "derivation" &
    (is.na(spec$variables$ref) | spec$variables$ref == "")
  if (any(needs_ref)) {
    bad <- paste0(spec$variables$domain, "/", spec$variables$variable)[needs_ref]
    stop(sprintf(
      "study spec: derivation row(s) without a ref: %s",
      str_flatten_comma(bad)
    ), call. = FALSE)
  }

  # Reference checks: a spec naming something the package cannot resolve
  # must fail at construction, not halfway through a build.
  der_rows <- spec$variables$transform == "derivation"
  unknown_der <- setdiff(unique(spec$variables$ref[der_rows]),
                         c(names(.derivations), names(derivations)))
  if (length(unknown_der) > 0) {
    stop(sprintf("study spec: unknown derivation ref(s): %s",
                 str_flatten_comma(unknown_der)), call. = FALSE)
  }

  need_fld <- spec$variables$transform == "derivation" &
    spec$variables$ref %in% .derivations_need_field &
    (is.na(spec$variables$crf_field) | spec$variables$crf_field == "")
  if (any(need_fld)) {
    bad <- paste0(spec$variables$domain, "/", spec$variables$variable)[need_fld]
    stop(sprintf("study spec: derivation row(s) missing crf_field: %s",
                 str_flatten_comma(bad)), call. = FALSE)
  }

  dec_refs <- spec$variables$ref[spec$variables$transform == "decode"]
  bad_dec <- setdiff(unique(dec_refs), unique(spec$codelists$ct))
  if (length(bad_dec) > 0) {
    stop(sprintf("study spec: decode row(s) with no matching codelist: %s",
                 str_flatten_comma(bad_dec)), call. = FALSE)
  }

  bad_supp_tr <- setdiff(unique(spec$supp$transform), .supp_transforms)
  if (length(bad_supp_tr) > 0) {
    stop(sprintf("study spec: unsupported SUPP transform(s): %s",
                 str_flatten_comma(bad_supp_tr)), call. = FALSE)
  }

  bad_dom <- setdiff(unique(spec$variables$domain), .sdtm_domains)
  if (length(bad_dom) > 0) {
    stop(sprintf("study spec: variables for unbuildable domain(s): %s",
                 str_flatten_comma(bad_dom)), call. = FALSE)
  }

  dup_ct <- spec$codelists |>
    count(ct, rave_decode) |>
    filter(n > 1)
  if (nrow(dup_ct) > 0) {
    stop("study spec: duplicate codelist decode entries", call. = FALSE)
  }
  dup_qnam <- spec$supp |> count(rdomain, qnam) |> filter(n > 1)
  if (nrow(dup_qnam) > 0) {
    stop("study spec: duplicate SUPP qnam entries", call. = FALSE)
  }
  dup_bds <- spec$bds |> count(domain, paramcd) |> filter(n > 1)
  if (nrow(dup_bds) > 0) {
    stop("study spec: duplicate BDS parameter entries", call. = FALSE)
  }

  # Trial design: the protocol must resolve against the tables it names,
  # and the XPT v5 limits are enforced here, not at write time.
  if (nrow(spec$elements) > 0) {
    if (anyDuplicated(spec$elements$ETCD) > 0) {
      stop("study spec: duplicate ETCD in elements", call. = FALSE)
    }
    long_etcd <- spec$elements$ETCD[!is.na(spec$elements$ETCD) &
                                      nchar(spec$elements$ETCD) > 8]
    if (length(long_etcd) > 0) {
      stop(sprintf("study spec: ETCD longer than 8 characters: %s",
                   str_flatten_comma(long_etcd)), call. = FALSE)
    }
    long_elem <- spec$elements$ELEMENT[!is.na(spec$elements$ELEMENT) &
                                         nchar(spec$elements$ELEMENT) > 40]
    if (length(long_elem) > 0) {
      stop(sprintf("study spec: ELEMENT longer than 40 characters: %s",
                   str_flatten_comma(long_elem)), call. = FALSE)
    }
    bad_dur <- spec$elements$TEDUR[
      !is.na(spec$elements$TEDUR) &
        !str_detect(str_to_upper(spec$elements$TEDUR), "^P(\\d+[YMDW])+$")
    ]
    if (length(bad_dur) > 0) {
      stop(sprintf("study spec: TEDUR is not an ISO 8601 duration: %s",
                   str_flatten_comma(bad_dur)), call. = FALSE)
    }
  }
  if (nrow(spec$ta) > 0) {
    unknown_arm <- setdiff(unique(spec$ta$ARMCD), unique(spec$arms$ARMCD))
    if (length(unknown_arm) > 0) {
      stop(sprintf("study spec: ta: ARMCD not in spec$arms: %s",
                   str_flatten_comma(unknown_arm)), call. = FALSE)
    }
    # intra-table shape before cross-table references: a ta row naming no
    # element yet must still be called out as duplicated on its own terms
    dup_ta <- spec$ta |> count(ARMCD, TAETORD) |> filter(n > 1)
    if (nrow(dup_ta) > 0) {
      stop("study spec: duplicated ARMCD/TAETORD in ta", call. = FALSE)
    }
    unknown_etcd <- setdiff(unique(spec$ta$ETCD), unique(spec$elements$ETCD))
    if (length(unknown_etcd) > 0) {
      stop(sprintf("study spec: ta: ETCD not in spec$elements: %s",
                   str_flatten_comma(unknown_etcd)), call. = FALSE)
    }
  }
  if (nrow(spec$ie) > 0) {
    bad_cat <- setdiff(unique(spec$ie$IECAT), c("INCLUSION", "EXCLUSION"))
    if (length(bad_cat) > 0) {
      stop(sprintf("study spec: ie: IECAT not INCLUSION/EXCLUSION: %s",
                   str_flatten_comma(bad_cat)), call. = FALSE)
    }
  }
  if (nrow(spec$ts) > 0) {
    if (anyDuplicated(spec$ts$TSPARMCD) > 0) {
      stop("study spec: duplicate TSPARMCD in ts", call. = FALSE)
    }
    both <- !is.na(spec$ts$TSVAL) & spec$ts$TSVAL != "" &
      !is.na(spec$ts$TSVALNF) & spec$ts$TSVALNF != ""
    if (any(both)) {
      stop(sprintf("study spec: both TSVAL and TSVALNF filled for: %s",
                   str_flatten_comma(spec$ts$TSPARMCD[both])), call. = FALSE)
    }
    neither <- (is.na(spec$ts$TSVAL) | spec$ts$TSVAL == "") &
      (is.na(spec$ts$TSVALNF) | spec$ts$TSVALNF == "")
    if (any(neither)) {
      stop(sprintf("study spec: neither TSVAL nor TSVALNF filled for: %s",
                   str_flatten_comma(spec$ts$TSPARMCD[neither])),
           call. = FALSE)
    }
  }

  class(spec) <- c("study_spec", "list")
  spec
}

#' @export
print.study_spec <- function(x, ...) {
  cat(sprintf("study_spec: %s (%s)\n", x$study$STUDYID, x$study$PROJECT))
  cat(sprintf("  sites: %d   arms: %d   visits: %d   codelists: %d\n",
              nrow(x$sites), nrow(x$arms), nrow(x$visits),
              length(unique(x$codelists$ct))))
  cat(sprintf("  supp qualifiers: %d   forms: %d   variables mapped: %d\n",
              nrow(x$supp), nrow(x$forms), nrow(x$variables)))
  cat(sprintf("  trial design: elements %d   ta %d   ie %d   ts %d\n",
              nrow(x$elements), nrow(x$ta), nrow(x$ie), nrow(x$ts)))
  invisible(x)
}

#' Pull one controlled-terminology lookup from a study spec
#'
#' @param spec A `study_spec` (see [new_study_spec()])
#' @param ct The codelist name, e.g. "AEOUT"
#' @return A named character vector: collected decode -> CDISC term.
#' @export
#' @examples
#' ct_lookup(spec_synth01, "AESEV")
ct_lookup <- function(spec, ct) {
  rows <- spec$codelists[spec$codelists$ct == ct, ]
  if (nrow(rows) == 0) {
    stop(sprintf("study spec: no codelist '%s'", ct), call. = FALSE)
  }
  set_names(rows$cdisc_term, rows$rave_decode)
}

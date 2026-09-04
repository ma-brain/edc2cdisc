# ============================================================================
# Title:   define.xml stub + value-level metadata
# Purpose: Emit a define-XML 2.0-shaped document describing the SDTM
#          datasets: one ItemGroupDef per domain (with a leaf pointing at
#          the XPT), per-domain ItemDefs carrying the labels, datatypes and
#          lengths computed from the built data, KeySequences, curated
#          codelists, and value-level metadata for the findings domains.
# Note:    A stub by design - structurally close to define 2.0 (validated
#          only as well-formed XML), not P21-clean. Codelists carry the
#          values observed in this study; a submission define would declare
#          full CDISC CT instead.
# ============================================================================

#' Build a define.xml stub for the mapped SDTM domains
#'
#' @param domains Named list of mapped SDTM datasets
#' @param spec A `study_spec` (study name/id, codelists)
#' @param path Output file path, e.g. `data/sdtm/define.xml`
#' @return `path`, invisibly.
#' @export
build_define_xml <- function(domains, spec, path) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("build_define_xml() needs the xml2 package", call. = FALSE)
  }

  studyid <- spec$study$STUDYID
  project <- spec$study$PROJECT

  # Key sequences per domain
  key_spec <- list(
    DM     = c("STUDYID", "USUBJID"),
    EX     = c("STUDYID", "USUBJID", "EXSEQ"),
    VS     = c("STUDYID", "USUBJID", "VSSEQ"),
    AE     = c("STUDYID", "USUBJID", "AESEQ"),
    CM     = c("STUDYID", "USUBJID", "CMSEQ"),
    DS     = c("STUDYID", "USUBJID", "DSSEQ"),
    SV     = c("STUDYID", "USUBJID", "VISITNUM"),
    LB     = c("STUDYID", "USUBJID", "LBSEQ"),
    MH     = c("STUDYID", "USUBJID", "MHSEQ"),
    SUPPDM = c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "QNAM"),
    SUPPAE = c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "QNAM"),
    SUPPEX = c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "QNAM"),
    CO     = c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "COSEQ"),
    RELREC = c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "RELID")
  )

  # Dataset structure strings
  structure_spec <- list(
    DM     = "One record per subject",
    EX     = "One record per subject per dosing interval per visit",
    VS     = "One record per subject per vital sign per visit",
    AE     = "One record per subject per adverse event",
    CM     = "One record per subject per medication",
    DS     = "One record per subject per disposition event",
    SV     = "One record per subject per visit",
    LB     = "One record per subject per lab test per visit",
    MH     = "One record per subject per medical history event",
    SUPPDM = "One record per subject per SUPPDM variable",
    SUPPAE = "One record per subject per AE record per SUPPAE variable",
    SUPPEX = "One record per subject per EX record per SUPPEX variable",
    CO     = "One record per subject per comment",
    RELREC = "One record per linked record (two records per RELID)"
  )

  # Origins: collected (CRF) is the default for tabulations; the derived
  # and assigned variables are declared here. Suffix rules catch families.
  origin_suffix <- c(SEQ = "Derived", DY = "Derived", NRIND = "Derived",
                     BLFL = "Derived", ENRTPT = "Derived", ENRF = "Derived")
  origin_named <- c(
    STUDYID  = "Assigned", DOMAIN = "Assigned", COUNTRY = "Assigned",
    RFSTDTC  = "Derived", RFENDTC = "Derived", RFXSTDTC = "Derived",
    RFXENDTC = "Derived", RFPENDTC = "Derived", AGE = "Derived", AGEU = "Derived",
    AEDECOD  = "Derived", AEBODSYS = "Derived", CMDECOD = "Derived",
    MHDECOD  = "Derived", MHBODSYS = "Derived",
    AESPID   = "Assigned", CMSPID = "Assigned", QNAM = "Assigned",
    QLABEL   = "Assigned", QORIG = "Assigned", IDVAR = "Assigned",
    RELTYPE  = "Assigned", RELID = "Assigned"
  )

  var_origin <- function(var) {
    hit <- origin_suffix[str_ends(var, names(origin_suffix))]
    if (length(hit) > 0) return(hit[[1]])
    if (var %in% names(origin_named)) return(unname(origin_named[[var]]))
    "CRF"
  }

  # Datatypes: types are taken from the built data (derive_seq produces
  # integers, DY variables too).
  var_type <- function(x) {
    if (is.integer(x)) return("integer")
    if (is.numeric(x)) return("float")
    "text"
  }
  var_length <- function(x) {
    len <- suppressWarnings(max(nchar(as.character(x)), na.rm = TRUE))
    if (!is.finite(len)) len <- 1L
    len
  }

  # Curated codelists: the values observed across all domains carrying the
  # variable.
  codelist_vars <- c("AESEV", "AEREL", "AEACN", "AEOUT", "LBNRIND", "DTHFL",
                     "QNAM", "RDOMAIN", "RELTYPE", "DSDECOD")
  codelist_values <- map(set_names(codelist_vars), \(v) {
    vals <- unlist(imap(domains, \(df, d) if (v %in% names(df)) unique(df[[v]])),
                   use.names = FALSE)
    sort(unique(na.omit(vals[vals != ""])))
  })
  # A variable whose values are all blank (RELTYPE on record-level links)
  # has nothing to enumerate: an empty codelist is not emitted, and the
  # ItemDef carries no CodeListRef.
  codelist_values <- keep(codelist_values, \(v) length(v) > 0)

  # Document -------------------------------------------------------------------
  doc <- xml2::xml_new_document()
  xml2::xml_add_child(
    doc, "ODM", ODMVersion = "1.3.2", FileType = "Snapshot",
    FileOID = str_c(studyid, "-DEFINE-", format(Sys.Date(), "%Y%m%d")),
    CreationDateTime = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    `xmlns:def` = "http://www.cdisc.org/ns/def/v2.0",
    `xmlns:xlink` = "http://www.w3.org/1999/xlink",
    `xmlns` = "http://www.cdisc.org/ns/odm/v1.3",
    `def:Context` = "Submission"
  )
  # xml_add_child on a document returns the document, not the new root
  root <- xml2::xml_root(doc)

  study <- xml2::xml_add_child(root, "Study", OID = studyid)
  gv <- xml2::xml_add_child(study, "GlobalVariables")
  xml2::xml_add_child(gv, "StudyName", project)
  xml2::xml_add_child(gv, "StudyDescription",
                      "Synthetic EDC extract mapped to SDTM (training project)")
  xml2::xml_add_child(gv, "ProtocolName", studyid)
  mdv <- xml2::xml_add_child(
    study, "MetaDataVersion", OID = "MDV.1", Name = "SDTM define stub",
    Description = "Generated by edc2cdisc::build_define_xml()",
    `def:DefineVersion` = "2.0.0"
  )

  add_text <- function(parent, node, text) {
    xml2::xml_add_child(xml2::xml_add_child(parent, node), "TranslatedText",
                        text, `xml:lang` = "en")
    invisible(NULL)
  }

  # Codelists (ItemDefs reference these)
  for (v in names(codelist_values)) {
    cl <- xml2::xml_add_child(mdv, "CodeList", OID = str_c("CL.", v), Name = v,
                              DataType = "text")
    for (i in seq_along(codelist_values[[v]])) {
      xml2::xml_add_child(cl, "EnumeratedItem",
                          CodedValue = codelist_values[[v]][i],
                          OrderNumber = as.character(i))
    }
  }

  for (d in names(domains)) {
    df  <- domains[[d]]
    low <- str_to_lower(d)
    keys <- key_spec[[d]]

    igd <- xml2::xml_add_child(
      mdv, "ItemGroupDef",
      OID = str_c("IG.", d), Name = d,
      Repeating = if (d == "DM") "No" else "Yes",
      IsReferenceData = "No", SASDatasetName = d,
      Purpose = "Tabulation",
      `def:Structure` = structure_spec[[d]],
      `def:ArchiveLocationID` = str_c("LF.", d)
    )
    if (nchar(d) == 2) xml2::xml_set_attr(igd, "Domain", d)
    add_text(igd, "Description", structure_spec[[d]])
    leaf <- xml2::xml_add_child(igd, "def:leaf", ID = str_c("LF.", d),
                                `xlink:href` = str_c("xpt/", low, ".xpt"))
    xml2::xml_add_child(leaf, "def:title", str_c(low, ".xpt"))

    for (i in seq_along(df)) {
      var <- names(df)[i]
      x   <- df[[var]]
      oid <- str_c("IT.", d, ".", var)
      it  <- xml2::xml_add_child(
        mdv, "ItemDef", OID = oid, Name = var, SASFieldName = var,
        DataType = var_type(x),
        Length = as.character(max(var_length(x), 1L))
      )
      lbl <- var_label(df)[[var]]
      add_text(it, "Description", if (is.null(lbl)) var else lbl)
      if (var %in% names(codelist_values)) {
        xml2::xml_add_child(it, "CodeListRef", CodeListOID = str_c("CL.", var))
      }
      xml2::xml_add_child(it, "def:Origin", Type = var_origin(var))

      ref <- xml2::xml_add_child(igd, "ItemRef", ItemOID = oid,
                                 OrderNumber = as.character(i),
                                 Mandatory = if (var %in% keys) "Yes" else "No")
      if (var %in% keys) {
        xml2::xml_set_attr(ref, "KeySequence", as.character(which(keys == var)))
      }
    }
  }

  # Value-level metadata for the findings domains ------------------------------
  # One value-level definition per test code. VS gets label + unit; LB also
  # gets the observed reference ranges - ranges are collected data here
  # (sex-specific), so the distinct pairs are the value-level story.
  vs_params <- domains$VS |>
    distinct(.data$VSTESTCD, .data$VSTEST, .data$VSSTRESU) |>
    arrange(.data$VSTESTCD)
  lb_params <- domains$LB |>
    distinct(.data$LBTESTCD, .data$LBTEST, .data$LBSTRESU,
             .data$LBSTNRLO, .data$LBSTNRHI) |>
    arrange(.data$LBTESTCD, .data$LBSTNRLO)

  findings <- list(
    list(domain = "VS", var = "VSSTRESN", codevar = "VSTESTCD",
         namevar = "VSTEST", unitvar = "VSSTRESU",
         codes = sort(unique(vs_params$VSTESTCD)), params = vs_params),
    list(domain = "LB", var = "LBSTRESN", codevar = "LBTESTCD",
         namevar = "LBTEST", unitvar = "LBSTRESU",
         codes = sort(unique(lb_params$LBTESTCD)), params = lb_params)
  )

  for (f in findings) {
    d <- f$domain
    vld <- xml2::xml_add_child(mdv, "ValueListDef",
                               OID = str_c("VL.", d, ".", f$var))
    for (i in seq_along(f$codes)) {
      code <- f$codes[[i]]
      p    <- f$params |> filter(.data[[f$codevar]] == code)
      oid  <- str_c("IT.", d, ".", f$var, ".", code)
      desc <- str_c(p[[f$namevar]][1], " (", p[[f$unitvar]][1], ")")
      if (d == "LB") {
        rngs <- p |>
          filter(!is.na(LBSTNRLO)) |>
          unite("rng", LBSTNRLO, LBSTNRHI, sep = " to ") |>
          pull(.data$rng) |>
          unique()
        if (length(rngs) > 0) {
          desc <- str_c(desc, " [reference range: ",
                        str_flatten_comma(rngs, "; "), " ", p$LBSTRESU[1], "]")
        }
      }

      it <- xml2::xml_add_child(mdv, "ItemDef", OID = oid, Name = f$var,
                                SASFieldName = f$var, DataType = "float",
                                Length = "8")
      add_text(it, "Description", desc)
      xml2::xml_add_child(it, "def:Origin", Type = "CRF")

      wc_oid <- str_c("WC.", d, ".", f$codevar, ".", code)
      wc <- xml2::xml_add_child(mdv, "WhereClauseDef", OID = wc_oid)
      rc <- xml2::xml_add_child(wc, "RangeCheck", Comparator = "EQ",
                                SoftHard = "Soft",
                                `def:ItemOID` = str_c("IT.", d, ".", f$codevar))
      xml2::xml_add_child(rc, "CheckValue", code)

      ref <- xml2::xml_add_child(vld, "ItemRef", ItemOID = oid,
                                 OrderNumber = as.character(i), Mandatory = "Yes")
      xml2::xml_add_child(ref, "def:WhereClauseRef", WhereClauseOID = wc_oid)
    }
    # hook the value list onto the parent variable's ItemRef. The bracket
    # sits outside the quotes: inside them the predicate matches the literal
    # name "]VS", xml_find_first() returns xml_missing, and xml_add_child()
    # on xml_missing is a silent no-op - the value list would be orphaned.
    xpath <- str_c("//ItemGroupDef[@Name='", d,
                   "']/ItemRef[@ItemOID='IT.", d, ".", f$var, "']")
    parent_ref <- xml2::xml_find_first(mdv, xpath)
    xml2::xml_add_child(parent_ref, "def:ValueListRef",
                        ValueListOID = str_c("VL.", d, ".", f$var))
  }

  xml2::write_xml(doc, path)
  invisible(path)
}

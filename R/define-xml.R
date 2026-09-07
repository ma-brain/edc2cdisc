# ============================================================================
# Title:   define.xml (Define-XML 2.0) + value-level metadata
# Purpose: Emit a Define-XML 2.0.0 document describing the SDTM datasets:
#          one ItemGroupDef per domain (class, structure, keys, archive leaf
#          pointing at the XPT), per-domain ItemDefs carrying labels,
#          datatypes, lengths and origins, curated codelists, value-level
#          metadata for the findings domains, and - via the method/comment
#          definitions - the derivations behind the computed variables.
# Note:    Validated in the test suite against the canonical CDISC 2.0
#          schema (inst/schema/); codelists carry the values observed in
#          this study, and a submission define would declare full CDISC CT.
# ============================================================================

# The canonical CDISC Define-XML 2.0 schema set, vendored under
# inst/schema/ (xml.xsd carries one patched schemaLocation so validation
# never touches the network). The tests validate the built document
# against it - the schema is the oracle for this builder.
.define_schema_path <- function() {
  system.file("schema/define/2.0/define2-0-0.xsd", package = "edc2cdisc")
}

#' Build a define.xml (Define-XML 2.0) for the mapped SDTM domains
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
    QS     = c("STUDYID", "USUBJID", "QSSEQ"),
    PE     = c("STUDYID", "USUBJID", "PESEQ"),
    EG     = c("STUDYID", "USUBJID", "EGSEQ"),
    SE     = c("STUDYID", "USUBJID", "SESEQ"),
    SUPPDM = c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "QNAM"),
    SUPPAE = c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "QNAM"),
    SUPPEX = c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "QNAM"),
    SUPPPE = c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "QNAM"),
    SUPPMH = c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "QNAM"),
    SUPPVS = c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "QNAM"),
    CO     = c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "COSEQ"),
    RELREC = c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "RELID"),
    TA     = c("STUDYID", "ARMCD", "TAETORD"),
    TE     = c("STUDYID", "ETCD"),
    TI     = c("STUDYID", "IETESTCD"),
    TV     = c("STUDYID", "VISITNUM"),
    TS     = c("STUDYID", "TSPARMCD")
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
    QS     = "One record per subject per questionnaire item per visit",
    PE     = "One record per subject per body system per visit",
    EG     = "One record per subject per ECG test per visit",
    SE     = "One record per subject per element",
    SUPPDM = "One record per subject per SUPPDM variable",
    SUPPAE = "One record per subject per AE record per SUPPAE variable",
    SUPPEX = "One record per subject per EX record per SUPPEX variable",
    SUPPPE = "One record per subject per PE record per SUPPPE variable",
    SUPPMH = "One record per subject per MH record per SUPPMH variable",
    SUPPVS = "One record per subject per VS record per SUPPVS variable",
    CO     = "One record per subject per comment",
    RELREC = "One record per linked record (two records per RELID)",
    TA     = "One record per arm per element",
    TE     = "One record per trial element",
    TI     = "One record per inclusion/exclusion criterion",
    TV     = "One record per planned visit",
    TS     = "One record per trial summary parameter"
  )

  # The SDTM class of each dataset - the schema leaves def:Class optional,
  # but P21 and every reviewing statistician require it
  class_spec <- list(
    DM = "SPECIAL PURPOSE", SV = "SPECIAL PURPOSE", SE = "SPECIAL PURPOSE",
    RELREC = "SPECIAL PURPOSE", CO = "SPECIAL PURPOSE",
    SUPPDM = "SPECIAL PURPOSE", SUPPAE = "SPECIAL PURPOSE",
    SUPPEX = "SPECIAL PURPOSE", SUPPPE = "SPECIAL PURPOSE",
    SUPPMH = "SPECIAL PURPOSE", SUPPVS = "SPECIAL PURPOSE",
    TA = "TRIAL DESIGN", TE = "TRIAL DESIGN", TI = "TRIAL DESIGN",
    TV = "TRIAL DESIGN", TS = "TRIAL DESIGN",
    EX = "INTERVENTIONS", CM = "INTERVENTIONS",
    AE = "EVENTS", DS = "EVENTS", MH = "EVENTS",
    VS = "FINDINGS", LB = "FINDINGS", QS = "FINDINGS",
    PE = "FINDINGS", EG = "FINDINGS"
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
    RELTYPE  = "Assigned", RELID = "Assigned",
    ETCD = "Protocol", ELEMENT = "Protocol", TESTRL = "Protocol",
    TEENRL = "Protocol", TEDUR = "Protocol", TAETORD = "Protocol",
    TABRANCH = "Protocol", TATRANS = "Protocol", IETESTCD = "Protocol",
    IETEST = "Protocol", IECAT = "Protocol", TSPARMCD = "Protocol",
    TSPARM = "Protocol", TSVAL = "Protocol", TSVALNF = "Protocol"
  )

  # Names shared with collected domains get a per-domain override: TA's
  # ARMCD is protocol fact, DM's stays CRF/Assigned as declared above.
  origin_by_domain <- list(
    TA = c(ARMCD = "Protocol", ARM = "Protocol", EPOCH = "Protocol"),
    TV = c(VISITNUM = "Protocol", VISIT = "Protocol")
  )

  var_origin <- function(var, domain) {
    dom_hit <- origin_by_domain[[domain]][var]
    if (length(dom_hit) == 1 && !is.na(dom_hit)) return(unname(dom_hit))
    hit <- origin_suffix[str_ends(var, names(origin_suffix))]
    if (length(hit) > 0) return(unname(hit[[1]]))
    if (var %in% names(origin_named)) return(unname(origin_named[[var]]))
    "CRF"
  }

  # Datatypes: -DTC columns are date or datetime depending on whether the
  # collected values actually carry a time part; -DY and -SEQ are integer;
  # other numerics float; everything else text. Types are taken from the
  # built data (derive_seq produces integers, DY variables too).
  var_datatype <- function(var, x) {
    if (is.integer(x)) return("integer")
    if (is.numeric(x)) return("float")
    if (str_ends(var, "DTC") && any(str_detect(x, "T"), na.rm = TRUE)) {
      return("datetime")
    }
    if (str_ends(var, "DTC")) return("date")
    "text"
  }
  var_length <- function(x) {
    len <- suppressWarnings(max(nchar(as.character(x)), na.rm = TRUE))
    if (!is.finite(len)) len <- 1L
    len
  }
  # Length is only defined for text/integer/float; SignificantDigits counts
  # the observed decimal places of a float. Returns the attribute list so
  # date/datetime ItemDefs simply omit what ODM does not define for them.
  item_type_attrs <- function(var, x) {
    dtype <- var_datatype(var, x)
    if (dtype %in% c("date", "datetime")) return(list(DataType = dtype))
    attrs <- list(DataType = dtype,
                  Length = as.character(max(var_length(x), 1L)))
    if (dtype == "float") {
      vals <- as.character(x)[!is.na(x)]
      # decimal places of the observed values: everything after the dot,
      # zero for values without one
      dec <- suppressWarnings(max(nchar(sub("^[^.]*\\.?", "", vals))))
      if (is.finite(dec) && dec > 0) {
        attrs[["SignificantDigits"]] <- as.character(dec)
      }
    }
    attrs
  }

  # Curated codelists: the values observed across all domains carrying the
  # variable.
  codelist_vars <- c("AESEV", "AEREL", "AEACN", "AEOUT", "LBNRIND", "DTHFL",
                     "QNAM", "RDOMAIN", "RELTYPE", "DSDECOD", "IECAT",
                     "PEORRES")
  codelist_values <- map(set_names(codelist_vars), \(v) {
    vals <- unlist(imap(domains, \(df, d) if (v %in% names(df)) unique(df[[v]])),
                   use.names = FALSE)
    sort(unique(na.omit(vals[vals != ""])))
  })
  # A variable whose values are all blank (RELTYPE on record-level links)
  # has nothing to enumerate: an empty codelist is not emitted, and the
  # ItemDef carries no CodeListRef.
  codelist_values <- keep(codelist_values, \(v) length(v) > 0)

  # Value-level metadata for the findings domains, computed before any XML
  # exists: the canonical MetaDataVersion sequence wants the ValueListDefs
  # and WhereClauseDefs emitted ahead of the ItemGroupDefs and ItemDefs.
  vs_params <- domains$VS |>
    distinct(.data$VSTESTCD, .data$VSTEST, .data$VSSTRESU) |>
    arrange(.data$VSTESTCD)
  lb_params <- domains$LB |>
    distinct(.data$LBTESTCD, .data$LBTEST, .data$LBSTRESU,
             .data$LBSTNRLO, .data$LBSTNRHI) |>
    arrange(.data$LBTESTCD, .data$LBSTNRLO)
  eg_params <- domains$EG |>
    distinct(.data$EGTESTCD, .data$EGTEST, .data$EGSTRESU) |>
    arrange(.data$EGTESTCD)
  qs_params <- domains$QS |>
    distinct(.data$QSTESTCD, .data$QSTEST, .data$QSSTRESU) |>
    arrange(.data$QSTESTCD)

  findings <- list(
    list(domain = "VS", var = "VSSTRESN", codevar = "VSTESTCD",
         namevar = "VSTEST", unitvar = "VSSTRESU",
         codes = sort(unique(vs_params$VSTESTCD)), params = vs_params),
    list(domain = "LB", var = "LBSTRESN", codevar = "LBTESTCD",
         namevar = "LBTEST", unitvar = "LBSTRESU",
         codes = sort(unique(lb_params$LBTESTCD)), params = lb_params),
    list(domain = "EG", var = "EGSTRESN", codevar = "EGTESTCD",
         namevar = "EGTEST", unitvar = "EGSTRESU",
         codes = sort(unique(eg_params$EGTESTCD)), params = eg_params),
    list(domain = "QS", var = "QSSTRESN", codevar = "QSTESTCD",
         namevar = "QSTEST", unitvar = "QSSTRESU",
         codes = sort(unique(qs_params$QSTESTCD)), params = qs_params)
  )
  # a findings domain whose parameter column is missing cannot be described:
  # fail loudly instead of emitting dangling ItemOIDs
  for (f in findings) {
    if (!f$var %in% names(domains[[f$domain]])) {
      stop(sprintf("build_define_xml: %s carries no %s column to describe",
                   f$domain, f$var), call. = FALSE)
    }
  }

  # the value-level description for one test code: unit-conditional so the
  # ordinal QS items read "Sleep Quality" instead of "Sleep Quality (NA)",
  # and - for LB, whose ranges are collected sex-specific data - the
  # distinct observed reference ranges
  vlm_description <- function(f, code) {
    p <- f$params |> filter(.data[[f$codevar]] == code)
    desc <- p[[f$namevar]][1]
    if (!is.na(p[[f$unitvar]][1]) && p[[f$unitvar]][1] != "") {
      desc <- str_c(desc, " (", p[[f$unitvar]][1], ")")
    }
    if (f$domain == "LB") {
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
    desc
  }

  # Document ---------------------------------------------------------------
  # The MetaDataVersion children follow the canonical Define-XML 2.0
  # sequence: ValueListDef*/WhereClauseDef*, then ItemGroupDef*/ItemDef*/,
  # then CodeList*, then MethodDef*/CommentDef*/leaf* (kept schema-valid by
  # the suite's validation against the vendored CDISC 2.0 schema).
  doc <- xml2::xml_new_document()
  xml2::xml_add_child(
    doc, "ODM", ODMVersion = "1.3.2", FileType = "Snapshot",
    FileOID = str_c(studyid, "-DEFINE-", format(Sys.Date(), "%Y%m%d")),
    CreationDateTime = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    `xmlns:def` = "http://www.cdisc.org/ns/def/v2.0",
    `xmlns:xlink` = "http://www.w3.org/1999/xlink",
    `xmlns` = "http://www.cdisc.org/ns/odm/v1.3"
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
    study, "MetaDataVersion", OID = "MDV.1", Name = "SDTM metadata",
    Description = "Generated by edc2cdisc::build_define_xml()",
    `def:DefineVersion` = "2.0.0",
    `def:StandardName` = "SDTMIG",
    `def:StandardVersion` = "3.1.2"
  )

  add_text <- function(parent, node, text) {
    xml2::xml_add_child(xml2::xml_add_child(parent, node), "TranslatedText",
                        text, `xml:lang` = "en")
    invisible(NULL)
  }

  # ---- ValueListDefs and WhereClauseDefs ---------------------------------
  for (f in findings) {
    d <- f$domain
    vld <- xml2::xml_add_child(mdv, "def:ValueListDef",
                               OID = str_c("VL.", d, ".", f$var))
    for (i in seq_along(f$codes)) {
      code <- f$codes[[i]]
      wc_oid <- str_c("WC.", d, ".", f$codevar, ".", code)
      ref <- xml2::xml_add_child(vld, "ItemRef",
                                 ItemOID = str_c("IT.", d, ".", f$var, ".", code),
                                 OrderNumber = as.character(i), Mandatory = "Yes")
      xml2::xml_add_child(ref, "def:WhereClauseRef", WhereClauseOID = wc_oid)
    }
  }
  for (f in findings) {
    d <- f$domain
    for (code in f$codes) {
      wc <- xml2::xml_add_child(mdv, "def:WhereClauseDef",
                                OID = str_c("WC.", d, ".", f$codevar, ".", code))
      rc <- xml2::xml_add_child(wc, "RangeCheck", Comparator = "EQ",
                                SoftHard = "Soft",
                                `def:ItemOID` = str_c("IT.", d, ".", f$codevar))
      xml2::xml_add_child(rc, "CheckValue", code)
    }
  }

  # Derived-origin variables get a MethodDef whose description comes from
  # the spec's derivation registry where it can, else from the family rule;
  # the ItemRefs carry the MethodOID. Dataset families whose shape invites
  # reviewer questions get one shared CommentDef each.
  method_desc_families <- c(
    DY    = "Study day derived from the collected date and the reference start date",
    SEQ   = "Record sequence number, assigned by the build",
    NRIND = "Reference range indicator derived from the result against the collected reference ranges",
    BLFL  = "Baseline flag: the last result on or before first dose per subject and test"
  )
  method_info <- function(var, domain) {
    if (var_origin(var, domain) != "Derived") return(NULL)
    spec_hit <- spec$variables$ref[spec$variables$domain == domain &
                                     spec$variables$variable == var]
    desc <- if (length(spec_hit) == 1 && !is.na(spec_hit)) {
      str_c("Derived by the edc2cdisc '", spec_hit, "' derivation")
    } else {
      fam <- method_desc_families[str_ends(var, names(method_desc_families))]
      if (length(fam) > 0) unname(fam[[1]]) else "Derived by the edc2cdisc build"
    }
    list(oid = str_c("MT.", domain, ".", var), desc = desc)
  }
  comment_keys <- c(RELREC = "RR", TA = "TD", TE = "TD",
                    TI = "TD", TV = "TD", TS = "TD")
  comment_of_domain <- function(d) {
    if (startsWith(d, "SUPP")) return("SP")
    unname(comment_keys[d])
  }
  comment_texts <- c(
    SP = paste("One SUPP-- row per selected parent record: QVAL carries the",
               "collected detail and IDVAR/IDVARVAL point back at the parent"),
    RR = "Related records are linked through the shared RELID; record-level links carry a blank RELTYPE",
    TD = "Derived entirely from the study specification - no collected CRF data"
  )

  needed_comments <- character()

  # ---- ItemGroupDefs: Description, ItemRefs, then the archive leaf -------
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
      `def:Class` = class_spec[[d]],
      `def:Structure` = structure_spec[[d]],
      `def:ArchiveLocationID` = str_c("LF.", d)
    )
    if (nchar(d) == 2) xml2::xml_set_attr(igd, "Domain", d)
    add_text(igd, "Description", structure_spec[[d]])
    for (i in seq_along(df)) {
      var <- names(df)[i]
      ref <- xml2::xml_add_child(igd, "ItemRef", ItemOID = str_c("IT.", d, ".", var),
                                 OrderNumber = as.character(i),
                                 Mandatory = if (var %in% keys) "Yes" else "No")
      if (var %in% keys) {
        xml2::xml_set_attr(ref, "KeySequence", as.character(which(keys == var)))
      }
      mi <- method_info(var, d)
      if (!is.null(mi)) xml2::xml_set_attr(ref, "MethodOID", mi$oid)
    }
    leaf <- xml2::xml_add_child(igd, "def:leaf", ID = str_c("LF.", d),
                                `xlink:href` = str_c("xpt/", low, ".xpt"))
    xml2::xml_add_child(leaf, "def:title", str_c(low, ".xpt"))
    ckey <- comment_of_domain(d)
    if (!is.na(ckey)) {
      xml2::xml_set_attr(igd, "def:CommentOID", str_c("COM.", ckey))
      needed_comments <- union(needed_comments, ckey)
    }
  }

  # ---- ItemDefs: the per-domain variables, then the value-level ones -----
  # The findings "parameter" variables carry the def:ValueListRef on their
  # own ItemDef - that is where the schema puts it, not on the ItemGroupDef
  vl_var <- c(VS = "VSSTRESN", LB = "LBSTRESN", EG = "EGSTRESN", QS = "QSSTRESN")
  for (d in names(domains)) {
    df <- domains[[d]]
    for (i in seq_along(df)) {
      var <- names(df)[i]
      x   <- df[[var]]
      oid <- str_c("IT.", d, ".", var)
      attrs <- item_type_attrs(var, x)
      it <- rlang::exec(xml2::xml_add_child, mdv, "ItemDef", OID = oid,
                        Name = var, SASFieldName = var, !!!attrs)
      lbl <- var_label(df)[[var]]
      add_text(it, "Description", if (is.null(lbl)) var else lbl)
      if (var %in% names(codelist_values)) {
        xml2::xml_add_child(it, "CodeListRef", CodeListOID = str_c("CL.", var))
      }
      xml2::xml_add_child(it, "def:Origin", Type = var_origin(var, d))
      if (!is.na(vl_var[d]) && vl_var[d] == var) {
        xml2::xml_add_child(it, "def:ValueListRef",
                            ValueListOID = str_c("VL.", d, ".", var))
      }
    }
  }
  for (f in findings) {
    d <- f$domain
    # the value-level items describe the parameter column itself, so they
    # inherit its datatype and length attributes
    attrs <- item_type_attrs(f$var, domains[[d]][[f$var]])
    for (i in seq_along(f$codes)) {
      code <- f$codes[[i]]
      it <- rlang::exec(xml2::xml_add_child, mdv, "ItemDef",
                        OID = str_c("IT.", d, ".", f$var, ".", code),
                        Name = f$var, SASFieldName = f$var, !!!attrs)
      add_text(it, "Description", vlm_description(f, code))
      xml2::xml_add_child(it, "def:Origin", Type = "CRF")
    }
  }

  # ---- CodeLists (after the ItemDefs that reference them) ----------------
  for (v in names(codelist_values)) {
    cl <- xml2::xml_add_child(mdv, "CodeList", OID = str_c("CL.", v), Name = v,
                              DataType = "text")
    for (i in seq_along(codelist_values[[v]])) {
      xml2::xml_add_child(cl, "EnumeratedItem",
                          CodedValue = codelist_values[[v]][i],
                          OrderNumber = as.character(i))
    }
  }

  # ---- MethodDefs, then the CommentDefs the ItemGroupDefs reference ------
  for (d in names(domains)) {
    for (var in names(domains[[d]])) {
      mi <- method_info(var, d)
      if (is.null(mi)) next
      mdef <- xml2::xml_add_child(mdv, "MethodDef", OID = mi$oid,
                                  Name = str_c(d, ".", var), Type = "Computation")
      add_text(mdef, "Description", mi$desc)
    }
  }
  for (key in c("SP", "RR", "TD")) {
    if (!key %in% needed_comments) next
    cdef <- xml2::xml_add_child(mdv, "def:CommentDef", OID = str_c("COM.", key))
    add_text(cdef, "Description", unname(comment_texts[[key]]))
  }

  xml2::write_xml(doc, path)
  invisible(path)
}

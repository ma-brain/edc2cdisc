# ============================================================================
# Title:   Data-masked column names
# Purpose: The SDTM/ADaM mappers are dplyr pipelines: variables like USUBJID
#          or AESTDTC are data-masked column names, not undefined globals.
#          Declaring them here keeps `R CMD check` focused on real problems.
#          (The alternative - .data$ everywhere - would bury the mapping
#          logic that these files exist to show.)
# ============================================================================

utils::globalVariables(c(
  ".base", ".bnrind", ".chg", ".conv", ".diff", ".eligible", ".expect", ".factor",
  ".key", ".lb_bl", ".lb_hi", ".lb_ind", ".lb_lo", ".n", ".orres_n", ".pchg",
  ".rank", ".rave_n", ".ref_trtsdt", ".ref_trtedt", ".unit", ".vs_bl",
  "ABLFL", "ADT", "ADY",
  "AEACN", "AEBODSYS", "AECOMNT", "AEDECOD", "AEDISCON", "AEDISCON.x",
  "AEDISCON.y", "AEENDTC", "AEENDY", "AEENRF", "AEENRTPT", "AENDT", "AENDTF",
  "AEOUT", "AEREL", "AESEQ", "AESER", "AESEV", "AESI", "AESI.x", "AESI.y",
  "AESPID", "AESTDTC", "AESTDY", "AETERM", "AGE", "AGEU", "ANRHI", "ANRHI.x",
  "ANRHI.y", "ANRIND", "ANRLO", "ANRLO.x", "ANRLO.y", "ARM", "ARMCD", "ASEQ",
  "ASTDT", "ASTDTF", "ASTDY", "AVAL", "AVISITN", "BASE", "BNRIND", "BRTHDT",
  "BRTHDTC", "BRTHDTF", "CHG", "CMDECOD", "CMDOSE", "CMDOSFRQ", "CMDOSU",
  "CMENDTC", "CMENDY", "CMENRF", "CMENRTPT", "CMINDC", "CMROUTE", "CMSEQ",
  "CMSPID", "CMSTDTC", "CMSTDY", "CMTRT", "CONV_FACTOR", "CONV_FROM",
  "COSEQ", "COUNTRY", "COVAL", "DCSREAS", "DOMAIN", "DSCAT", "DSDECOD",
  "DSSEQ", "DSSTDAT_DD", "DSSTDAT_MM", "DSSTDAT_YYYY", "DSSTDTC", "DSSTDY",
  "DSTERM", "DTHDT", "DTHDTC", "DTHDTF", "DTHFL", "ELEMENT", "ENRLSTDT",
  "EOSDT", "EOSDTF", "EOSSTT", "EPOCH", "ETCD", "ETHNIC", "EXADMBY",
  "EXDOSE", "EXDOSU", "EXENDAT_DD", "EXENDAT_MM", "EXENDAT_YYYY",
  "EXENDTC", "EXENDY", "EXOCCUR",
  "EXROUTE", "EXSEQ", "EXSTDAT_DD", "EXSTDAT_MM", "EXSTDAT_YYYY", "EXSTDTC",
  "EXSTDY", "EXTRT", "Folder", "FolderName", "FolderSeq", "FormOID", "IDVAR",
  "IDVARVAL", "ITTFL", "InstanceRepeatNumber", "LBBLFL", "LBCAT", "LBDAT_DD",
  "LBDAT_MM", "LBDAT_YYYY", "LBDTC", "LBDY", "LBFAST", "LBNRIND", "LBORNRHI",
  "LBORNRLO", "LBORRES", "LBORRESU", "LBPERF", "LBREASND", "LBSEQ", "LBSPEC",
  "LBSTAT", "LBSTNRHI", "LBSTNRLO", "LBSTRESC", "LBSTRESN", "LBSTRESU",
  "LBTEST", "LBTESTCD", "LBTIM", "LSTALVDT", "Length", "MHBODSYS", "MHDECOD",
  "MHENDTC", "MHENDY", "MHENRF", "MHENRTPT", "MHSEQ", "MHSTDTC", "MHSTDY",
  "MHTERM", "MaxUpdated", "MinCreated", "NRHI_O", "NRLO_O", "PARAMCD",
  "PARAMN", "PCHG", "PageRepeatNumber", "QLABEL", "QNAM", "QVAL", "RACE",
  "RAVE_STD", "RAVE_STDU", "RDOMAIN", "RECDT", "RELID", "RELTYPE", "RFENDTC", "RFICDTC",
  "RFPENDTC", "RFSTDTC", "RecordActive", "RecordDate", "SAFFL", "SEX",
  "SITEID", "STUDYID", "SUBJID", "SVENDTC", "SVENDY", "SVSTDTC", "SVSTDY",
  "SaveTs", "Subject", "TABRANCH", "TAETORD", "TATRANSAC", "TEDUR",
  "TEENRL", "TESTRL", "TRT01A", "TRT01ACD", "TRT01P", "TRT01PCD", "TRTDURD",
  "TRTEDT", "TRTEDTF", "TRTEMFL", "TRTSDT", "TRTSDTF", "TargetDays",
  "USUBJID", "VISIT", "VISITDY", "VISITNUM", "VISIT_SV", "VSBLFL",
  "VSDAT_DD", "VSDAT_MM", "VSDAT_YYYY", "VSDTC", "VSDY", "VSORRES",
  "VSORRESU", "VSPERF", "VSPOS", "VSPOS_DECODE", "VSREASND", "VSSEQ",
  "VSSTAT", "VSSTRESC", "VSSTRESN", "VSSTRESU", "VSTEST", "VSTESTCD",
  "VSTIM", "VariableLabel", "VariableName", "VariableOrdinal", "anrhi",
  "anrlo", "contactdtc", "conv_factor", "conv_from", "conv_to", "ct",
  "domain", "dsstdtc", "exendtc", "expect", "exstdtc", "n", "paramcd",
  "paramn", "qeval", "qlabel", "qnam", "qorig", "rave_decode", "rdomain",
  "recordposition", "ref", "src", "testcd", "trtedtc", "trtsdtc", "usubjid",
  "vd"
))

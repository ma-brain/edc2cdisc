# ============================================================================
# run_all.R - regenerate the synthetic Rave extract and rebuild every SDTM
#             domain from scratch. Run from the project root:
#
#   Rscript run_all.R          # or  source("run_all.R")  in an R session
#
# Order matters:
#   05  writes data/rave/*.csv         (synthetic source data)
#   10  DM  -> subject_ref (RFSTDTC)   every later domain needs it for --DY
#   11  EX      12  VS
#   13  AE      14  CM      15  DS
#   16  SV  (from the CRF header block; VISITNUM reference)
#   17  LB  (analyte-aware unit conversion + LBNRIND)
#   18  MH  (medical history log form)
#   19  SUPPDM   20  SUPPAE   21  SUPPEX   (non-standard CRF fields -> QNAM/QVAL)
#   22  CO  (free-text investigator comment on AE)
#   23  RELREC (CM <-> AE links)
#   24  structural validation          stops on any ERROR
#   25  define.xml stub for data/sdtm  (+ value-level metadata for VS/LB)
#
#   30  ADSL (ADaM subject level: populations, analysis dates, disposition)
#   31  ADAE (analysis AEs: imputed dates, TRTEMFL, SUPPAE merge-back)
#   32  ADVS (BDS: ABLFL/BASE/CHG/PCHG, ANRIND)
#   33  ADLB (BDS with record-level ranges: BNRIND, row-wise ANRLO/ANRHI)
#   39  ADaM content validation          stops on any ERROR
# ============================================================================

source("00_setup.R")
source("01_rave_io.R")
source("02_sdtm_helpers.R")

source("05_generate_rave_extract.R")

source("10_sdtm_dm.R")
source("11_sdtm_ex.R")
source("12_sdtm_vs.R")
source("13_sdtm_ae.R")
source("14_sdtm_cm.R")
source("15_sdtm_ds.R")
source("16_sdtm_sv.R")
source("17_sdtm_lb.R")
source("18_sdtm_mh.R")

source("19_sdtm_suppdm.R")
source("20_sdtm_suppae.R")
source("21_sdtm_suppex.R")
source("22_sdtm_co.R")
source("23_sdtm_relrec.R")

source("24_validate.R")
source("25_define_sdtm.R")

source("30_adam_adsl.R")
source("31_adam_adae.R")
source("32_adam_advs.R")
source("33_adam_adlb.R")
source("39_validate_adam.R")

message("run_all.R: done")
message("  SDTM .rds : ", paths$sdtm)
message("  SDTM .xpt : ", paths$xpt)
message("  ADaM .rds : ", paths$adam)
message("  ADaM .xpt : ", paths$xpt_adam)

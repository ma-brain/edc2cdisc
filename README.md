# Synthetic Rave extract → SDTM → ADaM (R training project)

A self-contained, **R-only** sandbox for practising EDC-to-SDTM-to-ADaM
conversion without access to a live Rave instance. An R generator writes a
fake extract that mimics Medidata Rave **Regular clinical views** as
delivered by SAS on Demand / the Biostat Adapter; a short pipeline of
tidyverse scripts maps it to 14 SDTM domains (including SUPP, CO and
RELREC), then to the four core ADaM datasets.
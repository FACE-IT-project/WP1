# data_collection.R
# The location of collected data and the code used when possible


# Setup -------------------------------------------------------------------

library(tidyverse)
library(pangaear)

# Function for downloading and prepping PANGAEA data for merging
pg_dl_prep <- function(pg_doi){
  # Get data
  dl_dat <- pg_data(pg_doi)
  # Extract data.frame
  # Attach URL and citation
  # Exit
}

# European Arctic ---------------------------------------------------------

### Cryosphere
## Sea ice concentration
# 25 km, 1978 - 2019: daily
# ftp://sidads.colorado.edu/pub/DATASETS/NOAA/G02202_V3/north/daily/
# 6.25 km, possibly 10 m, 2002 - 2021: daily
# https://seaice.uni-bremen.de/data/amsr2/


### Bio/chemi

## Zooplankton
# 1995-2008
# https://data.npolar.no/dataset/9167dae8-cab2-45b3-9cea-ad69541b0448

## CTD data on PANGAEA
pangaear::pg_search(query = "CTD", bbox = c(-60, 63, 60, 90), topic = "Oceans", count = 500)


# Svalbard ----------------------------------------------------------------

## Geology
# https://data.npolar.no/dataset/645336c7-adfe-4d5a-978d-9426fe788ee3
# https://data.npolar.no/dataset/616f7504-d68d-4018-a1ac-34e329d8ad45
# https://data.npolar.no/dataset/09dbe7b2-b5ee-485e-bb2f-60455c4f82cd


# Kongsfjorden ------------------------------------------------------------

# All Kongsfjorden data files
kong_all <- pangaear::pg_search(query = "kongsfjorden", count = 500)

# CTD data
kong_ctd <- pangaear::pg_search(query = "CTD", bbox = c(11, 78.86, 12.69, 79.1), count = 500)

# Get a swath of files from the same lead author

# Light: PAR
## NB: Only the first file is strictly for PAR
kong_PAR <- pangaear::pg_search(query = "parameter:PAR", bbox = c(11, 78.86, 12.69, 79.1), topic = "Oceans", count = 500)
kong_PAR_secchi <- pg_data(kong_PAR$doi[1])
save(kong_PAR_secchi, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/kong_PAR_secchi.RData")
write_csv(kong_PAR_secchi[[1]]$data, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/kong_PAR_secchi.csv")

# Precipitation. Needed but no link given.

# River discharge. Needed but no link given.

## Salinity: One site. Can be queried in dashboard:
# https://dashboard.awi.de/?dashboard=3760
# Downloaded from PANGAEA

## Salinity: Mooring. Scottish Association of Marine Sciences (SAMS): 
# Haakon Hop (NP) Finlo Cottier (SAMS) Jørgen Berge (UiT, Tromsø)

## Sedimentation. Needed but no link given.

## Temperature: One site. Can be queried in dashboard:
# https://dashboard.awi.de/?dashboard=2847
# Downloaded from PANGAEA

## Temperature: Mooring. Scottish Association of Marine Sciences (SAMS): 
# Haakon Hop (NP) Finlo Cottier (SAMS) Jørgen Berge (UiT, Tromsø)

## Wind: Direction. Needed but no link given.

## Wind: Max speed. Needed but no link given.

## Wind: Mean speed. Needed but no link given.

### Bio/chemi
## No links for: Biodiversity, ecological processes, invasive species, primary productivity,
  # species presence/absence, species survival/success

## Carbonate chemistry: pH, DIC, TA, pCO2. Available upon request:
# http://www.obs-vlfr.fr/~gattuso/data/awipev-CO2_web.html

## Carbonate chemistry: pH, DIC, TA, pCO2: vertical profiles. NPI
# 2012 - 2014
# https://data.npolar.no/dataset/e53eae53-147a-45df-b473-917bb5ba1ed4

# Nutrients: Vertical profiles. NPI
# https://data.npolar.no/dataset/c9de2d1f-54c1-49ca-b58f-a04cf5decca5

# Seabirds: Abundance, vital rates, diet. NPI, S Descamps.

# Phytoplankton: Vertical profiles. NPI

# Protists/nutrients/Chla:
# 2009 - 2014
# https://data.npolar.no/dataset/2bff82dc-22b9-41c0-8348-220e7d6ca4f4

# Zooplankton: Vertical profiles. NPI
# https://data.npolar.no/dataset/94b29b16-b03b-47d7-bfbc-1c3c4f7060d2

# Macroalgae: One site.
# Download from PANGAEA

# Macroalgae: Along fjord axis. Hop et al. 2016
# https://github.com/MikkoVihtakari/MarineDatabase

# Benthic invertebrates: one site. AWI.
# Download from PANGAEA

## Social
# No one here but us chickens

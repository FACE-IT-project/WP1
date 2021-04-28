# data_collection.R
# The location of collected data and the code used when possible
# Note that download lines that have been commented out have already been downloaded
# They are left in this script as a record of what has been done
# Uncommenting them will cause issues with how the script tracks the DOI's of downloads


# Setup -------------------------------------------------------------------

# Libraries used in this script
library(tidyverse)
library(pangaear)
library(doParallel); registerDoParallel(cores = 15)

# Previously downloaded PANGAEA data
pg_files <- dir("~/pCloudDrive/FACE-IT_data/", pattern = "pg_", recursive = T, full.names = T)
pg_files <- pg_files[grepl(".csv", pg_files)]

# Function that loads all PANGAEA data previously downloaded and checks for DOIs
# so as not to download the same files again
pg_doi_list_func <- function(pg_file){
  df <- read_csv(pg_file)
  res <- data.frame(doi = unique(str_remove(df$URL, "https://doi.org/")))
  return(res)
}
pg_doi_list <- plyr::ldply(pg_files, pg_doi_list_func, .parallel = T)

# Function for downloading and prepping PANGAEA data for merging
pg_dl_prep <- function(pg_doi){
  # Get data
  dl_dat <- pg_data(pg_doi)
  
  # Extract data.frame and attach URL + citation
  dl_df <- dl_dat[[1]]$data %>% 
    mutate(URL = dl_dat[[1]]$url,
           # doi = dl_dat[[1]]$doi,
           citation = dl_dat[[1]]$citation)
  
  # Exit
  return(dl_df)
}


# European Arctic ---------------------------------------------------------

### Cryosphere
## Sea ice concentration
# 25 km, 1978 - 2019: daily
# ftp://sidads.colorado.edu/pub/DATASETS/NOAA/G02202_V3/north/daily/
# 6.25 km, possibly 10 m, 2002 - 2021: daily
# https://seaice.uni-bremen.de/data/amsr2/

## EU Arctic oceanography CTD data on PANGAEA
pg_EU_ctd_1 <- pangaear::pg_search(query = "CTD", bbox = c(-60, 63, 60, 90), count = 500)
pg_EU_ctd_2 <- pangaear::pg_search(query = "CTD", bbox = c(-60, 63, 60, 90), count = 500, offset = 500)
pg_EU_ctd_3 <- pangaear::pg_search(query = "CTD", bbox = c(-60, 63, 60, 90), count = 500, offset = 1000)
pg_EU_ctd_4 <- pangaear::pg_search(query = "CTD", bbox = c(-60, 63, 60, 90), count = 500, offset = 1500)
pg_EU_ctd_5 <- pangaear::pg_search(query = "CTD", bbox = c(-60, 63, 60, 90), count = 500, offset = 2000)
pg_EU_ctd_6 <- pangaear::pg_search(query = "CTD", bbox = c(-60, 63, 60, 90), count = 500, offset = 2500)
pg_EU_ctd_all <- rbind(pg_EU_ctd_1, pg_EU_ctd_2, pg_EU_ctd_3, pg_EU_ctd_4, pg_EU_ctd_5, pg_EU_ctd_6)
pg_EU_ctd_all_dl <- plyr::ldply(pg_EU_ctd_all, pg_dl_prep)


# Svalbard ----------------------------------------------------------------


# Kongsfjorden ------------------------------------------------------------

# All Kongsfjorden data files
pg_kong_all <- pangaear::pg_search(query = "kongsfjorden", count = 500)

# CTD data
pg_kong_ctd <- pangaear::pg_search(query = "CTD", bbox = c(11, 78.86, 12.69, 79.1), count = 500)

# Get a swath of files from the same lead author
# pg_kong_ctd_Golubev <- plyr::ldply(pg_kong_ctd$doi[grepl("Golubev", pg_kong_ctd$citation)], pg_dl_prep)
# write_csv(pg_kong_ctd_Golubev, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_ctd_Golubev.csv")

# Light: PAR
## NB: Only the first file is strictly for PAR
pg_kong_PAR <- pangaear::pg_search(query = "parameter:PAR", bbox = c(11, 78.86, 12.69, 79.1), topic = "Oceans", count = 500)
pg_kong_PAR_secchi <- pg_data(pg_kong_PAR$doi[1])
save(pg_kong_PAR_secchi, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_PAR_secchi.RData")
pg_kong_PAR_secchi <- pg_dl_prep(pg_kong_PAR$doi[1])
write_csv(pg_kong_PAR_secchi, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_PAR_secchi.csv")

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

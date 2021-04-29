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
pg_files <- dir("~/pCloudDrive/FACE-IT_data", pattern = "pg_", recursive = T, full.names = T)
pg_files <- pg_files[grepl(".csv", pg_files)]
pg_doi_list <- read_csv("~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")

# Function that loads all PANGAEA data previously downloaded and checks for DOIs
# so as not to download the same files again
# Leaning away from running this here because the EU CTD file is so massive
# pg_doi_list_func <- function(pg_file){
#   df <- read_csv(pg_file)
#   res <- data.frame(doi = unique(str_remove(df$URL, "https://doi.org/")))
#   return(res)
# }
# pg_doi_list <- plyr::ldply(pg_files, pg_doi_list_func, .parallel = T)

# Function for extracting info from PANGAEA data
pg_dl_prep <- function(pg_dl){
  # Extract data.frame and attach URL + citation
  if(is.data.frame(pg_dl$data)){
    if(length(unique(colnames(pg_dl$data))) == length(colnames(pg_dl$data))){
      dl_single <- pg_dl$data %>% 
        mutate(URL = pg_dl$url,
               parent_doi = pg_dl$parent_doi,
               citation = pg_dl$citation)
    } else {
      dl_single <- data.frame(URL = pg_dl$url,
                              parent_doi = pg_dl$parent_doi,
                              citation = pg_dl$citation)
    }
  } else {
    dl_single <- data.frame(URL = pg_dl$url,
                            parent_doi = pg_dl$parent_doi,
                            citation = pg_dl$citation)
  }
  return(dl_single)
}

# Function for downloading and processing PANGAEA data for merging
pg_dl_proc <- function(pg_doi){
  # Get data
  dl_dat <- tryCatch(pg_data(pg_doi), error = function(pg_doi) NA)
  
  # Extract data from multiple lists as necessary
  if(!is.na(dl_dat)){
    dl_df <- plyr::ldply(dl_dat, pg_dl_prep) %>% 
      dplyr::select(URL, parent_doi, citation, everything())
  } else {
    dl_df <- NULL
  }
  
  # Exit
  return(dl_df)
}


# Key drivers -------------------------------------------------------------

## Cryosphere
# Coastal ice
# Fast ice
# Glacier
# Permafrost
# Sea ice
# Snow cover

## Chemistry
# CaCO3 saturation state
# Dissolved inorganic carbon
# Dissolved organic carbon
# Dissolved organic nitrogen
# Dissolved O2
# Nutrients: nitrate, nitrite, ammonium, phosphate, silicate
# Partial pressure of CO2
# pH
# Total alkalinity

## Biology
# Calcification
# Nitrogen fixation
# Photosynthesis
# Primary production
# Respiration
# Species: presence/absence, abundance/biomass

## Social
# Fish landings: commercial, recreational, quotas, seasonality
# Game landings: quotas, seasonality
# Local and national resource management
# National statistics: demography, income, unemployment
# Tourist arrivals: per month, nationality
# Tourist vessels: count, mileage



# European Arctic ---------------------------------------------------------

### Cryosphere
## Sea ice concentration
# 25 km, 1978 - 2019: daily
# ftp://sidads.colorado.edu/pub/DATASETS/NOAA/G02202_V3/north/daily/
# 6.25 km, possibly 10 m, 2002 - 2021: daily
# https://seaice.uni-bremen.de/data/amsr2/

## EU Arctic CTD data on PANGAEA
# pg_EU_ctd_1 <- pangaear::pg_search(query = "CTD", bbox = c(-60, 63, 60, 90), count = 500) %>%
#   filter(doi != "10.1594/PANGAEA.852715") # Dead links
# pg_EU_ctd_2 <- pangaear::pg_search(query = "CTD", bbox = c(-60, 63, 60, 90), count = 500, offset = 500)
# pg_EU_ctd_3 <- pangaear::pg_search(query = "CTD", bbox = c(-60, 63, 60, 90), count = 500, offset = 1000)
# pg_EU_ctd_4 <- pangaear::pg_search(query = "CTD", bbox = c(-60, 63, 60, 90), count = 500, offset = 1500)
# pg_EU_ctd_5 <- pangaear::pg_search(query = "CTD", bbox = c(-60, 63, 60, 90), count = 500, offset = 2000)
# pg_EU_ctd_6 <- pangaear::pg_search(query = "CTD", bbox = c(-60, 63, 60, 90), count = 500, offset = 2500)
# pg_EU_ctd_all <- rbind(pg_EU_ctd_1, pg_EU_ctd_2, pg_EU_ctd_3, pg_EU_ctd_4, pg_EU_ctd_5, pg_EU_ctd_6)
# pg_EU_ctd_all_dl <- plyr::ldply(pg_EU_ctd_all$doi, pg_dl_proc)

# Update PANGAEA DOI list
## NB: Some of these files will be removed from the following clean up of the data.frame
## This is an intentional choice as we don't want to re-download a file we decided we didn't need
## This includes the removal of the Voß et al. Absorbance datasets
# pg_doi_list <- distinct(data.frame(doi = pg_EU_ctd_all$doi))
# write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")

# Clean up the CTD file
# colnames(pg_EU_ctd_all_dl)
# pg_EU_ctd_all[grep("Absorbance", pg_EU_ctd_all$citation),]
# pg_EU_ctd_all_clean <- pg_EU_ctd_all_dl %>% 
#   select(-contains(c("ac std", "OD ", "Abund v", "Biovol v", "Absorbance ", "URL ",
#                      "ac1", "ac2", "ac3", "ac4", "ac5", "ac6", "ac7", "ac8", "ac9")))
# colnames(pg_EU_ctd_all_clean)
## Addtional filtering - not run/doesn't work
# num_cols <- colnames(pg_EU_ctd_all_clean)[unlist(lapply(pg_EU_ctd_all_clean, is.numeric))][c(-1, -2)]
# pg_EU_ctd_all_clean <- pg_EU_ctd_all_clean %>% 
  # mutate(row_sum = rowSums(across(all_of(num_cols)), na.rm = T)) %>% 
  # filter(!is.na(row_sum))

# Save as .csv
# write_csv(pg_EU_ctd_all_clean, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_ctd_all.csv")


# Svalbard ----------------------------------------------------------------


# Kongsfjorden ------------------------------------------------------------

## Salinity: Mooring. Scottish Association of Marine Sciences (SAMS): 
# Haakon Hop (NP) Finlo Cottier (SAMS) Jørgen Berge (UiT, Tromsø)

## Temperature: Mooring. Scottish Association of Marine Sciences (SAMS): 
# Haakon Hop (NP) Finlo Cottier (SAMS) Jørgen Berge (UiT, Tromsø)

## Carbonate chemistry: pH, DIC, TA, pCO2. Available upon request:
# http://www.obs-vlfr.fr/~gattuso/data/awipev-CO2_web.html

## Nutrients: Vertical profiles. NPI
# https://data.npolar.no/dataset/c9de2d1f-54c1-49ca-b58f-a04cf5decca5

## Seabirds: Abundance, vital rates, diet. NPI, S Descamps.

## Macroalgae: Along fjord axis. Hop et al. 2016
# https://github.com/MikkoVihtakari/MarineDatabase

## All Kongsfjorden data files
pg_kong_all <- pangaear::pg_search(query = "kongsfjorden", count = 500) %>% 
  filter(!doi %in% pg_doi_list$doi) %>% arrange(citation)

## Cryosphere - 0
pg_kong_cryo <- pangaear::pg_search(query = "kongsfjorden", topic = "Cryosphere", count = 500)

## Oceans - 41
pg_kong_oceans <- pangaear::pg_search(query = "kongsfjorden", topic = "Oceans", count = 500) %>%
  filter(!doi %in% pg_doi_list$doi) %>% arrange(citation)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_kong_oceans$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")

# Saccharina experiment - not a driver so not included in meta-database
# pg_kong_diehl_2021 <- pg_dl_proc(pg_kong_oceans$doi[2])

# Hydrographical time series
pg_kong_Fischer <- plyr::ldply(pg_kong_oceans$doi[grepl("Fischer", pg_kong_oceans$citation)], pg_dl_proc)
pg_kong_Fischer <- pg_kong_Fischer[!grepl("10.1594/PANGAEA.927379", pg_kong_Fischer$URL),] # Remove Helgoland data
write_csv(pg_kong_Fischer, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Fischer.csv")

# Physical values at fish sampling - This is in the EU CTD file
# pg_kong_Nahrgang <- pg_dl_proc(pg_kong_oceans$doi[26])
# write_csv(pg_kong_Nahrgang, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Nahrgang.csv")

# Seaweed morphology data
pg_kong_Scheschonk <- plyr::ldply(pg_kong_oceans$doi[grepl("Scheschonk", pg_kong_oceans$citation)], pg_dl_proc)
write_csv(pg_kong_Scheschonk, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Scheschonk.csv")

# Sediment, water and biomass characteristics
pg_kong_Woelfel <- plyr::ldply(pg_kong_oceans$doi[grepl("Woelfel", pg_kong_oceans$citation)], pg_dl_proc)
write_csv(pg_kong_Woelfel, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Woelfel.csv")

## Atmosphere - 0
pg_kong_atmosphere <- pangaear::pg_search(query = "kongsfjorden", topic = "Atmosphere", count = 500) %>%
  filter(!doi %in% pg_doi_list$doi) %>% arrange(citation)

## Chemistry - 83
pg_kong_chemistry <- pangaear::pg_search(query = "kongsfjorden", topic = "Chemistry", count = 500) %>%
  filter(!doi %in% pg_doi_list$doi) %>% arrange(citation)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_kong_chemistry$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")

# Abundances, dominance and diversity of benthic + abiotic variables
pg_kong_Bick <- plyr::ldply(pg_kong_chemistry$doi[grepl("Bick", pg_kong_chemistry$citation)], pg_dl_proc)
write_csv(pg_kong_Bick, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Bick.csv")

# Hydrological, biogeochemical and carbonate system data
pg_kong_Cantoni <- plyr::ldply(pg_kong_chemistry$doi[grepl("Cantoni", pg_kong_chemistry$citation)], pg_dl_proc)
write_csv(pg_kong_Cantoni, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Cantoni.csv")

# Seawater carbonate chemistry
pg_kong_Co <- plyr::ldply(pg_kong_chemistry$doi[grepl("Cantoni", pg_kong_chemistry$citation)], pg_dl_proc)
write_csv(pg_kong_Cantoni, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Cantoni.csv")

# Chemical and biological water column characteristics + zoo/phytoplankton %
pg_kong_van_De_Poll <- plyr::ldply(pg_kong_chemistry$doi[grepl("van De Poll", pg_kong_chemistry$citation)], pg_dl_proc)
write_csv(pg_kong_van_De_Poll, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_van_De_Poll.csv")

## Biological classification - 0
pg_kong_bio_class <- pangaear::pg_search(query = "kongsfjorden", topic = "Biological Classification", count = 500) %>%
  filter(doi %in% pg_doi_list$doi) %>% arrange(citation)

## Biosphere - 0
pg_kong_biosphere <- pangaear::pg_search(query = "kongsfjorden", topic = "Biosphere", count = 500) %>%
  filter(doi %in% pg_doi_list$doi) %>% arrange(citation)



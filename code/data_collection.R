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
pg_EU_ctd_1 <- pangaear::pg_search(query = "CTD", bbox = c(-60, 63, 60, 90), count = 500) %>%
  filter(doi != "10.1594/PANGAEA.852715") # Dead links
pg_EU_ctd_2 <- pangaear::pg_search(query = "CTD", bbox = c(-60, 63, 60, 90), count = 500, offset = 500)
pg_EU_ctd_3 <- pangaear::pg_search(query = "CTD", bbox = c(-60, 63, 60, 90), count = 500, offset = 1000)
pg_EU_ctd_4 <- pangaear::pg_search(query = "CTD", bbox = c(-60, 63, 60, 90), count = 500, offset = 1500)
pg_EU_ctd_5 <- pangaear::pg_search(query = "CTD", bbox = c(-60, 63, 60, 90), count = 500, offset = 2000)
pg_EU_ctd_6 <- pangaear::pg_search(query = "CTD", bbox = c(-60, 63, 60, 90), count = 500, offset = 2500)
pg_EU_ctd_all <- rbind(pg_EU_ctd_1, pg_EU_ctd_2, pg_EU_ctd_3, pg_EU_ctd_4, pg_EU_ctd_5, pg_EU_ctd_6)
pg_EU_ctd_all_dl <- plyr::ldply(pg_EU_ctd_all$doi, pg_dl_proc)

# Update PANGAEA DOI list
## NB: Some of these files will be removed from the following clean up of the data.frame
## This is an intentional choice as we don't want to re-download a file we decided we didn't need
## This includes the removal of the Voß et al. Absorbance datasets
pg_doi_list <- distinct(data.frame(doi = pg_EU_ctd_all$doi))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")

# Clean up the CTD file
colnames(pg_EU_ctd_all_dl)
pg_EU_ctd_all[grep("Absorbance", pg_EU_ctd_all$citation),]
pg_EU_ctd_all_clean <- pg_EU_ctd_all_dl %>%
  select(-contains(c("ac std", "OD ", "Abund v", "Biovol v", "Absorbance ", "URL ",
                     "ac1", "ac2", "ac3", "ac4", "ac5", "ac6", "ac7", "ac8", "ac9")))
colnames(pg_EU_ctd_all_clean)
## Addtional filtering - not run/doesn't work
# num_cols <- colnames(pg_EU_ctd_all_clean)[unlist(lapply(pg_EU_ctd_all_clean, is.numeric))][c(-1, -2)]
# pg_EU_ctd_all_clean <- pg_EU_ctd_all_clean %>% 
  # mutate(row_sum = rowSums(across(all_of(num_cols)), na.rm = T)) %>% 
  # filter(!is.na(row_sum))

# Save as .csv
write_csv(pg_EU_ctd_all_clean, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_ctd_all.csv")
# pg_EU_ctd_all_clean <- data.table::fread("~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_ctd_all.csv", nThread = 15)


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

## All Kongsfjorden data files - 199
pg_kong_all <- pangaear::pg_search(query = "kongsfjorden", count = 500) %>% 
  filter(!doi %in% pg_doi_list$doi) %>% arrange(citation)

# Abundances, dominance and diversity of benthic + abiotic variables
pg_kong_Bick <- plyr::ldply(pg_kong_all$doi[grepl("Bick", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_kong_Bick, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Bick.csv")

# Hydrological, biogeochemical and carbonate system data
pg_kong_Cantoni <- plyr::ldply(pg_kong_all$doi[grepl("Cantoni", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_kong_Cantoni, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Cantoni.csv")

# Seawater carbonate chemistry
pg_kong_Comeau <- plyr::ldply(pg_kong_all$doi[grepl("Comeau", pg_kong_all$citation)][1], pg_dl_proc)
write_csv(pg_kong_Comeau, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Comeau.csv")

# Saccharina experiment - not a driver so not included in meta-database
# pg_kong_diehl <- pg_dl_proc(pg_kong_oceans$doi[10])

# Hydrographical time series
pg_kong_Fischer <- plyr::ldply(pg_kong_all$doi[grepl("Fischer", pg_kong_all$citation)], pg_dl_proc)
pg_kong_Fischer <- pg_kong_Fischer[!grepl("10.1594/PANGAEA.927379", pg_kong_Fischer$URL),] # Remove Helgoland data
write_csv(pg_kong_Fischer, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Fischer.csv")

# Light data
pg_kong_Friedrichs <- plyr::ldply(pg_kong_all$doi[grepl("Friedrichs", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_kong_Friedrichs, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Friedrichs.csv")

# Three fjords of Svalbard - Login required
# pg_sval_Goldhammer <- plyr::ldply(pg_kong_all$doi[grepl("Goldhammer", pg_kong_all$citation)], pg_dl_proc)

# Organic matter composition of the water column - Column name issue
# pg_kong_Grosse <- plyr::ldply(pg_kong_all$doi[grepl("Grosse", pg_kong_all$citation)], pg_dl_proc)

# Assemblage of soft bottom community
pg_kong_Herrmann <- plyr::ldply(pg_kong_all$doi[grepl("Herrmann", pg_kong_all$citation)][-7], pg_dl_proc)
write_csv(pg_kong_Herrmann, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Herrmann.csv")

# Physical oceanography and benthic ocmmunity structures
pg_kong_Laudien <- pg_kong_all[grepl("Laudien", substr(pg_kong_all$citation, 1, 7)),]
pg_kong_Laudien <- pg_kong_Laudien[!grepl("Photograph", pg_kong_Laudien$citation),]
pg_kong_Laudien <- pg_kong_Laudien[!grepl("video", pg_kong_Laudien$citation),]
pg_kong_Laudien <- plyr::ldply(pg_kong_Laudien$doi, pg_dl_proc)
write_csv(pg_kong_Laudien, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Laudien.csv")

# Species data
pg_kong_Petrowski <- plyr::ldply(pg_kong_all$doi[grepl("Petrowski", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_kong_Petrowski, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Petrowski.csv")
  
# Seaweed morphology data
pg_kong_Scheschonk <- plyr::ldply(pg_kong_all$doi[grepl("Scheschonk", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_kong_Scheschonk, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Scheschonk.csv")

# Early succession in benthic hard bottom communities
pg_kong_Schmiing <- plyr::ldply(pg_kong_all$doi[grepl("Schmiing", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_kong_Schmiing, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Schmiing.csv")

# Temperature and light measurements at time series station 
pg_kong_Sevilgen <- plyr::ldply(pg_kong_all$doi[grepl("Sevilgen", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_kong_Sevilgen, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Sevilgen.csv")

# Chemical and biological water column characteristics + zoo/phytoplankton %
pg_kong_van_De_Poll <- plyr::ldply(pg_kong_all$doi[grepl("van De Poll", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_kong_van_De_Poll, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_van_De_Poll.csv")

# Abundance of zooplankton
pg_kong_Walkusz <- plyr::ldply(pg_kong_all$doi[grepl("Walkusz", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_kong_Walkusz, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Walkusz.csv")

# Sediment, water and biomass characteristics
pg_kong_Woelfel <- plyr::ldply(pg_kong_all$doi[grepl("Woelfel", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_kong_Woelfel, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Woelfel.csv")

# Update DOI list with Kongsfjorden
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_kong_all$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")


# Isfjorden ---------------------------------------------------------------

## All Isfjorden data files - 15
pg_is_all <- pangaear::pg_search(query = "isfjorden", count = 500) %>% 
  filter(!doi %in% pg_doi_list$doi) %>% arrange(citation)


# Inglefieldbukta ---------------------------------------------------------

## All Inglefieldbukta data files - 0
pg_ingle_all <- pangaear::pg_search(query = "inglefieldbukta", count = 500)


# Young sound -------------------------------------------------------------

## All Isfjorden data files - 0
pg_young_all <- pangaear::pg_search(query = "zackenburg", count = 500) #%>% 
  # filter(!doi %in% pg_doi_list$doi) %>% arrange(citation)


# Disko Bay ---------------------------------------------------------------

## All Isfjorden data files - 113
pg_disko_all <- pangaear::pg_search(query = "disko", count = 500) %>% 
  filter(!doi %in% pg_doi_list$doi) %>% arrange(citation)


# Nuup Kangerlua ----------------------------------------------------------

## All Nuup Kangerlua data files - 374
pg_nuup_all <- pangaear::pg_search(query = "nuuk", count = 500) %>% 
  filter(!doi %in% pg_doi_list$doi) %>% arrange(citation)


# Porsangerfjorden --------------------------------------------------------

## All Porsangerfjorden data files - 0
pg_por_all <- pangaear::pg_search(query = "porsangerfjorden", count = 500)


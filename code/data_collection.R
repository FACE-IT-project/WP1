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

# Function for printing PANGAEA meta-data
pg_meta_print <- function(pg_doi){
  pg_test <- pangaear::pg_data(pg_doi)
}

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
#

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

pg_EU_search <- function(bbox = c(-60, 63, 60, 90), ...){
  pg_EU_res_all <- data.frame()
  # query_min_score <- 100
  query_offset <- 0
  while(query_offset < 10000){
    pg_EU_res_query <- pangaear::pg_search(bbox = bbox, count = 500, offset = query_offset, ...)
    pg_EU_res_all <- rbind(pg_EU_res_all, pg_EU_res_query)
    query_offset <- query_offset+500
    # query_min_score <- min(pg_EU_cruise_all$score)
  }
  pg_EU_res_all <- distinct(arrange(pg_EU_res_all, citation)) %>% 
    filter(!grepl("core", citation), !grepl("video", citation), !grepl("photograph", citation))
  return(pg_EU_res_all)
}

## EU Arctic cruise Oceans data on PANGAEA - 272
pg_EU_cruise_oceans <- pg_EU_search(query = "cruise", topic = "Oceans") 
pg_doi_list <- distinct(data.frame(doi = pg_EU_cruise_oceans$doi))
pg_EU_cruise_oceans_dl <- plyr::ldply(pg_EU_cruise_oceans$doi, pg_dl_proc)
pg_EU_cruise_oceans_clean <- pg_EU_cruise_oceans_dl %>%
  select(-contains(c("File format", "File name", "File size", "URL file", 
                     "206Pb", "207Pb", "208Pb", "210Po", "210Pb")))
colnames(pg_EU_cruise_oceans_clean)
write_csv(pg_EU_cruise_oceans_clean, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Oceans.csv")


## EU Arctic cruise Atmosphere data on PANGAEA - 138
pg_EU_cruise_atmosphere <- pg_EU_search(query = "cruise", topic = "Atmosphere") %>% filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_atmosphere$doi)))
pg_EU_cruise_atmosphere_dl <- plyr::ldply(pg_EU_cruise_atmosphere$doi, pg_dl_proc)


## EU Arctic cruise Cryosphere data on PANGAEA - 7
pg_EU_cruise_cryosphere <- pg_EU_search(query = "cruise", topic = "Cryosphere") %>% filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_cryosphere$doi)))
pg_EU_cruise_cryosphere_dl <- plyr::ldply(pg_EU_cruise_cryosphere$doi, pg_dl_proc)
colnames(pg_EU_cruise_cryosphere_dl)


## EU Arctic cruise Biological Classification data on PANGAEA - 257
pg_EU_cruise_bio_class <- pg_EU_search(query = "cruise", topic = "Biological Classification") %>% filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_bio_class$doi)))
pg_EU_cruise_bio_class_dl <- plyr::ldply(pg_EU_cruise_bio_class$doi, pg_dl_proc)


## EU Arctic cruise Biosphere data on PANGAEA - 3
pg_EU_cruise_biosphere <- pg_EU_search(query = "cruise", topic = "Biosphere") %>% filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_biosphere$doi)))
pg_EU_cruise_biosphere_dl <- plyr::ldply(pg_EU_cruise_biosphere$doi, pg_dl_proc)


## EU Arctic cruise Ecology data on PANGAEA - 934
pg_EU_cruise_ecology <- pg_EU_search(query = "cruise", topic = "Ecology") %>% filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_ecology$doi)))
pg_EU_cruise_ecology_dl <- plyr::ldply(pg_EU_cruise_ecology$doi, pg_dl_proc)


## EU Arctic cruise Human Dimensions data on PANGAEA - 0
pg_EU_cruise_human <- pg_EU_search(query = "cruise", topic = "Human Dimensions") %>% filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_human$doi)))
pg_EU_cruise_human_dl <- plyr::ldply(pg_EU_cruise_human$doi, pg_dl_proc)


## EU Arctic cruise Chemistry data on PANGAEA west - 3800
pg_EU_cruise_chemistry_west <- pg_EU_search(query = "cruise", topic = "Chemistry",
                                            bbox = c(-60, 63, 0, 90)) %>% filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_chemistry_west$doi)))
pg_EU_cruise_chemistry_west_dl <- plyr::ldply(pg_EU_cruise_chemistry_west$doi, pg_dl_proc)


## EU Arctic cruise Chemistry data on PANGAEA east - 7989
pg_EU_cruise_chemistry_east <- pg_EU_search(query = "cruise", topic = "Chemistry",
                                            bbox = c(0, 63, 60, 90)) %>% filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_chemistry_east$doi)))
pg_EU_cruise_chemistry_east_dl <- plyr::ldply(pg_EU_cruise_chemistry_east$doi, pg_dl_proc)


## EU Arctic CTD data on PANGAEA - 931
pg_EU_CTD <- pg_EU_search(query = "CTD") %>% filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_CTD$doi)))
pg_EU_CTD_dl <- plyr::ldply(pg_EU_CTD$doi, pg_dl_proc)
# pg_EU_ctd_dl[grep("Absorbance", pg_EU_ctd_dl$citation),]
pg_EU_ctd_clean <- pg_EU_ctd_dl %>%
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

## All Kongsfjorden data files - 209
pg_kong_all_short <- pangaear::pg_search(query = "kongsfjord", count = 500) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_kong_all <- pangaear::pg_search(query = "kongsfjorden", count = 500) %>% 
  filter(!doi %in% pg_doi_list$doi) %>%
  filter(!doi %in% pg_kong_all_short$doi) %>%
  rbind(pg_kong_all_short) %>% 
  arrange(citation) %>% 
  distinct(); rm(pg_kong_all_short)

# Species density and composition
pg_kong_Bergmann <- plyr::ldply(pg_kong_all$doi[grepl("Bergmann", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_kong_Bergmann, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Bergmann.csv")

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

# Macroalgal biomass
pg_kong_Hop <- plyr::ldply(pg_kong_all$doi[grepl("Hop", substr(pg_kong_all$citation, 1, 3))], pg_dl_proc)
write_csv(pg_kong_Hop, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Hop.csv")

# Sea ice thickness and radiation - Doesn't contain data
# pg_kong_Krumpen <- plyr::ldply(pg_kong_all$doi[grepl("Krumpen", pg_kong_all$citation)], pg_dl_proc)

# Physical oceanography and benthic community structures
pg_kong_Laudien <- pg_kong_all[grepl("Laudien", substr(pg_kong_all$citation, 1, 7)),]
pg_kong_Laudien <- pg_kong_Laudien[!grepl("Photograph", pg_kong_Laudien$citation),]
pg_kong_Laudien <- pg_kong_Laudien[!grepl("video", pg_kong_Laudien$citation),]
pg_kong_Laudien <- plyr::ldply(pg_kong_Laudien$doi, pg_dl_proc)
write_csv(pg_kong_Laudien, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Laudien.csv")

# Turbulent flux and meteorological measurement
pg_kong_Luers <- plyr::ldply(pg_kong_all$doi[grepl("Lüers", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_kong_Luers, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Luers.csv")

# Species data
pg_kong_Nowak <- plyr::ldply(pg_kong_all$doi[grepl("Nowak", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_kong_Nowak, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Nowak.csv")

# Species data
pg_kong_Petrowski <- plyr::ldply(pg_kong_all$doi[grepl("Petrowski", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_kong_Petrowski, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Petrowski.csv")
  
# Seaweed morphology data
pg_kong_Scheschonk <- plyr::ldply(pg_kong_all$doi[grepl("Scheschonk", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_kong_Scheschonk, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Scheschonk.csv")

# Seawater carbonate chemistry - Column issues
pg_kong_Schmidt <- plyr::ldply(pg_kong_all$doi[grepl("Schmidt", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_kong_Schmidt, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Schmidt.csv")

# Early succession in benthic hard bottom communities
pg_kong_Schmiing <- plyr::ldply(pg_kong_all$doi[grepl("Schmiing", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_kong_Schmiing, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Schmiing.csv")

# Temperature and light measurements at time series station 
pg_kong_Sevilgen <- plyr::ldply(pg_kong_all$doi[grepl("Sevilgen", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_kong_Sevilgen, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Sevilgen.csv")

# Seawater pH predicted for the year 2100 - Column names issue
# pg_kong_Thor <- plyr::ldply(pg_kong_all$doi[grepl("Thor", pg_kong_all$citation)], pg_dl_proc)

# Chemical and biological water column characteristics + zoo/phytoplankton %
pg_kong_van_De_Poll <- plyr::ldply(pg_kong_all$doi[grepl("van De Poll", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_kong_van_De_Poll, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_van_De_Poll.csv")

# Meiobenthic assemblages
pg_kong_Veit_Kohler <- plyr::ldply(pg_kong_all$doi[grepl("Veit-Köhler", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_kong_Veit_Kohler, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Veit_Kohler.csv")

# Seawater carbonate chemistry - Column names issue
# pg_kong_Venello <- plyr::ldply(pg_kong_all$doi[grepl("Venello", pg_kong_all$citation)], pg_dl_proc)

# Abundance of zooplankton
pg_kong_Walkusz <- plyr::ldply(pg_kong_all$doi[grepl("Walkusz", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_kong_Walkusz, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Walkusz.csv")

# Sediment, water and biomass characteristics
pg_kong_Woelfel <- plyr::ldply(pg_kong_all$doi[grepl("Woelfel", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_kong_Woelfel, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Woelfel.csv")

# Glacier length change and mass balance data
# NB: This is an EU file
pg_EU_World_Glacier <- plyr::ldply(pg_kong_all$doi[grepl("World Glacier", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_EU_World_Glacier, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_World_Glacier.csv")

# Update DOI list with Kongsfjorden
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_kong_all$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")


# Isfjorden ---------------------------------------------------------------

## All Isfjorden data files - 15
pg_is_all_short <- pangaear::pg_search(query = "isfjord", count = 500) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_is_all <- pangaear::pg_search(query = "isfjorden", count = 500) %>% 
  filter(!doi %in% pg_doi_list$doi) %>%
  filter(!doi %in% pg_is_all_short$doi) %>%
  rbind(pg_is_all_short) %>% 
  arrange(citation) %>% 
  distinct(); rm(pg_is_all_short)

# Temperature dependence of CO2-enhanced primary production - Column names issue
# NB: This is an EU file
# pg_EU_Holding <- plyr::ldply(pg_is_all$doi[grepl("Holding", pg_is_all$citation)], pg_dl_proc)
# write_csv(pg_EU_Holding, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_Holding.csv")

# Shipborne visual observations of Arctic sea ice - Column names issue
# pg_is_King <- plyr::ldply(pg_is_all$doi[grepl("King", pg_is_all$citation)], pg_dl_proc)
# write_csv(pg_is_King, "~/pCloudDrive/FACE-IT_data/isfjorden/pg_is_King.csv")

# Update DOI list with Isfjorden
# NB: Not added yet as I want to sort out the above column name issue first
# pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_is_all$doi)))
# write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")


# Inglefieldbukta ---------------------------------------------------------

## All Inglefieldbukta data files - 0
pg_ingle_all <- pangaear::pg_search(query = "inglefieldbukta", count = 500)


# Svalbard ----------------------------------------------------------------

## All Svalbard data files - 271
# NB: These files were searched for after the specific sites intentionally
# This was so that site specific files would be allocated to the appropriate folders
# And the files not attributed to a given site would be downloaded in this chunk
# However, I'm currently thinking we don't want these data...
pg_sval_all <- pangaear::pg_search(query = "svalbard", count = 500, bbox = c(9, 76, 30, 81)) %>% 
  filter(!grepl("core", citation), !grepl("video", citation), !grepl("photograph", citation)) %>% 
  filter(!doi %in% pg_doi_list$doi) %>%  arrange(citation)


# Young Sound -------------------------------------------------------------

## All Young sound data files - 0
pg_young_all_short <- pangaear::pg_search(query = "zackenberg", count = 500) 
pg_young_all <- pangaear::pg_search(query = "young sound", count = 500,
                                    bbox = c(-22.367917, 74.210137, -19.907644, 74.624304),) %>% 
  filter(!doi %in% pg_doi_list$doi) %>%
  filter(!doi %in% pg_young_all_short$doi) %>%
  rbind(pg_young_all_short) %>% 
  arrange(citation) %>% 
  distinct(); rm(pg_young_all_short)

# Permafrost longterm monitoring sites
# NB: This is an EU file
pg_EU_Bartsch <- plyr::ldply(pg_young_all$doi[grepl("Bartsch", pg_young_all$citation)], pg_dl_proc)
write_csv(pg_EU_Bartsch, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_Bartsch.csv")

# Permafrost borehole characteristics
# NB: This is an EU file
pg_EU_Christiansen <- plyr::ldply(pg_young_all$doi[grepl("Christiansen", substr(pg_young_all$citation, 1, 12))], pg_dl_proc)
write_csv(pg_EU_Christiansen, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_Christiansen.csv")

# Freya Glacier
pg_young_Hynek <- plyr::ldply(pg_young_all$doi[grepl("Hynek", pg_young_all$citation)], pg_dl_proc)
write_csv(pg_young_Hynek, "~/pCloudDrive/FACE-IT_data/young_sound/pg_young_Hynek.csv")

# CO2 sampling - Login required
# pg_young_Jorgensen <- plyr::ldply(pg_young_all$doi[grepl("Jørgensen", pg_young_all$citation)][1], pg_dl_proc)

# Update DOI list with Young Sound
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_young_all$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")


# Disko Bay ---------------------------------------------------------------

## All Disko Bay data files - 113
pg_disko_all_short <- pangaear::pg_search(query = "Qeqertarsuup", count = 500) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_disko_all <- pangaear::pg_search(query = "disko", count = 500) %>% 
  filter(!doi %in% pg_doi_list$doi) %>%
  filter(!doi %in% pg_disko_all_short$doi) %>%
  rbind(pg_disko_all_short) %>% 
  arrange(citation) %>% 
  distinct(); rm(pg_disko_all_short)

# Tide amplitude, surface height, and flow velocities
pg_disko_Dietrich <- plyr::ldply(pg_disko_all$doi[grepl("Dietrich", pg_disko_all$citation)], pg_dl_proc)
write_csv(pg_disko_Dietrich, "~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_Dietrich.csv")

# Seawater carbonate chemistry and oxygen concentrations in the Greenland tidal pools - Column names issue
# pg_disko_Duarte <- plyr::ldply(pg_disko_all$doi[grepl("Duarte", pg_disko_all$citation)], pg_dl_proc)
# write_csv(pg_disko_Duarte, "~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_Duarte.csv")

# Phytoplankton info
pg_disko_Dunweber <- plyr::ldply(pg_disko_all$doi[grepl("Dünweber", substr(pg_disko_all$citation, 1, 8))], pg_dl_proc)
write_csv(pg_disko_Dunweber, "~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_Dunweber.csv")

# Phytoplankton info - Column names issue
pg_disko_Elferink <- plyr::ldply(pg_disko_all$doi[grepl("Elferink", pg_disko_all$citation)], pg_dl_proc)
write_csv(pg_disko_Elferink, "~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_Elferink.csv")

# Silicon isotopes in Arctic and sub-Arctic glacial meltwaters
# NB: This is an EU file
pg_EU_Hatton <- plyr::ldply(pg_disko_all$doi[grepl("Hatton", pg_disko_all$citation)], pg_dl_proc)
write_csv(pg_EU_Hatton, "~/pCloudDrive/FACE-IT_data/disko_bay/pg_EU_Hatton.csv")

# Light data -  Login required for data
# NB: This is probably an EU file
# pg_EU_Holinde <- plyr::ldply(pg_disko_all$doi[grepl("Holinde", pg_disko_all$citation)], pg_dl_proc)

# Ice, ChlA, whales
pg_disko_Laidre <- plyr::ldply(pg_disko_all$doi[grepl("Laidre", substr(pg_disko_all$citation, 1, 6))], pg_dl_proc)
write_csv(pg_disko_Laidre, "~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_Laidre.csv")

# Carbonate chemistry
pg_disko_Lichtfouse <- plyr::ldply(pg_disko_all$doi[grepl("Lichtfouse", pg_disko_all$citation)], pg_dl_proc)
write_csv(pg_disko_Lichtfouse, "~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_Lichtfouse.csv")

# Light data
pg_disko_Mascarenhas <- plyr::ldply(pg_disko_all$doi[grepl("Mascarenhas", pg_disko_all$citation)], pg_dl_proc)
write_csv(pg_disko_Mascarenhas, "~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_Mascarenhas.csv")

# Bathymetry
pg_disko_Schumann <- plyr::ldply(pg_disko_all$doi[grepl("Schumann", pg_disko_all$citation)], pg_dl_proc) %>% 
  filter(Latitude > 0) %>%  dplyr::select(URL:`Slope inc m [deg]`)
write_csv(pg_disko_Schumann, "~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_Schumann.csv")

# Mesozooplankton info
pg_disko_Swalethorp <- plyr::ldply(pg_disko_all$doi[grepl("Swalethorp", pg_disko_all$citation)][4:6], pg_dl_proc)
write_csv(pg_disko_Swalethorp, "~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_Swalethorp.csv")

# Phytoplankton and carbonate chemistry
# NB: These appear to be experimental data, not field observations
# pg_disko_Thoisen <- plyr::ldply(pg_disko_all$doi[grepl("Thoisen", pg_disko_all$citation)], pg_dl_proc)
# write_csv(pg_disko_Thoisen, "~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_Thoisen.csv")

# Ice velocity
# Not data, but rather links to other data files
# pg_disko_Vijay <- plyr::ldply(pg_disko_all$doi[grepl("Vijay", pg_disko_all$citation)], pg_dl_proc)

# Update DOI list with Disko Bay
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_disko_all$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")


# Nuup Kangerlua ----------------------------------------------------------

## All Nuup Kangerlua data files - 38
pg_nuup_all_short <- pangaear::pg_search(query = "kangerlua", count = 500,
                                         bbox = c(-53.32, 64.01, -48.93, 64.8))# %>% 
  # filter(!doi %in% pg_doi_list$doi)
pg_nuup_all <- pangaear::pg_search(query = "nuuk", count = 500,
                                   bbox = c(-53.32, 64.01, -48.93, 64.8)) %>% 
  filter(!doi %in% pg_doi_list$doi) %>% arrange(citation)

# Bathymetry - Login required for 2 files
# Not data, rather links to other data files
# pg_nuup_Dreutter <- plyr::ldply(pg_nuup_all$doi[grepl("Dreutter", substr(pg_nuup_all$citation, 1, 8))], pg_dl_proc)

# Winter temperatures
pg_nuup_Groissmayr <- plyr::ldply(pg_nuup_all$doi[grepl("Groissmayr", pg_nuup_all$citation)], pg_dl_proc)
write_csv(pg_nuup_Groissmayr, "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/pg_nuup_Groissmayr.csv")

# Physical oceanography and carbonate chemistry
pg_nuup_Johannessen <- plyr::ldply(pg_nuup_all$doi[grepl("Johannessen", substr(pg_nuup_all$citation, 1, 11))], pg_dl_proc)
write_csv(pg_nuup_Johannessen, "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/pg_nuup_Johannessen.csv")

# Physical oceanography and carbonate chemistry - Repeat column name issue
pg_nuup_Olsen <- plyr::ldply(pg_nuup_all$doi[grepl("Olsen", substr(pg_nuup_all$citation, 1, 5))], pg_dl_proc)
write_csv(pg_nuup_Olsen, "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/pg_nuup_Olsen.csv")

# Physical oceanography and carbonate chemistry
pg_nuup_Omar <- plyr::ldply(pg_nuup_all$doi[grepl("Omar", substr(pg_nuup_all$citation, 1, 4))], pg_dl_proc)
write_csv(pg_nuup_Omar, "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/pg_nuup_Omar.csv")

# Met station data
pg_nuup_Paulsen <- plyr::ldply(pg_nuup_all$doi[grepl("Paulsen", substr(pg_nuup_all$citation, 1, 7))][1:5], pg_dl_proc)
write_csv(pg_nuup_Paulsen, "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/pg_nuup_Paulsen.csv")

# Light data
pg_nuup_Zielinski <- plyr::ldply(pg_nuup_all$doi[grepl("Zielinski", substr(pg_nuup_all$citation, 1, 9))], pg_dl_proc)
write_csv(pg_nuup_Zielinski, "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/pg_nuup_Zielinski.csv")

# Update DOI list with Nuup Kangerlua - Repeat column name issue
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_nuup_all$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")


# Porsangerfjorden --------------------------------------------------------

## All Porsangerfjorden data files - 0
pg_por_all <- pangaear::pg_search(query = "porsanger", count = 500)


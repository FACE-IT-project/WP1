# code/data_collection.R
# The location of collected data and the code used when possible
# Note that download lines that have been commented out have already been downloaded
# They are left in this script as a record of what has been done
# Uncommenting them will cause issues with how the script tracks the DOI's of downloads


# Setup -------------------------------------------------------------------

# TODO: Add a date of collection column
# TODO: Add a marine or terrestrial column for users to be able to select one as they prefer

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
               # parent_doi = pg_dl$parent_doi,
               citation = pg_dl$citation) %>% 
        dplyr::select("URL", "citation",
                      contains(c("date", "longitude", "latitude", "depth", "bathy", "press", "density", "elev",  
                                 "temp", "°C", "sst", "sal", "O2", "DO", "ice", "snow", "turb", "PAR", "current", "vel", "u", "v", 
                                 "direction", "vol", "evap", "precip", "e-p", "p-e", "Q", "flux", "latent", "sensible", "longwave", 
                                 "shortwave", "radiation", "kd", "mixed", "MLD", "river", "discharge", "slp", "sedim", "som", "sim",
                                 "wind", "speed", "direction",  "DIC", "DOC", "DON", "pH", "pCO2", "CaCO3", "Arg", "Ara", 
                                 "Cal", "NO3", "NO2", "NH3", "PO4", "Si", "AT", "TA", "Chl a", "Chl b"))) %>% 
        dplyr::select(-contains(c("Rock", "feldspar", "Cluster", "File ", "URL ", "std dev", "Device", "Binary", "taxa",
                                  "phenotype", "Part conc ", "Part vol frac ", "Part conc frac ",
                                  # Can't use these because reprex misbehaves
                                  # "A. ", "B. ", "C. ", "D. ", "E. ", "F. ", "G. ", "H. ", "I. ", "J. ", "K. ", "L. ", "M. ", 
                                  # "N. ", "O. ", "P. ", "Q. ", "R. ", "S. ", "T. ", "U. ", "V. ", "W. ", "X. ", "Y. ", "Z. ",  
                                  # "Zn ", "Cu ", "Ni ", "Cd ", "As ", "Pb ", "Cr ", "Th ", "Mn ", "Co ", "Zr ", "Sr ", "Ba ",
                                  "Comment", "residues", "Stage", "Sample", "Country", "Province", "Station")))
      if("Longitude" %in% colnames(dl_single)){
        dl_single <- dl_single %>% 
          filter(Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
      }
    } else {
      dl_single <- data.frame(URL = pg_dl$url,
                              # parent_doi = pg_dl$parent_doi,
                              citation = pg_dl$citation)
    }
  } else {
    dl_single <- data.frame(URL = pg_dl$url,
                            # parent_doi = pg_dl$parent_doi,
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
    dl_df <- plyr::ldply(dl_dat, pg_dl_prep) #%>% 
      # dplyr::select(URL, parent_doi, citation, everything())
  } else {
    dl_df <- NULL
  }
  
  # Exit
  return(dl_df)
}

# Function for performing a more thorough query of PANGAEA data by bbox
pg_full_search <- function(bbox, ...){
  pg_res_all <- data.frame()
  # query_min_score <- 100
  query_offset <- 0
  while(query_offset < 10000){
    pg_res_query <- pangaear::pg_search(bbox = bbox, count = 500, offset = query_offset, ...)
    pg_res_all <- rbind(pg_res_all, pg_res_query)
    query_offset <- query_offset+500
    # query_min_score <- min(pg_EU_cruise_all$score)
  }
  pg_res_all <- distinct(arrange(pg_res_all, citation)) %>% 
    filter(!grepl("core", citation), !grepl("Core", citation), 
           !grepl("video", citation), !grepl("Video", citation), 
           !grepl("photograph", citation), !grepl("Photograph", citation), 
           !grepl("image", citation), !grepl("Image", citation))
  return(pg_res_all)
}
#

# Key drivers -------------------------------------------------------------

## Cryosphere
# Coastal ice (ice)
# Fast ice (ice)
# Glacier (glacier)
# Permafrost (permafrost)
# Sea ice (ice)
# Snow cover (snow)

## Physical
# Bathymetry (bathy)
# Current: direction, location, volume (current, vel, direction, vol, u , v )
# Evaporation/Precipitation: (evap, precip, e-p, p-e)
# Heatflux: net, latent/sensible, long/shortwave radiation (Q, flux, latent, sensible, longwave, shortwave, radiation)
# Light extinction coefficient (kd)
# Mixed layer depth (mixed, MLD)
# River discharge (river, discharge)
# Salinity (sal, psu)
# Sea level pressure (slp)
# Sedimentation rate (sedim)
# Suspended matter: organic, mineral (pom, pim, som, spm)
# (Seawater) temperature: surface, mid, bottom (°C, temp, sst)
# Wind: direction, speed (wind, speed, direction, u, v)

## Chemistry
# CaCO3 saturation state (CaCO3, Arg, Ara, Cal, omega)
# Dissolved inorganic carbon (DIC)
# Dissolved organic carbon (DOC)
# Dissolved organic nitrogen (DON)
# Dissolved O2 (DO, O2)
# Nutrients: nitrate (NO3), nitrite (NO2), ammonium (NH3), phosphate (PO4), silicate (Si04)
# Partial pressure of CO2 (pCO2)
# pH (ph)
# Total alkalinity (TA, AT)

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

### Geochemistry
## SOCAT datasets on PANGAEA
# Bakker et al...


# Specific author queries
pg_Riebesell <- pg_full_search(query = "Riebesell", bbox = c(-60, 63, 60, 90)) 
pg_Fransson <- pg_full_search(query = "Fransson", bbox = c(-60, 63, 60, 90)) 
pg_Chierici <- pg_full_search(query = "Chierici", bbox = c(-60, 63, 60, 90)) 
pg_Fischer <- pg_full_search(query = "Chierici", bbox = c(-60, 63, 60, 90)) 


## EU Arctic cruise Oceans data on PANGAEA - 272 - Some issues
pg_EU_cruise_oceans <- pg_full_search(query = "cruise", topic = "Oceans", bbox = c(-60, 63, 60, 90)) 
pg_doi_list <- distinct(data.frame(doi = pg_EU_cruise_oceans$doi))
pg_EU_cruise_oceans_dl <- plyr::ldply(pg_EU_cruise_oceans$doi, pg_dl_proc)
pg_EU_cruise_oceans_clean <- pg_EU_cruise_oceans_dl %>%
  filter(Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
colnames(pg_EU_cruise_oceans_clean)
data.table::fwrite(pg_EU_cruise_oceans_clean, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Oceans.csv")
data.table::fwrite(pg_EU_cruise_oceans_clean, "pg_EU_data/pg_EU_cruise_Oceans.csv")
rm(pg_EU_cruise_oceans_dl, pg_EU_cruise_oceans_clean); gc()


## EU Arctic cruise Atmosphere data on PANGAEA - 138 - Some issues
pg_EU_cruise_atmosphere <- pg_full_search(query = "cruise", topic = "Atmosphere", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_atmosphere$doi)))
pg_EU_cruise_atmosphere_dl <- plyr::ldply(pg_EU_cruise_atmosphere$doi, pg_dl_proc)
pg_EU_cruise_atmosphere_clean <- pg_EU_cruise_atmosphere_dl %>%
  filter(Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
colnames(pg_EU_cruise_atmosphere_clean)
data.table::fwrite(pg_EU_cruise_atmosphere_clean, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Atmosphere.csv")
data.table::fwrite(pg_EU_cruise_atmosphere_clean, "pg_EU_data/pg_EU_cruise_Atmosphere.csv")
rm(pg_EU_cruise_atmosphere_dl, pg_EU_cruise_atmosphere_clean); gc()


## EU Arctic cruise Cryosphere data on PANGAEA - 7
pg_EU_cruise_cryosphere <- pg_full_search(query = "cruise", topic = "Cryosphere", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_cryosphere$doi)))
pg_EU_cruise_cryosphere_dl <- plyr::ldply(pg_EU_cruise_cryosphere$doi, pg_dl_proc)
pg_EU_cruise_cryosphere_clean <- pg_EU_cruise_cryosphere_dl %>%
  select(-contains(c("Perc ", "Particles ", "Polymer particles ", "Polymer particle conc ", "(Calculated)"))) %>% 
  filter(Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
colnames(pg_EU_cruise_cryosphere_clean)
data.table::fwrite(pg_EU_cruise_cryosphere_clean, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Cryosphere.csv")
data.table::fwrite(pg_EU_cruise_cryosphere_clean, "pg_EU_data/pg_EU_cruise_Cryosphere.csv")
rm(pg_EU_cruise_cryosphere_dl, pg_EU_cruise_cryosphere_clean); gc()


## EU Arctic cruise Biological Classification data on PANGAEA - 259 - Some issues
pg_EU_cruise_bio_class <- pg_full_search(query = "cruise", topic = "Biological Classification", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_bio_class$doi)))
pg_EU_cruise_bio_class_dl <- plyr::ldply(pg_EU_cruise_bio_class$doi, pg_dl_proc)
pg_EU_cruise_bio_class_clean <- pg_EU_cruise_bio_class_dl %>%
  filter(Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
colnames(pg_EU_cruise_bio_class_clean)
data.table::fwrite(pg_EU_cruise_bio_class_clean, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Bio_class.csv")
data.table::fwrite(pg_EU_cruise_bio_class_clean, "pg_EU_data/pg_EU_cruise_Bio_class.csv")
rm(pg_EU_cruise_bio_class_dl, pg_EU_cruise_bio_class_clean); gc()


## EU Arctic cruise Biosphere data on PANGAEA - 3
pg_EU_cruise_biosphere <- pg_full_search(query = "cruise", topic = "Biosphere", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_biosphere$doi)))
pg_EU_cruise_biosphere_dl <- plyr::ldply(pg_EU_cruise_biosphere$doi, pg_dl_proc)
pg_EU_cruise_biosphere_clean <- pg_EU_cruise_biosphere_dl %>%
  filter(Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
colnames(pg_EU_cruise_biosphere_clean)
data.table::fwrite(pg_EU_cruise_biosphere_clean, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Biosphere.csv")
data.table::fwrite(pg_EU_cruise_biosphere_clean, "pg_EU_data/pg_EU_cruise_Biosphere.csv")
rm(pg_EU_cruise_biosphere_dl, pg_EU_cruise_biosphere_clean); gc()


## EU Arctic cruise Ecology data on PANGAEA - 934 - Some issues
## NB: Takes ~19 minutes
pg_EU_cruise_ecology <- pg_full_search(query = "cruise", topic = "Ecology", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_ecology$doi)))
pg_EU_cruise_ecology_dl <- plyr::ldply(pg_EU_cruise_ecology$doi, pg_dl_proc)
pg_EU_cruise_ecology_clean <- pg_EU_cruise_ecology_dl %>%
  dplyr::select(-contains(c("Perc total form", " indet", " cysts", "Reads ", "Alb ", "C upt ", 
                            "Parameter ", "Vol ", "Fatty acids ", "Grain size",
                            "Phae", "Turbellaria", "Tanaidacea", "Signal", "Taxa", "Polymer ",
                            "Persistent Identifier", "morphospecies", "Reference",
                            "Flag", "δ30Si", "Certainty", "Sensor", "Acryloni"))) %>% 
  filter(Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
colnames(pg_EU_cruise_ecology_clean)
data.table::fwrite(pg_EU_cruise_ecology_clean, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Ecology.csv")
data.table::fwrite(pg_EU_cruise_ecology_clean, "pg_EU_data/pg_EU_cruise_Ecology.csv")
rm(pg_EU_cruise_ecology_dl, pg_EU_cruise_ecology_clean); gc()


## EU Arctic cruise Human Dimensions data on PANGAEA - 0
pg_EU_cruise_human <- pg_full_search(query = "cruise", topic = "Human Dimensions", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)


## EU Arctic cruise Chemistry data on PANGAEA west - 3800 - Some issues
## MB ~16 minutes
pg_EU_cruise_chemistry_west <- pg_full_search(query = "cruise", topic = "Chemistry", bbox = c(-60, 63, 0, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_chemistry_west$doi)))
pg_EU_cruise_chemistry_west_dl <- plyr::ldply(pg_EU_cruise_chemistry_west$doi, pg_dl_proc)
pg_EU_cruise_chemistry_west_clean <- pg_EU_cruise_chemistry_west_dl %>%
  # select(-contains(c("Lu_", "Ed_", "Es_", "QC ", "OD", "ac std dev ",
  #                    "ac1", "ac2", "ac3", "ac4", "ac5", "ac6", "ac7", "ac8", "ac9"))) %>% 
  filter(Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
colnames(pg_EU_cruise_chemistry_west_clean)
data.table::fwrite(pg_EU_cruise_chemistry_west_clean, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Chemistry_west.csv")
data.table::fwrite(pg_EU_cruise_chemistry_west_clean, "pg_EU_data/pg_EU_cruise_Chemistry_west.csv")
rm(pg_EU_cruise_chemistry_west_dl, pg_EU_cruise_chemistry_west_clean); gc()


## EU Arctic cruise Chemistry data on PANGAEA east - 7989 - Some issues
## NB: ~58 minutes
pg_EU_cruise_chemistry_east <- pg_full_search(query = "cruise", topic = "Chemistry", bbox = c(0, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_chemistry_east$doi)))
pg_EU_cruise_chemistry_east_dl <- plyr::ldply(pg_EU_cruise_chemistry_east$doi, pg_dl_proc)
pg_EU_cruise_chemistry_east_clean <- pg_EU_cruise_chemistry_east_dl %>%
  select(-contains(c("Flag ", "acCDOM "))) %>%
  filter(Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
colnames(pg_EU_cruise_chemistry_east_clean)
data.table::fwrite(pg_EU_cruise_chemistry_east_clean, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Chemistry_east.csv")
data.table::fwrite(pg_EU_cruise_chemistry_east_clean, "pg_EU_data/pg_EU_cruise_Chemistry_east.csv")
rm(pg_EU_cruise_chemistry_east_dl, pg_EU_cruise_chemistry_east_clean); gc()


## EU Arctic CTD data on PANGAEA - 933 - Some issues
## NB: ~20 minutes
pg_EU_CTD <- pg_full_search(query = "CTD", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_CTD$doi)))
pg_EU_CTD_dl <- plyr::ldply(pg_EU_CTD$doi, pg_dl_proc)
pg_EU_CTD_clean <- pg_EU_CTD_dl %>%
  select(-contains(c("dpm/m", "Perc ", "Ratio ", "14C ", "Bact ", "heterotrophic", "DSI", "kappa",
                     "University", "Woods ", "Geological", "GSI", "HSI", "C15", "C17", "C19",
                     "Dinosterol", "Brassicasterol", "Foram", "Calibration ", "Chronozone",
                     "T tech ", "delta T ", "Species", "Spec code", "Chromatographic", "NOBS",
                     "Method comm ", "Mortality", "Microscopy", "Bacteria", "Pore size", 
                     "umbilical", "/TOC", "size fraction", "Bathy ", "pixel ", "Data record",
                     "Prasinophytes", "Chrysophytes", "MATLAB", "Total counts", "Locality",
                     "T-RF", "algae", "Picophytopl", "Cryptophytes", "δ13C", "Comm resp ", "Size ",
                     "Phylum", "Kingdom", "alpha ", "Rotaliina", "bacp660 ", "Pyrophytin a ",
                     "Phytin a ", "Phide a ", "Grain size ", "Status", "Sidesc", "Haptophytes ", 
                     "/Chl a", "ASAL ", "Fe ", "Sensitivity", "POC", "GCP", "Aphanizo ", "Conf ",
                     "Phytopl ", "CO3", "CSC", "Biogeograph", "E bur ", "DOF", "POC", "Seastate",
                     "228Ra", "228Th", "Transmission ", "beta470", "bac660 ", "DOS ", "LSi ",
                     "Mn2+ ", "Al ", "Ca ", "Phae ", "Acetate ", "C carb part ", "Sigma500",
                     "Fragilariopsis", "CSP", " TEP", "ESD", "SA dome ", "Vol dome ", "Catalog",
                     "Phaeodaria", "Dated material", "Age dated", "Basis", "Microzoopl", "δ18O",
                     "Frac Factor"))) %>%
  filter(Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
colnames(pg_EU_CTD_clean)
write_csv(pg_EU_CTD_clean, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_CTD.csv")
write_csv(pg_EU_CTD_clean, "pg_EU_data/pg_EU_CTD.csv")
rm(pg_EU_CTD_dl, pg_EU_CTD_clean); gc()

# Load file
# pg_EU_ctd_all_clean <- data.table::fread("~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_ctd_all.csv", nThread = 15)

# Save DOI list
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")


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

## All Kongsfjorden bbox data files - 3227
pg_kong_bbox <- pg_full_search(query = "", bbox = c(11, 78.86, 12.69, 79.1)) %>% # 3243 files
  filter(!doi %in% pg_doi_list$doi)
pg_kong_name_1 <- pangaear::pg_search(query = "kongsfjord", count = 500) %>% # 8 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_kong_bbox$doi)
pg_kong_name_2 <- pangaear::pg_search(query = "kongsfjorden", count = 500) %>% # 20 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_kong_bbox$doi, !doi %in% pg_kong_name_1$doi)
pg_kong_name_3 <- pg_full_search(query = "ny alesund", bbox = c(11, 78.86, 12.69, 79.1)) %>% # 0 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_kong_bbox$doi, !doi %in% pg_kong_name_1$doi, !doi %in% pg_kong_name_2$doi)
pg_kong_name_4 <- pg_full_search(query = "ny-alesund", bbox = c(11, 78.86, 12.69, 79.1)) %>% # 0 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_kong_bbox$doi, !doi %in% pg_kong_name_1$doi, 
         !doi %in% pg_kong_name_2$doi, !doi %in% pg_kong_name_3$doi)
pg_kong_all <- rbind(pg_kong_bbox, pg_kong_name_1, pg_kong_name_2, pg_kong_name_3, pg_kong_name_4) %>% 
  filter(!grepl("core", citation), !grepl("Core", citation), 
         !grepl("video", citation), !grepl("Video", citation), 
         !grepl("photograph", citation), !grepl("Photograph", citation), 
         !grepl("image", citation), !grepl("Image", citation)) %>% 
  arrange(citation) %>% 
  distinct()
rm(pg_kong_bbox, pg_kong_name_1, pg_kong_name_2, pg_kong_name_3, pg_kong_name_4); gc()

# Append DOI list
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_kong_all$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")

# Download files
pg_kong_dl <- plyr::ldply(pg_kong_all$doi, pg_dl_proc)
pg_kong_clean <- pg_kong_dl %>%
  select(-contains(c("Lu_", "dN/dlogDp", "MeSO2-PCB ", "3-MeSO2-pp-DDE ", "Bryozoa",
                     " cov ", " biom wm ", "AND ", "16b-OHT ", "6b-OHT ", "δ18O", "δ13C",
                     "Fatty alc ", "NOBS ", "Method comm ", "Particles ", "Flag",
                     "juveniles", "Description", "Species", "#/m", "ID ", "clays",
                     "g/m", "[#]", " indet ", "Size algal disc ")))
                     # , "Perc ", "Particles ",
                     # "Size algal disc ", "Pogonophora", "Polychaeta", "Tunicata", 
                     # "Sipunculida", "Crustacea", "Tablets ", "Taxa", "Flag",
                     # "/TOC", "Turbellaria", "Tachidiidae", "Oligochaeta", "Harpacticoida",
                     # "Oedicerotidae", "Persistent Identifier", "Data ", "Fossils"))) #%>%
  # filter(Longitude >= 11, Longitude <= 12.69, Latitude >= 78.86, Latitude <= 79.1)
colnames(pg_kong_clean)
data.table::fwrite(pg_kong_clean, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong.csv")
data.table::fwrite(pg_kong_clean, "data/pg_kong_data/pg_kong.csv")
rm(pg_kong_dl, pg_kong_clean); gc()

# NB: Most bathymetry files are just links to other files so aren't downloaded here
# They can be searched for directly with the query "bathymetry"

# Glacier length change and mass balance data
# NB: This is an EU file
pg_EU_World_Glacier <- plyr::ldply(pg_kong_all$doi[grepl("World Glacier", pg_kong_all$citation)], pg_dl_proc)
write_csv(pg_EU_World_Glacier, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_World_Glacier.csv")
write_csv(pg_EU_World_Glacier, "data/pg_EU_data/pg_EU_World_Glacier.csv")


# Isfjorden ---------------------------------------------------------------

## All Isfjorden data files - 350
pg_is_bbox <- pg_full_search(query = "", bbox = c(13.62, 78.03, 17.14, 78.71)) %>% # 298 files
  filter(!doi %in% pg_doi_list$doi)
pg_is_name_1 <- pangaear::pg_search(query = "isfjord", count = 500) %>% # 10 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_is_bbox$doi)
pg_is_name_2 <- pangaear::pg_search(query = "isfjorden", count = 500) %>% # 2 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_is_bbox$doi, !doi %in% pg_is_name_1$doi)
pg_is_name_3 <- pangaear::pg_search(query = "longyearbyen", count = 500) %>% # 369 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_is_bbox$doi, !doi %in% pg_is_name_1$doi, !doi %in% pg_is_name_2$doi)
pg_is_all <- rbind(pg_is_bbox, pg_is_name_1, pg_is_name_2, pg_is_name_3) %>% 
  filter(!grepl("core", citation), !grepl("Core", citation), 
         !grepl("video", citation), !grepl("Video", citation), 
         !grepl("photograph", citation), !grepl("Photograph", citation), 
         !grepl("image", citation), !grepl("Image", citation)) %>% 
  arrange(citation) %>% 
  distinct()
rm(pg_is_bbox, pg_is_name_1, pg_is_name_2, pg_is_name_3); gc()

# Update DOI list with Isfjorden
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_is_all$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")

# Download files
pg_is_dl <- plyr::ldply(pg_is_all$doi, pg_dl_proc)
pg_is_clean <- pg_is_dl %>%
  select(-contains(c("Diatom zone ", "Diatom spore ", "Diatoms indet ", "Liradiscus sp",
                     " indet ", "[#]", "Thick [mm]", "Biom std", "Plants vasc biom",
                     "Arom HC", "EF ", "N subset", "LCC ", "Perc ", "dN/dlogDp ",
                     "#/m", "#/10", "Sum [%]", "Method comm ")))
colnames(pg_is_clean)
data.table::fwrite(pg_is_clean, "~/pCloudDrive/FACE-IT_data/isfjorden/pg_is.csv")
data.table::fwrite(pg_is_clean, "data/pg_is_data/pg_is.csv")
rm(pg_is_dl, pg_is_clean); gc()


# Inglefieldbukta ---------------------------------------------------------

## All Inglefieldbukta data files - 15
pg_ingle_bbox <- pg_full_search(query = "", bbox = c(18.15, 77.87, 18.79, 78.05)) %>% # 15 files
  filter(!doi %in% pg_doi_list$doi)
pg_ingle_name_1 <- pangaear::pg_search(query = "inglefieldbukta", count = 500) # 0 files
pg_ingle_all <- rbind(pg_ingle_bbox, pg_ingle_name_1) %>% 
  filter(!grepl("core", citation), !grepl("Core", citation), 
         !grepl("video", citation), !grepl("Video", citation), 
         !grepl("photograph", citation), !grepl("Photograph", citation), 
         !grepl("image", citation), !grepl("Image", citation)) %>% 
  arrange(citation) %>% 
  distinct()
rm(pg_ingle_bbox, pg_ingle_name_1); gc()

# Update DOI list with Inglefieldbukta
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_ingle_all$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")

# Download files
pg_ingle_dl <- plyr::ldply(pg_ingle_all$doi, pg_dl_proc)
pg_ingle_clean <- pg_ingle_dl %>%
  select(-contains(c("N subset ", "OH-PCB ")))
colnames(pg_ingle_clean)
data.table::fwrite(pg_ingle_clean, "~/pCloudDrive/FACE-IT_data/inglefieldbukta/pg_ingle.csv")
data.table::fwrite(pg_ingle_clean, "data/pg_ingle_data/pg_ingle.csv")
rm(pg_ingle_dl, pg_ingle_clean); gc()


# Svalbard ----------------------------------------------------------------

## All Svalbard data files - 271
# NB: These files were searched for after the specific sites intentionally
# This was so that site specific files would be allocated to the appropriate folders
# And the files not attributed to a given site would be downloaded in this chunk
# However, I'm currently thinking we don't want these data...
pg_sval_all <- pangaear::pg_search(query = "svalbard", count = 500, bbox = c(9, 76, 30, 81)) %>% 
  filter(!grepl("core", citation), !grepl("video", citation), !grepl("photograph", citation)) %>% 
  filter(!doi %in% pg_doi_list$doi) %>%  arrange(citation)

# Svalbard team files - 10 datasets


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
write_csv(pg_EU_Bartsch, "pg_EU_data/pg_EU_Bartsch.csv")

# Permafrost borehole characteristics
# NB: This is an EU file
pg_EU_Christiansen <- plyr::ldply(pg_young_all$doi[grepl("Christiansen", substr(pg_young_all$citation, 1, 12))], pg_dl_proc)
write_csv(pg_EU_Christiansen, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_Christiansen.csv")
write_csv(pg_EU_Christiansen, "pg_EU_data/pg_EU_Christiansen.csv")

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
write_csv(pg_EU_Hatton, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_Hatton.csv")
write_csv(pg_EU_Hatton, "pg_EU_data/pg_EU_Hatton.csv")

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


# code/data_collection.R
# The location of collected data and the code used when possible


# Setup -------------------------------------------------------------------

# TODO: Figure out how to combine lookup tables in order to get lon/lat/date values for cruise data
# Be more strict about the removal of empty columns when they are an unknown format

# Libraries used in this script
source("code/functions.R")

# Previously downloaded PANGAEA data
# pg_files <- dir("~/pCloudDrive/FACE-IT_data", pattern = "pg_", recursive = T, full.names = T)
# pg_files <- pg_files[grepl(".csv", pg_files)]
# pg_doi_list <- read_csv("~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")


# Functions ---------------------------------------------------------------

# Function for printing PANGAEA meta-data
pg_meta_print <- function(pg_doi){
  pg_test <- pangaear::pg_data(pg_doi)
}

# Function for extracting info from PANGAEA data
pg_dl_prep <- function(pg_dl){
  # Extract data.frame and attach URL + citation
  if(is.data.frame(pg_dl$data)){
    if(length(unique(colnames(pg_dl$data))) == length(colnames(pg_dl$data))){
      if("Longitude" %in% colnames(pg_dl$data)){
        col_idx <- colnames(pg_dl$data)[colnames(pg_dl$data) %in% unique(query_ALL$pg_col_name)]
        dl_single <- pg_dl$data %>% 
          dplyr::select(all_of(col_idx)) %>%  
          # mutate_all(~na_if(., '')) %>% # This will throw errors from unknown column types
          janitor::remove_empty(which = c("rows", "cols")) %>% 
          mutate(date_accessed = as.Date(Sys.Date()),
                 URL = pg_dl$url,
                 citation = pg_dl$citation,
                 Longitude = case_when(as.numeric(Longitude) > 180 ~ as.numeric(Longitude)-360, 
                                       TRUE ~ as.numeric(Longitude))) %>% 
          filter(Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90) %>% 
          dplyr::select(date_accessed, URL, citation, everything())
      } else {
        dl_single <- NULL
      }
      # Check for any useful columns
      if(all(colnames(dl_single) %in% c("date_accessed", "URL", "citation", "Longitude", "Latitude", "Date/Time"))){
        dl_single <- NULL
      }
    } else {
      dl_single <- NULL
    }
  } else {
    dl_single <- NULL
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
pg_full_search <- function(lookup_table = F, ...){
  pg_res_all <- data.frame()
  # query_min_score <- 100
  query_offset <- 0
  while(query_offset < 10000){
    pg_res_query <- pangaear::pg_search(count = 500, offset = query_offset, ...)
    pg_res_all <- rbind(pg_res_all, pg_res_query)
    query_offset <- query_offset+500
    # query_min_score <- min(pg_EU_cruise_all$score)
  }
  if(lookup_table){
    pg_res_all <- distinct(arrange(pg_res_all, citation)) %>% 
      filter(grepl("station list|master tracks|metadata list|links to file", citation, ignore.case = T))
  } else {
    pg_res_all <- distinct(arrange(pg_res_all, citation)) %>% 
      filter(!grepl("video|photograph|image|station list|master tracks|aircraft|flight|
                    |airborne|metadata list|core|links to file|Multibeam survey|Radiosonde", citation, ignore.case = T)) %>% 
      filter(!grepl("ACLOUD|SOCAT", citation)) %>% 
      filter(!grepl("WOCE", citation)) # The WOCE data have formatting issues and should be downloaded via their own portal
  }
  return(pg_res_all)
}

# Function for quickly opening up a file based on doi
pg_test_dl <- function(pg_doi){
  # Get data
  dl_dat <- tryCatch(pg_data(pg_doi), error = function(pg_doi) stop("Download failed"))
  dl_single <- data.frame(URL = dl_dat[[1]]$url,
                          citation = dl_dat[[1]]$citation,
                          dl_dat[[1]]$data)
  return(dl_single)
}


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
# pg_Riebesell <- pg_full_search(query = "Riebesell", bbox = c(-60, 63, 60, 90))
# pg_Fransson <- pg_full_search(query = "Fransson", bbox = c(-60, 63, 60, 90))
# pg_Chierici <- pg_full_search(query = "Chierici", bbox = c(-60, 63, 60, 90))
# pg_Fischer <- pg_full_search(query = "Chierici", bbox = c(-60, 63, 60, 90))


# Test specific files
pg_test_1 <- pg_data(doi = "10.1594/PANGAEA.857405")[[2]]$data
pg_test_2 <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.774421")
pg_test_3 <- pg_test_dl("10.1594/PANGAEA.774421")


## EU Arctic cruise Oceans data on PANGAEA - 289 - Some issues
# NB: The following PG downloads have rows with missing lon/lat values
# This is a conscious choice for now because the spatial info may
# be stored in a different column name and we don't want to lose data here
pg_EU_cruise_oceans <- pg_full_search(query = "cruise", topic = "Oceans", bbox = c(-60, 63, 60, 90)) 
pg_doi_list <- distinct(data.frame(doi = pg_EU_cruise_oceans$doi))
system.time(
  pg_EU_cruise_oceans_dl <- plyr::ldply(pg_EU_cruise_oceans$doi, pg_dl_proc)
) # 163 seconds
# Get lookup table
# Combine and filter by coords
pg_EU_cruise_oceans_trim <- filter(pg_EU_cruise_oceans_dl, Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
data.table::fwrite(pg_EU_cruise_oceans_trim, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Oceans.csv")
data.table::fwrite(pg_EU_cruise_oceans_trim, "data/pg_data/pg_EU_cruise_Oceans.csv")
rm(pg_EU_cruise_oceans_dl, pg_EU_cruise_oceans_trim); gc()


## EU Arctic cruise Atmosphere data on PANGAEA - 141 - Some issues
# ~3 minutes
# NB: More than 90% of these files do not have lon/lat values so they get removed...
pg_EU_cruise_atmosphere <- pg_full_search(query = "cruise", topic = "Atmosphere", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_atmosphere$doi)))
pg_EU_cruise_atmosphere_dl <- plyr::ldply(pg_EU_cruise_atmosphere$doi, pg_dl_proc) 
pg_EU_cruise_atmosphere_trim <- filter(pg_EU_cruise_atmosphere_dl, Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
data.table::fwrite(pg_EU_cruise_atmosphere_trim, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Atmosphere.csv")
data.table::fwrite(pg_EU_cruise_atmosphere_trim, "data/pg_data/pg_EU_cruise_Atmosphere.csv")
rm(pg_EU_cruise_atmosphere_dl, pg_EU_cruise_atmosphere_trim); gc()


## EU Arctic cruise Cryosphere data on PANGAEA - 13
pg_EU_cruise_cryosphere <- pg_full_search(query = "cruise", topic = "Cryosphere", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_cryosphere$doi)))
pg_EU_cruise_cryosphere_dl <- plyr::ldply(pg_EU_cruise_cryosphere$doi, pg_dl_proc)
pg_EU_cruise_cryosphere_trim <- filter(pg_EU_cruise_cryosphere_dl, Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
data.table::fwrite(pg_EU_cruise_cryosphere_trim, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Cryosphere.csv")
data.table::fwrite(pg_EU_cruise_cryosphere_trim, "data/pg_data/pg_EU_cruise_Cryosphere.csv")
rm(pg_EU_cruise_cryosphere_dl, pg_EU_cruise_cryosphere_trim); gc()


## EU Arctic cruise Biological Classification data on PANGAEA - 295 - Some issues
# ~3 minutes
pg_EU_cruise_bio_class <- pg_full_search(query = "cruise", topic = "Biological Classification", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_bio_class$doi)))
pg_EU_cruise_bio_class_dl <- plyr::ldply(pg_EU_cruise_bio_class$doi, pg_dl_proc)
pg_EU_cruise_bio_class_trim <- filter(pg_EU_cruise_bio_class_dl, Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
data.table::fwrite(pg_EU_cruise_bio_class_trim, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Bio_class.csv")
data.table::fwrite(pg_EU_cruise_bio_class_trim, "data/pg_data/pg_EU_cruise_Bio_class.csv")
rm(pg_EU_cruise_bio_class_dl, pg_EU_cruise_bio_class_trim); gc()


## EU Arctic cruise Biosphere data on PANGAEA - 3
pg_EU_cruise_biosphere <- pg_full_search(query = "cruise", topic = "Biosphere", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_biosphere$doi)))
pg_EU_cruise_biosphere_dl <- plyr::ldply(pg_EU_cruise_biosphere$doi, pg_dl_proc)
pg_EU_cruise_biosphere_trim <- filter(pg_EU_cruise_biosphere_dl, Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
data.table::fwrite(pg_EU_cruise_biosphere_trim, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Biosphere.csv")
data.table::fwrite(pg_EU_cruise_biosphere_trim, "data/pg_data/pg_EU_cruise_Biosphere.csv")
rm(pg_EU_cruise_biosphere_dl, pg_EU_cruise_biosphere_trim); gc()


## EU Arctic cruise Ecology data on PANGAEA - 768 - Some issues
## NB: Takes ~19 minutes
pg_EU_cruise_ecology <- pg_full_search(query = "cruise", topic = "Ecology", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_ecology$doi)))
pg_EU_cruise_ecology_dl <- plyr::ldply(pg_EU_cruise_ecology$doi, pg_dl_proc)
pg_EU_cruise_ecology_trim <- filter(pg_EU_cruise_ecology_dl, Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
data.table::fwrite(pg_EU_cruise_ecology_trim, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Ecology.csv")
data.table::fwrite(pg_EU_cruise_ecology_trim, "data/pg_data/pg_EU_cruise_Ecology.csv")
rm(pg_EU_cruise_ecology_dl, pg_EU_cruise_ecology_trim); gc()


## EU Arctic cruise Human Dimensions data on PANGAEA - 0
pg_EU_cruise_human <- pg_full_search(query = "cruise", topic = "Human Dimensions", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)


## EU Arctic cruise Chemistry data on PANGAEA west - 3815 - Some issues
## MB ~20 minutes
pg_EU_cruise_chemistry_west <- pg_full_search(query = "cruise", topic = "Chemistry", bbox = c(-60, 63, 0, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_chemistry_west$doi)))
pg_EU_cruise_chemistry_west_dl <- plyr::ldply(pg_EU_cruise_chemistry_west$doi, pg_dl_proc)
pg_EU_cruise_chemistry_west_trim <- filter(pg_EU_cruise_chemistry_west_dl, Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
data.table::fwrite(pg_EU_cruise_chemistry_west_trim, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Chemistry_west.csv")
data.table::fwrite(pg_EU_cruise_chemistry_west_trim, "data/pg_data/pg_EU_cruise_Chemistry_west.csv")
rm(pg_EU_cruise_chemistry_west_dl, pg_EU_cruise_chemistry_west_trim); gc()


## EU Arctic cruise Chemistry data on PANGAEA east - 8005 - Some issues
## NB: ~58 minutes
pg_EU_cruise_chemistry_east <- pg_full_search(query = "cruise", topic = "Chemistry", bbox = c(0, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_cruise_chemistry_east$doi)))
pg_EU_cruise_chemistry_east_dl <- plyr::ldply(pg_EU_cruise_chemistry_east$doi, pg_dl_proc)
pg_EU_cruise_chemistry_east_trim <- filter(pg_EU_cruise_chemistry_east_dl, Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
# test1 <- data.frame(table(pg_EU_cruise_chemistry_east_trim$citation)) # Investigate which files contribute the most size
data.table::fwrite(pg_EU_cruise_chemistry_east_trim, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Chemistry_east.csv")
data.table::fwrite(pg_EU_cruise_chemistry_east_trim, "data/pg_data/pg_EU_cruise_Chemistry_east.csv")
rm(pg_EU_cruise_chemistry_east_dl, pg_EU_cruise_chemistry_east_trim); gc()


## EU Arctic CTD data on PANGAEA - 949 - Some issues
## NB: ~20 minutes
pg_EU_CTD <- pg_full_search(query = "CTD", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_EU_CTD$doi)))
pg_EU_CTD_dl <- plyr::ldply(pg_EU_CTD$doi, pg_dl_proc)
pg_EU_CTD_trim <- filter(pg_EU_CTD_dl, Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
data.table::fwrite(pg_EU_CTD_trim, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_CTD.csv")
data.table::fwrite(pg_EU_CTD_trim, "data/pg_data/pg_EU_CTD.csv") # 6.5 GB
rm(pg_EU_CTD_dl, pg_EU_CTD_trim); gc()


## Save DOI list
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")
rm(list = ls()[grep("pg_EU", ls())]); gc()


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

## All Kongsfjorden bbox data files - 2854
# NB: ~48 minutes
pg_kong_bbox <- pg_full_search(query = "", bbox = c(11, 78.86, 12.69, 79.1)) %>% # 2854 files
  filter(!doi %in% pg_doi_list$doi)
pg_kong_name_1 <- pg_full_search(query = "kongsfjord") %>% # 7 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_kong_bbox$doi)
pg_kong_name_2 <- pg_full_search(query = "kongsfjorden") %>% # 13 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_kong_bbox$doi, !doi %in% pg_kong_name_1$doi)
pg_kong_name_3 <- pg_full_search(query = "ny alesund") %>% # 11 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_kong_bbox$doi, !doi %in% pg_kong_name_1$doi, !doi %in% pg_kong_name_2$doi)
pg_kong_name_4 <- pg_full_search(query = "ny-alesund") %>% # 0 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_kong_bbox$doi, !doi %in% pg_kong_name_1$doi, 
         !doi %in% pg_kong_name_2$doi, !doi %in% pg_kong_name_3$doi)
pg_kong_all <- rbind(pg_kong_bbox, pg_kong_name_1, pg_kong_name_2, pg_kong_name_3, pg_kong_name_4) %>%
  filter(!doi %in% c("10.1594/PANGAEA.909130")) %>% # Wide file with no date values
  filter(!grepl("Multibeam survey|Radiosonde", citation, ignore.case = T)) %>% # This removes ~40 million rows of bathy data
  arrange(citation) %>% distinct()
rm(pg_kong_bbox, pg_kong_name_1, pg_kong_name_2, pg_kong_name_3, pg_kong_name_4); gc()

# Download files
system.time(
pg_kong_dl <- plyr::ldply(pg_kong_all$doi, pg_dl_proc)
) # 38 minutes
# colnames(pg_kong_dl)
# test1 <- data.frame(table(pg_kong_dl$citation)) # Investigate which files contribute the most size
# test2 <- filter(pg_kong_dl, grepl("914973", citation)) %>% janitor::remove_empty(which = "cols")
# NB: Can't filter these as they may contain data points used in another site file
# pg_kong_trim <- filter(pg_kong_dl, Longitude >= 11, Longitude <= 12.69, Latitude >= 78.86, Latitude <= 79.1) %>% 
  # janitor::remove_empty(which = "cols")
data.table::fwrite(pg_kong_dl, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong.csv")
data.table::fwrite(pg_kong_dl, "data/pg_data/pg_kong.csv")
rm(pg_kong_dl); gc()

# Append DOI list
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_kong_all$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")


# Isfjorden ---------------------------------------------------------------

## All Isfjorden data files - 793
pg_is_bbox <- pg_full_search(query = "", bbox = c(13.62, 78.03, 17.14, 78.71)) %>% # 251 files
  filter(!doi %in% pg_doi_list$doi)
pg_is_name_1 <- pg_full_search(query = "isfjord") %>% # 1 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_is_bbox$doi)
pg_is_name_2 <- pg_full_search(query = "isfjorden") %>% # 0 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_is_bbox$doi, !doi %in% pg_is_name_1$doi)
pg_is_name_3 <- pg_full_search(query = "longyearbyen") %>% # 664 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_is_bbox$doi, !doi %in% pg_is_name_1$doi, !doi %in% pg_is_name_2$doi)
pg_is_all <- rbind(pg_is_bbox, pg_is_name_1, pg_is_name_2, pg_is_name_3) %>% 
  filter(!doi %in% c("10.1594/PANGAEA.909130")) %>% # Wide file with no date values
  filter(!grepl("Multibeam survey|Radiosonde", citation, ignore.case = T)) %>% # This removes ~40 million rows of bathy data
  filter(!grepl("WOCE", citation)) %>% # The WOCE data have formatting issues and should be downloaded via there own portal
  arrange(citation) %>% distinct()
rm(pg_is_bbox, pg_is_name_1, pg_is_name_2, pg_is_name_3); gc()

# Download files
system.time(
pg_is_dl <- plyr::ldply(pg_is_all$doi, pg_dl_proc)
) # 15 minutes
# colnames(pg_is_dl)
# test1 <- data.frame(table(pg_is_dl$citation)) # Investigate which files contribute the most size
# test2 <- filter(pg_kong_dl, grepl("914973", citation)) %>% janitor::remove_empty(which = "cols")
# pg_is_trim <- filter(pg_is_dl, Longitude >= 13.62, Longitude <= 17.14, Latitude >= 78.03, Latitude <= 78.71)
data.table::fwrite(pg_is_dl, "~/pCloudDrive/FACE-IT_data/isfjorden/pg_is.csv")
data.table::fwrite(pg_is_dl, "data/pg_data/pg_is.csv")
rm(pg_is_dl); gc()

# Update DOI list with Isfjorden
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_is_all$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")


# Inglefieldbukta ---------------------------------------------------------

## All Inglefieldbukta data files - 14
pg_ingle_bbox <- pg_full_search(query = "", bbox = c(18.15, 77.87, 18.79, 78.05)) %>% # 47 files
  filter(!doi %in% pg_doi_list$doi)
# pg_ingle_name_1 <- pg_full_search(query = "inglefieldbukta") # 0 files
pg_ingle_all <- rbind(pg_ingle_bbox) %>% 
  filter(!doi %in% c("10.1594/PANGAEA.909130")) %>% # Wide file with no date values
  filter(!grepl("Multibeam survey|Radiosonde", citation, ignore.case = T)) %>% # This removes ~40 million rows of bathy data
  filter(!grepl("WOCE", citation)) %>% # The WOCE data have formatting issues and should be downloaded via their own portal
  arrange(citation) %>% distinct()
rm(pg_ingle_bbox, pg_ingle_name_1); gc()

# Download files
system.time(
pg_ingle_dl <- plyr::ldply(pg_ingle_all$doi, pg_dl_proc)
) # 5 seconds
# pg_ingle_trim <- filter(pg_ingle_dl, Longitude >= 18.15, Longitude <= 18.79, Latitude >= 77.87, Latitude <= 78.05) # Reduces to 0...
data.table::fwrite(pg_ingle_dl, "~/pCloudDrive/FACE-IT_data/inglefieldbukta/pg_ingle.csv")
data.table::fwrite(pg_ingle_dl, "data/pg_data/pg_ingle.csv")
rm(pg_ingle_dl); gc()

# Update DOI list with Inglefieldbukta
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_ingle_all$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")


# Svalbard ----------------------------------------------------------------

## All Svalbard data files - 348
# NB: These files were searched for after the specific sites intentionally
# This was so that site specific files would be allocated to the appropriate folders
# And the files not attributed to a given site would be downloaded in this chunk
# However, I'm currently thinking we don't want these data...
pg_sval_all <- pg_full_search(query = "", bbox = c(9, 76, 30, 81)) %>% 
  filter(!doi %in% pg_doi_list$doi) %>% arrange(citation) %>% distinct()

# Svalbard team files - 10 datasets
rm(pg_sval_all); gc()


# Young Sound -------------------------------------------------------------

## All Young Sound data files - 180
pg_young_bbox <- pg_full_search(query = "", bbox = c(-22.367917, 74.210137, -19.907644, 74.624304)) %>% # 184 files
  filter(!doi %in% pg_doi_list$doi)
pg_young_name_1 <- pg_full_search(query = "zackenberg") %>% # 3 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_young_bbox$doi)
pg_young_all <- rbind(pg_young_bbox, pg_young_name_1) %>% 
  filter(!grepl("WOCE", citation)) %>% # The WOCE data have formatting issues and should be downloaded via their own portal
  filter(!doi == "10.1594/PANGAEA.786674") %>%   # This file causes weird date issues and doesn't have any key drivers
  arrange(citation) %>% distinct()
rm(pg_young_bbox, pg_young_name_1); gc()

# Download files
system.time(
pg_young_dl <- plyr::ldply(pg_young_all$doi, pg_dl_proc)
) # 317 seconds
# test1 <- data.frame(table(pg_young_dl$citation)) # Investigate which files contribute the most size
# pg_young_trim <- filter(pg_young_dl, Longitude >= -22.367917, Longitude <= -19.907644, Latitude >= 74.210137, Latitude <= 74.624304)
data.table::fwrite(pg_young_dl, "~/pCloudDrive/FACE-IT_data/young_sound/pg_young.csv")
data.table::fwrite(pg_young_dl, "data/pg_data/pg_young.csv")
rm(pg_young_dl); gc()

# Update DOI list with Young Sound
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_young_all$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")


# Disko Bay ---------------------------------------------------------------

## All Disko Bay data files - 238
pg_disko_bbox <- pg_full_search(query = "", bbox = c(-55.56, 68.22, -49.55, 70.5)) %>% # 247 files
  filter(!doi %in% pg_doi_list$doi)
pg_disko_name_1 <- pg_full_search(query = "Qeqertarsuup") %>% # 0 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_disko_bbox$doi)
pg_disko_name_2 <- pg_full_search(query = "disko bay") %>% # 12 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_disko_bbox$doi)
pg_disko_name_3 <- pg_full_search(query = "disko_bay") %>% # 0 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_disko_bbox$doi, !doi %in% pg_disko_name_2$doi)
pg_disko_all <- rbind(pg_disko_bbox, pg_disko_name_1, pg_disko_name_2, pg_disko_name_3) %>% 
  filter(!grepl("sea level", citation)) %>% 
  filter(!doi %in% c("10.1594/PANGAEA.770250", "10.1594/PANGAEA.770249")) %>% # These two files are too massive
  filter(!doi %in% c("10.1594/PANGAEA.770247", "10.1594/PANGAEA.770248")) %>% # These are simple bathy files
  filter(!grepl("WOCE", citation)) %>% # The WOCE data have formatting issues and should be downloaded via their own portal
                     # "10.1594/PANGAEA.847501", "10.1594/PANGAEA.905012" # These two have broken date columns that can't be reconciled
  arrange(citation) %>% distinct()
rm(pg_disko_bbox, pg_disko_name_1, pg_disko_name_2, pg_disko_name_3); gc()

# Download files
system.time(
pg_disko_dl <- plyr::ldply(pg_disko_all$doi, pg_dl_proc)
) # 252 seconds
# colnames(pg_disko_dl)
# test1 <- data.frame(table(pg_disko_dl$citation)) # Investigate which files contribute the most size
data.table::fwrite(pg_disko_dl, "~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko.csv")
data.table::fwrite(pg_disko_dl, "data/pg_data/pg_disko.csv")
rm(pg_disko_dl); gc()

# Update DOI list with Disko Bay
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_disko_all$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")


# Nuup Kangerlua ----------------------------------------------------------

## All Nuup Kangerlua data files - 199
pg_nuup_bbox <- pg_full_search(query = "", bbox = c(-53.32, 64.01, -48.93, 64.8)) %>% # 156 files
  filter(!doi %in% pg_doi_list$doi)
# pg_nuup_name_1 <- pg_full_search(query = "kangerlua") # 0 files
pg_nuup_name_2 <- pg_full_search(query = "nuuk") %>% # 74 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_nuup_bbox$doi)
pg_nuup_all <- rbind(pg_nuup_bbox, pg_nuup_name_2) %>% 
  filter(!grepl("WOCE", citation)) %>% # The WOCE data have formatting issues and should be downloaded via their own portal
  arrange(citation) %>% distinct()
rm(pg_nuup_bbox, pg_nuup_name_1, pg_nuup_name_2); gc()

# Download files
system.time(
pg_nuup_dl <- plyr::ldply(pg_nuup_all$doi, pg_dl_proc)
) # 227 seconds
data.table::fwrite(pg_nuup_dl, "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/pg_nuup.csv")
data.table::fwrite(pg_nuup_dl, "data/pg_data/pg_nuup.csv")
rm(pg_nuup_dl); gc()

# Update DOI list with Nuup Kangerlua
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_nuup_all$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")


# Porsangerfjorden --------------------------------------------------------

## All Porsangerfjorden data files - 76
pg_por_bbox <- pg_full_search(query = "", bbox = c(24.5, 70, 27, 71.2)) %>% # 107 files
  filter(!doi %in% pg_doi_list$doi)
pg_por_name_1 <- pg_full_search(query = "Porsangerfjord") %>% # 0 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_por_bbox$doi)
pg_por_all <- rbind(pg_por_bbox, pg_por_name_1) %>% 
  filter(!grepl("Multibeam survey", citation, ignore.case = T)) %>% # This removes ~7 million rows of bathy data
  filter(!grepl("WOCE", citation)) %>% # The WOCE data have formatting issues and should be downloaded via their own portal
  arrange(citation) %>% distinct()
rm(pg_por_bbox, pg_por_name_1); gc()

# Download files
system.time(
pg_por_dl <- plyr::ldply(pg_por_all$doi, pg_dl_proc)
) # 44 seconds
# colnames(pg_por_dl)
# test1 <- data.frame(table(pg_por_dl$citation)) # Investigate which files contribute the most size
data.table::fwrite(pg_por_dl, "~/pCloudDrive/FACE-IT_data/porsangerfjorden/pg_por.csv")
data.table::fwrite(pg_por_dl, "data/pg_data/pg_por.csv")
rm(pg_por_dl); gc()

# Update DOI list with Porsangerfjord
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_por_all$doi)))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")


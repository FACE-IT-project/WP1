# code/data_collection.R
# The location of collected data and the code used when possible
# This is primarily used for the collection of the PANGAEA data
# It also contains the code used to subset and save hi-res gridded ice cover data per site


# Setup -------------------------------------------------------------------

# TODO: Figure out how to combine lookup tables in order to get lon/lat/date values for cruise data
# Be more strict about the removal of empty columns when they are an unknown format
# Consider using powerjoin for some of the issues caused by the pangaeaR package
# https://cran.r-project.org/web/packages/powerjoin/readme/README.html

# Libraries used in this script
source("code/functions.R")
source("code/key_drivers.R")

# Set cores
doParallel::registerDoParallel(cores = 15)

# Ice NetCdf locations
ice_1km_files <- dir("~/pCloudDrive/FACE-IT_data/ice/MASIE_1km", pattern = "masie_all", full.names = TRUE, recursive = T)
ice_4km_files <- dir("~/pCloudDrive/FACE-IT_data/ice/MASIE_4km", pattern = "masie_all", full.names = TRUE, recursive = T)

# Load MASIE coordinate systems
ice_coords_4km <- tidync::tidync("~/pCloudDrive/FACE-IT_data/ice/MASIE_4km/masie_lat_lon_4km.nc") %>% 
  tidync::hyper_tibble() %>% dplyr::rename(lon = longitude, lat = latitude)
# NB: This is too large to load
# ice_coords_1km <- tidync::tidync("~/pCloudDrive/FACE-IT_data/ice/MASIE_1km/masie_lat_lon_1km.nc") %>% tidync::hyper_tibble() %>%
#   dplyr::rename(lon = longitude, lat = latitude)
# It is necessary to only load the necessary subset. This is done per site below.
# Ice mask values
## 0 = missing/not sea ice
## 1 = ocean
## 2 = land
## 3 = sea ice
## 4 = coastline (land adjacent to ocean)
## 5 = lake
##6 = border of region images 

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
  
  # Prep for error reporting
  dl_error <- NULL
  
  # Extract data.frame or catch specific errors
  if(is.data.frame(pg_dl$data)){
    if(length(unique(colnames(pg_dl$data))) == length(colnames(pg_dl$data))){
      if("Longitude" %in% colnames(pg_dl$data) & "Latitude" %in% colnames(pg_dl$data)){
        if("Latitude 2" %in% colnames(pg_dl$data) & sum(grepl("Latitude", colnames(pg_dl$data))) == 1){
          colnames(pg_dl$data)[which(colnames(pg_dl$data) == "Latitude 2")] <- "Latitude"
        }
        col_idx <- colnames(pg_dl$data)[colnames(pg_dl$data) %in% unique(query_ALL$pg_col_name)]
        dl_single <- pg_dl$data %>% 
          dplyr::select(all_of(col_idx)) %>%  
          # mutate_all(~na_if(., '')) %>% # This will throw errors from unknown column types
          janitor::remove_empty(which = c("rows", "cols"))
        if("Longitude" %in% colnames(dl_single) & "Latitude" %in% colnames(dl_single)){
          dl_single <- dl_single %>% 
            mutate(date_accessed = as.Date(Sys.Date()),
                   URL = pg_dl$url,
                   citation = pg_dl$citation,
                   Longitude = case_when(as.numeric(Longitude) > 180 ~ as.numeric(Longitude)-360, 
                                         TRUE ~ as.numeric(Longitude))) %>% 
            filter(Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90) %>% 
            dplyr::select(date_accessed, URL, citation, everything())
        } else {
          dl_error <- "Lon/Lat column is empty"
        }
      } else {
        dl_error <- "Lon/Lat column is missing"
      }
    } else {
      dl_error <- "Multiple columns with same name"
    }
  } else {
    dl_error <- "No data in DOI reference"
  }
  
  # Determined that there are columns with key drivers
  if(is.null(dl_error)){
    if(all(colnames(dl_single) %in% c("date_accessed", "URL", "citation", "Longitude", "Latitude", "Date/Time"))){
      dl_error <- "No columns with key drivers"
    }
  }
  
  # Create error report if necessary
  if(!is.null(dl_error)) 
    dl_single <- data.frame(date_accessed = as.Date(Sys.Date()),
                            URL = pg_dl$url,
                            citation = pg_dl$citation,
                            Error = dl_error)
  
  # Exit
  return(dl_single)
}

# Function for downloading and processing PANGAEA data for merging
pg_dl_proc <- function(pg_doi){
  
  # Get data
  dl_error <- NULL
  suppressWarnings(
  dl_dat <- tryCatch(pg_data(pg_doi), error = function(pg_doi) {dl_error <<- "Cannot access data via pangaer"})
  )
  
  # Extract data from multiple lists as necessary
  if(is.null(dl_error)){
    dl_df <- plyr::ldply(dl_dat, pg_dl_prep)
  } else {
    dl_df <- data.frame(date_accessed = as.Date(Sys.Date()),
                        URL = paste0("https://doi.org/",pg_doi),
                        citation = NA,
                        Error = dl_error)
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
# pg_Bouman <- pg_full_search(query = "Bouman", bbox = c(-60, 63, 60, 90))

# Test specific files
# pg_test_1 <- pg_data(doi = "10.1594/PANGAEA.857405")[[2]]$data
# pg_test_2 <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.774421")
# pg_test_3 <- pg_test_dl("10.1594/PANGAEA.774421")


## EU Arctic cruise Oceans data on PANGAEA - 285 - Some issues
# NB: The following PG downloads have rows with missing lon/lat values
# This is a conscious choice for now because the spatial info may
# be stored in a different column name and we don't want to lose data here
print(paste0("Began run on pg_EU_cruise_oceans at ", Sys.time()))
pg_EU_cruise_oceans <- pg_full_search(query = "cruise", topic = "Oceans", bbox = c(-60, 63, 60, 90)) 
pg_doi_list <- distinct(data.frame(doi = pg_EU_cruise_oceans$doi, file = "pg_EU_cruise_oceans"))
pg_EU_cruise_oceans_dl <- plyr::ldply(pg_EU_cruise_oceans$doi, pg_dl_proc) # 184 seconds
# Get lookup table
# Combine and filter by coords
pg_EU_cruise_oceans_trim <- filter(pg_EU_cruise_oceans_dl, Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
data.table::fwrite(pg_EU_cruise_oceans_trim, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Oceans.csv")
data.table::fwrite(pg_EU_cruise_oceans_trim, "data/pg_data/pg_EU_cruise_Oceans.csv")
rm(pg_EU_cruise_oceans_dl, pg_EU_cruise_oceans_trim); gc()


## EU Arctic cruise Atmosphere data on PANGAEA - 139 - Some issues
# ~3 minutes
# NB: More than 90% of these files do not have lon/lat values so they get removed...
print(paste0("Began run on pg_EU_cruise_atmosphere at ", Sys.time()))
pg_EU_cruise_atmosphere <- pg_full_search(query = "cruise", topic = "Atmosphere", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi) %>% mutate(file = "pg_EU_cruise_atmosphere")
pg_doi_list <- distinct(rbind(pg_doi_list, pg_EU_cruise_atmosphere[c("doi", "file")]))
pg_EU_cruise_atmosphere_dl <- plyr::ldply(pg_EU_cruise_atmosphere$doi, pg_dl_proc) 
pg_EU_cruise_atmosphere_trim <- filter(pg_EU_cruise_atmosphere_dl, Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
data.table::fwrite(pg_EU_cruise_atmosphere_trim, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Atmosphere.csv")
data.table::fwrite(pg_EU_cruise_atmosphere_trim, "data/pg_data/pg_EU_cruise_Atmosphere.csv")
rm(pg_EU_cruise_atmosphere_dl, pg_EU_cruise_atmosphere_trim); gc()


## EU Arctic cruise Cryosphere data on PANGAEA - 8
print(paste0("Began run on pg_EU_cruise_cryosphere at ", Sys.time()))
pg_EU_cruise_cryosphere <- pg_full_search(query = "cruise", topic = "Cryosphere", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi) %>% mutate(file = "pg_EU_cruise_cryosphere")
pg_doi_list <- distinct(rbind(pg_doi_list, pg_EU_cruise_cryosphere[c("doi", "file")]))
pg_EU_cruise_cryosphere_dl <- plyr::ldply(pg_EU_cruise_cryosphere$doi, pg_dl_proc)
pg_EU_cruise_cryosphere_trim <- filter(pg_EU_cruise_cryosphere_dl, Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
data.table::fwrite(pg_EU_cruise_cryosphere_trim, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Cryosphere.csv")
data.table::fwrite(pg_EU_cruise_cryosphere_trim, "data/pg_data/pg_EU_cruise_Cryosphere.csv")
rm(pg_EU_cruise_cryosphere_dl, pg_EU_cruise_cryosphere_trim); gc()


## EU Arctic cruise Biological Classification data on PANGAEA - 262 - Some issues
# ~3 minutes
print(paste0("Began run on pg_EU_cruise_bio_class at ", Sys.time()))
pg_EU_cruise_bio_class <- pg_full_search(query = "cruise", topic = "Biological Classification", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi) %>% mutate(file = "pg_EU_cruise_bio_class")
pg_doi_list <- distinct(rbind(pg_doi_list, pg_EU_cruise_bio_class[c("doi", "file")]))
pg_EU_cruise_bio_class_dl <- plyr::ldply(pg_EU_cruise_bio_class$doi, pg_dl_proc)
pg_EU_cruise_bio_class_trim <- filter(pg_EU_cruise_bio_class_dl, Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
data.table::fwrite(pg_EU_cruise_bio_class_trim, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Bio_class.csv")
data.table::fwrite(pg_EU_cruise_bio_class_trim, "data/pg_data/pg_EU_cruise_Bio_class.csv")
rm(pg_EU_cruise_bio_class_dl, pg_EU_cruise_bio_class_trim); gc()


## EU Arctic cruise Biosphere data on PANGAEA - 3
print(paste0("Began run on pg_EU_cruise_biosphere at ", Sys.time()))
pg_EU_cruise_biosphere <- pg_full_search(query = "cruise", topic = "Biosphere", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi) %>% mutate(file = "pg_EU_cruise_biosphere")
pg_doi_list <- distinct(rbind(pg_doi_list, pg_EU_cruise_biosphere[c("doi", "file")]))
pg_EU_cruise_biosphere_dl <- plyr::ldply(pg_EU_cruise_biosphere$doi, pg_dl_proc)
pg_EU_cruise_biosphere_trim <- filter(pg_EU_cruise_biosphere_dl, Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
data.table::fwrite(pg_EU_cruise_biosphere_trim, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Biosphere.csv")
data.table::fwrite(pg_EU_cruise_biosphere_trim, "data/pg_data/pg_EU_cruise_Biosphere.csv")
rm(pg_EU_cruise_biosphere_dl, pg_EU_cruise_biosphere_trim); gc()


## EU Arctic cruise Ecology data on PANGAEA - 564 - Some issues
## NB: Takes ~18 minutes
print(paste0("Began run on pg_EU_cruise_ecology at ", Sys.time()))
pg_EU_cruise_ecology <- pg_full_search(query = "cruise", topic = "Ecology", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi) %>% mutate(file = "pg_EU_cruise_ecology")
pg_doi_list <- distinct(rbind(pg_doi_list, pg_EU_cruise_ecology[c("doi", "file")]))
pg_EU_cruise_ecology_dl <- plyr::ldply(pg_EU_cruise_ecology$doi, pg_dl_proc)
pg_EU_cruise_ecology_trim <- filter(pg_EU_cruise_ecology_dl, Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
data.table::fwrite(pg_EU_cruise_ecology_trim, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Ecology.csv")
data.table::fwrite(pg_EU_cruise_ecology_trim, "data/pg_data/pg_EU_cruise_Ecology.csv")
rm(pg_EU_cruise_ecology_dl, pg_EU_cruise_ecology_trim); gc()


## EU Arctic cruise Human Dimensions data on PANGAEA - 0
print(paste0("Began run on pg_EU_cruise_human at ", Sys.time()))
pg_EU_cruise_human <- pg_full_search(query = "cruise", topic = "Human Dimensions", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi)


## EU Arctic cruise Chemistry data on PANGAEA west - 1906 - Some issues
## MB ~16 minutes
print(paste0("Began run on pg_EU_cruise_chemistry_west at ", Sys.time()))
pg_EU_cruise_chemistry_west <- pg_full_search(query = "cruise", topic = "Chemistry", bbox = c(-60, 63, 0, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi) %>% mutate(file = "pg_EU_cruise_chemistry_west")
pg_doi_list <- distinct(rbind(pg_doi_list, pg_EU_cruise_chemistry_west[c("doi", "file")]))
pg_EU_cruise_chemistry_west_dl <- plyr::ldply(pg_EU_cruise_chemistry_west$doi, pg_dl_proc)
pg_EU_cruise_chemistry_west_trim <- filter(pg_EU_cruise_chemistry_west_dl, Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
data.table::fwrite(pg_EU_cruise_chemistry_west_trim, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Chemistry_west.csv")
data.table::fwrite(pg_EU_cruise_chemistry_west_trim, "data/pg_data/pg_EU_cruise_Chemistry_west.csv")
rm(pg_EU_cruise_chemistry_west_dl, pg_EU_cruise_chemistry_west_trim); gc()


## EU Arctic cruise Chemistry data on PANGAEA east - 5647 - Some issues
## NB: ~68 minutes
print(paste0("Began run on pg_EU_cruise_chemistry_east at ", Sys.time()))
pg_EU_cruise_chemistry_east <- pg_full_search(query = "cruise", topic = "Chemistry", bbox = c(0, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi) %>% mutate(file = "pg_EU_cruise_chemistry_east")
pg_doi_list <- distinct(rbind(pg_doi_list, pg_EU_cruise_chemistry_east[c("doi", "file")]))
pg_EU_cruise_chemistry_east_dl <- plyr::ldply(pg_EU_cruise_chemistry_east$doi, pg_dl_proc)
pg_EU_cruise_chemistry_east_trim <- filter(pg_EU_cruise_chemistry_east_dl, Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
# test1 <- data.frame(table(pg_EU_cruise_chemistry_east_trim$citation)) # Investigate which files contribute the most size
data.table::fwrite(pg_EU_cruise_chemistry_east_trim, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Chemistry_east.csv")
data.table::fwrite(pg_EU_cruise_chemistry_east_trim, "data/pg_data/pg_EU_cruise_Chemistry_east.csv")
rm(pg_EU_cruise_chemistry_east_dl, pg_EU_cruise_chemistry_east_trim); gc()


## EU Arctic CTD data on PANGAEA - 941 - Some issues
## NB: ~14 minutes
print(paste0("Began run on pg_EU_CTD at ", Sys.time()))
pg_EU_CTD <- pg_full_search(query = "CTD", bbox = c(-60, 63, 60, 90)) %>% 
  filter(!doi %in% pg_doi_list$doi) %>% mutate(file = "pg_EU_CTD")
pg_doi_list <- distinct(rbind(pg_doi_list, pg_EU_CTD[c("doi", "file")]))
pg_EU_CTD_dl <- plyr::ldply(pg_EU_CTD$doi, pg_dl_proc)
pg_EU_CTD_trim <- filter(pg_EU_CTD_dl, Longitude >= -60, Longitude <= 60, Latitude >= 63, Latitude <= 90)
data.table::fwrite(pg_EU_CTD_trim, "~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_CTD.csv")
data.table::fwrite(pg_EU_CTD_trim, "data/pg_data/pg_EU_CTD.csv") # 5.3 GB
rm(pg_EU_CTD_dl, pg_EU_CTD_trim); gc()


## Save DOI list
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")
rm(list = ls()[grep("pg_EU", ls())]); gc()


# Collect MUR 1km data for all site
## NB: This takes over 2 hours to run
## Feb 20-21 2021 are missing from the server
## The daily files are compiled below in each site section
doParallel::registerDoParallel(cores = 15)
MUR_dates <- seq(as.Date("2003-01-01"), as.Date("2021-12-31"), by = "day")[-c(6626, 6627)]
system.time(
plyr::l_ply(MUR_dates, download_MUR_ALL, .parallel = T)
) # 2.4 hours


# Kongsfjorden ------------------------------------------------------------

## All Kongsfjorden bbox data files - 2525
print(paste0("Began run on pg_kong at ", Sys.time()))
pg_kong_bbox <- pg_full_search(query = "", bbox = c(bbox_kong[1], bbox_kong[3], bbox_kong[2], bbox_kong[4])) %>% # 2493 files
  filter(!doi %in% pg_doi_list$doi)
pg_kong_name_1 <- pg_full_search(query = "kongsfjord") %>% # 7 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_kong_bbox$doi)
pg_kong_name_2 <- pg_full_search(query = "kongsfjorden") %>% # 16 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_kong_bbox$doi, !doi %in% pg_kong_name_1$doi)
pg_kong_name_3 <- pg_full_search(query = "ny alesund") %>% # 11 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_kong_bbox$doi, !doi %in% pg_kong_name_1$doi, !doi %in% pg_kong_name_2$doi)
pg_kong_name_4 <- pg_full_search(query = "ny-alesund") %>% # 0 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_kong_bbox$doi, !doi %in% pg_kong_name_1$doi, 
         !doi %in% pg_kong_name_2$doi, !doi %in% pg_kong_name_3$doi)
pg_kong_all <- rbind(pg_kong_bbox, pg_kong_name_1, pg_kong_name_2, pg_kong_name_3, pg_kong_name_4) %>%
  filter(!doi %in% c("10.1594/PANGAEA.909130")) %>% # Wide file with no date values
  # filter(!grepl("Multibeam survey|Radiosonde", citation, ignore.case = T)) %>% # This removes ~40 million rows of bathy data
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
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_kong_all$doi, file = "pg_kong_all")))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")

# Extract gridded ice data and save
## NB: Not added to FACE-IT product because they are gridded data
## 4 KM
ice_coords_4km_kong <- load_ice_coords("Kongsfjorden", "4km")
ice_4km_kong <- plyr::ldply(ice_4km_files, load_ice_gridded, ice_coords_4km_kong, .parallel = T)
save(ice_4km_kong, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/ice_4km_kong.RData") # NB: CSV files are too large
## 1 KM
ice_coords_1km_kong <- load_ice_coords("Kongsfjorden", "1km")
ice_1km_kong <- plyr::ldply(ice_1km_files, load_ice_gridded, ice_coords_1km_kong, .parallel = T)
save(ice_1km_kong, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/ice_1km_kong.RData")

# Compile MUR data downloaded in EU section
doParallel::registerDoParallel(cores = 15)
system.time(
  sst_MUR_kong_plyr <- plyr::ldply(dir("~/pCloudDrive/FACE-IT_data/MUR/kong/", full.names = T, pattern = ".rds"), 
                              read_rds, .parallel = TRUE)
) # 53 seconds
save(sst_MUR_kong, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/sst_MUR_kong.RData")


# Isfjorden ---------------------------------------------------------------

## All Isfjorden data files - 837
print(paste0("Began run on pg_is at ", Sys.time()))
pg_is_bbox <- pg_full_search(query = "", bbox = c(bbox_is[1], bbox_is[3], bbox_is[2], bbox_is[4])) %>% # 193 files
  filter(!doi %in% pg_doi_list$doi)
pg_is_name_1 <- pg_full_search(query = "isfjord") %>% # 1 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_is_bbox$doi)
pg_is_name_2 <- pg_full_search(query = "isfjorden") %>% # 0 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_is_bbox$doi, !doi %in% pg_is_name_1$doi)
pg_is_name_3 <- pg_full_search(query = "longyearbyen") %>% # 644 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_is_bbox$doi, !doi %in% pg_is_name_1$doi, !doi %in% pg_is_name_2$doi)
pg_is_all <- rbind(pg_is_bbox, pg_is_name_1, pg_is_name_2, pg_is_name_3) %>% 
  filter(!doi %in% c("10.1594/PANGAEA.909130")) %>% # Wide file with no date values
  # filter(!grepl("Multibeam survey|Radiosonde", citation, ignore.case = T)) %>% # This removes ~40 million rows of bathy data
  # filter(!grepl("WOCE", citation)) %>% # The WOCE data have formatting issues and should be downloaded via there own portal
  arrange(citation) %>% distinct()
rm(pg_is_bbox, pg_is_name_1, pg_is_name_2, pg_is_name_3); gc()

# Download files
system.time(
pg_is_dl <- plyr::ldply(pg_is_all$doi, pg_dl_proc)
) # 14 minutes
# colnames(pg_is_dl)
# test1 <- data.frame(table(pg_is_dl$citation)) # Investigate which files contribute the most size
# test2 <- filter(pg_kong_dl, grepl("914973", citation)) %>% janitor::remove_empty(which = "cols")
# pg_is_trim <- filter(pg_is_dl, Longitude >= 13.62, Longitude <= 17.14, Latitude >= 78.03, Latitude <= 78.71)
data.table::fwrite(pg_is_dl, "~/pCloudDrive/FACE-IT_data/isfjorden/pg_is.csv")
data.table::fwrite(pg_is_dl, "data/pg_data/pg_is.csv")
rm(pg_is_dl); gc()

# Update DOI list with Isfjorden
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_is_all$doi, file = "pg_is_all")))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")

# Ice data
## 4 KM
ice_coords_4km_is <- load_ice_coords("Isfjorden", "4km")
doParallel::registerDoParallel(cores = 15)
system.time(
 ice_4km_is <- plyr::ldply(ice_4km_files, load_ice_gridded, ice_coords_4km_is, .parallel = T)
) # 4 seconds for 100, 206 seconds for all
save(ice_4km_is, file = "~/pCloudDrive/FACE-IT_data/isfjorden/ice_4km_is.RData") # NB: CSV files are too large
## 1 KM
ice_coords_1km_is <- load_ice_coords("Isfjorden", "1km")
system.time(
 ice_1km_is <- plyr::ldply(ice_1km_files, load_ice_gridded, ice_coords_1km_is, .parallel = T)
) # 8 seconds for 100, 120 seconds for all
save(ice_1km_is, file = "~/pCloudDrive/FACE-IT_data/isfjorden/ice_1km_is.RData")

# Raw AIS data
is_AIS_raw_files <- dir("~/pCloudDrive/FACE-IT_data/isfjorden/AIS", full.names = TRUE, pattern = "ais_")
is_AIS_raw <- map_dfr(is_AIS_raw_files, read_delim, delim = ";")
is_AIS_raw <- data.frame(is_AIS_raw)
save(is_AIS_raw, file = "~/pCloudDrive/FACE-IT_data/isfjorden/AIS/is_AIS_raw.RData")


# Storfjorden -------------------------------------------------------------

## All Storfjorden data files - 57
print(paste0("Began run on pg_stor at ", Sys.time()))
pg_stor_bbox <- pg_full_search(query = "", bbox = c(bbox_stor[1], bbox_stor[3], bbox_stor[2], bbox_stor[4])) %>% # 34 files
  filter(!doi %in% pg_doi_list$doi)
pg_stor_name_1 <- pg_full_search(query = "storfjorden") # 12 files
pg_stor_name_2 <- pg_full_search(query = "storfjord") # 11 files
pg_stor_all <- rbind(pg_stor_bbox, pg_stor_name_1, pg_stor_name_2) %>% 
  # filter(!doi %in% c("10.1594/PANGAEA.909130")) %>% # Wide file with no date values
  # filter(!grepl("Multibeam survey|Radiosonde", citation, ignore.case = T)) %>% # This removes ~40 million rows of bathy data
  # filter(!grepl("WOCE", citation)) %>% # The WOCE data have formatting issues and should be downloaded via their own portal
  arrange(citation) %>% distinct()
rm(pg_stor_bbox, pg_stor_name_1, pg_stor_name_2); gc()

# Download files
system.time(
pg_stor_dl <- plyr::ldply(pg_stor_all$doi, pg_dl_proc)
) # 28 seconds
# pg_ingle_trim <- filter(pg_ingle_dl, Longitude >= 18.15, Longitude <= 18.79, Latitude >= 77.87, Latitude <= 78.05) # Reduces to 0...
data.table::fwrite(pg_stor_dl, "~/pCloudDrive/FACE-IT_data/storfjorden/pg_stor.csv")
data.table::fwrite(pg_stor_dl, "data/pg_data/pg_stor.csv")
rm(pg_stor_dl); gc()

# Update DOI list with Inglefieldbukta
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_stor_all$doi, file = "pg_stor_all")))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")

# Extract gridded ice data and save
## NB: Not added to FACE-IT product because they are gridded data
## 4 KM
ice_coords_4km_stor <- load_ice_coords("Storfjorden", "4km")
ice_4km_stor <- plyr::ldply(ice_4km_files, load_ice_gridded, ice_coords_4km_stor, .parallel = T)
save(ice_4km_stor, file = "~/pCloudDrive/FACE-IT_data/storfjorden/ice_4km_stor.RData") # NB: CSV files are too large
## 1 KM
ice_coords_1km_stor <- load_ice_coords("Storfjorden", "1km")
ice_1km_stor <- plyr::ldply(ice_1km_files, load_ice_gridded, ice_coords_1km_stor, .parallel = T)
save(ice_1km_stor, file = "~/pCloudDrive/FACE-IT_data/storfjorden/ice_1km_stor.RData")


# Svalbard ----------------------------------------------------------------

## All Svalbard data files - 319
# NB: These files were searched for after the specific sites intentionally
# This was so that site specific files would be allocated to the appropriate folders
# And the files not attributed to a given site would be downloaded in this chunk
# However, I'm currently thinking we don't want these data...
# pg_sval_all <- pg_full_search(query = "", bbox = c(9, 76, 30, 81)) %>% 
#   filter(!doi %in% pg_doi_list$doi) %>% arrange(citation) %>% distinct()

# Svalbard team files - 10 datasets
# rm(pg_sval_all); gc()


# Young Sound -------------------------------------------------------------

## All Young Sound data files - 178
print(paste0("Began run on pg_young at ", Sys.time()))
pg_young_bbox <- pg_full_search(query = "", bbox = c(bbox_young[1], bbox_young[3], bbox_young[2], bbox_young[4])) %>% # 176 files
  filter(!doi %in% pg_doi_list$doi)
pg_young_name_1 <- pg_full_search(query = "zackenberg") %>% # 3 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_young_bbox$doi)
pg_young_all <- rbind(pg_young_bbox, pg_young_name_1) %>% 
  # filter(!grepl("WOCE", citation)) %>% # The WOCE data have formatting issues and should be downloaded via their own portal
  filter(!doi == "10.1594/PANGAEA.786674") %>%   # This file causes weird date issues and doesn't have any key drivers
  arrange(citation) %>% distinct()
rm(pg_young_bbox, pg_young_name_1); gc()

# Download files
system.time(
pg_young_dl <- plyr::ldply(pg_young_all$doi, pg_dl_proc)
) # 306 seconds
# test1 <- data.frame(table(pg_young_dl$citation)) # Investigate which files contribute the most size
# pg_young_trim <- filter(pg_young_dl, Longitude >= -22.367917, Longitude <= -19.907644, Latitude >= 74.210137, Latitude <= 74.624304)
data.table::fwrite(pg_young_dl, "~/pCloudDrive/FACE-IT_data/young_sound/pg_young.csv")
data.table::fwrite(pg_young_dl, "data/pg_data/pg_young.csv")
rm(pg_young_dl); gc()

# Update DOI list with Young Sound
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_young_all$doi, file = "pg_young_all")))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")

# Extract gridded ice data and save
## NB: Not added to FACE-IT product because they are gridded data
## 4 KM
ice_coords_4km_young <- load_ice_coords("Young Sound", "4km")
ice_4km_young <- plyr::ldply(ice_4km_files, load_ice_gridded, ice_coords_4km_young, .parallel = T)
save(ice_4km_young, file = "~/pCloudDrive/FACE-IT_data/young_sound/ice_4km_young.RData") # NB: CSV files are too large
## 1 KM
ice_coords_1km_young <- load_ice_coords("Young Sound", "1km")
ice_1km_young <- plyr::ldply(ice_1km_files, load_ice_gridded, ice_coords_1km_young, .parallel = T)
save(ice_1km_young, file = "~/pCloudDrive/FACE-IT_data/young_sound/ice_1km_young.RData")


# Disko Bay ---------------------------------------------------------------

## All Disko Bay data files - 242
print(paste0("Began run on pg_disko at ", Sys.time()))
pg_disko_bbox <- pg_full_search(query = "", bbox = c(bbox_disko[1], bbox_disko[3], bbox_disko[2], bbox_disko[4])) %>% # 235 files
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
  # filter(!grepl("WOCE", citation)) %>% # The WOCE data have formatting issues and should be downloaded via their own portal
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
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_disko_all$doi, file = "pg_disko_all")))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")

# Extract gridded ice data and save
## NB: Not added to FACE-IT product because they are gridded data
## 4 KM
ice_coords_4km_disko <- load_ice_coords("Disko Bay", "4km")
ice_4km_disko <- plyr::ldply(ice_4km_files, load_ice_gridded, ice_coords_4km_disko, .parallel = T)
save(ice_4km_disko, file = "~/pCloudDrive/FACE-IT_data/disko_bay/ice_4km_disko.RData") # NB: CSV files are too large
## 1 KM
ice_coords_1km_disko <- load_ice_coords("Disko Bay", "1km")
ice_1km_disko <- plyr::ldply(ice_1km_files, load_ice_gridded, ice_coords_1km_disko, .parallel = T)
save(ice_1km_disko, file = "~/pCloudDrive/FACE-IT_data/disko_bay/ice_1km_disko.RData")


# Nuup Kangerlua ----------------------------------------------------------

## All Nuup Kangerlua data files - 199
print(paste0("Began run on pg_nuup at ", Sys.time()))
pg_nuup_bbox <- pg_full_search(query = "", bbox = c(bbox_nuup[1], bbox_nuup[3], bbox_nuup[2], bbox_nuup[4])) %>% # 156 files
  filter(!doi %in% pg_doi_list$doi)
# pg_nuup_name_1 <- pg_full_search(query = "kangerlua") # 0 files
pg_nuup_name_2 <- pg_full_search(query = "nuuk") %>% # 74 files
  filter(!doi %in% pg_doi_list$doi, !doi %in% pg_nuup_bbox$doi)
pg_nuup_all <- rbind(pg_nuup_bbox, pg_nuup_name_2) %>% 
  filter(!grepl("WOCE", citation)) %>% # The WOCE data have formatting issues and should be downloaded via their own portal
  arrange(citation) %>% distinct()
rm(pg_nuup_bbox,  pg_nuup_name_2); gc()

# Download files
system.time(
pg_nuup_dl <- plyr::ldply(pg_nuup_all$doi, pg_dl_proc)
) # 227 seconds
data.table::fwrite(pg_nuup_dl, "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/pg_nuup.csv")
data.table::fwrite(pg_nuup_dl, "data/pg_data/pg_nuup.csv")
rm(pg_nuup_dl); gc()

# Update DOI list with Nuup Kangerlua
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_nuup_all$doi, file = "pg_nuup_all")))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")

# Extract gridded ice data and save
## NB: Not added to FACE-IT product because they are gridded data
## 4 KM
ice_coords_4km_nuup <- load_ice_coords("Nuup Kangerlua", "4km")
ice_4km_nuup <- plyr::ldply(ice_4km_files, load_ice_gridded, ice_coords_4km_nuup, .parallel = T)
save(ice_4km_nuup, file = "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/ice_4km_nuup.RData") # NB: CSV files are too large
## 1 KM
ice_coords_1km_nuup <- load_ice_coords("Nuup Kangerlua", "1km")
ice_1km_nuup <- plyr::ldply(ice_1km_files, load_ice_gridded, ice_coords_1km_nuup, .parallel = T)
save(ice_1km_nuup, file = "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/ice_1km_nuup.RData")


# Porsangerfjorden --------------------------------------------------------

## All Porsangerfjorden data files - 76
print(paste0("Began run on pg_por at ", Sys.time()))
pg_por_bbox <- pg_full_search(query = "", bbox = c(bbox_por[1], bbox_por[3], bbox_por[2], bbox_por[4])) %>% # 107 files
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
pg_doi_list <- distinct(rbind(pg_doi_list, data.frame(doi = pg_por_all$doi, file = "pg_por_all")))
write_csv(pg_doi_list, "~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")

# Extract gridded ice data and save
## NB: Not added to FACE-IT product because they are gridded data
## 4 KM
ice_coords_4km_por <- load_ice_coords("Porsangerfjorden", "4km")
ice_4km_por <- plyr::ldply(ice_4km_files, load_ice_gridded, ice_coords_4km_por, .parallel = T)
save(ice_4km_por, file = "~/pCloudDrive/FACE-IT_data/porsangerfjorden/ice_4km_por.RData") # NB: CSV files are too large
## 1 KM
ice_coords_1km_por <- load_ice_coords("Porsangerfjorden", "1km")
ice_1km_por <- plyr::ldply(ice_1km_files, load_ice_gridded, ice_coords_1km_por, .parallel = T)
save(ice_1km_por, file = "~/pCloudDrive/FACE-IT_data/porsangerfjorden/ice_1km_por.RData")


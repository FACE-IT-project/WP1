# code/data_collection.R
# The location of collected data and the code used when possible
# This is primarily used for the collection of the PANGAEA data
# It also contains the code used to subset and save hi-res gridded ice cover data per site
# As well as load and prep the very large SOCAT data into an R format


# Setup -------------------------------------------------------------------

# TODO: Figure out how to combine lookup tables in order to get lon/lat/date values for cruise data
# Be more strict about the removal of empty columns when they are an unknown format
# Consider using powerjoin for some of the issues caused by the pangaeaR package
# https://cran.r-project.org/web/packages/powerjoin/readme/README.html
# Or some other cleaning methods:
# https://appsilon.com/data-cleaning-in-r/
# Consider only using 'child' datafiles to avoid repeat downloads

# Libraries used in this script
source("code/functions.R")
source("code/key_drivers.R")

# Set cores
doParallel::registerDoParallel(cores = 15)
#

# AIS data ----------------------------------------------------------------

# Raw AIS data
is_AIS_raw_files <- dir("~/pCloudDrive/FACE-IT_data/isfjorden/AIS", full.names = TRUE, pattern = "ais_")
is_AIS_raw <- map_dfr(is_AIS_raw_files, read_delim, delim = ";")
is_AIS_raw <- data.frame(is_AIS_raw)
save(is_AIS_raw, file = "~/pCloudDrive/FACE-IT_data/isfjorden/AIS/is_AIS_raw.RData")
rm(is_AIS_raw_files, is_AIS_raw); gc()


# MUR SST data ------------------------------------------------------------

# Compile MUR data
doParallel::registerDoParallel(cores = 15)
system.time(
  sst_MUR_kong_plyr <- plyr::ldply(dir("~/pCloudDrive/FACE-IT_data/MUR/kong/", full.names = T, pattern = ".rds"), 
                                   read_rds, .parallel = TRUE)
) # 53 seconds
save(sst_MUR_kong, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/sst_MUR_kong.RData")


# MASIE ice data ----------------------------------------------------------

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
## 6 = border of region images 

# Collect MUR 1km data for all site
## NB: This takes over 2 hours to run
## Feb 20-21 2021 are missing from the server
## The daily files are compiled below in each site section
doParallel::registerDoParallel(cores = 15)
MUR_dates <- seq(as.Date("2003-01-01"), as.Date("2021-12-31"), by = "day")[-c(6626, 6627)]
system.time(
plyr::l_ply(MUR_dates, download_MUR_ALL, .parallel = T)
) # 2.4 hours

# Kongsfjorden
## NB: Not added to FACE-IT product because they are gridded data
## 4 KM
ice_coords_4km_kong <- load_ice_coords("Kongsfjorden", "4km")
ice_4km_kong <- plyr::ldply(ice_4km_files, load_ice_gridded, ice_coords_4km_kong, .parallel = T)
save(ice_4km_kong, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/ice_4km_kong.RData") # NB: CSV files are too large
## 1 KM
ice_coords_1km_kong <- load_ice_coords("Kongsfjorden", "1km")
ice_1km_kong <- plyr::ldply(ice_1km_files, load_ice_gridded, ice_coords_1km_kong, .parallel = T)
save(ice_1km_kong, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/ice_1km_kong.RData")

# Isfjorden
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

# Storfjorden
## NB: Not added to FACE-IT product because they are gridded data
## 4 KM
ice_coords_4km_stor <- load_ice_coords("Storfjorden", "4km")
ice_4km_stor <- plyr::ldply(ice_4km_files, load_ice_gridded, ice_coords_4km_stor, .parallel = T)
save(ice_4km_stor, file = "~/pCloudDrive/FACE-IT_data/storfjorden/ice_4km_stor.RData") # NB: CSV files are too large
## 1 KM
ice_coords_1km_stor <- load_ice_coords("Storfjorden", "1km")
ice_1km_stor <- plyr::ldply(ice_1km_files, load_ice_gridded, ice_coords_1km_stor, .parallel = T)
save(ice_1km_stor, file = "~/pCloudDrive/FACE-IT_data/storfjorden/ice_1km_stor.RData")

# Young Sound
## NB: Not added to FACE-IT product because they are gridded data
## 4 KM
ice_coords_4km_young <- load_ice_coords("Young Sound", "4km")
ice_4km_young <- plyr::ldply(ice_4km_files, load_ice_gridded, ice_coords_4km_young, .parallel = T)
save(ice_4km_young, file = "~/pCloudDrive/FACE-IT_data/young_sound/ice_4km_young.RData") # NB: CSV files are too large
## 1 KM
ice_coords_1km_young <- load_ice_coords("Young Sound", "1km")
ice_1km_young <- plyr::ldply(ice_1km_files, load_ice_gridded, ice_coords_1km_young, .parallel = T)
save(ice_1km_young, file = "~/pCloudDrive/FACE-IT_data/young_sound/ice_1km_young.RData")

# Disko Bay
## NB: Not added to FACE-IT product because they are gridded data
## 4 KM
ice_coords_4km_disko <- load_ice_coords("Disko Bay", "4km")
ice_4km_disko <- plyr::ldply(ice_4km_files, load_ice_gridded, ice_coords_4km_disko, .parallel = T)
save(ice_4km_disko, file = "~/pCloudDrive/FACE-IT_data/disko_bay/ice_4km_disko.RData") # NB: CSV files are too large
## 1 KM
ice_coords_1km_disko <- load_ice_coords("Disko Bay", "1km")
ice_1km_disko <- plyr::ldply(ice_1km_files, load_ice_gridded, ice_coords_1km_disko, .parallel = T)
save(ice_1km_disko, file = "~/pCloudDrive/FACE-IT_data/disko_bay/ice_1km_disko.RData")

# Nuup Kangerlua
## NB: Not added to FACE-IT product because they are gridded data
## 4 KM
ice_coords_4km_nuup <- load_ice_coords("Nuup Kangerlua", "4km")
ice_4km_nuup <- plyr::ldply(ice_4km_files, load_ice_gridded, ice_coords_4km_nuup, .parallel = T)
save(ice_4km_nuup, file = "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/ice_4km_nuup.RData") # NB: CSV files are too large
## 1 KM
ice_coords_1km_nuup <- load_ice_coords("Nuup Kangerlua", "1km")
ice_1km_nuup <- plyr::ldply(ice_1km_files, load_ice_gridded, ice_coords_1km_nuup, .parallel = T)
save(ice_1km_nuup, file = "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/ice_1km_nuup.RData")

# Porsangerfjorden
## NB: Not added to FACE-IT product because they are gridded data
## 4 KM
ice_coords_4km_por <- load_ice_coords("Porsangerfjorden", "4km")
ice_4km_por <- plyr::ldply(ice_4km_files, load_ice_gridded, ice_coords_4km_por, .parallel = T)
save(ice_4km_por, file = "~/pCloudDrive/FACE-IT_data/porsangerfjorden/ice_4km_por.RData") # NB: CSV files are too large
## 1 KM
ice_coords_1km_por <- load_ice_coords("Porsangerfjorden", "1km")
ice_1km_por <- plyr::ldply(ice_1km_files, load_ice_gridded, ice_coords_1km_por, .parallel = T)
save(ice_1km_por, file = "~/pCloudDrive/FACE-IT_data/porsangerfjorden/ice_1km_por.RData")


# PANGAEA data ------------------------------------------------------------

# Load PANGAEA DOI list
pg_doi_list <- read_csv("~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")

# Or regenerate the PANGAEA DOI list here
# source("code/data_query.R")

# Previously downloaded PANGAEA data
# pg_files <- dir("~/pCloudDrive/FACE-IT_data", pattern = "pg_", recursive = T, full.names = T)
# pg_files <- pg_files[grepl(".csv", pg_files)]

# Specific author queries
# pg_Riebesell <- pg_full_search(query = "Riebesell", bbox = c(-60, 60, 60, 90))
# pg_Fransson <- pg_full_search(query = "Fransson", bbox = c(-60, 60, 60, 90))
# pg_Chierici <- pg_full_search(query = "Chierici", bbox = c(-60, 60, 60, 90))
# pg_Fischer <- pg_full_search(query = "Chierici", bbox = c(-60, 60, 60, 90))
# pg_Bouman <- pg_full_search(query = "Bouman", bbox = c(-60, 60, 60, 90))

# Test specific files
# pg_test_1 <- pg_data(doi = "10.1594/PANGAEA.857405")[[2]]$data
# pg_test_2 <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.774421")
# pg_test_3 <- pg_test_dl("10.1594/PANGAEA.774421")
# pg_test_4 <- pg_dl_prep(pg_data("10.1594/PANGAEA.896828")[[1]])
# pg_test_5 <- pg_dl_prep(pg_data("10.1594/PANGAEA.56580")[[1]])

### TODO: Add check to not download data that already have a DOI in a new list of downloaded data (to create)
### Allow a force override for this. Preferably with conditionals.

# Full PANGAEA query
## NB: It's possible to run this on multiple cores, but it will disable messages
# It also might be interfering with the saving of the files in some way
doParallel::registerDoParallel(cores = 7) # There are 7 files
system.time(
plyr::l_ply(unique(pg_doi_list$file), pg_dl_save, .parallel = F) 
) # ~ XXX hours

# Log size off old and new files
pg_file_sizes <- read_csv("metadata/pg_file_sizes.csv")
pg_file_sizes_new <- base::file.info(dir("data/pg_data", all.files = T, full.names = T, pattern = ".csv"), extra_cols = FALSE) |> 
  tibble::rownames_to_column(var = "file_name") |> 
  mutate(file_name = str_remove(file_name, "data/pg_data/"),
         size = round(size/1000000,2)) |>  # Convert from bytes to megabytes
  dplyr::rename(created = mtime) |> 
  dplyr::select(file_name, size, created) 
pg_file_sizes <- rbind(pg_file_sizes, pg_file_sizes_new) |> mutate_all(as.character) |> distinct()
rm(pg_file_sizes_new)
write_csv(pg_file_sizes, "metadata/pg_file_sizes.csv")

# Test files
test1 <- read_csv_arrow("~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_chemistry_west.csv")


# Error trapping ----------------------------------------------------------

# Re-load all PANGAEA files to extract error messages
pg_files <- dir("data/pg_data", pattern = "pg_*.csv", full.names = T)
pg_ref_meta <- map_dfr(pg_files, pg_ref_extract)
write_csv(pg_ref_meta, "metadata/pg_ref_meta.csv")
rm(pg_files, pg_ref_meta); gc()


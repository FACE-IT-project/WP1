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
# Consider only using 'child' data files to avoid repeat downloads

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

# Load to get DOI for already downloaded data
pg_doi_files <- map_dfr(dir("metadata", all.files = T, full.names = T, pattern = "_doi.csv"), read_csv_arrow) |> 
  mutate(doi = str_remove(URL, "https://doi.org/"))
pg_downloaded <- unique(c(pg_doi_files$doi, pg_doi_files$parent_doi))

# Strike out DOI for already downloaded data
pg_doi_dl <- pg_doi_list |>
  filter(!doi %in% pg_downloaded)

# Full PANGAEA query
## NB: It's possible to run this on multiple cores, but it will disable messages
## Rather better not to run it in parallel as functions therein are currently set to run in parallel
# doParallel::registerDoParallel(cores = 7) # The downloads are saved across 7 files
system.time(
  plyr::l_ply(unique(pg_doi_list$file), pg_dl_save, pg_doi_dl, .parallel = F)
) # ~160 seconds for 10 DOI for all sites; ~ XXX hours

# Or run one at a time
# pg_dl_save(unique(pg_doi_list$file)[7], pg_doi_dl)

# Keep it going in small chunks
while(nrow(pg_doi_dl) > 0){
  # Load to get DOI for already downloaded data
  pg_doi_files <- map_dfr(dir("metadata", all.files = T, full.names = T, pattern = "_doi.csv"), read_csv_arrow) |> 
    mutate(doi = str_remove(URL, "https://doi.org/"))
  pg_downloaded <- unique(c(pg_doi_files$doi, pg_doi_files$parent_doi))
  
  # Strike out DOI for already downloaded data
  pg_doi_dl <- pg_doi_list |>
    filter(!doi %in% pg_downloaded)
  
  # Download and save
  plyr::l_ply(unique(pg_doi_list$file), pg_dl_save, pg_doi_dl, .parallel = F)
}

# Log size of old and new files
pg_file_sizes <- read_csv("metadata/pg_file_sizes.csv")
pg_file_sizes_new <- base::file.info(dir("data/pg_data", all.files = T, full.names = T, pattern = ".csv"), extra_cols = FALSE) |> 
  tibble::rownames_to_column(var = "file_name") |> 
  mutate(file_name = str_remove(file_name, "data/pg_data/"),
         size = round(size/1000000, 2)) |>  # Convert from bytes to megabytes
  dplyr::rename(created = mtime) |> 
  dplyr::select(file_name, size, created) 
pg_file_sizes <- rbind(pg_file_sizes, pg_file_sizes_new) |> mutate_all(as.character) |> distinct()
rm(pg_file_sizes_new)
write_csv(pg_file_sizes, "metadata/pg_file_sizes.csv")

# Test files
# test1 <- read_csv_arrow("data/pg_data/pg_is.csv")


# Specific DOI ------------------------------------------------------------

# Load PANGAEA DOI list
pg_doi_list <- read_csv("~/pCloudDrive/FACE-IT_data/pg_doi_list.csv")

# Kongsfjorden data downloaded
pg_kong_doi <- read_csv("metadata/pg_kong_doi.csv")
pg_kong <- data.table::fread("data/pg_data/pg_kong.csv") |> 
  left_join(pg_kong_doi, by = "meta_idx")

# Fischer AWI files from: https://dashboard.awi.de/?dashboard=3865
fischer_files <- c("10.1594/PANGAEA.896828", "10.1594/PANGAEA.896822", "10.1594/PANGAEA.896821", 
                   "10.1594/PANGAEA.896771", "10.1594/PANGAEA.896770", "10.1594/PANGAEA.896170",
                   "10.1594/PANGAEA.897349", "10.1594/PANGAEA.927607", "10.1594/PANGAEA.929583", "10.1594/PANGAEA.950174")
fischer_check_list <- filter(pg_doi_list, doi %in% fischer_files)
fischer_check_doi <- filter(pg_kong_doi, URL %in% paste0("https://doi.org/",fischer_files))
fischer_check_dat <- filter(pg_kong, URL %in% paste0("https://doi.org/",fischer_files)) |> janitor::remove_empty()
# NB: Appear to be missing any depth data
# Likely need to correct this via the metadata that is embedded in the downloads

# OCEANIA Zooplankton data
test_doi <- filter(pg_kong_doi, URL == "https://doi.org/10.1594/PANGAEA.840853")
test_doi <- filter(pg_kong_doi, grepl("Wojciech", citation))


# Error trapping ----------------------------------------------------------

# Load all metadata files together to see errors in one dataframe by site
pg_ALL_doi <- map_dfr(dir("metadata", all.files = T, full.names = T, pattern = "_doi.csv"), load_pg)

## Look for suspiciously long/wide files
# Young Sound
pg_young <- read_csv_arrow("data/pg_data/pg_young.csv")
pg_young_size <- plyr::ddply(pg_young, c("meta_idx"), pg_size, .parallel = T)
pg_young_dup <- pg_duplicates(filter(pg_ALL_doi, site == "young"), pg_young_size)
pg_young_check <- pg_ALL_doi |> 
  filter(site == "young", meta_idx %in% c(485))
# 485 is precipitation and wind speeds
pg_young_fix <- filter(pg_young, !meta_idx %in% pg_young_dup)
write_csv_arrow(pg_young_fix, file = "data/pg_data/pg_young.csv")
rm(list = ls()[grep("pg_young", ls())]); gc()

# Disko Bay
pg_disko <- read_csv_arrow("data/pg_data/pg_disko.csv")
pg_disko_size <- plyr::ddply(pg_disko, c("meta_idx"), pg_size, .parallel = T)
pg_disko_dup <- pg_duplicates(filter(pg_ALL_doi, site == "disko"), pg_disko_size)
pg_disko_check <- pg_ALL_doi |> 
  filter(site == "disko", meta_idx %in% c(1046))
# 1046 is mesozooplankton size data
pg_disko_fix <- filter(pg_disko, !meta_idx %in% pg_disko_dup)
write_csv_arrow(pg_disko_fix, file = "data/pg_data/pg_disko.csv")
rm(list = ls()[grep("pg_disko", ls())]); gc()

# Nuup Kangerlua
pg_nuup <- read_csv_arrow("data/pg_data/pg_nuup.csv")
pg_nuup_size <- plyr::ddply(pg_nuup, c("meta_idx"), pg_size, .parallel = T)
pg_nuup_dup <- pg_duplicates(filter(pg_ALL_doi, site == "nuup"), pg_nuup_size)
pg_nuup_check <- pg_ALL_doi |> 
  filter(site == "nuup", meta_idx %in% c(692, 791, 790, 789))
# 692 is cruise CTD data
pg_nuup_fix <- filter(pg_nuup, !meta_idx %in% pg_nuup_dup)
write_csv_arrow(pg_nuup_fix, file = "data/pg_data/pg_nuup.csv")
rm(list = ls()[grep("pg_nuup", ls())]); gc()

# Porsangerfjorden
pg_por <- read_csv_arrow("data/pg_data/pg_por.csv")
pg_por_size <- plyr::ddply(pg_por, c("meta_idx"), pg_size, .parallel = T)
pg_por_dup <- pg_duplicates(filter(pg_ALL_doi, site == "por"), pg_por_size)
pg_por_check <- pg_ALL_doi |> 
  filter(site == "por", meta_idx %in% c(1505))
# 1505 is cruise data
pg_por_fix <- filter(pg_por, !meta_idx %in% pg_por_dup)
write_csv_arrow(pg_por_fix, file = "data/pg_data/pg_por.csv")
rm(list = ls()[grep("pg_por", ls())]); gc()

# Storfjorden
pg_stor <- read_csv_arrow("data/pg_data/pg_stor.csv")
pg_stor_size <- plyr::ddply(pg_stor, c("meta_idx"), pg_size, .parallel = T)
pg_stor_dup <- pg_duplicates(filter(pg_ALL_doi, site == "stor"), pg_stor_size)
pg_stor_check <- pg_ALL_doi |> 
  filter(site == "stor", meta_idx %in% c(1478, 1277, 1271:1273))
# These all check out
pg_stor_fix <- filter(pg_stor, !meta_idx %in% pg_stor_dup)
write_csv_arrow(pg_stor_fix, file = "data/pg_data/pg_stor.csv")
rm(list = ls()[grep("pg_stor", ls())]); gc()

# Isfjorden
pg_is <- data.table::fread("data/pg_data/pg_is.csv")
pg_is_size <- plyr::ddply(pg_is, c("meta_idx"), pg_size, .parallel = T)
pg_is_dup <- pg_duplicates(filter(pg_ALL_doi, site == "is"), pg_is_size)
pg_is_check <- pg_ALL_doi |> 
  filter(site == "is", meta_idx %in% c(1318, 69, 264, 3967, 4140, 4144))
# These large files are Met station and Mooring data
pg_is_fix <- filter(pg_is, !meta_idx %in% pg_is_dup)
rm(pg_is); gc()
data.table::fwrite(pg_is_fix, file = "data/pg_data/pg_is.csv")
rm(list = ls()[grep("pg_is", ls())]); gc()

# Kongsfjorden
pg_kong <- data.table::fread("data/pg_data/pg_kong.csv")
pg_kong_size <- plyr::ddply(pg_kong, c("meta_idx"), pg_size, .parallel = T)
pg_kong_dup <- pg_duplicates(filter(pg_ALL_doi, site == "kong"), pg_kong_size)
pg_kong_check <- pg_ALL_doi |> 
  filter(site == "kong", meta_idx %in% c(10, 16, 35, 3240, 3655))
# Continuous measurement data
pg_kong_fix <- filter(pg_kong, !meta_idx %in% pg_kong_dup)
write_csv_arrow(pg_kong_fix, file = "data/pg_data/pg_kong.csv")
rm(lkongt = ls()[grep("pg_kong", ls())]); gc()


# code/data_processing.R
# This script is used to process data downloaded from multiple sources


# Setup -------------------------------------------------------------------

# Start with common project code
source("code/functions.R")

# Script specific libraries
library(tidync)
library(ncdump)
library(ggOceanMaps)


# pCO2 data ---------------------------------------------------------------

# NB: These files are stored on pCloud
# Contact Robert Schlegel for access: robert.schlegel@imev-mer.fr

# NetCDF info
ncdump::NetCDF(paste0(pCloud_path,"FACE-IT_data/GLODAPv2.2016b_MappedClimatologies/GLODAPv2.2016b.TCO2.nc"))$variable

# Bottle data
GLODAP_Arctic_bottle <- read_csv(paste0(pCloud_path,"FACE-IT_data/GLODAPv2.2020_Arctic_Ocean.csv")) %>% 
  `colnames<-`(sub("G2", "", colnames(.))) %>% 
  mutate(t = as.Date(paste(year, month, day, sep = "-"))) %>% 
  dplyr::rename(lon = longitude, lat = latitude, TCO2 = tco2, TAlk = talk) %>% 
  dplyr::select(lon, lat, depth, t, TCO2, TAlk, salinity, temperature) %>% 
  replace(is.na(.), NA)

# Bottle data averaged over all sites/CTD stations
GLODAP_Arctic_bottle_mean <- GLODAP_Arctic_bottle %>% 
  group_by(lon, lat, depth) %>% 
  summarise_all(mean, na.rm = T, .groups = "drop") %>%
  # mutate(TCO2 = ifelse(is.na(TCO2), NA, TCO2))
  replace(is.na(.), NA)

# NetCDF layers
GLODAP_depth <- tidync::tidync(paste0(pCloud_path,"FACE-IT_data/GLODAPv2.2016b_MappedClimatologies/GLODAPv2.2016b.TCO2.nc")) %>% 
  tidync::activate("D2") %>% 
  tidync::hyper_tibble() %>% 
  dplyr::rename(depth = Depth)
GLODAP_TCO2_grid <- tidync(paste0(pCloud_path,"FACE-IT_data/GLODAPv2.2016b_MappedClimatologies/GLODAPv2.2016b.TCO2.nc")) %>% 
  hyper_tibble() %>%
  dplyr::select(lon, lat, depth_surface, TCO2)
GLODAP_TAlk_grid <- tidync(paste0(pCloud_path,"FACE-IT_data/GLODAPv2.2016b_MappedClimatologies/GLODAPv2.2016b.TAlk.nc")) %>% 
  hyper_tibble() %>% 
  dplyr::select(lon, lat, depth_surface, TAlk)
GLODAP_sal_grid <- tidync(paste0(pCloud_path,"FACE-IT_data/GLODAPv2.2016b_MappedClimatologies/GLODAPv2.2016b.salinity.nc")) %>% 
  hyper_tibble() %>% 
  dplyr::select(lon, lat, depth_surface, salinity)
GLODAP_temp_grid <- tidync(paste0(pCloud_path,"FACE-IT_data/GLODAPv2.2016b_MappedClimatologies/GLODAPv2.2016b.temperature.nc")) %>% 
  hyper_tibble() %>% 
  dplyr::select(lon, lat, depth_surface, temperature)

# Join depth data and correct longitudes to -180 to 180
GLODAP_carb_chem_grid <- GLODAP_TCO2_grid %>% 
  full_join(GLODAP_TAlk_grid, by = c("lon", "lat", "depth_surface")) %>% 
  full_join(GLODAP_sal_grid, by = c("lon", "lat", "depth_surface")) %>% 
  full_join(GLODAP_temp_grid, by = c("lon", "lat", "depth_surface")) %>% 
  mutate(lon = ifelse(lon > 180, lon-360, lon)) %>% 
  left_join(GLODAP_depth, by = c("depth_surface")) %>% 
  dplyr::select(lon, lat, depth, TCO2, TAlk, salinity, temperature)

# Request form Jean-Pierre Gattuso for Arctic carbonate chemistry layers
# 200 metres and deeper, gridded data
# 200 metres and shallower, station data (bottle data)
GLODAP_Arctic_carb_chem <- GLODAP_Arctic_bottle_mean %>% 
  filter(depth <= 200) %>% 
  rbind(., filter(GLODAP_carb_chem_grid, depth >= 200)) %>% 
  filter(lon >= min(GLODAP_Arctic_bottle_mean$lon),
         lon <= max(GLODAP_Arctic_bottle_mean$lon),
         lat >= min(GLODAP_Arctic_bottle_mean$lat),
         lat <= max(GLODAP_Arctic_bottle_mean$lat))
write_csv(GLODAP_Arctic_carb_chem, paste0(pCloud_path,"FACE-IT_data/GLODAP_Arctic_carb_chem.csv"))


# Shape files for bathymetry ----------------------------------------------

# In order to use custom bathymetry files in ggOceanMaps one must first create 
# matching land and glacier shapefiles
# Below is a convenience function that does all of this when given just a single file
# NB: This requires that one has pCloud mounted at the root directory
prep_bathy <- function(file_name){
  
}



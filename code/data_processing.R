# code/data_processing.R
# This script is used to process data downloaded from multiple sources


# Setup -------------------------------------------------------------------

library(tidyverse)
library(tidync)
library(ncdump)


# pCO2 data ---------------------------------------------------------------

# NB: These files are stored on pCloud
# Contact Robert Schlegel for access: robert.schlegel@imev-mer.fr

# NetCDF info
ncdump::NetCDF("~/pCloudDrive/Data/GLODAPv2.2016b_MappedClimatologies/GLODAPv2.2016b.TCO2.nc")$variable

# Bottle data
GLODAP_Arctic_bottle <- read_csv("~/pCloudDrive/Data/GLODAPv2.2020_Arctic_Ocean.csv") %>% 
  `colnames<-`(sub("G2", "", colnames(.))) %>% 
  mutate(t = as.Date(paste(year, month, day, sep = "-"))) %>% 
  dplyr::rename(lon = longitude, lat = latitude, TCO2 = tco2) %>% 
  dplyr::select(lon, lat, depth, t, TCO2)

# Bottle data averaged over all collections
GLODAP_Arctic_bottle_mean <- GLODAP_Arctic_bottle %>% 
  group_by(lon, lat, depth) %>% 
  summarise(TCO2 = mean(TCO2, na.rm = T), .groups = "drop") %>% 
  mutate(TCO2 = ifelse(is.na(TCO2), NA, TCO2))

# NetCDF layers
GLODAP_depth <- tidync("~/pCloudDrive/Data/GLODAPv2.2016b_MappedClimatologies/GLODAPv2.2016b.TCO2.nc") %>% 
  activate("D2") %>% 
  hyper_tibble() %>% 
  dplyr::rename(depth = Depth)
GLODAP_TCO2_grid <- tidync("~/pCloudDrive/Data/GLODAPv2.2016b_MappedClimatologies/GLODAPv2.2016b.TCO2.nc") %>% 
  hyper_tibble() %>% 
  left_join(GLODAP_depth, by = c("depth_surface")) %>% 
  dplyr::select(lon, lat, depth, TCO2) %>% 
  mutate(lon = ifelse(lon >180, lon-360, lon))

# Request form Jean-Pierre Gattuso for Arctic pCO2
# 200 metres and deeper, gridded data
# 200 metres and shallower, station data (bottle data)
Arctic_TCO2 <- GLODAP_Arctic_bottle_mean %>% 
  filter(depth <= 200) %>% 
  rbind(., filter(GLODAP_TCO2_grid, depth >= 200)) %>% 
  filter(lon >= min(GLODAP_Arctic_bottle_mean$lon),
         lon <= max(GLODAP_Arctic_bottle_mean$lon),
         lat >= min(GLODAP_Arctic_bottle_mean$lat),
         lat <= max(GLODAP_Arctic_bottle_mean$lat))
write_csv(Arctic_TCO2, "data/Arctic_TCO2.csv")


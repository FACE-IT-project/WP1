# code/data_processing.R
# This script is used to process data downloaded from multiple sources
# It is also used to prep FACE-IT files for PANGAEA that will not be amalgamated
# Generally this is for ex situ or lab experiments

# NB: These files are stored on pCloud
# Contact Robert Schlegel for access: robert.schlegel@imev-mer.fr


# Setup -------------------------------------------------------------------

# Start with common project code
source("code/functions.R")

# Script specific libraries
library(tidync)
library(ncdump)
library(ggOceanMaps)
library(worrms)


# SOCAT -------------------------------------------------------------------

# Process SOCAT data into R format
SOCAT_R <- read_delim("~/pCloudDrive/FACE-IT_data/socat/SOCATv2022.tsv", delim = "\t", skip = 6976)
SOCAT_R_sub <- dplyr::select(SOCAT_R, yr, mon, day, `longitude [dec.deg.E]`, `latitude [dec.deg.N]`,
                             `sample_depth [m]`, `ETOPO2_depth [m]`, `pCO2water_SST_wet [uatm]`) %>% 
  mutate(yr = as.numeric(yr), mon = as.numeric(mon), day = as.numeric(day))
write_rds(SOCAT_R_sub, "~/pCloudDrive/FACE-IT_data/socat/SOCATv2022.rds", compress = "gz")
rm(SOCAT_R, SOCAT_R_sub); gc()


# GLODAP ------------------------------------------------------------------

# Process GLODAP data into R format
GLODAP <- read_csv("~/pCloudDrive/FACE-IT_data/glodap/GLODAPv2.2022_Merged_Master_File.csv") %>% 
  `colnames<-`(gsub("G2","",colnames(.))) %>% 
  dplyr::rename(lon = longitude, lat = latitude) %>% 
  unite(year, month, day, sep = "-", remove = T, col = "date") %>% 
  mutate(date = as.Date(date))
write_rds(GLODAP, "~/pCloudDrive/FACE-IT_data/glodap/GLODAPv2.2022.rds", compress = "gz")
rm(GLODAP); gc()


# GLODAP bottles ----------------------------------------------------------

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


# MASIE 4km Ice data ------------------------------------------------------
# NB: Sea ice data 1 km not used because 4km data has longer time series and correlates well with 1 km data

# Load data
if(!exists("ice_4km_kong")) load("~/pCloudDrive/FACE-IT_data/kongsfjorden/ice_4km_kong.RData")
if(!exists("ice_4km_is")) load("~/pCloudDrive/FACE-IT_data/isfjorden/ice_4km_is.RData")
if(!exists("ice_4km_stor")) load("~/pCloudDrive/FACE-IT_data/storfjorden/ice_4km_stor.RData")
if(!exists("ice_4km_young")) load("~/pCloudDrive/FACE-IT_data/young_sound/ice_4km_young.RData")
if(!exists("ice_4km_disko")) load("~/pCloudDrive/FACE-IT_data/disko_bay/ice_4km_disko.RData")
if(!exists("ice_4km_nuup")) load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/ice_4km_nuup.RData")
if(!exists("ice_4km_por")) load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/ice_4km_por.RData")

# Snip out bits that aren't within the sites proper
ice_4km_kong_proc <- ice_4km_kong %>%
  mutate(sea_ice_extent = case_when(lon <= 11.5 & lat < 78.95 ~ as.integer(5),
                                    TRUE ~ sea_ice_extent), site = "kong")
ice_4km_is_proc <- ice_4km_is %>%
  mutate(sea_ice_extent = case_when(lon > 16 & lat > 78.75 ~ as.integer(5),
                                    lon < 13.5 & lat > 78.35 ~ as.integer(5),
                                    TRUE ~ sea_ice_extent), site = "is")
ice_4km_stor_proc <- ice_4km_stor %>% mutate(site = "stor") # No issues
ice_4km_young_proc <- ice_4km_young %>%
  mutate(sea_ice_extent = case_when(lon > -21.5 & lat > 74.55 ~ as.integer(5),
                                    lon < -21.7 & lat < 74.31 ~ as.integer(5),
                                    TRUE ~ sea_ice_extent), site = "young")
ice_4km_disko_proc <- ice_4km_disko %>%
  mutate(sea_ice_extent = case_when(sea_ice_extent == 5 ~ as.integer(2), # remove lake pixels
                                    lon > -52 & lon < -50 & lat > 70.2 ~ as.integer(5),
                                    lon > -52.2 & lon < -50.8 & lat < 68.47 ~ as.integer(5),
                                    TRUE ~ sea_ice_extent), site = "disko")
ice_4km_nuup_proc <- ice_4km_nuup %>% mutate(site = "nuup") # No issues
ice_4km_por_proc <- ice_4km_por %>%
  mutate(sea_ice_extent = case_when(lat > 71.01 ~ as.integer(5),
                                    lon > 26.3 & lat > 70.3 & lat < 70.75 ~ as.integer(5),
                                    lon > 26.65 & lat > 70.75 ~ as.integer(5),
                                    lon < 25.6 & lat > 70.75 ~ as.integer(5),
                                    lon < 24.9 & lat > 70.55 & lat < 70.75 ~ as.integer(5),
                                    TRUE ~ sea_ice_extent), site = "por")
# quick_plot_ice(ice_4km_young_proc, pixel_size = 20)

# Combine and save
ice_4km_proc <- rbind(ice_4km_kong_proc, ice_4km_is_proc, ice_4km_stor_proc, ice_4km_young_proc,
                      ice_4km_disko_proc, ice_4km_nuup_proc, ice_4km_por_proc)
save(ice_4km_proc, file = "data/analyses/ice_4km_proc.RData")
rm(ice_4km_kong, ice_4km_is, ice_4km_stor, ice_4km_young, ice_4km_disko, ice_4km_nuup, ice_4km_por,
   ice_4km_kong_proc, ice_4km_is_proc, ice_4km_stor_proc, ice_4km_young_proc,
   ice_4km_disko_proc, ice_4km_nuup_proc, ice_4km_por_proc, ice_4km_proc); gc()


# OISST data --------------------------------------------------------------

# TODO: Get data through 2023 once complete

# NOAA OISST data per site
if(!exists("sst_kong")) load("~/pCloudDrive/FACE-IT_data/kongsfjorden/sst_kong.RData")
if(!exists("sst_is")) load("~/pCloudDrive/FACE-IT_data/isfjorden/sst_is.RData")
if(!exists("sst_stor")) load("~/pCloudDrive/FACE-IT_data/storfjorden/sst_stor.RData")
if(!exists("sst_young")) load("~/pCloudDrive/FACE-IT_data/young_sound/sst_young.RData")
if(!exists("sst_disko")) load("~/pCloudDrive/FACE-IT_data/disko_bay/sst_disko.RData")
if(!exists("sst_nuup")) load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/sst_nuup.RData")
if(!exists("sst_por")) load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/sst_por.RData")

# Filter down to bbox
sst_kong_bbox <- filter(sst_kong, between(lon, bbox_kong[1], bbox_kong[2]), between(lat, bbox_kong[3], bbox_kong[4]))
sst_is_bbox <- filter(sst_is, between(lon, bbox_is[1], bbox_is[2]), between(lat, bbox_is[3], bbox_is[4]))
sst_stor_bbox <- filter(sst_stor, between(lon, bbox_stor[1], bbox_stor[2]), between(lat, bbox_stor[3], bbox_stor[4]))
sst_young_bbox <- filter(sst_young, between(lon, bbox_young[1], bbox_young[2]), between(lat, bbox_young[3]-0.25, bbox_young[4])) # Mouth only
sst_disko_bbox <- filter(sst_disko, between(lon, bbox_disko[1], bbox_disko[2]), between(lat, bbox_disko[3], bbox_disko[4]))
sst_nuup_bbox <- filter(sst_nuup, between(lon, bbox_nuup[1], bbox_nuup[2]), between(lat, bbox_nuup[3], bbox_nuup[4]))
sst_por_bbox <- filter(sst_por, between(lon, bbox_por[1], bbox_por[2]), between(lat, bbox_por[3], bbox_por[4]))

# Create site averaged time series
OISST_kong <- area_average(sst_kong_bbox, "kong")
OISST_is <- area_average(sst_is_bbox, "is")
OISST_stor <- area_average(sst_stor_bbox, "stor")
OISST_young <- area_average(sst_young_bbox, "young")
OISST_disko <- area_average(sst_disko_bbox, "disko")
OISST_nuup <- area_average(sst_nuup_bbox, "nuup")
OISST_por <- area_average(sst_por_bbox, "por")

# Combine and save
OISST_all <- rbind(OISST_kong, OISST_is, OISST_stor, OISST_young, OISST_disko, OISST_nuup, OISST_por) |> filter(value > -1.8) |>
  mutate(type = "OISST", depth = 0, lon = NA, lat = NA,
         date_accessed = as.Date("2023-10-13"),
         URL = "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/",
         citation = "Huang, B., Liu, C., Banzon, V., Freeman, E., Graham, G., Hankins, B., Smith, T., Zhang, H. (2021). Improvements of the Daily Optimum Interpolation Sea Surface Temperature (DOISST) Version 2.1. J. Climate, doi: 10.1175/JCLI-D-20-0166.1",
         variable = "temp [°C]", category = "phys", driver = "sea temp") |> 
  dplyr::select(date_accessed, URL, citation, site, type, category, driver, variable, lon, lat, date, depth, value)
save(OISST_all, file = "data/analyses/OISST_all.RData")
rm(sst_kong, sst_is, sst_stor, sst_young, sst_disko, sst_nuup, sst_por,
   sst_kong_bbox, sst_is_bbox, sst_stor_bbox, sst_young_bbox, sst_disko_bbox, sst_nuup_bbox, sst_por_bbox,
   OISST_kong, OISST_is, OISST_stor, OISST_young, OISST_disko, OISST_nuup, OISST_por); gc()

# Comparisons of SST pixels in/out of the site bbox
# ggplot(distinct(sst_young_bbox[c("lon", "lat")]), aes(x = lon, y = lat)) +
#   geom_tile(colour = "red") +
#   # geom_raster(distinct(sst_young_bbox[c("lon", "lat")]), aes(x = lon, y = lat)) +
#   geom_rect(aes(xmin = bbox_young[1], xmax = bbox_young[2], 
#                 ymin = bbox_young[3], ymax = bbox_young[4]))


# CCI data ----------------------------------------------------------------

# CCI SST extractions
## NB: These all take ~ 5 minutes to load
## Better to load them one at a time
if(!exists("sst_CCI_kong")) load("~/pCloudDrive/FACE-IT_data/kongsfjorden/sst_CCI_kong.RData")
if(!exists("sst_CCI_is")) load("~/pCloudDrive/FACE-IT_data/isfjorden/sst_CCI_is.RData")
if(!exists("sst_CCI_stor")) load("~/pCloudDrive/FACE-IT_data/storfjorden/sst_CCI_stor.RData")
if(!exists("sst_CCI_young")) load("~/pCloudDrive/FACE-IT_data/young_sound/sst_CCI_young.RData")
if(!exists("sst_CCI_disko")) load("~/pCloudDrive/FACE-IT_data/disko_bay/sst_CCI_disko.RData")
if(!exists("sst_CCI_nuup")) load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/sst_CCI_nuup.RData")
if(!exists("sst_CCI_por")) load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/sst_CCI_por.RData")

# Filter down to bbox
sst_CCI_kong_bbox <- filter(sst_CCI_kong, between(lon, bbox_kong[1], bbox_kong[2]), between(lat, bbox_kong[3], bbox_kong[4]))
sst_CCI_is_bbox <- filter(sst_CCI_is, between(lon, bbox_is[1], bbox_is[2]), between(lat, bbox_is[3], bbox_is[4]))
sst_CCI_stor_bbox <- filter(sst_CCI_stor, between(lon, bbox_stor[1], bbox_stor[2]), between(lat, bbox_stor[3], bbox_stor[4]))
sst_CCI_young_bbox <- filter(sst_CCI_young, between(lon, bbox_young[1], bbox_young[2]), between(lat, bbox_young[3], bbox_young[4]))
sst_CCI_disko_bbox <- filter(sst_CCI_disko, between(lon, bbox_disko[1], bbox_disko[2]), between(lat, bbox_disko[3], bbox_disko[4]))
sst_CCI_nuup_bbox <- filter(sst_CCI_nuup, between(lon, bbox_nuup[1], bbox_nuup[2]), between(lat, bbox_nuup[3], bbox_nuup[4]))
sst_CCI_por_bbox <- filter(sst_CCI_por, between(lon, bbox_por[1], bbox_por[2]), between(lat, bbox_por[3], bbox_por[4]))

# Create site averaged time series
CCI_kong <- area_average(sst_CCI_kong_bbox, "kong")
CCI_is <- area_average(sst_CCI_is_bbox, "is")
CCI_stor <- area_average(sst_CCI_stor_bbox, "stor")
CCI_young <- area_average(sst_CCI_young_bbox, "young")
CCI_disko <- area_average(sst_CCI_disko_bbox, "disko")
CCI_nuup <- area_average(sst_CCI_nuup_bbox, "nuup")
CCI_por <- area_average(sst_CCI_por_bbox, "por")

# Combine and save
CCI_all <- rbind(CCI_kong, CCI_is, CCI_stor, CCI_young, CCI_disko, CCI_nuup, CCI_por) |> filter(value > -1.8) |>
  mutate(type = "CCI", depth = 0, lon = NA, lat = NA,
         date_accessed = as.Date("2021-12-13"),
         URL = "http://dap.ceda.ac.uk/thredds/fileServer/neodc/c3s_sst/data/ICDR_v2/Analysis/L4/v2.0",
         citation = "Merchant, C. J., Embury, O., Bulgin, C. E., Block, T., Corlett, G. K., Fiedler, E., et al. (2019). Satellite-based time-series of sea-surface temperature since 1981 for climate applications. Scientific data 6, 1–18.",
         variable = "temp [°C]", category = "phys", driver = "sea temp") |> 
  dplyr::select(date_accessed, URL, citation, site, type, category, driver, variable, lon, lat, date, depth, value)
save(CCI_all, file = "data/analyses/CCI_all.RData")
rm(sst_CCI_kong, sst_CCI_is, sst_CCI_stor, sst_CCI_young, sst_CCI_disko, sst_CCI_nuup, sst_CCI_por,
   sst_CCI_kong_bbox, sst_CCI_is_bbox, sst_CCI_stor_bbox, sst_CCI_young_bbox, sst_CCI_disko_bbox, sst_CCI_nuup_bbox, sst_CCI_por_bbox,
   CCI_kong, CCI_is, CCI_stor, CCI_young, CCI_disko, CCI_nuup, CCI_por); gc()

# Testing
sst_1 <- sst_CCI_young_bbox |> filter(t == as.Date("1982-01-01"))
ggplot(data = distinct(sst_1[c("lon", "lat")]), aes(x = lon, y = lat)) +
  geom_rect(aes(xmin = bbox_young[1], xmax = bbox_young[2],
                ymin = bbox_young[3], ymax = bbox_young[4])) +
  geom_tile(colour = "red")



# Bartsch data ------------------------------------------------------------

## Light data from Inka
# NB: The sensors get dirty throughout the year so values after the winter dark period are best to remove
kong_light_Inka_1 <- read_csv("~/pCloudDrive/restricted_data/Inka_PAR/PAR_Hansneset_KelpForest_10M_above kelp canopy_July2012-June2013_final.csv") %>% 
  slice(1:15999) %>% mutate(date = as.Date(date, format = "%d/%m/%Y"), depth = 10) %>% 
  dplyr::select(-`...4`) %>% pivot_longer(`PAR [umol m-2 s-1]`, names_to = "variable")
kong_light_Inka_2 <- read_csv("~/pCloudDrive/restricted_data/Inka_PAR/PAR_Hansneset_KelpForest_15M_3Jul2012-13Jun2013_final.csv") %>% 
  slice(1:16074) %>% mutate(date = as.Date(date, format = "%d/%m/%Y"), depth = 15) %>% pivot_longer(`PAR [umol m-2 s-1]`, names_to = "variable")
klib <- read_csv("~/pCloudDrive/restricted_data/Inka_PAR/PAR_Hansneset_KelpForest_All_Depths_4July-31July2012_final.csv") 
kong_light_Inka_3 <- rbind(data.frame(date = klib$date, time = klib$time...2,
                                      value = klib$`2.3 UNTEN PAR [µmol m-2 s-1]`, variable = "2.3 UNTEN PAR [µmol m-2 s-1]"),
                           data.frame(date = klib$date, time = klib$time...4,
                                      value = klib$`1.7 OBEN PAR [µmol m-2 s-1]`, variable = "1.7 OBEN PAR [µmol m-2 s-1]"),
                           data.frame(date = klib$date, time = klib$time...6,
                                      value = klib$`4.2 OBEN PAR [µmol m-2 s-1]`, variable = "4.2 OBEN PAR [µmol m-2 s-1]"),
                           data.frame(date = klib$date, time = klib$time...8,
                                      value = klib$`4.8 UNTEN PAR [µmol m-2 s-1]`, variable = "4.8 UNTEN PAR [µmol m-2 s-1]"),
                           data.frame(date = klib$date, time = klib$time...10,
                                      value = klib$`9.2 OBEN PAR [µmol m-2 s-1]`, variable = "9.2 OBEN PAR [µmol m-2 s-1]"),
                           data.frame(date = klib$date, time = klib$time...12,
                                      value = klib$`9.8 UNTEN PAR [µmol m-2 s-1]`, variable = "9.8 UNTEN PAR [µmol m-2 s-1]"),
                           data.frame(date = klib$date, time = klib$time...14,
                                      value = klib$`14.8 PAR [µmol m-2 s-1]`, variable = "14.8 PAR [µmol m-2 s-1]")) %>% 
  mutate(depth = case_when(grepl("14.8", variable) ~ 14.8, grepl("1.7", variable) ~ 1.7, grepl("2.3", variable) ~ 2.3, 
                           grepl("4.2", variable) ~ 4.2, grepl("4.8", variable) ~ 4.8, grepl("9.2", variable) ~ 9.2, grepl("9.8", variable) ~ 9.8),
         date = as.Date(date, format = "%d/%m/%Y"),
         variable = case_when(grepl("OBEN", variable) ~ "PAR above canopy [µmol m-2 s-1]",
                              grepl("UNTEN", variable) ~ "PAR below canopy [µmol m-2 s-1]",
                              TRUE ~ "PAR [µmol m-2 s-1]")) %>% 
  filter(!is.na(time)) # Missing time is also 0 values
kong_light_Inka_hourly <- bind_rows(kong_light_Inka_1, kong_light_Inka_2, kong_light_Inka_3) %>% 
  mutate(note = case_when(grepl("above", variable) ~ "Above canopy",
                          grepl("below", variable) ~ "Below canopy",
                          TRUE ~ "No canopy"),
         `longitude [°E]` = 11.9872, `latitude [°N]` = 78.9958,
         date = paste(date, time, sep = "T")) %>% arrange(depth) %>% 
  dplyr::rename(`PAR [µmol m-2 s-1]` = value, `depth [m]` = depth, `date/time [UTC+0]` = date) %>% 
  dplyr::select(`longitude [°E]`, `latitude [°N]`, `date/time [UTC+0]`, `depth [m]`, `PAR [µmol m-2 s-1]`, note)
# write_delim(kong_light_Inka_hourly, "~/pCloudDrive/restricted_data/Inka_PAR/Bartsch_PAR_Hansneset.csv", delim = "\t")
# kong_light_Inka_PG <- read_delim("~/Downloads/Kongsfjorden_Hansneset_PAR.tab", delim = "\t", skip = 20) # The published data
kong_light_Inka <- bind_rows(kong_light_Inka_1, kong_light_Inka_2, kong_light_Inka_3) %>% 
  mutate(lon = 11.9872, lat = 78.9958,
         variable = case_when(variable == "PAR [umol m-2 s-1]" ~ "PAR [µmol m-2 s-1]", TRUE ~ variable),
         category = "phys", 
         date_accessed = as.Date("2022-03-24"),
         URL = "Received directly from Inka Bartsch", 
         citation = "Bartsch, I., Paar, M., Fredriksen, S., Schwanitz, M., Daniel, C., Hop, H., & Wiencke, C. (2016). Changes in kelp forest biomass and depth distribution in Kongsfjorden, Svalbard, between 1996–1998 and 2012–2014 reflect Arctic warming. Polar Biology, 39(11), 2021-2036.") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")
rm(kong_light_Inka_1, kong_light_Inka_2, kong_light_Inka_3, klib, kong_light_Inka_hourly); gc()


# Niedzwiedz data ---------------------------------------------------------

# Light and kelp data from Sarina's 2022 paper
kong_NiedzKelp <- read_csv("~/pCloudDrive/restricted_data/Niedzwiedz/dataKelp.csv") %>% 
  fill(Length, Width, Stipe.Length, Area.discs, µmol.0, µmol.0.h.cm, µmol.24, µmol.24.h.cm, 
       Comp.irr, Comp.irr.log, Chla.cm.tR, Acc.cm.tR, Acc.Chla.tR, N.Perc, C.Perc, CN) %>% 
  mutate(µmol.0 = round(µmol.0, 2), 
         µmol.0.h.cm = round(µmol.0.h.cm, 2), 
         Comp.irr = round(Comp.irr, 2), 
         Chla.cm = round(Chla.cm, 2), 
         Chla.cm.tR = round(Chla.cm.tR, 2), 
         Acc.cm = round(Acc.cm, 2), 
         Acc.cm.tR = round(Acc.cm.tR, 2),
         Acc.Chla = round(Acc.Chla, 2), 
         Acc.Chla.tR = round(Acc.Chla.tR, 2), 
         CN = round(CN, 2)) %>% 
  dplyr::rename(`Experiment day` = Exp.Day, `Treat temp [°C]` = Temperature,
                `phylloid length [cm]` = Length, `phylloid width [cm]` = Width, `cauloid length [cm]` = Stipe.Length,
                `disc area [cm-2]` = Area.discs, `FW [g]` = FW, `DW [g]` = DW, `Fv/Fm` = Fv.Fm,
                `O2 at 0 µmol photons m-2 s-1 [µmol l-1 s-1]` = µmol.0,
                `O2 at 0 µmol photons m-2 s-1 [µmol l-1 h-1 cm-2]` = µmol.0.h.cm,
                `O2 at 24 µmol photons m-2 s-1 [µmol l-1 s-1]` = µmol.24,
                `O2 at 24 µmol photons m-2 s-1 [µmol l-1 h-1 cm-2]` = µmol.24.h.cm,
                `Compensation E [mol m-2 s-1]` = Comp.irr,
                `Compensation E [log(mol m-2 s-1)]` = Comp.irr.log,
                `Chl a [µg cm-2]` = Chla.cm, `Chl a mean [µg cm-2]` = Chla.cm.tR,
                `Pigm acc [µg cm-2]` = Acc.cm, `Pigm acc mean [µg cm-2]` = Acc.cm.tR,
                `Pigm acc/chl a [µg cm-2]` = Acc.Chla, `Pigm acc/chl a mean [µg cm-2]` = Acc.Chla.tR,
                `N [%]` = N.Perc, `C [%]` = C.Perc) %>% 
  mutate(Species = case_when(Species == "Slat" ~ "Saccharina latissima",
                             Species == "Aesc" ~ "Alaria esculenta"))
write_delim(kong_NiedzKelp, "~/pCloudDrive/restricted_data/Niedzwiedz/dataKelp_PG.csv", delim = "\t")
kong_NiedzLight <- read_csv("~/pCloudDrive/restricted_data/Niedzwiedz/dataLight.csv")
kong_NiedzLight_PG <- kong_NiedzLight %>% 
  dplyr::rename(`longitude [°E]`= Longitude, `latitude [°N]` = Latitude, `depth [m]` = Depth,
                `PAR [µmol m-2 s-1]` = PAR, `PAR [log(µmol m-2 s-1)]`= `log(PAR)`,
                `UV-A [µmol m-2 s-1]` = UV.A, `UV-B [µmol m-2 s-1]` = UV.B, 
                `E [µmol m-2 s-1]` = Surface.irr, Sal = Surface.Salinity) %>% 
  mutate(DateTime = DateTime-7200) %>% # Correct from Svalbard (UTC+2) to UTC+0
  separate(DateTime, into = c("Date", "Time"), sep = " ") %>% 
  mutate(`date/time [UTC+0]` = paste(Date, Time, sep = "T")) %>%
  dplyr::select(Station, `latitude [°N]`, `longitude [°E]`, `date/time [UTC+0]`, `depth [m]`,
                `E [µmol m-2 s-1]`, `PAR [µmol m-2 s-1]`, `PAR [log(µmol m-2 s-1)]`, 
                `UV-A [µmol m-2 s-1]`, `UV-B [µmol m-2 s-1]`, Sal) %>% 
  group_by(Station, `latitude [°N]`, `longitude [°E]`) %>% 
  arrange(`depth [m]`, .by_group = TRUE) %>% ungroup()
write_delim(kong_NiedzLight_PG, "~/pCloudDrive/restricted_data/Niedzwiedz/dataLight_PG.csv", delim = "\t")


# Marambio dataset --------------------------------------------------------

# See "~/pCloudDrive/restricted_data/Marambio/.~lock.Marambio 2022_PANGEA_PALMARIA.xlsx"
# for metadata used below
# 1, 3, 6, 10, 15 and 21
# Load each sheet
mar_1 <- read_csv_arrow("~/pCloudDrive/restricted_data/Marambio/Marambio 2022_PANGEA_PALMARIA_1.csv")
mar_2 <- read_csv_arrow("~/pCloudDrive/restricted_data/Marambio/Marambio 2022_PANGEA_PALMARIA_2.csv")
mar_3 <- read_csv_arrow("~/pCloudDrive/restricted_data/Marambio/Marambio 2022_PANGEA_PALMARIA_3.csv")
mar_4 <- read_csv_arrow("~/pCloudDrive/restricted_data/Marambio/Marambio 2022_PANGEA_PALMARIA_4.csv")

# Process pigment file
mar_1_proc <- mar_1 %>% 
  mutate(Replicate = rep(seq(1, 3), nrow(mar_1)/3)) %>% 
  pivot_longer(t1:t6, names_to = "Day", values_to = "value") %>% 
  pivot_wider(names_from = Parameter, values_from = value)

# Process fv/fm file
mar_2_proc <- mar_2 %>% 
  mutate(Replicate = rep(seq(1, 3), nrow(mar_2)/3)) %>% 
  pivot_longer(t1:t6, names_to = "Day", values_to = "value") %>% 
  pivot_wider(names_from = Parameter, values_from = value)

# Process NPQ file
mar_3_proc <- mar_3 %>% 
  pivot_longer(HL_t1:LL_t6, names_to = "Day", values_to = "value") %>% 
  separate(Day, into = c("Treat_Light", "Day")) %>% 
  mutate(col_names = paste0("NPQ [PAM-Light Curve ",`PAM-Light Curve`," µmol photons m-2 s-1]")) %>% 
  dplyr::select(-Parameter, -`PAM-Light Curve`) %>% 
  pivot_wider(names_from = col_names, values_from = value)

# Process DPPH file
mar_4_proc <- mar_4 %>% 
  mutate(Replicate = rep(seq(1, 3), nrow(mar_4)/3)) %>% 
  pivot_longer(t1:t6, names_to = "Day", values_to = "value") %>% 
  pivot_wider(names_from = Parameter, values_from = value)

# Combine all files
join_columns <- c("Species", "Family", "Treat_Light", "Treat_SA", "Replicate", "Day")
mar_full <- left_join(mar_1_proc, mar_2_proc, by = join_columns) |>  
  left_join(mar_3_proc, by = join_columns) |>  
  left_join(mar_4_proc, by = join_columns) |>  
  mutate(Treat_Light = case_when(Treat_Light == "HL" ~ 500, Treat_Light == "LL" ~ 50),
       Day = case_when(Day  == "t1" ~ 1, Day  == "t2" ~ 3, Day  == "t3" ~ 6,
                       Day  == "t4" ~ 10, Day  == "t5" ~ 15, Day  == "t6" ~ 21)) |>  
  dplyr::select(Species, Family, Replicate, Day,  everything()) |>  
  dplyr::rename(`Repl [#]` = Replicate, `Treat light [µmol photons m-2 s-1]` = Treat_Light, `Treat sal` = Treat_SA, 
                `Chl a [µg/g]` = Chla, `Lut [µg/g]` = Lut, `β Car [µg/g]` = `β Car`, `Zea [µg/g]` = Zeax, `DPPH [µg/g]` = DPPH) |> 
  # Requested by PANGAEA
  mutate_at(12:19, ~as.character(.)) |> 
  mutate_at(12:19, ~replace_na(.,""))
write_csv(mar_full, "~/pCloudDrive/restricted_data/Marambio/Marambio_full.csv")


# Lebrun dataset ----------------------------------------------------------

Lebrun_data <- read_csv("~/pCloudDrive/restricted_data/Lebrun/Fauna_biomass_Table.csv") |> 
  dplyr::rename(DOI = Doi, `Cruise ID` = CruiseID, `Latitude [°N]` = Latitude, `Longitude [°E]` = Longitude,
                `Depth min [m]` = Min_depth, `Depth max [m]` = Max_depth, 
                `Month start` = st_month, `Month end` = end_month,
                `Year start` = st_year, `Year end` = end_year, `Day(s)` = Day,
                `Replicates [n]` = Replicates_number,
                `Biomass [AFDW g/m^2]` = Biomass_gAFDW_m2, `Biomass [AFDW g/m^2 sd]` = Biomass_gAFDW_m2_sd,
                `Biomass [C g/m^2]` = Biomass_gC_m2, `Biomass [C g/m^2 sd]` = Biomass_gC_m2_sd,
                `Biomass [ww g/m^2]` = Biomass_ww_g_m2, `Biomass [ww g/m^2 sd]` = Biomass_ww_g_m2_sd,
                `Abundance [ind/m^2]` = Abundance_ind_m2, `Abundance [ind/m^2 sd]` = Abundance_ind_m2_sd) |> 
  separate(`Day(s)`, into = c("day_start", "day_end"), sep = '-', fill = "right", remove = T, convert = T) |> 
  mutate(day_start = case_when(is.na(day_start) ~ as.integer(1), TRUE ~ day_start),
         day_end = case_when(is.na(day_end) ~ day_start, TRUE ~ day_end),
         month_start = match(`Month start`, tolower(month.name)),
         month_end = match(`Month end`, tolower(month.name))) |> 
  unite("Date/Time [start]", sep = "-", remove = T, `Year start`, month_start, day_start) |> 
  unite("Date/Time [end]", sep = "-", remove = T, `Year end`, month_end, day_end) |> 
  mutate(`Date/Time [start]` = as.Date(`Date/Time [start]`),
         `Date/Time [end]` = as.Date(`Date/Time [end]`)) |> 
  dplyr::select(-`Month start`, -`Month end`) |> 
  # Requested by PANGAEA
  mutate_at(1:26, ~as.character(.)) |> 
  mutate_at(1:26, ~replace_na(., ""))
write_csv(Lebrun_data, "~/pCloudDrive/restricted_data/Lebrun/Lebrun_data_tidy.csv")

# Test visual
ggplot(data = Lebrun_data, aes(x = `Longitude [°E]`, y = `Latitude [°N]`)) +
  borders() + geom_point()


# Gattuso dataset ---------------------------------------------------------

## NB: These times must be in UTC
# This is done automatically via source("code/functions.R") so shouldn't be necessary here
# Sys.setenv(TZ = 'UTC')
Gattuso_data <- read_csv_arrow("~/pCloudDrive/restricted_data/Gattuso/AWIPEV-CO2_v1.csv") |> 
  mutate(datetime = as.POSIXct(datetime, origin = "1970-01-01")) |> 
  separate(`date/time [UTC+0]`, into = c("Date", "Time"), sep = " ") |>  
  mutate(`date/time [UTC+0]` = paste(Date, Time, sep = "T")) |> 
  dplyr::select(`date/time [UTC+0]`, everything(), -Date, -Time) |> 
  mutate_at(1:13, ~as.character(.)) |>
  mutate_at(1:13, ~replace_na(., ""))
write_csv(Gattuso_data, "~/pCloudDrive/restricted_data/Gattuso/AWIPEV-CO2_v1_tidy.csv")

# Version 2
Gattuso_data2 <- read_csv_arrow("~/pCloudDrive/restricted_data/Gattuso/AWIPEV-CO2_v2.csv") |> 
  separate(`date/time [UTC+0]`, into = c("Date", "Time"), sep = "T") |>
  mutate(Time = case_when(Time == "NA" ~ "00:00:00", TRUE ~ Time)) |> 
  mutate(`date/time [UTC+0]` = paste(Date, Time, sep = "T")) |> 
  dplyr::select(`date/time [UTC+0]`, everything(), -Date, -Time) |> 
  mutate_at(1:13, ~as.character(.)) |>
  mutate_at(1:13, ~replace_na(., ""))
write_csv(Gattuso_data2, "~/pCloudDrive/restricted_data/Gattuso/AWIPEV-CO2_v2_PG.csv")


# Miller dataset ----------------------------------------------------------

# Load each dataset and join
Miller_flow <- read_csv("~/pCloudDrive/restricted_data/Miller/Flow Rate.csv", na = c("NaN", -99))
Miller_O2 <- read_csv("~/pCloudDrive/restricted_data/Miller/O2.csv", na = "NaN")
Miller_sal <- read_csv("~/pCloudDrive/restricted_data/Miller/Sal.csv", na = "NaN")
Miller_temp <- read_csv("~/pCloudDrive/restricted_data/Miller/Temp.csv", na = "NaN")
Miller_data <- left_join(Miller_flow, Miller_O2, by = "DateTime") |> 
  left_join(Miller_sal, by = "DateTime") |> left_join(Miller_temp, by = "DateTime") |> 
  separate(DateTime, into = c("date", "time"), sep = " ") |> 
  mutate(date = as.Date(date, "%m/%d/%y"),
         `date/time [UTC+0]` = paste(date, time, sep = "T")) |> 
  dplyr::select(`date/time [UTC+0]`, `C0M0_Flow rate (L min-1)`:C3M2_Temp) |> 
  pivot_longer(`C0M0_Flow rate (L min-1)`:C3M2_Temp) |> 
  separate(name, into = c("replicate", "variable"), sep = "_") |> 
  pivot_wider(names_from = variable, values_from = value) |> 
  dplyr::rename(`Flow rate [L/min]` = `Flow rate (L min-1)`, `O2 [%]` = `% O2`, `Temp [°C]` = Temp)
write_csv(Miller_data, "~/pCloudDrive/restricted_data/Miller/Miller_tidy.csv")


# Lund-Hansen -------------------------------------------------------------

# Ice algae mat dataset
ice_algae_1 <- read_csv("~/pCloudDrive/restricted_data/Lund-Hansen/Algae mat/aggregat_clean.csv")
ice_algae_2 <- read_csv("~/pCloudDrive/restricted_data/Lund-Hansen/Algae mat/flux_clean.csv")
ice_algae <- bind_rows(ice_algae_1, ice_algae_2) |> 
  mutate(`date/time [UTC+0]` = paste0(Date,"T00:00:00"),
         `Ice core depth [mm]` = case_when(is.na(`Ice core depth [mm]`) ~ `Ice core depth [cm]`/10, 
                                           TRUE ~ `Ice core depth [mm]`),
         `O2 conc. [%]` = case_when(is.na(`O2 conc. [%]`) ~ round(`O2 dissolved [umol/l]`/`O2 conc. [umol/l]`, 2),
                                    TRUE ~ `O2 conc. [%]`),
         `O2 conc. [%]` = case_when(is.infinite(`O2 conc. [%]`) ~ as.numeric(NA), TRUE ~ `O2 conc. [%]`)) |> 
  dplyr::select(`date/time [UTC+0]`, Profile, `Ice core depth [mm]`, `Temp. [°C]`, `Salt conc. [%]`, `Salinity`, 
                `O2 dissolved [umol/l]`, `O2 conc. [%]`, `O2 conc. [umol/l]`) |> 
  mutate_at(1:9, ~as.character(.)) |>
  mutate_at(1:9, ~replace_na(., ""))
write_csv(ice_algae, "~/pCloudDrive/restricted_data/Lund-Hansen/Algae mat/algae_mat_PG.csv")

# Photobiological
## Albedo
albedo <- read_csv("~/pCloudDrive/restricted_data/Lund-Hansen/Photobiological/Albedo_Transm.csv") |> 
  dplyr::rename(`E [mW m-2 nm-1] (above ice downwelling)` = `Ed0 [mW m-2 nm-1]`,
                `E [mW m-2 nm-1] (above ice upwelling)` = `Eu0 [mW m-2 nm-1]`,
                `E [mW m-2 nm-1] (under ice downwelling)` = `Edi [mW m-2 nm-1]`) |> 
  mutate_at(1:6, ~as.character(.)) |>
  mutate_at(1:6, ~replace_na(., ""))
write_csv(albedo, "~/pCloudDrive/restricted_data/Lund-Hansen/Photobiological/albedo_PG.csv")

## I-PAM
# 19 March 2013 - Day 78
# Φ_PSII
I_PAM <- read_csv("~/pCloudDrive/restricted_data/Lund-Hansen/Photobiological/Diel_I-PAM.csv") |> 
  mutate(`date/time [UTC+0]` = paste0(date,"T", time)) |> 
  dplyr::rename(`PAR [µmol m-2 s-1] (under ice)` = `Under ice PAR [µmol m-2 s-1]`,
                `Φ PSII [mean] (I-PAM)` = `I-PAM [avg]`, 
                `Φ PSII [std] (I-PAM)` = `I-PAM [std]`,
                `Φ PSII [mean] (Phyto-PAM)` = `Phyto-PAM [avg]`,
                `Φ PSII [std] (Phyto-PAM)` = `Phyto-PAM [std]`) |> 
  dplyr::select(`date/time [UTC+0]`, `PAR [µmol m-2 s-1] (under ice)`,
                `Φ PSII [mean] (I-PAM)`, `Φ PSII [std] (I-PAM)`,
                `Φ PSII [mean] (Phyto-PAM)`, `Φ PSII [std] (Phyto-PAM)`) |> 
  mutate_at(1:6, ~as.character(.)) |>
  mutate_at(1:6, ~replace_na(., ""))
write_csv(I_PAM,"~/pCloudDrive/restricted_data/Lund-Hansen/Photobiological/I-PAM_PG.csv")

## phytoPAM
phyto_PAM <- read_csv("~/pCloudDrive/restricted_data/Lund-Hansen/Photobiological/PhytoPAM.csv") |>
  dplyr::select(date, type, station, `volume before species ID [ml]`, `filtered water volume [ml]`,
                `volume filtered for chl-a [ml]`, `F`, Fm, alfa, ETRmax) |> 
  dplyr::rename(`water sample volume before species ID [ml]` = `volume before species ID [ml]`,
                `water sample volume after filtration for species ID [ml]` = `filtered water volume [ml]`,
                `water sample volume filtered for chl-a [ml]` = `volume filtered for chl-a [ml]`,
                `α [mol é mol−1 photons]` = alfa,
                `ETR max [µmol é m−2 s−1]` = ETRmax) |> 
  mutate_at(1:10, ~as.character(.)) |>
  mutate_at(1:10, ~replace_na(., ""))
write_csv(phyto_PAM, "~/pCloudDrive/restricted_data/Lund-Hansen/Photobiological/PhytoPAM_PG.csv")

## Species
### Load headers
header1 <- scan("~/pCloudDrive/restricted_data/Lund-Hansen/Photobiological/Kangerlussuaq.csv", 
                skip = 0, nlines = 1, what = character(), sep = ",")
header2 <- scan("~/pCloudDrive/restricted_data/Lund-Hansen/Photobiological/Kangerlussuaq.csv", 
                skip = 1, nlines = 1, what = character(), sep = ",")
header3 <- scan("~/pCloudDrive/restricted_data/Lund-Hansen/Photobiological/Kangerlussuaq.csv", 
                skip = 2, nlines = 1, what = character(), sep = ",")

### Load data and add headers
kang_base <- read_csv("~/pCloudDrive/restricted_data/Lund-Hansen/Photobiological/Kangerlussuaq.csv", 
                     skip = 3, col_names = paste(header1, header2, header3, sep = "__")) |> 
  dplyr::rename(Species = `Station__Dato__Value`) |> 
  pivot_longer(cols = `2+ai__20130314__Sum [n/l]`:`2+w__20130314__Sum [carbon µg l-1]`) |> 
  separate(name, into = c("station", "Date", "units"), sep = "__") |> 
  mutate(date = as.Date(Date, "%Y%m%d"),
         `date/time [UTC+0]` = paste0(date,"T00:00:00")) |> 
  pivot_wider(values_from = "value", names_from = "units") |> 
  dplyr::rename(`sum [n ml-1]` = `Sum [n/l]`, `sum [carbon µg ml-1]` = `Sum [carbon µg l-1]`, species = Species) |> 
  mutate(`size class [µm]` = case_when(grepl("00-05", species) ~ "00-05",
                                       grepl("05-10", species) ~ "05-10",
                                       grepl("10-15", species) ~ "10-15",
                                       grepl("15-20", species) ~ "15-20",
                                       grepl("10-20", species) ~ "10-20",
                                       grepl("20-50", species) ~ "20-50",
                                       grepl("41-60", species) ~ "41-60",
                                       grepl("< 10|<10", species) ~ "<10",
                                       grepl("< 20|<20", species) ~ "<20",
                                       grepl("> 20|>20", species) ~ ">20",
                                       grepl("> 25|>25", species) ~ ">25",
                                       grepl("> 50|>50", species) ~ ">50",
                                       grepl("> 60|>60", species) ~ ">60",
                                       TRUE ~ as.character(NA)),
         species = str_remove_all(species, "<|>|00-05|05-10|10-15|10-20|15-20|20-50|41-60|10|20|25|50|60|µm"),
         species = str_remove_all(species, "\\( \\)|\\(  \\)|\\(A\\)|-gruppen| auto"),
         species = str_replace_all(species, "DIATOMOPHYCEAE", "Bacillariophyceae"),
         species = str_replace_all(species, "kiselalger", "diatom"),
         species = str_replace_all(species, "Nøgne furealger", "Naked dinoflagellate"),
         species = str_replace_all(species, "Thekate furealger spp.", "Thecate dinoflagellate spp."),
         species = trimws(species))

## Get species IDs
# Consider exceptions for:
# Gyro-/Gymnodinium spp.
# Gyro-/Pleurosigma spp.
# Nitzschia clost./long.
# Pyramimonas spp./Tetraselmis spp.
kang_unq <- kang_base |> dplyr::select(species) |> distinct()
kang_ID <- plyr::ldply(kang_unq$species, wm_records_df, .parallel = T)

## Combine and save
kang_spp <- left_join(kang_base, kang_ID, by = "species") |> 
  dplyr::rename(`Species UID` = species,
                `Species UID (URI)` = url,
                `Species UID (Semantic URI)` = lsid) |> 
  dplyr::select( `date/time [UTC+0]`, station, `Species UID`, `Species UID (URI)`,
                 `Species UID (Semantic URI)`, `size class [µm]`, `sum [n ml-1]`, `sum [carbon µg ml-1]`) |> 
  mutate_at(1:8, ~as.character(.)) |>
  mutate_at(1:8, ~replace_na(., ""))
write_csv(kang_spp, "~/pCloudDrive/restricted_data/Lund-Hansen/Photobiological/kang_spp_PG.csv")
rm(header1, header2, header3, kang_base, kang_unq, kang_ID); gc()

# Upwelling irradiance
## TriOS
TriOS <- read_csv("~/pCloudDrive/restricted_data/Lund-Hansen/Upwelling irradiance/TriOS.csv") |> 
  dplyr::select(`Wave length [λ]`, `Ed-`, `Ed+`, `Eu-`, `Eu+`) |>
  dplyr::rename(`E [µmol photons m−2 s−1] (under ice no snow cover downwelling)` = `Ed-`, 
                `E [µmol photons m−2 s−1] (under ice snow cover downwelling)` = `Ed+`, 
                `E [µmol photons m−2 s−1] (under ice no snow cover upwelling)`= `Eu-`, 
                `E [µmol photons m−2 s−1] (under ice snow cover upwelling)` = `Eu+`) |> 
  mutate_at(1:5, ~as.character(.)) |>
  mutate_at(1:5, ~replace_na(., ""))
write_csv(TriOS, "~/pCloudDrive/restricted_data/Lund-Hansen/Upwelling irradiance/TriOS_PG.csv")

## PAR Down-Up
PAR_down_up <- read_csv("~/pCloudDrive/restricted_data/Lund-Hansen/Upwelling irradiance/PAR_Down_Up.csv") |> 
  mutate(date = as.Date(paste0(Year,"-",`Julian Day`), format = "%Y-%j"),
         time = paste0(gsub("^(.{2})(.*)$", "\\1:\\2", str_pad(time, 4, pad = 0)),":00"),
         `date/time [UTC+0]` = paste0(date,"T",time)) |> 
  dplyr::select(`date/time [UTC+0]`, `air temp [°C]`, Albedo, `E(down)`, `E(up)`, `E(ice)`) |>
  dplyr::rename(`PAR [µmol photons m−2 s−1] (above ice snow cover downwelling)` = `E(down)`, 
                `PAR [µmol photons m−2 s−1] (above ice snow cover upwelling)` = `E(up)`, 
                `PAR [µmol photons m−2 s−1] (under ice snow cover downwelling)` = `E(ice)`) |> 
  mutate_at(1:6, ~as.character(.)) |>
  mutate_at(1:6, ~replace_na(., ""))
write_csv(PAR_down_up, "~/pCloudDrive/restricted_data/Lund-Hansen/Upwelling irradiance/PAR_Down_Up_PG.csv")


# Duesedau ----------------------------------------------------------------

### 2021 data
## Age density depth structure
age_density_2021 <- read_delim("~/pCloudDrive/restricted_data/Duesedau/Hansneset_2021/Age_Density_2021_PANGEA.csv", delim = ";") |> 
  rename_with(~ gsub("_m2", "", .x, fixed = TRUE)) |> 
  pivot_longer(Juveniles:Total_Individuals, names_to = "Age class", values_to = "count") |> 
  rename(`Depth [m]` = Water_Depth_m) |> 
  mutate(Species = gsub("_", " ", Species)) |> 
  arrange(`Depth [m]`, Replicate, Species) |> 
  dplyr::select(-Year)
write_csv(age_density_2021, "~/pCloudDrive/restricted_data/Duesedau/Hansneset_2021/age_density_2021_PG.csv")

## Carbon Nitrogen 
C_N_2021 <- read_delim("~/pCloudDrive/restricted_data/Duesedau/Hansneset_2021/C_N_2021_PANGEA.csv", delim = ";") |> 
  rename(`Depth [m]` = Water_Depth_m, `Individual [#]` = Individual_No, 
         `C:N [ratio]` = `C:N_ratio`, `N [%]` = `%N`, `C [%]` = `%C`) |> 
  mutate(Species = gsub("_", " ", Species)) |> 
  arrange(`Depth [m]`, Replicate, Species) 
write_csv(C_N_2021, "~/pCloudDrive/restricted_data/Duesedau/Hansneset_2021/C_N_2021_PG.csv")

## Date matrix
datenmatrix_2021 <- read_delim("~/pCloudDrive/restricted_data/Duesedau/Hansneset_2021/Datenmatrix_2021_PANGEA.csv", delim = ";") |> 
  mutate(`date/time [UTC+0]` = paste0(`date/time [UTC+0]`,"T00:00:00")) |> 
  mutate_at(1:24, ~as.character(.)) |>
  mutate_at(1:24, ~replace_na(., ""))
write_csv(datenmatrix_2021, "~/pCloudDrive/restricted_data/Duesedau/Hansneset_2021/datenmatrix_2021_PG.csv")

## Lower depth distributions
lower_depth_2021 <- read_delim("~/pCloudDrive/restricted_data/Duesedau/Hansneset_2021/Lower_Depth_Distribution_2021_PANGEA.csv", delim = ";") |>
  mutate(`date/time [UTC+0]` = paste0(`date/time [UTC+0]`,"T00:00:00")) |> 
  arrange(`date/time [UTC+0]`, Transect, `Depth [m]`)
write_csv(lower_depth_2021, "~/pCloudDrive/restricted_data/Duesedau/Hansneset_2021/lower_depth_2021_PG.csv")

### Historic data
## kelp fresh weight
FW_historic <- read_delim("~/pCloudDrive/restricted_data/Duesedau/Hansneset_historic/FW_timeseries_Hansneset.csv", delim = ";") |> 
  pivot_longer(Acrosiphonia_Spongomorpha:young_Laminaria_spp., values_to = "mean fresh weight [kg m-2]", names_to = "species") |> 
  mutate(Replicate = str_remove(Replicate, "_2012|_2013|_2021"),
         species = trimws(species),
         species = str_replace_all(species, "_ |_", " "),
         species = gsub("sp$", "sp\\.", species),
         species = case_when(species == "Chordaria Dicytosiphon" ~ "Chordaria dictyosiphon",
                             species == "Turnerella penneyi" ~ "Turnerella pennyi", TRUE ~ species))
FW_spp <- plyr::ldply(unique(FW_historic$species), wm_records_df, .parallel = T)
FW_historic_spp <- left_join(FW_historic, FW_spp, by = "species") |> 
  dplyr::rename(`Species UID` = species, `Species UID (URI)` = url, `Species UID (Semantic URI)` = lsid) |> 
  dplyr::select(Year, `Depth [m]`, Replicate, `Species UID`, `Species UID (URI)`, `Species UID (Semantic URI)`, 
                `mean fresh weight [kg m-2]`) |> 
  mutate_at(1:7, ~as.character(.)) |>
  mutate_at(1:7, ~replace_na(., ""))
write_csv(FW_historic_spp, "~/pCloudDrive/restricted_data/Duesedau/Hansneset_historic/FW_historic_PG.csv")

## Kelp age and densities
kelp_age_density_historic <- read_delim("~/pCloudDrive/restricted_data/Duesedau/Hansneset_historic/Kelp_age_density_timeseries_Hansneset.csv", delim = "\t") |> 
  mutate(Species = str_replace_all(Species, "_", " ")) |> 
  left_join(FW_spp, by = c("Species" = "species")) |> 
  dplyr::rename(`Species UID` = Species, `Species UID (URI)` = url, `Species UID (Semantic URI)` = lsid,
                `count [n m-2]` = `Count [m^2]`, `mean age [years m-2]` = `Mean Age [m^2]`) |> 
  dplyr::select(Year, `Depth [m]`, Replicate, `Species UID`, `Species UID (URI)`, `Species UID (Semantic URI)`, 
                `count [n m-2]`, `mean age [years m-2]`) |> 
  mutate_at(1:8, ~as.character(.)) |>
  mutate_at(1:8, ~replace_na(., ""))
write_csv(kelp_age_density_historic, "~/pCloudDrive/restricted_data/Duesedau/Hansneset_historic/kelp_age_density_historic_PG.csv")

## Leaf area index
LAI_historic <- read_delim("~/pCloudDrive/restricted_data/Duesedau/Hansneset_historic/LAI_timeseries_Hansneset.csv", delim = ";") |> 
  mutate(Replicate = str_remove(Replicate, "_2012|_2013|_2021"),
         Class = case_when(Class != "Rhodophyta" ~ "Phaeophyceae", TRUE ~ Class),
         Species = gsub("sp$", "sp\\.", Species))
LAI_spp <- plyr::ldply(unique(LAI_historic$Species), wm_records_df, .parallel = T)
LAI_historic_spp <- left_join(LAI_historic, LAI_spp, by = c("Species" = "species")) |> 
  dplyr::rename(`Species UID` = Species, `Species UID (URI)` = url, `Species UID (Semantic URI)` = lsid) |> 
  dplyr::select(Year, `Depth [m]`, Replicate, Category, Class, 
                `Species UID`, `Species UID (URI)`, `Species UID (Semantic URI)`, `leaf area [m^2]`) |> 
  mutate_at(1:9, ~as.character(.)) |>
  mutate_at(1:9, ~replace_na(., ""))
write_csv(LAI_historic_spp, "~/pCloudDrive/restricted_data/Duesedau/Hansneset_historic/LAI_historic_PG.csv")


# Koch --------------------------------------------------------------------

# Sea urchin biomass/abundance
urchin_spp <- plyr::ldply(c("Strongylocentrotus droebachiensis", "Strongylocentrotus pallidus"), 
                          wm_records_df, .parallel = T)
urchin_biomass_abundance <- read_csv("~/pCloudDrive/restricted_data/Koch/SU_Biomass_Abundance_Porsanger_2022_2023_Pangaea.csv") |> 
  mutate(Species = str_replace_all(Species, "S_droebachiensis", "Strongylocentrotus droebachiensis"),
         Species = str_replace_all(Species, "S_pallidus", "Strongylocentrotus pallidus"),
         Colour = str_replace_all(Colour, "pik", "pink"),
         Notes = str_replace_all(Notes, "injurged", "injured"),
         Notes = str_replace_all(Notes, ", dead", " - dead")) |> 
  left_join(urchin_spp, by = c("Species" = "species")) |> 
  dplyr::rename(Quadrat = Quadrate, Location = Lokation, `diameter [cm]` = Diameter_cm, `weight [g]` = Weight_g,
                `Species UID` = Species, `Species UID (URI)` = url, `Species UID (Semantic URI)` = lsid) |> 
  dplyr::select(Year, Location, Quadrat, `Species UID`, `Species UID (URI)`, `Species UID (Semantic URI)`, 
                Colour, `diameter [cm]`, `weight [g]`, Notes) |> 
  mutate_at(1:10, ~as.character(.)) |>
  mutate_at(1:10, ~replace_na(., ""))
write_csv(urchin_biomass_abundance, "~/pCloudDrive/restricted_data/Koch/urchin_biomass_abundance_PG.csv")
rm(urchin_spp); gc()

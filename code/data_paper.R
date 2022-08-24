# code/data_paper.R
# The code used for the analyses in the WP1 data paper (D1.3)

# TODO: Add ability to filter out specific time series depending on length or some other criteria
# Need to be able to link the spatial and temporal mismatch of combined summary data with lack of trends etc.
# These issues are themselves an important part of the conclusions from the analysis
# Consider changing analysis to 0-5 m rather than 0-10 m
# For meta-data figures, also include the count of datasets/publications per variable per site

# Quotes are from Viitasalo and Bonsdorff (2022) unless stated otherwise
# https://esd.copernicus.org/articles/13/711/2022/
# "In addition, to better understand the effects of climate change on the biodiversity of the Baltic Sea, more emphasis should be placed on studies of shallow photic environments."


# Setup -------------------------------------------------------------------

source("code/functions.R")
library(ggpmisc)
library(listr) # For dealing with lists - not used
library(ggcorrplot) # For correlograms

# Ice cover colours
ice_cover_colours <- c(
  "ocean" = "navy",
  "land" = "slategrey",
  "sea ice" = "mintcream",
  "coast" = "dodgerblue",
  "exclude" = "gold"
)

# Key driver category colours
driver_cat_colours <- c(
  "cryo" = "mintcream",
  "phys" = "skyblue",
  "chem" = "#F6EA7C",
  "bio" = "#A2ED84",
  "soc" = "#F48080"
)


# Data --------------------------------------------------------------------

# FACE-IT collected data
load("~/pCloudDrive/FACE-IT_data/svalbard/full_product_sval.RData") # Some social data don't make it into the smaller files
load("~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")
load("~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.RData")
load("~/pCloudDrive/FACE-IT_data/storfjorden/full_product_stor.RData")
load("~/pCloudDrive/FACE-IT_data/young_sound/full_product_young.RData")
load("~/pCloudDrive/FACE-IT_data/disko_bay/full_product_disko.RData")
load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/full_product_nuup.RData")
load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/full_product_por.RData")
# NB: It's not clear if this is worthwhile
# full_product_all <- rbind(full_product_sval, full_product_kong, full_product_is, full_product_stor,
#                           full_product_young, full_product_disko, full_product_nuup)

# GEM data
load("~/pCloudDrive/restricted_data/GEM/young/young_GEM.RData")
load("~/pCloudDrive/restricted_data/GEM/disko/disko_GEM.RData")
load("~/pCloudDrive/restricted_data/GEM/nuup/nuup_GEM.RData")

# NOAA OISST extractions
load("~/pCloudDrive/FACE-IT_data/kongsfjorden/sst_kong.RData")
sst_kong_bbox <- filter(sst_kong, between(lon, bbox_kong[1], bbox_kong[2]), between(lat, bbox_kong[3], bbox_kong[4]))
load("~/pCloudDrive/FACE-IT_data/isfjorden/sst_is.RData")
sst_is_bbox <- filter(sst_is, between(lon, bbox_is[1], bbox_is[2]), between(lat, bbox_is[3], bbox_is[4]))
load("~/pCloudDrive/FACE-IT_data/storfjorden/sst_stor.RData")
sst_stor_bbox <- filter(sst_stor, between(lon, bbox_stor[1], bbox_stor[2]), between(lat, bbox_stor[3], bbox_stor[4]))
load("~/pCloudDrive/FACE-IT_data/young_sound/sst_young.RData")
sst_young_bbox <- filter(sst_young, between(lon, bbox_young[1], bbox_young[2]), between(lat, bbox_young[3]-0.25, bbox_young[4])) # Mouth only
load("~/pCloudDrive/FACE-IT_data/disko_bay/sst_disko.RData")
sst_disko_bbox <- filter(sst_disko, between(lon, bbox_disko[1], bbox_disko[2]), between(lat, bbox_disko[3], bbox_disko[4]))
load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/sst_nuup.RData")
sst_nuup_bbox <- filter(sst_nuup, between(lon, bbox_nuup[1], bbox_nuup[2]), between(lat, bbox_nuup[3], bbox_nuup[4]))
load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/sst_por.RData")
sst_por_bbox <- filter(sst_por, between(lon, bbox_por[1], bbox_por[2]), between(lat, bbox_por[3], bbox_por[4]))

# CCI SST extractions
## NB: These all take ~ 5 minutes to load
load("~/pCloudDrive/FACE-IT_data/kongsfjorden/sst_CCI_kong.RData")
sst_CCI_kong_bbox <- filter(sst_CCI_kong, between(lon, bbox_kong[1], bbox_kong[2]), between(lat, bbox_kong[3], bbox_kong[4]))
load("~/pCloudDrive/FACE-IT_data/isfjorden/sst_CCI_is.RData")
sst_CCI_is_bbox <- filter(sst_CCI_is, between(lon, bbox_is[1], bbox_is[2]), between(lat, bbox_is[3], bbox_is[4]))
load("~/pCloudDrive/FACE-IT_data/storfjorden/sst_CCI_stor.RData")
sst_CCI_stor_bbox <- filter(sst_CCI_stor, between(lon, bbox_stor[1], bbox_stor[2]), between(lat, bbox_stor[3], bbox_stor[4]))
load("~/pCloudDrive/FACE-IT_data/young_sound/sst_CCI_young.RData")
sst_CCI_young_bbox <- filter(sst_CCI_young, between(lon, bbox_young[1], bbox_young[2]), between(lat, bbox_young[3], bbox_young[4]))
load("~/pCloudDrive/FACE-IT_data/disko_bay/sst_CCI_disko.RData")
sst_CCI_disko_bbox <- filter(sst_CCI_disko, between(lon, bbox_disko[1], bbox_disko[2]), between(lat, bbox_disko[3], bbox_disko[4]))
load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/sst_CCI_nuup.RData")
sst_CCI_nuup_bbox <- filter(sst_CCI_nuup, between(lon, bbox_nuup[1], bbox_nuup[2]), between(lat, bbox_nuup[3], bbox_nuup[4]))
load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/sst_CCI_por.RData")
sst_CCI_por_bbox <- filter(sst_CCI_por, between(lon, bbox_por[1], bbox_por[2]), between(lat, bbox_por[3], bbox_por[4]))

# Comparisons of SST pixels in/out of the site bbox
# ggplot(distinct(sst_young_bbox[c("lon", "lat")]), aes(x = lon, y = lat)) +
#   geom_tile(colour = "red") +
#   # geom_raster(distinct(sst_young_bbox[c("lon", "lat")]), aes(x = lon, y = lat)) +
#   geom_rect(aes(xmin = bbox_young[1], xmax = bbox_young[2], 
#                 ymin = bbox_young[3], ymax = bbox_young[4]))

# Sea ice data 4 km
load("~/pCloudDrive/FACE-IT_data/kongsfjorden/ice_4km_kong.RData")
load("~/pCloudDrive/FACE-IT_data/isfjorden/ice_4km_is.RData")
load("~/pCloudDrive/FACE-IT_data/storfjorden/ice_4km_stor.RData")
load("~/pCloudDrive/FACE-IT_data/young_sound/ice_4km_young.RData")
load("~/pCloudDrive/FACE-IT_data/disko_bay/ice_4km_disko.RData")
load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/ice_4km_nuup.RData")
load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/ice_4km_por.RData")

# Sea ice data 1 km
## Not currently used
# load("~/pCloudDrive/FACE-IT_data/kongsfjorden/ice_1km_kong.RData")
# load("~/pCloudDrive/FACE-IT_data/isfjorden/ice_1km_is.RData")
# load("~/pCloudDrive/FACE-IT_data/storfjorden/ice_1km_stor.RData")
# load("~/pCloudDrive/FACE-IT_data/young_sound/ice_1km_young.RData")
# load("~/pCloudDrive/FACE-IT_data/disko_bay/ice_1km_disko.RData")
# load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/ice_1km_nuup.RData")
# load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/ice_1km_por.RData")


# Section 2 ---------------------------------------------------------------
# Mostly annual and monthly mean states of key drivers
# Alongfjord gradients is about as complex as we want to get
# Limit to the top 10 metres. Consider the bottom 10 metres.
# Line plots comparing averages values across sites
# Solid colour for in situ data, dashed line for NOAA, dotted for CCI
# Also produce summary stats
## Mean, median, min, max, skewness, kurtosis


## Ocean Temperature -------------------------------------------------------

# Remove air, CO2, and pH related temperature values
# TTT is air temperature from cruise data on PANGAEA. e.g. https://doi.pangaea.de/10.1594/PANGAEA.326679
# MAAT + MAGT = ground temperatures e.g. https://doi.pangaea.de/10.1594/PANGAEA.808512
# MAT = mean annual temperature e.g. https://doi.pangaea.de/10.1594/PANGAEA.907818
# Remove overly processed variables (e.g. average summer SST)
# Remove slightly different variables
# tequ = temperature at equilibrium; ~+0.6°C than the corresponding water temp
# e.g. https://doi.pangaea.de/10.1594/PANGAEA.849863
# T intern [°C] = internal temperature; ~+0.03°C than the corresponding water temp
# e.g. https://doi.pangaea.de/10.1594/PANGAEA.930028
# Removing tpot (Potential temperature) is a potentially controversial decision...
# t [°C] = ground/snow temperatures e.g. https://doi.pangaea.de/10.1594/PANGAEA.930472
# T tech [°C] + T cal [°C] = Temperatures from an experiment e.g. https://doi.pangaea.de/10.1594/PANGAEA.847626
kong_OISST <- sst_kong_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
kong_CCI <- sst_CCI_kong_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
kong_SST <- review_filter_var(full_product_kong, "kong", "temp|°C",
                              "air|co2|ph_|pHint_|TTT|MAAT|MAGT|MAT|
                              |SST sum|SST win|Temp min|Temp max|Temp interp|
                              |tequ|tpot|T intern") %>%
  bind_rows(kong_OISST, kong_CCI) %>% mutate(site = "kong")
is_OISST <- sst_is_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
is_CCI <- sst_CCI_is_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
is_SST <- review_filter_var(full_product_is, "is", "temp|°C", 
                            "SST sum|SST win|TTT|MAT|MAGT|MAAT|Tpot|Tequ|air|T intern|T tech|T cal|pHT|
                            |T sum|T win|SST anomaly|theta", c("t [°C]", "SST (1-12) [°C]")) %>% # Can re-add if annual values
  bind_rows(is_OISST, is_CCI) %>% mutate(site = "is")
stor_OISST <- sst_stor_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
stor_CCI <- sst_CCI_stor_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
stor_SST <- review_filter_var(full_product_stor, "stor", "temp|°C", "Tpot|Tequ|theta|fco2|Tmax|TTT|
                              |SST anomaly", c("t [°C]", "SST (1-12) [°C]")) %>% 
  bind_rows(stor_OISST, stor_CCI) %>% mutate(site = "stor")
young_OISST <- sst_young_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
young_CCI <- sst_CCI_young_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
young_SST <- review_filter_var(rbind(full_product_young, young_GEM), "young", "temp|°C", 
                               "Tpot|Tequ|theta|fco2|pot_temp|SST sum|SST win|MAGT|MAAT|TTT") %>% 
  bind_rows(young_OISST, young_CCI) %>% mutate(site = "young")
disko_OISST <- sst_disko_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
disko_CCI <- sst_CCI_disko_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
disko_SST <- review_filter_var(rbind(full_product_disko, disko_GEM), "disko", "temp|°C", 
                               "Tequ|potential|theta|fco2|SST sum|SST win|TTT|SST anomaly|ice_", "SST (1-12) [°C]") %>% 
  bind_rows(disko_OISST, disko_CCI) %>% mutate(site = "disko")
nuup_OISST <- sst_nuup_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
nuup_CCI <- sst_CCI_nuup_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
nuup_SST <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "nuup", "temp|°C", 
                              "Tequ|T tech|Tpot|SST sum|SST win|TTT") %>% 
  bind_rows(nuup_OISST, nuup_CCI) %>% mutate(site = "nuup")
por_OISST <- sst_por_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
por_CCI <- sst_CCI_por_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
por_SST <- review_filter_var(full_product_por, "por", "temp|°C", 
                             "Tequ|Tpot|TTT|wet bulb|SST anomaly|T air|MAAT", "SST (1-12) [°C]") %>% 
  bind_rows(por_OISST, por_CCI) %>% mutate(site = "por")
# review_filter_check(por_SST)

# Combined cleaned data
clean_SST <- rbind(kong_SST, is_SST, stor_SST, young_SST, disko_SST, nuup_SST, por_SST) %>% 
  mutate(var_name = "temp [°C]", var_type = "phys",
         depth = case_when(is.na(depth) & type %in% c("OISST", "CCI") ~ 0, TRUE ~ depth),
         date_accessed = as.Date(date_accessed),
         date_accessed = case_when(type == "CCI" ~ as.Date("2021-12-13"),
                                   type == "OISST" ~ as.Date("2021-12-03"),
                                   TRUE ~ date_accessed),
         URL = case_when(type == "CCI" ~ "http://dap.ceda.ac.uk/thredds/fileServer/neodc/c3s_sst/data/ICDR_v2/Analysis/L4/v2.0",
                         type == "OISST" ~ "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/",
                         TRUE ~ URL),
         citation = case_when(type == "CCI" ~ "Merchant, C. J., Embury, O., Bulgin, C. E., Block, T., Corlett, G. K., Fiedler, E., et al. (2019). Satellite-based time-series of sea-surface temperature since 1981 for climate applications. Scientific data 6, 1–18.",
                              type == "OISST" ~ "Huang, B., Liu, C., Banzon, V., Freeman, E., Graham, G., Hankins, B., Smith, T., Zhang, H. (2021). Improvements of the Daily Optimum Interpolation Sea Surface Temperature (DOISST) Version 2.1. J. Climate, doi: 10.1175/JCLI-D-20-0166.1",
                              TRUE ~ citation),
         var_group = "Sea temp") %>% 
  filter(depth >= 0)
rm(kong_SST, is_SST, stor_SST, young_SST, disko_SST, nuup_SST, por_SST); gc()

# Summary analyses
summary_SST <- review_summary(filter(clean_SST, depth >= 0, depth <= 10))

# Plot results
# NB: The apparent cooling trend from in situ data is due to the lack of winter temperatures from pre-satellite era data
review_summary_plot(summary_SST, "temp")

## Plot showing spatial difference between temperature products
### This may not work well across all sites


## Air temperature --------------------------------------------------------

# Get air temperature for all sites
kong_air <- review_filter_var(full_product_kong, "kong", "air|temp|°C|TTT", 
                              "SST|co2|intern|tequ|f_|p_|par_|temp_|interp|_ctd|_sf|MAGT|MAT|MAAT|mean_|T air", # We want T air but it requires processing before this step
                              var_precise = "Temp [°C]", atmos = T) %>% filter(is.na(depth) | depth < 0) %>% 
  filter(URL != "https://doi.org/10.1594/PANGAEA.882432", URL != "https://doi.org/10.1594/PANGAEA.839802") %>% add_depth()
is_air <- review_filter_var(full_product_is, "is", "air|temp|°C|TTT", 
                            "SST|tequ|intern|bulb|pressure|MAGT|MAAT|MAT|T cal|T tech|mean_|T air|T sum|T win",  
                            var_precise = c("Temp [°C]", "t [°C]"), atmos = T) %>% 
  filter(is.na(depth) | depth < 0) %>% add_depth()
stor_air <- review_filter_var(full_product_stor, "stor", "air|temp|°C", "SST|Tmax|mean_",
                              var_precise = c("Temp [°C]", "t [°C]"), atmos = T) %>% 
  filter(is.na(depth) | depth < 0) %>% add_depth()
young_air <- review_filter_var(full_product_young, "young", "air|temp|°C", "MAGT|MAAT", var_precise = "Temp [°C]", atmos = T) %>% 
  filter(is.na(depth) | depth < 0) %>% add_depth() # No air temperatures
disko_air <- review_filter_var(full_product_disko, "disko", "air|temp|°C", "SST|tequ", var_precise = "Temp [°C]", atmos = T) %>% 
  filter(is.na(depth) | depth < 0) %>% add_depth()
nuup_air <- review_filter_var(full_product_nuup, "nuup", "air|temp|°C", "tequ", var_precise = "Temp [°C]", atmos = T) %>% 
  filter(is.na(depth) | depth < 0) %>% add_depth()
por_air <- review_filter_var(full_product_por, "por", "air|temp|°C", "SST|tequ|bulb|MAAT|T air", 
                             var_precise = c("Temp [°C]", "temp [°C]"), atmos = T) %>% 
  filter(is.na(depth) | depth < 0) %>% add_depth()

# Combined cleaned data
clean_air <- rbind(kong_air, is_air, stor_air, young_air, disko_air, nuup_air, por_air) %>% mutate(var_group = "Air temp")
rm(kong_air, is_air, stor_air, young_air, disko_air, nuup_air, por_air); gc()

# Summary analyses
summary_air <- review_summary(clean_air)

# Plot results
review_summary_plot(summary_air, "air")


## Salinity ---------------------------------------------------------------

# "Knowledge gaps include uncertainties in projecting the future salinity level, as well as stratification and potential rate of internal loading, under different climate forcings."
# "This weakens our ability to project how pelagic productivity, fish populations and macroalgal communities may change in the future."
# "the decline of marine taxa has usually been proposed to be linked to a decrease of salinity (Suikkanen et al., 2013; Hänninen et al., 2015)"

# Get all salinity data
# NB: Remove Sal [mg/l]
# Remove overly processed variables
# sal interp e.g. https://doi.org/10.1594/PANGAEA.877869
# Remove glacial drainage land stations
kong_sal <- review_filter_var(full_product_kong, "kong", "sal|PSU|s_", "interp|ph|oxy|ws",
                              cit_filter = "land station|drainage|meltwater")
is_sal <- review_filter_var(full_product_is, "is", "sal|PSU", "interp|mg/l")
stor_sal <- review_filter_var(full_product_stor, "stor", "sal|PSU", "interp|acu|ent")
young_sal <- review_filter_var(rbind(full_product_young, young_GEM), "young", "sal|PSU", "sal interp|acu|ent")
disko_sal <- review_filter_var(rbind(full_product_disko, disko_GEM), "disko", "sal|PSU", "sal interp")
nuup_sal <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "nuup", "sal|PSU", "sal interp|acu|ent")
por_sal <- review_filter_var(full_product_por, "por", "sal|PSU", "Sal interp")
clean_sal <- rbind(kong_sal, is_sal, stor_sal, young_sal, disko_sal, nuup_sal, por_sal) %>%
  mutate(var_name = "sal", var_group = "Salinity")
rm(kong_sal, is_sal, stor_sal, young_sal, disko_sal, nuup_sal, por_sal); gc()

# Summary analyses
summary_sal <- review_summary(filter(clean_sal, depth >= 0, depth <= 10))

# Plot results
review_summary_plot(summary_sal, "sal")


## PAR --------------------------------------------------------------------

# Contact Kai about what he would want done about spectrally resolved data
# Also about UV, but first check what data exist

# Get all PAR data
kong_PAR <- review_filter_var(full_product_kong, "kong", "PAR", "Onc|Gym|Para|below|abys")
is_PAR <- review_filter_var(full_product_is, "is", "PAR", "aeuch|eleg") # No PAR data
stor_PAR <- review_filter_var(full_product_stor, "stor", "PAR") # No PAR data
young_PAR <- review_filter_var(rbind(full_product_young, young_GEM), "young", "PAR", "vella|tinn")
## NB: It is unclear if these values should be divided by 10 or not
## It is also unclear what the time dimension is for the data
disko_PAR <- review_filter_var(rbind(full_product_disko, disko_GEM), "disko", "PAR", "milli") %>% filter(value > 0)
## NB: Some of these values are also very high
nuup_PAR <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "nuup", "PAR", "trip|vella|sulc|lip|lib|parv")
por_PAR <- review_filter_var(full_product_por, "por", "PAR") # No PAR data
clean_PAR <- rbind(kong_PAR, is_PAR, stor_PAR, young_PAR, disko_PAR, nuup_PAR, por_PAR) %>% 
  mutate(var_name = "PAR [µmol m-2 s-1]", var_group = "PAR")
rm(kong_PAR, is_PAR, stor_PAR, young_PAR, disko_PAR, nuup_PAR, por_PAR); gc()

# Summary analyses
summary_PAR <- review_summary(filter(clean_PAR, depth >= 0, depth <= 10))

# Plot results
review_summary_plot(summary_PAR, "par")


## Sea ice ----------------------------------------------------------------

# Collect all ice related data
# https://doi.org/10.1594/PANGAEA.935267; bi [code] = ice of land origin, ci [code] = sea ice concentration, zi [code] = ice situation
# https://doi.org/10.1594/PANGAEA.269605; t [°C] = temperature ice/snow
# https://doi.org/10.1594/PANGAEA.269619; DI [code] = bearing of principal ice edge, l_s [code] = type of ice accretion
# https://doi.org/10.1594/PANGAEA.935267; EsEs [m] = sea ice thickness, EsEs acc [cm] = thickness of ice accretion
# https://doi.org/10.1594/PANGAEA.896581; RGI 6.0 ID = Randolph glacier inventory, SWE [m] = snow water equivalent, SWE unc [m] - Uncertainty
# https://doi.org/10.1594/PANGAEA.869294; IP [km**3/day] = sea ice production
# https://doi.org/10.1594/PANGAEA.908494; SIC d [months/a] = Sea ice cover duration NB: This file is a good candidate for checking pipeline errors
# https://doi.org/10.1594/PANGAEA.815951; Glac w [km] = Glacier width
# https://doi.org/10.1594/PANGAEA.59224; IRD [arbitrary units] = Ice rafted debris, general
kong_sea_ice <- filter(full_product_kong, var_type == "cryo", var_name == "ice cover [%]") %>% mutate(site = "kong") # Sea ice percent cover of inner fjord
is_sea_ice <- filter(full_product_is, var_type == "cryo",
                     URL != "https://doi.org/10.1594/PANGAEA.57721", # Glacial maximum ice sheet extension
                     var_name %in% c("EsEs acc [cm]", "Ice extent")) %>% mutate(site = "is") # Sea ice extent and ice accretion
stor_sea_ice <- filter(full_product_stor, var_type == "cryo",
                       URL != "https://doi.org/10.1594/PANGAEA.57721",
                       var_name %in% c("Ice conc [tenths]", "Ice cov [%]", "Ice extent", 
                                       "IP [km**3/day]", "EsEs [m]")) %>% mutate(site = "stor") # Sea ice percent cover and thickness
young_sea_ice <- filter(rbind(full_product_young, young_GEM), var_type == "cryo",
                        !grepl("snow", var_name),
                        !grepl("Hynek, Bernhard; Binder, Daniel;", citation),
                        !grepl("Hynek, Bernhard; Weyss, Gernot;", citation)) %>% mutate(site = "young") # A lot of GEM data
disko_sea_ice <- filter(rbind(full_product_disko, disko_GEM), var_type == "cryo") %>% mutate(site = "disko") %>% slice(0) # No sea ice data
nuup_sea_ice <- filter(rbind(full_product_nuup, nuup_GEM), var_type == "cryo") %>% mutate(site = "nuup") %>% slice(0) # No sea ice data
por_sea_ice <- filter(full_product_por, var_type == "cryo", URL != "https://doi.org/10.1594/PANGAEA.57721") %>% mutate(site = "por")
clean_sea_ice <- rbind(kong_sea_ice, is_sea_ice, stor_sea_ice, young_sea_ice, disko_sea_ice, nuup_sea_ice, por_sea_ice) %>% 
  mutate(type = "in situ", var_group = "Ice vars")
rm(kong_sea_ice, is_sea_ice, stor_sea_ice, young_sea_ice, disko_sea_ice, nuup_sea_ice, por_sea_ice); gc()

# Figures
## Need custom figures per site
## Consistent metadata files may not be useful across sites
# filter(all_sea_ice, !var_name %in% c("Open water [start date]", "Open water [end date]"))
ggplot(clean_sea_ice, aes(x = date, y = value, colour = site)) +
  geom_point() + geom_line() + 
  facet_wrap(~var_name, scales = "free_y")
ggsave("~/Desktop/anlyses_output/ice_var_ts.png", width = 20, height = 16)

## Not a lot of common sea ice data between sites
## The gridded data sea ice cover will be the best comparison between sites
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

# Sea ice proportion cover change over time
ice_4km_proc <- rbind(ice_4km_kong_proc, ice_4km_is_proc, ice_4km_stor_proc, ice_4km_young_proc,
                      ice_4km_disko_proc, ice_4km_nuup_proc, ice_4km_por_proc)
save(ice_4km_proc, file = "data/analyses/ice_4km_proc.RData")
ice_4km_prop <- plyr::ddply(ice_4km_proc, c("site"), ice_cover_prop, .parallel = T)
# rm(ice_4km_kong, ice_4km_is, ice_4km_stor, ice_4km_young, ice_4km_disko, ice_4km_nuup, ice_4km_por,
#    ice_4km_kong_proc, ice_4km_is_proc, ice_4km_stor_proc, ice_4km_young_proc,
#    ice_4km_disko_proc, ice_4km_nuup_proc, ice_4km_por_proc); gc()

# Calculate trends
ice_4km_trend <- plyr::ddply(dplyr::rename(ice_4km_prop, val = mean_prop), c("site", "month"), trend_calc, .parallel = T)

# Combine with other clean data
ice_4km_prop_long <- ice_4km_prop %>%
  dplyr::rename(value = mean_prop) %>% 
  dplyr::select(date, value, site) %>% 
  mutate(var_name = "sea ice cover [proportion]")
ice_4km_trend_long <- ice_4km_trend %>% 
  pivot_longer(trend:sd_val, names_to = "var_name") %>% 
  mutate(var_name = case_when(var_name == "trend" ~ paste0("sea ice cover ",month," [annual proportion trend]"),
                              var_name == "p.value" ~ paste0("sea ice cover ",month," [annual proportion trend p-value]"),
                              var_name == "mean_val" ~ paste0("sea ice cover ",month," [mean proportion]"),
                              var_name == "sd_val" ~ paste0("sea ice cover ",month," [SD proportion]"))) %>% 
  dplyr::select(-month)
ice_4km_stats <- bind_rows(ice_4km_prop_long, ice_4km_trend_long) %>% 
  mutate(type = "remote",
         var_type = "cryo",
         var_group = "Ice vars",
         date_accessed = as.Date("2022-04-26"),
         URL = "https://doi.org/10.7265/N5GT5K3K",
         citation = "U.S. National Ice Center and National Snow and Ice Data Center. Compiled by F. Fetterer, M. Savoie, S. Helfrich, and P. Clemente-Colón. 2010, updated daily. Multisensor Analyzed Sea Ice Extent - Northern Hemisphere (MASIE-NH), Version 1. 4km resolution. Boulder, Colorado USA. NSIDC: National Snow and Ice Data Center. doi: https://doi.org/10.7265/N5GT5K3K.")
clean_sea_ice <- bind_rows(clean_sea_ice, ice_4km_stats)

# Analyses
summary_ice <- review_summary(clean_sea_ice)

# Proportion figures
ice_4km_trend$x <- as.Date("2003-06-01")
ice_4km_trend$y <- rep(seq(0, 1, length.out = 7), each = 12)
ggplot(ice_4km_prop, aes(x = date, y = mean_prop, colour = site)) +
  geom_point() + geom_smooth(method = "lm", se = F) + facet_wrap(~month) + 
  geom_label(data = ice_4km_trend, show.legend = F,
             aes(x = x, y = y, colour = site,
                 label = paste0(trend,"/year\n p = ", p.value))) +
  scale_x_date(limits = c(as.Date("2000-09-01"), as.Date("2021-12-31")), expand = c(0, 0)) +
  labs(x = NULL, y = "Sea ice cover [proportion]", colour = "Site")
ggsave("~/Desktop/analyses_output/ice_prop_ts.png", height = 12, width = 20)
ggplot(ice_4km_prop, aes(x = as.factor(month), y = mean_prop, fill = site)) +
  geom_boxplot() + facet_wrap(~month, scales = "free_x") +
  labs(x = "Month", y = "Sea ice cover [proportion]", colour = "Site")
ggsave("~/Desktop/analyses_output/ice_prop_box_month.png", height = 6, width = 12)
ggplot(ice_4km_prop, aes(x = as.factor(month), y = mean_prop, fill = site)) +
  geom_boxplot() + facet_wrap(~site, scales = "free_x") +
  labs(x = "Month", y = "Sea ice cover [proportion]", colour = "Site")
ggsave("~/Desktop/analyses_output/ice_prop_box_site.png", height = 9, width = 12)
rm(ice_4km_prop, ice_4km_prop_long, ice_4km_trend, ice_4km_trend_long, ice_4km_stats); gc()

# Calculate sea ice breakup and formation dates
## Not sure if this is useful/comparable for all the different sites. e.g. Young Sound vs. Disko Bay
## Consider calculating open water days


## Snow --------------------------------------------------------------------

# See sea ice section for notes on curious variables
kong_snow <- filter(full_product_kong, var_type == "cryo") %>% mutate(site = "kong") %>% slice(0) # No snow data
is_snow <- filter(full_product_is, var_type == "cryo",
                  URL != "https://doi.org/10.1594/PANGAEA.930472", # Benthos data
                  var_name %in% c("t [°C]", "Snow depth, unc [m]", "Density snow [kg/m**3]", "Snow density, unc [kg/m**3]",
                                  "SWE [m]", "SWE unc [m]" )) %>% mutate(site = "is") # Many snow values
stor_snow <- filter(full_product_stor, var_type == "cryo",
                    var_name %in% c("t [°C]", "Depth ice/snow [m]", "Snow depth, unc [m]", "Density snow [kg/m**3]",
                                    "Snow density, unc [kg/m**3]", "SWE [m]", "SWE unc [m]", "Snow thick [m]")) %>% mutate(site = "stor") # Many snow values
young_snow <- filter(rbind(full_product_young, young_GEM), var_type == "cryo",
                     var_name %in% c("Snow h [m]", "Density snow [kg/m**3]", "Sea ice snow thickness [cm]")) %>% 
  mutate(site = "young") # A lot of geospatial glacier data... consider removing or averaging
disko_snow <- filter(rbind(full_product_disko, disko_GEM), var_type == "cryo", grepl("snow", var_name, ignore.case = T)) %>% mutate(site = "disko") # Snow depth and snow line
nuup_snow <- filter(rbind(full_product_nuup, nuup_GEM), var_type == "cryo", grepl("snow", var_name, ignore.case = T)) %>% mutate(site = "nuup") # Snow depth
por_snow <- filter(full_product_por, var_type == "cryo") %>% mutate(site = "por") %>% slice(0) # No snow data
clean_snow <- bind_rows(kong_snow, is_snow, stor_snow, young_snow, disko_snow, nuup_snow, por_snow) %>% 
  mutate(type = "in situ", var_group = "Snow vars")
rm(kong_snow, is_snow, stor_snow, young_snow, disko_snow, nuup_snow, por_snow); gc()

# Summary analyses
summary_snow <- review_summary(clean_snow)

# Plot results
review_summary_plot(summary_snow, "snow")


## Glacier -----------------------------------------------------------------

# NB: Chose not to get many variables from Geyman et al. 2021

# Test check for all cryo vars to make sure no glacier vars are missed
as.vector(distinct(filter(full_product_stor, var_type == "cryo"), var_name))
as.vector(distinct(filter(nuup_GEM, var_type == "cryo"), var_name))

# Get all glacier variables
kong_glacier <- review_filter_var(full_product_kong, "kong", "balance|glacier|area|volume|slope")
is_glacier <- review_filter_var(full_product_is, "is", "balance|glacier|area|volume|slope")
stor_glacier <- review_filter_var(full_product_stor, "stor", "balance|glacier|area|volume|slope")
young_glacier <- review_filter_var(rbind(full_product_young, young_GEM), "young", "balance|glacier|ablation")
disko_glacier <- review_filter_var(rbind(full_product_disko, disko_GEM), "disko", "balance|glacier|ablation")
nuup_glacier <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "nuup", "glac", "poro")
por_glacier <- review_filter_var(full_product_por, "por", "balance|glac") # No glacier data
clean_glacier <- rbind(kong_glacier, is_glacier, stor_glacier, young_glacier, disko_glacier, nuup_glacier, por_glacier) %>% 
  mutate(var_group = "Glacier vars")
rm(kong_glacier, is_glacier, stor_glacier, young_glacier, disko_glacier, nuup_glacier, por_glacier); gc()

# Summary analyses
summary_glacier <- review_summary(clean_glacier)

# Plot results
review_summary_plot(summary_glacier, "glacier")

# Grab glacier values directly from EU or Svalbard products for certainty
# Look for specific DOI in each site file

## River discharge --------------------------------------------------------

# Pedro Duarte has contacted a colleague to get Kongsfjorden area river discharge data

# GRDC river discharge data
## NB: These are restricted data so they are not added to 'full_product_EU'
# lta_discharge = long-term average discharge, cubic metre per sec
# r_vol_yr = mean annual volume, cubic kilometre
# r_height_yr	= mean annual runoff depth, mm
EU_GRDC <- read_csv("~/pCloudDrive/restricted_data/GRDC/grdc_arctichycos_stations.csv")
site_GRDC <- map_dfr(dir("~/pCloudDrive/restricted_data/GRDC", pattern = "Cmd.txt", full.names = T), load_GRDC)

# Get all river discharge data from full/GEM products
kong_river <- review_filter_var(full_product_kong, "kong", "river|disc|Q", "Disco|hetero|equ|AT|dhdt") # No river discharge data
is_river <- review_filter_var(full_product_is, "is", "river|disc|Q", "equ|hPa|dhdt") # No river discharge data
stor_river <- review_filter_var(full_product_stor, "stor", "river|disc|Q", "equ|AT|dhdt") # No river discharge data
young_river <- review_filter_var(rbind(full_product_young, young_GEM), "young", "river|disc|Q", "coscin|Qnet")
disko_river <- review_filter_var(rbind(full_product_disko, disko_GEM), "disko", "river|disc|Q", "equ") # No river discharge data
nuup_river <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "nuup", "river|disc|Q", "equ|coscin|prot|psamm")
por_river <- review_filter_var(full_product_por, "por", "river|disc|Q", "equ")# No river discharge data

# Get river data from GRDC database
FACE_IT_GRDC <- site_GRDC %>% 
  mutate(site = case_when(lon >= bbox_kong[1] & lon <= bbox_kong[2] & lat >= bbox_kong[3] & lat <= bbox_kong[4] ~ "kong",
                          lon >= bbox_is[1] & lon <= bbox_is[2] & lat >= bbox_is[3] & lat <= bbox_is[4] ~ "is",
                          lon >= bbox_stor[1] & lon <= bbox_stor[2] & lat >= bbox_stor[3] & lat <= bbox_stor[4] ~ "stor",
                          lon >= bbox_young[1] & lon <= bbox_young[2] & lat >= bbox_young[3] & lat <= bbox_young[4] ~ "young",
                          lon >= bbox_disko[1] & lon <= bbox_disko[2] & lat >= bbox_disko[3] & lat <= bbox_disko[4] ~ "disko",
                          lon >= bbox_nuup[1] & lon <= bbox_nuup[2] & lat >= bbox_nuup[3] & lat <= bbox_nuup[4] ~ "nuup",
                          lon >= bbox_por[1] & lon <= bbox_por[2] & lat >= bbox_por[3] & lat <= bbox_por[4] ~ "por")) %>% 
  filter(!is.na(site)) %>% 
  pivot_longer(`Q [m3/s]`, names_to = "var_name") %>% 
  mutate(var_type = "cryo", date_accessed = as.Date("2022-06-13"), type = "in situ",
         URL = "https://www.bafg.de/GRDC/EN/04_spcldtbss/41_ARDB/ardb_node.html", 
         citation = "Arctic Region Discharge Data (2021). The Global Runoff Data Centre, 56068 Koblenz, Germany") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value, site, type)

# Combine all datasets and clean up
clean_river <- rbind(kong_river, is_river, stor_river, young_river, disko_river, nuup_river, por_river, FACE_IT_GRDC) %>% 
  mutate(var_group = "river")
rm(kong_river, is_river, stor_river, young_river, disko_river, nuup_river, por_river, EU_GRDC, FACE_IT_GRDC); gc()

# Summary analyses
summary_river <- review_summary(clean_river)

# Plot results
review_summary_plot(summary_river, "river")


## Oxygen -----------------------------------------------------------------

# NB: According to Mikael Sejr (via Slack) the older Young Sound O2 data that are much higher are 
# due to sensor drift, but there's not much to be done for it.
# Therefore these data should be marked as "raw sensor output"
# Oxygen isotope data may be of interest to someone in FACE-IT

# Test check for all bio vars to make sure no glacier vars are missed
as.vector(distinct(filter(full_product_is, var_type == "chem"), var_name))
as.vector(distinct(filter(nuup_GEM, var_type == "chem"), var_name))

# Get all O2 variables
# oxy [1] is "Fractional saturation of oxygen in sea water" 
# ncdump::NetCDF("/home/robert/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_S/1841_RCM_464_QC.nc")
# Contacted Johnna (2022-05-24) about the difference between corrected and uncorrected 02 in Young Sound
# Confirmed with Johnna Holding (2022-06-01) that 'oxygen_corrected_umol_kg' are the better values to use
# It is unclear what Oxygen a and b are: https://data.g-e-m.dk/datasets?doi=10.17897/WH30-HT61
kong_O2 <- review_filter_var(full_product_kong, "kong", "O2|ox|02", "pc|no|amph|tel|rhis")
is_O2 <- review_filter_var(full_product_is, "is", "O2|ox|02", "tonnes|pc|no|tc|vmax")
stor_O2 <- review_filter_var(full_product_stor, "stor", "O2|ox|02", "tonnes|pc|no|tc|fc")
young_O2 <- review_filter_var(rbind(full_product_young, young_GEM), "young", "O2|ox|02", "no|pc|favella|oxygen_umol_kg")
disko_O2 <- review_filter_var(rbind(full_product_disko, disko_GEM), "disko", "O2|ox|02", "pc|no|tc|fc") # a and b values are odd
nuup_O2 <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "nuup", "O2|ox|02", "pc|no|cox|myox")
por_O2 <- review_filter_var(full_product_por, "por", "O2|ox|02", "pc") # No O2 data
clean_O2 <- rbind(kong_O2, is_O2, stor_O2, young_O2, disko_O2, nuup_O2, por_O2) %>% 
  # Correct negative proportion/percentage and remove other negative and 0 values
  mutate(value = case_when(var_name == "oxygen [% sat]" ~ value + 100, TRUE ~ value)) %>% 
  filter(value > 0) %>% 
  # create standardised names
  mutate(var_name = case_when(var_name %in% c("oxygen [µmol/l]") ~ "O2 [µmol/l]",
                              var_name %in% c("oxygen [μmol kg-1]", "oxygen_umol_kg [µmol kg-1]", 
                                              "oxygen_umol_kg [µmol kg-1]", "oxygen [µmol/kg]",
                                              "oxygen_corrected_umol_kg [µmol kg-1]") ~ "O2 [µmol/kg]",
                              var_name %in% c("oxygen_mmkg [mmol/kg]") ~ "O2 [mmol/kg]",
                              var_name %in% c("O2 sat [%]", "OXYS [1]", "oxygen_sat [%]", "oxygen [% sat]") ~ "O2 [% sat]",
                              var_name %in% c("diss_oxygen [mg/l]" ) ~ "O2 [mg/l]",
                              var_name %in% c("OXY [mol m-3]") ~ "O2 [mol/m^3]",
                              var_name %in% c("oxy [1]") ~ "O2 [fractional saturation]",
                              var_name %in% c("oxygen a [mg/l]") ~ "O2 a [mg/l]",
                              var_name %in% c("oxygen b [mg/l]") ~ "O2 b [mg/l]",
                              var_name %in% c("oxygen [ml/l]") ~ "O2 [ml/l]",
                              TRUE ~ var_name),
         var_group = "O2")
rm(kong_O2, is_O2, stor_O2, young_O2, disko_O2, nuup_O2, por_O2); gc()

# Summary analyses
summary_O2 <- review_summary(clean_O2)

# Plot results
review_summary_plot(summary_O2, "O2")


## pCO2 -------------------------------------------------------------------

# NB: For this and other chemistry variables see best practices sent by JP on Slack
# Also see e-mail from Liqing Jiang

# Keep pCO2_calc as a separate variable because they can't be taken as absolutely the same
# Same foe PCO2water_SST_wet
# Can use SeaCarb to transform fco2 to pCO2

# Get all pCO2 data
# Note that there are duplicates from GLODAP and the underlying files downloaded via PANGAEA
# But this is actually a good thing as it allows us to acknowledge specific contributors,
# which is something that the GLODAP product requests that we do.
kong_pCO2 <- review_filter_var(full_product_kong, "kong", "CO2")
is_pCO2 <- review_filter_var(full_product_is, "is", "CO2", "emissions|tco2")
stor_pCO2 <- review_filter_var(full_product_stor, "stor", "CO2", "emissions|tco2|fco2")
young_pCO2 <- review_filter_var(rbind(full_product_young, young_GEM), "young", "CO2") # No pCO2 data
disko_pCO2 <- review_filter_var(rbind(full_product_disko, disko_GEM), "disko", "CO2", "fco2|tco2")
nuup_pCO2 <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "nuup", "CO2")
por_pCO2 <- review_filter_var(full_product_por, "por", "CO2")
clean_pCO2 <- rbind(kong_pCO2, is_pCO2, stor_pCO2, young_pCO2, disko_pCO2, nuup_pCO2, por_pCO2) %>% 
  mutate(var_group = "pCO2")
rm(kong_pCO2, is_pCO2, stor_pCO2, young_pCO2, disko_pCO2, nuup_pCO2, por_pCO2); gc()

# Summary analyses
summary_pCO2 <- review_summary(filter(clean_pCO2, depth >= 0, depth <= 10))

# Plot results
review_summary_plot(summary_pCO2, "pCO2")


## pH ----------------------------------------------------------------------

# This is perhaps unnecessary if we are also looking at pCO2


## Nutrients ---------------------------------------------------------------

# "The associated increase in N:P ratio may contribute to maintaining the “vicious circle of eutrophication”. "
# "An increase of riverine dissolved organic matter (DOM) may also decrease primary production, but the relative importance of this process in different sea areas is not well known."
# "Climate change will probably delay the effects of nutrient abatement and tend to keep the ecosystem in its “novel” state."
# "However, several modelling studies conclude that nutrient reductions will be a stronger driver for ecosystem functioning of the Baltic Sea than climate change."
# "Such studies highlight the importance of studying the Baltic Sea as an interlinked socio-ecological system."

# [µmol/l] is the same as [µg-at/l]
# [µmol/l] vs [μmol kg-1] are different, a conversion must be made between them

# Keep Nitrate + Nitrite

# Same same
# - [NO2]- vs NO2
# - PO4 vs [PO4]3-

# Get all nutrient data
kong_nutrients <- review_filter_var(full_product_kong, "kong", "nitr|amon|phos|silic|NO3|NO2|NH4|PO4|SiO4", "stddev|stephos", 
                                    var_precise = c("[NO3]- + [NO2]- [µmol/l]", "PO4 biog [%]"))
is_nutrients <- review_filter_var(full_product_is, "is", "nitr|amon|phos|silic|NO3|NO2|NH4|PO4|SiO4", var_precise = "PO4 biog [%]")
stor_nutrients <- review_filter_var(full_product_stor, "stor", "nitr|amon|phos|silic|NO3|NO2|NH4|PO4|SiO4",
                                    var_precise = "NO3 pen depth [mm]")
young_nutrients <- review_filter_var(rbind(full_product_young, young_GEM), "young", "nitr|amon|phos|silic|NO3|NO2|NH4|PO4|SiO4", 
                                     "nitracline", "NO2_NO3 [µmol/l]")
disko_nutrients <- review_filter_var(rbind(full_product_disko, disko_GEM), "disko", "nitr|amon|phos|silic|NO3|NO2|NH4|PO4|SiO4",
                                     var_precise = "[NO3]- + [NO2]- [µmol/l]")
nuup_nutrients <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "nuup", "nitr|amon|phos|silic|NO3|NO2|NH4|PO4|SiO4",
                                    "chlam", var_precise = "[NO3]- + [NO2]- [µmol/l]")
por_nutrients <- review_filter_var(full_product_por, "por", "nitr|amon|phos|silic|NO3|NO2|NH4|PO4|SiO4") # No nutrient data
clean_nutrients <- rbind(kong_nutrients, is_nutrients, stor_nutrients, young_nutrients, disko_nutrients, nuup_nutrients, por_nutrients) %>% 
  # Change GLODAP var_name to match PANGAEA standard 
  mutate(var_name = case_when(var_name == "nitrate [μmol kg-1]" ~ "NO3 [µmol/l]",   
                              var_name == "nitrite [μmol kg-1]" ~ "NO2 [µmol/l]",   
                              var_name == "silicate [μmol kg-1]" ~ "SiO4 [µmol/l]",
                              var_name == "phosphate [μmol kg-1]" ~ "PO4 [µmol/l]",
                              TRUE ~ var_name),
         var_group = "Nutrients"); unique(clean_nutrients$var_name)
rm(kong_nutrients, is_nutrients, stor_nutrients, young_nutrients, disko_nutrients, nuup_nutrients, por_nutrients); gc()

# Summary analyses
summary_nutrients <- review_summary(filter(clean_nutrients, depth >= 0, depth <= 10))

# Plot results
review_summary_plot(summary_nutrients, "nutrients")


## Chla --------------------------------------------------------------------

# "For phytoplankton, clear symptoms of climate change, such as prolongation of the growing season, are evident and can be explained by the warming, but otherwise climate effects vary from species to species and area to area."
# "A 15-year study (2000–2014) using FerryBox observations, covering the area between Helsinki (Gulf of Finland) and Travemünde (Mecklenburg Bight), confirmed that spring bloom intensity was mainly determined by winter nutrient concentration, while bloom timing and duration co-varied with meteorological conditions." 
# "The authors conclude that the bloom magnitude has been affected by the reduction of nutrient loading from land, while bloom phenology can also be modified by global climate change affecting seasonal oceanographic and biogeochemical processes (Groetsch et al., 2016)."

# [10um] vs [GFF] are different methods and both are valid.
# Must keep the difference between them documented.

# Collect all ChlA data
# https://zenodo.org/record/5572041#.YW_Lc5uxU5m: chl_flu [µg chl m-3] = chlorophyll a calculated from fluorescence profile
kong_chla <- review_filter_var(full_product_kong, "kong", "chl", "phyceae|dinium|monas")
is_chla <- review_filter_var(full_product_is, "is", "chl")
stor_chla <- review_filter_var(full_product_stor, "stor", "chl") # No ChlA data
young_chla <- review_filter_var(rbind(full_product_young, young_GEM), "young", "chl", "alpha|pm_|pp_|_frac|max|TOTAL")
disko_chla <- review_filter_var(rbind(full_product_disko, disko_GEM), "disko", "chl")
nuup_chla <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "nuup", "chl", "chlam|macu")
por_chla <- review_filter_var(full_product_por, "por", "chl") # No ChlA data
clean_chla <- rbind(kong_chla, is_chla, stor_chla, young_chla, disko_chla, nuup_chla, por_chla) %>% 
  mutate(var_group = "Chla")
rm(kong_chla, is_chla, stor_chla, young_chla, disko_chla, nuup_chla, por_chla); gc()

# Summary analyses
summary_chla <- review_summary(filter(clean_chla, depth >= 0, depth <= 10))

# Plot results
review_summary_plot(summary_chla, "chla")


## Biomass -----------------------------------------------------------------

# Check this for lot's of variables in Young Sound: https://zenodo.org/record/5572041#.YW_Lc5uxU5m

# Test check for all bio vars to make sure no glacier vars are missed
as.vector(distinct(filter(full_product_kong, var_type == "bio"), var_name))
as.vector(distinct(filter(nuup_GEM, var_type == "bio"), var_name))

# Get all biomass variables
kong_biomass <- filter(full_product_kong, var_type == "bio",
                       !grepl("zooplankton|phytoplankton", citation, ignore.case = T)) %>%
  mutate(site = "kong") %>% slice(0) # No biomass data
is_biomass <- filter(full_product_is, var_type == "bio",
                     var_name == "P CO2 upt Vmax [µmol/kg/s]") %>% mutate(site = "is")
stor_biomass <- filter(full_product_stor, var_type == "bio") %>% mutate(site = "stor") # No bio data
young_biomass <- filter(rbind(full_product_young, young_GEM), var_type == "bio",
                        !grepl("Species Composition", citation),
                        grepl("pp_", var_name, ignore.case = T)) %>% mutate(site = "young") # "pp_" may be too restrictive
disko_biomass <- filter(rbind(full_product_disko, disko_GEM), var_type == "bio") %>% mutate(site = "disko") %>% slice(0) # No biomass data
nuup_biomass <- filter(rbind(full_product_nuup, nuup_GEM), var_type == "bio",
                       !grepl("Species Composition", citation),
                       !grepl("fluor|Chl a", var_name)) %>% mutate(site = "nuup") # Should filter some of the tip growth data
por_biomass <- filter(full_product_por, var_type == "bio") %>% mutate(site = "por") # No bio data
clean_biomass <- rbind(kong_biomass, is_biomass, stor_biomass, young_biomass, disko_biomass, nuup_biomass, por_biomass) %>% 
  mutate(type = "in situ", var_group = "Biomass")
rm(kong_biomass, is_biomass, stor_biomass, young_biomass, disko_biomass, nuup_biomass, por_biomass); gc()

# Summary analyses
summary_biomass <- review_summary(clean_biomass)

# Plot results
review_summary_plot(summary_biomass, "biomass")


## Species assemblage ------------------------------------------------------
# NB: Contact Allison Bailey about more recent plankton species data for Kongsfjorden
# Contacted, but she is in Ny-Alesund until May 23rd.

# For the time being it may be easier to convert these values to the more basic count of simply phytoplankton or zooplankton
# This would then allow for a more simple summary of the meta/data but requires knowledge of the genus per species etc.

# "Several modelling studies project a decrease of phytoplankton bloom in spring and an increase in cyanobacteria blooms in summer."
# "However, uncertainties remain because some field studies claim that cyanobacteria have not increased and some experimental studies show that responses of cyanobacteria to temperature, salinity and pH vary from species to species. "
# "Warming of seawater in spring also speeds up zooplankton growth and shortens the time lag between phytoplankton and zooplankton peaks, which may lead to decreasing of phytoplankton in spring"
# "In summer, a shift towards smaller-sized zooplankton and a decline of marine copepod species has been projected."
# "In the shallower photic systems, heatwaves may produce eutrophication-like effects, e.g. overgrowth of bladderwrack by epiphytes, due to a trophic cascade."
# "It has also been suggested that in the future climate higher temperatures and less ice will cause an earlier bloom of both diatoms and dinoflagellates, with increased dinoflagellate dominance (Hjerne et al., 2019)."
# "Other studies did not find any explanation for the observed changes in the biovolumes of different taxa, e.g. decrease in diatoms and increase in certain dinoflagellate taxa, 
#  and concluded that phytoplankton community in the Baltic Sea is not in a steady state (Olli et al., 2011), or noted that stochastic dynamics at local scales confound any commonalities between phytoplankton groups (Griffiths et al., 2020)."

# Test check for all bio vars to make sure no glacier vars are missed
as.vector(distinct(filter(full_product_is, var_type == "bio"), var_name))
as.vector(distinct(filter(nuup_GEM, var_type == "bio"), var_name))

# Get all species variables
kong_sp_ass <- filter(full_product_kong, var_type == "bio") %>% 
  filter(!grepl("Temperature, salinity, light", citation),
         !grepl("Marine biogeochemistry", citation)) %>% mutate(site = "kong")
is_sp_ass <- filter(full_product_is, var_type == "bio") %>% 
  filter(!var_name %in% c("P CO2 upt Vmax [µmol/kg/s]", "Chlorophyll A - 10um [µg/l]",
                          "Chlorophyll A - GFF [µg/l]", "Phaeophytin - 10um [µg/l]", 
                          "Phaeophytin - GFF [µg/l]")) %>% mutate(site = "is")
stor_sp_ass <- filter(full_product_stor, var_type == "bio") %>% mutate(site = "stor") # No bio data
young_sp_ass <- filter(rbind(full_product_young, young_GEM), var_type == "bio") %>% 
  filter(grepl("Phytoplankton", citation)) %>% mutate(site = "young")
disko_sp_ass <- filter(rbind(full_product_disko, disko_GEM), var_type == "bio") %>% mutate(site = "disko") %>% slice(0) # No species data
nuup_sp_ass <- filter(rbind(full_product_nuup, nuup_GEM), var_type == "bio",
                      grepl("Species Composition", citation)) %>% mutate(site = "nuup")
por_sp_ass <- filter(full_product_por, var_type == "bio") %>% mutate(site = "por") # No bio data
clean_sp_ass <- rbind(kong_sp_ass, is_sp_ass, stor_sp_ass, young_sp_ass, disko_sp_ass, nuup_sp_ass, por_sp_ass) %>% 
  mutate(type = "in situ", var_group = "Species")
rm(kong_sp_ass, is_sp_ass, stor_sp_ass, young_sp_ass, disko_sp_ass, nuup_sp_ass, por_sp_ass); gc()

# Summary analyses
# Need to think about how these analyses should proceed
# One idea is to extract or focus on specific species
# Rather converting them to count data in order to apply the same analyses as for the other variables
summary_sp_ass <- clean_sp_ass %>% 
  filter(value != 0) %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_group, site, type) %>% 
  summarise(value = as.numeric(n()), .groups = "drop") %>% 
  mutate(var_name = "sps_count") %>% 
  review_summary()

# Plot results
review_summary_plot(summary_sp_ass, "sp_ass")


## Tourism ----------------------------------------------------------------

# Test check for all soc vars to make sure no desired vars are missed
as.vector(distinct(filter(full_product_por, var_type == "soc"), var_name))
as.vector(distinct(filter(nuup_GEM, var_type == "soc"), var_name))

# Get tourism variables
kong_tourism <- review_filter_var(full_product_kong, "kong", "Tourist")
is_tourism <- review_filter_var(full_product_is, "is", "calls|Days in port|passengers") %>% 
  bind_rows(review_filter_var(full_product_sval, "is", "Longyearbyen|arrival|guest night"))
stor_tourism <- review_filter_var(full_product_stor, "stor", "touris") # No tourism data
young_tourism <- review_filter_var(rbind(full_product_young, young_GEM), "young", "tour") # No tourism data
disko_tourism <- review_filter_var(rbind(full_product_disko, disko_GEM), "disko", "tour") # No tourism data
nuup_tourism <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "nuup", "tour") # No tourism data
por_tourism <- review_filter_var(full_product_por, "por", "tour") # No tourism data
clean_tourism <- rbind(kong_tourism, is_tourism, stor_tourism, young_tourism, disko_tourism, nuup_tourism, por_tourism) %>% 
  mutate(var_group = "Tourism")
rm(kong_tourism, is_tourism, stor_tourism, young_tourism, disko_tourism, nuup_tourism, por_tourism); gc()

# Summary analyses
summary_tourism <- review_summary(clean_tourism)

# Plot results
review_summary_plot(summary_tourism, "tourism")


## Shipping ---------------------------------------------------------------

# Test check for all soc vars to make sure no desired vars are missed
as.vector(distinct(filter(full_product_is, var_type == "soc"), var_name))
as.vector(distinct(filter(nuup_GEM, var_type == "soc"), var_name))

# Get shipping variables
kong_shipping <- review_filter_var(full_product_kong, "kong", "Vessels")
is_shipping <- review_filter_var(full_product_is, "is", "trips|gross|berths|nautical|duration|fuel|power|emissions|tonnage") %>% 
  bind_rows(review_filter_var(full_product_sval, "is", "Isfjorden"))
stor_shipping <- review_filter_var(full_product_stor, "stor", "touris") %>%  # No shipping data
  bind_rows(review_filter_var(full_product_sval, "stor", "Storfjorden"))
young_shipping <- review_filter_var(rbind(full_product_young, young_GEM), "young", "berths") # No shipping data
disko_shipping <- review_filter_var(rbind(full_product_disko, disko_GEM), "disko", "berths") # No shipping data
nuup_shipping <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "nuup", "berths") # No shipping data
por_shipping <- review_filter_var(full_product_por, "por", "berths") # No shipping data
clean_shipping <- rbind(kong_shipping, is_shipping, stor_shipping, young_shipping, disko_shipping, nuup_shipping, por_shipping) %>% 
  mutate(var_type = "soc", var_group = "Shipping")
rm(kong_shipping, is_shipping, stor_shipping, young_shipping, disko_shipping, nuup_shipping, por_shipping); gc()

# Summary analyses
summary_shipping <- review_summary(clean_shipping)

# Plot results
review_summary_plot(summary_shipping, "shipping")


## Landings ----------------------------------------------------------------

# Currently no landings data
# Need to find this from government statistic/fisheries sites


## Save clean data ---------------------------------------------------------

clean_all <- rbind(clean_SST, clean_air, clean_sal, clean_PAR, 
                   clean_sea_ice, clean_snow, clean_glacier, clean_river,
                   clean_O2, clean_pCO2, clean_nutrients, #clean_pH, # pH shouldn't be necessary if we have pCO2
                   clean_chla, clean_biomass, clean_sp_ass,
                   clean_tourism, clean_shipping)
save(clean_all, file = "data/analyses/clean_all.RData")
plyr::l_ply(unique(clean_all$var_type), save_category, .parallel = T,
            df = clean_all, data_type = "clean", site_name = "all")


## References --------------------------------------------------------------

# NB: Check for N-ICE and remove if present
all_ref <- bind_rows(summary_ice$citations, summary_snow$citations, summary_glacier$citations, summary_river$citations,
                     summary_SST$citations, summary_air$citations, summary_sal$citations, summary_PAR$citations,
                     summary_O2$citations, summary_pCO2$citations, summary_nutrients$citations, 
                     summary_chla$citations, summary_biomass$citations, summary_sp_ass$citations, 
                     summary_tourism$citations, summary_shipping$citations)
save(all_ref, file = "data/analyses/all_ref.RData")


## Summary -----------------------------------------------------------------

# Combine analysed data
all_meta <- rbind(summary_ice$monthly, summary_snow$monthly, summary_glacier$monthly, summary_river$monthly,
                  summary_SST$monthly, summary_air$monthly, summary_sal$monthly, summary_PAR$monthly,
                  summary_O2$monthly, summary_pCO2$monthly, summary_nutrients$monthly, 
                  summary_chla$monthly, summary_biomass$monthly, summary_sp_ass$monthly, 
                  summary_tourism$monthly, summary_shipping$monthly)
save(all_meta, file = "data/analyses/all_meta.RData")

all_meta %>% 
  filter(!is.na(value_mean)) %>% 
  mutate(month = lubridate::month(date)) %>% 
  ggplot(aes(x = as.factor(month), y = count_days_name)) +
  geom_boxplot(aes(fill = site), position = "dodge", outlier.colour = NA) +
  geom_jitter(aes(colour = log10(count))) +
  scale_colour_viridis_c() + scale_y_continuous(limits = c(0, 32), expand = c(0, 0)) +
  labs(y = paste0("Unique days with data points"), x = "Month", fill = "Site", colour = "Count [log10(n)]") +
  facet_grid(site~var_group) +
  theme(panel.border = element_rect(colour = "black", fill = NA))
ggsave("~/Desktop/analyses_output/meta_meta_box.png", width = 16, height = 12)


# Section 3 ---------------------------------------------------------------
# Relationships between data analysed for Section 2
# NB: Only necessary to run the `Setup` section

## Provide literature reviews on the relationships they have with each other as well as data analyses of these relationships
# What is changing in the EU Arctic (setting the scene):
#   "Boundary conditions"
# e.g. Warming ocean, loss of sea ice, etc.
# We then go through the key drivers and ask for each driver whether there are general results about Arctic fjords across all sites
# Differences in some sites could be used as exceptions that prove the rule

## Potential specific comparisons
# Cloudiness and heat flux
# Nutrients and biomass
# Ice cover, freshwater input, and light
# Temperature and sea ice

## Generally speaking, the number of variables needs to be reduced/combined
# For some reason sea temperature is not being correlated with anything

## "In general, heatwaves favoured crawling or burrowing predators and suspension feeders, while the abundance of detritivores decreased, suggesting a climate-induced change in dominant zoobenthic traits (Pansch et al., 2018)."

# Load all clean data
clean_all <- map_dfr(dir("data/full_data", pattern = "clean", full.names = T), read_csv)

# Combine some variables for better correlations
clean_all_sp_count <- clean_all %>% 
  filter(var_group == "Species", value != 0) %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_group, site, type) %>% 
  summarise(value = as.numeric(n()), .groups = "drop") %>% 
  mutate(var_name = "sps_count")
clean_all_clean <- clean_all %>% 
  filter(var_group != "Species") %>% 
  rbind(clean_all_sp_count)
rm(clean_all, clean_all_sp_count); gc()

# Create monthly means
clean_all_monthly <- clean_all_clean %>% 
  filter(!is.na(date)) %>% 
  # filter(depth <= 10 | is.na(depth)) %>% 
  mutate(month_year = lubridate::floor_date(date, "month")) %>% 
  group_by(var_group, var_name, site, month_year) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# NB: Create monthly clims and run correlations on those

# Wide format for correlations
clean_all_wide <- clean_all_monthly %>% 
  pivot_wider(id_cols = c(var_group, site, month_year), names_from = var_name, values_from = value)

# Coorelations
clean_all_corr <- clean_all_wide %>% 
  dplyr::select(-var_group, -month_year) %>%
  group_nest(site) %>%
  mutate(cor = map(data, cor, use = "na.or.complete")) %>%
  dplyr::select(-data) %>%
  unnest(cor) #%>%
# mutate(site = as.numeric(as.factor(site))) %>% 
# group_by(site) %>% 
# summarise(cor = cor(.), .groups = "drop")
# janitor::remove_empty()
rownames_to_column(clean_all_corr, var = "rowname")
colnames(clean_all_corr) <- c("site", rownames(clean_all_corr))
# split(.$site) %>% 
# map(dplyr::select, -c(site)) %>% 
# map(cor) %>%
# list_extract()
# bind_rows(.id = NULL)
# group_by(site) %>% 
# summarise(cor = cor(.))
clean_all_corr_kong <- cor(dplyr::select(filter(clean_all_wide, site == "kong"), -var_group, -month_year, -site), use = "pairwise.complete.obs")
clean_all_corr_is <- cor(dplyr::select(filter(clean_all_wide, site == "is"), -var_group, -month_year, -site), use = "pairwise.complete.obs")
clean_all_corr_stor <- cor(dplyr::select(filter(clean_all_wide, site == "stor"), -var_group, -month_year, -site), use = "pairwise.complete.obs")
clean_all_corr_young <- cor(dplyr::select(filter(clean_all_wide, site == "young"), -var_group, -month_year, -site), use = "pairwise.complete.obs")
clean_all_corr_disko <- cor(dplyr::select(filter(clean_all_wide, site == "disko"), -var_group, -month_year, -site), use = "pairwise.complete.obs")
clean_all_corr_nuup <- cor(dplyr::select(filter(clean_all_wide, site == "nuup"), -var_group, -month_year, -site), use = "pairwise.complete.obs")
clean_all_corr_por <- cor(dplyr::select(filter(clean_all_wide, site == "por"), -var_group, -month_year, -site), use = "pairwise.complete.obs")

# Plot
cor_plot_kong <- ggcorrplot(clean_all_corr_kong, title = "Kongsfjorden") + theme(panel.background = element_rect(fill = NA, colour = "black"))
cor_plot_is <- ggcorrplot(clean_all_corr_is, title = "Isfjorden") + theme(panel.background = element_rect(fill = NA, colour = "black"))
cor_plot_stor <- ggcorrplot(clean_all_corr_stor, title = "Storfjorden") + theme(panel.background = element_rect(fill = NA, colour = "black"))
cor_plot_young <- ggcorrplot(clean_all_corr_young, title = "Young Sound") + theme(panel.background = element_rect(fill = NA, colour = "black"))
cor_plot_disko <- ggcorrplot(clean_all_corr_disko, title = "Disko bay") + theme(panel.background = element_rect(fill = NA, colour = "black"))
cor_plot_nuup <- ggcorrplot(clean_all_corr_nuup, title = "Nuup Kangerlua") + theme(panel.background = element_rect(fill = NA, colour = "black"))
cor_plot_por <- ggcorrplot(clean_all_corr_por, title = "Porsangerfjorden") + theme(panel.background = element_rect(fill = NA, colour = "black"))

# Arrange and save
cor_plot_all <- ggpubr::ggarrange(cor_plot_kong, cor_plot_is, cor_plot_stor, cor_plot_young, cor_plot_disko, cor_plot_nuup, cor_plot_por) + theme_bw()
ggsave("~/Desktop/analyses_output/cor_plot_all.png", height = 20, width = 25)

# Get count of cor sum per column
test1 <- clean_all_corr %>%
  data.frame() %>% 
  replace(is.na(.), 0) %>% 
  summarise(across(everything(), ~ sum(.)))


# Section 4 ---------------------------------------------------------------
# Future projections of data analysed for Section 2 and relationships from Section 3
# NB: Only necessary to run the `Setup` section

## Introduce the future by talking about the whole Arctic
# These data can be downloaded from the IPCC interactive website
# https://interactive-atlas.ipcc.ch
# Based on the relationships elucidated in the previous section we can then look at any possibly useful projections into the future with the model data

## "It has also been suggested that the various drivers of climate change may contribute to increase blooms and toxicity of cyanobacteria in the Baltic Sea. 
## For instance, the intracellular toxin concentration of the cyanobacterium Dolichospermum sp. may increase with elevated temperature (+4∘C) (Brutemark et al., 2015; Wulff et al., 2018) and with decreased salinity (from 6 to 3) (Wulff et al., 2018)."
## "an increase in temperature from 16 to 18–20∘C led to an earlier peak of cyanobacteria, while the biomass of cyanobacteria, especially that of nitrogen-fixer Dolichospermum sp. declined (Berner et al., 2018)."
## "To sum up, a shift towards smaller-sized zooplankton and a stronger linkage between mesozooplankton and the microbial food web is probable in a warmer Baltic Sea."
## "It has been projected that macroalgae will decline in hard bottoms and vascular plants increase in the more sheltered soft-bottom areas (Torn et al., 2020)."
## "Climate change will most probably mean milder winters, and if soils remain thawed, more nutrients will leak from the terrestrial areas into the freshwater system."
## "Several recent studies have however pointed out, for example, that macroalgae (Rothäusler et al., 2018; Rugiu et al., 2018a) and zooplankton (Karlsson and Winder, 2020) have phenotypic plasticity and potential for adaptation against gradual changes in the abiotic environment."

# Morten model data
model_kong <- load_model("kongsfjorden_rcp")
model_is <- load_model("isfjorden_rcp")
model_stor <- load_model("storfjorden_rcp")
model_young <- load_model("young_sound_rcp")
model_por <- load_model("porsangerfjorden_rcp")
model_trom <- load_model("tromso_rcp")

# Convert data to even grid
model_kong_even_grid <- model_kong %>% 
  filter(date == "2020-01-31", land == 1) %>% 
  dplyr::select(lon, lat, date, Temp) %>% 
  distinct() %>%  
  convert_even_grid(., "Temp", 0.1) %>% 
  na.omit()

# Plot raster
coords <- bbox_kong
coastline_full_df_sub <- coastline_full_df %>% 
  filter(x >= coords[1]-10, x <= coords[2]+10,
         y >= coords[3]-10, y <= coords[4]+10)
ggplot(model_kong_even_grid, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = z)) +
  geom_point(data = filter(model_kong, date == "2020-01-31", land == 1), size = 4, colour = "black") +
  geom_point(data = filter(model_kong, date == "2020-01-31", land == 1), size = 3, aes(colour = Temp)) +
  geom_polygon(data = coastline_full_df_sub, fill = "grey70", colour = "black",
               aes(x = x, y = y, group = polygon_id)) + 
  scale_colour_continuous(limits = range(model_kong_even_grid$z)) +
  coord_quickmap(expand = T,
                 xlim = c(coords[1]-1.5, coords[2]+1.5), 
                 ylim = c(coords[3]-0.4, coords[4]+0.4))


# Figure 1 ----------------------------------------------------------------
# Map of the study area that also manages to show SST, ice cover, and any other well covered drivers 
# NB: Only necessary to run the `Setup` section

# EU bbox
bbox_EU_poly <- bbox_to_poly(bbox_EU, "EU")

# EU Arctic land shapes
coastline_Arctic <- filter(coastline_full_df, y > 50, x < 90, x > -90)

# Study sites
site_points <- data.frame(site = factor(x = c("Kongsfjorden", "Isfjorden", "Storfjorden", 
                                              "Young Sound", "Disko Bay", "Nuup Kangerlua", 
                                              "Porsangerfjorden"),
                                        levels = c("Kongsfjorden", "Isfjorden","Storfjorden", 
                                                   "Young Sound", "Disko Bay", "Nuup Kangerlua", 
                                                   "Porsangerfjorden")),
                          lon = c(11.845, 14.365, 19.88, -21.237, -52.555, -50.625, 25.75),
                          lat = c(78.98, 78.235, 77.78, 74.517, 69.36, 64.405, 70.6))

# Colour palette for sites
site_colours <- c(
  "Kongsfjorden" = "tan1", 
  "Isfjorden" = "sienna1", 
  "Storfjorden" = "orange1", 
  "Young Sound" = "darkseagreen1", 
  "Disko Bay" = "seagreen1", 
  "Nuup Kangerlua" = "palegreen1", 
  "Porsangerfjorden" = "plum1"
)

# EU SST trends
## NB: This file is very large, only load if necessary
# load("~/pCloudDrive/FACE-IT_data/EU_arctic/sst_EU_arctic.RData")
# sst_EU_arctic_annual <- sst_EU_arctic %>%
#   mutate(year = lubridate::year(t)) %>%
#   filter(year <= 2021) %>% 
#   group_by(lon, lat, year) %>%
#   summarise(temp_annual = mean(temp, na.rm = T), .groups = "drop")
# sst_EU_arctic_annual_trends <- plyr::ddply(dplyr::rename(sst_EU_arctic_annual, val = temp_annual),
#                                            c("lon", "lat"), trend_calc, .parallel = T)
# save(sst_EU_arctic_annual_trends, file = "data/analyses/sst_EU_arctic_annual_trends.RData")
load("data/analyses/sst_EU_arctic_annual_trends.RData")

# Per pixels ice cover trends
load("data/analyses/ice_4km_proc.RData")
ice_4km_annual_prop <- ice_4km_proc %>% 
  filter(sea_ice_extent == 3,
         date <= "2021-12-31") %>% 
  mutate(sea_ice_extent = 1) %>% # Convert to binary yes/no
  group_by(site, lon, lat) %>% 
  complete(date = seq.Date(as.Date("2006-01-01"), as.Date("2021-12-31"), by = "day")) %>%
  ungroup() %>% 
  replace(is.na(.), 0) %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(site, lon, lat, year) %>% 
  summarise(annual_ice_cover_days = sum(sea_ice_extent), .groups = "drop")

# Per pixel annual trends in ice cover days
# NB: This takes a while to run, better to just load the results
# ice_4km_annual_trends <- plyr::ddply(dplyr::rename(ice_4km_annual_prop, val = annual_ice_cover_days), 
#                                      c("site", "lon", "lat"), trend_calc, .parallel = T)
# save(ice_4km_annual_trends, file = "data/analyses/ice_4km_annual_trends.RData")
load("data/analyses/ice_4km_annual_trends.RData")

# Remove one suspect pixel in Disko Bay
ice_4km_annual_trends <- filter(ice_4km_annual_trends, trend < 10)

# Convert ice data to an even grid for plotting
# Not used
# ice_4km_annual_trends_grid <- plyr::ddply(ice_4km_annual_trends, c("site"), convert_even_grid, z_col = "trend", pixel_res = 0.04)

# Top panel: polar projection of SST trends
fig_1_a <- basemap(limits = c(-60, 60, 60, 90), bathymetry = F) +
  geom_spatial_tile(data = filter(sst_EU_arctic_annual_trends, lat >= 60),
                    crs = 4326, colour = NA,
                    aes(fill = trend*10, x = lon, y = lat)) +
  geom_spatial_point(data = site_points, size = 5, crs = 4326,
                     aes(x = lon, y = lat), colour = "black") +
  geom_spatial_point(data = site_points, size = 4, crs = 4326,
                     aes(x = lon, y = lat, colour = site)) +
  annotation_spatial(bbox_EU_poly, fill = NA, colour = "black", alpha = 0.1) +
  scale_colour_manual(values = site_colours, guide = "none") +
  scale_fill_gradient2(low = scales::muted("blue"), high = scales::muted("red"), guide = "none") +
  labs(colour = "Site", fill = "Trend (°C/dec)",
       # title = "Region and study sites",
       x  = NULL, y = NULL) +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "white", colour = "white"),
        # legend.position = c(0.948, 0.29),
        legend.position = "bottom",
        axis.text = element_text(colour = "black"),
        # legend.margin = margin(10, 10, 10, 10),
        legend.box.margin = margin(10, 10, 10, 10),
        legend.box.background = element_rect(fill = "white", colour = "black"))
fig_1_a$layers <- fig_1_a$layers[c(2,1,3,4,5)] # Reorder land shape and SST rasters
ggsave("~/Desktop/anlyses_output/fig_1_a.png", fig_1_a, width = 10, height = 7)

# Side panels: ice cover trends by site (days of year of ice cover)
(fig_1_b_kong <- ice_trend_grid_plot("Kongsfjorden", 0.03, check_conv = F) + 
    coord_quickmap(xlim = c(11.1, 12.6), ylim = c(78.89, 79.09), expand = F))
(fig_1_b_is <- ice_trend_grid_plot("Isfjorden", 0.04, check_conv = F))
(fig_1_b_stor <- ice_trend_grid_plot("Storfjorden", 0.04, check_conv = F))
(fig_1_b_young <- ice_trend_grid_plot("Young Sound", 0.04, check_conv = F, lat_nudge = 0.025))
(fig_1_b_disko <- ice_trend_grid_plot("Disko Bay", 0.05, check_conv = F))
(fig_1_b_nuup <- ice_trend_grid_plot("Nuup Kangerlua", 0.05, check_conv = F) + 
    coord_quickmap(xlim = c(-53.0, -49.6), ylim = c(64.01, 64.80), expand = F))
(fig_1_b_por <- ice_trend_grid_plot("Porsangerfjorden", 0.05, check_conv = F) + 
    coord_quickmap(xlim = c(24.83, 26.63), ylim = c(70.02, 71.03), expand = F))

# Get legends
temp_legend <- filter(sst_EU_arctic_annual_trends, lat >= 60) %>% 
  dplyr::select(trend) %>% distinct() %>% 
  mutate(x = 1:n(), y = 1) %>% 
  ggplot() + geom_point(aes(x = x, y = y, colour = trend*10)) +
  scale_colour_gradient2(low = scales::muted("blue"), high = scales::muted("red")) +
  labs(colour = "SST trend\n(°C/dec)") +
  theme(legend.position = "bottom", legend.key.width = unit(3, "cm"),
        legend.box.background = element_rect(fill = NA, colour = "black"))
temp_legend <- ggpubr::get_legend(temp_legend)
ice_legend <- ice_4km_annual_trends %>% 
  dplyr::select(trend) %>% distinct() %>% 
  mutate(x = 1:n(), y = 1) %>% 
  ggplot() + geom_point(aes(x = x, y = y, colour = trend)) +
  scale_fill_gradient2(low = "darkolivegreen", mid = "white", high = "deepskyblue", aesthetics = c("colour", "fill"),
                       limits = c(min(ice_4km_annual_trends$trend), max(ice_4km_annual_trends$trend))) +
  labs(colour = "Ice cover\n(days/year)") +
  theme(legend.position = "bottom", legend.key.width = unit(3, "cm"),
        legend.box.background = element_rect(fill = NA, colour = "black"))
ice_legend <- ggpubr::get_legend(ice_legend)

# Create base for layering plots
base_df <- data.frame(x = c(-1, 0, 1), y = c(-1, 0, 1))
fig_1_base <- ggplot(data = base_df, aes(x = x, y = y)) + 
  geom_point(colour = "white") + 
  scale_x_continuous(breaks = seq(-1, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1)) +
  theme_void()

# Combine
fig_1 <- fig_1_base +
  # EU Arctic
  geom_grob(aes(x = 0, y = 0, label = list(cowplot::as_grob(fig_1_a))), vp.width = 0.7, vp.height = 0.7, 
            nudge_x = -0.033, add.segments = F) +
  # Kongsfjorden
  geom_grob(aes(x = 0.065, y = 0.16, label = list(cowplot::as_grob(fig_1_b_kong))),
            nudge_x = -0.07, nudge_y = 0.55, segment.colour = "tan1") +
  # Isfjorden
  geom_grob(aes(x = 0.08, y = 0.15, label = list(cowplot::as_grob(fig_1_b_is))),
            nudge_x = 0.35, nudge_y = 0.6, vp.width = 0.25, vp.height = 0.25, segment.colour = "sienna1") +
  # Storfjorden
  geom_grob(aes(x = 0.12, y = 0.15, label = list(cowplot::as_grob(fig_1_b_stor))),
            nudge_x = 0.65, nudge_y = 0.03, vp.width = 0.25, vp.height = 0.25, segment.colour = "orange1") +
  # Young Sound
  geom_grob(aes(x = -0.15, y = 0.05, label = list(cowplot::as_grob(fig_1_b_young))),
            nudge_x = -0.41, nudge_y = 0.53, vp.width = 0.20, vp.height = 0.20, segment.colour = "darkseagreen1") +
  # Disko bay
  geom_grob(aes(x = -0.45, y = 0.11, label = list(cowplot::as_grob(fig_1_b_disko))),
            nudge_x = -0.41, nudge_y = -0.03, vp.width = 0.25, vp.height = 0.25, segment.colour = "seagreen1") +
  # Nuup Kangerlua
  geom_grob(aes(x = -0.55, y = -0.03, label = list(cowplot::as_grob(fig_1_b_nuup))),
            nudge_x = -0.26, nudge_y = -0.4, vp.width = 0.25, vp.height = 0.25, segment.colour = "palegreen1") +
  # Porsangerfjorden
  geom_grob(aes(x = 0.23, y = -0.05, label = list(cowplot::as_grob(fig_1_b_por))),
            nudge_x = 0.5, nudge_y = -0.46, vp.width = 0.3, vp.height = 0.3, segment.colour = "plum1") +
  # Temperature legend
  geom_grob(aes(x = -0.1, y = -0.74, label = list(cowplot::as_grob(temp_legend)))) +
  # Ice legend
  geom_grob(aes(x = 0.0, y = -0.94, label = list(cowplot::as_grob(ice_legend))))
# fig_1
ggsave("figures/fig_1.png", fig_1, width = 12, height = 10)


# Figure 2 ----------------------------------------------------------------

# Metadata figure showing the coverage of the key drivers
# This should concisely show how many datasets/points are available for each key drivers and site
# But one should be cautious about focussing too much on the sites
load("data/analyses/all_meta.RData")

# Filter out remote products
all_meta_insitu <- filter(all_meta, type == "in situ") %>% 
  mutate(month = lubridate::month(date), 
         site = factor(site, levels = c("kong", "is", "stor", "young", "disko", "nuup", "por"),
                       labels = c("Kongsfjorden", "Isfjorden", "Storfjorden", 
                                  "Young Sound", "Disko Bay", "Nuup Kangelrua",
                                  "Porsangerfjorden")))

# Create bar plot for each category
fig_2_plot <- function(var_filter, var_title){
  
  # Calculate mean stats
  df_mean <- filter(all_meta_insitu, var_type == var_filter) %>% 
    mutate(month = as.factor(month),
           var_group = as.factor(var_group)) %>% 
    group_by(site, month, var_group) %>% 
    summarise(mean_count_days_group = mean(count_days_group, na.rm = T),
              sd_count_days_group = sd(count_days_group, na.rm = T), .groups = "drop")
  
  # get all levels
  df_full <- expand(df_mean, site, month, var_group) %>% 
    left_join(df_mean, by = c("site", "month", "var_group")) %>% 
    replace(is.na(.), 0)
  
  # Plot
  fig_2_panel <- ggplot(data = df_full, aes(x = as.factor(month), y =  mean_count_days_group)) +
    # geom_boxplot(aes(x = as.factor(month), y = mean_count_days_group, fill = var_group), outlier.colour = NA) +
    # geom_point() +
    geom_col(position = "dodge", colour = "black", aes(fill = var_group)) +
    # geom_errorbar(position = dodge, 
    #               aes(ymin = mean_count_days_group-sd_count_days_group, ymax = mean_count_days_group+sd_count_days_group)) +
    # scale_fill_manual(values = driver_cat_colours, aesthetics = "fill") +
    facet_wrap(~site, nrow = 1) + #guides(fill = "none") +
    scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) +
    labs(x = "Month", y = "Unique days of measurement", 
         title = var_title, fill = "Driver group")
  fig_2_panel
}
fig_2_a <- fig_2_plot("cryo", "Cryosphere")
fig_2_b <- fig_2_plot("phys", "Physical")
fig_2_c <- fig_2_plot("chem", "Chemistry")
fig_2_d <- fig_2_plot("bio", "Eco/biology")
fig_2_e <- fig_2_plot("soc", "Social")

# Combine
fig_2 <- ggpubr::ggarrange(fig_2_a, fig_2_b, fig_2_c, fig_2_d, fig_2_e, ncol = 1, labels = c("A)", "B)", "C)", "D)", "E)"))
ggsave("figures/fig_2.png", width = 15, height = 15)


# Table 1 -----------------------------------------------------------------

# Create table of sources for each category/group
load("data/analyses/all_ref.RData") # NB: Must add data sources to this output

# Pivot wider to get var_type
all_ref_var_type <- all_ref %>% 
  distinct() %>% 
  mutate(var_type_count = 1) %>% 
  pivot_wider(id_cols = c(type, URL, citation), names_from = var_type, values_from = var_type_count, values_fn = mean)

# Combine
all_ref_wide <- all_ref %>% 
  left_join(all_ref_var_type, by = c("type", "URL", "citation")) %>% 
  dplyr::select(-var_type) %>% distinct() %>% 
  filter(type == "in situ") # Remove non in situ sources

# Get summaries by site
table_1_func <- function(site_name, site_long){
  # site_col <- colnames(all_ref_wide)[which(colnames(all_ref_wide) == site_name)]
  # df_sub <- all_ref_wide
  site_name <- enquo(site_name)
  df_ref <- all_ref_wide %>% 
    filter(!is.na(!!site_name)) %>% 
    dplyr::select(-type, -URL, -citation) %>% 
    summarise_all(sum, na.rm = T) %>% 
    mutate(Site = site_long) %>% 
    dplyr::select(Site, !!site_name, cryo:soc, PANGAEA, NPDC, GEM, NMDC, NSIDC:`Port of Longyearbyen`)
  colnames(df_ref)[2] <- "Total"
  return(df_ref)
}

# Final table
table_1_kong <- table_1_func(kong, "Kongsfjorden")
table_1_is <- table_1_func(is, "Isfjorden")
table_1_stor <- table_1_func(stor, "Storfjorden")
table_1_young <- table_1_func(young, "Young Sound")
table_1_disko <- table_1_func(disko, "Disko Bay")
table_1_nuup <- table_1_func(nuup, "Nuup Kangerlua")
table_1_por <- table_1_func(por, "Porsangerfjorden")
table_1 <- rbind(table_1_kong, table_1_is, table_1_stor, table_1_young, table_1_disko, table_1_nuup, table_1_por) %>% 
  mutate(Other = Reduce("+",.[12:23])) %>% 
  dplyr::select(Site:NMDC, Other)
write_csv(table_1, "data/analyses/table_1.csv")
knitr::kable(table_1)
table_1_plot <- ggplot() +
  annotate(geom = "table", x = 0, y = 0, label = list(table_1)) +
  theme_void()
ggsave("figures/table_1.png", table_1_plot, width = 6.2, height = 1.55)

# Some acronyms:
# "NSIDC" = National Snow & Ice Data Center
# "NMDC" = Norwegian Marine Data Centre
# "NMI" = Norwegian Meteorological Institute
# "NIRD" = National Infrastructure for Research Data


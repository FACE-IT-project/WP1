# code/review_paper.R
# The code used for the analyses in the WP1 review paper (D1.3)

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

# Ice cover colours
ice_cover_colours <- c(
  "ocean" = "navy",
  "land" = "slategrey",
  "sea ice" = "mintcream",
  "coast" = "dodgerblue",
  "exclude" = "gold"
)


# Data --------------------------------------------------------------------

# FACE-IT collected data
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

# Model data
model_kong <- load_model("kongsfjorden_rcp")
model_is <- load_model("isfjorden_rcp")
model_stor <- load_model("storfjorden_rcp")
model_young <- load_model("young_sound_rcp")
model_por <- load_model("porsangerfjorden_rcp")
model_trom <- load_model("tromso_rcp")

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
## NB: Load one-by-one, not all in one go, they are very large files
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
ggplot(distinct(sst_young_bbox[c("lon", "lat")]), aes(x = lon, y = lat)) +
  geom_tile(colour = "red") +
  # geom_raster(distinct(sst_young_bbox[c("lon", "lat")]), aes(x = lon, y = lat)) +
  geom_rect(aes(xmin = bbox_young[1], xmax = bbox_young[2], 
                ymin = bbox_young[3], ymax = bbox_young[4]))

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

# Kongsfjorden
kong_OISST <- sst_kong_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
kong_CCI <- sst_CCI_kong_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
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
kong_SST <- review_filter_var(full_product_kong, "kong", "temp|°C",
                              "air|co2|ph_|pHint_|TTT|MAAT|MAGT|MAT|
                              |SST sum|SST win|Temp min|Temp max|Temp interp|
                              |tequ|tpot|T intern") %>%
  bind_rows(kong_OISST, kong_CCI) %>% mutate(site = "kong")
# review_filter_check(kong_SST, "MAT [°C]", "Gattuso")

# Isfjorden
is_OISST <- sst_is_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
is_CCI <- sst_CCI_is_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
# t [°C] = ground/snow temperatures e.g. https://doi.pangaea.de/10.1594/PANGAEA.930472
# T tech [°C] + T cal [°C] = Temperatures from an experiment e.g. https://doi.pangaea.de/10.1594/PANGAEA.847626
is_SST <- review_filter_var(full_product_is, "is", "temp|°C", 
                            "SST sum|SST win|TTT|MAT|MAGT|MAAT|Tpot|Tequ|air|T intern|T tech|T cal|pHT|
                            |T sum|T win|SST anomaly|theta", c("t [°C]", "SST (1-12) [°C]")) %>% # Can re-add if annual values
  bind_rows(is_OISST, is_CCI) %>% mutate(site = "is")
# review_filter_check(is_SST, "SST (1-12) [°C]")

# Storfjorden
stor_OISST <- sst_stor_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
stor_CCI <- sst_CCI_stor_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
stor_SST <- review_filter_var(full_product_stor, "stor", "temp|°C", "Tpot|Tequ|theta|fco2|Tmax|TTT|
                              |SST anomaly", c("t [°C]", "SST (1-12) [°C]")) %>% 
  bind_rows(stor_OISST, stor_CCI) %>% mutate(site = "stor")
# review_filter_check(stor_SST, "t [°C]")

# Young Sound
young_OISST <- sst_young_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
young_CCI <- sst_CCI_young_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
young_SST <- review_filter_var(rbind(full_product_young, young_GEM), "young", "temp|°C", 
                               "Tpot|Tequ|theta|fco2|pot_temp|SST sum|SST win|MAGT|MAAT") %>% 
  bind_rows(young_OISST, young_CCI) %>% mutate(site = "young")
# review_filter_check(young_SST, "pot_temp [°C]")

# Disko Bay
disko_OISST <- sst_disko_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
disko_CCI <- sst_CCI_disko_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
disko_SST <- review_filter_var(rbind(full_product_disko, disko_GEM), "disko", "temp|°C", 
                               "Tequ|potential|theta|fco2|SST sum|SST win|TTT|SST anomaly", "SST (1-12) [°C]") %>% 
  bind_rows(disko_OISST, disko_CCI) %>% mutate(site = "disko")
# review_filter_check(disko_SST)

# Nuup Kangerlua
nuup_OISST <- sst_nuup_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
nuup_CCI <- sst_CCI_nuup_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
nuup_SST <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "nuup", "temp|°C", 
                              "Tequ|T tech|Tpot|SST sum|SST win|TTT") %>% 
  bind_rows(nuup_OISST, nuup_CCI) %>% mutate(site = "nuup")
# review_filter_check(nuup_SST)

# Porsangerfjorden
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
                              TRUE ~ citation)) %>% 
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
                              "co2|intern|tequ|f_|p_|par_|temp_|interp|_ctd|_sf|MAGT|MAT|MAAT|T air", # We want T air but it requires processing before this step
                              var_precise = "Temp [°C]", atmos = T) %>% 
  filter(URL != "https://doi.org/10.1594/PANGAEA.882432", URL != "https://doi.org/10.1594/PANGAEA.839802")
is_air <- add_depth(review_filter_var(full_product_is, "is", "air|temp|°C", "tequ|intern|bulb|pressure|MAGT|MAAT|T air",  
                                      var_precise = c("Temp [°C]", "t [°C]"), atmos = T))
stor_air <- add_depth(review_filter_var(full_product_stor, "stor", "air|temp|°C", "Tmax",
                                        var_precise = c("Temp [°C]", "t [°C]"), atmos = T))
young_air <- add_depth(review_filter_var(full_product_young, "young", "air|temp|°C", var_precise = "Temp [°C]", atmos = T))
disko_air <- add_depth(review_filter_var(full_product_disko, "disko", "air|temp|°C", "tequ", var_precise = "Temp [°C]", atmos = T))
nuup_air <- add_depth(review_filter_var(full_product_nuup, "nuup", "air|temp|°C", "tequ", var_precise = "Temp [°C]", atmos = T))
por_air <- add_depth(review_filter_var(full_product_por, "por", "air|temp|°C", "tequ|bulb|MAAT|T air", 
                                       var_precise = "Temp [°C]", atmos = T)) %>% filter(!var_name == "temp [°C]")

# Combined cleaned data
clean_air <- rbind(kong_air, is_air, stor_air, young_air, disko_air, nuup_air, por_air)
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
kong_sal <- review_filter_var(full_product_kong, "kong", "sal|PSU|s_", "interp|ph",
                              cit_filter = "land station|drainage|meltwater")
is_sal <- review_filter_var(full_product_is, "is", "sal|PSU", "interp|mg/l")
stor_sal <- review_filter_var(full_product_stor, "stor", "sal|PSU", "interp")
young_sal <- review_filter_var(rbind(full_product_young, young_GEM), "young", "sal|PSU", "sal interp")
disko_sal <- review_filter_var(rbind(full_product_disko, disko_GEM), "disko", "sal|PSU", "sal interp")
nuup_sal <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "nuup", "sal|PSU", "sal interp")
por_sal <- review_filter_var(full_product_por, "por", "sal|PSU", "Sal interp")
clean_sal <- rbind(kong_sal, is_sal, stor_sal, young_sal, disko_sal, nuup_sal, por_sal) %>% mutate(var_name = "Sal")
rm(kong_sal, is_sal, stor_sal, young_sal, disko_sal, nuup_sal, por_sal); gc()

# Summary analyses
summary_sal <- review_summary(filter(clean_sal, depth >= 0, depth <= 10))

# Plot results
review_summary_plot(summary_sal, "sal")


## Light ------------------------------------------------------------------

# Get all PAR data
kong_PAR <- review_filter_var(full_product_kong, "kong", "PAR", "Onc|Gym|Para|below")
is_PAR <- review_filter_var(full_product_is, "is", "PAR") # No PAR data
stor_PAR <- review_filter_var(full_product_stor, "stor", "PAR") # No PAR data
young_PAR <- review_filter_var(rbind(full_product_young, young_GEM), "young", "PAR")
## NB: It is unclear if these values should be divided by 10 or not
## It is also unclear what the time dimension is for the data
disko_PAR <- review_filter_var(rbind(full_product_disko, disko_GEM), "disko", "PAR", "milli") %>% filter(value > 0)
## NB: Some of these values are also very high
nuup_PAR <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "nuup", "PAR")
por_PAR <- review_filter_var(full_product_por, "por", "PAR") # No PAR data
clean_PAR <- rbind(kong_PAR, is_PAR, stor_PAR, young_PAR, disko_PAR, nuup_PAR, por_PAR) %>% mutate(var_name = "PAR [µmol m-2 s-1]")
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
  mutate(type = "in situ")
rm(kong_sea_ice, is_sea_ice, stor_sea_ice, young_sea_ice, disko_sea_ice, nuup_sea_ice, por_sea_ice); gc()

# Figures
## Need custom figures per site
## Consistent metadata files may not be useful across sites
# filter(all_sea_ice, !var_name %in% c("Open water [start date]", "Open water [end date]"))
ggplot(clean_sea_ice, aes(x = date, y = value, colour = site)) +
  geom_point() + geom_line() + 
  facet_wrap(~var_name, scales = "free_y")
ggsave("~/Desktop/ice_var_ts.png", width = 20, height = 16)

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
ice_4km_prop <- plyr::ddply(rbind(ice_4km_kong_proc, ice_4km_is_proc, ice_4km_stor_proc, ice_4km_young_proc,
                                  ice_4km_disko_proc, ice_4km_nuup_proc, ice_4km_por_proc), 
                            c("site"), ice_cover_prop, .parallel = T)
rm(ice_4km_kong, ice_4km_is, ice_4km_stor, ice_4km_young, ice_4km_disko, ice_4km_nuup, ice_4km_por,
   ice_4km_kong_proc, ice_4km_is_proc, ice_4km_stor_proc, ice_4km_young_proc,
   ice_4km_disko_proc, ice_4km_nuup_proc, ice_4km_por_proc); gc()

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
ggsave("~/Desktop/ice_prop_ts.png", height = 12, width = 20)
ggplot(ice_4km_prop, aes(x = as.factor(month), y = mean_prop, fill = site)) +
  geom_boxplot() + facet_wrap(~month, scales = "free_x") +
  labs(x = "Month", y = "Sea ice cover [proportion]", colour = "Site")
ggsave("~/Desktop/ice_prop_box_month.png", height = 6, width = 12)
ggplot(ice_4km_prop, aes(x = as.factor(month), y = mean_prop, fill = site)) +
  geom_boxplot() + facet_wrap(~site, scales = "free_x") +
  labs(x = "Month", y = "Sea ice cover [proportion]", colour = "Site")
ggsave("~/Desktop/ice_prop_box_site.png", height = 9, width = 12)
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
                                       "Snow density, unc [kg/m**3]", "SWE [m]", "SWE unc [m]", "Snow thick [m]")) %>% mutate(site = "stor") # 
young_snow <- filter(rbind(full_product_young, young_GEM), var_type == "cryo",
                     var_name %in% c("Snow h [m]", "Density snow [kg/m**3]", "Sea ice snow thickness [cm]")) %>% 
  mutate(site = "young") # A lot of geospatial glacier data... consider removing or averaging
disko_snow <- filter(rbind(full_product_disko, disko_GEM), var_type == "cryo") %>% mutate(site = "disko") %>% slice(0) # No snow data
nuup_snow <- filter(rbind(full_product_nuup, nuup_GEM), var_type == "cryo") %>% mutate(site = "nuup") %>% slice(0) # No snow data
por_snow <- filter(full_product_por, var_type == "cryo") %>% mutate(site = "por") %>% slice(0) # No snow data
clean_snow <- bind_rows(kong_snow, is_snow, stor_snow, young_snow, disko_snow, nuup_snow, por_snow) %>% mutate(type = "in situ")
rm(kong_snow, is_snow, stor_snow, young_snow, disko_snow, nuup_snow, por_snow); gc()

# Summary analyses
summary_snow <- review_summary(clean_snow)

# Plot results
review_summary_plot(summary_snow, "snow")


## Glacier -----------------------------------------------------------------

# Test check for all cryo vars to make sure no glacier vars are missed
as.vector(distinct(filter(full_product_por, var_type == "cryo"), var_name))
as.vector(distinct(filter(nuup_GEM, var_type == "cryo"), var_name))

# Get all glacier variables
kong_glacier <- review_filter_var(full_product_kong, "kong", "balance|glacier")
is_glacier <- review_filter_var(full_product_is, "is", "balance|glacier")
stor_glacier <- review_filter_var(full_product_stor, "stor", "balance|glacier")
young_glacier <- review_filter_var(rbind(full_product_young, young_GEM), "young", "balance|glacier|ablation")
disko_glacier <- review_filter_var(rbind(full_product_disko, disko_GEM), "disko", "balance|glacier|ablation")
nuup_glacier <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "nuup", "glac")
por_glacier <- review_filter_var(full_product_por, "por", "balance|glac") # No glacier data
clean_glacier <- rbind(kong_glacier, is_glacier, stor_glacier, young_glacier, disko_glacier, nuup_glacier, por_glacier)
rm(kong_glacier, is_glacier, stor_glacier, young_glacier, disko_glacier, nuup_glacier, por_glacier); gc()

# Summary analyses
summary_glacier <- review_summary(clean_glacier)

# Plot results
review_summary_plot(summary_glacier, "glacier")

# Grab glacier values directly from EU or Svalbard products for certainty
# Look for specific DOI in each site file


## Oxygen -----------------------------------------------------------------


## pCO2 --------------------------------------------------------------------

# NB: For this and other chemistry variables see best practices sent by JP on Slack

# Keep pCO2_calc as a separate variable because they can't be taken as absolutely the same
# Same foe PCO2water_SST_wet
# Can use SeaCarb to transform fco2 to pCO2

# Get all pCO2 data
# Note that there are duplicates from GLODAP and the underlying files downloaded via PANGAEA
# But this is actually a good thing as it allows us to acknowledge specific contributors,
# which is something that the GLODAP product requests that we do.
kong_pCO2 <- review_filter_var(full_product_kong, "kong", "CO2")
is_pCO2 <- review_filter_var(full_product_is, "is", "CO2", "emissions")
stor_pCO2 <- review_filter_var(full_product_stor, "stor", "CO2", "emissions|tco2|fco2")
young_pCO2 <- review_filter_var(rbind(full_product_young, young_GEM), "young", "CO2") # No pCO2 data
disko_pCO2 <- review_filter_var(rbind(full_product_disko, disko_GEM), "disko", "CO2", "fco2|tco2")
nuup_pCO2 <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "nuup", "CO2")
por_pCO2 <- review_filter_var(full_product_por, "por", "CO2")
clean_pCO2 <- rbind(kong_pCO2, is_pCO2, stor_pCO2, young_pCO2, disko_pCO2, nuup_pCO2, por_pCO2)
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
kong_nutrients <- review_filter_var(full_product_kong, "kong", "nitr|amon|phos|silic|NO3|NO2|NH4|PO4|SiO4", "stddev", 
                                    var_precise = c("[NO3]- + [NO2]- [µmol/l]", "PO4 biog [%]"))
is_nutrients <- review_filter_var(full_product_is, "is", "nitr|amon|phos|silic|NO3|NO2|NH4|PO4|SiO4", var_precise = "PO4 biog [%]")
stor_nutrients <- review_filter_var(full_product_stor, "stor", "nitr|amon|phos|silic|NO3|NO2|NH4|PO4|SiO4",
                                    var_precise = "NO3 pen depth [mm]")
young_nutrients <- review_filter_var(rbind(full_product_young, young_GEM), "young", "nitr|amon|phos|silic|NO3|NO2|NH4|PO4|SiO4", 
                                     "nitracline", "NO2_NO3 [µmol/l]")
disko_nutrients <- review_filter_var(rbind(full_product_disko, disko_GEM), "disko", "nitr|amon|phos|silic|NO3|NO2|NH4|PO4|SiO4",
                                     var_precise = "[NO3]- + [NO2]- [µmol/l]")
nuup_nutrients <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "nuup", "nitr|amon|phos|silic|NO3|NO2|NH4|PO4|SiO4",
                                    var_precise = "[NO3]- + [NO2]- [µmol/l]")
por_nutrients <- review_filter_var(full_product_por, "por", "nitr|amon|phos|silic|NO3|NO2|NH4|PO4|SiO4") # No nutrient data
clean_nutrients <- rbind(kong_nutrients, is_nutrients, stor_nutrients, young_nutrients, disko_nutrients, nuup_nutrients, por_nutrients) %>% 
  # Change GLODAP var_name to match PANGAEA standard 
  mutate(var_name = case_when(var_name == "nitrate [μmol kg-1]" ~ "NO3 [µmol/l]",   
                              var_name == "nitrite [μmol kg-1]" ~ "NO2 [µmol/l]",   
                              var_name == "silicate [μmol kg-1]" ~ "SiO4 [µmol/l]",
                              var_name == "phosphate [μmol kg-1]" ~ "PO4 [µmol/l]",
                              TRUE ~ var_name)); unique(clean_nutrients$var_name)
rm(kong_nutrients, is_nutrients, stor_nutrients, young_nutrients, disko_nutrients, nuup_nutrients, por_nutrients); gc()

# Summary analyses
summary_nutrients <- review_summary(filter(clean_nutrients, depth >= 0, depth <= 10))

# Plot results
review_summary_plot(summary_nutrients, "nutrients")


## ChlA --------------------------------------------------------------------

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
nuup_chla <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "nuup", "chl") # No ChlA data
por_chla <- review_filter_var(full_product_por, "por", "chl") # No ChlA data
clean_chla <- rbind(kong_chla, is_chla, stor_chla, young_chla, disko_chla, nuup_chla, por_chla)
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
                       !grepl("Zooplankton in Kongsfjorden|Phytoplankton Seasonal", citation)) %>%
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
  mutate(type = "in situ")
rm(kong_biomass, is_biomass, stor_biomass, young_biomass, disko_biomass, nuup_biomass, por_biomass); gc()

# Summary analyses
summary_biomass <- review_summary(clean_biomass)

# Plot results
review_summary_plot(summary_biomass, "biomass")


## Species assemblage ------------------------------------------------------
# NB: Contact Allison Bailey about more recent plankton species data for Kongsfjorden

# For the time being it may be easier to convert these values to the more basic count of simply phytoplankton or zooplankton
# This would then allow for a more simple summary of the meta/data.

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
is_sp_ass <- filter(full_product_is, var_type == "bio") %>% mutate(site = "is") %>% slice(0) # No species data
stor_sp_ass <- filter(full_product_stor, var_type == "bio") %>% mutate(site = "stor") # No bio data
young_sp_ass <- filter(rbind(full_product_young, young_GEM), var_type == "bio") %>% 
  filter(grepl("Water Chlorophyll a", citation)) %>% mutate(site = "young")
disko_sp_ass <- filter(rbind(full_product_disko, disko_GEM), var_type == "bio") %>% mutate(site = "disko") %>% slice(0) # No species data
nuup_sp_ass <- filter(rbind(full_product_nuup, nuup_GEM), var_type == "bio",
                      grepl("Species Composition", citation)) %>% mutate(site = "nuup")
por_sp_ass <- filter(full_product_por, var_type == "bio") %>% mutate(site = "por") # No bio data
clean_sp_ass <- rbind(kong_sp_ass, is_sp_ass, stor_sp_ass, young_sp_ass, disko_sp_ass, nuup_sp_ass, por_sp_ass) %>% 
  mutate(type = "in situ")
rm(kong_sp_ass, is_sp_ass, stor_sp_ass, young_sp_ass, disko_sp_ass, nuup_sp_ass, por_sp_ass); gc()

# Need to think about how these analyses should proceed
# One idea is to extract or focus on specific species

# Summary analyses
# summary_sp_ass <- review_summary(clean_sp_ass)

# Plot results
# review_summary_plot(summary_sp_ass, "sp_ass")


## Save clean data ---------------------------------------------------------

clean_all <- rbind(clean_SST, clean_air, clean_sal, clean_PAR, clean_sea_ice, clean_snow,
                   # clean_glacier,
                   # clean_biomass,
                   # clean_spass,
                   clean_pCO2, clean_nutrients, clean_chla)
plyr::l_ply(unique(clean_all$var_type), save_category, .parallel = T,
            df = clean_all, data_type = "clean", site_name = "all")


## References --------------------------------------------------------------

# NB: Check for N-ICE and remove if present


## Summary -----------------------------------------------------------------

# Combine analysed data
all_meta <- rbind(mutate(summary_SST$monthly, var_group = "SST"),
                  mutate(summary_air$monthly, var_group = "Air temp"),
                  mutate(summary_sal$monthly, var_group = "Salinity"),
                  mutate(summary_PAR$monthly, var_group = "PAR"),
                  mutate(summary_ice$monthly, var_group = "Ice vars"),
                  mutate(summary_snow$monthly, var_group = "Snow vars"),
                  mutate(summary_pCO2$monthly, var_group = "pCO2"),
                  mutate(summary_nutrients$monthly, var_group = "Nutrients"),
                  mutate(summary_chla$monthly, var_group = "ChlA"))

all_meta %>% 
  filter(!is.na(value_mean)) %>% 
  mutate(month = lubridate::month(date)) %>% 
  ggplot(aes(x = as.factor(month), y = count_days)) +
  geom_boxplot(aes(fill = site), position = "dodge", outlier.colour = NA) +
  geom_jitter(aes(colour = log10(count))) +
  scale_colour_viridis_c() + scale_y_continuous(limits = c(0, 32), expand = c(0, 0)) +
  labs(y = paste0("Unique days with data points"), x = "Month", fill = "Site", colour = "Count [log10(n)]") +
  facet_grid(site~var_group) +
  theme(panel.border = element_rect(colour = "black", fill = NA))
ggsave("~/Desktop/meta_meta_box.png", width = 16, height = 12)


# Section 3 ---------------------------------------------------------------
# Relationships between data analysed for Section 2

# "In general, heatwaves favoured crawling or burrowing predators and suspension feeders, while the abundance of detritivores decreased, suggesting a climate-induced change in dominant zoobenthic traits (Pansch et al., 2018)."

# Section 4 ---------------------------------------------------------------
# Future projections of data analysed for Section 2 and relationships from Section 3

# "It has also been suggested that the various drivers of climate change may contribute to increase blooms and toxicity of cyanobacteria in the Baltic Sea. 
# For instance, the intracellular toxin concentration of the cyanobacterium Dolichospermum sp. may increase with elevated temperature (+4∘C) (Brutemark et al., 2015; Wulff et al., 2018) and with decreased salinity (from 6 to 3) (Wulff et al., 2018)."
# "an increase in temperature from 16 to 18–20∘C led to an earlier peak of cyanobacteria, while the biomass of cyanobacteria, especially that of nitrogen-fixer Dolichospermum sp. declined (Berner et al., 2018)."
# "To sum up, a shift towards smaller-sized zooplankton and a stronger linkage between mesozooplankton and the microbial food web is probable in a warmer Baltic Sea."
# "It has been projected that macroalgae will decline in hard bottoms and vascular plants increase in the more sheltered soft-bottom areas (Torn et al., 2020)."
# "Climate change will most probably mean milder winters, and if soils remain thawed, more nutrients will leak from the terrestrial areas into the freshwater system."
# "Several recent studies have however pointed out, for example, that macroalgae (Rothäusler et al., 2018; Rugiu et al., 2018a) and zooplankton (Karlsson and Winder, 2020) have phenotypic plasticity and potential for adaptation against gradual changes in the abiotic environment."


# Figure 1 ----------------------------------------------------------------



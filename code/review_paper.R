# code/review_paper.R
# The code used for the analyses in the WP1 review paper (D1.3)

# TODO: Add ability to filter out specific time series depending on length or some other criteria
# Do not include old data in trend analysis, but include them in the metadata summary of available data
# Create summary metadata figure showing sampling bias over time and clim months
# Add slope values to time constrained figures as well
# Need to be able to link the spatial and temporal mismatch of combined summary data with lack of trends etc.
# These issues are themselves an important part of the conclusions from the analysis
# Create and document a treatment for outliers (i.e. salinity)
# Consider changing analysis to 0-5 m rather than 0-10 m


# Setup -------------------------------------------------------------------

source("code/functions.R")


# Data --------------------------------------------------------------------

# FACE-IT collected data
load("~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")
load("~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.RData")
load("~/pCloudDrive/FACE-IT_data/storfjorden/full_product_stor.RData")
load("~/pCloudDrive/FACE-IT_data/young_sound/full_product_young.RData")
load("~/pCloudDrive/FACE-IT_data/disko_bay/full_product_disko.RData")
load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/full_product_nuup.RData")
load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/full_product_por.RData")

# GEM data
load("~/pCloudDrive/restricted_data/GEM/young_GEM.RData")
load("~/pCloudDrive/restricted_data/GEM/disko_GEM.RData")
load("~/pCloudDrive/restricted_data/GEM/nuup_GEM.RData")

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
kong_SST <- review_filter_var(full_product_kong, "Kong", "temp|°C",
                              "air|co2|ph_|pHint_|TTT|MAAT|MAGT|MAT|
                              |SST sum|SST win|Temp min|Temp max|Temp interp|
                              |tequ|tpot|T intern") %>%
  bind_rows(kong_OISST, kong_CCI) %>% mutate(site = "Kong")
# review_filter_check(kong_SST, "MAT [°C]", "Gattuso")

# Isfjorden
is_OISST <- sst_is_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
is_CCI <- sst_CCI_is_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
# t [°C] = ground/snow temperatures e.g. https://doi.pangaea.de/10.1594/PANGAEA.930472
# T tech [°C] + T cal [°C] = Temperatures from an experiment e.g. https://doi.pangaea.de/10.1594/PANGAEA.847626
is_SST <- review_filter_var(full_product_is, "Is", "temp|°C", 
                            "SST sum|SST win|TTT|MAT|MAGT|MAAT|Tpot|Tequ|air|T intern|T tech|T cal|pHT", "t [°C]") %>% 
  bind_rows(is_OISST, is_CCI) %>% mutate(site = "Is")
# review_filter_check(is_SST, "Temperature [ITS-90, deg C]", "VEINS Members;")

# Storfjorden
stor_OISST <- sst_stor_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
stor_CCI <- sst_CCI_stor_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
stor_SST <- review_filter_var(full_product_stor, "Stor", "temp|°C", "Tpot|Tequ|theta|fco2") %>% 
  bind_rows(stor_OISST, stor_CCI) %>% mutate(site = "Stor")
# review_filter_check(stor_SST)

# Young Sound
young_OISST <- sst_young_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
young_CCI <- sst_CCI_young_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
young_SST <- review_filter_var(rbind(full_product_young, young_GEM), "Young", "temp|°C", "Tpot|Tequ|theta|fco2|pot_temp") %>% 
  bind_rows(young_OISST, young_CCI) %>% mutate(site = "Young")
# review_filter_check(young_SST, "pot_temp [°C]")

# Disko Bay
disko_OISST <- sst_disko_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
disko_CCI <- sst_CCI_disko_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
disko_SST <- review_filter_var(rbind(full_product_disko, disko_GEM), "Disko", "temp|°C", "Tequ|potential|theta|fco2") %>% 
  bind_rows(disko_OISST, disko_CCI) %>% mutate(site = "Disko")
# review_filter_check(disko_SST)

# Nuup Kangerlua
nuup_OISST <- sst_nuup_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
nuup_CCI <- sst_CCI_nuup_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
nuup_SST <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "Nuup", "temp|°C", "Tequ|T tech") %>% 
  bind_rows(nuup_OISST, nuup_CCI) %>% mutate(site = "Nuup")
# review_filter_check(nuup_SST)

# Porsangerfjorden
por_OISST <- sst_por_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
por_CCI <- sst_CCI_por_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
por_SST <- review_filter_var(full_product_por, "Por", "temp|°C", "Tequ|Tpot|TTT") %>% 
  bind_rows(por_OISST, por_CCI) %>% mutate(site = "Por")
# review_filter_check(por_SST)

# Summary analyses
summary_SST <- review_summary(rbind(kong_SST, is_SST, stor_SST, young_SST, disko_SST, nuup_SST, por_SST))

# Plot results
# NB: The apparent cooling trend from in situ data is due to the lack of winter temperatures from pre-satellite era data
review_summary_plot(summary_SST, "temp", date_filter = c("1982-01-01", "2020-12-31"))

## Plot showing spatial difference between temperature products
### This may not work well across all sites


## Air temperature --------------------------------------------------------

# Get air temperature for all sites
kong_air <- review_filter_var(full_product_kong, "Kong", "air|temp|°C", "co2|intern|tequ|f_|p_|par_|temp_|_ctd|_sf",
                              var_precise = "Temp [°C]", atmos = T) %>% 
  filter(URL != "https://doi.org/10.1594/PANGAEA.882432",
         URL != "https://doi.org/10.1594/PANGAEA.839802")
is_air <- add_depth(review_filter_var(full_product_is, "Is", "air|temp|°C", "tequ|intern|bulb|pressure",  
                                      var_precise = "Temp [°C]", atmos = T)) %>% filter(!var_name == "t [°C]")
stor_air <- add_depth(review_filter_var(full_product_stor, "Stor", "air|temp|°C", 
                                        var_precise = "Temp [°C]", atmos = T)) %>% filter(!var_name == "t [°C]")
young_air <- add_depth(review_filter_var(full_product_young, "Young", "air|temp|°C", var_precise = "Temp [°C]", atmos = T))
disko_air <- add_depth(review_filter_var(full_product_disko, "Disko", "air|temp|°C", "tequ", var_precise = "Temp [°C]", atmos = T))
nuup_air <- add_depth(review_filter_var(full_product_nuup, "Nuup", "air|temp|°C", "tequ", var_precise = "Temp [°C]", atmos = T))
por_air <- add_depth(review_filter_var(full_product_por, "Por", "air|temp|°C", "tequ|bulb", 
                                       var_precise = "Temp [°C]", atmos = T)) %>% filter(!var_name == "temp [°C]")

# Summary analyses
summary_air <- review_summary(rbind(kong_air, is_air, stor_air, young_air, disko_air, nuup_air, por_air))

# Plot results
review_summary_plot(summary_air, "air", date_filter = c("1982-01-01", "2020-12-31"))


## Salinity ---------------------------------------------------------------

# Kongsfjorden
# Remove overly processed variables
# sal interp e.g. https://doi.org/10.1594/PANGAEA.877869
# Remove glacial drainage land stations
# Temporarily remove Ferry box data until documentation for variable names is located
kong_sal <- review_filter_var(full_product_kong, "Kong", "sal|PSU|s_", "interp|ph",
                              cit_filter = "land station|drainage|meltwater")
# review_filter_check(kong_sal, "psal [1e-3]", "Skogseth")

# Isfjorden
is_sal <- review_filter_var(full_product_is, "Is", "sal|PSU", "interp")
# review_filter_check(is_sal, "Sal [mg/l]", "Knittel")

# Storfjorden
stor_sal <- review_filter_var(full_product_stor, "Stor", "sal|PSU")
# review_filter_check(stor_sal, "Sal [mg/l]", "Olsen, Are")

# Young Sound
young_sal <- review_filter_var(rbind(full_product_young, young_GEM), "Young", "sal|PSU", "sal interp")
# review_filter_check(young_sal, "Sal [mg/l]")

# Disko Bay
disko_sal <- review_filter_var(rbind(full_product_disko, disko_GEM), "Disko", "sal|PSU", "sal interp")
# review_filter_check(disko_sal, "Sal [mg/l]")

# Nuup Kangerlua
nuup_sal <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "Nuup", "sal|PSU", "sal interp")
# review_filter_check(nuup_sal, "Sal [mg/l]")

# Porsangerfjorden
# Some dubious values in "Mankettikkara"
por_sal <- review_filter_var(full_product_por, "Por", "sal|PSU", "Sal interp") %>% filter(value > 0)
# review_filter_check(por_sal, "sal", "Mankettikkara")

# Summary analyses
summary_sal <- review_summary(rbind(kong_sal, is_sal, stor_sal, young_sal, disko_sal, nuup_sal, por_sal))

# Plot results
review_summary_plot(summary_sal, "sal", date_filter = c("1982-01-01", "2020-12-31"))


## Light ------------------------------------------------------------------

# Kongsfjorden
kong_PAR <- review_filter_var(full_product_kong, "Kong", "PAR", "Onc|Gym|Para|below")
# review_filter_check(kong_PAR)

# Isfjorden
# NB: No PAR data
is_PAR <- review_filter_var(full_product_is, "Is", "PAR")
# review_filter_check(is_PAR)

# Storfjorden
# NB: No PAR data
stor_PAR <- review_filter_var(full_product_stor, "Stor", "PAR")
# review_filter_check(stor_PAR)

# Young Sound
young_PAR <- review_filter_var(rbind(full_product_young, young_GEM), "Young", "PAR")
# review_filter_check(young_sal)

# Disko Bay
## NB: It is unclear if these values should be divided by 10 or not
## It is also unclear what the time dimension is for the data
disko_PAR <- review_filter_var(rbind(full_product_disko, disko_GEM), "Disko", "PAR") %>% filter(value > 0)
# review_filter_check(disko_PAR)

# Nuup Kangerlua
## NB: Some of these values are also very high
nuup_PAR <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "Nuup", "PAR")
# review_filter_check(nuup_PAR)

# Porsangerfjorden
## NB: No PAR data
por_PAR <- review_filter_var(full_product_por, "Por", "PAR")
# review_filter_check(por_PAR)

# Summary analyses
summary_PAR <- review_summary(rbind(kong_PAR, is_PAR, stor_PAR, young_PAR, disko_PAR, nuup_PAR, por_PAR))

# Plot results
review_summary_plot(summary_PAR, "par", date_filter = c("1982-01-01", "2020-12-31"))


# Section 3 ---------------------------------------------------------------
# Relationships between data analysed for Section 2


# Section 4 ---------------------------------------------------------------
# Future projections of data analysed for Section 2 and relationships from Section 3


# Figure 1 ----------------------------------------------------------------



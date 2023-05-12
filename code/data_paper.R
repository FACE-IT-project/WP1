# code/data_paper.R
# The code used for the analyses in the WP1 data paper (D1.3)

# It is here that the drivers are assigned to the data

# Quotes are from Viitasalo and Bonsdorff (2022) unless stated otherwise
# https://esd.copernicus.org/articles/13/711/2022/
# "In addition, to better understand the effects of climate change on the biodiversity of the Baltic Sea, more emphasis should be placed on studies of shallow photic environments."

# NB: Individual clean_*_all.csv are available at ~/WP1/data/full_data/


# Setup -------------------------------------------------------------------

source("code/functions.R")
library(ggpmisc) # For plotting tables
library(listr) # For dealing with lists - not used
library(ggplotify) # For working with complex data visuals
library(ggcorrplot) # For correlograms
library(ggalluvial) # For alluvial plot
library(ggasym) # For correlation plots with multiple colour bars
library(ggrepel) # For labels with segments
library(treemapify) # For gridded tree map
library(qrcode)

# QR code for data paper
# QR_data_paper <- qr_code("https://essd.copernicus.org/preprints/essd-2022-455/")
# plot(QR_data_paper)
# generate_svg(QR_data_paper, filename = "presentations/QR_data_paper.svg")


# Data --------------------------------------------------------------------

# FACE-IT collected data
## NB: It is not useful to combine all of these files into a single dataframe
load("~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")
load("~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.RData")
load("~/pCloudDrive/FACE-IT_data/storfjorden/full_product_stor.RData")
load("~/pCloudDrive/FACE-IT_data/young_sound/full_product_young.RData")
load("~/pCloudDrive/FACE-IT_data/disko_bay/full_product_disko.RData")
load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/full_product_nuup.RData")
load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/full_product_por.RData")

# Extra data by site name only in EU, sval, green, and nor files
load("~/pCloudDrive/FACE-IT_data/EU_arctic/full_product_EU.RData")
load("~/pCloudDrive/FACE-IT_data/svalbard/full_product_sval.RData")
load("~/pCloudDrive/FACE-IT_data/greenland/full_product_green.RData")
load("~/pCloudDrive/FACE-IT_data/norway/full_product_nor.RData")

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


# Section 3 ---------------------------------------------------------------

# Mostly annual and monthly mean states of key drivers
# Alongfjord gradients is about as complex as we want to get
# Limit to the top 10 metres. Consider the bottom 10 metres.
# Line plots comparing averages values across sites
# Solid colour for in situ data, dashed line for NOAA, dotted for CCI
# Also produce summary stats
## Mean, median, min, max, skewness, kurtosis
## Summary of data available per month/season for sites


## Cryosphere --------------------------------------------------------------

### Sea ice ----------------------------------------------------------------

# Test check for all bio vars to make sure no biomass vars are missed
as.vector(distinct(filter(full_product_por, category == "cryo"), variable))
as.vector(distinct(filter(nuup_GEM, category == "cryo"), variable))

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
sea_ice_kong <- review_filter_var(full_product_kong, "ice|EsEs", "glacier|ind/m3|extent") # bi, ci, zi, and ice extent not useful
sea_ice_is <- review_filter_var(full_product_is, "ice|EsEs", "glacier|ind/m3|extent")# A couple EsEs acc values...
sea_ice_stor <- review_filter_var(full_product_stor, "ice|EsEs|IP", "precip|glacier|snow|extent|Storfjord") # A lot of ice production data
sea_ice_young <- review_filter_var(rbind(full_product_young, young_GEM), "ice|open", "snow") # A lot of GEM data
sea_ice_disko <- review_filter_var(rbind(full_product_disko, disko_GEM), "ice") %>% slice(0) # No sea ice data
sea_ice_nuup <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "ice") # No sea ice data
sea_ice_por <- review_filter_var(full_product_por, "ice", "extent") # Ice cover in km^2
clean_sea_ice <- rbind(sea_ice_kong, sea_ice_is, sea_ice_stor, sea_ice_young, sea_ice_disko, sea_ice_nuup, sea_ice_por) %>% 
  filter(!is.na(value)) %>% mutate(driver = "sea ice", depth = NA, # Deeper depths are bottom depths and should be converted to NA
                                   date_accessed = as.Date(date_accessed)) 
rm(sea_ice_kong, sea_ice_is, sea_ice_stor, sea_ice_young, sea_ice_disko, sea_ice_nuup, sea_ice_por); gc()

# Figures
## Need custom figures per site
## Consistent metadata files may not be useful across sites
# filter(all_sea_ice, !variable %in% c("Open water [start date]", "Open water [end date]"))
ggplot(clean_sea_ice, aes(x = date, y = value, colour = site)) +
  geom_point() + geom_line() + 
  facet_wrap(~variable, scales = "free_y")
ggsave("~/Desktop/analyses_output/ice_var_ts.png", width = 20, height = 16)

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
rm(ice_4km_kong, ice_4km_is, ice_4km_stor, ice_4km_young, ice_4km_disko, ice_4km_nuup, ice_4km_por,
   ice_4km_kong_proc, ice_4km_is_proc, ice_4km_stor_proc, ice_4km_young_proc,
   ice_4km_disko_proc, ice_4km_nuup_proc, ice_4km_por_proc); gc()
# load("data/analyses/ice_4km_proc.RData")
ice_4km_prop <- plyr::ddply(ice_4km_proc, c("site"), ice_cover_prop, .parallel = T)

# Calculate trends
ice_4km_trend <- plyr::ddply(dplyr::rename(ice_4km_prop, val = mean_prop), c("site", "month"), trend_calc, .parallel = T)

# Combine with other clean data
ice_4km_prop_long <- ice_4km_prop %>%
  dplyr::rename(value = mean_prop) %>% 
  dplyr::select(date, value, site) %>% 
  mutate(variable = "sea ice cover [proportion]")
ice_4km_trend_long <- ice_4km_trend %>% 
  pivot_longer(trend:sd_val, names_to = "variable") %>% 
  mutate(variable = case_when(variable == "trend" ~ paste0("sea ice cover ",month," [annual proportion trend]"),
                              variable == "p.value" ~ paste0("sea ice cover ",month," [annual proportion trend p-value]"),
                              variable == "mean_val" ~ paste0("sea ice cover ",month," [mean proportion]"),
                              variable == "sd_val" ~ paste0("sea ice cover ",month," [SD proportion]"))) %>% 
  dplyr::select(-month)
ice_4km_stats <- bind_rows(ice_4km_prop_long, ice_4km_trend_long) %>% 
  mutate(type = "MASIE",
         category = "cryo",
         driver = "sea ice",
         date_accessed = as.Date("2022-04-26"),
         URL = "https://doi.org/10.7265/N5GT5K3K",
         citation = "U.S. National Ice Center and National Snow and Ice Data Center. Compiled by F. Fetterer, M. Savoie, S. Helfrich, and P. Clemente-Colón. 2010, updated daily. Multisensor Analyzed Sea Ice Extent - Northern Hemisphere (MASIE-NH), Version 1. 4km resolution. Boulder, Colorado USA. NSIDC: National Snow and Ice Data Center. doi: https://doi.org/10.7265/N5GT5K3K.")

# Bind together
clean_sea_ice <- bind_rows(clean_sea_ice, ice_4km_stats) %>% distinct()

# Analyses
summary_sea_ice <- review_summary(clean_sea_ice)

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


### Glacier -----------------------------------------------------------------

# NB: Chose not to get many variables from Geyman et al. 2021

# TODO: Include `t [°C]` here

# Test check for all cryo vars to make sure no glacier vars are missed
as.vector(distinct(filter(full_product_stor, category == "cryo"), variable))
as.vector(distinct(filter(nuup_GEM, category == "cryo"), variable))

# Get all glacier variables
glacier_kong <- review_filter_var(full_product_kong, "balance|glacier|area|volume|slope")
glacier_is <- review_filter_var(full_product_is, "balance|glacier|area|volume|slope")
glacier_stor <- review_filter_var(full_product_stor, "balance|glacier|area|volume|slope")
glacier_young <- review_filter_var(rbind(full_product_young, young_GEM), "balance|glacier|ablation")
glacier_disko <- review_filter_var(rbind(full_product_disko, disko_GEM), "balance|glacier|ablation")
glacier_nuup <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "glac", "poro")
glacier_por <- review_filter_var(full_product_por, "balance|glac") # No glacier data
clean_glacier <- rbind(glacier_kong, glacier_is, glacier_stor, glacier_young, glacier_disko, glacier_nuup, glacier_por) %>% 
  mutate(driver = "glacier")
rm(glacier_kong, glacier_is, glacier_stor, glacier_young, glacier_disko, glacier_nuup, glacier_por); gc()

# Summary analyses
summary_glacier <- review_summary(clean_glacier)

# Plot results
review_summary_plot(summary_glacier, "glacier")

# Grab glacier values directly from EU or Svalbard products for certainty
# Look for specific DOI in each site file


### Runoff ------------------------------------------------------------------

# Pedro Duarte has contacted a colleague to get Kongsfjorden area river discharge data

# GRDC river discharge data
## NB: These are restricted data so they are not added to 'full_product_EU'
# lta_discharge = long-term average discharge, cubic metre per sec
# r_vol_yr = mean annual volume, cubic kilometre
# r_height_yr	= mean annual runoff depth, mm
EU_GRDC <- read_csv("~/pCloudDrive/restricted_data/GRDC/grdc_arctichycos_stations.csv")
site_GRDC <- map_dfr(dir("~/pCloudDrive/restricted_data/GRDC", pattern = "Cmd.txt", full.names = T), load_GRDC)

# Get all river discharge data from full/GEM products
kong_runoff <- review_filter_var(full_product_kong, "river|disc|Q|run", "Disco|hetero|equ|AT|dhdt") # No discharge data
is_runoff <- review_filter_var(full_product_is, "river|disc|Q|run", "equ|hPa|dhdt") # No discharge data
stor_runoff <- review_filter_var(full_product_stor, "river|disc|Q|run", "equ|AT|dhdt") # No discharge data
young_runoff <- review_filter_var(rbind(full_product_young, young_GEM), "river|disc|Q|run", "coscin|Qnet")
disko_runoff <- review_filter_var(rbind(full_product_disko, disko_GEM), "river|disc|Q|run", "equ") # No discharge data
nuup_runoff <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "river|disc|Q|run", "equ|coscin|prot|psamm")
por_runoff <- review_filter_var(full_product_por, "river|disc|Q|run", "equ")# No discharge data

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
  pivot_longer(`Q [m3/s]`, names_to = "variable") %>% 
  mutate(category = "cryo", date_accessed = as.Date("2022-06-13"), type = "in situ",
         URL = "https://www.bafg.de/GRDC/EN/04_spcldtbss/41_ARDB/ardb_node.html", 
         citation = "Arctic Region Discharge Data (2021). The Global Runoff Data Centre, 56068 Koblenz, Germany") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site, type)

# Combine all datasets and clean up
clean_runoff <- rbind(kong_runoff, is_runoff, stor_runoff, young_runoff, disko_runoff, nuup_runoff, por_runoff, FACE_IT_GRDC) %>% 
  mutate(driver = "runoff")
rm(kong_runoff, is_runoff, stor_runoff, young_runoff, disko_runoff, nuup_runoff, por_runoff, EU_GRDC, FACE_IT_GRDC); gc()

# Summary analyses
summary_runoff <- review_summary(clean_runoff)

# Plot results
review_summary_plot(summary_runoff, "runoff")


## Physics ----------------------------------------------------------------

### Sea temp ----------------------------------------------------------------

# TODO: Look into temperature values above 20°C

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
OISST_kong <- sst_kong_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
CCI_kong <- sst_CCI_kong_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
sea_temp_kong <- review_filter_var(full_product_kong, "temp|°C",
                                   "air|co2|ph_|pHint_|TTT|MAAT|MAGT|MAT|mean_|
                                   |SST sum|SST win|Temp min|Temp max|Temp interp|
                                   |tequ|tpot|T intern") %>%
  bind_rows(OISST_kong, CCI_kong) %>% mutate(site = "kong")
OISST_is <- sst_is_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
CCI_is <- sst_CCI_is_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
sea_temp_is <- review_filter_var(full_product_is, "temp|°C", 
                                 "SST sum|SST win|TTT|MAT|MAGT|MAAT|Tpot|Tequ|air|T intern|T tech|T cal|pHT|
                                 |T sum|T win|SST anomaly|theta|mean_", c("t [°C]", "SST (1-12) [°C]")) %>% # Can re-add if annual values
  bind_rows(OISST_is, CCI_is) %>% mutate(site = "is")
OISST_stor <- sst_stor_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
CCI_stor <- sst_CCI_stor_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
sea_temp_stor <- review_filter_var(full_product_stor, "temp|°C", "Tpot|Tequ|theta|fco2|Tmax|TTT|SST anomaly|mean_", 
                                   var_precise = c("t [°C]", "SST (1-12) [°C]")) %>% 
  bind_rows(OISST_stor, CCI_stor) %>% mutate(site = "stor")
OISST_young <- sst_young_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
CCI_young <- sst_CCI_young_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
sea_temp_young <- review_filter_var(rbind(full_product_young, young_GEM), "temp|°C", 
                                    "Tpot|Tequ|theta|fco2|pot_temp|SST sum|SST win|MAGT|MAAT|TTT") %>% 
  bind_rows(OISST_young, CCI_young) %>% mutate(site = "young")
OISST_disko <- sst_disko_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
CCI_disko <- sst_CCI_disko_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
sea_temp_disko <- review_filter_var(rbind(full_product_disko, disko_GEM), "temp|°C", 
                                    "Tequ|potential|theta|fco2|SST sum|SST win|TTT|SST anomaly|ice_", "SST (1-12) [°C]") %>% 
  bind_rows(OISST_disko, CCI_disko) %>% mutate(site = "disko")
OISST_nuup <- sst_nuup_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
CCI_nuup <- sst_CCI_nuup_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
sea_temp_nuup <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "temp|°C", 
                                   "Tequ|T tech|Tpot|SST sum|SST win|TTT") %>% 
  bind_rows(OISST_nuup, CCI_nuup) %>% mutate(site = "nuup")
OISST_por <- sst_por_bbox %>% dplyr::rename(date = t) %>% 
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "OISST")
CCI_por <- sst_CCI_por_bbox %>% dplyr::rename(date = t) %>%  
  group_by(date) %>% summarise(value = mean(temp, na.rm = T)) %>% mutate(type = "CCI")
sea_temp_por <- review_filter_var(full_product_por, "temp|°C", 
                                  "Tequ|Tpot|TTT|wet bulb|SST anomaly|T air|MAAT", "SST (1-12) [°C]") %>% 
  bind_rows(OISST_por, CCI_por) %>% mutate(site = "por")
# review_filter_check(por_SST)

# Combined cleaned data
clean_sea_temp <- rbind(sea_temp_kong, sea_temp_is, sea_temp_stor, sea_temp_young, sea_temp_disko, sea_temp_nuup, sea_temp_por) %>% 
  mutate(depth = case_when(is.na(depth) & type %in% c("OISST", "CCI") ~ 0, TRUE ~ depth),
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
         variable = case_when(variable %in% c("t_fb [°C]") ~ "temp_pco2 [°C]", # Temperatures for pCO2 analyses
                              TRUE ~ "temp [°C]"),
         category = "phys",
         driver = "sea temp") %>% 
  filter(depth >= 0, value > -1.8)
rm(sea_temp_kong, sea_temp_is, sea_temp_stor, sea_temp_young, sea_temp_disko, sea_temp_nuup, sea_temp_por); gc()

# Summary analyses
summary_sea_temp <- review_summary(clean_sea_temp)

# Plot results
# NB: The apparent cooling trend from in situ data is due to the lack of winter temperatures from pre-satellite era data
review_summary_plot(summary_sea_temp, "sea temp")

## Plot showing spatial difference between temperature products
### This may not work well across all sites


### Salinity ---------------------------------------------------------------

# "Knowledge gaps include uncertainties in projecting the future salinity level, as well as stratification and potential rate of internal loading, under different climate forcings."
# "This weakens our ability to project how pelagic productivity, fish populations and macroalgal communities may change in the future."
# "the decline of marine taxa has usually been proposed to be linked to a decrease of salinity (Suikkanen et al., 2013; Hänninen et al., 2015)"

# Get all salinity data
# NB: Remove Sal [mg/l]
# Remove overly processed variables
# sal interp e.g. https://doi.org/10.1594/PANGAEA.877869
# Remove glacial drainage land stations
sal_kong <- review_filter_var(full_product_kong, "sal|PSU|s_", "interp|ph|oxy|ws|mass_",
                              cit_filter = "land station|drainage|meltwater")
sal_is <- review_filter_var(full_product_is, "sal|PSU", "interp|mg/l")
sal_stor <- review_filter_var(full_product_stor, "sal|PSU", "interp|acu|ent")
sal_young <- review_filter_var(rbind(full_product_young, young_GEM), "sal|PSU", "sal interp|acu|ent")
sal_disko <- review_filter_var(rbind(full_product_disko, disko_GEM), "sal|PSU", "sal interp")
sal_nuup <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "sal|PSU", "sal interp|acu|ent")
sal_por <- review_filter_var(full_product_por, "sal|PSU", "Sal interp")
clean_sal <- rbind(sal_kong, sal_is, sal_stor, sal_young, sal_disko, sal_nuup, sal_por) %>%
  mutate(variable = case_when(variable %in% c("s_fb [unit]") ~ "sal_pco2", # Salinity for pCO2 analyses
                              TRUE ~ "sal"), 
         driver = "salinity") %>% 
  filter(value > 0)
rm(sal_kong, sal_is, sal_stor, sal_young, sal_disko, sal_nuup, sal_por); gc()

# Summary analyses
summary_sal <- review_summary(clean_sal)

# Plot results
review_summary_plot(summary_sal, "sal")


### Light ------------------------------------------------------------------

# Get all PAR+UV data
light_kong <- review_filter_var(full_product_kong, "PAR|UV", "Onc|Gym|Para|below|abys|harp|chae|ostr|clio|cirr|biva")
light_is <- review_filter_var(full_product_is, "PAR|UV", "aeuch|eleg|UVEL") # No PAR data
light_stor <- review_filter_var(full_product_stor, "PAR|UV") # No PAR data
light_young <- review_filter_var(rbind(full_product_young, young_GEM),  "PAR|UV", "vella|tinn")
light_disko <- review_filter_var(rbind(full_product_disko, disko_GEM), "PAR|UV", "milli")
light_nuup <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "PAR|UV", "trip|vella|sulc|lip|lib|parv")
light_por <- review_filter_var(full_product_por, "PAR|UV", "Para") # No PAR data
clean_light <- rbind(light_kong, light_is, light_stor, light_young, light_disko, light_nuup, light_por) %>% 
  filter(value > 0,
         !grepl("volt", variable)) %>%
  mutate(value = case_when(str_detect(variable, "mmol") ~ value/1000, TRUE ~ value),
         variable = case_when(str_detect(variable, "PAR|par") ~ "PAR [µmol m-2 s-1]",
                              str_detect(variable, "UVA") ~ "UV-A [W*m^2]", # TODO: Keep or remove?
                              str_detect(variable, "UVB") ~ "UV-B [W*m^2]",
                              TRUE ~ variable), driver = "light")
rm(light_kong, light_is, light_stor, light_young, light_disko, light_nuup, light_por); gc()

# Summary analyses
summary_light <- review_summary(clean_light)

# Plot results
review_summary_plot(summary_light, "light")


## Chemistry ---------------------------------------------------------------

### Carb -------------------------------------------------------------------

# TODO: Sort out the variable conversions etc.
# Bring DIC back into dataset
# pH is not always the same, there are different scales with differences of up to 0.2
## It requires expert knowledge and review of each citation to determine the provenence of the pH scale...
# Difference in measured vs calculated pCO2, and difference in SST and normalised temperature

# From Liqing Jiang:
# I like the idea of adding the carbon parameter pair used to conduct the CO2 system calculation to the variable name. 
# After all, they could have different associated uncertainties. 
# For data submission purposes, please feel free to use these new names as you suggested. 

# Check all variables in a product
unique(filter(full_product_kong, category == "chem")$variable)

# Keep pCO2_calc as a separate variable because they can't be taken as absolutely the same
# Same for PCO2water_SST_wet
# Can use SeaCarb to transform fco2 to pCO2
# Note that there are duplicates from GLODAP and the underlying files downloaded via PANGAEA
# But this is actually a good thing as it allows us to acknowledge specific contributors,
# which is something that the GLODAP product requests that we do.
carb_kong <- review_filter_var(filter(full_product_kong, category == "chem"), 
                               "DIC|CO2|pH|TA|AT|Alk|CaCO3|calc|carb|diox", "O2 sat|PO4|NO2|NO3|NH4")
carb_is <- review_filter_var(filter(full_product_is, category == "chem"),
                             "CO2|pH|TA|AT|Alk|CaCO3|calc|carb|diox", 
                             "O2 sat|PO4|NO2|NO3|nitrate|silicate|phosphate|tco2|Isfjord|EP TA")
carb_stor <- review_filter_var(filter(full_product_stor, category == "chem"), 
                               "CO2|pH|TA|AT|Alk|CaCO3|calc|carb|diox", 
                               "O2 sat|nitrate|silicate|phosphate|tco2|fco2|Storfjord")
carb_young <- review_filter_var(filter(rbind(full_product_young, young_GEM), category == "chem"), 
                                "CO2|pH|TA|AT|Alk|CaCO3|calc|carb|diox", "nitrate")
carb_disko <- review_filter_var(filter(rbind(full_product_disko, disko_GEM), category == "chem"), 
                                "CO2|pH|TA|AT|Alk|CaCO3|calc|carb|diox", "oxygen|nitrate|silicate|phosphate|tco2|fco2")
carb_nuup <- review_filter_var(filter(rbind(full_product_nuup, nuup_GEM), category == "chem"), 
                               "CO2|pH|TA|AT|Alk|CaCO3|calc|carb|diox", "nitrate")
carb_por <- review_filter_var(filter(full_product_por, category == "chem"), 
                              "CO2|pH|TA|AT|Alk|CaCO3|calc|carb|diox", "O2 sat")
clean_carb <- rbind(carb_kong, carb_is, carb_stor, carb_young, carb_disko, carb_nuup, carb_por) %>% 
  filter(!variable %in% c("pH_dur [total scale]", "ph_s_sf_t_insi [total scale]", 
                          "ph_s_dur_t_fb [total scale]", "pH_calc [total scale]", "phts25p0")) %>% 
  mutate(value = case_when(variable == "AT [mmol(eq)/l]" ~ value*1000, TRUE ~ value), # Convert to µmol/l
         variable = case_when(variable %in% c("AT [mmol(eq)/l]", "AT [µmol/kg]",
                                              "talk [μmol kg-1]") ~ "TA [µmol/kg]",
                              variable %in% c("pco2 [uatm]") ~ "pCO2 [µatm]", 
                              variable %in% c("pCO2water_SST_wet [uatm]") ~ "pCO2water_SST_wet [µatm]",
                              variable %in% c("pco2_calc [uatm]") ~ "pCO2_calc [µatm]",
                              variable %in% c("pH_sf [total scale]", "pHT in situ", "phtsinsitutp") ~ "pH in situ [total scale]",
                              variable %in% c("pH") ~ "pH [unknown scale]",
                              TRUE ~ variable),
         driver = "carb")
# unique(clean_carb$variable[str_detect(clean_carb$variable, "ph|pH")]) # Double check that only pH values are screened this way
# unique(clean_carb$variable)
# test_df <- dplyr::select(clean_carb, citation, variable) %>% distinct() %>% filter(str_detect(variable, "ph|pH"))
# test_df <- filter(clean_carb, variable == "phtsinsitutp")
rm(carb_kong, carb_is, carb_stor, carb_young, carb_disko, carb_nuup, carb_por); gc()

# Summary analyses
summary_carb <- review_summary(clean_carb)

# Plot results
review_summary_plot(summary_carb, "carb")


### Nutrients ---------------------------------------------------------------

# TODO: Create report showing difference in GLODAP l and kg values

# "The associated increase in N:P ratio may contribute to maintaining the “vicious circle of eutrophication”. "
# "An increase of riverine dissolved organic matter (DOM) may also decrease primary production, but the relative importance of this process in different sea areas is not well known."
# "Climate change will probably delay the effects of nutrient abatement and tend to keep the ecosystem in its “novel” state."
# "However, several modelling studies conclude that nutrient reductions will be a stronger driver for ecosystem functioning of the Baltic Sea than climate change."
# "Such studies highlight the importance of studying the Baltic Sea as an interlinked socio-ecological system."

# [µmol/l] is the same as [µg-at/l]
# [µmol/l] vs [μmol kg-1] are different, a conversion should be made between them, but they appear to be used interchangeably

# Keep Nitrate + Nitrite

# Same same
# - [NO2]- vs NO2
# - PO4 vs [PO4]3-

# Get all nutrient data
nutrients_kong <- review_filter_var(full_product_kong, "nitr|amon|phos|silic|NO3|NO2|NH4|PO4|SiO4", "stddev|stephos")
nutrients_is <- review_filter_var(full_product_is, "nitr|amon|phos|silic|NO3|NO2|NH4|PO4|SiO4")
nutrients_stor <- review_filter_var(full_product_stor, "nitr|amon|phos|silic|NO3|NO2|NH4|PO4|SiO4")
nutrients_young <- review_filter_var(rbind(full_product_young, young_GEM), 
                                     "nitr|amon|phos|silic|NO3|NO2|NH4|PO4|SiO4", "nitracline")
nutrients_disko <- review_filter_var(rbind(full_product_disko, disko_GEM),
                                     "nitr|amon|phos|silic|NO3|NO2|NH4|PO4|SiO4")
nutrients_nuup <- review_filter_var(rbind(full_product_nuup, nuup_GEM), 
                                    "nitr|amon|phos|silic|NO3|NO2|NH4|PO4|SiO4", "chlam")
nutrients_por <- review_filter_var(full_product_por, "nitr|amon|phos|silic|NO3|NO2|NH4|PO4|SiO4")
clean_nutrients <- rbind(nutrients_kong, nutrients_is, nutrients_stor, nutrients_young, nutrients_disko, nutrients_nuup, nutrients_por) %>% 
  filter(value > 0) %>%
  filter(variable != "NO3 [µmol/kg]") %>% # TODO: Fix this conversion in data_product.R seacarb::rho() see help file
        # Change GLODAP variable to match PANGAEA standard 
  mutate(variable = case_when(variable == "nitrate [μmol kg-1]" ~ "NO3 [µmol/l]",   
                              variable == "nitrite [μmol kg-1]" ~ "NO2 [µmol/l]",   
                              variable == "silicate [μmol kg-1]" ~ "SiO4 [µmol/l]",
                              variable == "phosphate [μmol kg-1]" ~ "PO4 [µmol/l]",
                              TRUE ~ variable),
         # Convert other variable names to a single standard
         variable = case_when(variable %in% c("[NO3]- [µmol/l]",
                                              # "NO3 [µmol/kg]", # Possible units issue
                                              "NO3 [µg-at/l]") ~ "NO3 [µmol/l]", 
                              variable %in% c("[PO4]3- [µmol/l]", "PO4 [µg-at/l]") ~ "PO4 [µmol/l]",
                              variable %in% c("[NH4]+ [µmol/l]", "[NH4]+ [µg-at/l]") ~ "NH4 [µmol/l]",
                              variable %in% c("[NO2]- [µmol/l]", "[NO2]- [µg-at/l]") ~ "NO2 [µmol/l]",
                              variable %in% c("nitrate+nitrite [µmol/l]", "[NO3]- + [NO2]- [µmol/l]",
                                              "NO2_NO3 [µmol/l]") ~ "NO3+NO2 [µmol/l]",
                              TRUE ~ variable),
         driver = "nutrients")
# unique(clean_nutrients$variable)
# test_df <- filter(clean_nutrients, variable == "NO3 [µmol/l]")
rm(nutrients_kong, nutrients_is, nutrients_stor, nutrients_young, nutrients_disko, nutrients_nuup, nutrients_por); gc()

# Summary analyses
summary_nutrients <- review_summary(clean_nutrients)

# Plot results
review_summary_plot(summary_nutrients, "nutrients")


## Biology -----------------------------------------------------------------

### Primary production ------------------------------------------------------

# TODO: Look into making PP conversion calculations with existing data 

# Phaeopygments etc are not measures of PP, don't need fluorescence either

# "For phytoplankton, clear symptoms of climate change, such as prolongation of the growing season, are evident and can be explained by the warming, but otherwise climate effects vary from species to species and area to area."
# "A 15-year study (2000–2014) using FerryBox observations, covering the area between Helsinki (Gulf of Finland) and Travemünde (Mecklenburg Bight), confirmed that spring bloom intensity was mainly determined by winter nutrient concentration, while bloom timing and duration co-varied with meteorological conditions." 
# "The authors conclude that the bloom magnitude has been affected by the reduction of nutrient loading from land, while bloom phenology can also be modified by global climate change affecting seasonal oceanographic and biogeochemical processes (Groetsch et al., 2016)."

# [10um] vs [GFF] are different methods and both are valid.
# Must keep the difference between them documented.

# Collect all ChlA data
# https://zenodo.org/record/5572041#.YW_Lc5uxU5m: chl_flu [µg chl m-3] = chlorophyll a calculated from fluorescence profile
pp_kong <- review_filter_var(full_product_kong, "chl|pp|prim|prod", "sp|spp|hPa|ppt|phyceae|append|scripp")
pp_is <- review_filter_var(full_product_is, "chl|pp|prim|prod", "hPa|spp|ppt")
pp_stor <- review_filter_var(full_product_stor, "chl|pp|prim|prod", "hPa|ppt") # No PP data
pp_young <- review_filter_var(rbind(full_product_young, young_GEM), "chl|pp|prim|prod", "dippl|scripp|max") # Lot's of different variables
pp_disko <- review_filter_var(rbind(full_product_disko, disko_GEM), "chl|pp|prim|prod", "hPa|ppt")
pp_nuup <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "chl|pp|prim|prod", "hPa|chlamy|individ|nodos")
pp_por <- review_filter_var(full_product_por, "chl|pp|prim|prod", "hPa") # No PP data
clean_pp <- rbind(pp_kong, pp_is, pp_stor, pp_young, pp_disko, pp_nuup, pp_por) %>% 
  filter(value > 0) %>%
  mutate(variable = case_when(variable %in% c("chlA [µg/l]", "Chl a [µg/l]") ~ "Chla [µg/l]", 
                              variable == "Chlorophyll A - 10um [µg/l]" ~ "Chla - 10um [µg/l]",
                              variable == "Chlorophyll A - GFF [µg/l]" ~ "Chla - GFF [µg/l]",
                              TRUE ~ variable),
         driver = "prim prod")
# unique(clean_pp$variable)
# unique(clean_pp$variable[str_detect(clean_pp$variable, "PP|pp")]) # Double check that only pH values are screened this way
# test_df <- filter(clean_pp, variable == "TOTAL_chla_area")
rm(pp_kong, pp_is, pp_stor, pp_young, pp_disko, pp_nuup, pp_por); gc()

# Summary analyses
summary_pp <- review_summary(clean_pp)

# Plot results
review_summary_plot(summary_pp, "pp")


### Biomass -----------------------------------------------------------------

# TODO: Check this for lot's of variables in Young Sound: https://zenodo.org/record/5572041#.YW_Lc5uxU5m
# TODO: Look into creating phytoplankton biomass conversion using Chl a data

# Test check for all bio vars to make sure no biomass vars are missed
as.vector(distinct(filter(full_product_kong, category == "bio"), variable))
as.vector(distinct(filter(nuup_GEM, category == "bio"), variable))

# Get all biomass variables
biomass_kong <- filter(full_product_kong, category == "bio",
                       !grepl("biogeochemistry|Norstore", citation, ignore.case = T))
biomass_is <- filter(full_product_is, category == "bio",
                     !grepl("Domaschke|Norstore", citation, ignore.case = T)) # NB: Domaschke should be removed earlier in PG pipeline
biomass_stor <- filter(full_product_stor, category == "bio")# No bio data
biomass_young <- filter(rbind(full_product_young, young_GEM), category == "bio",
                        grepl("Phytoplankton", citation, ignore.case = T)) # This is perhaps rather just species richness data
biomass_disko <- filter(rbind(full_product_disko, disko_GEM), category == "bio") %>% slice(0) # No biomass data
biomass_nuup <- filter(rbind(full_product_nuup, nuup_GEM), category == "bio",
                       !grepl("CTD|Primary|Chlorophyll", citation))
biomass_por <- filter(full_product_por, category == "bio") # No bio data
biomass_EU <- filter(full_product_EU, category == "bio", is.na(lon), variable != "fluor") # No biomass data
biomass_sval <- filter(full_product_sval, category == "bio", is.na(lon))
biomass_green <- filter(full_product_green, category == "bio", is.na(lon))
biomass_nor <- filter(full_product_nor, category == "bio")
clean_biomass <- rbind(biomass_kong, biomass_is, biomass_stor, biomass_young, biomass_disko, biomass_nuup, biomass_por,
                       biomass_EU, biomass_sval, biomass_green, biomass_nor) %>% 
  filter(!variable  %in% c("chlA [µg/l]", "Chla [µg/l]"), !grepl("\\[\\%\\]", variable)) %>% 
  filter(!grepl("blade growth", variable), !grepl("tip growth", variable)) %>% # NB: Decided to remove growth data
  filter(!grepl("\\[presence\\]", variable)) %>% # Presence data used in species richness driver
  mutate(variable = str_replace(variable, "individuals\\/m3", "ind\\/m3"), 
         variable = str_replace(variable, "Biomass - ", ""),
         type = "in situ", driver = "biomass")
# unique(clean_biomass$variable)
rm(biomass_kong, biomass_is, biomass_stor, biomass_young, biomass_disko, biomass_nuup, biomass_por,
   biomass_EU, biomass_sval, biomass_green, biomass_nor); gc()

# Summary analyses
summary_biomass <- review_summary(clean_biomass)

# Plot results
review_summary_plot(summary_biomass, "biomass")


### Species richness ------------------------------------------------------

# This usefulness of this value will be adversely affected by how deep into the taxonomy a researcher has gone in one site vs another
# E.g. by giving all species, or just grouping by a larger taxa
# So don't use these comparisons in the data paper
# Just describe the data

# "Several modelling studies project a decrease of phytoplankton bloom in spring and an increase in cyanobacteria blooms in summer."
# "However, uncertainties remain because some field studies claim that cyanobacteria have not increased and some experimental studies show that responses of cyanobacteria to temperature, salinity and pH vary from species to species. "
# "Warming of seawater in spring also speeds up zooplankton growth and shortens the time lag between phytoplankton and zooplankton peaks, which may lead to decreasing of phytoplankton in spring"
# "In summer, a shift towards smaller-sized zooplankton and a decline of marine copepod species has been projected."
# "In the shallower photic systems, heatwaves may produce eutrophication-like effects, e.g. overgrowth of bladderwrack by epiphytes, due to a trophic cascade."
# "It has also been suggested that in the future climate higher temperatures and less ice will cause an earlier bloom of both diatoms and dinoflagellates, with increased dinoflagellate dominance (Hjerne et al., 2019)."
# "Other studies did not find any explanation for the observed changes in the biovolumes of different taxa, e.g. decrease in diatoms and increase in certain dinoflagellate taxa, 
#  and concluded that phytoplankton community in the Baltic Sea is not in a steady state (Olli et al., 2011), or noted that stochastic dynamics at local scales confound any commonalities between phytoplankton groups (Griffiths et al., 2020)."
# "In general, heatwaves favoured crawling or burrowing predators and suspension feeders, 
# while the abundance of detritivores decreased, suggesting a climate-induced change in dominant zoobenthic traits (Pansch et al., 2018)."

# Test check for all bio vars to make sure no species assemblage vars are missed
as.vector(distinct(filter(full_product_is, category == "bio"), variable))
as.vector(distinct(filter(nuup_GEM, category == "bio"), variable))

# Get all species variables
# This is done by taking the biomass data and counting the different variables on a given day
# This requires a bit of cleaning up of the variable names first
spp_rich_kong <- filter(full_product_kong, category == "bio",
                        !grepl("biogeochemistry|Norstore", citation, ignore.case = T))
spp_rich_is <- filter(full_product_is, category == "bio",
                      !grepl("Domaschke|Norstore", citation, ignore.case = T)) # NB: Domaschke should be removed earlier in PG pipeline
spp_rich_stor <- filter(full_product_stor, category == "bio")# No bio data
spp_rich_young <- filter(rbind(full_product_young, young_GEM), category == "bio",
                         grepl("Phytoplankton", citation, ignore.case = T))
spp_rich_disko <- filter(rbind(full_product_disko, disko_GEM), category == "bio") %>% slice(0) # No species richness data
spp_rich_nuup <- filter(rbind(full_product_nuup, nuup_GEM), category == "bio",
                        !grepl("CTD|Primary|Chlorophyll", citation))
spp_rich_por <- filter(full_product_por, category == "bio") # No bio data
spp_rich_EU <- filter(full_product_EU, category == "bio", is.na(lon))
spp_rich_sval <- filter(full_product_sval, category == "bio", is.na(lon))
spp_rich_green <- filter(full_product_green, category == "bio", is.na(lon))
spp_rich_nor <- filter(full_product_nor, category == "bio")
clean_spp_rich <- rbind(spp_rich_kong, spp_rich_is, spp_rich_stor, spp_rich_young, spp_rich_disko, spp_rich_nuup, spp_rich_por,
                        spp_rich_EU, spp_rich_sval, spp_rich_green, spp_rich_nor) %>% 
  filter(!variable  %in% c("chlA [µg/l]", "Chla [µg/l]")) %>% 
  filter(value > 0) %>% 
         # Fix sp. and spp.
  mutate(variable = str_replace(variable, " sp | spp | spp. ", " sp. "),
         # Remove life stages
         variable = str_replace(variable, " \\(CI\\)| \\(CII\\)| \\(CIII\\)| \\(CIV\\)| \\(CV\\)|
                                | \\(CI-CIII\\)| \\(CI-CV\\)|
                                | - AF| - AM| - CI| - CII| - CIII| - CIV| - CV", ""),
         variable = str_replace(variable, "-CIII|-CV|\\/AM", ""),
         variable = str_replace(variable, "longiremisI|longiremisII|longiremisV", "longiremis"),
         variable = str_replace(variable, " - juvenile| - cypris| - facetotecta| - nauplii| - adult|
                                | - zoea| - veliger| - parasitic nauplii| - larvae| - ova| - medusae|
                                | - megalopa| - pilidium| non.det| not det.", ""),
         # Remove units
         variable = str_replace(variable, " \\[ind\\/m3\\]", ""),
         variable = str_replace(variable, " \\[\\%\\]", ""),
         variable = str_replace(variable, " \\[cells\\/l]", ""),
         variable = str_replace(variable, " \\[individuals\\/m3\\]", ""),
         variable = str_replace(variable, " \\[count\\]", ""),
         variable = str_replace(variable, " 30-40um| 40-50um| 50-60um| 70-80um", ""),
         # Remove other specifications
         variable = str_replace(variable, " \\(veliger\\)| \\(AF\\)| \\(cypris\\)| \\(nauplii\\)|
                                | \\(AF/AM\\)| \\(furcilia\\)| \\(AM\\)| \\(calyptopis\\)| \\(larvae\\)|
                                | \\(secondary larvae\\)| \\(trochophora\\)| \\(metatrochophora\\)| \\(medusae\\)|
                                | \\(mitraria\\)| \\(adult\\)| \\(zoea\\)| \\(megalopa\\)| \\(pilidium\\)|
                                | \\(veliger \\(incl. Margarites and Velutina\\)\\)| \\(coxiella form\\)| \\(GG6\\)|
                                |cyst| > 10um| 3-7um| < 10um|  5-10um|  Non det. 5-10um| >7um| ~3um| 10-20um|
                                | non det.| non det| Non det.| indet.| 1| 2| 3| 4| 5| 30-40um| 20-30um|
                                |0-30um|0-40um| indet.0-50um| cf. normanii| cf. Cerinula - larvae| - larvae", ""),
         # Fix double spacing
         variable = str_replace(variable, "  ", " "),
         # Other small scale fixes
         variable = str_replace(variable, "finmarchicusI|finmarchicusII|finmarchicusV", "finmarchicus"),
         variable = str_replace(variable, "glacialisI|glacialisII|glacialisV", "glacialis"),
         variable = str_replace(variable, "; lt|; gte|;0 gte|; gt", ""),
         variable = str_replace(variable, "sp.0-40um|sp.0-30um", "sp."),
         variable = str_replace(variable, "sp.3", 'sp.')) %>% 
         # Fixes by species
  mutate(variable = case_when(str_detect(variable, "A. nodosum") ~ "A. nodosum", 
                              str_detect(variable, "S. latissima") ~ "S. latissima", 
                              str_detect(variable, "CiliophoraNon") ~ "Ciliophora",
                              str_detect(variable, "Calanus finmarchicus") ~ "Calanus finmarchicus",
                              str_detect(variable, "Calanus glacialis") ~ "Calanus glacialis",
                              str_detect(variable, "Calanus hyperboreus") ~ "Calanus hyperboreus",
                              str_detect(variable, "Metridia longa") ~ "Metridia longa",
                              str_detect(variable, "Metridia lucens") ~ "Metridia lucens",
                              str_detect(variable, "Neoscolecithrix farrani") ~ "Neoscolecithrix farrani",
                              str_detect(variable, "Navicula sp") ~ "Navicula sp.",
                              str_detect(variable, "Nitzschia sp.") ~ "Nitzschia sp.",
                              str_detect(variable, "Pennate.") ~ "Pennate diatoms",
                              str_detect(variable, "Pseudocalanus minutus") ~ "Pseudocalanus minutus",
                              str_detect(variable, "Pseudocalanus acuspes") ~ "Pseudocalanus acuspes",
                              str_detect(variable, "Pseudocalanus sp.") ~ "Pseudocalanus sp.",
                              str_detect(variable, "Scolecithricella minor") ~ "Scolecithricella minor",
                              str_detect(variable, "Polar cod") ~ "Boreogadus saida",
                              TRUE ~ variable)) %>% 
         # More specific fixes
  mutate(variable = case_when(variable == "Acartia longiremisI" ~ "Acartia longiremis", 
                              variable == "AetideidaeV" ~ "Aetideidae", 
                              TRUE ~ variable)) %>% 
        # Remove unidentified things
  mutate(variable = case_when(variable %in% c("Centric diatoms not determined", "centric diatoms not determined",
                                              "Cell 7 domek", "Centric diatoms not det.") ~ as.character(NA),
                              TRUE ~ variable)) %>% 
  mutate(variable = paste0(variable," [presence]"), value = 1,
         driver = "spp rich", type = "in situ") %>% 
  filter(!is.na(variable)) %>% arrange(variable)

# From the cleaned up data create a species count variable
  # From here the species are combined into counts - the names are therefore lost
spp_count <- clean_spp_rich %>% 
  group_by(lon, lat, date, depth, category, driver, site, type) %>% 
  summarise(value = as.numeric(n()), .groups = "drop") %>% 
  mutate(variable = "spp count [n]",
         date_accessed = as.Date(Sys.Date()), 
         URL = "None", 
         citation = "Value derived for FACE-IT dataset") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, driver, site, type, variable, value) %>% 
  distinct()

# Combine and clean up
clean_spp_rich <- rbind(clean_spp_rich, spp_count) %>% distinct()
rm(spp_rich_kong, spp_rich_is, spp_rich_stor, spp_rich_young, spp_rich_disko, spp_rich_nuup, spp_rich_por,
   spp_rich_EU, spp_rich_sval, spp_rich_green, spp_rich_nor); gc()

# Summary analyses
summary_spp_rich <- review_summary(clean_spp_rich)

# Plot results
review_summary_plot(summary_spp_rich, "spp rich")


## Social ------------------------------------------------------------------

# NB: There is quite a lot of data in the social category for provinces/cities etc.
# that are outside of the seven FACE-IT study sites.
# It was unclear what was to be done with these data so they were included in the v1 dataset.
# For future versions we may possibly remoe all data for settlements etc. not within the seven study sites

### Relevant sites

## Norway
# Troms og Finnmark: Province(s) for Porsangerfjorden
# Lakselv: Main city for Porsangerfjorden (?)
# Lakselv Banak + Honningsvåg Valan: Airports on Porsangerfjorden 

## Svalbard
# Svalbard: Province for Svalbard
# Longyearbyen: Main city in Isfjorden
# Svalbard Longyear: Airport on Isfjorden
# Ny-Alesund: Main village in Kongsfjorden

## Greenland
# Sermersooq: Municipality for Nuup Kangerlua
# Nuuk: Main city in Nuup Kangerlua, also an airport
# Qeqertalik: Municipality for Disko bay
# Avannaata: Municipality that borders pn Disko Bay (relevant for demographics, fish landings, etc.)
# Qeqertarsuaq: Main city in Disko Bay (?)
# Aasiaat: Port on southern edge of Disko Bay
# Ilulissat: Port on eastern edge of Disko Bay, also an airport
# Qasigiannguit: Port on eastern edge of Disko Bay
# Uummannaq: City North of Disko Bay (possibly relevant for fish landings etc.)
# Kangaatsiaq: Port south of Disko Bay (possibly relevant for fish landings etc.)
# Outside municipalities: Young Sound appears to fall outside of a municipality


### Governance --------------------------------------------------------------

gov_kong <- review_filter_var(full_product_kong, "gov") %>% slice(0)
gov_is <- review_filter_var(full_product_is, "gov")
gov_stor <- review_filter_var(full_product_stor, "gov")
gov_young <- review_filter_var(rbind(full_product_young, young_GEM), "gov")
gov_disko <- review_filter_var(rbind(full_product_disko, disko_GEM), "gov")
gov_nuup <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "gov")
gov_por <- review_filter_var(full_product_por, "gov")
gov_EU <- filter(full_product_EU, category == "soc")
gov_sval <- filter(full_product_sval, category == "soc")
gov_green <- filter(full_product_green, category == "soc")
gov_nor <- filter(full_product_nor, category == "soc")
clean_gov <- rbind(gov_kong, gov_is, gov_stor, gov_young, gov_disko, gov_nuup, gov_por,
                   gov_EU, gov_sval, gov_green, gov_nor) %>%
  filter(!grepl("Received", URL)) %>%
  filter(!grepl("arrival|guest|Guest|Passengers|passengers|overnight|dogs|
                |Export|Catch|Quota|Advice|price|Coastal|Offshore", variable)) %>% 
  mutate(driver = "gov", type = "in situ") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, driver, site, type, variable, value) %>% 
  distinct()
# unique(clean_gov$variable)
# unique(clean_gov$site)
rm(gov_kong, gov_is, gov_stor, gov_young, gov_disko, gov_nuup, gov_por,
   gov_EU, gov_sval, gov_green, gov_nor); gc()

# Summary analyses
summary_gov <- review_summary(clean_gov)

# Plot results
# Don't run this, too many small variables
# review_summary_plot(summary_gov, "gov")


### Tourism ----------------------------------------------------------------

# Test check for all soc vars to make sure no desired tourism vars are missed
as.vector(distinct(filter(full_product_nor, category == "soc"), variable))
as.vector(distinct(filter(nuup_GEM, category == "soc"), variable))

# Get tourism variables
tourism_kong <- review_filter_var(full_product_kong, "Tourist|Passenger")
tourism_is <- review_filter_var(full_product_is, "Tourist|Calls|Cruise|Pleasure", 
                                "Cargo|Teaching|Fishing|Navy|Polar|Pilot|Other")
tourism_stor <- review_filter_var(full_product_stor, "tour") # No social data
tourism_young <- review_filter_var(rbind(full_product_young, young_GEM), "tour") # No tourism data
tourism_disko <- review_filter_var(rbind(full_product_disko, disko_GEM), "tour") # No tourism data
tourism_nuup <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "tour") # No tourism data
tourism_por <- review_filter_var(full_product_por, "tour") # No tourism data
tourism_EU <- review_filter_var(full_product_EU, "tour")
tourism_sval <- review_filter_var(full_product_sval, "arrival|guest")
tourism_green <- review_filter_var(full_product_green, "dogs|stays|guests|arrival|passenger|capacity")
tourism_nor <- review_filter_var(full_product_nor, "Guest|Passenger")
clean_tourism <- rbind(tourism_kong, tourism_is, tourism_stor, tourism_young, tourism_disko, tourism_nuup, tourism_por,
                       tourism_EU, tourism_sval, tourism_green, tourism_nor) %>% 
  mutate(driver = "tourism") %>% 
  filter(!is.na(value))
# unique(clean_tourism$variable)
rm(tourism_kong, tourism_is, tourism_stor, tourism_young, tourism_disko, tourism_nuup, tourism_por,
   tourism_EU, tourism_sval, tourism_green, tourism_nor); gc()

# Summary analyses
# One variable is throwing an error
summary_tourism <- review_summary(clean_tourism)

# Plot results
# Don't run this, too many small variables
# review_summary_plot(summary_tourism, "tourism")


### Fisheries ---------------------------------------------------------------

# NB: Ship traffic is included here as it is mostly due to industry and not tourism

# Test check for all soc vars to make sure no desired fisheries vars are missed
as.vector(distinct(filter(full_product_nor, category == "soc"), variable))
as.vector(distinct(filter(nuup_GEM, category == "soc"), variable))

# Get shipping variables
fisheries_kong <- review_filter_var(full_product_kong, "Vessels", "Passenger|Pleasure")
fisheries_is <- review_filter_var(full_product_is, "trips|gross|berths|nautical|duration|fuel|power|emissions|tonnage|calls",
                                 "Cruise|Tourist|Day trip|Pleasure")
fisheries_stor <- review_filter_var(full_product_stor, "trips|gross|berths|nautical|duration|fuel|power|emissions|tonnage|calls")
fisheries_young <- review_filter_var(rbind(full_product_young, young_GEM), "trips") # No social data
fisheries_disko <- review_filter_var(rbind(full_product_disko, disko_GEM), "trips") # No social data
fisheries_nuup <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "trips") # No social data
fisheries_por <- review_filter_var(full_product_por, "trips") # No social data
fisheries_EU <- review_filter_var(full_product_EU, "fish")
fisheries_sval <- review_filter_var(full_product_sval, "fish")
fisheries_green <- review_filter_var(full_product_green, "Export|Catch|Quota|Advice|price|Coastal|Offshore")
fisheries_nor <- review_filter_var(full_product_nor, "Export")
clean_fisheries <- rbind(fisheries_kong, fisheries_is, fisheries_stor, fisheries_young, fisheries_disko, fisheries_nuup, fisheries_por,
                         fisheries_EU, fisheries_sval, fisheries_green, fisheries_nor) %>% 
  filter(!grepl("\\[Month Trips\\]", variable)) %>% # NB: It is unclear what exactly these are
  mutate(variable = case_when(str_detect(variable, "CO2 emissions \\(tonnes\\)") ~ "CO2 emissions total [tonnes; sum]",
                              str_detect(variable, "\\[Nautical miles\\]") ~ "nautical miles [sum]",
                              str_detect(variable, "Duration \\(hours\\)") ~ "duration [hours; sum]",
                              str_detect(variable, "Duration in port \\(hours\\)") ~ "duration in port [hours; sum]",
                              str_detect(variable, "Fuel \\(tonnes\\)") ~ "Total fuel [tonnes; sum]",
                              str_detect(variable, "Fuel in port \\(tonnes\\)") ~ "Fuel in port [tonnes; sum]",
                              str_detect(variable, "Fuel propulsion \\(tonnes\\)") ~ "Fuel propulsion [tonnes; sum]",
                              str_detect(variable, "NOx emissions total \\(tonnes\\)") ~ "NOx emissions total [tonnes; sum]",
                              str_detect(variable, "NOx emissions in port\\(tonnes\\)") ~ "NOx emissions in port [tonnes; sum]",
                              str_detect(variable, "SOx emissions total \\(tonnes\\)") ~ "SOx emissions total [tonnes; sum]",
                              str_detect(variable, "SOx emissions in port \\(tonnes\\)") ~ "SOx emissions in port [tonnes; sum]",
                              str_detect(variable, "PM emissions total \\(tonnes\\)") ~ "PM emissions total [tonnes; sum]",
                              str_detect(variable, "PM emissions in port \\(tonnes\\)") ~ "PM emissions in port [tonnes; sum]",
                              str_detect(variable, "Power \\(GWh\\)") ~ "Power total [GWh; sum]",
                              str_detect(variable, "Power in port \\(GWh\\)") ~ "Power in port [tonnes; sum]",
                              str_detect(variable, "\\[Number of trips pr year\\]") ~ "trips [n]",
                              TRUE ~ variable), 
         driver = "fisheries", category = "soc") %>% arrange(variable)
# unique(clean_fisheries$category)
# unique(clean_fisheries$driver)
# unique(clean_fisheries$variable)
rm(fisheries_kong, fisheries_is, fisheries_stor, fisheries_young, fisheries_disko, fisheries_nuup, fisheries_por,
   fisheries_EU, fisheries_sval, fisheries_green, fisheries_nor); gc()

# Summary analyses
summary_fisheries <- review_summary(clean_fisheries)

# Plot results
# Don't plot this, too many variables
# review_summary_plot(summary_fisheries, "fisheries")


## Save clean data ---------------------------------------------------------

# Combine and select columns to match final standard
clean_all <- rbind(clean_sea_ice, clean_glacier, clean_runoff,
                   clean_sea_temp, clean_sal, clean_light,
                   clean_carb, clean_nutrients,
                   clean_pp, clean_biomass, clean_spp_rich,
                   clean_gov, clean_tourism, clean_fisheries) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value)

# NB: Temporarily adding ECC species data here
# This will need to be incorporated at the normal location once the pipeline is functional
load("~/pCloudDrive/FACE-IT_data/EU_arctic/EU_species.RData")
load("~/pCloudDrive/FACE-IT_data/svalbard/sval_species.RData")
load("~/pCloudDrive/FACE-IT_data/kongsfjorden/kong_species.RData")
load("~/pCloudDrive/FACE-IT_data/isfjorden/is_species.RData")
load("~/pCloudDrive/restricted_data/GEM/nuup/nuup_species_GEM.RData")
load("~/pCloudDrive/restricted_data/GEM/young/young_species_GEM.RData")
clean_all <- rbind(clean_all, 
                   EU_species, sval_species,
                   kong_species, is_species,
                   nuup_species_GEM, young_species_GEM) |> 
  distinct()

# Save all data in one file
save(clean_all, file = "data/analyses/clean_all.RData")

# Save all data by driver/site
save_data(df = clean_all, data_type = "clean")


## References --------------------------------------------------------------

# TODO: Correct automagic reference classification for governance data
# NB: Check for N-ICE and remove if present
all_ref <- bind_rows(summary_sea_ice$citations, summary_glacier$citations, summary_runoff$citations,
                     summary_sea_temp$citations, summary_sal$citations, summary_light$citations,
                     summary_carb$citations, summary_nutrients$citations, 
                     summary_pp$citations, summary_biomass$citations, summary_spp_rich$citations, 
                     summary_gov$citations, summary_tourism$citations, summary_fisheries$citations)
all_ref[grepl("N-ICE", all_ref$citation),]
save(all_ref, file = "data/analyses/all_ref.RData")


## Summary -----------------------------------------------------------------

# Combine analysed data
all_meta <- rbind(summary_sea_ice$monthly, summary_glacier$monthly, summary_runoff$monthly,
                  summary_sea_temp$monthly, summary_sal$monthly, summary_light$monthly,
                  summary_carb$monthly, summary_nutrients$monthly, 
                  summary_pp$monthly, summary_biomass$monthly, summary_spp_rich$monthly, 
                  summary_gov$monthly, summary_tourism$monthly, summary_fisheries$monthly)
save(all_meta, file = "data/analyses/all_meta.RData")
# load("data/analyses/all_meta.RData")

# Too many small variables at the moment
# all_meta %>% 
#   filter(!is.na(value_mean)) %>% 
#   mutate(month = lubridate::month(date)) %>% 
#   ggplot(aes(x = as.factor(month), y = count_days_name)) +
#   geom_boxplot(aes(fill = site), position = "dodge", outlier.colour = NA) +
#   geom_jitter(aes(colour = log10(count))) +
#   scale_colour_viridis_c() + scale_y_continuous(limits = c(0, 32), expand = c(0, 0)) +
#   labs(y = paste0("Unique days with data points"), x = "Month", fill = "Site", colour = "Count [log10(n)]") +
#   facet_grid(site~driver) +
#   theme(panel.border = element_rect(colour = "black", fill = NA))
# ggsave("~/Desktop/analyses_output/meta_meta_box.png", width = 16, height = 12)

# annual_temp <- MUR_data %>%
#   mutate(year = lubridate::year(t)) %>% 
#   group_by(lon, lat, year) %>%
#   summarise(annual_minn_temp = min(temp, na.rm = T),
#             annual_mean_temp = mean(temp, na.rm = T),
#             annual_max_temp = max(temp, na.rm = T), .groups = "drop")


## PANGAEA file ------------------------------------------------------------

# Load all clean data
# clean_all <- map_dfr(dir("data/full_data", pattern = "clean", full.names = T), read_csv)
if(!exists("clean_all")) load("data/analyses/clean_all.RData")

# Leave an NA shadow so users know the data exist and where to find them
data_shadow <- "g-e-m|GRDC|Received directly from Mikael Sejr"
data_shadow_df <- filter(clean_all, grepl(data_shadow, URL)) |> 
  mutate(lon = as.numeric(NA), lat = as.numeric(NA), 
         date = as.Date(NA), depth = as.numeric(NA), value = as.numeric(NA)) |> 
  mutate(variable = case_when(driver %in% c("biomass", "spp rich") ~ as.character(NA), TRUE ~ variable)) |> 
  distinct()

# Prep for PANGAEA standard
FACE_IT_v1.1 <- clean_all |> 
  # Remove shadow data
  filter(!grepl(data_shadow, URL)) |> 
  # Convert to PANGAEA date standard
  rbind(data_shadow_df) |> 
  dplyr::rename(`date/time [UTC+0]` = date, `depth [m]` = depth,
                `longitude [°E]` = lon, `latitude [°N]` = lat) |> 
  mutate(`date/time [UTC+0]` = paste0(`date/time [UTC+0]`,"T00:00:00"),
         citation = str_replace_all(citation, ";", "."))

# Double check data shadows have been applied correctly
shadow_test <- filter(FACE_IT_v1.1, grepl(data_shadow, URL))
rm(shadow_test); gc()

# Save as .csv
write_csv_arrow(FACE_IT_v1.1, "~/pCloudDrive/FACE-IT_data/FACE_IT_v1.1.csv")
write_csv_arrow(FACE_IT_v1.1, "data/full_data/FACE_IT_v1.1.csv")

# Cryo data
FACE_IT_v1.1_cryo <- filter(FACE_IT_v1.1, category == "cryo") %>% pivot_wider(names_from = variable, values_from = value)
write_delim(FACE_IT_v1.1_cryo, "~/pCloudDrive/FACE-IT_data/FACE_IT_v1.1_cryo.csv", delim = ";")

# Phys data
FACE_IT_v1.1_phys <- filter(FACE_IT_v1.1, category == "phys") %>% pivot_wider(names_from = variable, values_from = value, values_fn = mean)
write_delim(FACE_IT_v1.1_phys, "~/pCloudDrive/FACE-IT_data/FACE_IT_v1.1_phys.csv", delim = ";")

# Chem data
FACE_IT_v1.1_chem <- filter(FACE_IT_v1.1, category == "chem") %>% pivot_wider(names_from = variable, values_from = value)
write_delim(FACE_IT_v1.1_chem, "~/pCloudDrive/FACE-IT_data/FACE_IT_v1.1_chem.csv", delim = ";")

# Bio data
FACE_IT_v1.1_bio <- filter(FACE_IT_v1.1, category == "bio") %>% pivot_wider(names_from = variable, values_from = value, values_fn = mean)
write_delim(FACE_IT_v1.1_bio, "~/pCloudDrive/FACE-IT_data/FACE_IT_v1.1_bio.csv", delim = ";")

# Soc data
FACE_IT_v1.1_soc <- filter(FACE_IT_v1.1, category == "soc") %>% pivot_wider(names_from = variable, values_from = value, values_fn = mean)
write_delim(FACE_IT_v1.1_soc, "~/pCloudDrive/FACE-IT_data/FACE_IT_v1.1_soc.csv", delim = ";")


# Section 4 ---------------------------------------------------------------
# Relationships between data analysed for Section 3
# NB: Only necessary to run the `Setup` section

# Load all clean data
# clean_all <- map_dfr(dir("data/full_data", pattern = "clean", full.names = T), read_csv)
if(!exists("clean_all")) load("data/analyses/clean_all.RData")

# Clean/remove some variables for better comparisons
clean_all_cryo <- filter(clean_all, category == "cryo") %>% 
  filter(!str_detect(variable, "cover Jan|cover Feb|cover Mar|cover Apr|cover May|cover Jun|
                     |cover Jul|cover Aug|cover Sep|cover Oct|cover Nov|cover Dec|
                     |end date|start date|Snow|snow|
                     |_1936|_1990|_2010")) # Add these back in if possible
clean_all_phys <- filter(clean_all, category == "phys") %>% 
  filter(!grepl("UV-A", variable)) %>% # Don't want this for comparisons... maybe remove completely...
  filter(!grepl("emissions", variable)) # Removing ship emissions for the moment
clean_all_chem <- filter(clean_all, category == "chem") %>% 
  filter(!grepl("pH \\[unknown scale\\]", variable)) %>% # Only want known pH scale data for comparisons
  filter(!grepl("emissions", variable)) # Removing ship emissions for the moment
clean_all_biomass <- filter(clean_all, driver == "biomass") %>% # Just get sum of all species counts per sample... not ideal
  mutate(variable = case_when(str_detect(variable, "ind\\/m3") ~ "spp count [ind/m3]", 
                              str_detect(variable, "cells\\/l") ~ "spp count [cells/l]",
                              TRUE ~ variable)) %>% 
  group_by(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth) %>% 
  summarise(value = sum(value, na.rm = T), .groups = "drop")
clean_all_bio <- filter(clean_all, category == "bio") %>% 
  filter(driver == "prim prod" |variable == "spp count [n]") %>% # Remove everything except the species count value created above 
  rbind(clean_all_biomass); rm(clean_all_biomass)
clean_all_gov <- filter(clean_all, driver == "gov") %>% 
  mutate(variable = case_when(str_detect(variable, "Population - ") ~ "Population [n]",
                              str_detect(variable, "Taxable income - ") ~ "Taxable income [DKK]",
                              str_detect(variable, "All industries - main employment - Total") ~ "Employement [n/month]",
                              str_detect(variable, "All industries - income - Total") ~ "Income [DKK/month]",
                              str_detect(variable, "Unemployed - Total ") ~ "Unemployed [n]",
                              str_detect(variable, "pop \\[Longyearbyen & Ny-Alesund") ~ "Population [n]",
                              str_detect(variable, "pop \\[Barentsburg and Pyramiden") ~ "Population [n]",
                              str_detect(variable, "pop \\[Hornsund") ~ "Population [n]",
                              TRUE ~ variable)) %>% 
  filter(!str_detect(variable, "Taxable income - |- main employment| - income - |Unemployed - |Employment - ")) %>%
  filter(!is.na(variable), !is.na(value)) %>% 
  group_by(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth) %>% 
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>% arrange(variable)
clean_all_tourism <- filter(clean_all, driver == "tourism") %>% 
  mutate(variable = case_when(variable %in% c("Calls [Cruise boats (overseas)]", "Calls [Tourist boats (expedition cruise)]",
                                              "Calls [Day trip boats (local boats)]", "Calls [Day trip boats (12 PAX RIB mm)]",
                                              "Calls [Pleasure boats (Sail charter engine)]") ~ "Calls - tourism [n]",
                              variable == "Guest nights - Total - Total [n]" ~ "Guest nights [n]",
                              variable == "Cruise capacity (Total) arrivals [n]" ~ "Cruise capacity arrivals [n]",
                              variable == "Cruise capacity (Total) passengers [n]" ~ "Cruise capacity passengers [n]",
                              str_detect(variable, "Passengers") ~ "Passengers [n]",
                              str_detect(variable, "arrival") ~ "Arrivals [n]",
                              str_detect(variable, "Days in port") ~ "Days in port - tourism [count]",
                              str_detect(variable, "guest night ") ~ "Guest nights [n]",
                              str_detect(variable, "Vessels ") ~ "Vessels [n]",
                              TRUE ~ variable)) %>% 
  # Remove some unwanted ship values like duration in the fjord
  filter(!str_detect(variable, "Duration|duration|Fuel|gross weight|Month trips|emissions|
                     |Number of trips pr year|Average speed|Power|Total fuel|Tonnage|
                     |Month Trips|Number of ships|(annual)|Cruise passengers - |
                     |Guest nights - |Cruise capacity \\(|- total \\[n\\]")) %>% # These are annual values or are otherwise accounted for
  # Select sum rather than mean values
  mutate(variable = case_when(str_detect(variable, "mean") ~ as.character(NA),
                              str_detect(variable, "; sum") ~ gsub("; sum", "", variable),
                              TRUE ~ variable)) %>% 
  filter(!is.na(variable), !is.na(value)) %>% 
  group_by(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth) %>% 
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>% arrange(variable)
clean_all_fisheries <- filter(clean_all, driver == "fisheries") %>% 
  mutate(variable = case_when(variable %in% c("Calls [Cargo boats]", "Calls [Teaching / research]",
                                              "Calls [Fishing boats]", "Calls [Navy / Coast Guard]",
                                              "Calls [Polar / Nordsyssel]", "Calls [Pilot boat]",
                                              "Calls [Other vessels]") ~ "Calls - commercial [n]",
                              variable == "Export -  total [1,000 DKK]" ~ "Export [1,000 DKK]",
                              variable == "Export -  total [Tonnes]" ~ "Export [Tonnes]",
                              str_detect(variable, "Advice - ") ~ "Advice [pieces]",
                              str_detect(variable, "Quarter ") ~ as.character(NA),
                              str_detect(variable, " - Coastal \\[1,000 DKK\\]") ~ "Landings - coastal [1,000 DKK]",
                              str_detect(variable, " - Coastal \\[Tonnes\\]") ~ "Landings - coastal [Tonnes]",
                              str_detect(variable, " - Offshore \\[1,000 DKK\\]") ~ "Landings - offshore [1,000 DKK]",
                              str_detect(variable, " - Offshore \\[Tonnes\\]") ~ "Landings - offshore [Tonnes]",
                              str_detect(variable, "Average kilo price - ") ~ "Average kilo price [index]",
                              str_detect(variable, "Quota - ") ~ "Quota [Tonnes]",
                              str_detect(variable, "Catch - ") ~ "Catch [pieces]",
                              str_detect(variable, "Vessels ") ~ "Vessels [n]",
                              TRUE ~ variable)) %>% 
  # Remove some unwanted ship values like duration in the fjord
  filter(!str_detect(variable, "Duration|duration|Fuel|gross weight|Month trips|emissions|
                     |Number of trips pr year|Average speed|Power|Total fuel|Tonnage|
                     |Month Trips|Number of ships|Export - ")) %>% # These are annual values or are otherwise accounted for
  # Select sum rather than mean values
  mutate(variable = case_when(str_detect(variable, "mean") ~ as.character(NA),
                              str_detect(variable, "; sum") ~ gsub("; sum", "", variable),
                              TRUE ~ variable)) %>% 
  filter(!is.na(variable), !is.na(value)) %>% 
  group_by(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth) %>% 
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>% arrange(variable)

# Combine
clean_all_clean <- clean_all_cryo %>% 
  rbind(clean_all_phys) %>% 
  rbind(clean_all_chem) %>%
  rbind(clean_all_bio) %>% 
  rbind(clean_all_gov) %>% 
  rbind(clean_all_tourism) %>% 
  rbind(clean_all_fisheries)
rm(clean_all_cryo, clean_all_phys, clean_all_chem, clean_all_bio, clean_all_gov, clean_all_tourism, clean_all_fisheries); gc()
save(clean_all_clean, file = "data/analyses/clean_all_clean.RData")

# Load cleaned up clean data
if(!exists("clean_all_clean")) load("data/analyses/clean_all_clean.RData")

### Relationships from the network analysis - created via the review paper
# We want to see which sites have what relationships, and if there are any obvious outliers
# This is one of the main points that will feed back into the review paper
# NB: These have laready been run and can be loaded below as `driver_alll`

# List of drivers and variables
unique(clean_all_clean$driver)
unique(clean_all_clean$variable)
table(clean_all_clean$driver, clean_all_clean$variable)

## Cryosphere
ice_temp <- driver2_lm("sea ice", "sea temp") # sea ice -> sea temp
ice_light <- driver2_lm("sea ice", "light") # sea ice -> light
ice_biomass <- driver2_lm("sea ice", "biomass") # sea ice -> biomass
ice_spp <- driver2_lm("sea ice", "spp rich") # sea ice -> spp richness
gmb_runoff <- driver2_lm("glacier", "runoff") # gmb -> runoff
gmb_spp <- driver2_lm("glacier", "spp rich") # gmb -> spp richness
runoff_temp <- driver2_lm("runoff", "sea temp") # runoff -> sea temp
runoff_sal <- driver2_lm("runoff", "salinity") # runoff -> salinity
runoff_light <- driver2_lm("runoff", "light") # runoff -> light
runoff_carb <- driver2_lm("runoff", "carb") # runoff -> carb system
runoff_nut <- driver2_lm("runoff", "nutrients") # runoff -> nutrients
gc()

## Physics
temp_ice <- driver2_lm("sea temp", "sea ice") # sea temp -> sea ice
temp_spp <- driver2_lm("sea temp", "spp rich") # sea temp -> spp richness
temp_biomass <- driver2_lm("sea temp", "biomass") # sea temp -> biomass
temp_PP <- driver2_lm("sea temp", "prim prod") # sea temp -> PP
sal_spp <- driver2_lm("salinity", "spp rich") # salinity -> spp richness
sal_biomass <- driver2_lm("salinity", "biomass") # salinity -> biomass
light_spp <- driver2_lm("light", "spp rich") # light -> spp richness
light_biomass <- driver2_lm("light", "biomass") # light -> biomass
light_PP <- driver2_lm("light", "prim prod") # light -> PP
gc()

## Chemistry
carb_spp <- driver2_lm("carb", "spp rich") # carb system -> spp richness
nut_PP <- driver2_lm("nutrients", "prim prod") # nutrients -> PP
gc()

## Biology
PP_biomass <- driver2_lm("prim prod", "biomass") # PP -> biomass
biomass_spp <- driver2_lm("biomass", "spp rich") # biomass -> spp richness
gc()

## Social
gov_tour <- driver2_lm("gov", "tourism") # governance -> tourism # No governance data
gov_fish <- driver2_lm("gov", "fisheries") # governance -> fisheries # No governance data
tour_nut <- driver2_lm("tourism", "nutrients") # tourism -> nutrients
fish_biomass <- driver2_lm("fisheries", "biomass") # fisheries -> biomass
fish_spp <- driver2_lm("fisheries", "spp rich") # fisheries -> spp richness
gc()

# Look across sites for differences in r2 values for variables that are shared between sites
driver_all <- rbind(ice_temp, ice_light, ice_biomass, ice_spp, gmb_runoff, gmb_spp, 
                    runoff_temp, runoff_sal, runoff_light, runoff_carb, runoff_nut,
                    temp_ice, temp_spp, temp_biomass, temp_PP, sal_spp, sal_biomass, light_spp, light_biomass, light_PP,
                    carb_spp, nut_PP, 
                    PP_biomass, biomass_spp,
                    gov_tour, gov_fish, tour_nut, fish_biomass, fish_spp) %>% distinct()
save(driver_all, file = "data/analyses/driver_all.RData")

# Load relationship data
if(!exists("driver_all")) load(file = "data/analyses/driver_all.RData")

# Quick fix for plotting
driver_all_asym <- asymmetrize(driver_all, variable, variable_y) %>% 
  mutate(nobs = replace_na(nobs, 0))

# Visualise one site
filter(driver_all, site == "kong") %>% 
  asymmetrize(variable, variable_y) %>% 
  # mutate(nobs = replace_na(nobs, 0)) %>% # Weird behaviour...
  ggplot(aes(x = variable, y = variable_y)) +
  geom_asymmat(aes(fill_tl = rsq, fill_br = pval, fill_diag = slope), na.rm = TRUE) +
  scale_fill_tl_gradient2(low = "blue", mid = "white", high = "red") +
  scale_fill_br_gradient(low = "black", high = "white") +
  scale_fill_diag_gradient2(low = "blue", mid = "white", high = "red") +
  facet_wrap(~depth) +
  # facet_grid(depth.y~site) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Beefy asymetry plot of all sites at once - not terribly helpful other than to show the lack of relationships
ggplot(driver_all_asym, aes(x = variable, y = variable_y)) +
  geom_asymmat(aes(fill_tl = rsq, fill_br = pval, fill_diag = slope)) +
  scale_fill_tl_gradient(low = "lightpink", high = "tomato") +
  scale_fill_br_gradient(low = "lightblue1", high = "dodgerblue") +
  scale_fill_diag_gradient(low = "yellow", high = "orange3") +
  # facet_wrap(~site, nrow = 2)
  facet_grid(depth~site)

# Get count of comparisons by site to see which can be made across most sites
driver_all_site_count <- driver_all %>% 
  group_by(type, type_y, driver, driver_y, variable, variable_y, depth, depth_y) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  filter(count >= 3, !is.na(type_y))

# Filter out combos with only one site available
driver_all_filter <- driver_all %>% 
  right_join(driver_all_site_count)

# Heatmap of relationships by site
# These are possibly of interest
unique(driver_all_filter$driver)
unique(driver_all_filter$driver_y)
driver_all_filter %>% 
  filter(driver == "sea temp") %>%
  filter(variable_y != "PAR [µmol m-2 s-1]") %>% # Investigate why these values are so high
  filter(variable_y != "NO2 [µmol/l]") %>%  # Investigate why these values are so low
  unite(variable_x_y, c(variable, variable_y)) %>%
  ggplot(aes(x = variable_x_y, y = site)) +
  # unite(variable_depth_x_y, c(variable.x, depth.x, variable.y, depth.y)) %>%
  # ggplot(aes(x = variable_depth_x_y, y = site)) +
  geom_tile(aes(fill = slope)) +
  # geom_tile(aes(fill = rsq)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  # facet_wrap(~count, scales = "free_x") +
  # facet_grid(count~driver.y, scales = "free_x") +
  # facet_grid(~driver.y, scales = "free_x") +
  facet_grid(depth~depth_y, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# TODO: Look into very deep PAR data
# TODO: Look into dichotomy of Q and ablation for disko vs young
# TODO: Look into differences between PAR and Chla/Spp count for nuup vs young
# TODO: Look into funny relationship between Open water [annual days] and temp [°C] in Young Sound
# TODO: Massive negative relationship between temp and spp count at Young sound

# Get monthly means by depth across entire site
df_mean_month_depth <- clean_all_clean %>% 
  dplyr::select(type, site, driver, variable, date, depth, value) %>% 
  filter(!is.na(date)) %>% distinct() %>% 
  mutate(date = lubridate::round_date(date, unit = "month"),
         depth = case_when(depth < 0 ~ "land",
                           is.na(depth) ~ "surface",
                           depth <= 10 ~ "0 to 10",
                           depth <= 50 ~ "10 to 50",
                           depth <= 200 ~ "50 to 200",
                           depth > 200 ~ "+200")) %>%
  group_by(type, site, driver, variable, date, depth) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop") %>% 
  filter(!is.na(value)); gc()

# Get two specific datasets to compare
unique(clean_all_clean$variable)
df_1 <- df_mean_month_depth %>% 
  filter(variable == "temp [°C]", depth == "0 to 10", site == "stor", type == "in situ")
df_2 <- df_mean_month_depth %>% 
  filter(variable == "sea ice cover [proportion]", depth == "surface", site == "stor")
df_3 <- left_join(df_1, df_2, by = c("site", "date")) %>% filter(!is.na(value.x), !is.na(value.y))
broom::glance(lm(value.x ~ value.y, data = df_3))
ggplot(data = df_3, aes(x = value.x, y = value.y)) +
  geom_point() + geom_smooth(method = "lm")


# Section 5 ---------------------------------------------------------------
# Future projections of data analysed for Section 3 and relationships from Section 4
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

# Load cleaned up clean data
if(!exists("clean_all_clean")) load("data/analyses/clean_all_clean.RData")

# Load relationship data
if(!exists("driver_all")) load(file = "data/analyses/driver_all.RData")

# Morten model data
## NB: Nit = Nitrate 
model_kong <- load_model("kongsfjorden_rcp")
model_is <- load_model("isfjorden_rcp")
model_stor <- load_model("storfjorden_rcp")
model_young <- load_model("young_sound_rcp")
model_por <- load_model("porsangerfjorden_rcp")
# model_trom <- load_model("tromso_rcp")

# Convert data to even grid
# NB: Not necessary to create spatial average of entire fjord
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

# Get the RMSE between data and model data
# Also average average decadal trends for the pixels within the bounding box of each site
model_kong_stats <- model_bbox_stats(model_kong, "kong")
model_is_stats <- model_bbox_stats(model_is, "is")
model_stor_stats <- model_bbox_stats(model_stor, "stor")
model_young_stats <- model_bbox_stats(model_young, "young")
model_por_stats <- model_bbox_stats(model_por, "por")
model_ALL_stats <- rbind(model_kong_stats, model_is_stats, model_stor_stats,
                        model_young_stats, model_por_stats)
rm(model_kong_stats, model_is_stats, model_stor_stats,
   model_young_stats, model_por_stats); gc()
save(model_ALL_stats, file = "data/analyses/model_ALL_stats.RData")
if(!exists("model_ALL_stats")) load("data/analyses/model_ALL_stats.RData")

# Get historic slopes as these tend to be higher than RCP 8.5
historic_trend <- driver_all %>% 
  filter(is.na(variable_y)) %>% 
  mutate(hist_trend = slope*120) %>% 
  dplyr::select(site, type, category, driver, variable, depth, hist_trend)

# Get the product of the historic relationship between drivers and the projected change in the independent driver
future_stats <- driver_all %>% 
  filter(!is.na(variable_y)) %>% 
  left_join(model_ALL_stats, by = c("site", "variable", "depth")) %>% 
  left_join(historic_trend, by = c("site", "type", "category", "driver", "variable", "depth")) %>% 
  filter(!is.na(`RCP 2.6`)) %>% 
  mutate(change_hist = slope*(hist_trend*8),
         change_2.6 = slope*(`RCP 2.6`*8),
         change_4.5 = slope*(`RCP 4.5`*8),
         change_8.5 = slope*(`RCP 8.5`*8)) %>% 
  mutate(mean_hist = mean_val+change_hist,
         mean_2.6 = mean_val+change_2.6,
         mean_4.5 = mean_val+change_4.5,
         mean_8.5 = mean_val+change_8.5)

# Boxplots to explore results
future_stats %>% 
  dplyr::select(site:depth_y, mean_val, mean_hist:mean_8.5) %>% 
  pivot_longer(cols = mean_val:mean_8.5) %>% 
  mutate(name = factor(name, levels = c("mean_val", "mean_hist", "mean_2.6", "mean_4.5", "mean_8.5"))) %>% 
  # QC
  mutate(value = case_when(variable_y == "sea ice cover [proportion]" & value < -1 ~ as.numeric(NA), 
                           variable_y == "Chla [µg/l]" & value < -3 ~ as.numeric(NA),
                           variable_y == "Chla [µg/l]" & value > 3 ~ as.numeric(NA),
                           TRUE ~ value)) %>% 
  #
  ggplot(aes(x = name, y = value)) +
  geom_boxplot(aes(fill = variable), outlier.colour = NA) +
  geom_jitter(aes(colour = site)) +
  facet_wrap(~variable_y, scales = "free_y") +
  scale_fill_brewer("Driver", palette = "Dark2") +
  # facet_grid(depth~variable_y, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 30))


# Figure 1 ----------------------------------------------------------------
# Map of the study area that also manages to show SST, ice cover, and any other well covered drivers 
# NB: Only necessary to run the `Setup` section

# EU bbox
bbox_EU_poly <- bbox_to_poly(bbox_EU, "EU")

# EU Arctic land shapes
coastline_Arctic <- filter(coastline_full_df, y > 50, x < 90, x > -90)

# Study sites
site_points <- data.frame(site = factor(x = c("Kongsfjorden", "Isfjorden", "Storfjorden", 
                                              "Young Sound", "Qeqertarsuup Tunua", "Nuup Kangerlua", 
                                              "Porsangerfjorden"),
                                        levels = c("Kongsfjorden", "Isfjorden","Storfjorden", 
                                                   "Young Sound", "Qeqertarsuup Tunua", "Nuup Kangerlua", 
                                                   "Porsangerfjorden")),
                          lon = c(11.845, 14.365, 19.88, -21.237, -52.555, -50.625, 25.75),
                          lat = c(78.98, 78.235, 77.78, 74.517, 69.36, 64.405, 70.6))

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
ggsave("~/Desktop/analyses_output/fig_1_a.png", fig_1_a, width = 10, height = 7)

# Side panels: ice cover trends by site (days of year of ice cover)
(fig_1_b_kong <- ice_trend_grid_plot("Kongsfjorden", 0.03, check_conv = F) + 
    coord_quickmap(xlim = c(11.1, 12.6), ylim = c(78.89, 79.09), expand = F))
(fig_1_b_is <- ice_trend_grid_plot("Isfjorden", 0.04, check_conv = F))
(fig_1_b_stor <- ice_trend_grid_plot("Storfjorden", 0.04, check_conv = F))
(fig_1_b_young <- ice_trend_grid_plot("Young Sound", 0.04, check_conv = F, lat_nudge = 0.025))
(fig_1_b_disko <- ice_trend_grid_plot("Qeqertarsuup Tunua", 0.05, check_conv = F))
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
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
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
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.box.background = element_rect(fill = NA, colour = "black"))
ice_legend <- ggpubr::get_legend(ice_legend)

# Create base for layering plots
base_df <- data.frame(x = c(-1, 0, 1), y = c(-1, 0, 1))
fig_1_base <- ggplot(data = base_df, aes(x = x, y = y)) + 
  geom_point(colour = "white", size = 0.0001) +
  scale_x_continuous(breaks = seq(-1, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1)) +
  theme_void()

# TODO: Updates to packages messed up this figure.
# Need to fix spacing and segment colours

# Combine
fig_1 <- fig_1_base +
  # EU Arctic
  geom_grob(aes(x = 0, y = 0, label = list(cowplot::as_grob(fig_1_a))), vp.width = 0.7, vp.height = 0.7, 
            nudge_x = -0.033, add.segments = F) +
  # Kongsfjorden
  geom_grob(aes(x = 0.065, y = 0.16, label = list(cowplot::as_grob(fig_1_b_kong))),
            nudge_x = -0.07, nudge_y = 0.55, default.colour = "chocolate4") +
  # Isfjorden
  geom_grob(aes(x = 0.08, y = 0.15, label = list(cowplot::as_grob(fig_1_b_is))),
            nudge_x = 0.35, nudge_y = 0.6, vp.width = 0.25, vp.height = 0.25, default.colour = "chocolate3") +
  # Storfjorden
  geom_grob(aes(x = 0.12, y = 0.15, label = list(cowplot::as_grob(fig_1_b_stor))),
            nudge_x = 0.65, nudge_y = 0.03, vp.width = 0.25, vp.height = 0.25, default.colour = "chocolate1") +
  # Young Sound
  geom_grob(aes(x = -0.15, y = 0.05, label = list(cowplot::as_grob(fig_1_b_young))),
            nudge_x = -0.41, nudge_y = 0.53, vp.width = 0.20, vp.height = 0.20, default.colour = "springgreen4") +
  # Disko bay
  geom_grob(aes(x = -0.45, y = 0.11, label = list(cowplot::as_grob(fig_1_b_disko))),
            nudge_x = -0.41, nudge_y = -0.03, vp.width = 0.25, vp.height = 0.25, default.colour = "springgreen3") +
  # Nuup Kangerlua
  geom_grob(aes(x = -0.55, y = -0.03, label = list(cowplot::as_grob(fig_1_b_nuup))),
            nudge_x = -0.26, nudge_y = -0.4, vp.width = 0.25, vp.height = 0.25, default.colour = "springgreen1") +
  # Porsangerfjorden
  geom_grob(aes(x = 0.23, y = -0.05, label = list(cowplot::as_grob(fig_1_b_por))),
            nudge_x = 0.5, nudge_y = -0.46, vp.width = 0.3, vp.height = 0.3, default.colour = "plum4") +
  # Temperature legend
  geom_grob(aes(x = -0.1, y = -0.74, label = list(cowplot::as_grob(temp_legend)))) +
  # Ice legend
  geom_grob(aes(x = 0.0, y = -0.94, label = list(cowplot::as_grob(ice_legend))))
# fig_1
ggsave("figures/dp_fig_1.png", fig_1, width = 12, height = 10)


# Figure 2 ----------------------------------------------------------------
# Square treemap plots showing count of datasets and count of data points
# NB: Must run Setup to get necessary objects

# Load cleaned up clean data
if(!exists("clean_all_clean")) load("data/analyses/clean_all_clean.RData")

# Create frequency data.frame of datasets for category -> driver -> variable
data_point_freq <- clean_all_clean %>% 
  filter(type == "in situ") %>% 
  group_by(category, driver, variable) %>% 
  summarise(freq = n(), .groups = "drop")
data_set_freq <- clean_all_clean %>% 
  filter(type == "in situ") %>% 
  dplyr::select(citation, category, driver, variable) %>%
  distinct() %>%
  group_by(category, driver, variable) %>% 
  summarise(freq = n(), .groups = "drop")

# Total count of data points and most frequent driver
sum(data_point_freq$freq)
sum(data_point_freq$freq[data_point_freq$driver == "sea temp"])
sum(data_point_freq$freq[data_point_freq$driver == "salinity"])

# Total count of datasets and most frequent driver
sum(data_set_freq$freq)
sum(data_set_freq$freq[data_point_freq$driver == "sea temp"])
sum(data_set_freq$freq[data_point_freq$driver == "salinity"])

# Tree map of datasets
fig_2_a <- ggplot(data_set_freq, 
       aes(area = freq, group = category, subgroup = driver, subgroup2 = variable)) +
  geom_treemap(aes(fill = category)) +
  geom_treemap_subgroup2_border() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(aes(label = driver), place = "top") +
  # geom_treemap_subgroup2_text(aes(label = variable), place = "bottom") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual("Category",
                    breaks = c("cryo", "phys", "chem", "bio", "soc"),
                    values = c("mintcream", "skyblue", "#F6EA7C", "#A2ED84", "#F48080")) +
  theme(legend.background = element_rect(fill = "grey90", colour = "black"),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 40, unit = "pt"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.box.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"))

# Tree map of data points
fig_2_b <- ggplot(data_point_freq, 
                 aes(area = freq, group = category, subgroup = driver, subgroup2 = variable)) +
  geom_treemap(aes(fill = category)) +
  geom_treemap_subgroup2_border() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(aes(label = driver), place = "top") +
  # geom_treemap_subgroup2_text(aes(label = variable), place = "bottom") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual("Category",
                    breaks = c("cryo", "phys", "chem", "bio", "soc"),
                    values = c("mintcream", "skyblue", "#F6EA7C", "#A2ED84", "#F48080")) +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 40, unit = "pt"))

# Combine and save
fig_2 <- ggpubr::ggarrange(fig_2_a, fig_2_b, ncol = 2, #hjust = 0,
                           labels = c("A)", "B)"), font.label = list(size = 20),
                           legend = "bottom", common.legend = T)  + 
  ggpubr::bgcolor("white") + ggpubr::border(color = "white") 
ggsave("figures/dp_fig_2.png", fig_2, width = 12, height = 7)


# Figure 3 ----------------------------------------------------------------
# Metadata figure showing the coverage of the key drivers
# This should concisely show how many datasets/points are available for each key drivers and site
# But one should be cautious about focussing too much on the sites

if(!exists("clean_all_clean")) load("data/analyses/clean_all_clean.RData")

# Filter out remote products
season_insitu <- filter(clean_all_clean, type == "in situ") %>% 
  filter(!is.na(date)) %>%  # NB: There shouldn't be any values with missing dates
  mutate(month = lubridate::month(date), 
         month_day = format(date, format = "%m-%d"),
         month_days = lubridate::days_in_month(date),
         season = case_when(month %in% 1:3 ~ "Winter", 
                            month %in% 4:6 ~ "Spring",
                            month %in% 7:9 ~ "Summer",
                            month %in% 10:12 ~ "Autumn"),
         season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn")))

# Calculate mean stats
season_mean <- season_insitu %>% 
  dplyr::select(season, month, month_day, month_days, category, driver) %>% 
  distinct() %>% 
  group_by(season, month, month_days, category, driver) %>% 
  summarise(unique_days = n(), .groups = "drop") %>% 
  mutate(prop_days = unique_days/month_days) %>% 
  group_by(season, category, driver) %>% 
  summarise(mean_days = mean(prop_days), .groups = "drop")

# Expand all levels
season_mean_full <- expand(season_mean, season, nesting(category, driver)) %>% 
  left_join(season_mean, by = c("season", "category", "driver")) %>% 
  mutate(season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn")),
         driver = factor(driver, levels = c("sea ice", "glacier", "runoff",
                                            "sea temp", "salinity", "light",
                                            "carb", "nutrients",
                                            "prim prod", "biomass", "spp rich",      
                                            "gov", "tourism", "fisheries"))) %>%
  replace(is.na(.), 0)

# Plot
fig_3 <- ggplot(data = season_mean_full, aes(x = driver, y = mean_days)) +
  geom_col(position = "stack", colour = "black", aes(fill = category)) +
  facet_wrap(~season, ncol = 1) + #guides(fill = "none") +
  scale_y_continuous(limits = c(0, 1.1), expand = c(0, 0)) +
  labs(x = "Driver", y = "Proportion of seasonal coverage") +
  scale_fill_manual("Category",
                    breaks = c("cryo", "phys", "chem", "bio", "soc"),
                    values = c("mintcream", "skyblue", "#F6EA7C", "#A2ED84", "#F48080")) +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black"),
        # plot.background = element_rect(fill = NA, colour = "black"),
        legend.background = element_rect(fill = "grey90", colour = "black"),
        legend.position = "bottom")
# fig_3

# Save
ggsave("figures/dp_fig_3.png", fig_3, width = 7, height = 6)


# Figure 4 ----------------------------------------------------------------
# Show how data availability over time by driver (not variable) has changed by site
# Colour of bars per year should show count of sites, not individual colours per site
# Height of bars shows available data per year
# May want to cut this up by depth, at least for temp/sal

if(!exists("clean_all_clean")) load("data/analyses/clean_all_clean.RData")

# Simple annual presence of drivers by site
clean_all_annual <- clean_all_clean %>% 
  filter(type == "in situ") %>% 
  mutate(year = lubridate::year(date),
         site = case_when(str_detect(site, "Avannaanta|Qeqertalik") ~ "disko",
                          str_detect(site, "Sermersooq") ~ "nuup",
                          TRUE ~ site)) %>%
  filter(site %in% c("kong", "is", "stor", "young", "disko", "nuup", "por")) %>% 
  group_by(site, year, category, driver) %>% 
  summarise(driver_count = n(), .groups = "drop") %>% 
  group_by(year, category, driver) %>% 
  summarise(site_count = n(), 
            driver_count_sum = sum(driver_count), .groups = "drop") %>% 
  mutate(driver = factor(driver, levels = c("sea ice", "glacier", "runoff",
                                            "sea temp", "salinity", "light",
                                            "carb", "nutrients",
                                            "prim prod", "biomass", "spp rich",      
                                            "gov", "tourism", "fisheries")),
         site_count = as.factor(site_count)) %>% 
  left_join(long_driver_names, by = "driver") %>% 
  mutate(driver_long = factor(driver_long, 
                              levels = c("sea ice", "glacier mass balance", "terrestrial runoff",
                                         "seawater temperature", "salinity", "light",
                                         "carbonate chemistry", "nutrients",
                                         "primary production", "biomass", "species richness",
                                         "governance",  "tourism", "fisheries")))

# Specifically the earliest dataset per driver
earliest_driver <- clean_all_clean %>% 
  group_by(driver) %>% 
  filter(date == min(date, na.rm = T)) %>% 
  arrange(driver)

# Stats
clean_all_annual %>% 
  group_by(category, driver) %>% 
  summarise(year = min(year, na.rm = T)) %>% 
  arrange(year)

# Tests
df_1 <- clean_all_clean %>% 
  filter(driver == "prim prod")

# Final proc to get sums before 1957
clean_all_annual_proc <- clean_all_annual %>% 
  mutate(year = case_when(year < 1957 ~ 1952, TRUE ~ year)) %>% 
  group_by(year, category, driver, driver_long) %>% 
  summarise(site_count = max(as.numeric(site_count), na.rm = T),
            driver_count_sum = sum(driver_count_sum, na.rm = T), 
            .groups = "drop") %>% 
  mutate(site_count = as.factor(site_count))

# Labels for plotting
clean_all_labels <- clean_all_annual %>% 
  group_by(category, driver_long) %>% 
  filter(driver_count_sum == max(driver_count_sum)) %>%
  filter(year == min(year)) %>% 
  ungroup() %>% 
  arrange(driver_long) %>% 
  # Manually assign label position along x-axis
  mutate(x_idx = c(-5, -5, -20,
                   -22, -20, -10,
                   -5, -5,
                   -7, -15, -18,
                   -15, -30, -6))

# Stacked barplots of driver presence in a given year
fig_4 <- ggplot(data = clean_all_annual_proc, aes(x = year, y = driver_count_sum)) +
  geom_col(position = "stack", aes(fill = site_count), colour = "black") +
  geom_text(data = clean_all_labels, hjust = 0,
             aes(x = 1957, y = driver_count_sum*0.65, label = driver_long)) +
  geom_text_repel(data = clean_all_labels, nudge_x = clean_all_labels$x_idx, min.segment.length = 0.0,
                  aes(x = year, y = driver_count_sum, label = scales::comma(driver_count_sum))) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 1955, linetype = "dotted", alpha = 0.5) +
  facet_wrap(~driver_long, ncol = 1, scales = "free_y") +
  labs(fill = "Count of sites\nwith data", x = NULL, y = "Count of data points") +
  scale_fill_manual(values = c(blues9[3:9])) +
  scale_x_continuous(limits = c(1949, 2022),
                     expand = c(0, 0),
                     breaks = c(1952, 1955, 1960, 1980, 2000, 2020),
                     labels = c("1876\n1956", "", "1960", "1980", "2000", "2020")) +
  guides(fill = guide_legend(nrow = 1, label.position = "bottom")) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        # axis.line = element_line(colour = "black"),
        # panel.border = element_rect(fill = NA, colour = "black"),
        # panel.border = theme_border(type = c("bottom","right","left")),
        # axis.line.x = element_line(color = 'black'),
        # axis.line.y.left   = element_line(color = 'black'),
        # axis.line.y.right  = element_line(color = 'black'),
        # axis.text.y.right  = element_blank(),
        # axis.ticks.y.right = element_blank(),
        strip.text = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom",
        # legend.spacing.x = unit(0, "mm"),
        legend.background = element_rect(fill = "grey90", colour = "black"),
        legend.title = element_text(margin = margin(r = 5)))
# fig_4

# Save
ggsave("figures/dp_fig_4.png", fig_4, width = 7, height = 8)


# Figure 5 ----------------------------------------------------------------
# Somehow show the relationships between drivers 
# Importance is to show difference between sites
# Heatmap or corplot: https://jhrcook.github.io/ggasym/index.html

# Load relationship data
if(!exists("driver_all")) load(file = "data/analyses/driver_all.RData")

# Get count of comparisons by site regardless of data type and depth
driver_all_site_count <- driver_all %>% 
  dplyr::select(driver, driver_y, variable, variable_y, site) %>% 
  distinct() %>% 
  filter(!is.na(driver_y)) %>% 
  group_by(driver, driver_y, variable, variable_y) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  filter(count >= 4)

# Filter combos with four or more sites available
driver_all_filter <- driver_all %>% 
  right_join(driver_all_site_count) %>% 
  mutate(comp = paste0(variable,"\nvs\n", variable_y),
         depth_comp = paste0(depth,"\nvs\n", depth_y)) %>% 
  filter(comp != "sea ice cover [proportion]\nvs\ntemp [°C]") %>%  # Redundant
  mutate(comp = factor(comp, 
                       levels = c("temp [°C]\nvs\nsea ice cover [proportion]",
                                  "Q [m3/s]\nvs\nsal", "Q [m3/s]\nvs\ntemp [°C]",
                                  "sal\nvs\nspp count [n]", "temp [°C]\nvs\nspp count [n]",
                                  "Q [m3/s]\nvs\nPAR [µmol m-2 s-1]",
                                  "sea ice cover [proportion]\nvs\nPAR [µmol m-2 s-1]")))

# Test stats
df_test <- driver_all_filter %>% 
  filter(comp == "temp [°C]\nvs\nsea ice cover [proportion]")
mean(df_test$slope)
median(df_test$slope)
sd(df_test$slope)

# Boxplots of slopes for each comparison
# With the sites and depths of comparison making up the points
fig_5 <- driver_all_filter %>% 
  left_join(long_site_names, by = "site") %>% 
  # filter(depth == depth_y) %>% # Too restrictive
  ggplot(aes(x = comp, y = slope)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(colour = site_long),
              size = 3, position = position_jitter(0.3)) +
  facet_wrap(~comp, scales = "free") +
  labs(x = NULL, y = "Slope [Y/X]") +
  scale_colour_manual("Site", values = site_colours) +
  guides(colour = guide_legend(nrow = 3, override.aes = list(shape = 15, size = 10))) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.65, 0.154),
        legend.direction = "horizontal", 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14), 
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        panel.border = element_rect(fill = NA, colour = "black"))
# fig_5

# Save
ggsave("figures/dp_fig_5.png", fig_5, width = 12, height = 12)


# Figure 6 ----------------------------------------------------------------
# A figure or table showing similarity between model and amalgamated data. 

# Load cleaned up clean data
if(!exists("clean_all_clean")) load("data/analyses/clean_all_clean.RData")

# Load relationship data
if(!exists("driver_all")) load(file = "data/analyses/driver_all.RData")

# Load moel stats
if(!exists("model_ALL_stats")) load("data/analyses/model_ALL_stats.RData")

# RMSE between in situ/remote and model
fig_6 <- model_ALL_stats %>% 
  filter(type == "in situ") %>% 
  mutate(mean_mod_greater = ifelse(mean_mod > mean_dat, 1, 0)) %>% 
  left_join(long_site_names, by = "site") %>% 
  mutate(variable = factor(variable, 
                           levels = c("temp [°C]", "sal", "pCO2 [µatm]", 
                                      "NO3 [µmol/l]", "PO4 [µmol/l]", "SiO4 [µmol/l]"),
                           labels = c("temp [°C]", "sal", "pCO2 [µatm]", 
                                      "NO3 [µmol l-1]", "PO4 [µmol l-1]", "SiO4 [µmol l-1]"))) %>% 
  group_by(site_long, type, depth, variable) %>% 
  summarise(mean_mod_greater = mean(mean_mod_greater, na.rm = T),
            rmse = mean(rmse, na.rm = T), .groups = "drop") %>% 
  ggplot(aes(x = depth, y = variable)) +
  geom_tile(aes(fill = as.factor(mean_mod_greater)), 
            colour = "black", alpha = 0.5, show.legend = F) +
  geom_text(aes(label = round(rmse, 2))) +
  facet_wrap(~site_long, nrow = 1) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = c("blue", "red")) +
  coord_cartesian(expand = F) +
  labs(x = "Depth", y = "Variable") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.border = element_rect(fill = NA, colour = "black"))
# fig_6

# Save
ggsave("figures/dp_fig_6.png", fig_6, width = 7, height = 3)


# Figure 7 ----------------------------------------------------------------
# Projections of data where possible

# Load relationship data
if(!exists("driver_all")) load(file = "data/analyses/driver_all.RData")

# Load moel stats
if(!exists("model_ALL_stats")) load("data/analyses/model_ALL_stats.RData")

# Create wide model slopes for better merging
model_wide <- model_ALL_stats %>% 
  dplyr::select(site, type, proj, depth, variable, slope) %>% 
  pivot_wider(names_from = proj, values_from = slope)

# Get historic slopes as these tend to be higher than RCP 8.5
historic_trend <- driver_all %>% 
  filter(is.na(variable_y)) %>% 
  mutate(hist_trend = slope*120) %>% 
  dplyr::select(site, type, category, driver, variable, depth, hist_trend)

# Get the product of the historic relationship between drivers and the projected change in the independent driver
future_stats <- driver_all %>% 
  filter(!is.na(variable_y)) %>% 
  left_join(model_wide, by = c("site", "type", "variable", "depth")) %>% 
  left_join(historic_trend, by = c("site", "type", "category", "driver", "variable", "depth")) %>% 
  filter(!is.na(`RCP 2.6`)) %>% 
  mutate(change_hist = slope*(hist_trend*8),
         change_2.6 = slope*(`RCP 2.6`*8),
         change_4.5 = slope*(`RCP 4.5`*8),
         change_8.5 = slope*(`RCP 8.5`*8)) %>% 
  mutate(mean_hist = mean_val+change_hist,
         mean_2.6 = mean_val+change_2.6,
         mean_4.5 = mean_val+change_4.5,
         mean_8.5 = mean_val+change_8.5)

# The figure
fig_7 <- future_stats %>% 
  dplyr::select(site:depth_y, mean_val, mean_hist:mean_8.5) %>% 
  pivot_longer(cols = mean_val:mean_8.5) %>% 
  left_join(long_site_names, by = "site") %>% 
  filter(variable == "temp [°C]",
         variable_y %in% c("sea ice cover [proportion]", "spp count [n]", "Chla [µg/l]")) %>% 
  mutate(name = case_when(name == "mean_hist" ~ paste0(name,"_",type), TRUE ~ name),
         name = factor(name, levels = c("mean_val", "mean_hist_in situ", "mean_hist_OISST", "mean_hist_CCI",
                                        "mean_2.6", "mean_4.5", "mean_8.5"),
                       labels = c("Historic", "in situ", "OISST", "CCI",
                                  "RCP 2.6", "RCP 4.5", "RCP 8.5")),
         variable_y = factor(variable_y,
                             levels = c("sea ice cover [proportion]", "Chla [µg/l]", "spp count [n]"),
                             labels = c("sea ice cover [proportion]", "Chla [µg l-1]", "spp count [n]"))) %>% 
  # QC
  mutate(value = case_when(variable_y == "sea ice cover [proportion]" & value < -1 ~ as.numeric(NA), 
                           variable_y == "Chla [µg/l]" & value < -3 ~ as.numeric(NA),
                           variable_y == "Chla [µg/l]" & value > 3 ~ as.numeric(NA),
                           TRUE ~ value)) %>% 
  #
  ggplot(aes(x = name, y = value)) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(aes(colour = site_long)) +
  facet_wrap(~variable_y, scales = "free_y") +
  # scale_fill_brewer("Variable", palette = "Set3") +
  scale_colour_manual("Site", values = site_colours) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(shape = 15, size = 10))) +
  labs(y = "Projected mean value\nin the year 2100", x = "Projection") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14), 
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "bottom",
        legend.box = "vertical",
        panel.border = element_rect(fill = NA, colour = "black"))
# fig_7
ggsave("figures/dp_fig_7.png", fig_7, width = 8, height = 4)


# Table 1 -----------------------------------------------------------------

# List of the categories, drivers, and their variables
if(!exists("clean_all_clean")) load("data/analyses/clean_all_clean.RData")

# Get ref for UNIS CTD database
# ref_UNIS <- unique(clean_all_clean$citation[grepl("UNIS", clean_all_clean$citation)])

# table(all_meta$category, all_meta$driver, all_meta$variable)
table_1 <- clean_all_clean %>% 
  dplyr::select(category, driver) %>% 
  distinct() %>% 
  mutate(category = factor(category, levels = c("cryo", "phys", "chem", "bio", "soc"))) %>% 
  arrange(category, driver)
write_csv(table_1, "data/analyses/table_1.csv")

# Pivot wide
table_1_wide <- table_1 %>% 
  group_by(category) %>% 
  mutate(row_idx = 1:n()) %>% 
  pivot_wider(names_from = category, values_from = driver) %>% 
  dplyr::select(-row_idx)

# Or rather just manually create this table
table_1_wide <- data.frame(cryo = c("sea ice", "glacier", "runoff"),
                           phys = c("sea temp", "salinity", "light"),
                           chem = c("carb", "nutrients", ""),
                           bio = c("prim prod", "biomass", "spp rich"),
                           soc = c("gov", "tourism", "fisheries"))
write_csv(table_1_wide, "data/analyses/table_1_wide.csv")

# Create figure for now because Google docs tables are red hot garbage
table_1_plot <- ggplot() +
  annotate(geom = "table", x = 0, y = 0, label = list(table_1_wide)) +
  theme_void()
ggsave("figures/table_1.png", table_1_plot, width = 3.5, height = 0.8)


# Table 2 -----------------------------------------------------------------

# Create table of sources for each category/group
load("data/analyses/all_ref.RData")

# Fix uncaught source names
all_ref_fix <- all_ref %>% 
  pivot_longer(cols = kong:`https://data.ssb.no/api/v0/en/table/08818/`, values_to = "source", names_to = "name") %>% 
  filter(!is.na(source)) %>% 
  mutate(name = case_when(!name %in% c("kong", "por", "young", "stor", "is", "disko", "nuup",  
                                       "PANGAEA", "NPDC", "NMDC", "GLODAP", "SOCAT", "Zenodo",
                                       "GEM", "MOSJ", "NIRD", "Kings Bay", "Port of Longyearbyen",
                                       "NSIDC", "NMI", "NOAA", "CCI", "Author") ~ "Other", TRUE ~ name)) %>% 
  pivot_wider(values_from = source, names_from = name, values_fn = mean)

# Pivot wider to get category
all_ref_var_type <- all_ref_fix %>% 
  distinct() %>% 
  mutate(var_type_count = 1) %>% 
  pivot_wider(id_cols = c(type, URL, citation), names_from = category, values_from = var_type_count, values_fn = mean)

# Combine
all_ref_wide <- all_ref_fix %>% 
  left_join(all_ref_var_type, by = c("type", "URL", "citation")) %>% 
  dplyr::select(-category) %>% distinct() %>% 
  filter(type == "in situ") # Remove non in situ sources

# Get summaries by site
table_2_func <- function(site_name, site_long){
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
table_2_kong <- table_2_func(kong, "Kongsfjorden")
table_2_is <- table_2_func(is, "Isfjorden")
table_2_stor <- table_2_func(stor, "Storfjorden")
table_2_young <- table_2_func(young, "Young Sound")
table_2_disko <- table_2_func(disko, "Qeqertarsuup Tunua")
table_2_nuup <- table_2_func(nuup, "Nuup Kangerlua")
table_2_por <- table_2_func(por, "Porsangerfjorden")
table_2 <- rbind(table_2_kong, table_2_is, table_2_stor, table_2_young, table_2_disko, table_2_nuup, table_2_por) %>% 
  mutate(Other = Reduce("+",.[12:ncol(table_2_kong)])) %>% 
  dplyr::select(Site:NMDC, Other)
write_csv(table_2, "data/analyses/table_2.csv")
knitr::kable(table_2)
table_2_plot <- ggplot() +
  annotate(geom = "table", x = 0, y = 0, label = list(table_2)) +
  theme_void()
ggsave("figures/table_2.png", table_2_plot, width = 6.75, height = 1.55)

# Some acronyms:
# "NSIDC" = National Snow & Ice Data Center
# "NMDC" = Norwegian Marine Data Centre
# "NMI" = Norwegian Meteorological Institute
# "NIRD" = National Infrastructure for Research Data


# Table 3 -----------------------------------------------------------------

# Table showing the drivers that were able to be compared and those that could not.
# The check mark showing a possible comparison could be changed to show if the comparisons are both in situ, or only a remotely sensed time series is being compared.
# Use two check marks per box to accomplish this.

# Load relationship data
if(!exists("driver_all")) load(file = "data/analyses/driver_all.RData")

# Get count of comparisons by site regardless of data type and depth
driver_all_site_count <- driver_all %>% 
  dplyr::select(driver, driver_y, variable, variable_y, site) %>% 
  distinct() %>% 
  filter(!is.na(driver_y)) %>% 
  group_by(driver, driver_y, variable, variable_y) %>% 
  summarise(count = n(), .groups = "drop") 

# Count of site counts
driver_count_count <- driver_all_site_count %>% 
  group_by(count) %>% 
  summarise(count_count = n())

# Checks
driver_check <- driver_all_site_count %>% 
  filter(driver == "sea ice")

# Create table
table_3 <- driver_all_site_count %>% 
  filter(count >= 4) %>% 
  arrange(-count) %>% 
  dplyr::rename(`driver x` = driver, `driver y` = driver_y,
                `variable x` = variable, `variable y` = variable_y, `site count` = count) %>% 
  slice(-1) # Remove redundant sea ice sea temp comparison
write_csv(table_3, "data/analyses/table_3.csv")

# Create figure for now because Google docs tables are red hot garbage
table_3_plot <- ggplot() +
  annotate(geom = "table", x = 0, y = 0, label = list(table_3)) +
  theme_void()
ggsave("figures/table_3.png", table_3_plot, width = 5.4, height = 1.8, dpi = 600)


# Table 4 -----------------------------------------------------------------
# Difference in projected trends

# Load relationship data
if(!exists("driver_all")) load(file = "data/analyses/driver_all.RData")

# Load moel stats
if(!exists("model_ALL_stats")) load("data/analyses/model_ALL_stats.RData")

# Create wide model slopes for better merging
model_wide <- model_ALL_stats %>% 
  dplyr::select(site, type, proj, depth, variable, slope) %>% 
  pivot_wider(names_from = proj, values_from = slope) %>% 
  filter(variable %in% c("temp [°C]", "sal"))

# Get historic slopes as these tend to be higher than RCP 8.5
historic_trend <- driver_all %>% 
  filter(is.na(variable_y)) %>% 
  mutate(hist_trend = slope*120) %>% 
  dplyr::select(site, type, category, driver, variable, depth, hist_trend)

# Get the product of the historic relationship between drivers and the projected change in the independent driver
future_stats <- driver_all %>% 
  filter(!is.na(variable_y)) %>% 
  right_join(model_wide, by = c("site", "type", "variable", "depth")) %>% 
  left_join(historic_trend, by = c("site", "type", "category", "driver", "variable", "depth"))

# The trends of the model data against those of the amalgamated data
table_4 <- future_stats %>% 
  dplyr::select(site, type, variable, depth, `RCP 2.6`, `RCP 4.5`, `RCP 8.5`, hist_trend) %>% 
  distinct() %>% 
  pivot_wider(names_from = type, values_from = hist_trend) %>% 
  left_join(long_site_names, by = "site") %>% 
  filter(!is.na(site_long)) %>% 
  dplyr::select(site_long, variable, depth, `in situ`, OISST, CCI, `RCP 2.6`, `RCP 4.5`, `RCP 8.5`) %>% 
  mutate(site_long = factor(site_long, levels = c("Kongsfjorden", "Isfjorden",
                                                  "Storfjorden", "Porsangerfjorden")),
         variable = factor(variable, levels = c("temp [°C]", "sal"))) %>% 
  filter(!is.na(variable)) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  arrange(site_long, variable) %>% 
  dplyr::rename(site = site_long) %>% 
  filter(!is.na(site), depth != "+200") 
write_csv(table_4, "data/analyses/table_4.csv")

# The table
table_4_plot <- ggplot() +
  annotate(geom = "table", x = 0, y = 0, label = list(table_4)) +
  theme_void()
ggsave("figures/table_4.png", table_4_plot, width = 5.8, height = 4.8, dpi = 600)


# Table A1 ----------------------------------------------------------------

# List of the categories, drivers, and their variables
if(!exists("clean_all_clean")) load("data/analyses/clean_all_clean.RData")

# table(all_meta$category, all_meta$driver, all_meta$variable)
table_A1 <- clean_all_clean %>% 
  dplyr::select(category, driver, variable) %>% 
  distinct() %>% 
  mutate(category = factor(category, levels = c("cryo", "phys", "chem", "bio", "soc"))) %>% 
  arrange(category, driver, variable)
write_csv(table_A1, "data/analyses/table_A1.csv")

# Create figure for now because Google docs tables are red hot garbage
table_A1_plot <- ggplot() +
  annotate(geom = "table", x = 0, y = 0, label = list(table_A1)) +
  theme_void()
ggsave("figures/table_A1.png", table_A1_plot, width = 4.25, height = 21.5, dpi = 600)


# Specific examples -------------------------------------------------------

# Load data
if(!exists("clean_all_clean")) load("data/analyses/clean_all_clean.RData")

# Sea ice cover vs seawater temperature at surface and depth
# Difference between West Greenland and West Svalbard
demo_df <- clean_all_clean |> 
  dplyr::select(type, site, driver, variable, date, depth, value) |> 
  filter(site %in% c("nuup", "is"),
         driver %in% c("sea ice", "sea temp"),
         variable != "EsEs acc [cm]",
         !is.na(date)) |>  
  distinct() |> 
  mutate(date = lubridate::round_date(date, unit = "month"),
         month = lubridate::month(date, label = TRUE, abbr = TRUE),
         depth = case_when(depth < 0 ~ "land",
                           is.na(depth) ~ "surface",
                           depth <= 10 ~ "0 to 10",
                           depth <= 50 ~ "10 to 50",
                           depth <= 200 ~ "50 to 200",
                           depth > 200 ~ "+200"),
         depth = factor(depth, levels = c("surface", "0 to 10", "10 to 50", "50 to 200", "+200"))) %>%
  group_by(type, site, driver, variable, date, month, depth) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop") %>% 
  filter(!is.na(value)); gc()
unique(demo_df$site); unique(demo_df$variable); unique(demo_df$depth)

# Merge sea ice onto seawater temperature for better plotting
demo_sea_ice <- demo_df |> 
  filter(variable == "sea ice cover [proportion]") |> 
  pivot_wider(names_from = variable, values_from = value) |>
  dplyr::select(-depth, -driver, -type)
demo_wide <- demo_df |> 
  filter(variable != "sea ice cover [proportion]",
         type == "in situ") |> 
  pivot_wider(names_from = variable, values_from = value) |>
  left_join(demo_sea_ice) |> 
  na.omit()

# Seawater temperature boxplots by month
demo_wide |> 
  mutate(`sea ice cover [proportion]` = `sea ice cover [proportion]` * 10) |> 
  pivot_longer(cols = c(`sea ice cover [proportion]`, `temp [°C]`), names_to = "variable", values_to = "value") |> 
  ggplot(aes(x = month)) +
  geom_boxplot(aes(y = value, fill = variable)) +
  scale_y_continuous("temp [°C]",
                     limits = c(-2, 10.5),
                     breaks = c(0, 5, 10),
                     sec.axis = sec_axis(name = "sea ice cover [proportion]",
                                         trans = ~ . + 0,
                                         breaks = c(0, 5, 10),
                                         labels = c("0.0", "0.5", "1.0"))) +
  facet_grid(depth~site)

# Boxplots of sea ice cover
demo_wide |> 
  ggplot(aes(x = month)) +
  geom_boxplot(aes(y = `sea ice cover [proportion]`, fill = month)) +
  facet_wrap(~site) +
  theme(legend.position = "none")
ggsave("presentations/demo_sea_ice_cover.png", width = 8, height = 4)

# Boxplots of seawater temperature
demo_wide |> 
  ggplot(aes(x = month)) +
  geom_boxplot(aes(y = `temp [°C]`, fill = month)) +
  facet_grid(depth~site) +
  theme(legend.position = "none")
ggsave("presentations/demo_seawater_temp.png", width = 8, height = 6)

# TS of sea ice cover
demo_wide |> 
  ggplot(aes(x = date, y = `sea ice cover [proportion]`)) +
  geom_point(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm", se = FALSE, linewidth = 3) +
  facet_wrap(~site)
ggsave("presentations/demo_sea_ice_cover.png", width = 12, height = 6)

# TS of seawater temp
demo_wide |> 
  ggplot(aes(x = date, y = `temp [°C]`)) +
  geom_point(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm", se = FALSE, linewidth = 2) +
  facet_grid(depth~site)
ggsave("presentations/demo_seawater_temp.png", width = 12, height = 8)

# Scatterplots of seawater and sea ice cover
ggplot(data = demo_wide, aes(x = `temp [°C]`, y = `sea ice cover [proportion]`)) +
  geom_point(aes(colour = depth)) +
  geom_smooth(aes(colour = depth), method = "lm", se = TRUE, linewidth = 3) +
  scale_colour_brewer(palette = "Dark2") +
  coord_cartesian(xlim = c(-2, 10), ylim = c(-0.02, 1.02), expand = F) +
  facet_grid(~site) +
  theme(legend.position = "bottom")
ggsave("presentations/demo_ice_temp.png", width = 12, height = 6)


# Meta-analyses -----------------------------------------------------------

# Load data
if(!exists("clean_all_clean")) load("data/analyses/clean_all_clean.RData")

# Get counts of data
clean_all_count_cat <- clean_all_clean |> 
  summarise(count = n(), .by = c(category)) |> 
  left_join(long_cat_names, by = "category")
clean_all_count_cat_site <- clean_all_clean |> 
  summarise(count = n(), .by = c(category, site)) |> 
  left_join(long_cat_names, by = "category") |> 
  filter(site %in% long_site_names$site) |> 
  left_join(long_site_names, by = "site")
clean_all_count_driv <- clean_all_clean |> 
  summarise(count = n(), .by = c(category, driver)) |> 
  left_join(long_driver_names, by = "driver")

# Log10 of data by category
cat_log10 <- ggplot(data = clean_all_count_cat) +
  geom_bar(aes(x = category_long, y = count, fill = category), 
           show.legend = FALSE, stat = "identity", colour = "black") +
  # scale_y_log10(expand = c(0, 0), breaks = c(0, 100, 10000, 1000000)) +
  scale_y_log10(labels = scales::comma_format(big.mark = ',',
                                              decimal.mark = '.')) +
  scale_fill_manual(values = CatColAbr) +
  labs(y = "Total daily data count [log10]", x = NULL,
       title = "Available data per category (log10 transformed)") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        title = element_text(size = 20),
        panel.background = element_rect(fill = NULL, colour = "black"))
cat_log10
ggsave("metadata/cat_log10.png", cat_log10, width = 10, height = 6)

# Count of data by category
cat_n <- ggplot(data = clean_all_count_cat) +
  geom_bar(aes(x = category_long, y = count, fill = category), 
           show.legend = FALSE, stat = "identity", colour = "black") +
  scale_y_continuous(labels = scales::comma_format(big.mark = ',',
                                                   decimal.mark = '.')) +
  scale_fill_manual(values = CatColAbr) +
  labs(y = "Total daily data count [n]", x = NULL,
       title = "Available data per category") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        title = element_text(size = 20),
        panel.background = element_rect(fill = NULL, colour = "black"))
cat_n
ggsave("metadata/cat_n.png", cat_n, width = 10, height = 6)

# Physical data per site
cat_n_site <- ggplot(data = filter(clean_all_count_cat_site, category == "phys")) +
  geom_bar(aes(x = site_long, y = count, fill = site_long), 
           stat = "identity", position = "dodge", show.legend = FALSE, colour = "black") +
  scale_y_continuous(labels = scales::comma_format(big.mark = ',',
                                                   decimal.mark = '.')) +
  scale_fill_manual(values = site_colours) +
  labs(y = "Total daily data count [n]", x = NULL,
       title = "Available physical data per site") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 20, vjust = 1.0, hjust = 0.9),
        title = element_text(size = 20),
        panel.background = element_rect(fill = NULL, colour = "black"))
cat_n_site
ggsave("metadata/cat_n_site.png", cat_n_site, width = 10, height = 6)

# Log10 of data by driver
driv_log10 <- ggplot(data = clean_all_count_driv) +
  geom_bar(aes(x = driver_long, y = count, fill = category), 
           show.legend = FALSE, stat = "identity", colour = "black") +
  scale_y_log10(labels = scales::comma_format(big.mark = ',',
                                              decimal.mark = '.')) +
  scale_fill_manual(values = CatColAbr) +
  labs(y = "Total daily data count [log10]", x = NULL,
       title = "Available data per driver (log10 transformed)") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 20, vjust = 1.0, hjust = 0.9),
        title = element_text(size = 20),
        panel.background = element_rect(fill = NULL, colour = "black"))
driv_log10
ggsave("metadata/driv_log10.png", driv_log10, width = 10, height = 6)

# Log10 of data by driver
driv_n <- ggplot(data = clean_all_count_driv) +
  geom_bar(aes(x = driver_long, y = count, fill = category), 
           show.legend = FALSE, stat = "identity", colour = "black") +
  scale_y_continuous(labels = scales::comma_format(big.mark = ',',
                                                   decimal.mark = '.')) +
  scale_fill_manual(values = CatColAbr) +
  labs(y = "Total daily data count [n]", x = NULL,
       title = "Available data per driver") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        title = element_text(size = 20),
        axis.text.x = element_text(angle = 20, vjust = 1.0, hjust = 0.9),
        panel.background = element_rect(fill = NULL, colour = "black"))
driv_n
ggsave("metadata/driv_n.png", driv_n, width = 10, height = 6)

# Daily availability of phys data
clean_all_phys_date <- clean_all_clean |> 
  filter(category == "phys") |> 
  dplyr::select(category, driver, date) |> 
  distinct() |> 
  mutate(date_floor = floor_date(date, unit = "month")) |> 
  summarise(monthly_count = n(), .by = c(category, driver, date_floor))

# Plot data by date
driv_date <- ggplot(data = clean_all_phys_date) +
  geom_bar(aes(x = date_floor, y = monthly_count, fill = driver), 
           stat = "identity", position = "dodge", show.legend = FALSE) +
  labs(y = "Unique days of data per month [n]", x = NULL,
       title = "Available physical data per month") +
  scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) +
  scale_x_date(breaks = c(as.Date("1880-01-01"), as.Date("1920-01-01"), as.Date("1960-01-01"), 
                          as.Date("1980-01-01"), as.Date("2000-01-01"), as.Date("2020-01-01"))) +
  facet_wrap(~driver, ncol = 1) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        title = element_text(size = 20),
        strip.text = element_text(size = 12),
        panel.background = element_rect(fill = NULL, colour = "black"),
        legend.position = c(0.5, 0.6),
        legend.background = element_rect(fill = NULL, colour = "black"))
driv_date
ggsave("metadata/driv_date.png", driv_date, width = 10, height = 6)


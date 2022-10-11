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
library(ggplotify) # For working with complex data visuals
library(ggcorrplot) # For correlograms
library(ggalluvial) # For alluvial plot
library(ggasym) # For correlation plots with multiple colour bars
library(treemapify) # For gridded tree map

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

# TODO: Look into sea ice variables with depths below the surface
# Check if ablation [m w.e.] should be a land or surface value
# Force sea ice cover proportion values of 0 to remain in the data.frame
# Convert this piece of the pipeline to use review_filter_var()

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
sea_ice_kong <- filter(full_product_kong, category == "cryo", variable == "ice cover [%]") # Sea ice percent cover of inner fjord
sea_ice_is <- filter(full_product_is, category == "cryo",
                     URL != "https://doi.org/10.1594/PANGAEA.57721", # Glacial maximum ice sheet extension
                     variable %in% c("EsEs acc [cm]", "Ice extent")) # Sea ice extent and ice accretion
sea_ice_stor <- filter(full_product_stor, category == "cryo",
                       URL != "https://doi.org/10.1594/PANGAEA.57721",
                       variable %in% c("Ice conc [tenths]", "Ice cov [%]", "Ice extent", 
                                       "IP [km**3/day]", "EsEs [m]")) %>% mutate(site = "stor") # Sea ice percent cover and thickness
sea_ice_young <- filter(rbind(full_product_young, young_GEM), category == "cryo",
                        !grepl("snow", variable),
                        !grepl("Hynek, Bernhard; Binder, Daniel;", citation),
                        !grepl("Hynek, Bernhard; Weyss, Gernot;", citation)) # A lot of GEM data
sea_ice_disko <- filter(rbind(full_product_disko, disko_GEM), category == "cryo") %>% slice(0) # No sea ice data
sea_ice_nuup <- filter(rbind(full_product_nuup, nuup_GEM), category == "cryo") %>% slice(0) # No sea ice data
sea_ice_por <- filter(full_product_por, category == "cryo", URL != "https://doi.org/10.1594/PANGAEA.57721")
clean_sea_ice <- rbind(sea_ice_kong, sea_ice_is, sea_ice_stor, sea_ice_young, sea_ice_disko, sea_ice_nuup, sea_ice_por) %>% 
  mutate(type = "in situ", driver = "sea ice")
rm(sea_ice_kong, sea_ice_is, sea_ice_stor, sea_ice_young, sea_ice_disko, sea_ice_nuup, sea_ice_por); gc()

# Figures
## Need custom figures per site
## Consistent metadata files may not be useful across sites
# filter(all_sea_ice, !variable %in% c("Open water [start date]", "Open water [end date]"))
ggplot(clean_sea_ice, aes(x = date, y = value, colour = site)) +
  geom_point() + geom_line() + 
  facet_wrap(~variable, scales = "free_y")
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
rm(ice_4km_kong, ice_4km_is, ice_4km_stor, ice_4km_young, ice_4km_disko, ice_4km_nuup, ice_4km_por,
   ice_4km_kong_proc, ice_4km_is_proc, ice_4km_stor_proc, ice_4km_young_proc,
   ice_4km_disko_proc, ice_4km_nuup_proc, ice_4km_por_proc); gc()

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
  mutate(type = "remote",
         category = "cryo",
         driver = "sea ice",
         date_accessed = as.Date("2022-04-26"),
         URL = "https://doi.org/10.7265/N5GT5K3K",
         citation = "U.S. National Ice Center and National Snow and Ice Data Center. Compiled by F. Fetterer, M. Savoie, S. Helfrich, and P. Clemente-Colón. 2010, updated daily. Multisensor Analyzed Sea Ice Extent - Northern Hemisphere (MASIE-NH), Version 1. 4km resolution. Boulder, Colorado USA. NSIDC: National Snow and Ice Data Center. doi: https://doi.org/10.7265/N5GT5K3K.")

# Bind together
clean_sea_ice <- bind_rows(clean_sea_ice, ice_4km_stats)

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

# More summary ideas
# Filter out the chosen drivers
sea_ice_df <- filter(clean_all_clean, driver == "sea ice")
# List variables
unique(ice_vars$variable)
# Meta-data by variable and site
# References per variable
ice_ref <- ice_temp %>% 
  dplyr::select(site, variable, citation) %>% 
  distinct() %>% 
  group_by(site, variable) %>% 
  summarise(n_ref = n(), .groups = "drop")
# Availability by month - proportion
ice_temp_month <- ice_temp %>% 
  dplyr::select(site, variable, date) %>% 
  distinct() %>% 
  mutate(month = month(date)) %>% 
  filter(!is.na(month)) %>% 
  group_by(site, variable, month) %>% 
  summarise(n_month = n(), .groups = "drop") %>% 
  group_by(site, variable) %>% 
  mutate(sum_month = sum(n_month, na.rm = T),
         prop_month = n_month/sum_month)
# Availability at depth
ice_temp_depth <- ice_temp %>% 
  dplyr::select(site, variable, depth) %>% 
  distinct() %>% 
  mutate(depth = round(depth, -1)) %>% 
  # filter(!is.na(depth)) %>% # May want to convert NA to 0 as these are almost always on-the-surface values
  group_by(site, variable, depth) %>% 
  summarise(n_depth = n(), .groups = "drop") %>% 
  group_by(site, variable) %>% 
  mutate(sum_depth = sum(n_depth, na.rm = T),
         prop_depth = n_depth/sum_depth)
# Availability over time
ice_temp_date <- ice_temp %>% 
  dplyr::select(site, variable, date) %>% 
  filter(!is.na(date)) %>%
  distinct() %>% 
  mutate(year = year(date)) %>%
  group_by(site, variable, year) %>% 
  summarise(n_year = n(), .groups = "drop") %>% 
  group_by(site, variable) %>% 
  mutate(sum_year = sum(n_year, na.rm = T),
         prop_year = round(n_year/sum_year, 4))
# Changes over time by depth
## By monthly and by annually
ice_temp_depth_change <- ice_temp %>% 
  dplyr::select(site, variable, date, depth, value) %>% 
  filter(!is.na(date)) %>%
  distinct() %>% 
  mutate(year = year(date),
         depth = case_when(depth < 0 ~ "land",
                           is.na(depth) ~ "surface",
                           depth <= 10 ~ "0 to 10",
                           depth <= 50 ~ "10 to 50",
                           depth <= 200 ~ "50 to 200",
                           depth > 200 ~ "+200")) %>%
  group_by(site, variable, year, depth) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop") %>% 
  filter(!is.na(value))review_filter_var
# Monthly climatologies by depth
ice_temp_depth_clim <- ice_temp %>% 
  dplyr::select(site, variable, date, depth, value) %>% 
  filter(!is.na(date)) %>%
  distinct() %>% 
  mutate(month = month(date),
         depth = case_when(depth < 0 ~ "land",
                           is.na(depth) ~ "surface",
                           depth <= 10 ~ "0 to 10",
                           depth <= 50 ~ "10 to 50",
                           depth <= 200 ~ "50 to 200",
                           depth > 200 ~ "+200")) %>%
  group_by(site, variable, month, depth) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop") %>% 
  filter(!is.na(value))


### Glacier -----------------------------------------------------------------

# NB: Chose not to get many variables from Geyman et al. 2021

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
sea_temp_stor <- review_filter_var(full_product_stor, "stor", "Tpot|Tequ|theta|fco2|Tmax|TTT|
                                   |SST anomaly", c("t [°C]", "SST (1-12) [°C]")) %>% 
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
  mutate(variable = "temp [°C]", category = "phys",
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
         driver = "sea temp") %>% 
  filter(depth >= 0)
rm(sea_temp_kong, sea_temp_is, sea_temp_stor, sea_temp_young, sea_temp_disko, sea_temp_nuup, sea_temp_por); gc()

# Summary analyses
summary_sea_temp <- review_summary(filter(clean_sea_temp, depth >= 0, depth <= 10))

# Plot results
# NB: The apparent cooling trend from in situ data is due to the lack of winter temperatures from pre-satellite era data
# TODO: Figure out why this is breaking
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
  mutate(variable = "sal", driver = "salinity")
rm(sal_kong, sal_is, sal_stor, sal_young, sal_disko, sal_nuup, sal_por); gc()

# Summary analyses
summary_sal <- review_summary(filter(clean_sal, depth >= 0, depth <= 10))

# Plot results
review_summary_plot(summary_sal, "sal")


### Light ------------------------------------------------------------------

# Get all PAR+UV data
light_kong <- review_filter_var(full_product_kong, "PAR|UV", "Onc|Gym|Para|below|abys|harp|chae|ostr|clio|cirr|biva")
light_is <- review_filter_var(full_product_is, "PAR|UV", "aeuch|eleg|UVEL") # No PAR data
light_stor <- review_filter_var(full_product_stor, "PAR|UV") # No PAR data
light_young <- review_filter_var(rbind(full_product_young, young_GEM),  "PAR|UV", "vella|tinn")
## NB: It is unclear if these values should be divided by 10 or not
## It is also unclear what the time dimension is for the data
light_disko <- review_filter_var(rbind(full_product_disko, disko_GEM), "PAR|UV", "milli") %>% filter(value > 0)
## NB: Some of these values are also very high
light_nuup <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "PAR|UV", "trip|vella|sulc|lip|lib|parv")
light_por <- review_filter_var(full_product_por, "PAR|UV") # No PAR data
clean_light <- rbind(light_kong, light_is, light_stor, light_young, light_disko, light_nuup, light_por) %>% 
  mutate(variable = case_when(str_detect(variable, "PAR|par") ~ "PAR [µmol m-2 s-1]",
                              str_detect(variable, "UVA") ~ "UV-A [W*m^2]",
                              str_detect(variable, "UVB") ~ "UV-B [W*m^2]",
                              TRUE ~ variable), driver = "light")
rm(light_kong, light_is, light_stor, light_young, light_disko, light_nuup, light_por); gc()

# Summary analyses
summary_light <- review_summary(filter(clean_light, depth >= 0, depth <= 10))

# Plot results
review_summary_plot(summary_light, "light")


## Chemistry ---------------------------------------------------------------

### Carb -------------------------------------------------------------------

# NB: For this and other chemistry variables see best practices sent by JP on Slack
# Also see e-mail from Liqing Jiang

# Keep pCO2_calc as a separate variable because they can't be taken as absolutely the same
# Same for PCO2water_SST_wet
# Can use SeaCarb to transform fco2 to pCO2
unique(filter(full_product_kong, category == "chem")$variable)
# Get all pCO2 data
# Note that there are duplicates from GLODAP and the underlying files downloaded via PANGAEA
# But this is actually a good thing as it allows us to acknowledge specific contributors,
# which is something that the GLODAP product requests that we do.
carb_kong <- review_filter_var(full_product_kong, "CO2|pH|TA|Alk|CaCO3|calc|carb|diox", 
                               "Precip|Aeti|Agla|Amph|Aphe|Cten|Dimo|Echi|Euk|Euph|Facet|Gaet|Megan|Parae|
                               |Paras|Pleur|Polyc|Pseudo|Sabine|Scaph|Scyph|Siphon|Spino|Thys|Typh|Crypto|
                               |Flagel|Hetero|Algiro|Phaeo|Thal|Dino|Cilio|Chloro|Alex|")
is_pCO2 <- review_filter_var(full_product_is, "CO2|pH|TA|Alk|CaCO3|calc|carb|diox", "emissions|tco2|calls")
stor_pCO2 <- review_filter_var(full_product_stor, "stor", "CO2", "emissions|tco2|fco2")
young_pCO2 <- review_filter_var(rbind(full_product_young, young_GEM), "young", "CO2") # No pCO2 data
disko_pCO2 <- review_filter_var(rbind(full_product_disko, disko_GEM), "disko", "CO2", "fco2|tco2")
nuup_pCO2 <- review_filter_var(rbind(full_product_nuup, nuup_GEM), "nuup", "CO2")
por_pCO2 <- review_filter_var(full_product_por, "por", "CO2")
clean_pCO2 <- rbind(kong_pCO2, is_pCO2, stor_pCO2, young_pCO2, disko_pCO2, nuup_pCO2, por_pCO2) %>% 
  mutate(driver = "carb")
rm(kong_pCO2, is_pCO2, stor_pCO2, young_pCO2, disko_pCO2, nuup_pCO2, por_pCO2); gc()

# Summary analyses
summary_pCO2 <- review_summary(filter(clean_pCO2, depth >= 0, depth <= 10))

# Plot results
review_summary_plot(summary_pCO2, "pCO2")


### Nutrients ---------------------------------------------------------------

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
  # Change GLODAP variable to match PANGAEA standard 
  mutate(variable = case_when(variable == "nitrate [μmol kg-1]" ~ "NO3 [µmol/l]",   
                              variable == "nitrite [μmol kg-1]" ~ "NO2 [µmol/l]",   
                              variable == "silicate [μmol kg-1]" ~ "SiO4 [µmol/l]",
                              variable == "phosphate [μmol kg-1]" ~ "PO4 [µmol/l]",
                              TRUE ~ variable),
         driver = "Nutrients"); unique(clean_nutrients$variable)
rm(kong_nutrients, is_nutrients, stor_nutrients, young_nutrients, disko_nutrients, nuup_nutrients, por_nutrients); gc()

# Summary analyses
summary_nutrients <- review_summary(filter(clean_nutrients, depth >= 0, depth <= 10))

# Plot results
review_summary_plot(summary_nutrients, "nutrients")


## Biology -----------------------------------------------------------------

### Primary production ------------------------------------------------------

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
  mutate(driver = "Chla")
rm(kong_chla, is_chla, stor_chla, young_chla, disko_chla, nuup_chla, por_chla); gc()

# Summary analyses
summary_chla <- review_summary(filter(clean_chla, depth >= 0, depth <= 10))

# Plot results
review_summary_plot(summary_chla, "chla")


### Biomass -----------------------------------------------------------------

# Check this for lot's of variables in Young Sound: https://zenodo.org/record/5572041#.YW_Lc5uxU5m

# Test check for all bio vars to make sure no biomass vars are missed
as.vector(distinct(filter(full_product_kong, category == "bio"), variable))
as.vector(distinct(filter(nuup_GEM, category == "bio"), variable))

# Get all biomass variables
kong_biomass <- filter(full_product_kong, category == "bio",
                       !grepl("zooplankton|phytoplankton", citation, ignore.case = T)) %>%
  mutate(site = "kong") %>% slice(0) # No biomass data
is_biomass <- filter(full_product_is, category == "bio",
                     variable == "P CO2 upt Vmax [µmol/kg/s]") %>% mutate(site = "is")
stor_biomass <- filter(full_product_stor, category == "bio") %>% mutate(site = "stor") # No bio data
young_biomass <- filter(rbind(full_product_young, young_GEM), category == "bio",
                        !grepl("Species Composition", citation),
                        grepl("pp_", variable, ignore.case = T)) %>% mutate(site = "young") # "pp_" may be too restrictive
disko_biomass <- filter(rbind(full_product_disko, disko_GEM), category == "bio") %>% mutate(site = "disko") %>% slice(0) # No biomass data
nuup_biomass <- filter(rbind(full_product_nuup, nuup_GEM), category == "bio",
                       !grepl("Species Composition", citation),
                       !grepl("fluor|Chl a", variable)) %>% mutate(site = "nuup") # Should filter some of the tip growth data
por_biomass <- filter(full_product_por, category == "bio") %>% mutate(site = "por") # No bio data
clean_biomass <- rbind(kong_biomass, is_biomass, stor_biomass, young_biomass, disko_biomass, nuup_biomass, por_biomass) %>% 
  mutate(type = "in situ", driver = "Biomass")
rm(kong_biomass, is_biomass, stor_biomass, young_biomass, disko_biomass, nuup_biomass, por_biomass); gc()

# Summary analyses
summary_biomass <- review_summary(clean_biomass)

# Plot results
review_summary_plot(summary_biomass, "biomass")


### Species richness ------------------------------------------------------

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
# "In general, heatwaves favoured crawling or burrowing predators and suspension feeders, 
# while the abundance of detritivores decreased, suggesting a climate-induced change in dominant zoobenthic traits (Pansch et al., 2018)."

# Test check for all bio vars to make sure no species assemblage vars are missed
as.vector(distinct(filter(full_product_is, category == "bio"), variable))
as.vector(distinct(filter(nuup_GEM, category == "bio"), variable))

# Get all species variables
kong_sp_ass <- filter(full_product_kong, category == "bio") %>% 
  filter(!grepl("Temperature, salinity, light", citation),
         !grepl("Marine biogeochemistry", citation)) %>% mutate(site = "kong")
is_sp_ass <- filter(full_product_is, category == "bio") %>% 
  filter(!variable %in% c("P CO2 upt Vmax [µmol/kg/s]", "Chlorophyll A - 10um [µg/l]",
                          "Chlorophyll A - GFF [µg/l]", "Phaeophytin - 10um [µg/l]", 
                          "Phaeophytin - GFF [µg/l]")) %>% mutate(site = "is")
stor_sp_ass <- filter(full_product_stor, category == "bio") %>% mutate(site = "stor") # No bio data
young_sp_ass <- filter(rbind(full_product_young, young_GEM), category == "bio") %>% 
  filter(grepl("Phytoplankton", citation)) %>% mutate(site = "young")
disko_sp_ass <- filter(rbind(full_product_disko, disko_GEM), category == "bio") %>% mutate(site = "disko") %>% slice(0) # No species data
nuup_sp_ass <- filter(rbind(full_product_nuup, nuup_GEM), category == "bio",
                      grepl("Species Composition", citation)) %>% mutate(site = "nuup")
por_sp_ass <- filter(full_product_por, category == "bio") %>% mutate(site = "por") # No bio data
clean_sp_ass <- rbind(kong_sp_ass, is_sp_ass, stor_sp_ass, young_sp_ass, disko_sp_ass, nuup_sp_ass, por_sp_ass) %>% 
  mutate(type = "in situ", driver = "Species")
rm(kong_sp_ass, is_sp_ass, stor_sp_ass, young_sp_ass, disko_sp_ass, nuup_sp_ass, por_sp_ass); gc()

# Summary analyses
# Need to think about how these analyses should proceed
# One idea is to extract or focus on specific species
# Rather converting them to count data in order to apply the same analyses as for the other variables
summary_sp_ass <- clean_sp_ass %>% 
  filter(value != 0) %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, driver, site, type) %>% 
  summarise(value = as.numeric(n()), .groups = "drop") %>% 
  mutate(variable = "sps_count") %>% 
  review_summary()

# Plot results
review_summary_plot(summary_sp_ass, "sp_ass")


## Social ------------------------------------------------------------------

### Governance --------------------------------------------------------------

# NB: Currently no governance data exist

# Just create an empty slice for each site
disko_govern <- filter(rbind(full_product_disko, disko_GEM), category == "soc") %>% mutate(site = "disko") %>% slice(0)


### Tourism ----------------------------------------------------------------

# Test check for all soc vars to make sure no desired tourism vars are missed
as.vector(distinct(filter(full_product_por, category == "soc"), variable))
as.vector(distinct(filter(nuup_GEM, category == "soc"), variable))

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
  mutate(driver = "Tourism")
rm(kong_tourism, is_tourism, stor_tourism, young_tourism, disko_tourism, nuup_tourism, por_tourism); gc()

# Summary analyses
summary_tourism <- review_summary(clean_tourism)

# Plot results
review_summary_plot(summary_tourism, "tourism")


### Fisheries ---------------------------------------------------------------

# NB: Ship traffic is included here as it is mostly due to industry and not tourism

# Test check for all soc vars to make sure no desired fisheries vars are missed
as.vector(distinct(filter(full_product_is, category == "soc"), variable))
as.vector(distinct(filter(nuup_GEM, category == "soc"), variable))

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
  mutate(category = "soc", driver = "Shipping")
rm(kong_shipping, is_shipping, stor_shipping, young_shipping, disko_shipping, nuup_shipping, por_shipping); gc()

# Summary analyses
summary_shipping <- review_summary(clean_shipping)

# Plot results
review_summary_plot(summary_shipping, "shipping")


# Currently no landings data
# Need to find this from government statistic/fisheries sites


## Save clean data ---------------------------------------------------------

# Combine and change columns to match final standard
clean_all <- rbind(clean_sea_ice, clean_glacier, clean_discharge,
                   clean_SST, clean_sal, clean_light,
                   clean_pCO2, clean_TA, clean_nutrients,
                   clean_PP, clean_biomass, clean_sp_ass,
                   clean_governance, clean_tourism, clean_fisheries) %>% 
  dplyr::rename(category = category, driver = driver, variable = variable) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value)
save(clean_all, file = "data/analyses/clean_all.RData")
plyr::l_ply(unique(clean_all$category), save_category, .parallel = T,
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
# load("data/analyses/all_meta.RData")

all_meta %>% 
  filter(!is.na(value_mean)) %>% 
  mutate(month = lubridate::month(date)) %>% 
  ggplot(aes(x = as.factor(month), y = count_days_name)) +
  geom_boxplot(aes(fill = site), position = "dodge", outlier.colour = NA) +
  geom_jitter(aes(colour = log10(count))) +
  scale_colour_viridis_c() + scale_y_continuous(limits = c(0, 32), expand = c(0, 0)) +
  labs(y = paste0("Unique days with data points"), x = "Month", fill = "Site", colour = "Count [log10(n)]") +
  facet_grid(site~driver) +
  theme(panel.border = element_rect(colour = "black", fill = NA))
ggsave("~/Desktop/analyses_output/meta_meta_box.png", width = 16, height = 12)

annual_temp <- MUR_data %>%
  mutate(year = lubridate::year(t)) %>% 
  group_by(lon, lat, year) %>%
  summarise(annual_minn_temp = min(temp, na.rm = T),
            annual_mean_temp = mean(temp, na.rm = T),
            annual_max_temp = max(temp, na.rm = T), .groups = "drop")


# Section 4 ---------------------------------------------------------------
# Relationships between data analysed for Section 2
# NB: Only necessary to run the `Setup` section

## Provide literature reviews on the relationships they have with each other as well as data analyses of these relationships
# What is changing in the EU Arctic (setting the scene):
#   "Boundary conditions"
# e.g. Warming ocean, loss of sea ice, etc.
# We then go through the key drivers and ask for each driver whether there are general results about Arctic fjords across all sites
# Differences in some sites could be used as exceptions that prove the rule
# Specific comparisons taken from network plot for relationships

# Load all clean data
# clean_all <- map_dfr(dir("data/full_data", pattern = "clean", full.names = T), read_csv)
load("data/analyses/clean_all.RData")

# Combine some variables for better correlations
## NB: This should be done earlier in the data management process and is done here as a temporary fix
## This is part of the normal evolution of a workflow
clean_all_cryo <- clean_all %>% 
  filter(category == "cryo", value != 0) %>% 
  filter(variable != "sal") %>% # Investigate these weird values
  filter(!str_detect(variable, "cover Jan|cover Feb|cover Mar|cover Apr|cover May|cover Jun|
                     |cover Jul|cover Aug|cover Sep|cover Oct|cover Nov|cover Dec|
                     |end date|start date|Snow|snow|
                     |_1936|_1990|_2010")) # Add these back in if possible
clean_all_phys <- clean_all %>% 
  filter(category == "phys", value != 0, variable != "TTT [°C]") # Simply need to remove air temperature
clean_all_chem <- clean_all %>% 
  filter(category == "chem", value != 0, driver != "O2") %>% # Need to remove O2
  mutate(variable = case_when(variable %in% c("pco2 [uatm]", "pCO2water_SST_wet [µatm]",
                                              "pCO2water_SST_wet [uatm]", "pco2_calc [uatm]") ~ "pCO2 [µatm]", # This is not correct to do. See e-mail.
                              variable %in% c("[NO3]- [µmol/l]", "NO3 [µg-at/l]", "NO3 [µmol/kg]") ~ "NO3 [µmol/l]", # Incorrect to do
                              variable %in% c("[PO4]3- [µmol/l]", "PO4 [µg-at/l]") ~ "PO4 [µmol/l]", # Incorrect to do
                              variable %in% c("[NH4]+ [µmol/l]", "[NH4]+ [µg-at/l]") ~ "NH4 [µmol/l]", # Incorrect to do
                              variable %in% c("[NO2]- [µmol/l]", "[NO2]- [µg-at/l]") ~ "NO2 [µmol/l]", # Incorrect to do
                              variable == "nitrate+nitrite [µmol/l]" ~ "NO3+NO2 [µmol/l]",
                              TRUE ~ variable)) # Need to better standardise names
clean_all_bio <- clean_all %>% 
  filter(category == "bio", driver != "Species", value != 0) %>% 
  filter(variable != "pCO2") %>% # Remove these values as they are for terrestrial lichen
  mutate(variable = case_when(str_detect(variable, "Avg g dw") ~ "A. nodosum [Avg g dw]", 
                              str_detect(variable, "segment") ~ as.character(NA),
                              str_detect(variable, "cm/year") ~ "S. latissima [cm/year]",
                              str_detect(variable, "g C/year") ~ "S. latissima [g C/year]",
                              TRUE ~ variable)) %>% 
  filter(!is.na(variable)) %>% 
  group_by(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")
clean_all_spp_count <- clean_all %>% 
  filter(driver == "Species", value != 0) %>% 
  group_by(date_accessed, URL, citation, type, site, category, driver, lon, lat, date, depth) %>% 
  summarise(value = as.numeric(n()), .groups = "drop") %>% 
  mutate(variable = "Spp count")
clean_all_soc <- clean_all %>% 
  filter(category == "soc", value != 0) %>% 
  mutate(driver = case_when(driver == "Shipping" ~ "Fisheries", # Need to fix this above
                            TRUE ~ driver),
         variable = case_when(variable %in% c("Calls [Cruise boats (overseas)]", "Calls [Tourist boats (expedition cruise)]",
                                              "Calls [Day trip boats (local boats)]", "Calls [Day trip boats (12 PAX RIB mm)]",
                                              "Calls [Pleasure boats (Sail charter engine)]") ~ "Calls - tourism [n]",
                              variable %in% c("Calls [Cargo boats]", "Calls [Teaching / research]",
                                              "Calls [Fishing boats]", "Calls [Navy / Coast Guard]",
                                              "Calls [Polar / Nordsyssel]", "Calls [Pilot boat]",
                                              "Calls [Other vessels]") ~ "Calls - commercial [n]",
                              str_detect(variable, "Passengers") ~ "Passengers [n]",
                              str_detect(variable, "arrival") ~ "Arrivals [n]",
                              str_detect(variable, "Days in port") ~ "Days in port - tourism [count]",
                              str_detect(variable, "pop ") ~ "Population [n]",
                              str_detect(variable, "guest night ") ~ "Guest nights [n]",
                              str_detect(variable, "Vessels ") ~ "Vessels [n]",
                              TRUE ~ variable)) %>% 
  group_by(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth) %>% 
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>% 
  # Remove some unwanted ship values like duration in the fjord
  filter(!str_detect(variable, "Duration|duration|Fuel|gross weight|Month trips|emissions|
                     |Number of trips pr year|Average speed|Power|Total fuel|Tonnage|
                     |Month Trips|Number of ships")) %>% # These are annual values 
  # Remove Isfjorden/Storfjorden label and select sum rather than mean values
  mutate(variable = case_when(str_detect(variable, "CO2 emissions \\(tonnes\\)") ~ "CO2 emissions total [tonnes]",
                              str_detect(variable, "Nautical miles|nautical miles") ~ "Nautical miles",
                              str_detect(variable, "Number of ships") ~ "Number of ships [n]",
                              str_detect(variable, "mean") ~ as.character(NA),
                              str_detect(variable, "; sum") ~ gsub("; sum", "", variable),
                              TRUE ~ variable)) %>% 
  filter(!is.na(variable))
clean_all_clean <- clean_all_cryo %>% 
  rbind(clean_all_phys) %>% 
  rbind(clean_all_chem) %>%
  rbind(clean_all_bio) %>% 
  rbind(clean_all_spp_count) %>% 
  rbind(clean_all_soc)
# rm(clean_all, clean_all_spp_count); gc()

### Relationships from the network analysis - created via the review paper
# We want to see which sites have what relationships, and if there are any obvious outliers
# This is one of the main points that will feed back into the review paper

#List of drivers and variables
unique(clean_all_clean$driver)
unique(clean_all_clean$variable)
table(clean_all_clean$driver, clean_all_clean$variable)

## Cryosphere
ice_temp <- driver2_lm("Ice vars", "Sea temp") # sea ice -> sea temp
ice_light <- driver2_lm("Ice vars", "PAR") # sea ice -> light
ice_biomass <- driver2_lm("Ice vars", "Biomass") # sea ice -> biomass
ice_spp <- driver2_lm("Ice vars", "Species") # sea ice -> spp richness
ice_gov <- driver2_lm("Ice vars", "Governance") # sea ice -> governance
gmb_discharge <- driver2_lm("Glacier vars", "river") # gmb -> discharge
gmb_spp <- driver2_lm("Glacier vars", "Species") # gmb -> spp richness
discharge_temp <- driver2_lm("river", "Sea temp") # discharge -> sea temp
discharge_sal <- driver2_lm("river", "Salinity") # discharge -> salinity
discharge_light <- driver2_lm("river", "PAR") # discharge -> light
discharge_carb <- driver2_lm("river", "pCO2") # discharge -> carb system
discharge_nut <- driver2_lm("river", "Nutrients") # discharge -> nutrients

## Physics
temp_ice <- driver2_lm("Sea temp", "Ice vars") # sea temp -> sea ice
temp_spp <- driver2_lm("Sea temp", "Species") # sea temp -> spp richness
temp_biomass <- driver2_lm("Sea temp", "Biomass") # sea temp -> biomass
temp_PP <- driver2_lm("Sea temp", "Chla") # sea temp -> PP
sal_spp <- driver2_lm("Salinity", "Species") # salinity -> spp richness
sal_biomass <- driver2_lm("Salinity", "Biomass") # salinity -> biomass
light_spp <- driver2_lm("PAR", "Species") # light -> spp richness
light_biomass <- driver2_lm("PAR", "Biomass") # light -> biomass
light_PP <- driver2_lm("PAR", "Chla") # light -> PP

## Chemistry
carb_spp <- driver2_lm("pCO2", "Species") # carb system -> spp richness
nut_PP <- driver2_lm("Nutrients", "Chla") # nutrients -> PP

## Biology
PP_biomass <- driver2_lm("Chla", "Biomass") # PP -> biomass
biomass_spp <- driver2_lm("Biomass", "Species") # biomass -> spp richness

## Social
gov_tour <- driver2_lm("Governance", "Tourism") # governance -> tourism # No governance data
gov_fish <- driver2_lm("Governance", "Shipping") # governance -> fisheries # No governance data
tour_nut <- driver2_lm("Tourism", "Nutrients") # tourism -> nutrients
tour_light <- driver2_lm("Tourism", "PAR") # tourism -> light
fish_biomass <- driver2_lm("Fisheries", "Biomass") # fisheries -> biomass
fish_spp <- driver2_lm("Fisheries", "Species") # fisheries -> spp richness

# Look across sites for differences in r2 values for variables that are shared between sites
driver_all <- rbind(ice_temp, ice_light, ice_biomass, ice_spp, ice_gov, gmb_discharge, gmb_spp, 
                    discharge_temp, discharge_sal, discharge_light, discharge_carb, discharge_nut,
                    temp_ice, temp_spp, temp_biomass, temp_PP, sal_spp, sal_biomass, light_spp, light_biomass, light_PP,
                    carb_spp, nut_PP, 
                    PP_biomass, biomass_spp,
                    gov_tour, gov_fish, tour_nut, tour_light, fish_biomass, fish_spp) %>% distinct()

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
  filter(driver == "Sea temp") %>%
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
# TODO: Massive negative relationship between temp and Spp count at Young sound

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

# TODO: Get linear relationships for variables consistent across sites
# Then multiply those linear trends by the future projections in the model
# With a bit of table joining help, this could be done in an automated fashion
# One must code the model variables to similar present day data at similar depths
# One then takes the projected increases by 2100 at different RCP and multiplies them
# by the historic relationships

# Rather we just subset the pixels to those within the bounding box of each site
model_kong_stats <- model_bbox_stats(model_kong, "kong")
model_is_stats <- model_bbox_stats(model_is, "is")
model_stor_stats <- model_bbox_stats(model_stor, "stor")
model_young_stats <- model_bbox_stats(model_young, "young")
model_por_stats <- model_bbox_stats(model_por, "por")
model_ALL_stats <- rbind(model_kong_stats, model_is_stats, model_stor_stats,
                        model_young_stats, model_por_stats)
rm(model_kong_stats, model_is_stats, model_stor_stats,
   model_young_stats, model_por_stats); gc()

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
# Square treemap plots showing count of datasets and count of data points
# NB: Must run Setup and the beginning of Section 4 to get necessary objects

# TODO: Need a note in the legend of the plot saying what the actual total of datasets is
# Because this plot doubles up datasets at the caetgory and driver level

# TODO: Go through clean_all_freq to find errors in data management

# Create frequency data.frame of datasets for category -> driver -> variable
data_point_freq <- clean_all_clean %>% 
  filter(!driver %in% c("Air temp", "O2", "Snow vars"),
         type == "in situ") %>% 
  group_by(category, driver, variable) %>% 
  summarise(freq = n(), .groups = "drop")
data_set_freq <- clean_all_clean %>% 
  filter(!driver %in% c("Air temp", "O2", "Snow vars"),
         type == "in situ") %>% 
  dplyr::select(citation, category, driver, variable) %>%
  distinct() %>%
  group_by(category, driver, variable) %>% 
  summarise(freq = n(), .groups = "drop")

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
                    values = c("mintcream", "skyblue", "#F6EA7C", "#A2ED84", "#F48080"))

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
                    values = c("mintcream", "skyblue", "#F6EA7C", "#A2ED84", "#F48080"))

# Combine and save
fig_2 <- ggpubr::ggarrange(fig_2_a, fig_2_b,
                           ncol = 1, labels = c("A)", "B)"), legend = "left", common.legend = T)
ggsave("figures/dp_fig_2.png", fig_2, width = 7, height = 12)


# Figure 3 ----------------------------------------------------------------
# Metadata figure showing the coverage of the key drivers
# This should concisely show how many datasets/points are available for each key drivers and site
# But one should be cautious about focussing too much on the sites
load("data/analyses/all_meta.RData")

# TODO: Rather show this globally to reduce complexity
# Facet by season
# x-axis is driver
# fill is category

# Filter out remote products
all_meta_insitu <- filter(all_meta, type == "in situ") %>% 
  dplyr::rename(category = category, driver = driver, variable = variable) %>% 
  filter(!driver %in% c("Air temp", "O2", "Snow vars")) %>% 
  mutate(month = lubridate::month(date), 
         season = case_when(month %in% 1:3 ~ "Winter", 
                            month %in% 4:6 ~ "Spring",
                            month %in% 7:9 ~ "Summer",
                            month %in% 10:12 ~ "Autumn"),
         season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn")),
         site = factor(site, levels = c("kong", "is", "stor", "young", "disko", "nuup", "por"),
                       labels = c("Kongsfjorden", "Isfjorden", "Storfjorden", 
                                  "Young Sound", "Disko Bay", "Nuup Kangelrua",
                                  "Porsangerfjorden")))

# Calculate mean stats
all_meta_mean <- all_meta_insitu %>% 
  group_by(season, month, category, driver) %>% 
  summarise(mean_count_days_group = mean(count_days_group, na.rm = T),
            sd_count_days_group = sd(count_days_group, na.rm = T), .groups = "drop") %>% 
  group_by(season, category, driver) %>% 
  summarise(sum_season_count_days_group = sum(mean_count_days_group, na.rm = T),
            prop_count = sum_season_count_days_group/92, .groups = "drop") # Make the proportion value more accurate if we use this

# Expand all levels
all_meta_full <- expand(all_meta_mean, season, category, driver) %>% 
  left_join(all_meta_mean, by = c("season", "category", "driver")) %>% 
  mutate(season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn")),
         driver = factor(driver, levels = c("Ice vars", "Glacier vars", "river", # NB: These need to be updated to final standard
                                            "Sea temp", "Salinity", "PAR",
                                            "pCO2", "Nutrients",
                                            "Chla", "Biomass", "Species",      
                                            "Tourism", "Shipping"))) %>%
  replace(is.na(.), 0)

# Plot
fig_3 <- ggplot(data = all_meta_full, aes(x = driver, y = prop_count)) +
  geom_col(position = "stack", colour = "black", aes(fill = category)) +
  facet_wrap(~season, ncol = 1) + #guides(fill = "none") +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(x = "Driver", y = "Proportion of seasonal coverage") +
  scale_fill_manual("Category",
                    breaks = c("cryo", "phys", "chem", "bio", "soc"),
                    values = c("mintcream", "skyblue", "#F6EA7C", "#A2ED84", "#F48080")) +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.9))
# fig_3_panel

# Save
ggsave("figures/dp_fig_3.png", fig_3, width = 7, height = 6)


# Figure 4 ----------------------------------------------------------------
# Show how data availability over time by driver (not variable) has changed by site
# Time series plots with linear lines
# One plot per driver with site shown as colours in stacked barplot
# NB: Uses all_meta_insitu from Figure 3

# Simple annual presence of drivers by site
all_meta_annual <- all_meta_insitu %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(site, year, category, driver) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  mutate(presence = 1,
         driver = factor(driver, levels = c("Ice vars", "Glacier vars", "river", # NB: These need to be updated to final standard
                                            "Sea temp", "Salinity", "PAR",
                                            "pCO2", "Nutrients",
                                            "Chla", "Biomass", "Species",      
                                            "Tourism", "Shipping")))

# Stacked barplots of drvier presence in a given year
fig_4 <- ggplot(data = all_meta_annual, aes(x = year, y = presence)) +
  geom_col(position = "stack", aes(fill = site)) +
  facet_wrap(~driver, ncol = 1) +
  labs(fill = "Site") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom")

# Save
ggsave("figures/dp_fig_4.png", fig_4, width = 7, height = 12)


# Figure 5 ----------------------------------------------------------------
# Somehow show the relationships between drivers 
# Importance is to show difference between sites
# Heatmap or corplot: https://jhrcook.github.io/ggasym/index.html


# Figure 6 ----------------------------------------------------------------
# Show the differences in R2 etc between sites
# Heatmap of differences


# Table 1 -----------------------------------------------------------------

# Create table of sources for each category/group
load("data/analyses/all_ref.RData") # NB: Must add data sources to this output

# Pivot wider to get category
all_ref_var_type <- all_ref %>% 
  distinct() %>% 
  mutate(var_type_count = 1) %>% 
  pivot_wider(id_cols = c(type, URL, citation), names_from = category, values_from = var_type_count, values_fn = mean)

# Combine
all_ref_wide <- all_ref %>% 
  left_join(all_ref_var_type, by = c("type", "URL", "citation")) %>% 
  dplyr::select(-category) %>% distinct() %>% 
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


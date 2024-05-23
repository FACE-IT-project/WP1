# code/data_analysis.R
# This script contains the analyses performed on the data products created for WP1
# Note that this code mostly serves as a testing area for the creation of the data summary functions
# that are then used to create the nice layout in the data_summary.Rmd page


# Setup -------------------------------------------------------------------

# Libraries
source("code/functions.R")


# Load data ---------------------------------------------------------------

load("~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")
load("~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.RData")
load("~/pCloudDrive/FACE-IT_data/storfjorden/full_product_stor.RData")
load("~/pCloudDrive/FACE-IT_data/young_sound/full_product_young.RData")
load("~/pCloudDrive/FACE-IT_data/disko_bay/full_product_disko.RData")
load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/full_product_nuup.RData")
load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/full_product_por.RData")


# Meta+spatial+temporal+depth summary -------------------------------------

# Kongsfjorden
sum_fig_kong <- data_summary_plot(full_product_kong, "Kongsfjorden")
ggsave("figures/summary_kong.png", sum_fig_kong, width = 10, height = 12)
ggsave("docs/assets/summary_kong.png", sum_fig_kong, width = 10, height = 12)

# Isfjorden
sum_fig_is <- data_summary_plot(full_product_is, "Isfjorden")
ggsave("figures/summary_is.png", sum_fig_is, width = 10, height = 12)
ggsave("docs/assets/summary_is.png", sum_fig_is, width = 10, height = 12)

# Storfjorden
sum_fig_stor <- data_summary_plot(full_product_stor, "Storfjorden")
ggsave("figures/summary_stor.png", sum_fig_stor, width = 10, height = 12)
ggsave("docs/assets/summary_stor.png", sum_fig_stor, width = 10, height = 12)

# Young sound
sum_fig_young <- data_summary_plot(full_product_young, "Young Sound")
ggsave("figures/summary_young.png", sum_fig_young, width = 10, height = 12)
ggsave("docs/assets/summary_young.png", sum_fig_young, width = 10, height = 12)

# Disko bay
sum_fig_disko <- data_summary_plot(full_product_disko, "Disko Bay")
ggsave("figures/summary_disko.png", sum_fig_disko, width = 10, height = 12)
ggsave("docs/assets/summary_disko.png", sum_fig_disko, width = 10, height = 12)

# Nuup Kangerlua
sum_fig_nuup <- data_summary_plot(full_product_nuup, "Nuup Kangerlua")
ggsave("figures/summary_nuup.png", sum_fig_nuup, width = 10, height = 12)
ggsave("docs/assets/summary_nuup.png", sum_fig_nuup, width = 10, height = 12)

# Porsangerfjorden
sum_fig_por <- data_summary_plot(full_product_por, "Porsangerfjorden")
ggsave("figures/summary_por.png", sum_fig_por, width = 10, height = 12)
ggsave("docs/assets/summary_por.png", sum_fig_por, width = 10, height = 12)


# Climatology -------------------------------------------------------------

# Function that produces a 12 facet panel of monthly climatologies
# Temperature
# Salinity
# Oxygen
# Ice cover

# Consider spatial interpolation

# Consider depth interpolation

# Could combine spatial and depth interpolations into an accessible dataset via a shiny interface

# Kongsfjorden
clim_fig_kong <- data_clim_plot(full_product_kong, "Kongsfjorden")
ggsave("figures/clim_kong.png", clim_fig_kong, width = 8, height = 12)
ggsave("docs/assets/clim_kong.png", clim_fig_kong, width = 8, height = 12)

# Isfjorden
clim_fig_is <- data_clim_plot(full_product_is, "Isfjorden")
ggsave("figures/clim_is.png", clim_fig_is, width = 8, height = 12)
ggsave("docs/assets/clim_is.png", clim_fig_is, width = 8, height = 12)

# Storfjorden
clim_fig_stor <- data_clim_plot(full_product_stor, "Storfjorden")
ggsave("figures/clim_stor.png", clim_fig_stor, width = 8, height = 12)
ggsave("docs/assets/clim_stor.png", clim_fig_stor, width = 8, height = 12)

# Young Sound
## NB: Not enough data
clim_fig_young <- data_clim_plot(full_product_young, "Young Sound")

# Disko Bay
clim_fig_disko <- data_clim_plot(full_product_disko, "Disko Bay")
ggsave("figures/clim_disko.png", clim_fig_disko, width = 8, height = 12)
ggsave("docs/assets/clim_stor.png", clim_fig_disko, width = 8, height = 12)

# Nuup Kangerlua
clim_fig_nuup <- data_clim_plot(full_product_nuup, "Nuup Kangerlua")
ggsave("figures/clim_nuup.png", clim_fig_nuup, width = 8, height = 12)
ggsave("docs/assets/clim_nuup.png", clim_fig_nuup, width = 8, height = 12)

# Porsangerfjorden
clim_fig_por <- data_clim_plot(full_product_por, "Porsangerfjorden")
ggsave("figures/clim_por.png", clim_fig_por, width = 8, height = 12)
ggsave("docs/assets/clim_por.png", clim_fig_por, width = 8, height = 12)


# Trend summary -----------------------------------------------------------

# Kongsfjorden
trend_fig_kong <- data_trend_plot(full_product_kong, "Kongsfjorden")
ggsave("figures/trend_kong.png", trend_fig_kong, width = 8, height = 12)
ggsave("docs/assets/trend_kong.png", trend_fig_kong, width = 8, height = 12)

# Isfjorden
trend_fig_is <- data_trend_plot(full_product_is, "Isfjorden")
ggsave("figures/trend_is.png", trend_fig_is, width = 8, height = 12)
ggsave("docs/assets/trend_is.png", trend_fig_is, width = 8, height = 12)

# Storfjorden
trend_fig_stor <- data_trend_plot(full_product_stor, "Storfjorden")
ggsave("figures/trend_stor.png", trend_fig_stor, width = 8, height = 12)
ggsave("docs/assets/trend_stor.png", trend_fig_stor, width = 8, height = 12)

# Young Sound
## NB: Not enough data
trend_fig_young <- data_trend_plot(full_product_young, "Young Sound")

# Disko Bay
## NB: Not enough data
trend_fig_disko <- data_trend_plot(full_product_disko, "Disko Bay")

# Nuup Kangerlua
trend_fig_nuup <- data_trend_plot(full_product_nuup, "Nuup Kangerlua")
ggsave("figures/trend_nuup.png", trend_fig_nuup, width = 8, height = 12)
ggsave("docs/assets/trend_nuup.png", trend_fig_nuup, width = 8, height = 12)

# Porsangerfjorden
trend_fig_por <- data_trend_plot(full_product_por, "Porsangerfjorden")
ggsave("figures/trend_por.png", trend_fig_por, width = 8, height = 12)
ggsave("docs/assets/trend_por.png", trend_fig_por, width = 8, height = 12)


# Range summary -----------------------------------------------------------

# Somehow summarise the ranges in primary drivers...


# Model summary -----------------------------------------------------------

# Kongsfjorden
model_kong <- load_model("kongsfjorden_rcp")
model_fig_kong <- model_summary(model_kong, "Kongsfjorden")
ggsave("figures/model_kong.png", model_fig_kong, height = 8, width = 8)
ggsave("docs/assets/model_kong.png", model_fig_kong, height = 8, width = 8)

# Isfjorden
model_is <- load_model("isfjorden_rcp")
model_fig_is <- model_summary(model_is, "Isfjorden")
ggsave("figures/model_is.png", model_fig_is, height = 8, width = 8)
ggsave("docs/assets/model_is.png", model_fig_is, height = 8, width = 8)

# Inglefieldbukta
## NB: These are from the old site, these have been replaced by Storfjorden
model_ingle <- load_model("inglefieldbukta_rcp")
model_fig_ingle <- model_summary(model_ingle, "Inglefieldbukta")
ggsave("figures/model_ingle.png", model_fig_ingle, height = 8, width = 8)
ggsave("docs/assets/model_ingle.png", model_fig_ingle, height = 8, width = 8)

# Storfjorden
model_stor <- load_model("storfjorden_rcp")
model_fig_stor <- model_summary(model_stor, "Storfjorden")
ggsave("figures/model_stor.png", model_fig_stor, height = 8, width = 8)
ggsave("docs/assets/model_stor.png", model_fig_stor, height = 8, width = 8)

# Young Sound
model_young <- load_model("young_sound_rcp")
model_fig_young <- model_summary(model_young, "Young Sound")
ggsave("figures/model_young.png", model_fig_young, height = 8, width = 8)
ggsave("docs/assets/model_young.png", model_fig_young, height = 8, width = 8)

# Disko Bay
## No model data

# Nuup Kangerlua
## Mo model data

# Porsangerfjorden
model_por <- load_model("porsangerfjorden_rcp")
model_fig_por <- model_summary(model_por, "Porsangerfjorden")
ggsave("figures/model_por.png", model_fig_por, height = 8, width = 8)
ggsave("docs/assets/model_por.png", model_fig_por, height = 8, width = 8)

# Tromso
model_trom <- load_model("tromso_rcp")
model_fig_trom <- model_summary(model_trom, "Tromso")
ggsave("figures/model_trom.png", model_fig_trom, height = 8, width = 8)
ggsave("docs/assets/model_trom.png", model_fig_trom, height = 8, width = 8)


# Species analysis --------------------------------------------------------

# Libraries
source("users/calin/code/formulas.R")
library(vegan)
library(codyn)
library(reshape2)
library(metafor)
library(R.utils)
library(sp)
library(raster)
library(ncdf4)
library(rgdal)
library(stringi)
library(metafor)
library(rgeos)
library(RColorBrewer)
library(glmulti)

# Load data
load("~/pCloudDrive/restricted_data/GEM/nuup/nuup_bird_nb.RData")
load("~/pCloudDrive/restricted_data/GEM/nuup/nuup_seabird_count.RData")
load("~/pCloudDrive/restricted_data/GEM/young/young_bird_nests_hatch.RData")
load("~/pCloudDrive/restricted_data/GEM/young/young_bird_broods.RData")

# Prep for step 1
NUUP_1_n <- nuup_bird_nb %>% 
  # transform( nuup_bird_nb_GEM, TimeSeries_id = as.numeric(factor(`URL`))) %>% 
  # filter(!grepl('presence', variable)) %>% # Create the TimeSeries_id by URL
  mutate(Speci = substr(gsub("\\(.*", "", variable), start = 7, stop = 80), # Take the latin name
         Taxon = stri_replace_last(Speci, replacement = "", regex = " "), # Delete the last 'space'
         Year = year(date),
         Site = "nuup",
         TimeSeries_id = 1) %>% 
  dplyr::group_by(Site, TimeSeries_id, Year, Taxon) %>%
  dplyr::summarise(Density = sum(value), .groups = "drop") %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density)

NUUP_2_n <- nuup_seabird_count %>% 
  # transform( nuup_bird_nb_GEM, TimeSeries_id = as.numeric(factor(`URL`))) %>% 
  # filter(!grepl('presence', variable)) %>% # Create the TimeSeries_id by URL
  mutate(Speci = substr(gsub("\\(.*", "", variable), start = 7, stop = 80), # Take the latin name
         Taxon = stri_replace_last(Speci, replacement = "", regex = " "), # Delete the last 'space'
         Year = year(date),
         Site = "nuup",
         TimeSeries_id = 2) %>% 
  dplyr::group_by(Site, TimeSeries_id, Year, Taxon) %>%
  dplyr::summarise(Density = sum(value), .groups = "drop") %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density)

YOUNG_1_n <- young_bird_nests_hatch %>% 
  filter(!is.na(value)) %>%
  mutate(Speci = substr(gsub("\\(.*", "", variable), start = 7, stop = 80), # Take the latin name
         Taxon = stri_replace_last(Speci, replacement = "", regex = " "), # Delete the last 'space'
         Year = year(date),
         Site = "young",
         TimeSeries_id = 3) %>% 
  dplyr::group_by(Site, TimeSeries_id, Year, Taxon) %>%
  dplyr::summarise(Density = sum(value), .groups = "drop") %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density)

YOUNG_2_n <- young_bird_broods %>% 
  filter(!is.na(value)) %>%
  mutate(Speci = substr(gsub("\\(.*", "", variable), start = 7, stop = 80), # Take the latin name
         Taxon = stri_replace_last(Speci, replacement = "", regex = " "), # Delete the last 'space'
         Year = year(date),
         Site = "young",
         TimeSeries_id = 4) %>% 
  dplyr::group_by(Site, TimeSeries_id, Year, Taxon) %>%
  dplyr::summarise(Density = sum(value), .groups = "drop") %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Year))

# Step 1
DATA1 <- species_analysis_step1(df = NUUP_1_n, tsname = "DATA1", country = "GREENLAND", 
                                lati = 64.5, longi = -51.383333, alt = 2, taxono = "birds")
DATA2 <- species_analysis_step1(NUUP_2_n, "DATA2",  "GREENLAND", 64.5, -51.383333, 5, "birds")
DATA3 <- species_analysis_step1(YOUNG_1_n, "DATA3", "GREENLAND", 74.383333, -20.4, 0, "birds")
DATA4 <- species_analysis_step1(YOUNG_2_n, "DATA4", "GREENLAND", 74.383333, -20.4, 1, "birds")

DATA <- rbind(DATA1, DATA2, DATA3, DATA4)

# Step 2
# Transform variables (standardize continuous variables, and log- or sqrt- transform the ones that are not normally distributed)

DATA$StudyLength <- 1+(DATA$endYear-DATA$startYear)
DATA$Lat.s <- decostand(DATA$Lat, "standardize")
DATA$StudyLength.s <- decostand(DATA$StudyLength, "standardize")
DATA$Alt.Log.s <- decostand(log(DATA$Alt+2), "standardize")
DATA$TMean_S.s <- decostand(sqrt(DATA$TMean_S+154), "standardize")
DATA$Lon.s <- decostand(sqrt(DATA$Lon+9) , "standardize")
DATA$Naturalness.s <- decostand(DATA$Naturalness, "standardize")
DATA$PTot_S.s <- decostand(DATA$PTot_S, "standardize")


# CO2 flux ----------------------------------------------------------------

# Draw a graph to show the normality of the distribution of the flux values
# Can it be normalised?
# Or do we just have to forget it and compare one distribution against another
# What is the best estimate we can derive from the dataset
# Can we arrive at a global value? With a range and uncertainty?
# Or must we have regional values
# Also difference between restored and natural systems
# And the decrease in storage rates years after initial restoration

# Create a scatterplot with CO2 uptake in a year (y-axis) vs number of years since restoration (x-axis)
# Show this with natural and restored sites as different colours

# Timeline
# e-mail the figures and results by Tuesday, June 4th
# Two figures: one for accumulation rate, one for gas flux
# Include statistical measure of skewness of these two distributions
# One plot of the relationship between CO2 uptake and year since restoration
# Superimpose the statistical model that shows it (probably negative exponential)
# Also settle on what the best measure of central tendency is
## Show both mean and median estimates
# Compare accumulation rates of natural vs restored salt marshes

# Remember that the main issue is upscaling daily chamber measurements to annual average rates


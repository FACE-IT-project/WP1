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


# SST bbox ----------------------------------------------------------------

# Look at some bbox averaged SST values sent to Sebastien Descamps

# Fjord names

# load the files
fjord_sites <- map_dfr(dir("~/pCloudDrive/FACE-IT_data/fjord_SST", full.names = TRUE), load_fjord_site)

# Average by month
# fjord_sites_monthly <- fjord_sites

# Visualise each one
ggplot(data = fjord_sites, aes(x = t, y = temp)) +
  annotate("segment", x = as.Date("2016-01-01"), xend = as.Date("2016-01-01"),
           y = -2, yend = 13, colour = "purple", linewidth = 2, linetype = "dashed") +
  geom_line() + geom_smooth(method = "lm", se = FALSE) + 
  facet_wrap("site") + labs(x = NULL, y = "Temperature [°C]")


# CO2 flux ----------------------------------------------------------------

# Specific statistics libraries for this section
library(Renext)
library(moments)
library(fitdistrplus)
library(univariateML)
library(gamlss)
library(gamlss.dist)
library(trend)

# Load data
CO2_flux <- readxl::read_xls("~/pCloudDrive/restricted_data/CO2/Aug2023_GlobalCarbonReview_FullDataset.xls", sheet = "All data")
colnames(CO2_flux)

# Primary variables
# c_acc, 

# Predictor variables
# time_since_restoration, natural_or_restored, 

# Interesting variables
# continent, country, marsh_size_ha, rest_area_ha, marsh_position, rest_cat_simp, main_climate, sediment_type, grazed, diked

# Number of rows
nrow(CO2_flux) # 2055
filter(CO2_flux, !is.na(c_acc)) |> nrow() # 394
filter(CO2_flux, !is.na(time_since_restoration)) |> nrow() # 314
filter(CO2_flux, !is.na(time_since_restoration), !is.na(c_acc)) |> nrow() # 96
filter(CO2_flux, !is.na(time_since_restoration), !is.na(c_acc)) |> summarise(max(time_since_restoration)) # 97.3

# Prep data for easier testing
CO2_flux_c_acc_time <- CO2_flux |> filter(!is.na(c_acc), !is.na(time_since_restoration)) |> 
  dplyr::select(c_acc, time_since_restoration, natural_or_restored) #|> 
  # filter(c_acc != 0)
  # mutate(c_acc = case_when(c_acc == 0 ~ c_acc+0.00000000001, TRUE ~ c_acc))
# It seems odd that there are several 'natural' data points in the graph of time since restoration.  
# If the sites are 'natural', then I am surprised that there are also data entries for 'time since restoration'.  Best ignored??
CO2_flux_c_acc_time_restored <- CO2_flux_c_acc_time |> filter(natural_or_restored == "restored")
CO2_flux_c_acc_time_natural <- CO2_flux_c_acc_time |> filter(natural_or_restored == "natural")

# Vectors for simpler stats
c_acc_vec <- as.vector(filter(CO2_flux, !is.na(c_acc)) |> dplyr::select(c_acc))[[1]]
c_acc_time_vec <- as.vector(CO2_flux_c_acc_time |> dplyr::select(c_acc))[[1]]
c_acc_time_rest_vec <- as.vector(CO2_flux_c_acc_time_restored |> dplyr::select(c_acc))[[1]]
c_acc_time_natur_vec <- as.vector(CO2_flux_c_acc_time_natural |> dplyr::select(c_acc))[[1]]
time_vec <- as.vector(filter(CO2_flux, !is.na(time_since_restoration)) |> dplyr::select(time_since_restoration))[[1]]
time_c_acc_vec <- as.vector(filter(CO2_flux, !is.na(c_acc), !is.na(time_since_restoration)) |> dplyr::select(time_since_restoration))[[1]]

# c_acc_time_vec stats
mean(c_acc_time_vec) # 4.0
median(c_acc_time_vec) # 2.3
# Geometric mean
base::prod(c_acc_time_vec)^(1/length(c_acc_time_vec)) # 2.4
# Harmonic mean
1/mean(1/c_acc_time_vec[c_acc_time_vec != 0]) # 1.4
shapiro.test(c_acc_time_vec) # p< 0.001 - not normal distribution
ks.test(c_acc_time_vec, 'pnorm') # p< 0.001 - not normal distribution
skewness(c_acc_time_vec, na.rm = TRUE) # skewness = 3.686574 - heavy positive skewness
kurtosis(c_acc_time_vec, na.rm = TRUE) # kurtosis = 18.83658 - leptokurtic - long tail(s)
descdist(c_acc_time_vec, discrete = FALSE)
fitdist(c_acc_time_vec, "norm")
fitdist(c_acc_time_vec, "exp") # Much better fit
model_select(c_acc_time_vec, criterion = "aic") # Skew Student-t model 
model_select(c_acc_time_rest_vec, criterion = "aic") # Lomax model
model_select(c_acc_time_natur_vec, criterion = "aic") # Skew Normal model
ks.test(c_acc_time_rest_vec, 'plomax') # p< 0.001 - not lomax distribution
flomax(c_acc_time_rest_vec, plot = TRUE)
mod <- gamlss(c_acc~time_since_restoration, family = PARETO2o, data = CO2_flux_c_acc_time_restored)
plot(mod) # Diagnostic plots
wp(mod, ylim.all = c(1)) # Worm-plot (detrended Q-Q plot)
dtop(mod) # Detrended Owen's plot
# Extract for plotting
mod_df <- data.frame(x = mod$mu.x[1:80, 2], y = mod$y, mu = mod$mu.fv)

# c_acc_time_rest_vec stats
mean(c_acc_time_rest_vec) # 4.519297
median(c_acc_time_rest_vec) # 2.86
# Geometric mean
base::prod(c_acc_time_rest_vec)^(1/length(c_acc_time_rest_vec)) # 2.6
# Harmonic mean
1/mean(1/c_acc_time_rest_vec) # 1.4
shapiro.test(c_acc_time_rest_vec) # p< 0.001 - not normal distribution
ks.test(c_acc_time_rest_vec, 'pnorm') # p< 0.001 - not normal distribution
skewness(c_acc_time_rest_vec, na.rm = TRUE) # skewness = 3.377091 - heavy positive skewness
kurtosis(c_acc_time_rest_vec, na.rm = TRUE) # kurtosis = 16.04111 - leptokurtic - long tail(s)

# time_c_acc_vec stats
mean(time_c_acc_vec, na.rm = TRUE) # 13.9
median(time_c_acc_vec, na.rm = TRUE) # 7.5
shapiro.test(time_c_acc_vec) # p< 0.001 - not normal distribution
ks.test(time_c_acc_vec, 'pnorm') # p< 0.001 - not normal distribution
skewness(time_c_acc_vec, na.rm = TRUE) # skewness = 2.732808 - heavy positive skewness
kurtosis(time_c_acc_vec, na.rm = TRUE) # kurtosis = 12.33001 - leptokurtic - long tail(s)

# Relationship between the two
lm(CO2_flux_c_acc_time$c_acc ~ CO2_flux_c_acc_time$time_since_restoration) # intercept of 5.324 with linear trend of -0.09393
lm(log(CO2_flux_c_acc_time$c_acc) ~ CO2_flux_c_acc_time$time_since_restoration) # intercept of 1.17493 with log trend of -0.02092
lm(CO2_flux_c_acc_time_natural$c_acc ~ CO2_flux_c_acc_time_natural$time_since_restoration) # intercept of 1.504037 with linear trend of 0.007461
lm(log(CO2_flux_c_acc_time_restored$c_acc) ~ CO2_flux_c_acc_time_restored$time_since_restoration) # intercept 0.43116 slope -0.01054
lm(log(CO2_flux_c_acc_time_restored$c_acc) ~ log(CO2_flux_c_acc_time_restored$time_since_restoration)) # intercept 2.0692 slope -0.5417

# Compare accumulation rates of natural vs restored salt marshes
# Draw a graph to show the normality of the distribution of the flux values
# Create a scatterplot with CO2 uptake in a year (y-axis) vs number of years since restoration (x-axis)
# Show this with natural and restored sites as different colours
ggplot(data = CO2_flux_c_acc_time, aes(x = time_since_restoration, y = c_acc)) +
  geom_point(aes(colour = natural_or_restored), size = 3) +
  geom_smooth(data = CO2_flux_c_acc_time_natural, method = "lm", se = FALSE, formula = y ~ x, colour = "black") +
  geom_smooth(data = CO2_flux_c_acc_time_restored, method = "lm", se = FALSE, formula = y ~ log(x), colour = "yellow") +
  geom_smooth(data = CO2_flux_c_acc_time_restored, method = "lm", se = FALSE, formula = log(y) ~ x, colour = "red") +
  geom_smooth(data = CO2_flux_c_acc_time_restored, method = "lm", se = FALSE, formula = log(y) ~ log(x), colour = "purple") +
  geom_line(dat = mod_df, aes(x = x, y = mu), colour = "blue") +
  # geom_smooth(method = "glm", se = FALSE, formula = y ~ x, colour = "black") +
  # geom_smooth(method = "glm", se = FALSE, formula = exp(y) ~ x, aes(colour = natural_or_restored)) +
  # geom_smooth(method = "glm", se = FALSE, formula = log(y) ~ log(x), aes(colour = natural_or_restored)) +
  # scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  # scale_y_continuous(limits = c(-10, 40), expand = c(0, 0)) +
  theme(panel.border = element_rect(colour = "black", fill = NA))
ggsave("~/pCloudDrive/restricted_data/CO2/c_acc_vs_time_since_restore.png", height = 4, width = 7)

ggplot(data = CO2_flux_c_acc_time_restored, aes(x = log(time_since_restoration), y = log(c_acc))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x, colour = "black") +
  theme(panel.border = element_rect(colour = "black", fill = NA))
ggsave("~/pCloudDrive/restricted_data/CO2/c_acc_vs_time_since_restore_restored_log.png", height = 4, width = 4)


# Can it be normalised?
# Or do we just have to forget it and compare one distribution against another
# What is the best estimate we can derive from the dataset
# Can we arrive at a global value? With a range and uncertainty?
# Or must we have regional values
# Also difference between restored and natural systems
# And the decrease in storage rates years after initial restoration


# Timeline
# e-mail the figures and results by Tuesday, June 4th
# Two figures: one for accumulation rate, one for gas flux
# Include statistical measure of skewness of these two distributions
# One plot of the relationship between CO2 uptake and year since restoration
# Superimpose the statistical model that shows it (probably negative exponential)
# Also settle on what the best measure of central tendency is
## Show both mean and median estimates

# Remember that the main issue is upscaling daily chamber measurements to annual average rates

# Proposed workflow
## Separately for global natural and global restored; provide median values of CH4 flux and for N2O flux (as CO2e ha-1 yr-1)
## Extract c_acc for natural and restored systems separately
## Breakpoint analysis for c_acc
## Calc mean, medial, and lm for each break group

# Load data
CO2_flux <- readxl::read_xls("~/pCloudDrive/restricted_data/CO2/Aug2023_GlobalCarbonReview_FullDataset.xls", sheet = "All data")
colnames(CO2_flux)

# Prep data for easier testing
c_acc_restored <- CO2_flux |> filter(!is.na(c_acc), !is.na(time_since_restoration), natural_or_restored == "restored") |> 
  dplyr::select(c_acc, time_since_restoration, natural_or_restored)

## Breakpoint analysis (pettitt test)
# Get annual trends
min(c_acc_restored$time_since_restoration); max(c_acc_restored$time_since_restoration) # 1 - 97
c_acc_annual_trends <- c_acc_restored |> 
  mutate(time_since_restoration = round(time_since_restoration)) |> 
  nest() |> 
  mutate(ts_out = purrr::map(data, ~ts(.x$c_acc, start = 1, end = 97, frequency = 1))) |> 
  mutate(sens = purrr::map(ts_out, ~sens.slope(.x, conf.level = 0.95)), 
         pettitt = purrr::map(ts_out, ~pettitt.test(.x)),
         lm = purrr::map(data, ~lm(c_acc ~ time_since_restoration, .x))) |> 
  mutate(Sens_Slope = as.numeric(unlist(sens)[1]),
         P_Value = as.numeric(unlist(sens)[3]),
         Change_Point_Year = time(ts_out[[1]])[as.numeric(unlist(pettitt)[3])],
         Change_Point_pvalue = as.numeric(unlist(pettitt)[4]),
         lm_slope = unlist(lm)$coefficients.time_since_restoration) |> 
  # Add step of cutting time series in 2 using Change_Point_Year 
  mutate(pre_ts = purrr::map(ts_out, ~window(.x, start = 1, end = Change_Point_Year)),
         post_ts = purrr::map(ts_out, ~window(.x, start = Change_Point_Year, end = 97))) |> 
  # Add step of calculating sen's slope and p-value to pre and post change point year
  mutate(sens_pre = purrr::map(pre_ts, ~sens.slope(.x, conf.level = 0.95)),
         Sens_Slope_pre = as.numeric(unlist(sens_pre)[1]), 
         P_Value_pre = as.numeric(unlist(sens_pre)[3]),
         sens_post = purrr::map(post_ts, ~sens.slope(.x, conf.level = 0.95)),
         Sens_Slope_post = as.numeric(unlist(sens_post)[1]),
         P_Value_post = as.numeric(unlist(sens_post)[3])) |> 
  dplyr::select(Sens_Slope, P_Value, Change_Point_Year, Change_Point_pvalue, lm_slope,
                Sens_Slope_pre, P_Value_pre, Sens_Slope_post, P_Value_post)

# Calculate common sense slopes
c_acc_sense_trends <- c_acc_restored |> 
  mutate(cut_point = base::cut(time_since_restoration, breaks = c(0, 4, 19, 100))) |> 
  group_by(cut_point) |> nest() |> 
  mutate(ts_out = purrr::map(data, ~ts(.x$c_acc, start = 1, end = 97, frequency = 1))) |>
  mutate(sens = purrr::map(ts_out, ~sens.slope(.x, conf.level = 0.95)),
         lm = purrr::map(data, ~lm(c_acc ~ time_since_restoration, .x)),
         lm_tidy = purrr::map(lm, ~broom::tidy(.x)),
         sens_tidy = purrr::map(sens, ~broom::tidy(.x))) |> 
  dplyr::select(cut_point, lm_tidy) |> 
  unnest(lm_tidy) |> arrange(cut_point) |> ungroup() |> 
  filter(term == "time_since_restoration") |> 
  dplyr::select(cut_point, estimate, p.value) |> 
  dplyr::rename(slope = estimate) |> 
  mutate(slope = round(slope, 4), p.value = round(p.value, 3)) |> 
  mutate(x_pos = c(0, 11, 60), lab_name = c("Rapid", "Moderate", "Slow"))

# Plot with labels
c_acc_restored |> 
  ggplot(aes(x = time_since_restoration, y = c_acc)) +
  geom_point() +
  # geom_vline(xintercept = 0, colour = "red1") + geom_vline(xintercept = 3.9, colour = "red1") + 
  geom_vline(xintercept = 4, colour = "red4") + #geom_vline(xintercept = 18.9, colour = "orange1") +
  geom_vline(xintercept = 20, colour = "goldenrod4") +
  # annotate("label", x = 2, y = -1, label = "Rapid c_acc < 4 years") +
  # annotate("label", x = 19, y = -1, label = "Moderate c_acc < 19 years") +
  # annotate("label", x = 20, y = -1, label = "7") +
  geom_vline(xintercept = 34, colour = "dodgerblue4") +
  geom_label(data = c_acc_sense_trends, aes(x = x_pos, y = -1, label = paste0(lab_name,"\n",slope," (",p.value,")"))) +
  annotate("label", x = 34, y = 20, label = "Breakpoint = 34 years\n[Pettitt test]") +
  theme(panel.border = element_rect(colour = "black", fill = NA))


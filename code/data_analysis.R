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
## NB: These are from the old site, need to be updated to Storfjorden
## This requires asking Morten Skogen to fetch new data...
model_ingle <- load_model("inglefieldbukta_rcp")
model_fig_ingle <- model_summary(model_ingle, "Inglefieldbukta")
ggsave("figures/model_ingle.png", model_fig_ingle, height = 8, width = 8)
ggsave("docs/assets/model_ingle.png", model_fig_ingle, height = 8, width = 8)

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


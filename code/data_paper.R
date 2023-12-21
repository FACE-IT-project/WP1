# code/data_paper.R
# The code used for the analyses in the WP1 data paper (D1.3)


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
QR_data_paper <- qr_code("https://essd.copernicus.org/preprints/essd-2022-455/")
plot(QR_data_paper)
generate_svg(QR_data_paper, filename = "presentations/QR_data_paper.svg")


# Section 3 ---------------------------------------------------------------
# The processing of site based data products into driver based data products

# NB: This code has been moved to the bottom of `code/data_product.R`


# Section 4 ---------------------------------------------------------------
# Relationships between data cleaned up and analysed for Section 3

# Load cleaned up clean data
if(!exists("clean_all_clean")) load("data/analyses/clean_all_clean.RData")

### Relationships from the network analysis - created via the review paper
# We want to see which sites have what relationships, and if there are any obvious outliers
# This is one of the main points that will feed back into the review paper
# NB: These have already been run and can be loaded below as `driver_alll`

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

# Stats for amount of overlap for drivers comparable within 4+ sites
summary(driver_all_filter$nobs)


# Supplementary -----------------------------------------------------------
# Future projections of data analysed for Section 3 and relationships from Section 4
# NB: Only necessary to run the `Setup` section

## Introduce the future by talking about the whole Arctic
# These data can be downloaded from the IPCC interactive website
# https://interactive-atlas.ipcc.ch
# Based on the relationships elucidated in the previous section we can then look at any possibly useful projections into the future with the model data

# Load cleaned up clean data
if(!exists("clean_all_clean")) load("data/analyses/clean_all_clean.RData")

# Load relationship data
if(!exists("driver_all")) load(file = "data/analyses/driver_all.RData")

# Load map
if(!exists("coastline_full")) load("metadata/coastline_full_df.RData")

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
if(!exists("coastline_full")) load("metadata/coastline_full_df.RData")
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
  facet_wrap(~season, ncol = 1) +
  scale_y_continuous(limits = c(0, 1.1), expand = c(0, 0)) +
  labs(x = "Driver", y = "Proportion of seasonal coverage") +
  scale_fill_manual("Category",
                    breaks = c("cryo", "phys", "chem", "bio", "soc"),
                    values = c("mintcream", "skyblue", "#F6EA7C", "#A2ED84", "#F48080")) +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black"),
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
        strip.text = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill = "grey90", colour = "black"),
        legend.title = element_text(margin = margin(r = 5)))
# fig_4

# Save
ggsave("figures/dp_fig_4.png", fig_4, width = 7, height = 8)


# Figure S1 ---------------------------------------------------------------
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

# Boxplots of slopes for each comparison
# With the sites and depths of comparison making up the points
fig_S1 <- driver_all_filter %>% 
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
# fig_S1

# Save
ggsave("figures/dp_fig_S1.png", fig_5, width = 12, height = 12)


# Figure S2 ---------------------------------------------------------------
# A figure or table showing similarity between model and amalgamated data. 

# Load cleaned up clean data
if(!exists("clean_all_clean")) load("data/analyses/clean_all_clean.RData")

# Load relationship data
if(!exists("driver_all")) load(file = "data/analyses/driver_all.RData")

# Load moel stats
if(!exists("model_ALL_stats")) load("data/analyses/model_ALL_stats.RData")

# RMSE between in situ/remote and model
fig_S2 <- model_ALL_stats %>% 
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
# fig_S2

# Save
ggsave("figures/dp_fig_S2.png", fig_S1, width = 7, height = 3)


# Figure S3 ---------------------------------------------------------------
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
fig_S3 <- future_stats %>% 
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
# fig_S3
ggsave("figures/dp_fig_S2.png", fig_S3, width = 8, height = 4)


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

# Get drivers available at each site
driver_site <- driver_all |> 
  filter(site %in% long_site_names$site) |> 
  dplyr::select(site, type, category, driver, variable) |> 
  distinct()

driver_site_wide <- driver_site |> 
  pivot_wider(names_from = site, values_from = type)

driver_site_count <- driver_site |> 
  summarise(count = n(), .by = c(type, category, driver, variable))

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


# Table S1 -----------------------------------------------------------------
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
table_S1 <- future_stats %>% 
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
write_csv(table_S1, "data/analyses/table_S1.csv")

# The table
table_S1_plot <- ggplot() +
  annotate(geom = "table", x = 0, y = 0, label = list(table_4)) +
  theme_void()
ggsave("figures/table_S1.png", table_4_plot, width = 5.8, height = 4.8, dpi = 600)


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

# Available data for drivers
unique(clean_all_clean$driver)
unique(clean_all_clean$variable)
quick_plot_avail("sea ice", legend_tweak = c(0.1, 0.68))
quick_plot_avail("glacier")
quick_plot_avail("runoff", legend_tweak = c(0.2, 0.7))
quick_plot_avail("salinity", legend_tweak = c(0.2, 0.5))
quick_plot_avail("nutrients", legend_tweak = c(0.2, 0.45))
quick_plot_avail("Chla", filter_choice = "variable", legend_tweak = c(0.2, 0.3))
quick_plot_avail("spp rich")
quick_plot_avail("spp count", filter_choice = "variable")


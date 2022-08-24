# code/review_paper.R
# The code used for the tables and figures in the WP1 review paper (D1.3)


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


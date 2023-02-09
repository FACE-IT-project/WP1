# code/study_sites.R
# This script houses code used in managing study site info

# Notes on site coordinators contacted for bbox and transect coordinates
# Western Greenland: Nuup Kangerlua (Godthåbsfjord) (GF), Qeqertarsuup Tunua (Disko Bay) (DB): Thomas Juul-Pedersen
# Eastern Greenland: Young Sound (YS): Mikael Kristian Sejr
# Svalbard: Kongsfjorden (KF): Allison Bailey
# Svalbard: Isfjorden (IF), Inglefieldbukta (IB) and Agardhbukta: Janne Søreide; UNIS - Borge Damsgard
# Norway: Porsangerfjorden (PF), Finnmark: Lis Lindal Jørgensen

# Search the literature for the necessary bounding boxes and transects

# Maps per study site that are more zoomed out and show the coordinates labelled per corner of
# the bounding boxes so it is more clear within what lon/lat confines the data are desired for


# Setup -------------------------------------------------------------------

# Start with common project code
source("code/functions.R")
library(gdalUtils)

# Load FACE-IT logo
logo <- grid::rasterGrob(png::readPNG("FACE-IT_Logo_900.png"), interpolate = TRUE)


# Single points -----------------------------------------------------------

site_points <- data.frame(site = c("Kongsfjorden", "Isfjorden", "Inglefieldbukta", "Storfjorden", 
                                   "Young Sound", "Disko Bay", "Nuup Kangerlua", 
                                   "Porsangerfjorden"),
                          lon = c(11.845, 14.365, 18.31, 19.88, -21.237, -52.555, -50.625, 25.75),
                          lat = c(78.98, 78.235, 77.87, 77.78, 74.517, 69.36, 64.405, 70.6))


# Bounding box polygons ---------------------------------------------------

# European Arctic
bbox_EU_poly <- bbox_to_poly(bbox_EU, "EU")

# Svalbard
bbox_sval_poly <- bbox_to_poly(bbox_sval, "Svalbard")
bbox_kong_poly <- bbox_to_poly(bbox_kong, "Kongsfjorden")
bbox_is_poly <- bbox_to_poly(bbox_is, "Isfjorden")
bbox_stor_poly <- bbox_to_poly(bbox_stor, "Storfjorden")
bbox_ingle_poly <- bbox_to_poly(bbox_ingle, "Inglefieldbukta")

# Greenland
bbox_young_poly <- bbox_to_poly(bbox_young, "Young Sound")
bbox_disko_poly <- bbox_to_poly(bbox_disko, "Disko Bay")
bbox_nuup_poly <- bbox_to_poly(bbox_nuup, "Nuup Kangerlua")

# Norway
bbox_por_poly <- bbox_to_poly(bbox_por, "Porsangerfjorden")


# Transects ---------------------------------------------------------------

# Svalbard
trnsct_sval <- data.frame(site = c("Kongsfjorden", "Isfjorden", "Inglefieldbukta"),
                          lon1 = c(12.440833, 14.0, 18.19), lon2 = c(11.139333, 16.66, 18.47),
                          lat1 = c(78.89650, 78.13, 77.89), lat2 = c( 79.04633, 78.66, 77.91))

# Norway
trnsct_nor <- data.frame(site = c("Porsangerfjorden"),
                         lat1 = c(69.7), lat2 = c(71.5),
                         lon1 = c(24.5), lon2 = c(27))

# East Greenland
trnsct_east <- data.frame(site = c("Young Sound"),
                          lat1 = c(74.611242), lat2 = c(74.214067),
                          lon1 = c(-22.073400), lon2 = c(-20.043937))

# West Greenland
trnsct_west <- data.frame(site = c("Disko Bay", "Nuup Kangerlua"),
                          lat1 = c(69.15, 64.736), lat2 = c(68.89, 64.074),
                          lon1 = c(-51.49, -50.584), lon2 = c(-54.15, -51.979))


# Hi-res site map files ---------------------------------------------------

# Or rather just save the hi-res data as csv and make the maps dynamically in the app
coastline_hi_sub <- coastline_full_df %>% 
  rename(lon = x, lat = y, group = id) %>%
  filter(between(lon, -55, 28), lat > 59)
write_csv_arrow(coastline_hi_sub, file = "~/WP1/shiny/dataAccess/coastline_hi_sub.csv")

# Convenience wrapper for creating hi-res site maps
map_site <- function(site_name){
  bbox <- bbox_from_name(site_name); bbox_wide <- bbox_wide_from_name(site_name)
  coastline_rename <- coastline_full_df %>% rename(lon = x, lat = y, group = id)
  map_df <- filter(coastline_rename, 
                   between(lon, bbox_wide[1], bbox_wide[2]), 
                   between(lat, bbox_wide[3], bbox_wide[4]))
  map_plot <- ggplot() + 
    geom_polygon(data = map_df, fill = "grey80", colour = "black",
                 aes(x = lon, y = lat, group = group, text = "Land")) +
    coord_quickmap(xlim = bbox[1:2], ylim = bbox[3:4], expand = F) +
    # coord_cartesian(xlim = bbox[1:2], ylim = bbox[3:4], expand = F) +
    labs(x = NULL, y = NULL) + theme_bw() +
    theme(panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
          axis.text = element_text(size = 12, colour = "black"),
          axis.ticks = element_line(colour = "black"), legend.position = "none")
  return(map_plot)
}
map_kong <- map_site("kong")
saveRDS(map_kong, file = "~/WP1/shiny/dataAccess/map_kong.rds")
save(map_kong, file = "~/WP1/shiny/dataAccess/map_kong.RData")
write_csv_arrow(map_filt(bbox_kong_wide), file = "~/WP1/shiny/dataAccess/map_kong.csv")
write_csv_arrow(map_filt(bbox_is_wide), file = "~/WP1/shiny/dataAccess/map_is.csv")
write_csv_arrow(map_filt(bbox_stor_wide), file = "~/WP1/shiny/dataAccess/map_stor.csv")
write_csv_arrow(map_filt(bbox_young_wide), file = "~/WP1/shiny/dataAccess/map_young.csv")
write_csv_arrow(map_filt(bbox_disko_wide), file = "~/WP1/shiny/dataAccess/map_disko.csv")
write_csv_arrow(map_filt(bbox_nuup_wide), file = "~/WP1/shiny/dataAccess/map_nuup.csv")
write_csv_arrow(map_filt(bbox_por_wide), file = "~/WP1/shiny/dataAccess/map_por.csv")


# EU shapefiles -----------------------------------------------------------

# rb <- raster_bathymetry(bathy = paste0(pCloud_path,"FACE-IT_data/shape_files/GEBCO_2020.nc"),
#                         depths = c(25, 50, 100, 200, 500, 1000, 2000, 10000), 
#                         proj.out = "+init=epsg:3995", 
#                         boundary = c(-60, 60, 66, 90))
# bs_bathy <- vector_bathymetry(rb)
# world <- rgdal::readOGR(paste0(pCloud_path,"FACE-IT_data/shape_files/ne_10m_land.shp"), verbose = F)
# islands <- rgdal::readOGR(paste0(pCloud_path,"FACE-IT_data/shape_files/ne_10m_minor_islands.shp"), verbose = F)
# world <- rbind(world, islands)
# bs_land <- clip_shapefile(world, lims)
# bs_land <- sp::spTransform(bs_land, CRSobj = sp::CRS(projection))
# if(!rgeos::gIsValid(bs_land)){  # Has to return TRUE, if not use rgeos::gBuffer
#   bs_land <- rgeos::gBuffer(bs_land, byid = TRUE, width = 0)
# }
# glaciers <- rgdal::readOGR(paste0(pCloud_path,"FACE-IT_data/shape_files/ne_10m_glaciated_areas.shp"), verbose = F)
# if(!rgeos::gIsValid(glaciers)){ # Needs buffering
#   glaciers <- rgeos::gBuffer(glaciers, byid = TRUE, width = 0)
# }
# bs_glacier <- clip_shapefile(glaciers, lims)
# bs_glacier <- sp::spTransform(bs_glacier, CRSobj = sp::CRS(projection))

# EU_shapes <- bbox_to_bathy(bbox_EU)
# save("~/pCloudDrive/FACE-IT_data/shape_files/EU_shapes.RData")
# load(paste0(pCloud_path,"FACE-IT_data/shape_files/EU_shapes.RData"))


# Bathymetry --------------------------------------------------------------

extract_bathy(bbox_sval, "sval")
extract_bathy(bbox_kong, "kong")
extract_bathy(bbox_is, "is")
extract_bathy(bbox_stor, "stor")
extract_bathy(bbox_young, "young")
extract_bathy(bbox_disko, "disko")
extract_bathy(bbox_nuup, "nuup")
extract_bathy(bbox_por, "por")


# Map plots ---------------------------------------------------------------

# Svalbard study sites
map_sval <- basemap(c(7, 30, 75.5, 81.5), bathymetry = T) +
  annotation_spatial(bbox_sval_poly, fill = "darkgreen", colour = "black", alpha = 0.1) +
  geom_spatial_point(data = site_points[c(1,2,4),], size = 9, crs = 4326,
                     aes(x = lon, y = lat), colour = "black") +
  geom_spatial_point(data = site_points[c(1,2,4),], size = 8, crs = 4326,
                     aes(x = lon, y = lat, colour = site)) +
  labs(title = "Svalbard sites", colour = "Site") +
  theme(panel.border = element_rect(colour = "black", fill = NA),
       # legend.position = c(0.899, 0.779),
       legend.box.margin = margin(10, 10, 10, 10),
       legend.box.background = element_rect(fill = "white", colour = "black"))
ggsave("figures/map_svalbard.png", map_sval, width = 7, height = 6)
ggsave("docs/assets/map_svalbard.png", map_sval, width = 7, height = 6)

# Kongsfjorden
# NB: These bathy data are too burly to use with stat_contour
# bathy_kong <- tidync::tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/bathymetry_kongsfjorden/kartverket_5-50m_resolution_Kongsfjorden.nc") %>% 
#   tidync::hyper_tibble() %>% 
#   dplyr::rename(lon = LON, lat = LAT, depth = DEPTH) %>% 
#   mutate(depth = -depth)
map_kong <- bbox_to_map(bbox_kong, bathy_data = NA, lon_pad = 1, lat_pad = 0.2) + ggtitle("Kongsfjorden")
ggsave("figures/map_kongsfjorden.png", map_kong, width = 8, height = 6)
ggsave("docs/assets/map_kongsfjorden.png", map_kong, width = 8, height = 6)

# Isfjorden
map_is <- bbox_to_map(bbox_is, lon_pad = 2, lat_pad = 0.5) + ggtitle("Isfjorden")
ggsave("figures/map_isfjorden.png", map_is, width = 8, height = 6)
ggsave("docs/assets/map_isfjorden.png", map_is, width = 8, height = 6)

# Inglefieldbukta
map_ingle <- bbox_to_map(bbox_ingle, lon_pad = 1, lat_pad = 0.2) + ggtitle("Inglefieldbukta")
ggsave("figures/map_inglefieldbukta.png", map_ingle, width = 8, height = 6)
ggsave("docs/assets/map_inglefieldbukta.png", map_ingle, width = 8, height = 6)

# Storfjorden
map_stor <- bbox_to_map(bbox_stor, lon_pad = 2, lat_pad = 0.5) + ggtitle("Storfjorden")
ggsave("figures/map_storfjorden.png", map_stor, width = 8, height = 6)
ggsave("docs/assets/map_storfjorden.png", map_stor, width = 8, height = 6)

# Young Sound
bathy_young <- tidync::tidync("~/pCloudDrive/FACE-IT_data/maps/ys_bathy_v3.0_raw.nc") %>% 
  tidync::hyper_tibble() %>% 
  dplyr::rename(lon = Longitude, lat = Latitude) %>% 
  mutate(depth = -Bathymetry)
map_young <- bbox_to_map(bbox_young, lon_pad = 1, lat_pad = 0.2,
                         bathy_data = bathy_young) + ggtitle("Young Sound")
ggsave("figures/map_young_sound.png", map_young, width = 12, height = 6)
ggsave("docs/assets/map_young_sound.png", map_young, width = 12, height = 6)

## Western Greenland study sites
# Disko Bay
map_disko <- bbox_to_map(bbox_disko, lon_pad = 2, lat_pad = 0.5) + ggtitle("Disko Bay")
ggsave("figures/map_disko_bay.png", map_disko, width = 6, height = 6)
ggsave("docs/assets/map_disko_bay.png", map_disko, width = 6, height = 6)

# Nuup Kangerlua
map_nuup <- bbox_to_map(bbox_nuup, lon_pad = 1.5, lat_pad = 0.35) + ggtitle("Nuup Kangerlua")
ggsave("figures/map_nuup_kangerlua.png", map_nuup, width = 11, height = 6)
ggsave("docs/assets/map_nuup_kangerlua.png", map_nuup, width = 11, height = 6)

# Porsangerfjorden
map_por <- bbox_to_map(bbox_por, lon_pad = 1, lat_pad = 0.3) + ggtitle("Porsangerfjorden")
ggsave("figures/map_porsangerfjorden.png", map_por, width = 6, height = 6)
ggsave("docs/assets/map_porsangerfjorden.png", map_por, width = 6, height = 6)

# Full study area
map_full <- basemap(limits = c(-60, 60, 60, 90), bathymetry = T) +
  annotation_spatial(bbox_EU_poly, fill = "darkgreen", colour = "black", alpha = 0.1) +
  geom_spatial_point(data = site_points[-3,], size = 9, crs = 4326,
                     aes(x = lon, y = lat), colour = "black") +
  geom_spatial_point(data = site_points[-3,], size = 8, crs = 4326,
                     aes(x = lon, y = lat, colour = site)) +
  labs(title = "FACE-IT study area and focal sites",
       colour = "Site",
       caption = "robert.schlegel@imev-mer.fr\nSorbonne Université") +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        legend.position = c(0.948, 0.29),
        # legend.margin = margin(10, 10, 10, 10),
        legend.box.margin = margin(10, 10, 10, 10), 
        legend.box.background = element_rect(fill = "white", colour = "black"))
ggsave("figures/map_full.png", map_full, height = 10, width = 16)
ggsave("docs/assets/map_full.png", map_full, height = 10, width = 16)

# Assemble smaller figures
map_small <- ggpubr::ggarrange(map_kong, map_is, map_stor, map_young, map_disko, map_nuup, map_por, logo, ncol = 3, nrow = 3)

# Put them together
map_all <- ggpubr::ggarrange(map_full, map_small, ncol = 1, nrow = 2, heights = c(0.7, 1))
ggsave("figures/map_all.png", map_all, height = 24, width = 16)
ggsave("docs/assets/map_all.png", map_all, height = 24, width = 16)


# Bathy test --------------------------------------------------------------

# Test the large bathy NetCDF processed by Pedro

bathy_kong <- tidync::tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/bathymetry_kongsfjorden/kartverket_5-50m_resolution_Kongsfjorden.nc") %>% 
  tidync::hyper_tibble()

bathy_kong_plot <- ggplot(data = bathy_kong, aes(x = LON, y = LAT)) +
  geom_point(aes(colour = DEPTH), size = 0.001) +
  scale_colour_viridis_c() +
  labs(x = NULL, y = NULL)
ggsave("figures/map_kong_hires_bathy.png", bathy_kong_plot, height = 10, width = 12)


# Hi-res Kong bathy from Norsk Polarinstitut
# NB: This takes several minutes to load and then fails
bathy_Norsk_kong <- readGDAL("data/restricted/NP_S100_Raster_10m/S100_Raster_10m.jp2")

# This works but creates an enormous file
gdalUtils::gdal_translate(src_dataset = "data/restricted/NP_S100_Raster_10m/S100_Raster_10m.jp2", 
                          dst_dataset = "data/restricted/NP_S100_Raster_10m/S100_Raster_10m.tif", verbose = TRUE)

# After the conversion
bathy_Norsk_kong <- readGDAL("data/restricted/NP_S100_Raster_10m/S100_Raster_10m.tif")


# Data problems -----------------------------------------------------------

# Figures that highlight where we need to change the bbox for the study site

# Kongsfjorden
## See e-mail from Allison Bailey for feedback
load("~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")
coastline_kong <- coastline_full_df %>% 
  filter(x >= bbox_kong[1]-1, x <= bbox_kong[2]+1, y >= bbox_kong[3]-1, y <= bbox_kong[4]+1)
plot_problems_kong <- full_product_kong %>%
  mutate(lon = round(lon, 2), lat = round(lat, 2)) %>% 
  group_by(lon, lat) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  ggplot() +
  geom_polygon(data = coastline_kong, fill = "grey70", colour = "black",
               aes(x = x, y = y, group = polygon_id)) +
  geom_tile(aes(x = lon, y = lat, fill = log10(count))) +
  annotate("rect", colour = "black", fill = NA,
           xmin = bbox_kong[1], xmax = bbox_kong[2], ymin = bbox_kong[3], ymax = bbox_kong[4]) +
  annotate("rect",  colour = "red", fill = "red", alpha = 0.1,
           xmin = bbox_kong[1], xmax = 11.5, ymin = bbox_kong[3], ymax = 78.95) +
  annotate("rect", colour = "red", fill = "red", alpha = 0.1,
           xmin = bbox_kong[1], xmax = 11.34, ymin = 78.95, ymax = bbox_kong[4]) +
  scale_fill_viridis_c(option = "E") +
  coord_quickmap(expand = F,
                 xlim = c(bbox_kong[1]-0.3, bbox_kong[2]+0.3), 
                 ylim = c(bbox_kong[3]-0.05, bbox_kong[4]+0.05)) +
  labs(x = NULL, y = NULL, fill = "Count\n(log10)",
       title = paste0("Count of data binned at 0.01° (~1 km) resolution")) +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        legend.position = "bottom")
plot_problems_kong
ggsave("figures/bbox_kong.png", plot_problems_kong)

# Isfjorden
## See e-mail from Janne
load("~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.RData")
coastline_is <- coastline_full_df %>% 
  filter(x >= bbox_is[1]-1, x <= bbox_is[2]+1, y >= bbox_is[3]-1, y <= bbox_is[4]+1)
plot_problems_is <- full_product_is %>%
  mutate(lon = round(lon, 2), lat = round(lat, 2)) %>% 
  group_by(lon, lat) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  ggplot() +
  geom_polygon(data = coastline_is, fill = "grey70", colour = "black",
               aes(x = x, y = y, group = polygon_id)) +
  geom_tile(aes(x = lon, y = lat, fill = log10(count))) +
  annotate("rect", colour = "black", fill = NA,
           xmin = bbox_is[1], xmax = bbox_is[2], ymin = bbox_is[3], ymax = bbox_is[4]) +
  scale_fill_viridis_c(option = "E") +
  coord_quickmap(expand = F,
                 xlim = c(bbox_is[1]-0.5, bbox_is[2]+0.4), 
                 ylim = c(bbox_is[3]-0.1, bbox_is[4]+0.2)) +
  labs(x = NULL, y = NULL, fill = "Count\n(log10)",
       title = paste0("Count of data binned at 0.01° (~1 km) resolution")) +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        legend.position = "bottom")
plot_problems_is
ggsave("figures/bbox_is.png", plot_problems_is, height = 6)

# Inglefieldbukta
## See e-mail from Janne
load("~/pCloudDrive/FACE-IT_data/inglefieldbukta/full_product_ingle.RData")
coastline_ingle <- coastline_full_df %>% 
  filter(x >= bbox_ingle[1]-1, x <= bbox_ingle[2]+1, y >= bbox_ingle[3]-1, y <= bbox_ingle[4]+1)
plot_problems_ingle <- full_product_ingle %>%
  mutate(lon = round(lon, 2), lat = round(lat, 2)) %>% 
  group_by(lon, lat) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  ggplot() +
  geom_polygon(data = coastline_ingle, fill = "grey70", colour = "black",
               aes(x = x, y = y, group = polygon_id)) +
  geom_tile(aes(x = lon, y = lat, fill = log10(count))) +
  annotate("rect",  colour = "green", fill = NA, alpha = 0.1,
           xmin = bbox_ingle[1], xmax = bbox_ingle[2], ymin = bbox_ingle[3], ymax = bbox_ingle[4]+0.03) +
  annotate("rect", colour = "black", fill = NA,
           xmin = bbox_ingle[1], xmax = bbox_ingle[2], ymin = bbox_ingle[3], ymax = bbox_ingle[4]) +
  scale_fill_viridis_c(option = "E") +
  coord_quickmap(expand = F,
                 xlim = c(bbox_ingle[1]-0.5, bbox_ingle[2]+0.4), 
                 ylim = c(bbox_ingle[3]-0.1, bbox_ingle[4]+0.2)) +
  labs(x = NULL, y = NULL, fill = "Count\n(log10)",
       title = paste0("Count of data binned at 0.01° (~1 km) resolution")) +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        legend.position = "bottom")
plot_problems_ingle
ggsave("figures/bbox_ingle.png", plot_problems_ingle, height = 6)

# Young Sound
## This appears fine. No issues.
load("~/pCloudDrive/FACE-IT_data/young_sound/full_product_young.RData")
coastline_young <- coastline_full_df %>% 
  filter(x >= bbox_young[1]-2, x <= bbox_young[2]+2, y >= bbox_young[3]-2, y <= bbox_young[4]+2)
plot_problems_young <- full_product_young %>%
  mutate(lon = round(lon, 2), lat = round(lat, 2)) %>% 
  group_by(lon, lat) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  ggplot() +
  geom_polygon(data = coastline_young, fill = "grey70", colour = "black",
               aes(x = x, y = y, group = polygon_id)) +
  geom_tile(aes(x = lon, y = lat, fill = log10(count))) +
  annotate("rect", colour = "black", fill = NA,
           xmin = bbox_young[1], xmax = bbox_young[2], ymin = bbox_young[3], ymax = bbox_young[4]) +
  scale_fill_viridis_c(option = "E") +
  coord_quickmap(expand = F,
                 xlim = c(bbox_young[1]-0.5, bbox_young[2]+0.4), 
                 ylim = c(bbox_young[3]-0.1, bbox_young[4]+0.2)) +
  labs(x = NULL, y = NULL, fill = "Count\n(log10)",
       title = paste0("Count of data binned at 0.01° (~1 km) resolution")) +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        legend.position = "bottom")
plot_problems_young
ggsave("figures/bbox_young.png", plot_problems_young, width = 6)

# Disko Bay
## Thomas is happy with the bbox as is
load("~/pCloudDrive/FACE-IT_data/disko_bay/full_product_disko.RData")
coastline_disko <- coastline_full_df %>% 
  filter(x >= bbox_disko[1]-10, x <= bbox_disko[2]+10, y >= bbox_disko[3]-10, y <= bbox_disko[4]+10)
plot_problems_disko <- full_product_disko %>%
  mutate(lon = round(lon, 1), lat = round(lat, 1)) %>% 
  group_by(lon, lat) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  ggplot() +
  geom_polygon(data = coastline_disko, fill = "grey70", colour = "black",
               aes(x = x, y = y, group = polygon_id)) +
  geom_tile(aes(x = lon, y = lat, fill = log10(count))) +
  annotate("rect", colour = "black", fill = NA,
           xmin = bbox_disko[1], xmax = bbox_disko[2], ymin = bbox_disko[3], ymax = bbox_disko[4]) +
  scale_fill_viridis_c(option = "E") +
  coord_quickmap(expand = F,
                 xlim = c(bbox_disko[1]-0.5, bbox_disko[2]+0.5), 
                 ylim = c(bbox_disko[3]-0.2, bbox_disko[4]+0.2)) +
  labs(x = NULL, y = NULL, fill = "Count\n(log10)",
       title = paste0("Count of data binned at 0.1° (~10 km) resolution")) +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        legend.position = "bottom")
plot_problems_disko
ggsave("figures/bbox_disko.png", plot_problems_disko, height = 7)

# Nuup Kangerlua
## Thomas is happy with the bbox as is
## The extra areas can be used to inform mouth water characteristics
load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/full_product_nuup.RData")
coastline_nuup <- coastline_full_df %>% 
  filter(x >= bbox_nuup[1]-10, x <= bbox_nuup[2]+10, y >= bbox_nuup[3]-10, y <= bbox_nuup[4]+10)
plot_problems_nuup <- full_product_nuup %>%
  mutate(lon = round(lon, 2), lat = round(lat, 2)) %>% 
  group_by(lon, lat) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  ggplot() +
  geom_polygon(data = coastline_nuup, fill = "grey70", colour = "black",
               aes(x = x, y = y, group = polygon_id)) +
  geom_tile(aes(x = lon, y = lat, fill = log10(count))) +
  annotate("rect",  colour = "red", fill = "red", alpha = 0.1,
           xmin = bbox_nuup[1], xmax = -52.2, ymin = bbox_nuup[3], ymax = bbox_nuup[4]) +
  annotate("rect",  colour = "red", fill = "red", alpha = 0.1,
           xmin = -52.2, xmax = -51.8, ymin = 64.65, ymax = bbox_nuup[4]) +
  annotate("rect", colour = "black", fill = NA,
           xmin = bbox_nuup[1], xmax = bbox_nuup[2], ymin = bbox_nuup[3], ymax = bbox_nuup[4]) +
  scale_fill_viridis_c(option = "E") +
  coord_quickmap(expand = F,
                 xlim = c(bbox_nuup[1]-0.5, bbox_nuup[2]+0.5), 
                 ylim = c(bbox_nuup[3]-0.2, bbox_nuup[4]+0.2)) +
  labs(x = NULL, y = NULL, fill = "Count\n(log10)",
       title = paste0("Count of data binned at 0.01° (~1 km) resolution")) +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        legend.position = "bottom")
plot_problems_nuup
ggsave("figures/bbox_nuup.png", plot_problems_nuup, width = 7)

# Porsangerfjorden
## See e-mail from Lis for figure with more refined bbox
load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/full_product_por.RData")
coastline_por <- coastline_full_df %>% 
  filter(x >= bbox_por[1]-10, x <= bbox_por[2]+10, y >= bbox_por[3]-10, y <= bbox_por[4]+10)
plot_problems_por <- full_product_por %>%
  mutate(lon = round(lon, 2), lat = round(lat, 2)) %>% 
  group_by(lon, lat) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  ggplot() +
  geom_polygon(data = coastline_por, fill = "grey70", colour = "black",
               aes(x = x, y = y, group = polygon_id)) +
  geom_tile(aes(x = lon, y = lat, fill = log10(count))) +
  annotate("rect",  colour = "red", fill = "red", alpha = 0.1,
           xmin = 26.3, xmax = bbox_por[2], ymin = 70.3, ymax = 70.75) +
  annotate("rect",  colour = "red", fill = "red", alpha = 0.1,
           xmin = 26.65, xmax = bbox_por[2], ymin = 70.75, ymax = 70.97) +
  annotate("rect",  colour = "red", fill = "red", alpha = 0.1,
           xmin = bbox_por[1], xmax = 25.5, ymin = 70.75, ymax = bbox_por[4]) +
  annotate("rect",  colour = "red", fill = "red", alpha = 0.1,
           xmin = bbox_por[1], xmax = 24.9, ymin = 70.55, ymax = 70.75) +
  annotate("rect", colour = "black", fill = NA,
           xmin = bbox_por[1], xmax = bbox_por[2], ymin = bbox_por[3], ymax = bbox_por[4]) +
  scale_fill_viridis_c(option = "E") +
  coord_quickmap(expand = F,
                 xlim = c(bbox_por[1]-0.5, bbox_por[2]+0.5), 
                 ylim = c(bbox_por[3]-0.2, bbox_por[4]+0.2)) +
  labs(x = NULL, y = NULL, fill = "Count\n(log10)",
       title = paste0("Count of data binned at 0.01° (~1 km) resolution")) +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        legend.position = "bottom")
plot_problems_por
ggsave("figures/bbox_por.png", plot_problems_por, height = 7)


# Site regions ------------------------------------------------------------

# Cut up the bounding boxes to smaller fjord relevant regions

# Kongsfjorden
## Load data
load("~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")
coastline_kong <- coastline_full_df %>% 
  dplyr::filter(x >= bbox_kong[1]-1, x <= bbox_kong[2]+1, y >= bbox_kong[3]-1, y <= bbox_kong[4]+1)
full_product_kong_coords <- full_product_kong %>% 
  dplyr::select(lon, lat) %>% distinct()

## Manually create regions
# bbox_kong <- c(11, 12.69, 78.86, 79.1)
### TODO: Probably more effective to carve out these shapes from the hi-res coastline polygons
bbox_regions_kong <- data.frame(region = factor(c("Inner", "Mid", "Outer", "Mouth", "Discard"),
                                                levels = c("Inner", "Mid", "Outer", "Mouth", "Discard")),
                                lon1 = c(12.2, 11.7, 11.34, 11, 11),
                                lon2 = c(12.69, 12.2, 11.7, 11.34, 11.5),
                                lat1 = c(78.86, 78.86, 78.86, 78.95, 78.86),
                                lat2 = c(79.1, 79.1, 79.1, 79.1, 78.95))

## Find data in regions
bbox_regions_kong_sub <- filter(bbox_regions_kong, region == "Inner")
sp::point.in.polygon(point.x = full_product_kong_coords[["lon"]], point.y = full_product_kong_coords[["lat"]],
                     pol.x = bbox_regions_kong_sub[["lon"]], pol.y = bbox_regions_kong_sub[["lat"]])
test <- points_in_region("Outer", bbox_regions_kong, full_product_kong_coords)
full_region_kong <- plyr::ldply(unique(bbox_regions_kong$region), points_in_region, .parallel = F, 
                                bbox_df = bbox_regions_kong, data_df = full_product_kong_coords)
region_labels_kong <- full_region_kong %>% 
  group_by(region) %>% 
  summarise(lon = mean(range(lon)),
            lat = mean(range(lat)),
            count = n(), .groups = "drop")

## Plot
plot_regions_kong <- ggplot() +
  geom_polygon(data = coastline_kong, fill = "grey70", colour = "black",
               aes(x = x, y = y, group = polygon_id)) +
  # geom_polygon(data = bbox_regions_kong, aes(x = lon, y = lat, group = region, fill = region)) +
  # geom_tile(aes(x = lon, y = lat)) +
  geom_rect(data = bbox_regions_kong, aes(xmin = lon1, xmax = lon2, ymin = lat1, ymax = lat2, fill = region), alpha = 0.3) +
  geom_point(data = distinct(full_region_kong), aes(x = lon, y = lat, colour = region), show.legend = F) +
  geom_label(data = region_labels_kong, aes(x = lon, y = lat, label = paste0("n = ",count)), alpha = 0.8) +
  # geom_point(data = distinct(dplyr::select(coords_in, lon, lat, in_grid)), aes(x = lon, y = lat, colour = as.factor(in_grid))) +
  # annotate("rect", colour = "black", fill = NA,
  #          xmin = bbox_kong[1], xmax = bbox_kong[2], ymin = bbox_kong[3], ymax = bbox_kong[4]) +
  # annotate("rect",  colour = "red", fill = "red", alpha = 0.1,
  #          xmin = bbox_kong[1], xmax = 11.5, ymin = bbox_kong[3], ymax = 78.95) +
  # annotate("rect", colour = "red", fill = "red", alpha = 0.1,
  #          xmin = bbox_kong[1], xmax = 11.34, ymin = 78.95, ymax = bbox_kong[4]) +
  # scale_fill_viridis_c(option = "E") +
  coord_quickmap(expand = F,
                 xlim = c(bbox_kong[1]-0.3, bbox_kong[2]+0.3), 
                 ylim = c(bbox_kong[3]-0.05, bbox_kong[4]+0.05)) +
  labs(x = NULL, y = NULL, fill = "Region",
       title = paste0("")) +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        legend.position = "bottom")
plot_regions_kong
ggsave("figures/regions_kong.png", plot_regions_kong)


# Map requests ------------------------------------------------------------

# From Annika:
# Nuup Kangerlua, Isfjorden and Porsangerfjorden
# It would be great if you can add the names and locations of major settlements. 
# In the workshop discussions, there was a fair amount of focus on land as well. 
# Would it be possible to add topographical detail and/or key features, such as 
# glaciers, for the land or use some other trick to make it as important as the water part?

# Nuup Kangerlua
ggOceanMaps::basemap(limits = bbox_nuup, bathymetry = TRUE, glaciers = TRUE, legend.position = "bottom",
                     bathy.style = "poly_blues", gla.border.col = "thistle1", gla.col = "snow") +
  # geom_spatial_point(aes(x = -51.8422923, y = 64.259721)) +
  geom_spatial_label(aes(x = -51.8422923, y = 64.259721, label = "Nuuk")) +
  geom_spatial_label(aes(x = -50.3450035, y = 64.50, label = "Neriunaq")) +
  geom_spatial_label(aes(x = -50.2744783, y = 64.37, label = "Kapisillit")) +
  labs(title = "Nuup Kangerlua", x = NULL, y = NULL) +
  theme(plot.background = element_rect(colour = NA, fill = "white"))
ggsave("figures/requests/map_nuup_WP4.png", height = 6, width = 6)

# Isfjorden
ggOceanMaps::basemap(limits = bbox_is_wide, bathymetry = TRUE, glaciers = TRUE, legend.position = "bottom",
                     bathy.style = "poly_blues", gla.border.col = "thistle1", gla.col = "snow") +
  geom_spatial_label(aes(x = 15.48789, y = 78.2253587, label = "Longyearbyen")) +
  geom_spatial_label(aes(x = 14.186268, y = 78.0664196, label = "Barentsburg")) +
  geom_spatial_label(aes(x = 16.3262847, y = 78.6565062, label = "Pyramiden")) +
  geom_spatial_label(aes(x = 11.6861781, y = 78.8756255, label = "Ny-Ålesund")) +
  labs(title = "Svalbard/Isfjorden", x = NULL, y = NULL) +
  theme(plot.background = element_rect(colour = NA, fill = "white"))
ggsave("figures/requests/map_is_WP4.png", height = 6, width = 6)

# Porsangerfjorden
ggOceanMaps::basemap(limits = bbox_por, bathymetry = TRUE, glaciers = TRUE, legend.position = "bottom",
                     bathy.style = "poly_blues", gla.border.col = "thistle1", gla.col = "snow") +
  geom_spatial_label(aes(x = 24.9, y = 70.022, label = "Lakselv")) +
  geom_spatial_label(aes(x = 24.9014675, y = 70.2073595, label = "Bevkop")) +
  geom_spatial_label(aes(x = 25.0441326, y = 70.3154637, label = "Pillavuono")) +
  geom_spatial_label(aes(x = 25.3, y = 70.035, label = "Nyby")) +
  geom_spatial_label(aes(x = 25.5447693, y = 70.318202, label = "Børselv")) +
  labs(title = "Porsangerfjorden", x = NULL, y = NULL) +
  theme(plot.background = element_rect(colour = NA, fill = "white"))
ggsave("figures/requests/map_por_WP4.png", height = 6, width = 6)


# code/study_sites.R
# This script houses code used in managing study site info

# Notes on site coordinators contacted for bbox and transect coordinates
# Western Greenland: Nuup Kangerlua (Godthåbsfjord) (GF), Qeqertarsuup Tunua (Disko Bay) (DB): Thomas Juul-Pedersen
# Eastern Greenland: Young Sound (YS): Mikael Kristian Sejr
# Svalbard: Kongsfjorden (KF): Allison Bailey
# Svalbard: Isfjorden (IF), Inglefieldbukta (IB) and Agardhbukta: Janne Søreide; UNIS - Borge Damsgard
# Norway: Porsangerfjorden (PF), Finnmark: Lis Lindal Jørgensen

# Search the literature for the necessary bounding boxes and transects
# Isfjorden needs to be extended to the east
# Add the bathymetry data from JP to the young sound bounding box
# IBCAO for bathy data
# Add bathymetry for all of the sites
# Use ggOceanMaps to do this


# Setup -------------------------------------------------------------------

# Start with common project code
source("code/functions.R")


# Single points -----------------------------------------------------------

site_points <- data.frame(site = c("Kongsfjorden", "Isfjorden", "Inglefieldbukta", "Young Sound", 
                                   "Disko Bay", "Nuup Kangerlua", "Porsangerfjorden"),
                          lon = c(11.845, 14.365, 18.31, -21.237, -52.555, -50.625, 25.75),
                          lat = c(78.98, 78.235, 77.87, 74.517, 69.36, 64.405, 70.6))


# Bounding boxes ----------------------------------------------------------

# European Arctic
bbox_EU <- data.frame(lon1 = -60, lon2 = 60,
                      lat1 = 66, lat2 = 90)
bbox_EU_poly <- bbox_to_poly(c(-60, 60, 66, 90), "EU")

# Svalbard
bbox_kong <- bbox_to_poly(c(11, 12.69, 78.86, 79.1), "Kongsfjorden")
bbox_is <- bbox_to_poly(c(12.95, 15.78, 78.04, 78.43), "Isfjorden")
bbox_ingle <- bbox_to_poly(c(18.04, 18.58, 77.92, 77.82), "Inglefieldbukta")
# bbox_sval <- data.frame(site = c("Kongsfjorden", "Isfjorden", "Inglefieldbukta"),
#                         lon1 = c(11, 12.95, 18.04), lon2 = c(12.69, 15.78, 18.58),
#                         lat1 = c(78.86, 78.04, 77.92), lat2 = c(79.1, 78.43, 77.82))
# bbox_sval_poly <- nest(group_by(bbox_sval, site)) %>%
#   mutate(data = map(data, bbox_to_poly, ID = site))

# Eastern Greenland
bbox_young <- bbox_to_poly(c(-22.367917, -19.907644, 74.210137, 74.624304), "Young Sound")
# bbox_east <- data.frame(site = c("Young Sound"),
#                         lon1 = c(-22.367917), lon2 = c(-19.907644),
#                         lat1 = c(74.210137), lat2 = c(74.624304))
# bbox_east_poly <- bbox_to_poly(bbox_east)

# Western Greenland
bbox_disko <- bbox_to_poly(c(-55.56, -49.55, 68.22, 70.5), "Disko Bay")
bbox_nuup <- bbox_to_poly(c(-53.32, -48.93, 64.01, 64.8), "Nuup Kangerlua")
# bbox_west <- data.frame(site = c("Disko Bay", "Nuup Kangerlua"),
#                         lon1 = c(-55.56, -52.32), lon2 = c(-49.55, -48.93),
#                         lat1 = c(68.22, 64.01), lat2 = c(70.5, 64.8))
# bbox_west_poly <- nest(group_by(bbox_west, site)) %>%
#   mutate(data = map(data, bbox_to_poly, ID = site))

# Norway
bbox_por <- bbox_to_poly(c(24.5, 27, 70, 71.2), "Porsangerfjorden")
# bbox_nor <- data.frame(site = c("Porsangerfjorden"),
#                        lon1 = c(24.5), lon2 = c(27),
#                        lat1 = c(70), lat2 = c(71.2))
# bbox_nor_poly <- bbox_to_poly(bbox_nor)


# Transects ---------------------------------------------------------------

# Svalbard
trnsct_sval <- data.frame(site = c("Kongsfjorden", "Isfjorden", "Inglefieldbukta"),
                          lon1 = c(12.440833, 15.1, 18.19),
                          lon2 = c(11.139333, 13.39, 18.47),
                          lat1 = c(78.89650, 78.32, 77.89),
                          lat2 = c( 79.04633, 78.13, 77.91))

# Norway
trnsct_nor <- data.frame(site = c("Porsangerfjorden"),
                         lat1 = c(69.7),
                         lat2 = c(71.5),
                         lon1 = c(24.5),
                         lon2 = c(27))

# East Greenland
trnsct_east <- data.frame(site = c("Young Sound"),
                          lat1 = c(74.611242),
                          lat2 = c(74.214067),
                          lon1 = c(-22.073400),
                          lon2 = c(-20.043937))

# West Greenland
trnsct_west <- data.frame(site = c("Disko Bay", "Nuup Kangerlua"),
                          lat1 = c(69.15, 64.736),
                          lat2 = c(68.89, 64.074),
                          lon1 = c(-51.49, -50.584),
                          lon2 = c(-54.15, -51.979))


# Map ---------------------------------------------------------------------

# Svalbard study sites
map_sval <- bbox_to_ggOcean(c(9, 30, 76, 81)) +
  geom_spatial_point(data = site_points[1:3,], size = 9, crs = 4326,
                     aes(x = lon, y = lat, colour = site)) +
  ggtitle("Svalbard")
ggsave("figures/map_svalbard.png", map_sval, width = 12, height = 6)

# Kongsfjorden
map_kong <- bbox_to_ggOcean(bbox_kong) + ggtitle("Kongsfjorden")
ggsave("figures/map_kongsfjorden.png", map_kong, width = 8, height = 6)

# Isfjorden
map_is <- bbox_to_ggOcean(bbox_is) + ggtitle("Isfjorden")
ggsave("figures/map_isfjorden.png", map_is, width = 8, height = 6)

# Porsangerfjorden
map_por <- bbox_to_ggOcean(bbox_por) + ggtitle("Porsangerfjorden")
ggsave("figures/map_kongsfjorden.png", map_por, width = 12, height = 6)

# Eastern Greenland study sites
map_east <- bbox_to_ggOcean(bbox_east, lon_pad = 0.2, lat_pad = 0.1,
                            bathy_file = paste0(pCloud_path,"FACE-IT_data/shape_files/ys_bathy_v3.0_raw.nc")) +
  annotation_spatial(bbox_east_poly, fill = "cadetblue1", colour = "black", alpha = 0.1) 
map_east

# Western Greenland study sites
map_west <- bbox_to_ggOcean(bbox_west, lon_pad = 0.5, lat_pad = 0.5,
                            depths = c(0, 50, 100, 200, 500, 1000, 2000)) +
  geom_spatial_point(data = site_points[5:6, ], size = 2, crs = 4326,
                     aes(x = lon, y = lat), colour = "black") +
  annotation_spatial(bbox_west_poly$data[[2]], fill = "cadetblue1", colour = "black", alpha = 0.1) +
  annotation_spatial(bbox_west_poly$data[[1]], fill = "forestgreen", colour = "black", alpha = 0.1)
map_west

# Full study area
map_full <- basemap(limits = c(-50, 60, 60, 90), bathymetry = T) +
  annotation_spatial(bbox_EU_poly, fill = "cadetblue1", colour = "black", alpha = 0.1) +
  geom_spatial_point(data = site_points, size = 9, crs = 4326,
                     aes(x = lon, y = lat), colour = "black") +
  geom_spatial_point(data = site_points, size = 8, crs = 4326,
                     aes(x = lon, y = lat, colour = site)) +
  labs(colour = "Site") +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        legend.position = c(0.948, 0.276),
        # legend.margin = margin(10, 10, 10, 10),
        legend.box.margin = margin(10, 10, 10, 10), 
        legend.box.background = element_rect(fill = "white", colour = "black"))
map_full
ggsave("figures/map_full.png", map_full, height = 10, width = 16)

# Assemble smaller figures
map_small <- ggpubr::ggarrange(map_sval, map_fin, map_east, map_west, ncol = 2, nrow = 2)

# Put them together
map_all <- ggpubr::ggarrange(map_full, map_small, ncol = 2)
ggsave("figures/map_all.png", map_all, height = 10, width = 16)
map_all


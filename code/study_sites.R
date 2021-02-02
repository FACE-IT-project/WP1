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
bbox_EU <- bbox_to_poly(c(-60, 60, 63, 90), "EU")

# Svalbard
bbox_kong <- bbox_to_poly(c(11, 12.69, 78.86, 79.1), "Kongsfjorden")
bbox_is <- bbox_to_poly(c(12.95, 15.78, 78.04, 78.43), "Isfjorden")
bbox_ingle <- bbox_to_poly(c(18.04, 18.58, 77.92, 77.82), "Inglefieldbukta")

# Eastern Greenland
bbox_young <- bbox_to_poly(c(-22.367917, -19.907644, 74.210137, 74.624304), "Young Sound")

# Western Greenland
bbox_disko <- bbox_to_poly(c(-55.56, -49.55, 68.22, 70.5), "Disko Bay")
bbox_nuup <- bbox_to_poly(c(-53.32, -48.93, 64.01, 64.8), "Nuup Kangerlua")

# Norway
bbox_por <- bbox_to_poly(c(24.5, 27, 70, 71.2), "Porsangerfjorden")


# Transects ---------------------------------------------------------------

# Svalbard
trnsct_sval <- data.frame(site = c("Kongsfjorden", "Isfjorden", "Inglefieldbukta"),
                          lon1 = c(12.440833, 15.1, 18.19), lon2 = c(11.139333, 13.39, 18.47),
                          lat1 = c(78.89650, 78.32, 77.89), lat2 = c( 79.04633, 78.13, 77.91))

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


# Map ---------------------------------------------------------------------

# Svalbard study sites
map_sval <- basemap(c(9, 30, 76, 81)) +
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

# Inglefieldbukta
map_ingle <- bbox_to_ggOcean(bbox_ingle) + ggtitle("Inglefieldbukta")
ggsave("figures/map_inglefieldbukta.png", map_ingle, width = 8, height = 6)

# Porsangerfjorden
map_por <- bbox_to_ggOcean(bbox_por) + ggtitle("Porsangerfjorden")
ggsave("figures/map_porsangerfjorden.png", map_por, width = 6, height = 6)

# Young Sound
map_young <- bbox_to_ggOcean(bbox_young, lon_pad = 0.2, lat_pad = 0.1,
                             bathy_file = paste0(pCloud_path,"FACE-IT_data/shape_files/ys_bathy_v3.0_raw.nc")) +
  annotation_spatial(bbox_young, fill = "cadetblue1", colour = "black", alpha = 0.1) 
ggsave("figures/map_young_sound.png", map_young, width = 12, height = 6)

## Western Greenland study sites
# Disko Bay
map_disko <- bbox_to_ggOcean(bbox_disko) + ggtitle("Disko Bay")
ggsave("figures/map_disko_bay.png", map_disko, width = 6, height = 6)

# Nuup Kangerlua
map_nuup <- bbox_to_ggOcean(bbox_nuup) + ggtitle("Nuup Kangerlua")
ggsave("figures/map_nuup_kangerlua.png", map_nuup, width = 6, height = 6)

# Full study area
map_full <- basemap(limits = c(-60, 60, 60, 90), bathymetry = T) +
  annotation_spatial(bbox_EU, fill = "cadetblue1", colour = "black", alpha = 0.1) +
  geom_spatial_point(data = site_points, size = 9, crs = 4326,
                     aes(x = lon, y = lat), colour = "black") +
  geom_spatial_point(data = site_points, size = 8, crs = 4326,
                     aes(x = lon, y = lat, colour = site)) +
  labs(colour = "Site") +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        legend.position = c(0.948, 0.29),
        # legend.margin = margin(10, 10, 10, 10),
        legend.box.margin = margin(10, 10, 10, 10), 
        legend.box.background = element_rect(fill = "white", colour = "black"))
ggsave("figures/map_full.png", map_full, height = 10, width = 16)

# Assemble smaller figures
map_small <- ggpubr::ggarrange(map_kong, map_is, map_ingle, map_young, map_disko, map_nuup, map_por, ncol = 2, nrow = 4)

# Put them together
map_all <- ggpubr::ggarrange(map_full, map_small, ncol = 2, widths = c(2, 1))
ggsave("figures/map_all.png", map_all, height = 10, width = 24)


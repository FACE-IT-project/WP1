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
# devtools:::install_github("gearslaboratory/gdalUtils")
source("code/functions.R")
library(ggOceanMaps)
library(ggOceanMapsData)
library(gdalUtils)
library(stars)
library(sfheaders)
sf_use_s2(FALSE) # Fixes cropping issues with polygons

# Load FACE-IT logo
logo <- grid::rasterGrob(png::readPNG("FACE-IT_Logo_900.png"), interpolate = TRUE)

# Remove scientific notation
options(scipen=999)


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

# Load shape
if(!exists("coastline_full")) load("metadata/coastline_full_df.RData")

# Or rather just save the hi-res data as csv and make the maps dynamically in the app
coastline_hi_sub <- coastline_full_df %>% 
  rename(lon = x, lat = y, group = id) %>%
  filter(between(lon, -55, 28), lat > 59)
write_csv_arrow(coastline_hi_sub, file = "~/WP1/shiny/dataAccess/coastline_hi_sub.csv")

# Convenience wrapper for creating hi-res site maps
map_site <- function(site_name){
  bbox <- bbox_from_name(site_name); bbox_wide <- bbox_wide_from_name(site_name)
  if(!exists("coastline_full")) load("metadata/coastline_full_df.RData")
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


# Bathy prep --------------------------------------------------------------

# Test the large bathy NetCDF processed by Pedro
bathy_kong <- tidync::tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/bathymetry_kongsfjorden/kartverket_5-50m_resolution_Kongsfjorden.nc") %>% 
  tidync::hyper_tibble()

# Plot data
bathy_kong_plot <- ggplot(data = bathy_kong, aes(x = LON, y = LAT)) +
  geom_point(aes(colour = DEPTH), size = 0.001) +
  scale_colour_viridis_c() +
  labs(x = NULL, y = NULL)
ggsave("figures/map_kong_hires_bathy.png", bathy_kong_plot, height = 10, width = 12)

# Hi-res Kong shape files from Norsk Polarinstitut
glacier_Norsk_kong <- read_sf("~/pCloudDrive/FACE-IT_data/kongsfjorden/bathymetry_Norsk_Polarinstitut/06_Norsk_Polarinsitut/NP_S100_SHP/S100_Isbreer_f.shp")
elev_Norsk_kong <- read_sf("~/pCloudDrive/FACE-IT_data/kongsfjorden/bathymetry_Norsk_Polarinstitut/06_Norsk_Polarinsitut/NP_S100_SHP/S100_Koter_l.shp")
coast_Norsk_kong <- read_sf("~/pCloudDrive/FACE-IT_data/kongsfjorden/bathymetry_Norsk_Polarinstitut/03_Daten_Norwegian_Mapping_Authority/Kystkontur_m_flater/Kystkontur.shp")
bathy_Norsk_poly_kong <- read_sf("~/pCloudDrive/FACE-IT_data/kongsfjorden/bathymetry_Norsk_Polarinstitut/03_Daten_Norwegian_Mapping_Authority/Dybdedata/Dybdeareal.shp")
bathy_Norsk_line_kong <- read_sf("~/pCloudDrive/FACE-IT_data/kongsfjorden/bathymetry_Norsk_Polarinstitut/03_Daten_Norwegian_Mapping_Authority/Dybdedata/Dybdekurve.shp")
bathy_Norsk_point_kong <- read_sf("~/pCloudDrive/FACE-IT_data/kongsfjorden/bathymetry_Norsk_Polarinstitut/03_Daten_Norwegian_Mapping_Authority/Dybdedata/Dybdepunkt.shp")
# NB: This file is enormous. Too large to plot.
# shape_Norsk <- tiff::readTIFF("~/pCloudDrive/FACE-IT_data/kongsfjorden/bathymetry_Norsk_Polarinstitut/NP_J100_Raster_10m/J100_Raster_10m.tif")
bathy_Kong <- read_delim_arrow("~/pCloudDrive/FACE-IT_data/kongsfjorden/bathymetry_Norsk_Polarinstitut/NO2B1278.xyz",
                               col_names = c("lon", "lat", "depth"), delim = " ")
bathy_Kong_HiRes <- read_delim_arrow("~/pCloudDrive/FACE-IT_data/kongsfjorden/bathymetry_Norsk_Polarinstitut/NO2B1078.xyz",
                                     col_names = c("lon", "lat", "depth"), delim = " ")

# Plot a raw shape file
ggplot(data = glacier_Norsk_kong) + geom_sf()
# ggplot(data = bathy_Kong, aes(x = lon, y = lat, fill = depth)) + geom_raster()
# plot(shape_Norsk)

# Convert to lon/lat degree decimals
bathy_point_kong_deg <- st_transform(bathy_Norsk_point_kong, 4326) |> dplyr::select(DYBDE, geometry)
bathy_poly_kong_deg <- st_transform(bathy_Norsk_poly_kong, 4326) |> dplyr::select(DYBDE_MIN, DYBDE_MAX, geometry)

# Subset to Kongsfjorden
bbox_sub <- st_bbox(c(xmin = bbox_kong[1], ymin = bbox_kong[3],
                      xmax = bbox_kong[2], ymax = bbox_kong[4]),
                    crs = st_crs(4326))
bathy_point_kong_deg_sub <- st_crop(x = bathy_point_kong_deg, y = bbox_sub)
bathy_poly_kong_deg_sub <- st_crop(x = bathy_poly_kong_deg, y = bbox_sub)

# Convert to dataframes to save as .csv
bathy_point_df <- sf_to_df(bathy_point_kong_deg_sub, fill = TRUE) |> 
  dplyr::select(x, y, DYBDE) |> dplyr::rename(lon = x, lat = y, depth = DYBDE)
write_csv_arrow(bathy_point_df, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/bathymetry_Norsk_Polarinstitut/bathy_point.csv")

# Plot points
bathy_point_kong_deg_sub |> 
  # filter(DYBDE <= 200) |>
  ggplot() +
  geom_sf(aes(colour = DYBDE)) +
  scale_colour_viridis_c() + 
  labs(x = NULL, y = NULL) +
  theme(legend.position = "bottom")
ggsave("figures/demo_map_point.png", width = 12, height = 10)

# Plot polygons
ggplot(data = bathy_poly_kong_deg_sub) +
  geom_sf(aes(colour = DYBDE_MIN)) +
  scale_colour_viridis_c() + 
  labs(x = NULL, y = NULL) +
  theme(legend.position = "bottom")
ggsave("figures/demo_map_polygon.png", width = 12, height = 10)

# Convert to even grid
# bathy_kong_grid <- st_make_grid(bathy_point_kong_deg_sub, cellsize = 0.001)#, what = "centers")
bathy_kong_rast <- st_rasterize(bathy_poly_kong_deg_sub)
# bathy_kong_rast <- st_rasterize(bathy_poly_kong_deg_sub, template = st_as_stars(bathy_kong_grid))
# bathy_kong_rast <- st_rasterize(bathy_poly_kong_deg_sub, template = st_as_stars(st_bbox(bathy_kong_grid)))

# Convert to dataframe to save as .csv
bathy_rast_df <- as.data.frame(bathy_kong_rast, xy = TRUE) |> 
  dplyr::rename(lon = x, lat = y, depth_min = DYBDE_MIN, depth_max = DYBDE_MAX)
write_csv_arrow(bathy_rast_df, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/bathymetry_Norsk_Polarinstitut/bathy_raster.csv")

# Plot raster
plot(bathy_kong_rast)
ggplot(data = bathy_rast_df, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = depth_min)) +
  scale_fill_viridis_c() + 
  labs(x = NULL, y = NULL) +
  theme(legend.position = "bottom")
ggsave("figures/demo_map_raster.png", width = 12, height = 10)

# Save as .asc
bathy_kong_rast <- as(bathy_kong_rast, "Raster")
writeRaster(bathy_kong_rast, "~/bathy_kong_rast.asc", overwrite = TRUE)
bathy_kong_rast <- raster("~/bathy_kong_rast.asc")
plot(bathy_kong_rast)

# NB: This runs for over 10 minutes without finishing
# bathy_kong_rast <- st_intersection(x = bathy_kong_deg_sub, y = bathy_kong_grid)

# Stitch together hi-res Porsangerfjorden bathy, convert to 4326, and save
bathy_por_inner <- read_delim("~/pCloudDrive/FACE-IT_data/maps/Kartverket/por/por_inner/data/xyz/xyz_0.xyz",
                              col_names = c("x", "y", "depth"))
bathy_por_outer <- read_delim("~/pCloudDrive/FACE-IT_data/maps/Kartverket/por/por_outer/data/xyz/xyz_0.xyz",
                              col_names = c("x", "y", "depth"))
bathy_por <- rbind(bathy_por_inner, bathy_por_outer) |> distinct()
bathy_por_df <- convert_UTM_deg_grid(bathy_por, 25833) |> dplyr::rename(depth = layer) |> filter(depth < 0)
ggplot(data = bathy_por_df, aes(x = lon, y = lat)) + geom_raster(aes(fill = depth))
write_csv(bathy_por_df, "~/pCloudDrive/FACE-IT_data/maps/Kartverket/por/por_hires_bathy.csv")
rm(bathy_por_inner, bathy_por_outer, bathy_por, bathy_por_df); gc()

# Stitch together hi-res Isfjorden bathy
bathy_is_inner <- read_delim("~/pCloudDrive/FACE-IT_data/maps/Kartverket/is/is_inner/data/xyz/xyz_0.xyz",
                              col_names = c("x", "y", "depth"))
bathy_is_outer <- read_delim("~/pCloudDrive/FACE-IT_data/maps/Kartverket/is/is_outer/data/xyz/xyz_0.xyz",
                              col_names = c("x", "y", "depth"))
bathy_is <- rbind(bathy_is_inner, bathy_is_outer) |> distinct()
bathy_is_df <- convert_UTM_deg_grid(bathy_is, 25833) |> dplyr::rename(depth = layer) |> filter(depth < 0)
ggplot(data = bathy_is_df, aes(x = lon, y = lat)) + geom_raster(aes(fill = depth))
write_csv(bathy_is_df, "~/pCloudDrive/FACE-IT_data/maps/Kartverket/is/is_hires_bathy.csv")
rm(bathy_is_inner, bathy_is_outer, bathy_is, bathy_is_df); gc()

# Stitch together hi-res Storfjorden bathy
bathy_stor_1 <- read_delim("~/pCloudDrive/FACE-IT_data/maps/Kartverket/stor/stor_1/data/xyz/xyz_0.xyz",
                             col_names = c("x", "y", "depth"))
bathy_stor_2 <- read_delim("~/pCloudDrive/FACE-IT_data/maps/Kartverket/stor/stor_2/data/xyz/xyz_0.xyz",
                             col_names = c("x", "y", "depth"))
bathy_stor_3 <- read_delim("~/pCloudDrive/FACE-IT_data/maps/Kartverket/stor/stor_3/data/xyz/xyz_0.xyz",
                           col_names = c("x", "y", "depth"))
bathy_stor <- rbind(bathy_stor_1, bathy_stor_2, bathy_stor_3) |> distinct()
bathy_stor_df <- convert_UTM_deg_grid(bathy_stor, 25833) |> dplyr::rename(depth = layer) |> filter(depth < 0)
ggplot(data = bathy_stor_df, aes(x = lon, y = lat)) + geom_raster(aes(fill = depth))
write_csv(bathy_stor_df, "~/pCloudDrive/FACE-IT_data/maps/Kartverket/stor/stor_hires_bathy.csv")
rm(bathy_stor_1, bathy_stor_2, bathy_stor_3, bathy_stor, bathy_stor_df); gc()

# Stitch together hi-res Tromso bathy
bathy_tromso_1 <- read_delim("~/pCloudDrive/FACE-IT_data/maps/Kartverket/tromso/tromso_1/data/xyz/xyz_0.xyz",
                           col_names = c("x", "y", "depth"))
bathy_tromso_2 <- read_delim("~/pCloudDrive/FACE-IT_data/maps/Kartverket/tromso/tromso_2/data/xyz/xyz_0.xyz",
                           col_names = c("x", "y", "depth"))
bathy_tromso_3 <- read_delim("~/pCloudDrive/FACE-IT_data/maps/Kartverket/tromso/tromso_3/data/xyz/xyz_0.xyz",
                           col_names = c("x", "y", "depth"))
bathy_tromso_4 <- read_delim("~/pCloudDrive/FACE-IT_data/maps/Kartverket/tromso/tromso_4/data/xyz/xyz_0.xyz",
                             col_names = c("x", "y", "depth"))
bathy_tromso_5 <- read_delim("~/pCloudDrive/FACE-IT_data/maps/Kartverket/tromso/tromso_5/data/xyz/xyz_0.xyz",
                             col_names = c("x", "y", "depth"))
bathy_tromso_6 <- read_delim("~/pCloudDrive/FACE-IT_data/maps/Kartverket/tromso/tromso_6/data/xyz/xyz_0.xyz",
                             col_names = c("x", "y", "depth"))
bathy_tromso_7 <- read_delim("~/pCloudDrive/FACE-IT_data/maps/Kartverket/tromso/tromso_7/data/xyz/xyz_0.xyz",
                             col_names = c("x", "y", "depth"))
bathy_tromso <- rbind(bathy_tromso_1, bathy_tromso_2, bathy_tromso_3, bathy_tromso_4, 
                      bathy_tromso_5, bathy_tromso_6, bathy_tromso_7) |> distinct()
bathy_tromso_df <- convert_UTM_deg_grid(bathy_tromso, 25833) |> dplyr::rename(depth = layer) |> filter(depth < 0)
ggplot(data = bathy_tromso_df, aes(x = lon, y = lat)) + geom_raster(aes(fill = depth))
write_csv(bathy_tromso_df, "~/pCloudDrive/FACE-IT_data/maps/Kartverket/tromso/tromso_hires_bathy.csv")
rm(bathy_tromso_1, bathy_tromso_2, bathy_tromso_3, bathy_tromso_4, 
   bathy_tromso_5, bathy_tromso_6, bathy_tromso_7, bathy_tromso, bathy_tromso_df); gc()

# Extract hi-res bathy for Disko Bay from IceBridge product
# ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/maps/IceBridge/BedMachineGreenland-v5.nc")
bbox_disko_utm <- bbox_to_UTM(bbox_disko, 3413)
bathy_disko <- tidync::tidync("~/pCloudDrive/FACE-IT_data/maps/IceBridge/BedMachineGreenland-v5.nc") |> 
  tidync::hyper_filter(x = dplyr::between(x, min(bbox_disko_utm[,1]), max(bbox_disko_utm[,1])), 
                       y = dplyr::between(y, min(bbox_disko_utm[,2]), max(bbox_disko_utm[,2]))) |> 
  tidync::hyper_tibble()
bathy_disko_df <- convert_UTM_deg_grid(bathy_disko, 3413, "bed") |> dplyr::rename(depth = layer) |> filter(depth < 0)
ggplot(data = bathy_disko_df, aes(x = lon, y = lat)) + geom_raster(aes(fill = depth))
write_csv(bathy_disko_df, "~/pCloudDrive/FACE-IT_data/maps/IceBridge/disko_hires_bathy.csv")

# Extract hi-res bathy for Nuup Kangerlua from IceBridge product
bbox_nuup_utm <- bbox_to_UTM(bbox_nuup, 3413)
bathy_nuup <- tidync::tidync("~/pCloudDrive/FACE-IT_data/maps/IceBridge/BedMachineGreenland-v5.nc") |> 
  tidync::hyper_filter(x = dplyr::between(x, min(bbox_nuup_utm[,1]), max(bbox_nuup_utm[,1])), 
                       y = dplyr::between(y, min(bbox_nuup_utm[,2]), max(bbox_nuup_utm[,2]))) |> 
  tidync::hyper_tibble()
bathy_nuup_df <- convert_UTM_deg_grid(bathy_nuup, 3413, "bed") |> dplyr::rename(depth = layer) |> filter(depth < 0)
ggplot(data = bathy_nuup_df, aes(x = lon, y = lat)) + geom_raster(aes(fill = depth))
write_csv(bathy_nuup_df, "~/pCloudDrive/FACE-IT_data/maps/IceBridge/nuup_hires_bathy.csv")


# Data problems -----------------------------------------------------------

# Figures that highlight where we need to change the bbox for the study site

# Kongsfjorden
## See e-mail from Allison Bailey for feedback
load("~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")
if(!exists("coastline_full")) load("metadata/coastline_full_df.RData")
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
if(!exists("coastline_full")) load("metadata/coastline_full_df.RData")
coastline_kong <- coastline_full_df %>% 
  dplyr::filter(x >= bbox_kong[1]-1, x <= bbox_kong[2]+1, y >= bbox_kong[3]-1, y <= bbox_kong[4]+1) |> 
  dplyr::select(x, y) |> dplyr::rename(lon = x, lat = y)
full_product_kong_coords <- full_product_kong %>% 
  dplyr::select(lon, lat) %>% distinct()

## Manually create regions
kong_inner <- coastline_kong[270,] %>% 
  rbind(data.frame(lon = c(12.36, 12.65, 12.65), lat = c(78.86, 78.86, 79.01958))) %>% 
  rbind(coastline_kong[c(560:570, 536, 420),]) %>% 
  rbind(data.frame(lon = 12.36003, lat = 78.945)) %>% mutate(region = "inner")
kong_trans <- coastline_kong[c(157:270),] %>% 
  rbind(data.frame(lon = 12.36003, lat = 78.945)) %>% 
  rbind(coastline_kong[c(420, 536, 570:589, 500:470),]) %>% mutate(region = "transition")
kong_middle <- coastline_kong[c(76:157, 470:500, 589:666),] %>% mutate(region = "middle")
kong_outer <- coastline_kong[c(76, 666),] %>% rbind(data.frame(lon = 11.178, lat = 79.115)) %>% mutate(region = "outer")
kong_shelf <- coastline_kong[1:76,] %>% 
  rbind(data.frame(lon = c(11.178, 11.178, 11, 11, 11.72653), lat = c(79.115, 79.2, 79.2, 78.85, 78.85))) %>%  mutate(region = "shelf")
kong_regions <- rbind(kong_inner, kong_trans, kong_middle, kong_outer, kong_shelf) %>% 
  mutate(region = factor(region, levels = c("inner", "transition", "middle", "outer", "shelf")))

## Find data in regions
full_region_kong <- plyr::ldply(unique(kong_regions$region), points_in_region, .parallel = F, 
                                bbox_df = kong_regions, data_df = full_product_kong_coords)
region_labels_kong <- full_region_kong %>% 
  group_by(region) %>% 
  summarise(lon = mean(range(lon)),
            lat = mean(range(lat)),
            count = n(), .groups = "drop")

## Plot
## NB: Outdated code. No longer runs.
plot_regions_kong <- ggplot() +
  geom_polygon(data = kong_regions, fill = "grey70", colour = "black",
               aes(x = lon, y = lat, group = region)) +
               # aes(x = x, y = y, group = polygon_id)) +
  # geom_polygon(data = bbox_regions_kong, aes(x = lon, y = lat, group = region, fill = region)) +
  # geom_tile(aes(x = lon, y = lat)) +
  # geom_rect(data = bbox_regions_kong, aes(xmin = lon1, xmax = lon2, ymin = lat1, ymax = lat2, fill = region), alpha = 0.3) +
  geom_point(data = distinct(full_region_kong), aes(x = lon, y = lat, colour = region), show.legend = F) +
  # geom_label(data = region_labels_kong, aes(x = lon, y = lat, label = paste0("n = ",count)), alpha = 0.8) +
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



# Jean-Pierre request -----------------------------------------------------
# Map of Kongsfjorden (bbox only) with bathymetry sans contour lines
# Send as .svg and .RData

# ggOceaMaps version
basemap(limits = c(10, 35, 75.5, 82), bathymetry = TRUE, shapefiles = "BarentsSea",
        legends = FALSE, glaciers = TRUE)
basemap(limits = c(11.00, 12.69, 78.86, 79.10), bathymetry = TRUE, shapefiles = "Svalbard",
        legends = FALSE, glaciers = TRUE)
basemap(limits = c(11.00, 12.69, 78.86, 79.10), bathymetry = TRUE, shapefiles = "IBCAO",
        legends = FALSE, glaciers = TRUE)

# Bounding box
bbox_deg <- st_bbox(c(xmin = bbox_kong[1], ymin = bbox_kong[3], 
                      xmax = bbox_kong[2], ymax = bbox_kong[4]), crs = st_crs(4326))
bbox_deg_wide <- st_bbox(c(xmin = bbox_kong_wide[1], ymin = bbox_kong_wide[3], 
                           xmax = bbox_kong_wide[2], ymax = bbox_kong_wide[4]), crs = st_crs(4326))
# This should be improved, but will suffice for now
bbox_utm <- as.data.frame(st_transform(st_as_sfc(bbox_deg_wide), 3996))[[1]][[1]][[1]]

# Hi-res Coastline
coast_Norsk_kong <- read_sf("~/pCloudDrive/FACE-IT_data/kongsfjorden/bathymetry_Norsk_Polarinstitut/03_Daten_Norwegian_Mapping_Authority/Kystkontur_m_flater/Kystkontur.shp")
coast_Norsk_kong_deg <- st_transform(coast_Norsk_kong, 4326)
coast_Norsk_kong_deg_sub <- st_crop(x = coast_Norsk_kong_deg, y = bbox_deg_wide)

# GEBCO
# GEBCO_data <- tidync::tidync("~/pCloudDrive/FACE-IT_data/maps/GEBCO/GEBCO_2020.nc") %>% 
#   tidync::hyper_filter(lon = dplyr::between(lon, bbox_kong[1], bbox_kong[2]), 
#                        lat = dplyr::between(lat, bbox_kong[3], bbox_kong[4])) %>% 
#   tidync::hyper_tibble()
# ggplot() + geom_raster(data = GEBCO_data, aes(x = lon, y = lat, fill = elevation))

# IBCAO
IBCAO_data <- tidync::tidync("~/pCloudDrive/FACE-IT_data/maps/IBCAO/IBCAO_v4_200m.nc") |> 
  tidync::hyper_filter(x = dplyr::between(x, min(bbox_utm[,1]), max(bbox_utm[,1])), 
                       y = dplyr::between(y, min(bbox_utm[,2]), max(bbox_utm[,2]))) |> 
  tidync::hyper_tibble()
# ggplot() + geom_raster(data = IBCAO_data, aes(x = x, y = y, fill = z)) + coord_sf()

# Correctly reproject the data, keeping an even grid
IBCAO_utm <- st_as_sf(IBCAO_data, coords = c("x", "y"), crs = 3996)
IBCAO_rast <- st_rasterize(IBCAO_utm)
IBCAO_raster <- as(IBCAO_rast, "Raster")
IBCAO_raster_deg <- projectRaster(IBCAO_raster, crs = 4326)
IBCAO_df <- as.data.frame(IBCAO_raster_deg, xy = TRUE) |> 
  dplyr::rename(lon = x, lat = y, elevation = layer) |> 
  filter(between(lon, bbox_kong[1]-0.1, bbox_kong[2]+0.1),
         between(lat, bbox_kong[3]-0.1, bbox_kong[4]+0.1)) |>
  mutate(elev_10 = round(elevation, -1),
         elev_cut = cut(elevation, c(-400, -200, -50, -25, -10, 0, 10, 50, 100, 600, 1200)))
rm(IBCAO_data, IBCAO_utm, IBCAO_raster_deg); gc()

# Colour palettes
blue.col <- colorRampPalette(c("darkblue", "lightblue"))
yellow.col <- colorRampPalette(c("lightyellow", "orange"))

# Range of topography
range(IBCAO_df$elevation, na.rm = T)
range(IBCAO_df$elev_10, na.rm = T)
levels(IBCAO_df$elev_cut)

# Set bathymetry steps
bathy_steps <- c(-300, -200, -50, 0, 50, 100, 600, 1200)
bathy_labels <- c(-300, -200, -50, 0, 50, 100, 600, 1200)

# The figure
kong_topo_fig <- ggplot() +
  geom_raster(data = IBCAO_df,
              aes(x = lon, y = lat, fill = elevation)) +
  # geom_contour_filled(data = IBCAO_raster_df, breaks = bathy_steps,
               # aes(x = lon, y = lat, z = elevation)) +
  geom_sf(data = coast_Norsk_kong_deg_sub) +
  scale_fill_gradientn(colours = c("darkblue", "lightblue", "grey40", "white"),
                       values = c(0, 0.243, 0.244, 1),
                       breaks = bathy_steps) +
  guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black")) +
  labs(x = NULL, y = NULL, fill = "Elevation [m]") +
  coord_sf(xlim = c(bbox_kong[1:2]), ylim = c(bbox_kong[3:4]), expand = F) +
  theme(legend.position = "bottom",
        panel.background = element_rect(colour = "black", fill = "grey70"),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.key.width = unit(4, "cm"))
kong_topo_fig
save(kong_topo_fig, file = "figures/requests/map_kong_topo_WP1.RData")
ggsave(kong_topo_fig, file = "figures/requests/map_kong_topo_WP1.eps", width = 10, height = 8)

# Just bathymetry
kong_bathy_fig <- ggplot() +
#   geom_raster(data = filter(IBCAO_df, elevation <= 0),
#               aes(x = lon, y = lat, fill = elevation)) +
  # geom_sf(data = coast_Norsk_kong_poly, fill = "grey") +
  geom_sf(data = kong_shape) +
  # scale_fill_gradientn(colours = c("darkblue", "lightblue"),
  #                      # values = c(0, 0.243, 0.244, 1),
  #                      breaks = c(-300, -200, -100, -50, -25, -10, 0)) +
  # guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black")) +
  labs(x = NULL, y = NULL, fill = "Elevation [m]") +
  coord_sf(xlim = c(bbox_kong[1:2]), ylim = c(bbox_kong[3:4]), expand = F) +
  theme(legend.position = "bottom",
        panel.background = element_rect(colour = "black", fill = "grey70"),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.key.width = unit(4, "cm"))
kong_bathy_fig
save(kong_bathy_fig, file = "figures/requests/map_kong_bathy_WP1.RData")
ggsave(kong_bathy_fig, file = "figures/requests/map_kong_bathy_WP1.eps", width = 8, height = 6)


# Annika request ----------------------------------------------------------
# Nuup Kangerlua, Isfjorden and Porsangerfjorden
# It would be great if you can add the names and locations of major settlements. 
# In the workshop discussions, there was a fair amount of focus on land as well. 
# Would it be possible to add topographical detail and/or key features, such as 
# glaciers, for the land or use some other trick to make it as important as the water part?

# Nuup Kangerlua
# Marine terminating glaciers (NS, AS, KNS)
# Land terminating glaciers (SS, KS, QS)
points_nuup <- data.frame(site = c("Nuuk", "Neriunaq", "Kapisillit"),
                          lon = c(-51.8422923, -50.3450035, -50.2744783),
                          lat = c(64.259721, 64.4506417, 64.4328821))
ggOceanMaps::basemap(limits = bbox_nuup, bathymetry = TRUE, glaciers = TRUE, legend.position = "bottom",
                     bathy.style = "poly_blues", gla.border.col = "thistle1", gla.col = "snow") +
  geom_spatial_point(data = points_nuup, aes(x = lon, y = lat), size = 3) +
  geom_spatial_text_repel(data = points_nuup, aes(x = lon, y = lat, label = site), size = 5) +
  labs(title = "Nuup Kangerlua", x = NULL, y = NULL) +
  theme(plot.background = element_rect(colour = NA, fill = "white"))
ggsave("figures/requests/map_nuup_WP4.png", height = 200, width = 200, units = "mm")

# Svalbard
# Isfjorden, Barentsburg, Pyramiden, Ny Ålesund, Nordaustlandet, Edgeøya, Longyearbyen,
points_sval <- data.frame(site = c("Isfjorden", "Barentsburg", "Pyramiden", "Ny Ålesund", 
                                   "Nordaustlandet", "Edgeøya", "Longyearbyen"),
                          lon = c(14.8902883, 14.186268, 16.3262847, 11.6861781, 
                                  20.3032202, 21.4287958, 15.48789),
                          lat = c(78.3034181, 78.0664196, 78.6565062, 78.8756255, 
                                  79.8401006, 77.7680091, 78.2253587))
ggOceanMaps::basemap(limits = c(8, 23.5, 76.5, 81), bathymetry = TRUE, glaciers = TRUE, legend.position = "bottom",
                     bathy.style = "poly_blues", gla.border.col = "thistle1", gla.col = "snow") +
  geom_spatial_point(data = points_sval, aes(x = lon, y = lat), size = 3) +
  geom_spatial_text_repel(data = points_sval, aes(x = lon, y = lat, label = site), size = 5) +
  labs(title = "Svalbard", x = NULL, y = NULL) +
  theme(plot.background = element_rect(colour = NA, fill = "white"))
ggsave("figures/requests/map_sval_WP4.png", height = 200, width = 200, units = "mm")

# Porsangerfjorden
# Smørfjord, Holmfjord, Olderfjord, Kistrand, Igeldas, Østerbotn, Brenna, Repvåg
points_por <- data.frame(site = c("Lakselv", "Bevkop", "Pillavuono", "Nyby", "Børselv",
                                  "Smørfjord", "Holmfjord", "Olderfjord", "Kistrand", 
                                  "Igeldas", "Østerbotn", "Brenna", "Repvåg"),
                         lon = c(24.9188931, 24.9014675, 25.0441326, 25.1235309, 25.5447693,
                                 25.1151665, 25.4504358, 25.2787512, 25.1978622,
                                 24.9118956, 25.1448369, 25.7423743, 25.6685029),
                         lat = c(70.0463953, 70.2073595, 70.3154637, 70.0613209, 70.318202, 
                                 70.4576637, 70.3672181, 70.49862, 70.438682,
                                 70.2138445, 70.1036608, 70.5110818, 70.7474239))
ggOceanMaps::basemap(limits = bbox_por, bathymetry = TRUE, glaciers = TRUE, legend.position = "bottom",
                     bathy.style = "poly_blues", gla.border.col = "thistle1", gla.col = "snow") +
  geom_spatial_point(data = points_por, aes(x = lon, y = lat), size = 3, crs = 4326) +
  geom_spatial_text_repel(data = points_por, aes(x = lon, y = lat, label = site), size = 5) +
  labs(title = "Porsangerfjorden", x = NULL, y = NULL) +
  theme(plot.background = element_rect(colour = NA, fill = "white"))
ggsave("figures/requests/map_por_WP4.png", height = 200, width = 200, units = "mm")


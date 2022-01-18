# code/functions.R
# This script houses functions and other preparatory things used throughout the project


# Setup -------------------------------------------------------------------

# Libraries used in all other scripts
library(tidyverse)
library(tidync)
library(PCICt) # For 'noleap' date conversions
library(FNN)
library(geosphere)
library(grid)
library(gridExtra)
library(gtable)
library(ggOceanMaps)
library(RColorBrewer)
library(sp)
library(sf)
library(pangaear)
library(doParallel); registerDoParallel(cores = 12)

# Find who is the user and define the pCloud path
if (Sys.getenv("LOGNAME") == "gattuso"){
  pCloud_path = "~/pCloud\ Drive/"
} else {
  pCloud_path = "~/pCloudDrive/" 
}

# Remove scientific notation
options(scipen = 9999)

# Set Timezone to UTC
Sys.setenv(TZ = "UTC")

# Bounding boxes
bbox_EU <- c(-60, 60, 63, 90)
bbox_sval <- c(9, 30, 76, 81)
bbox_kong <- c(11, 12.69, 78.86, 79.1)
bbox_is <- c(12.97, 17.50, 77.95, 78.90)
bbox_ingle <- c(18.15, 18.79, 77.87, 78.08)
bbox_stor <- c(17.35, 21.60, 77.33, 78.13)
bbox_young <- c(-22.367917, -19.907644, 74.210137, 74.624304)
bbox_disko <- c(-55.56, -49.55, 68.22, 70.5)
bbox_nuup <- c(-53.32, -48.93, 64.01, 64.8)
bbox_por <- c(24.5, 27, 70, 71.2)

# Project wide category colours
CatCol <- c(
  "Cryosphere" = "mintcream",
  "Physical" = "skyblue",
  "Chemistry" = "#F6EA7C",
  "Biology" = "#A2ED84",
  "Social" = "F48080"
)

# Same but with abbreviations for the categories
CatColAbr <- c(
  "cryo" = "mintcream",
  "phys" = "skyblue",
  "chem" = "#F6EA7C",
  "bio" = "#A2ED84",
  "soc" = "#F48080"
)

# Project wide colours for depth categories

DepthCol <- c(
  "0 - 10 m" = brewer.pal(9, "Blues")[4],
  "10 - 50 m" = brewer.pal(9, "Blues")[5], 
  "50 - 200 m" = brewer.pal(9, "Blues")[6], 
  "200 - 1000 m" = brewer.pal(9, "Blues")[7], 
  "1000 - 2000 m" = brewer.pal(9, "Blues")[8], 
  "2000+ m" = brewer.pal(9, "Blues")[9]
)


# Meta-data ---------------------------------------------------------------

# The base global map
map_base <- readRDS("metadata/map_base.Rda")

## Hi-res coastlines
# Load shapefile
coastline_full <- read_sf("~/pCloudDrive/FACE-IT_data/maps/GSHHG/GSHHS_shp/f/GSHHS_f_L1.shp")

# Convert to data.frame
coastline_full_df <- sfheaders::sf_to_df(coastline_full, fill = TRUE)

# Full map
# ggplot(data = coastline_full) +
#   geom_sf()


# Functions ---------------------------------------------------------------

# Find the nearest grid cells for each site
grid_match <- function(coords1, coords2){
  coords2$idx <- 1:nrow(coords2)
  grid_index <- data.frame(coords1,
                           idx = knnx.index(data = as.matrix(coords2[,1:2]),
                                            query = as.matrix(coords1[,1:2]), k = 1))
  grid_points <- left_join(grid_index, coords2, by = c("idx")) %>% 
    mutate(dist = round(distHaversine(cbind(lon.x, lat.x),
                                      cbind(lon.y, lat.y))/1000, 2), idx = NULL)
  return(grid_points)
}

# Function for finding and cleaning up points within a given region polygon
points_in_region <- function(region_in, bbox_df, data_df){
  region_sub <- bbox_df %>% 
    filter(region == region_in)
  distinct_df <- data_df %>%
    dplyr::select(lon, lat) %>%
    distinct()
  coords_in <- distinct_df %>%
    mutate(in_grid = sp::point.in.polygon(point.x = distinct_df[["lon"]], point.y = distinct_df[["lat"]],
                                          pol.x = region_sub[["lon"]], pol.y = region_sub[["lat"]])) %>%
    filter(in_grid >= 1) %>%
    mutate(region = region_in) %>%
    dplyr::select(lon, lat, region)
  return(coords_in)
}

# Convenience function for getting bbox from site name
bbox_from_name <- function(site_name){
  # get correct bounding box
  if(site_name == "Kongsfjorden") bbox_name <- bbox_kong
  if(site_name == "Isfjorden") bbox_name <- bbox_is
  if(site_name == "Inglefieldbukta") bbox_name <- bbox_ingle
  if(site_name == "Storfjorden") bbox_name <- bbox_stor
  if(site_name == "Young Sound") bbox_name <- bbox_young
  if(site_name == "Disko Bay") bbox_name <- bbox_disko
  if(site_name == "Nuup Kangerlua") bbox_name <- bbox_nuup
  if(site_name == "Porsangerfjorden") bbox_name <- bbox_por
  # This has potentially been coded to allow an error here
  return(bbox_name)
}


# Function that takes 4 bounding box coordinates and converts them to a polygon for ggOceanMaps
# The 'ID' value can be used to hold the name of the site for the bounding box
bbox_to_poly <- function(coords, ID = 1){
  
  # Get the coordinates
  if(is.data.frame(coords)){
    lon1 <- min(coords$lon1); lon2 <- max(coords$lon2)
    lat1 <- min(coords$lat1); lat2 <- max(coords$lat2)
  } else if(is.vector(coords)){
    lon1 <- coords[1]; lon2 <- coords[2]
    lat1 <- coords[3]; lat2 <- coords[4]
  } else {
    stop("Uh oh")
  }
  
  # Create bounding box that can curve on a polar projection
  bbox_top <- data.frame(lon = seq(lon1, lon2, length.out = 100), 
                         lat = lat1, id = "bbox")
  bbox_bottom <- data.frame(lon = seq(lon2, lon1, length.out = 100), 
                            lat = lat2, id = "bbox")
  bbox_df <- rbind(bbox_top, bbox_bottom)
  
  # only want lon-lats in the list, not the names
  bbox_list <- lapply(split(bbox_df, bbox_df$id), function(x) { x["id"] <- NULL; x })
  
  # Convert to polygon and add id variable 
  bbox_poly <- Polygons(sapply(bbox_list, Polygon), ID = ID)
  
  # Create SpatialPolygons object
  bbox_spatial <- SpatialPolygons(list(bbox_poly), 
                                  proj4string = CRS("+init=epsg:4326 +proj=longlat")) 
  return(bbox_spatial)
}

# Function that prepares custom bathymetry for ggOceanMaps based on bounding box
# lon1 <- bbox_nor$lon1; lon2 <- bbox_nor$lon2; lat1 <- bbox_nor$lat1; lat2 <- bbox_nor$lat2
bbox_to_bathy <- function(coords, lon_pad = 0, lat_pad = 0,
                          bathy_file = NA, projection = NA,
                          depths = c(25, 50, 100, 200, 300, 500, 1000, 2000, 10000)){
  
  # Get the coordinates
  if(is.data.frame(coords)){
    lon1 <- min(coords$lon1); lon2 <- max(coords$lon2)
    lat1 <- min(coords$lat1); lat2 <- max(coords$lat2)
  } else if(is.vector(coords)){
    lon1 <- coords[1]; lon2 <- coords[2]
    lat1 <- coords[3]; lat2 <- coords[4]
  } else if(is(coords, "SpatialPolygons")) {
    lon1 <- coords@bbox[1,1]; lon2 <- coords@bbox[1,2]
    lat1 <- coords@bbox[2,1]; lat2 <- coords@bbox[2,2]
  } else {
    stop("Uh oh")
  }
  
  # Use the default hi-res Arctic bathy unless the user specifies something else
  # if(is.na(bathy_file)) bathy_file <- paste0(pCloud_path,"FACE-IT_data/shape_files/IBCAO_v4_200m.nc") # Super hi-res, but doesn't work...
  # if(is.na(bathy_file)) bathy_file <- paste0(pCloud_path,"FACE-IT_data/shape_files/ETOPO1_Ice_g_gmt4.grd")
  if(is.na(bathy_file)) bathy_file <- paste0(pCloud_path,"FACE-IT_data/maps/GEBCO/GEBCO_2020.nc")
    
  # Set limits for bathy projection
  xlon <- c(lon1-lon_pad, lon2+lon_pad)
  xlat <- c(lat1-lat_pad, lat2+lat_pad)
  lims <- c(xlon, xlat)
  
  # Set projection
  if(is.na(projection)){
    # projection <- "+init=epsg:6070"
    projection <- "+init=epsg:3995" # Arctic Polar Stereographic
    # projection <- "+init=epsg:4326" # Cartesian global
    # projection <- "+init=epsg:32636"
  } 
  
  # Check the limits
  # basemap(limits = lims)
  
  # Convert NetCDF to raster
  rb <- raster_bathymetry(bathy = bathy_file,
                          depths = depths, 
                          proj.out = projection, 
                          boundary = lims)
  
  # Check raster output
  # class(rb)
  # names(rb)
  # raster::plot(rb$raster)
  
  # Convert to raster vector for plotting
  bs_bathy <- vector_bathymetry(rb)
  # sp::plot(bs_bathy)
  
  # Convert land file for use with new bathy file
  world <- rgdal::readOGR(paste0(pCloud_path,"FACE-IT_data/shape_files/ne_10m_land.shp"), verbose = F)
  islands <- rgdal::readOGR(paste0(pCloud_path,"FACE-IT_data/shape_files/ne_10m_minor_islands.shp"), verbose = F)
  world <- rbind(world, islands)
  bs_land <- clip_shapefile(world, lims)
  bs_land <- sp::spTransform(bs_land, CRSobj = sp::CRS(projection))
  if(!rgeos::gIsValid(bs_land)){  # Has to return TRUE, if not use rgeos::gBuffer
    bs_land <- rgeos::gBuffer(bs_land, byid = TRUE, width = 0)
  }
  # sp::plot(bs_land)
  
  # Create glacier shape files
  glaciers <- rgdal::readOGR(paste0(pCloud_path,"FACE-IT_data/shape_files/ne_10m_glaciated_areas.shp"), verbose = F)
  if(!rgeos::gIsValid(glaciers)){ # Needs buffering
    glaciers <- rgeos::gBuffer(glaciers, byid = TRUE, width = 0)
  }
  bs_glacier <- clip_shapefile(glaciers, lims)
  if(dim(bs_glacier)[1] > 0){
    bs_glacier <- sp::spTransform(bs_glacier, CRSobj = sp::CRS(projection))
    # rgeos::gIsValid(bs_glacier)
    # sp::plot(bs_glacier)
  } else { 
    bs_glacier <- NA
  }
  # sp::plot(bs_glacier)
  # basemap(shapefiles = list(land = bs_land, glacier = bs_glacier, bathy = bs_bathy), bathymetry = TRUE, glaciers = TRUE)
  
  # Return results
  res <- list(bathy = bs_bathy, land = bs_land, glacier = bs_glacier)
  return(res)
}

# Convenience function that allows a user to directly produce a ggOceanMaps from a bounding box
## Note that a release of ggOceanMaps broke the backward compatibility of this function
bbox_to_ggOcean <- function(coords, bathy_file = NA, lon_pad = 0, lat_pad = 0, add_bbox = F,
                            depths = c(25, 50, 100, 200, 300, 500, 1000, 2000, 10000)){
  
  # Prep the shape files
  bs_res <- bbox_to_bathy(coords, bathy_file = bathy_file, lon_pad = lon_pad, lat_pad = lat_pad, depths = depths)
  
  # Plot on ggOceanMaps
  if(!is.na(bs_res[[3]])){
    map_res <- basemap(shapefiles = list(bathy = bs_res$bathy, land = bs_res$land, glacier = bs_res$glacier),
                       bathymetry = TRUE, glaciers = TRUE)
  } else{
    map_res <- basemap(shapefiles = list(bathy = bs_res$bathy, land = bs_res$land, glacier = NULL), 
                       bathymetry = TRUE, glaciers = FALSE)
  }
  map_res <- map_res + scale_fill_viridis_d("Depth (m)")
  
  # Add the bounding box as desired
  if(add_bbox){
    if(!is(coords, "SpatialPolygons")){
      bbox_spatial <- bbox_to_poly(coords)
    } else {
      bbox_spatial <- coords
    }
    map_res <- map_res + annotation_spatial(bbox_spatial, fill = "cadetblue1", alpha = 0.2)
  }
  return(map_res)
}

# Function for making a site map out of a bounding box
bbox_to_map <- function(coords, bathy_data = NA, lon_pad = 0, lat_pad = 0, add_bbox = F,
                        depths = c(25, 50, 100, 200, 300, 500, 1000, 2000, 10000)){
  
  # Prepare bathymetry data
  if(is.na(bathy_data)){
    bathy_data <- tidync::tidync("~/pCloudDrive/FACE-IT_data/maps/GEBCO/GEBCO_2020.nc") %>% 
      tidync::hyper_filter(lon = dplyr::between(lon, coords[1]-lon_pad, coords[2]+lon_pad), 
                           lat = dplyr::between(lat, coords[3]-lat_pad, coords[4]+lat_pad)) %>% 
      tidync::hyper_tibble() %>% 
      mutate(depth = -elevation) %>% 
      filter(depth > 0)
  }
  
  # Clip coastline polygons for faster plotting
  coastline_full_df_sub <- coastline_full_df %>% 
    filter(x >= coords[1]-lon_pad-10, x <= coords[2]+lon_pad+10,
           y >= coords[3]-lat_pad-10, y <= coords[4]+lat_pad+10)
  
  # Get list of contour depths used in figure
  depths_sub <- depths[depths < max(bathy_data$depth)] 
  
  # Map with coast shapefile and bathy contours
  map_res <- ggplot(bathy_data, aes(x = lon, y = lat)) +
    geom_tile(aes(fill = depth)) +
    geom_contour(aes(z = depth, colour = after_stat(level)), breaks = depths_sub, size = 0.3, show.legend = F) +
    geom_polygon(data = coastline_full_df_sub, aes(x = x, y = y, group = polygon_id), 
                 fill = "grey70", colour = "black") +
    annotate("rect",  colour = "darkgreen", fill = "darkgreen", alpha = 0.1,
             xmin = coords[1], xmax = coords[2], ymin = coords[3], ymax = coords[4]) +
    scale_fill_distiller(palette = "Blues", direction = 1) +
    scale_colour_distiller(palette = "Greys", direction = -1) +
    # scale_fill_viridis_c(option = "E") +
    coord_quickmap(expand = F,
                   xlim = c(coords[1]-lon_pad, coords[2]+lon_pad), 
                   ylim = c(coords[3]-lat_pad, coords[4]+lat_pad)) +
    labs(x = NULL, y = NULL, fill = "depth (m)",
         subtitle = paste0("Contours at: ",paste(c(depths_sub), collapse = ", "), " m")) +
    theme(panel.border = element_rect(fill = NA, colour = "black"),
          panel.background = element_rect(fill = "white"),
          legend.position = "bottom")
  # map_res
  
  # Exit
  return(map_res)
}

# Function for smoother meta-data creation
# Not currently used
make_meta_data <- function(dat, type, data_name, file_name, URL, reference, note = NA){
  
  # Find longitude range
  lon_col <- c(colnames(dat)[str_detect(colnames(dat), "lon")],
               colnames(dat)[str_detect(colnames(dat), "Lon")])
  if(length(lon_col) > 0){
    lon_min <- round(min(dat[,lon_col], na.rm = T), 2)
    lon_max <- round(max(dat[,lon_col], na.rm = T), 2)
    lon_range <- paste0(lon_min," to ", lon_max)
  } else {
    lon_min <- NA; lon_max <- NA
    lon_range <- NA
  }
  
  # Find latitude range
  lat_col <- c(colnames(dat)[str_detect(colnames(dat), "lat")],
               colnames(dat)[str_detect(colnames(dat), "Lat")])
  if(length(lat_col) > 0){
    lat_min <- round(min(dat[,lat_col], na.rm = T), 2)
    lat_max <- round(max(dat[,lat_col], na.rm = T), 2)
    lat_range <- paste0(lat_min," to ", lat_max)
  } else {
    lat_min <- NA; lat_max <- NA
    lat_range <- NA
  }
  
  # Find depth range
  depth_col <- c(colnames(dat)[str_detect(colnames(dat), "depth")],
                 colnames(dat)[str_detect(colnames(dat), "Depth")])
  depth_col <- depth_col[!str_detect(depth_col, "bot")]
  depth_col <- depth_col[!str_detect(depth_col, "Bot")]
  if(length(depth_col) > 0){
    depth_min <- round(min(dat[,depth_col], na.rm = T), 2)
    depth_max <- round(max(dat[,depth_col], na.rm = T), 2)
    depth_range <- paste0(depth_min," to ", depth_max)
  } else {
    depth_min <- NA; depth_max <- NA
    depth_range <- NA
  }
  
  # Find depth range
  date_col <- c(colnames(dat)[str_detect(colnames(dat), "date")],
                colnames(dat)[str_detect(colnames(dat), "Date")])
  year_col <- c(colnames(dat)[str_detect(colnames(dat), "year")],
                colnames(dat)[str_detect(colnames(dat), "Year")])
  if(length(date_col) > 0){
    date_dat <- dat[,date_col] %>% `colnames<-`("t")
    if(!is.na(as.Date(date_dat$t[1], "%Y-%m-%d"))){
      date_min <- min(lubridate::year(as.Date(date_dat$t, "%Y-%m-%d")), na.rm = T)
      date_max <- max(lubridate::year(as.Date(date_dat$t, "%Y-%m-%d")), na.rm = T)
    } else if(!is.na(as.Date(date_dat$t[1], "%Y%m%d"))){
      date_min <- min(lubridate::year(as.Date(date_dat$t, "%Y%m%d")), na.rm = T)
      date_max <- max(lubridate::year(as.Date(date_dat$t, "%Y%m%d")), na.rm = T)
    }else if(!is.na(as.Date(date_dat$t[1], "%Y/%m/%d"))){
      date_min <- min(lubridate::year(as.Date(date_dat$t, "%Y/%m/%d")), na.rm = T)
      date_max <- max(lubridate::year(as.Date(date_dat$t, "%Y/%m/%d")), na.rm = T)
    } else if(!is.na(as.Date(date_dat$t[1], "%d/%m/%Y"))){
      date_min <- min(lubridate::year(as.Date(date_dat$t, "%d/%m/%Y")), na.rm = T)
      date_max <- max(lubridate::year(as.Date(date_dat$t, "%d/%m/%Y")), na.rm = T)
    } else if(!is.na(as.Date(date_dat$t[1], "%m/%d/%Y"))){
      date_min <- min(lubridate::year(as.Date(date_dat$t, "%m/%d/%Y")), na.rm = T)
      date_max <- max(lubridate::year(as.Date(date_dat$t, "%m/%d/%Y")), na.rm = T)
    } else {
      date_min <- NA; date_max <- NA
    }
  }
  if(is.na(depth_min) & length(year_col) > 0) {
    year_dat <- dat[,year_col] %>% `colnames<-`("year")
    date_min <- min(year_dat$year, na.rm = T)
    date_max <- max(year_dat$year, na.rm = T)
  }
  if(!is.na(date_min)){
    date_range <- paste0(date_min," to ", date_max)
  } else {
    date_range <- NA
  }
  
  # Determine ecoregions
  # Find point in MEOW polygons
  
  # Combine and output
  res <- data.frame(type, data_name, 
                    date_range, lon_range, lat_range, depth_range,
                    # date_min, date_max, lon_min, lon_max, lat_min, lat_max, depth_min, depth_max, 
                    file_name, URL, reference,
                    note)
  return(res)
}

# Function for loading raster files with utm coords and converting to lon/lat
load_utm <- function(file_name){
  ras_1 <- raster(file_name)
  crs_1 <- ras_1@crs
  utm1 <- as.data.frame(ras_1, xy = T)
  coordinates(utm1) <- ~x+y 
  proj4string(utm1) <- crs_1
  utm2 <- spTransform(utm1, CRS("+proj=longlat +datum=WGS84"))
  utm3 <- as.data.frame(utm2) %>% 
    dplyr::rename(lon = x, lat = y) %>% 
    dplyr::select(lon, lat, everything())
}

# Function for loading individual variables from a difficult NetCDF CTD file
CTD_to_long <- function(nc_file, var_id){
  # Get attributes
  nc_TIME <- ncdf4::ncvar_get(nc_file, varid = "TIME")
  nc_PRES <- ncdf4::ncvar_get(nc_file, varid = "PRES")
  nc_LONGITUDE <- ncdf4::ncvar_get(nc_file, varid = "LONGITUDE")
  nc_LATITUDE <- ncdf4::ncvar_get(nc_file, varid = "LATITUDE")
  # Extract one variable and melt that it
  nc_val <- data.frame(t(ncdf4::ncvar_get(nc_file, varid = var_id))) %>% 
    `colnames<-`(nc_PRES) %>% 
    cbind(nc_TIME, nc_LONGITUDE, nc_LATITUDE) %>%
    pivot_longer(min(nc_PRES):max(nc_PRES), values_to = "value", names_to = "depth") %>% 
    filter(!is.na(value)) %>% 
    mutate(date = as.Date(as.POSIXct((nc_TIME*86400), origin = "1950-01-01 00:00:00"), .keep = "unused"),
           depth = as.numeric(depth)) %>% 
    dplyr::rename(lon = nc_LONGITUDE, lat = nc_LATITUDE) %>% 
    dplyr::select(lon, lat, date, depth, value) %>% 
    `colnames<-`(c("lon", "lat", "date", "depth", tolower(var_id))); gc()
  return(nc_val)
}

# Simple wrapper for loading Isfjorden mooring NetCDF files
load_is_mooring <- function(file_name){
  
  # Get NetCDF metadata
  is_dump <- ncdump::NetCDF(file_name)
  is_units <- is_dump$variable %>% 
    filter(!name %in% c("depth", "lon", "lat")) %>% 
    dplyr::select(name, units)
  is_start <- is_dump$attribute$global$time_coverage_start
  # is_citation <- is_dump$attribute$global$references
  file_short <- sapply(strsplit(file_name, "/"), "[[", 8)
  
  # Get correct URL and citation to add to data
  is_ref_info <- data.frame(short_name = c("IN1516.nc", "IN1617.nc", "IN1718.nc", # North moorings
                                           "IS0506.nc", "IS0607.nc", "IS0708.nc", "IS1011.nc", "IS1112.nc", "IS1213.nc", "IS1314.nc", 
                                           "IS1415.nc", "IS1516_ADCP.nc", "IS1516.nc", "IS1617_ADCP.nc", "IS1617.nc", "IS1718.nc"),
                            URL = c("https://data.npolar.no/dataset/111aca43-7f5c-4c15-9f31-dcd3214dbfcb", "https://data.npolar.no/dataset/3078f619-9955-4a7f-9316-fab598fec382",
                                    "https://data.npolar.no/dataset/e9106051-6c44-4849-9d62-04e4a82f1ca9", # North moorings
                                    "https://data.npolar.no/dataset/176eea39-7d99-49d7-a082-b18acf42850c", "https://data.npolar.no/dataset/a1239ca3-79e6-4284-bba5-38028358994a", 
                                    "https://data.npolar.no/dataset/064a09b7-f590-4448-810e-3f287b182dd2", "https://data.npolar.no/dataset/b0e473c4-b5b9-4ebc-96eb-411d47f1d850", 
                                    "https://data.npolar.no/dataset/2be7bdee-c899-45b8-901b-9ec5baa9397a", "https://data.npolar.no/dataset/a247e9a9-4b62-4149-bbf4-83df3576a7c4", 
                                    "https://data.npolar.no/dataset/6813ce6d-bdc9-4375-a310-679e074bee6b", "https://data.npolar.no/dataset/11b7e849-e53d-40d8-909b-13e29c7971a0", 
                                    # Duplicated intentionally
                                    "https://data.npolar.no/dataset/21838303-c9a0-4fc4-aac3-f537b37356df", "https://data.npolar.no/dataset/21838303-c9a0-4fc4-aac3-f537b37356df",
                                    # Duplicated intentionally
                                    "https://data.npolar.no/dataset/cd7a2f7c-abed-4284-b7c5-a9ff43c89afc", "https://data.npolar.no/dataset/cd7a2f7c-abed-4284-b7c5-a9ff43c89afc", 
                                    "https://data.npolar.no/dataset/54dcd0c9-b863-41b1-a72b-0827099ad2b0"), # South moorings
                            citation = c("Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - North (I-N) during 31 Aug 2015 to 12 Aug 2016 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.111aca43",
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - North (I-N) during 15 Oct 2016 to 2 Oct 2017 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.3078f619",
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - North (I-N) during 5 Oct 2017 to 24 Aug 2018 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.e9106051",
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during September 2005 to September 2006 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.176eea39", 
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during September 2006 to September 2007 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.a1239ca3", 
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the outer Isfjorden - South (I-S) during September 2007 to January 2008 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.064a09b7", 
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 9 Sep 2010 to 3 Sep 2011 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.b0e473c4", 
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 8 Sep 2011 to 3 Sep 2012 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.2be7bdee", 
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 6 Sep 2012 to 28 Aug 2013 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.a247e9a9", 
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 2 Sep 2013 to 26 Aug 2014 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.6813ce6d", 
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 31 Aug 2014 to 24 Aug 2015 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.11b7e849", 
                                         # Duplicated intentionally
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 31 Aug 2015 to 12 Aug 2016 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.21838303", 
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 31 Aug 2015 to 12 Aug 2016 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.21838303", 
                                         # Duplicated intentionally
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 19 Aug 2016 to 2 Oct 2017 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.cd7a2f7c", 
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 19 Aug 2016 to 2 Oct 2017 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.cd7a2f7c", 
                                         "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 5 Oct 2017 to 25 Aug 2018 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.54dcd0c9"))

  # Process data
  res <- hyper_tibble(tidync(file_name)) %>% 
    cbind(hyper_tibble(activate(tidync(file_name), "D2"))) %>%  
    mutate(date = as.Date(as.POSIXct(TIME*86400, origin = "1950-01-01", tz = "UTC")), .keep = "unused") %>% 
    dplyr::rename(lon = LONGITUDE, lat = LATITUDE, depth = MPRES) %>% 
    pivot_longer(cols = c(-"depth", -"STATION", -"lat", -"lon", -"FDEP", -"date"), names_to = "var_name", values_to = "value") %>% 
    filter(!is.na(value)) %>% 
    left_join(is_units, by = c("var_name" = "name")) %>% 
    mutate(units = case_when(units == "degree_Celsius" ~ "°C", TRUE ~ units),
           var_name = paste0(var_name, " [", units,"]"),
           var_type = "phys",
           depth = round(depth, 2),
           date_accessed = as.Date("2021-04-15"),
           URL = is_ref_info$URL[is_ref_info$short_name == file_short], 
           citation = is_ref_info$citation[is_ref_info$short_name == file_short]) %>% 
    group_by(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name) %>% 
    summarise(value = round(mean(value, na.rm = T), 5), .groups = "drop")
  return(res)
}

# Simple wrapper for loading GFI mooring NetCDF files
load_GFI <- function(file_name){
  
  # Get NetCDF metadata
  GFI_dump <- ncdump::NetCDF(file_name)
  GFI_units <- GFI_dump$variable %>% 
    filter(!name %in% c("depth", "lon", "lat")) %>% 
    dplyr::select(name, units)
  GFI_start <- GFI_dump$attribute$global$instrument_start_time
  GFI_citation <- GFI_dump$attribute$global$citation
  file_short <- sapply(strsplit(file_name, "/"), "[[", 8)
  
  # Get correct FTP link to add to data
  FTP_URL <- as.character(NA)
  if(file_short %in% c("1249_RCM_3148_QC.nc", "1250_RCM_4040_QC.nc", "1251_RCM_6798_QC.nc")) 
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/FJ_199006_A/"
  if(file_short %in% c("1252_RCM_9708_QC.nc", "1253_RCM_9993_QC.nc", "1254_RCM_235_QC.nc", "1255_RCM_6197_QC.nc")) 
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/FJ_199006_B/"
  if(file_short %in% c("1256_RCM_3160_QC.nc", "1257_RCM_9707_QC.nc", "1258_RCM_2761_QC.nc")) 
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/FJ_199006_C/"
  if(file_short %in% c("1259_RCM_8006_QC.nc", "1260_RCM_10007_QC.nc", "1261_RCM_9994_QC.nc", "1262_RCM_2016_QC.nc")) 
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/FJ_199006_D/"
  if(file_short %in% c("1263_RCM_9706_QC.nc", "1264_RCM_9130_QC.nc")) 
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/FJ_199006_E/"
  if(file_short %in% c("1883_RCM_645_QC.nc", "1884_RCM_8003_QC.nc", "1885_RCM_646_QC.nc"))
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/KF_201408/"
  if(file_short %in% c("1766_RCM_10907_QC.nc", "1856_RCM_645_QC.nc", "1857_RCM_646_QC.nc"))
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201308_A/"
  if(file_short %in% c("0712_RCM_12347_QC.nc", "1848_RCM_645_QC.nc", "1849_RCM_646_QC.nc"))
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201209_D/"
  if(file_short %in% c("0706_RCM_10907_QC.nc", "1844_RCM_645_QC.nc", "1845_RCM_646_QC.nc", "0686_RCM_8003_QC.nc", "0687_RCM_2761_QC.nc"))
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201109_A/"
  if(file_short %in% c("1954_RCM_1318_QC.nc", "1955_RCM_645_QC.nc", "1956_RCM_646_QC.nc"))
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201508_Isfjorden/"
  if(file_short %in% c("0688_RCM_784_QC.nc"))
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201009_B/"
  if(file_short %in% c("0710_RCM_783_QC.nc", "0711_RCM_784_QC.nc", "1841_RCM_464_QC.nc"))
    FTP_URL <- "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201209_A/"
  
  # Process data
  suppressWarnings(
  res <- hyper_tibble(tidync(file_name)) %>% 
    cbind(hyper_tibble(activate(tidync(file_name), "S"))) %>%  # This line throws an unneeded warning
    mutate(date = as.Date(as.POSIXct(time*86400, origin = GFI_start)), .keep = "unused") %>% 
    pivot_longer(c(GFI_units$name), names_to = "var_name", values_to = "value") %>% 
    filter(!is.na(value)) %>% 
    group_by(lon, lat, date, depth, var_name) %>% 
    summarise(value = case_when(var_name == "dir" ~ as.numeric(round(mean.circular(circular(value, units = "degrees")))),
                                TRUE ~ round(mean(value, na.rm = T), 3)), .groups = "drop") %>% 
    distinct() %>% 
    mutate(value = case_when(var_name == "dir" & value < 0 ~ value + 360, TRUE ~ value)) %>% 
    replace(is.na(.), NA) %>% 
    left_join(GFI_units, by = c("var_name" = "name")) %>% 
    mutate(URL = FTP_URL,
           citation = GFI_citation,
           units = case_when(units == "Celsius" ~ "°C", units == "degree" ~ "°", TRUE ~ units),
           var_type = "phys",
           var_name = paste0(var_name, " [", units,"]")) %>% 
    dplyr::select(URL, citation, lon, lat, date, depth, var_type, var_name, value)
  )
  return(res)
}

# Simple wrapper for loading SAMS mooring NetCDF files
# dir("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_SAMS", full.names = T)
# file_names <- dir("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_SAMS/", full.names = T)#13-25
# test1 <- load_SAMS(file_names[17])
# file_name <- file_names[1]
load_SAMS <- function(file_name){
  
  # Get NetCDF metadata
  SAMS_dump <- ncdump::NetCDF(file_name)
  SAMS_units <- SAMS_dump$variable %>% 
    filter(!name %in% c("depth", "ndepth", "bottom_depth", "nominal_depth",
                        "longitude", "latitude", "mooring", "time")) %>% 
    dplyr::select(name, units)
  SAMS_start <- SAMS_dump$attribute$global$time_coverage_start
  file_short <- sapply(strsplit(file_name, "/"), "[[", 9)
  
  # Get correct FTP link to add to data
  SAMS_URL <- as.character(NA)
  SAMS_ref <- as.character(NA)
  if(file_short %in% c("KF_17_18_pro_sbe16p_6067_34m.nc", "KF_17_18_pro_sbe37_7248_33m.nc", "KF_17_18_pro_sbe37_9114_217m.nc",
                       "KF_17_18_pro_sbe56_2444_38m.nc", "KF_17_18_pro_sbe56_2445_48m.nc", "KF_17_18_pro_sbe56_2447_59m.nc",
                       "KF_17_18_pro_sbe56_2650_80m.nc", "KF_17_18_pro_sbe56_2656_103m.nc", "KF_17_18_pro_sbe56_2657_126m.nc",
                       "KF_17_18_pro_sbe56_2658_156m.nc", "KF_17_18_pro_sbe56_2659_186m.nc", "KF_17_18_pro_sbe56_2660_217m.nc")){
    SAMS_URL <- "https://archive.sigma2.no/pages/public/datasetDetail.jsf?id=10.11582/2021.00065"
    SAMS_ref <- "Cottier, F., Berge, J., Dumont, E., Kopec, T. P., Venables, E. J., Vogedes, D. L., UiT The Arctic University of Norway, Scottish Association for Marine Science (2021).Temperature, salinity, light and fluorescence (CTD) measurements from the Kongsfjorden (Svalbard) marine observatory (mooring) August 2017-August 2018 [Data set]. Norstore. https://doi.org/10.11582/2021.00065"
  } else if(file_short %in% c("KF_16_17_pro_sbe16p_6066_26m.nc", "KF_16_17_pro_sbe37_5509_96m.nc", "KF_16_17_pro_sbe37_5510_208m.nc",
                              "KF_16_17_pro_sbe37_8478_25m.nc", "KF_16_17_pro_sbe56_5207_31m.nc", "KF_16_17_pro_sbe56_5208_41m.nc",
                              "KF_16_17_pro_sbe56_5209_51m.nc", "KF_16_17_pro_sbe56_5210_72m.nc", "KF_16_17_pro_sbe56_5211_95m.nc",
                              "KF_16_17_pro_sbe56_5212_118m.nc", "KF_16_17_pro_sbe56_5213_148m.nc", "KF_16_17_pro_sbe56_5214_178m.nc",
                              "KF_16_17_pro_sbe56_5215_208m.nc")){
    SAMS_URL <- "https://archive.sigma2.no/pages/public/datasetDetail.jsf?id=10.11582/2021.00062"
    SAMS_ref <- "Cottier, F., Berge, J., Dumont, E., Griffith, C., Beaton, J., Vogedes, D. L., UiT The Arctic University of Norway, Scottish Association for Marine Science (2021).Temperature, salinity, light and fluorescence (CTD) measurements from the Kongsfjorden (Svalbard) marine observatory (mooring) August 2016-August 2017 [Data set]. Norstore. https://doi.org/10.11582/2021.00062"
  } else if(file_short %in% c("KF_15_16_pro_sbe16p_6101_25m.nc", "KF_15_16_pro_sbe37_9112_27m.nc", "KF_15_16_pro_sbe37_9114_209m.nc",
                              "KF_15_16_pro_sbe56_2444_37m.nc", "KF_15_16_pro_sbe56_2445_47m.nc", "KF_15_16_pro_sbe56_2447_57m.nc",
                              "KF_15_16_pro_sbe56_2650_79m.nc", "KF_15_16_pro_sbe56_2656_102m.nc", "KF_15_16_pro_sbe56_2657_125m.nc",
                              "KF_15_16_pro_sbe56_2658_155m.nc", "KF_15_16_pro_sbe56_2659_185m.nc", "KF_15_16_pro_sbe56_2669_215m.nc")){
    SAMS_URL <- "https://archive.sigma2.no/pages/public/datasetDetail.jsf?id=10.11582/2021.00061"
    SAMS_ref <- "Cottier, F., Berge, J., Griffith, C., Dumont, E., Beaton, J., Vogedes, D. L., UiT The Arctic University of Norway, Scottish Association for Marine Science (2021).Temperature, salinity, light and fluorescence (CTD) measurements from the Kongsfjorden (Svalbard) marine observatory (mooring) September 2015-August 2016 [Data set]. Norstore. https://doi.org/10.11582/2021.00061"
  } else if(file_short %in% c("KF_20_20_pro_avg24h_sbe16p_5181.nc", "KF_20_20_pro_avg24h_sbe56_10012.nc", "KF_20_20_pro_avg24h_sbe56_10040.nc",
                              "KF_20_20_pro_avg24h_sbe56_10055.nc", "KF_20_20_pro_avg24h_sbe56_10059.nc", "KF_20_20_pro_avg24h_sbe56_10060.nc",
                              "KF_20_20_pro_avg24h_sbe56_10061.nc", "KF_20_20_pro_avg24h_sbe56_10062.nc", "KF_20_20_pro_avg24h_sbe56_10066.nc",
                              "KF_20_20_pro_avg24h_sbe56_10067.nc", "KF_20_20_pro_sbe16p_5181.nc", "KF_20_20_pro_sbe56_10012.nc",
                              "KF_20_20_pro_sbe56_10040.nc", "KF_20_20_pro_sbe56_10055.nc", "KF_20_20_pro_sbe56_10059.nc",
                              "KF_20_20_pro_sbe56_10060.nc", "KF_20_20_pro_sbe56_10061.nc", "KF_20_20_pro_sbe56_10062.nc",
                              "KF_20_20_pro_sbe56_10066.nc", "KF_20_20_pro_sbe56_10067.nc")){
    SAMS_URL <- "https://archive.sigma2.no/pages/public/datasetDetail.jsf?id=10.11582/2021.00010"
    SAMS_ref <- "Berge, J., Cottier, F., Kopec, T., Dumont, E., Venables, E., Vogedes, D. (2021).Temperature, salinity, light and chl a measurements from the Kongsfjorden SIOS marine observatory January-September 2020 [Data set]. Norstore. https://doi.org/10.11582/2021.00010"
  }
  
  # Get base CTD data as there are two different formats
  if(file_short %in% c("KF_20_20_pro_avg24h_sbe16p_5181.nc", "KF_20_20_pro_avg24h_sbe56_10012.nc", "KF_20_20_pro_avg24h_sbe56_10040.nc",
                       "KF_20_20_pro_avg24h_sbe56_10055.nc", "KF_20_20_pro_avg24h_sbe56_10059.nc", "KF_20_20_pro_avg24h_sbe56_10060.nc",
                       "KF_20_20_pro_avg24h_sbe56_10061.nc", "KF_20_20_pro_avg24h_sbe56_10062.nc", "KF_20_20_pro_avg24h_sbe56_10066.nc",
                       "KF_20_20_pro_avg24h_sbe56_10067.nc", "KF_20_20_pro_sbe16p_5181.nc", "KF_20_20_pro_sbe56_10012.nc",
                       "KF_20_20_pro_sbe56_10040.nc", "KF_20_20_pro_sbe56_10055.nc", "KF_20_20_pro_sbe56_10059.nc",
                       "KF_20_20_pro_sbe56_10060.nc", "KF_20_20_pro_sbe56_10061.nc", "KF_20_20_pro_sbe56_10062.nc",
                       "KF_20_20_pro_sbe56_10066.nc", "KF_20_20_pro_sbe56_10067.nc")) {
    suppressWarnings(
      res_base <- hyper_tibble(tidync(file_name)) %>% 
        cbind(hyper_tibble(activate(tidync(file_name), "S")))  # This line throws an unneeded warning
    )
  } else {
    res_base <- hyper_tibble(tidync(file_name))
  }
  
  # Correct for some missing depth columns
  if("nominal_depth" %in% colnames(res_base) & !"depth" %in% colnames(res_base)){
    res_base <- dplyr::rename(res_base, depth = nominal_depth)
  }
  if("ndepth" %in% colnames(res_base) & !"depth" %in% colnames(res_base)){
    res_base <- dplyr::rename(res_base, depth = ndepth)
  }
  
  # Process data
  res <- res_base %>% 
    mutate(date = as.Date(as.POSIXct(time, origin = "1970-01-01")), .keep = "unused") %>% 
    pivot_longer(c(SAMS_units$name), names_to = "var_name", values_to = "value") %>% 
    filter(!is.na(value)) %>% 
    dplyr::rename(lon = longitude, lat = latitude) %>% 
    group_by(lon, lat, date, var_name) %>% 
    summarise(depth = round(mean(depth, na.rm = T), 2), # The mooring moves very slightly up and down over a day
              value = round(mean(value, na.rm = T), 4), .groups = "drop") %>% 
    distinct() %>% 
    replace(is.na(.), NA) %>% 
    left_join(SAMS_units, by = c("var_name" = "name")) %>% 
    mutate(URL = SAMS_URL,
           citation = SAMS_ref,
           units = case_when(units == "degrees_Celsius" ~ "°C", TRUE ~ units),
           var_type = case_when(var_name %in% c("fluo_V", "fluo") ~ "bio", TRUE ~ "phys"),
           var_name = paste0(var_name, " [", units,"]")) %>% 
    dplyr::select(URL, citation, lon, lat, date, depth, var_type, var_name, value)
  return(res)
}

# Simple wrapper for loading met station NetCDF data
# TODO: Add code to this that creates a reference from global info in the NetCDF file
load_met_NetCDF <- function(file_name){
  
  # Get NetCDF metadata
  file_short <- sapply(strsplit(file_name, "/"), "[[", 5)
  
  # Determine URL
  met_URL <- paste0("https://thredds.met.no/thredds/catalog/met.no/observations/stations/catalog.html?dataset=met.no/observations/stations/",file_short)
  
  # Process data
  suppressWarnings(
  res <- hyper_tibble(tidync(file_name)) %>% 
    mutate(across(everything(), ~replace(., . == 9969209968386869046778552952102584320, NA)),
           date = as.Date(as.POSIXct(time, origin = "1970-01-01"))) %>% 
    group_by(date) %>% 
    summarise(air_temperature_2m = mean(air_temperature_2m, na.rm = T),
              air_pressure_at_sea_level = mean(air_pressure_at_sea_level, na.rm = T),
              surface_air_pressure_2m = mean(surface_air_pressure_2m, na.rm = T),
              wind_speed_10m = mean(wind_speed_10m, na.rm = T),
              relative_humidity = mean(relative_humidity, na.rm = T),
              air_pressure_at_sea_level_qnh = mean(air_pressure_at_sea_level_qnh, na.rm = T),
              wind_from_direction_10m = as.numeric(round(mean.circular(circular(wind_from_direction_10m, units = "degrees"), na.rm = T)))) %>% 
    cbind(hyper_tibble(activate(tidync(file_name), "S"))) %>%  # This line throws an unneeded warning
    dplyr::rename(lon = longitude, lat = latitude) %>% 
    pivot_longer(air_temperature_2m:air_pressure_at_sea_level_qnh, names_to = "var_name", values_to = "value") %>% 
    filter(!is.na(value)) %>% 
    mutate(URL = met_URL,
           citation = NA, depth = NA,
           units = case_when(var_name == "relative_humidity" ~ "1",
                             var_name == "surface_air_pressure_2m" ~ "Pa",
                             var_name == "air_temperature_2m" ~ "K",
                             var_name == "wind_from_direction_10m" ~ "°",
                             var_name == "wind_speed_10m" ~ "m s-1",
                             var_name == "air_pressure_at_sea_level" ~ "Pa",
                             var_name == "air_pressure_at_sea_level_qnh" ~ "hPa"),
           var_name = paste0(var_name," [", units,"]"),
           var_type = "phys") %>% 
    dplyr::select(URL, citation, lon, lat, date, depth, var_type, var_name, value)
  )
  return(res)
}

# Function for loading Norwegian hydrographic data
# year_choice <- 2009 # tester...
load_nor_hydro <- function(year_choice, date_accessed){
  
  # Get file names
  nor_hydro_files <- dir("~/pCloudDrive/FACE-IT_data/porsangerfjorden/UiT", full.names = T)
  
  # Prepare URL and citation info
  cite_info <- data.frame(year = c(1953, 1955, 1957, 1958, 1959, 1966, 1979, 1980, 1981, 1982, 1983, 
                                   1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 
                                   1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 
                                   2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013),
                          URL = c("https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/KOBJ5W",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/J1BEYR",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/P916SW",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/AHXUIC",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/VQHYNU",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/JFNJXM",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/JTOHCC",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/EGG0AF",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/18AWWA",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/OJBUB9",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/E8YQPO",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/4LDOM0",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/DQHZYY",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/BOBHPL",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/SATEDJ",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/Y7QF58",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/ABONWP",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/LYOY9A",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/D6WLUE",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/M2IWUU",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/QY9WNB",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/G6INDC",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/BYDJU6",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/RYFBFS",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/7CNDUJ",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/XOHNT7",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/PFJWSN",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/FD6BVN",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/0NGBMI",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/RKCFIP",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/H3NZSX",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/FXHQYN",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/9IR4ML",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/JEZXYF",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/KSHAH5",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/CT6XYN",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/DFQSRS",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/PPIVEO",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/20VUXR",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/EZPFBU",
                                  "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/HDUPX3"),
                          citation = c('Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2019, "Hydrographic data from Northern Norwegian fjords - 1953", https://doi.org/10.18710/KOBJ5W, DataverseNO, V1, UNF:6:U+/7OuDYgkpYfywsAA4hcg== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2019, "Hydrographic data from Northern Norwegian fjords - 1955", https://doi.org/10.18710/J1BEYR, DataverseNO, V1, UNF:6:4kJ+l/55yWZvA2Sig7j1/Q== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2019, "Hydrographic data from Northern Norwegian fjords - 1957", https://doi.org/10.18710/P916SW, DataverseNO, V1, UNF:6:DCfYcMk77aH4awCjob44aQ== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2019, "Hydrographic data from Northern Norwegian fjords - 1958", https://doi.org/10.18710/AHXUIC, DataverseNO, V1, UNF:6:ZT2z1HQ5AEh1ElIohNbAAA== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2019, "Hydrographic data from Northern Norwegian fjords - 1959", https://doi.org/10.18710/VQHYNU, DataverseNO, V1, UNF:6:Ga6QZneh2eYPjYui9OlD0Q== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2019, "Hydrographic data from Northern Norwegian fjords - 1966", https://doi.org/10.18710/JFNJXM, DataverseNO, V1, UNF:6:nrDwQaaH0qbImYgo+ZsbCA== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf, 2019, "Hydrographic data from Northern Norwegian fjords - 1979", https://doi.org/10.18710/JTOHCC, DataverseNO, V1, UNF:6:mnZkBDq8vSnj4ZHNXzu0Ow== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2019, "Hydrographic data from Northern Norwegian fjords - 1980", https://doi.org/10.18710/EGG0AF, DataverseNO, V1, UNF:6:LgKTqfUGRvwLiaMD1+AunQ== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2019, "Hydrographic data from Northern Norwegian fjords - 1981", https://doi.org/10.18710/18AWWA, DataverseNO, V1, UNF:6:qtDFSir+lAFFcK5NinXMSg== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2019, "Hydrographic data from Northern Norwegian fjords - 1982", https://doi.org/10.18710/OJBUB9, DataverseNO, V1, UNF:6:0MyG3WJe0WTvqQMEw81dSQ== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2019, "Hydrographic data from Northern Norwegian fjords - 1983", https://doi.org/10.18710/E8YQPO, DataverseNO, V1, UNF:6:Nz6wtToO689GyTSm7lSteA== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2019, "Hydrographic data from Northern Norwegian fjords - 1984", https://doi.org/10.18710/4LDOM0, DataverseNO, V1, UNF:6:pnZJNguSfyRl92dhNq8Iig== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2019, "Hydrographic data from Northern Norwegian fjords - 1985", https://doi.org/10.18710/DQHZYY, DataverseNO, V1, UNF:6:rwZj9dKm4dCOXdzCWjmYCw== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2019, "Hydrographic data from Northern Norwegian fjords - 1986", https://doi.org/10.18710/BOBHPL, DataverseNO, V1, UNF:6:FzYW+79TOxhtiLq7opQGBg== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2019, "Hydrographic data from Northern Norwegian fjords - 1987", https://doi.org/10.18710/SATEDJ, DataverseNO, V1, UNF:6:YLM/LNPpIEl64VjAgflbug== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2019, "Hydrographic data from Northern Norwegian fjords - 1988", https://doi.org/10.18710/Y7QF58, DataverseNO, V1, UNF:6:3GX0zJ87HG2hqQvp2ZkbWg== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2019, "Hydrographic data from Northern Norwegian fjords - 1989", https://doi.org/10.18710/ABONWP, DataverseNO, V1, UNF:6:+pxpAE3V7Sw/LNhL3ZVA3Q== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 1990", https://doi.org/10.18710/LYOY9A, DataverseNO, V1, UNF:6:tdP+AhP2kKYjoypOmCLYSw== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 1991", https://doi.org/10.18710/D6WLUE, DataverseNO, V1, UNF:6:3tap+ji0IJP6OZm94bG5Fw== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 1992", https://doi.org/10.18710/M2IWUU, DataverseNO, V1, UNF:6:HWjN+ptN14Crh9QVC+42Gw== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 1993", https://doi.org/10.18710/QY9WNB, DataverseNO, V1, UNF:6:sQsN3YOltOxjzg5dotk5zg== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 1994", https://doi.org/10.18710/G6INDC, DataverseNO, V1, UNF:6:cju+LW3fpKYGvIcvypCjdA== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 1995", https://doi.org/10.18710/BYDJU6, DataverseNO, V1, UNF:6:l7fIO8PYNU4Mk7Yk42cURA== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 1996", https://doi.org/10.18710/RYFBFS, DataverseNO, V1, UNF:6:vEmlDRhivki61fHOQ6ijzA== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 1997", https://doi.org/10.18710/7CNDUJ, DataverseNO, V1, UNF:6:l6GPGt1jCCLNzhKPwm6ZRA== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 1998", https://doi.org/10.18710/XOHNT7, DataverseNO, V1, UNF:6:U+YggbzuI+fW+9ueqvd3uA== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 1999", https://doi.org/10.18710/PFJWSN, DataverseNO, V1, UNF:6:h4mv7XkepqZ0OVIHm9DS6A== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 2000", https://doi.org/10.18710/FD6BVN, DataverseNO, V1, UNF:6:R3dstLk3piyR7hjtbXiu8w== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 2001", https://doi.org/10.18710/0NGBMI, DataverseNO, V1, UNF:6:Jr2Ism1cRI8wmXkcX4sP2Q== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 2002", https://doi.org/10.18710/RKCFIP, DataverseNO, V1, UNF:6:wpaUwIuvwW7taPbBnTfLEQ== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 2003", https://doi.org/10.18710/H3NZSX, DataverseNO, V1, UNF:6:LR5EjU5pBBLIjThcxBTS1w== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 2004", https://doi.org/10.18710/FXHQYN, DataverseNO, V1, UNF:6:XKq/t0WTJIPesFbgdUBuJA== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 2005", https://doi.org/10.18710/9IR4ML, DataverseNO, V1, UNF:6:InE/gQ/5sZo8A4b1XF33zg== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 2006", https://doi.org/10.18710/JEZXYF, DataverseNO, V1, UNF:6:XHlv71Z+03e23XnEWrJasQ== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 2007", https://doi.org/10.18710/KSHAH5, DataverseNO, V1, UNF:6:s/c5PNYshJLXmJ8WXxteIg== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 2008", https://doi.org/10.18710/CT6XYN, DataverseNO, V1, UNF:6:In414SgzxVtks0XOvJAUvw== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 2009", https://doi.org/10.18710/DFQSRS, DataverseNO, V1, UNF:6:aHfoNHJB2YHbAvj01vzmsQ== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 2010", https://doi.org/10.18710/PPIVEO, DataverseNO, V1, UNF:6:eUOo8mYL1/ZWp+jmLIY7kA== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 2011", https://doi.org/10.18710/20VUXR, DataverseNO, V1, UNF:6:Ku9NJ9iIJUPTHMsi2X6SQA== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 2012", https://doi.org/10.18710/EZPFBU, DataverseNO, V1, UNF:6:vAkMKNoql1CS8Z46ZXdH4g== [fileUNF] ',
                                       'Mankettikkara, Rahman; Normann, Ulf; Eilertsen, Hans Chr, 2018, "Hydrographic data from Northern Norwegian fjords - 2013", https://doi.org/10.18710/HDUPX3, DataverseNO, V1, UNF:6:sjeZqtEw7ZmwTEWTkLHpNg== [fileUNF] '))
  
  # Load the historic data if a specific year isn't called
  if(year_choice < 1953){
    suppressMessages(
    res_raw <- read_csv(nor_hydro_files[grep("historic", nor_hydro_files)]) %>% 
      mutate(URL = "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/ZEHCIN",
             citation = 'Mankettikkara, Rahman; Eilertsen, Hans Chr, 2018, "Historic hydrographic data from Northern Norwegian fjords and coasts", https://doi.org/10.18710/ZEHCIN, DataverseNO, V1, UNF:6:m1/JT5Q0HncOrUyhzsY05Q== [fileUNF] ') %>% 
      dplyr::rename(temp = Temp, sal = Salinity, dens = Density, depth = `Depth (m)`, date = Date, lat = `Lat (N)`, lon = `Lon (E)`)
    )
  } else {
    sub_files <- nor_hydro_files[grep(year_choice, nor_hydro_files)]
    if(length(sub_files) == 0) return()
    suppressMessages(res_data <- read_csv(sub_files[1]))
    suppressMessages(res_stations <- read_csv(sub_files[2]))
    if(typeof(res_data$station_number) != typeof(res_stations$`Station Number`)){
      res_data$station_number <- as.numeric(res_data$station_number)
      res_stations$`Station Number` <- as.numeric(res_stations$`Station Number`)
    }
    res_raw <- left_join(res_data, res_stations, by = c("station_number" = "Station Number")) %>% 
      mutate(URL = cite_info$URL[cite_info$year == year_choice],
             citation = cite_info$citation[cite_info$year == year_choice]) %>% 
      dplyr::rename(date = Date, lat = `Latitude °N`, lon = `Longitude °E`)
  }
  
  # Detect columns in data for pivoting longer
  col_pivot <- colnames(res_raw)[grepl("temp|sal|dens", colnames(res_raw))]
  
  # Process and exit
  res <- res_raw %>% 
    filter(sal != -9999,
           lon >= bbox_por[1], lon <= bbox_por[2],
           lat >= bbox_por[3], lat <= bbox_por[4]) %>% 
    pivot_longer(all_of(col_pivot), names_to = "var_name", values_to = "value") %>% 
    mutate(date_accessed = date_accessed, var_type = "phys",
           var_name = case_when(var_name == "temp" ~ "temp [°C]", 
                                var_name == "sal" ~ "sal [PSU]",
                                var_name == "dens" ~ "dens [kg/m3]"),
           depth = as.numeric(depth)) %>% 
    dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value) %>% 
    distinct()
  # print(paste0("Loaded ",lubridate::year(min(res_raw$date)),": ", nrow(res)," rows"))
  return(res)
}

# Function for loading multiple similar model files
load_model <- function(file_stub, pCloud = F){
  
  # Set file pathway
  if(pCloud){
    file_path <- "~/pCloudDrive/FACE-IT_data/model/"
  } else{
    file_path <- "~/WP1/data/model/"
  }
  
  # Basic model info
  # ncdump::NetCDF(paste0(file_path, file_stub, "2.6.nc"))
  model_vars <- ncdump::NetCDF(paste0(file_path, file_stub, "2.6.nc"))$variable
  model_coords <- tidync::tidync(paste0(file_path, file_stub, "2.6.nc")) %>% activate("D0,D1") %>% hyper_tibble()
  
  # Load individual RCPs
  model_2.6 <- tidync::tidync(paste0(file_path, file_stub, "2.6.nc")) %>% hyper_tibble() %>% mutate(proj = "RCP 2.6")
  model_4.5 <- tidync::tidync(paste0(file_path, file_stub, "4.5.nc")) %>% hyper_tibble() %>% mutate(proj = "RCP 4.5")
  model_8.5 <- tidync::tidync(paste0(file_path, file_stub, "8.5.nc")) %>% hyper_tibble() %>% mutate(proj = "RCP 8.5")
  
  # Combine all RCPs and exit
  model_all <- rbind(model_2.6, model_4.5, model_8.5) %>% 
    left_join(model_coords, by = c("X", "Y")) %>% 
    mutate(date = as.Date(as.character(as.PCICt(T*3600, cal = "noleap", origin = "1950-01-01 01:00:00", tz = "UTC")))) %>% 
    dplyr::rename(lon = Long, lat = Latt, topo = Topo, land = RMask, depth = Z) %>% 
    dplyr::select(proj, land, lon, lat, topo, date, depth, Salt:pco2w)
  return(model_all)
}

# Data summary plotting function
data_summary_plot <- function(full_product, site_name){
  
  # Tweaks for consistent plotting
  full_product <-  full_product %>% 
    mutate(var_type = as.factor(var_type),
           year = lubridate::year(date))
  
  # get correct bounding box
  bbox_plot <- bbox_from_name(site_name)
  
  # Pixel size
  if(range(full_product$lat, na.rm = T)[2]-range(full_product$lat, na.rm = T)[1] > 1){
    res_round <- 1
    res_text <- "0.1° (~10 km) resolution"
  } else{
    res_round <- 2
    res_text <- "0.01° (~1 km) resolution"
  }
  
  # Table of meta-stats
  meta_table <- data.frame(table(full_product$var_type)) %>% 
    pivot_wider(names_from = Var1, values_from = Freq) %>% 
    mutate(files = length(unique(full_product$citation)),
           lon = paste0(round(min(full_product$lon, na.rm = T), 2), " to ", round(max(full_product$lon, na.rm = T), 2)), 
           lat = paste0(round(min(full_product$lat, na.rm = T), 2), " to ", round(max(full_product$lat, na.rm = T), 2)),
           date = paste0(min(full_product$year, na.rm = T), " to ", max(full_product$year, na.rm = T)),
           depth = paste0(min(full_product$depth, na.rm = T), " to ", max(full_product$depth, na.rm = T))) %>% #,
           # site = site_name) %>%
    dplyr::select(files, lon, lat, date, depth, everything())
  
  # Graphic version
  # meta_table_g <- gtable_add_grob(tableGrob(meta_table, rows = NULL),
  #                                 grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
  #                                 t = 2, b = nrow(meta_table), l = 1, r = ncol(meta_table))
  meta_table_g <- tableGrob(meta_table, rows = NULL)# + labs(title = "Meta-data")
  
  # Clip coastline polygons for faster plotting
  coastline_full_df_sub <- coastline_full_df %>% 
    filter(x >= min(full_product$lon, na.rm = T)-10,
           x <= max(full_product$lon, na.rm = T)+10,
           y >= min(full_product$lat, na.rm = T)-10,
           y <= max(full_product$lat, na.rm = T)+10)
  
  # Count per grid cell
  plot_spatial <- full_product %>% 
    mutate(lon = round(lon, res_round),
           lat = round(lat, res_round)) %>% 
    group_by(lon, lat) %>% 
    summarise(count = n(), .groups = "drop") %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_polygon(data = coastline_full_df_sub, aes(x = x, y = y, group = polygon_id), 
                 fill = "grey70", colour = "black") +
    geom_tile(aes(fill = log10(count))) +
    scale_fill_viridis_c(option = "E") +
    coord_quickmap(expand = F,
                   xlim = c(bbox_plot[1:2]), 
                   ylim = c(bbox_plot[3:4])) +
    labs(x = NULL, y = NULL, fill = "Count\n(log10)",
         title = paste0("Count of data binned at ", res_text)) +
    theme(panel.border = element_rect(fill = NA, colour = "black"),
          legend.position = "bottom")
  # plot_spatial
  
  # Count of data over time
  plot_time <- full_product %>% 
    mutate(year = lubridate::year(date)) %>%
    group_by(year, var_type) %>% 
    dplyr::summarise(count = n(), .groups = "drop") %>% 
    ggplot() +
    # geom_col(aes(x = year, y = count, fill = var_type)) +
    geom_col(aes(x = year, y = log10(count), fill = var_type), width = 1, show.legend = F) +
    coord_cartesian(expand = F) +
    scale_fill_manual(values = CatColAbr) +
    labs(x = NULL, fill = "Variable", y = "Count\n(log10)",
         title = "Count of data per year") +
    theme(panel.border = element_rect(fill = NA, colour = "black"), legend.position = "bottom")
  # plot_time
  
  # Count of data at depth by var type
  plot_depth <- full_product %>% 
    # filter(depth >= 0) %>%
    # filter(!is.na(depth)) %>% 
    mutate(depth = ifelse(is.na(depth), 0, depth)) %>%
    mutate(depth = round(depth, -1)) %>%
    group_by(depth, var_type) %>% 
    dplyr::summarise(count = n(), .groups = "drop") %>% 
    ggplot() +
    geom_col(aes(x = depth, y = log10(count), fill = var_type), show.legend = F) +
    scale_x_reverse() +
    coord_flip(expand = F) +
    scale_fill_manual(values = CatColAbr) +
    # guides(fill = guide_legend(nrow = length(unique(full_product$var_type)))) +
    labs(x = NULL, fill = "Variable", y = "Count (log10)",
         title = "Count of data at\ndepth (10 m bins)") +
    theme(panel.border = element_rect(fill = NA, colour = "black"), legend.position = "bottom")
  # plot_depth
  
  # Plot legend for categories
  plot_blank <- full_product %>% 
    mutate(year = lubridate::year(date)) %>%
    group_by(year, var_type) %>% 
    dplyr::summarise(count = n(), .groups = "drop") %>% 
    ggplot() +
    geom_col(aes(x = year, y = count, fill = var_type), width = 1) +
    scale_fill_manual(values = CatColAbr) +
    guides(fill = guide_legend(nrow = length(unique(full_product$var_type)))) +
    labs(fill = "Variable Type")
  plot_legend <- ggpubr::get_legend(plot_blank)
  
  # Full summary plot
  plot_summary_left <- ggpubr::ggarrange(plot_spatial, plot_time, labels = c("B)", "C)"), nrow = 2, heights = c(1, 0.4))
  plot_summary_right <- ggpubr::ggarrange(plot_depth, plot_legend, labels = c("D)", ""), nrow = 2, heights = c(1, 0.4))
  plot_summary_bottom <- ggpubr::ggarrange(plot_summary_left, plot_summary_right, nrow = 1, labels = c("", "D)"), widths = c(1, 0.3))
  # plot_summary <- ggpubr::ggarrange(meta_table_g, plot_summary_bottom, ncol = 1, heights = c(0.1, 1))
  plot_summary <- ggpubr::ggarrange(meta_table_g, plot_summary_bottom, heights = c(0.2, 1), labels = c("A)", ""), ncol = 1) +
    theme(plot.background = element_rect(fill = "white", color = NA))
  return(plot_summary)
}

# Data climatology plotting function
data_clim_plot <- function(full_product, site_name){
  
  # Create factors for more consistent plotting
  full_product$var_type <- as.factor(full_product$var_type)
  
  # get correct bounding box
  bbox_plot <- bbox_from_name(site_name)
  
  # Pixel size
  if(range(full_product$lat, na.rm = T)[2]-range(full_product$lat, na.rm = T)[1] > 1){
    res_round <- 1
    res_text <- "0.1° (~10 km) resolution"
  } else{
    res_round <- 2
    res_text <- "0.01° (~1 km) resolution"
  }
  
  # Calculate monthly depth climatologies per pixel
  depth_monthly_clims_pixel <- full_product %>% 
    filter(depth >= 0) %>% 
    filter(value < 9999) %>% 
    mutate(lon = round(lon, res_round),
           lat = round(lat, res_round),
           month = lubridate::month(date),
           depth = round(depth, -1),
           depth_cat = case_when(depth > 0 & depth <= 10 ~ "0 - 10 m",
                                 depth > 10 & depth <= 50 ~ "10 - 50 m",
                                 depth > 50 & depth <= 200 ~ "50 - 200 m",
                                 depth > 200 & depth <= 1000 ~ "200 - 1000 m",
                                 depth > 1000 & depth <= 2000 ~ "1000 - 2000 m",
                                 depth > 2000 ~ "2000+ m",
                                 TRUE ~ as.character(NA)),
           depth_cat = factor(depth_cat, levels = c("0 - 10 m", "10 - 50 m", "50 - 200 m", 
                                                    "200 - 1000 m", "1000 - 2000 m", "2000+ m")),
           var_cat = case_when(grepl("°C", var_name, ignore.case = F) ~ "temp",
                               grepl("sal", var_name, ignore.case = F) ~ "sal",
                               grepl("cndc", var_name, ignore.case = F) ~ "sal")) %>% 
    filter(!is.na(depth_cat), !is.na(var_cat), !is.na(month)) %>% 
    group_by(lon, lat, month, depth_cat, var_cat) %>% 
    summarise(value = mean(value, na.rm = T), .groups = "drop") %>% 
    right_join(expand.grid(month = 1:12, depth_cat = c("0 - 10 m", "10 - 50 m", "50 - 200 m", "200 - 1000 m", 
                                                       "1000 - 2000 m", "2000+ m"), var_cat = c("temp", "sal")),
               by = c("month", "depth_cat", "var_cat"))
  
  # Calculate monthly depth climatologies
  depth_monthly_clims <- depth_monthly_clims_pixel %>%
    group_by(month, depth_cat, var_cat) %>%
    summarise(value = mean(value, na.rm = T), .groups = "drop")
  
  # Get limits for consistent legends
  lims_temp <- filter(depth_monthly_clims_pixel, var_cat == "temp")
  lims_sal <- filter(depth_monthly_clims_pixel, var_cat == "sal")
  
  # Clip coastline polygons for faster plotting
  coastline_full_df_sub <- coastline_full_df %>% 
    filter(x >= min(full_product$lon, na.rm = T)-10,
           x <= max(full_product$lon, na.rm = T)+10,
           y >= min(full_product$lat, na.rm = T)-10,
           y <= max(full_product$lat, na.rm = T)+10)
  
  # Plot overall monthly depth temperature clims
  plot_depth_temp_clims <- depth_monthly_clims %>% 
    filter(var_cat == "temp") %>%
    ggplot() +
    geom_tile(aes(x = as.factor(month), y = depth_cat, fill = value), show.legend = F) +
    scale_y_discrete(limits = rev) +
    scale_fill_viridis_c(limits = range(lims_temp$value, na.rm = T)) +
    labs(x = "Month", y = "Depth", fill = "Temp. (°C)", title = "Temperature climatologies at depth") +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(fill = NA, colour = "black"))
  # plot_depth_temp_clims
  
  # Plot temperature clims per pixel
  plot_spatial_temp_clim <- depth_monthly_clims_pixel %>% 
    filter(var_cat == "temp", depth_cat == "0 - 10 m") %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_polygon(data = coastline_full_df_sub, aes(x = x, y = y, group = polygon_id), 
                 fill = "grey70", colour = "black") +
    geom_tile(aes(fill = value)) +
    scale_fill_viridis_c(limits = range(lims_temp$value, na.rm = T)) +
    coord_quickmap(xlim = c(bbox_plot[1:2]), 
                   ylim = c(bbox_plot[3:4])) +
    facet_wrap(~month) +
    labs(x = NULL, y = NULL, fill = "Temp. (°C)",
         title = paste0("Surface (0 to 10 m) temperature clims at ", res_text)) +
    theme(panel.border = element_rect(fill = NA, colour = "black"), 
          legend.direction = "horizontal")
  # plot_spatial_temp_clim
  
  # Plot overall monthly depth salinity clims
  plot_depth_sal_clims <- depth_monthly_clims %>% 
    filter(var_cat == "sal") %>%
    ggplot() +
    geom_tile(aes(x = as.factor(month), y = depth_cat, fill = value), show.legend = F) +
    scale_y_discrete(limits = rev) +
    scale_fill_viridis_c(option = "A", limits = range(lims_sal$value, na.rm = T)) +
    labs(x = "Month", y = "Depth", fill = "Salinity", title = "Salinity climatologies at depth") +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(fill = NA, colour = "black"))
  # plot_depth_sal_clims
  
  # Plot salinity clims per pixel
  plot_spatial_sal_clim <- depth_monthly_clims_pixel %>% 
    filter(var_cat == "sal", depth_cat == "0 - 10 m") %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_polygon(data = coastline_full_df_sub, aes(x = x, y = y, group = polygon_id), 
                 fill = "grey70", colour = "black") +
    geom_tile(aes(fill = value)) +
    scale_fill_viridis_c(option = "A") +
    coord_quickmap(xlim = c(bbox_plot[1:2]), 
                   ylim = c(bbox_plot[3:4])) +
    facet_wrap(~month) +
    labs(x = NULL, y = NULL, fill = "Salinity",
         title = paste0("Surface (0 to 10 m) salinity clims at ", res_text)) +
    theme(panel.border = element_rect(fill = NA, colour = "black"),
          legend.direction = "horizontal")
  # plot_spatial_sal_clim
  
  # Put them together
  plot_clim <- ggpubr::ggarrange(plot_depth_temp_clims, ggpubr::get_legend(plot_spatial_temp_clim), plot_spatial_temp_clim + theme(legend.position = "none"), 
                                 plot_depth_sal_clims, ggpubr::get_legend(plot_spatial_sal_clim), plot_spatial_sal_clim + theme(legend.position = "none"),
                                 heights = c(0.3, 0.1, 1, 0.3, 0.1, 1), labels = c("A)", "", "B)", "C)", "", "D)"), ncol = 1, nrow = 6) +
    theme(plot.background = element_rect(fill = "white", color = NA))
  return(plot_clim)
}

# Function for calculating and plotting the trends in the data from full products
data_trend_plot <- function(full_product, site_name){
  
  # Create factors for more consistent plotting
  full_product$var_type <- as.factor(full_product$var_type)
  
  # get correct bounding box
  bbox_plot <- bbox_from_name(site_name)
  
  # Pixel size
  if(range(full_product$lat, na.rm = T)[2]-range(full_product$lat, na.rm = T)[1] > 1){
    res_round <- 1
    res_text <- "0.1° (~10 km) resolution"
  } else{
    res_round <- 2
    res_text <- "0.01° (~1 km) resolution"
  }
  
  # Calculate trends 
  depth_trend_pixel <- full_product %>% 
    filter(depth >= 0) %>% 
    filter(value < 9999) %>% 
    mutate(lon = round(lon, res_round),
           lat = round(lat, res_round),
           year = lubridate::year(date),
           depth = round(depth, -1),
           depth_cat = case_when(depth > 0 & depth <= 10 ~ "0 - 10 m",
                                 depth > 10 & depth <= 50 ~ "10 - 50 m",
                                 depth > 50 & depth <= 200 ~ "50 - 200 m",
                                 depth > 200 & depth <= 1000 ~ "200 - 1000 m",
                                 depth > 1000 & depth <= 2000 ~ "1000 - 2000 m",
                                 depth > 2000 ~ "2000+ m",
                                 TRUE ~ as.character(NA)),
           depth_cat = factor(depth_cat, levels = c("0 - 10 m", "10 - 50 m", "50 - 200 m", 
                                                    "200 - 1000 m", "1000 - 2000 m", "2000+ m")),
           var_cat = case_when(grepl("°C", var_name, ignore.case = F) ~ "temp",
                               grepl("sal", var_name, ignore.case = F) ~ "sal",
                               grepl("cndc", var_name, ignore.case = F) ~ "sal")) %>% 
    filter(!is.na(depth_cat), !is.na(var_cat), !is.na(year)) %>% 
    group_by(lon, lat, year, depth_cat, var_cat) %>% 
    summarise(value = mean(value, na.rm = T), .groups = "drop") %>%
    # mutate(plot_group = as.numeric(as.factor(paste0(lon, lat)))) %>% 
    right_join(expand.grid(year = seq(min(lubridate::year(full_product$date), na.rm = T), 
                                      max(lubridate::year(full_product$date), na.rm = T)),
                           depth_cat = c("0 - 10 m", "10 - 50 m", "50 - 200 m", "200 - 1000 m", 
                                         "1000 - 2000 m", "2000+ m"), var_cat = c("temp", "sal")),
               by = c("year", "depth_cat", "var_cat")) %>%
    filter(!is.na(lon)) %>% 
    mutate(lonlat = paste0(lon, lat))
    # group_by(lon, lat, year, depth_cat, var_cat) %>%
    # do(broom::tidy(lm(value ~ year, .)))
  
  # Calculate depth trends
  depth_trend <- depth_trend_pixel %>%
    group_by(year, depth_cat, var_cat) %>%
    summarise(value = mean(value, na.rm = T), .groups = "drop")
  
  # Get limits for consistent plotting
  lims_temp <- filter(depth_trend_pixel, var_cat == "temp")
  lims_sal <- filter(depth_trend_pixel, var_cat == "sal")
  
  # Plot temperature trends
  # TODO: Remove the raw data points once you figure out what is funny with them
  # TODO: Make all symbols and lines smaller
  plot_trend_temp <- depth_trend_pixel %>% 
    filter(var_cat == "temp") %>%
    ggplot(aes(x = year, y = value)) +
    # geom_point(aes(colour = depth_cat), alpha = 0.5) +
    # geom_line(aes(group = lonlat), stat = "smooth", method = "lm", formula = y ~ x, linetype = "dashed", alpha = 0.5) +
    geom_point(data = filter(depth_trend, var_cat == "temp"), aes(group = depth_cat), colour = "black", size = 3, shape = 18, alpha = 0.7) +
    geom_point(data = filter(depth_trend, var_cat == "temp"), aes(colour = depth_cat), size = 2, shape = 18, alpha = 0.7) +
    geom_smooth(data = filter(depth_trend, var_cat == "temp"), aes(group = depth_cat), colour = "black", method = "lm", se = F, size = 2, alpha = 0.7) +
    geom_smooth(data = filter(depth_trend, var_cat == "temp"), aes(colour = depth_cat), method = "lm", se = F, size = 1, alpha = 0.7) +
    coord_cartesian(expand = F, ylim = range(lims_temp$value)) +
    scale_colour_manual(values = DepthCol) +
    labs(x = NULL, y = "Temp. (°C)", colour = "Depth", title = "Temperature trends at depth") +
    theme(panel.border = element_rect(fill = NA, colour = "black"), 
          legend.direction = "horizontal", legend.position = "bottom")
  # plot_trend_temp
  
  # Plot salinity trends
  plot_trend_sal <- depth_trend_pixel %>% 
    filter(var_cat == "sal") %>%
    ggplot(aes(x = year, y = value)) +
    # geom_point(aes(colour = depth_cat), alpha = 0.5) +
    # geom_line(aes(group = lonlat), stat = "smooth", method = "lm", formula = y ~ x, linetype = "dashed", alpha = 0.5) +
    geom_point(data = filter(depth_trend, var_cat == "sal"), aes(group = depth_cat), colour = "black", size = 3, shape = 18, alpha = 0.7) +
    geom_point(data = filter(depth_trend, var_cat == "sal"), aes(colour = depth_cat), size = 2, shape = 18, alpha = 0.7) +
    geom_smooth(data = filter(depth_trend, var_cat == "sal"), aes(group = depth_cat), colour = "black", method = "lm", se = F, size = 2, alpha = 0.7) +
    geom_smooth(data = filter(depth_trend, var_cat == "sal"), aes(colour = depth_cat), method = "lm", se = F, size = 1, alpha = 0.7) +
    coord_cartesian(expand = F, ylim = range(lims_sal$value)) +
    scale_colour_manual(values = DepthCol) +
    labs(x = NULL, y = "Salinity", colour = "Depth", title = "Salinity trends at depth") +
    theme(panel.border = element_rect(fill = NA, colour = "black"))
  # plot_trend_sal
  
  # Put them together
  plot_trend <- ggpubr::ggarrange(plot_trend_temp + theme(legend.position = "none"), ggpubr::get_legend(plot_trend_temp),  plot_trend_sal + theme(legend.position = "none"), 
                                  ncol = 1, labels = c("A)", "", "B)"), heights = c(1, 0.2, 1)) +
    theme(plot.background = element_rect(fill = "white", color = NA))
  return(plot_trend)
}

# Function for plotting a quick summary of a model product
model_summary <- function(model_product, site_name){
  
  # Clip coastline polygons for faster plotting
  coastline_full_df_sub <- coastline_full_df %>% 
    filter(x >= min(model_product$lon, na.rm = T)-10,
           x <= max(model_product$lon, na.rm = T)+10,
           y >= min(model_product$lat, na.rm = T)-10,
           y <= max(model_product$lat, na.rm = T)+10)
  
  # Spatial temperature
  ## TODO: Only show one map, no facets
  plot_map <- model_product %>% 
    filter(land == 1) %>% 
    group_by(proj, lon, lat, depth) %>% 
    summarise_all(mean) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_polygon(data = coastline_full_df_sub, fill = "grey70", colour = "black",
                 aes(x = x, y = y, group = polygon_id)) +  
    geom_point(aes(colour = Temp), size = 3) +
    scale_colour_viridis_c() +
    coord_quickmap(xlim = range(model_product$lon),
                   ylim = range(model_product$lat)) +
    facet_grid(proj ~ depth) +
    labs(x = NULL, y = NULL, colour = "Temp. (°C)",
         title = "Average temperature per pixel")
  
  # Spatial temperature time series
  plot_trend <- model_product %>% 
    filter(land == 1) %>% 
    mutate(year = lubridate::year(date)) %>% 
    group_by(depth, proj, year) %>% 
    summarise_all(mean) %>% 
    ggplot(aes(x = year, y = Temp)) +
    geom_point(aes(colour = proj), alpha = 0.25, show.legend = F) +
    geom_smooth(method = "lm", se = F, colour = "black", size = 2, show.legend = F) +
    geom_smooth(method = "lm", se = F, aes(colour = proj), show.legend = F) +
    scale_x_continuous(expand = c(0, 0)) +
    facet_grid(proj~depth) +
    labs(x = NULL, y  = "Temperature (°C)", 
         title = "Average temperature across all pixels")
  
  # Combine and exit
  plot_all <- ggpubr::ggarrange(plot_map, plot_trend, ncol = 1, labels = c("A)", "B)"), heights = c(2, 1)) +
    theme(plot.background = element_rect(fill = "white", color = NA))
  return(plot_all)
}

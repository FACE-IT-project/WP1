# code/functions.R
# This script houses functions and other preparatory things used throughout the project


# Setup -------------------------------------------------------------------

# Libraries used in all other scripts
library(tidyverse)
library(grid)
library(gridExtra)
library(gtable)
library(ggOceanMaps)
library(RColorBrewer)
library(sp)
library(sf)
library(pangaear)
library(doParallel); registerDoParallel(cores = 15)

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
bbox_kong <- c(11, 12.69, 78.86, 79.1)
bbox_is <- c(13.62, 17.14, 78.03, 78.71)
bbox_ingle <- c(18.15, 18.79, 77.87, 78.05)
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
  "soc" = "F48080"
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


# Workflowr code ----------------------------------------------------------

# All analysis files
# dir("analysis", pattern = ".Rmd", full.names = T)

# Run this to re-compile the entire project
# system.time(
#   workflowr::wflow_publish(files = c("analysis/index.Rmd",
#                                      # "analysis/socat-glodap.Rmd", # Don't knit this unless necessary, it takes a long time
#                                      "analysis/key_drivers.Rmd",
#                                      "analysis/metadatabase.Rmd",
#                                      "analysis/data_summary.Rmd", # NB: This takes a couple minutes
#                                      "analysis/review.Rmd"
#   ),
#   message = "Re-built site.")
# ) # 311 seconds with the SOCAT analysis, 8 seconds without


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
# lon1=9; lon2=30; lat1=76; lat2=81
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
    mutate(date = as.POSIXct((nc_TIME*86400), origin = "1950-01-01 00:00:00"), .keep = "unused") %>% 
    dplyr::rename(lon = nc_LONGITUDE, lat = nc_LATITUDE) %>% 
    dplyr::select(lon, lat, date, depth, value) %>% 
    `colnames<-`(c("lon", "lat", "date", "depth", tolower(var_id))); gc()
  return(nc_val)
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

# Data summary plotting function
data_summary_plot <- function(full_product, site_name){
  
  # Create factors for more consistent plotting
  full_product$var_type <- as.factor(full_product$var_type)
  
  # get correct bounding box
  if(site_name == "Kongsfjorden") bbox_plot <- bbox_kong
  if(site_name == "Isfjorden") bbox_plot <- bbox_is
  if(site_name == "Inglefieldbukta") bbox_plot <- bbox_ingle
  if(site_name == "Young Sound") bbox_plot <- bbox_young
  if(site_name == "Disko Bay") bbox_plot <- bbox_disko
  if(site_name == "Nuup Kangerlua") bbox_plot <- bbox_nuup
  if(site_name == "Porsangerfjorden") bbox_plot <- bbox_por
  
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
           date = paste0(min(full_product$date, na.rm = T), " to ", max(full_product$date, na.rm = T)),
           depth = paste0(min(full_product$depth, na.rm = T), " to ", max(full_product$depth, na.rm = T))) %>% #,
           # site = site_name) %>%
    dplyr::select(files, lon, lat, date, depth, everything())
  
  # Graphic version
  # meta_table_g <- gtable_add_grob(tableGrob(meta_table, rows = NULL),
  #                                 grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
  #                                 t = 2, b = nrow(meta_table), l = 1, r = ncol(meta_table))
  meta_table_g <- tableGrob(meta_table, rows = NULL)
  
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
  plot_summary <- ggpubr::ggarrange(meta_table_g, plot_summary_bottom, heights = c(0.2, 1), labels = c("A)", ""), ncol = 1)
  return(plot_summary)
}

# Data climatology plotting function
data_clim_plot <- function(full_product, site_name){
  
  # Create factors for more consistent plotting
  full_product$var_type <- as.factor(full_product$var_type)
  
  # get correct bounding box
  if(site_name == "Kongsfjorden") bbox_plot <- bbox_kong
  if(site_name == "Isfjorden") bbox_plot <- bbox_is
  if(site_name == "Inglefieldbukta") bbox_plot <- bbox_ingle
  if(site_name == "Young Sound") bbox_plot <- bbox_young
  if(site_name == "Disko Bay") bbox_plot <- bbox_disko
  if(site_name == "Nuup Kangerlua") bbox_plot <- bbox_nuup
  if(site_name == "Porsangerfjorden") bbox_plot <- bbox_por
  
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
                                 heights = c(0.3, 0.1, 1, 0.3, 0.1, 1), labels = c("A)", "", "B)", "C)", "", "D)"), ncol = 1, nrow = 6)
  return(plot_clim)
}

# Function for calculating and plotting the trends in the data from full products
data_trend_plot <- function(full_product, site_name){
  
  
  # Create factors for more consistent plotting
  full_product$var_type <- as.factor(full_product$var_type)
  
  # get correct bounding box
  if(site_name == "Kongsfjorden") bbox_plot <- bbox_kong
  if(site_name == "Isfjorden") bbox_plot <- bbox_is
  if(site_name == "Inglefieldbukta") bbox_plot <- bbox_ingle
  if(site_name == "Young Sound") bbox_plot <- bbox_young
  if(site_name == "Disko Bay") bbox_plot <- bbox_disko
  if(site_name == "Nuup Kangerlua") bbox_plot <- bbox_nuup
  if(site_name == "Porsangerfjorden") bbox_plot <- bbox_por
  
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
    geom_point(aes(colour = depth_cat), alpha = 0.5) +
    geom_line(aes(group = lonlat), stat = "smooth", method = "lm", formula = y ~ x, linetype = "dashed", alpha = 0.5) +
    geom_point(data = filter(depth_trend, var_cat == "temp"), aes(group = depth_cat), colour = "black", size = 6, shape = 18, alpha = 0.7) +
    geom_point(data = filter(depth_trend, var_cat == "temp"), aes(colour = depth_cat), size = 5, shape = 18, alpha = 0.7) +
    geom_smooth(data = filter(depth_trend, var_cat == "temp"), aes(group = depth_cat), colour = "black", method = "lm", se = F, size = 4, alpha = 0.7) +
    geom_smooth(data = filter(depth_trend, var_cat == "temp"), aes(colour = depth_cat), method = "lm", se = F, size = 3, alpha = 0.7) +
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
    geom_point(aes(colour = depth_cat), alpha = 0.5) +
    geom_line(aes(group = lonlat), stat = "smooth", method = "lm", formula = y ~ x, linetype = "dashed", alpha = 0.5) +
    geom_point(data = filter(depth_trend, var_cat == "sal"), aes(group = depth_cat), colour = "black", size = 6, shape = 18, alpha = 0.7) +
    geom_point(data = filter(depth_trend, var_cat == "sal"), aes(colour = depth_cat), size = 5, shape = 18, alpha = 0.7) +
    geom_smooth(data = filter(depth_trend, var_cat == "sal"), aes(group = depth_cat), colour = "black", method = "lm", se = F, size = 4, alpha = 0.7) +
    geom_smooth(data = filter(depth_trend, var_cat == "sal"), aes(colour = depth_cat), method = "lm", se = F, size = 3, alpha = 0.7) +
    coord_cartesian(expand = F, ylim = range(lims_sal$value)) +
    scale_colour_manual(values = DepthCol) +
    labs(x = NULL, y = "Salinity", colour = "Depth", title = "Salinity trends at depth") +
    theme(panel.border = element_rect(fill = NA, colour = "black"))
  # plot_trend_sal
  
  # Put them together
  plot_trend <- ggpubr::ggarrange(plot_trend_temp + theme(legend.position = "none"), ggpubr::get_legend(plot_trend_temp),  plot_trend_sal + theme(legend.position = "none"), 
                                  ncol = 1, labels = c("A)", "", "B)"), heights = c(1, 0.2, 1))
  return(plot_trend)
}


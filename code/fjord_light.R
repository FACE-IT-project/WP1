# code/fjord_light.R
# Code for exploring the data from Bernard's FjordLight package


# Setup -------------------------------------------------------------------

library(tidyverse)
library(FjordLight) # NB: Installed directly from .tar.gz
library(geosphere)
library(doParallel); doParallel::registerDoParallel(cores = 15)


# Get fjord data ----------------------------------------------------------

# Set name
fjord <- "kong"

# Download
fl_DownloadFjord(fjord, dirdata = "data/PAR")

# Load
fjorddata <- fl_LoadFjord(fjord, dirdata = "data/PAR")
str(fjorddata)

## Extract bathymetry
# all depths (what = "s" ; s for Sea), as raster
bathy_rast <- flget_bathymetry(fjorddata, what = "s", mode = "raster", PLOT = TRUE)
# coastal zone [0-200m] (what = "c" ; c for Coastal), as raster
# as 3 columns data frame (mode = "3col" : longitude, latitude, depth)
coast_df <- flget_bathymetry(fjorddata, what = "c", mode = "3col", PLOT = FALSE) |> 
  dplyr::rename(lon = longitude, lat = latitude)
sea_df <- flget_bathymetry(fjorddata, what = "s", mode = "3col", PLOT = FALSE) |> 
  dplyr::rename(lon = longitude, lat = latitude)


# Get pixel sizes ---------------------------------------------------------

# Calculate surface area for a single pixel
grid_size_one <- function(df, lon_half, lat_half){
  # Distance for longitude
  lon_dist <- distm(c(df$lon-lon_half, df$lat), c(df$lon+lon_half, df$lat), fun = distHaversine)/1000
  # Distance for latitude
  lat_dist <- distm(c(df$lon, df$lat+lat_half), c(df$lon, df$lat-lat_half), fun = distHaversine)/1000
  # Total area
  sq_area <- data.frame(sq_area = lon_dist*lat_dist)
  # Combine and exit
  res <- cbind(df, sq_area)
  return(res)
}

# Calculate the square kilometre surface area of each pixel
# This function assumes a lon lat column on a 0.08333333 degree grid
# NB: requires a 'lon' and 'lat' column on a full even grid
grid_size <- function(df){
  
  # Find average size of pixels
  unique_lon <- arrange(distinct(df[1]), lon) |> 
    mutate(diff = lon - lag(lon, default = first(lon))) |> 
    filter(diff != 0) # Get rid of first value
  unique_lat <- arrange(distinct(df[2]), lat) %>% 
    mutate(diff = lat - lag(lat, default = first(lat))) |> 
    filter(diff != 0)
  
  # Check for grid regularity
  if(length(unique(round(unique_lon$diff, 6))) != 1) stop("Data are not on an even grid")
  if(length(unique(round(unique_lat$diff, 6))) != 1) stop("Data are not on an even grid")
  
  # Get half pixel lengths
  lon_half <- unique(round(unique_lon$diff, 6))/2
  lat_half <- unique(round(unique_lat$diff, 6))/2
  
  # Get all sizes
  res <- plyr::ddply(mutate(df, plyr_idx = 1:n()), c("plyr_idx"), grid_size_one, .parallel = T,
                     lon_half = lon_half, lat_half = lat_half) |> 
    dplyr::select(-plyr_idx)
  return(res)
  # rm(df, unique_lon, unique_lat, lon_half, lat_half, lon_dist, lat_dist, sq_area, res)
}
kong_res <- grid_size(sea_df)

# Merge with coastal sub
## Not necessary, rather just filter by depth
coast_res <- coast_df |> 
  left_join(kong_res)


# Compare surface areas ---------------------------------------------------

# PAR0m and kdpar for year 2012 as 3 columns data frame
P02012 <- flget_optics(fjorddata, "PAR0m", "Yearly", year = 2012, mode = "3col")
PB2012 <- flget_optics(fjorddata, "PARbottom", "Yearly", year = 2012, mode = "3col")
P0global <- flget_optics(fjorddata, "PAR0m", "Global", mode = "3col")
PBglobal <- flget_optics(fjorddata, "PARbottom", "Global", mode = "3col")

# Get surface areas from FjordLight package
# as a function
fG <- flget_Pfunction(fjorddata, "Global", plot = FALSE)
# then you can use it; for instance :
irradiance_levels <- c(0.1, 1, 10)
fG(irradiance_levels)

# As a 2 column data.frame
f2012 <- flget_Pfunction(fjorddata, "Yearly", year = 2012, mode = "2col")
fglobal <- flget_Pfunction(fjorddata, "Global", mode = "2col")

# Plot
flget_Pfunction(fjorddata, "Global", PLOT = TRUE, lty = 1, col = 1, lwd = 2, Main = paste(fjord, "P-functions"), ylim = c(0, 50))

# Manual calculation
kong_surf_total <- sum(kong_res$sq_area)
kong_surf_sea <- kong_res |> 
  filter(depth <= 0) |> 
  summarise(sq_area = sum(sq_area, na.rm = T))
kong_surf_coast <- kong_res |> 
  filter(depth >= -200, depth <= 0) |> 
  summarise(sq_area = sum(sq_area, na.rm = T))

# Manual proportions of surface > 10 PAR
kong_global_coast_PAR10 <- P0global |> 
  dplyr::rename(lon = longitude, lat = latitude) |> 
  left_join(kong_res) |> 
  filter(depth >= -200, depth <= 0) |> 
  filter(PAR0m_Global >= 10) |> 
  summarise(sq_area = sum(sq_area, na.rm = T))
kong_global_coast_PAR10/kong_surf_coast

# Manual proportions of surface > 0.1 PAR
kong_global_coast_PAR0.1 <- P0global |> 
  dplyr::rename(lon = longitude, lat = latitude) |> 
  left_join(kong_res) |> 
  filter(depth >= -200, depth <= 0) |> 
  filter(PAR0m_Global >= 0.1) |> 
  summarise(sq_area = sum(sq_area, na.rm = T))
kong_global_coast_PAR0.1/kong_surf_coast


# Get pixels in regions ---------------------------------------------------

# Combine into single file and save
Arctic_AM <- left_join(Arctic_land_distance, Arctic_depth, by = c("lon", "lat")) %>% 
  dplyr::select(lon, lat, everything()) %>% 
  mutate(in_grid = sp::point.in.polygon(point.x = Arctic_land_distance[["lon"]], point.y = Arctic_land_distance[["lat"]], 
                                        pol.x = Arctic_boundary[["lon"]], pol.y = Arctic_boundary[["lat"]])) %>% 
  filter(in_grid >= 1) %>% 
  dplyr::select(-in_grid)

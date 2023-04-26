# Code/species_names.R
# script to test function before adding them to the formulas script

# Set up ------------------------------------------------------------------

# source('code/functions.R')
library(tidyverse)


# Function ----------------------------------------------------------------











































# UTM function ------------------------------------------------------------
# New best practice for converting UTM projected points to an even lon/lat grid
# NB: Expects three columns; x, y, and a data column
convert_UTM_deg_grid <- function(df, proj_base, third_col = NULL){
  if(!is.null(third_col)) df <- df[,c("x", "y", third_col)]
  df_utm <- st_as_sf(df, coords = c("x", "y"), crs = proj_base)
  df_rast <- st_rasterize(df_utm)
  df_raster <- as(df_rast, "Raster")
  df_raster_deg <- projectRaster(df_raster, crs = 4326)
  df_df <- as.data.frame(df_raster_deg, xy = TRUE) |> 
    dplyr::rename(lon = x, lat = y) |> 
    filter(!is.na(layer))
  return(df_df)
}


# UTM ---------------------------------------------------------------------

# Function for converting UTM to decimal degrees
# NB: Expects an 'Easting' and 'Northing' column
convert_UTM_deg <- function(df, utm_zone){
  
  # Fill NA with 0 for ease of use
  df$Easting <- replace_na(df$Easting, 0)
  df$Northing <- replace_na(df$Northing, 0)
  
  # Convert coordinates to lon/lat
  suppressWarnings(
    utmcoor <- sp::SpatialPoints(cbind(df$Easting, df$Northing), 
                                 proj4string = sp::CRS(paste0("+proj=utm +zone=",utm_zone)))
  )
  suppressWarnings(
  longlatcoor <- sp::spTransform(utmcoor, sp::CRS("+proj=longlat"))
  )
  
  # Attach to data.frame and replace 0 with NA
  df$lon <- sp::coordinates(longlatcoor)[,1]
  df$lon[df$Easting == 0] <- NA
  df$Easting[df$Easting == 0] <- NA
  df$lat <- sp::coordinates(longlatcoor)[,2]
  df$lat[df$Northing == 0] <- NA
  df$Northing[df$Northing == 0] <- NA
  return(df)
}



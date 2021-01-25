# code/visualising.R
# This script houses code used to visualise raw and processed data


# Setup -------------------------------------------------------------------

# Start with common project code
source("code/functions.R")

# Additional libraries
library(tidync)
library(ncdf4)
library(ncdump)
library(ggOceanMaps)


# TCO2 data ---------------------------------------------------------------

# Load data
Arctic_carb_chem <- read_csv(paste0(pCloud_path,"FACE-IT_data/GLODAP_Arctic_carb_chem.csv"))

# The count per grid cell/station
Arctic_carb_chem_count <- Arctic_carb_chem %>% 
  group_by(lon, lat) %>% 
  summarise(count = n(),
            layer = ifelse(max(depth) > 200, "grid", "bottle"), .groups = "drop")

# Using ggOceanMaps
basemap(limits = 60) +
  geom_point(data = transform_coord(Arctic_carb_chem_count), aes(x = lon, y = lat))

# using base ggplot2
Arctic_carb_chem_count %>%
  # filter(layer == "bottle") %>% 
  filter(layer == "grid") %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(data = map_base, aes(group = group), colour = "black", fill = "grey80") +
  geom_point(aes(colour = count)) +
  scale_colour_distiller(palette = "Greens", direction = 1) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  # coord_quickmap(ylim = c(60, 90), expand = F) +
  coord_map("ortho", orientation = c(90, 0, 0), ylim = c(60, 90)) +
  labs(x = NULL, y = NULL, title = "Count per grid cell from 200 m to 5500 m depth")

# Mean value per grid/station


# Bathymetry --------------------------------------------------------------

# As a first trial run for visualising bathy data we will use a high-res file for Young Sound
bathy_YS <- tidync(paste0(pCloud_path,"FACE-IT_data/shape_files/ys_bathy_v3.0_raw.nc")) %>% 
  hyper_tibble() %>% 
  filter(Landmask == 0)

# Set limits for bathy projection
xlon <- range(ncvar_get(nc_open(paste0(pCloud_path,"FACE-IT_data/shape_files/ys_bathy_v3.0_raw.nc")), varid = 'Longitude'))
xlat <- range(ncvar_get(nc_open(paste0(pCloud_path,"FACE-IT_data/shape_files/ys_bathy_v3.0_raw.nc")), varid = 'Latitude'))
lims <- c(xlon, xlat) # These are rough rounded values
projection <- "+init=epsg:32636"

# Check the limits
basemap(limits = lims)

# Convert NetCDF to raster
rb <- raster_bathymetry(bathy = paste0(pCloud_path,"FACE-IT_data/shape_files/ys_bathy_v3.0_raw.nc"),
                        depths = c(0, 10, 25, 50, 100, 200, 300, 500, 1000, 2000, 10000), 
                        proj.out = projection, 
                        boundary = lims
)

# Check raster output
class(rb)
names(rb)
raster::plot(rb$raster)

# Convert to raster vector for plotting
bs_bathy <- vector_bathymetry(rb)
sp::plot(bs_bathy)

# Convert land file for use with new bathy file
world <- rgdal::readOGR(paste0(pCloud_path,"FACE-IT_data/shape_files/ne_10m_land.shp"))
islands <- rgdal::readOGR(paste0(pCloud_path,"FACE-IT_data/shape_files/ne_10m_minor_islands.shp"))
world <- rbind(world, islands)
bs_land <- clip_shapefile(world, lims)
bs_land <- sp::spTransform(bs_land, CRSobj = sp::CRS(projection))
rgeos::gIsValid(bs_land) # Has to return TRUE, if not use rgeos::gBuffer
# bs_land <- rgeos::gBuffer(bs_land, byid = TRUE, width = 0)
sp::plot(bs_land)

# Create glacier shape files
glaciers <- rgdal::readOGR(paste0(pCloud_path,"FACE-IT_data/shape_files/ne_10m_glaciated_areas.shp"))
rgeos::gIsValid(glaciers) # Needs buffering
glaciers <- rgeos::gBuffer(glaciers, byid = TRUE, width = 0)
bs_glacier <- clip_shapefile(glaciers, lims)
bs_glacier <- sp::spTransform(bs_glacier, CRSobj = sp::CRS(projection))
rgeos::gIsValid(bs_glacier)
sp::plot(bs_glacier)

# Plot on ggOceanMaps
basemap(shapefiles = list(land = bs_land, glacier = bs_glacier, bathy = bs_bathy), bathymetry = TRUE, glaciers = TRUE) +
  scale_fill_viridis_d("Depth (m)")


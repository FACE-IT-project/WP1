# code/metadata.R
# The code used for the creation/extraction of meta-data for the project


# Setup -------------------------------------------------------------------

# Need to start phasing out rgdal and rgeos
# options("sp_evolution_status" = 2)

# Libraries used in all other scripts
library(tidyverse)
library(ncdf4)
library(tidync)
library(PCICt) # For 'noleap' date conversions
library(FNN)
library(geosphere)
library(grid)
library(gridExtra)
library(gtable)
library(ggOceanMaps)
library(ggraph) # Necessary to load here as one function is manually overwritten below
library(RColorBrewer)
library(raster)
library(rgdal)
# library(sp) # This needs to be phased out...
library(sf)
library(circular) # For calculating mean daily wind direction from degree values
library(pangaear)
library(worrms)
library(arrow)
library(doParallel); registerDoParallel(cores = 12)

# Find who is the user and define the pCloud path
if(Sys.getenv("LOGNAME") == "gattuso"){
  pCloud_path = "~/pCloud\ Drive/"
} else {
  pCloud_path = "~/pCloudDrive/" 
}

# Remove scientific notation
options(scipen = 9999)

# Set Timezone to UTC
Sys.setenv(TZ = "UTC")

# Set system time to English
Sys.setlocale("LC_TIME", "en_GB.UTF-8")


# Meta-data ---------------------------------------------------------------

# Full category -> driver -> variable list
# NB: This was first generated a posteriori from v1.0 of the data product
# It was then updated vor v1.4 in the 'Driver conversion' section of 'code/data_product.R'
full_var_list <- read_csv("metadata/full_var_list.csv")

# manually new variables to list
# full_var_list <- rbind(full_var_list,
#                        data.frame(category = c("chem"),
#                                   driver = c("carb"),
#                                   variable = c("DIC [µmol kg-1]"))) |>
#   distinct() |> arrange(category, driver, variable)
# write_csv(full_var_list, "metadata/full_var_list.csv")

# Add variables via an entire dataset
# full_var_list <- rbind(full_var_list,
#                        dplyr::select(mutate(filter(sval_AIS, variable != "PAR [µmol m-2 s-1]"), driver = "fisheries"),
#                                      category, driver, variable)) |> 
#   distinct() |> arrange(category, driver, variable)
# write_csv(full_var_list, "metadata/full_var_list.csv")

# Check for duplicates
# summarise(full_var_list, count = n(), .by = "variable") |> filter(count > 1)

# Remove a specific variables
# full_var_list <- filter(full_var_list, variable != "Reinhardtius hippoglossoides [presence]")
# write_csv(full_var_list, "metadata/full_var_list.csv")

# Site list for province etc. conversions
full_site_list <- read_csv("metadata/full_site_list.csv")

# Manually add new sites
full_site_list <- rbind(full_site_list,
                       data.frame(site = "green",
                                  site_long = "Greenland",
                                  site_alt = c("Westgreenland", "Eastgreenland"))) |>
  distinct() |> arrange(site, site_long, site_alt)
write_csv(full_site_list, "metadata/full_site_list.csv")


# Base maps ---------------------------------------------------------------

# The base global map
if(!exists("map_base")) map_base <- readRDS("metadata/map_base.Rda")

# Hi-res coastlines
# if(!exists("coastline_full")) coastline_full <- read_sf("~/pCloudDrive/FACE-IT_data/maps/GSHHG/GSHHS_shp/f/GSHHS_f_L1.shp")
# if(!exists("coastline_full_df")) coastline_full_df <- sfheaders::sf_to_df(coastline_full, fill = TRUE)
# save(coastline_full_df, file = "metadata/coastline_full_df.RData")
# NB: This is large, so is only loaded on command

# Full map
# ggplot(data = coastline_full) + geom_sf()


# Bounding boxes ----------------------------------------------------------

bbox_EU <- c(-60, 60, 60, 90) # Note that this is intentionally different from the Copernicus definition of c(-25, 60, 66, 90)
bbox_sval <- c(9, 30, 76, 81)
bbox_kong <- c(11, 12.69, 78.86, 79.1)
bbox_kong_wide <- c(9.5, 14.0, 78.0, 79.5)
bbox_is <- c(12.97, 17.50, 77.95, 78.90)
bbox_is_wide <- c(10.0, 18.0, 77.0, 79.0)
bbox_ingle <- c(18.15, 18.79, 77.87, 78.08)
bbox_stor <- c(17.35, 21.60, 77.33, 78.13)
bbox_stor_wide <- c(17.0, 22.0, 77.0, 78.5)
bbox_young <- c(-22.367917, -19.907644, 74.210137, 74.624304)
bbox_young_wide <- c(-22.5, -17.5, 73.0, 75.5)
bbox_disko <- c(-55.56, -49.55, 68.22, 70.5)
bbox_disko_wide <- c(-56.0, -49.0, 68.0, 71.0)
bbox_nuup <- c(-53.32, -48.93, 64.01, 64.8)
bbox_nuup_wide <- c(-53.5, -48.5, 63.5, 65.0)
bbox_por <- c(24.5, 27, 70, 71.2)
bbox_por_wide <- c(23.5, 28, 69, 72.0)
bbox_trom <- c(17.5, 21.0, 69.0, 70.5)

# Single site bbox for use with points_in_region()
bbox_kong_df <- data.frame(region = "kong", lon = bbox_kong[1:2], lat = bbox_kong[3:4])
bbox_is_df <- data.frame(region = "is", lon = bbox_is[1:2], lat = bbox_is[3:4])
bbox_stor_df <- data.frame(region = "stor", lon = bbox_stor[1:2], lat = bbox_stor[3:4])
bbox_young_df <- data.frame(region = "young", lon = bbox_young[1:2], lat = bbox_young[3:4])
bbox_disko_df <- data.frame(region = "disko", lon = bbox_disko[1:2], lat = bbox_disko[3:4])
bbox_nuup_df <- data.frame(region = "nuup", lon = bbox_nuup[1:2], lat = bbox_nuup[3:4])
bbox_por_df <- data.frame(region = "por", lon = bbox_por[1:2], lat = bbox_por[3:4])
bbox_ALL_df <- rbind(bbox_kong_df, bbox_is_df, bbox_stor_df,
                     bbox_young_df, bbox_disko_df, bbox_nuup_df, bbox_por_df)

# Colour palettes ---------------------------------------------------------

# Project wide category colours
CatCol <- c(
  "Cryosphere" = "mintcream",
  "Physical" = "skyblue",
  "Chemistry" = "#F6EA7C",
  "Biology" = "#A2ED84",
  "Social" = "#F48080"
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

# Ice cover colours
ice_cover_colours <- c(
  "ocean" = "navy",
  "land" = "slategrey",
  "sea ice" = "mintcream",
  "coast" = "dodgerblue",
  "exclude" = "gold"
)

# Colour palette for sites
site_colours <- c(
  "Kongsfjorden" = "chocolate3",# "chocolate4", 
  "Isfjorden" = "goldenrod",# "chocolate3", 
  "Storfjorden" = "burlywood3",# "chocolate1", 
  "Young Sound" = "chartreuse3",# "springgreen4", 
  "Qeqertarsuup Tunua" = "springgreen3", 
  "Nuup Kangerlua" = "palegreen1",# "springgreen1", 
  "Porsangerfjorden" = "plum3"# "plum4"
)


# Long names --------------------------------------------------------------

# Long names for merging
factor_unique <- function(x) factor(x, levels = unique(x)) # Hack to assign levels easier
long_site_names <- data.frame(site = c("kong", "is", "stor", "young", "disko", "nuup", "por"),
                              site_long = factor_unique(c("Kongsfjorden", "Isfjorden", "Storfjorden",
                                                          "Young Sound", "Qeqertarsuup Tunua", "Nuup Kangerlua",
                                                          "Porsangerfjorden")))
long_cat_names <- data.frame(category = c("cryo", "phys", "chem", "bio", "soc"),
                             category_long = factor_unique(x = c("cryosphere", "physics", 
                                                                 "chemistry", "biology", "social")))
long_driver_names <- data.frame(driver = c("sea ice", "glacier", "runoff",
                                           "sea temp", "salinity", "light",
                                           "carb", "nutrients", 
                                           "prim prod", "biomass", "spp rich", 
                                           "gov", "tourism", "fisheries"),
                                driver_long = factor_unique(x = c("sea ice", "glacier mass balance", "terrestrial runoff",
                                                                  "seawater temperature", "salinity", "light",
                                                                  "carbonate chemistry", "nutrients",
                                                                  "primary production", "biomass", "species richness",
                                                                  "governance", "tourism", "fisheries")))

# For finding meta-data in PG files
lon_names <- c("LONGITUDE", "Longitude", "longitude", "long", "lon")
lat_names <- c("LATITUDE", "Latitude", "latitude", "lat")

# Nuup site coords
nuup_bird_coords <- data.frame(Point = LETTERS[1:13],
                               lon = c(64.134685, 64.135155, 64.134592, 64.13239, 64.131052,
                                       64.129385, 64.131761, 64.132669, 64.134509, 64.135639,
                                       64.133636, 64.132841, 64.131031),
                               lat = c(-51.385105, -51.391187, -51.396234, -51.39359, -51.38916,
                                       -51.37833, -51.379398, -51.374116, -51.363874, -51.355553,
                                       -51.344558, -51.336278, -51.326204))


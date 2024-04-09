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
library(sf)
library(circular) # For calculating mean daily wind direction from degree values
library(pangaear)
library(worrms)
library(arrow)
library(seacarb)
library(taxizedb)
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

# Download NCBI database for species names
# NB: Doesn't need to be run often
# db_download_ncbi(verbose = TRUE, overwrite = FALSE)
# db_download_itis(verbose = TRUE, overwrite = FALSE)
# db_download_tpl(verbose = TRUE, overwrite = FALSE)
# db_download_wfo(verbose = TRUE, overwrite = FALSE)
# db_download_col(verbose = TRUE, overwrite = FALSE)
# db_download_gbif(verbose = TRUE, overwrite = FALSE)
# db_download_wikidata(verbose = TRUE, overwrite = FALSE)


# Sites --------------------------------------------------------

## Relevant sites --------------------------------------------------------

## Norway
# Troms og Finnmark: Province(s) for Porsangerfjorden
# Lakselv: Main city for Porsangerfjorden (?)
# Lakselv Banak + Honningsvåg Valan: Airports on Porsangerfjorden 

## Svalbard
# Svalbard: Province for Svalbard
# east ice and west ice: Svalbard survey regions
# Longyearbyen: Main city in Isfjorden
# Isfjorden sites:
# Longyearbyen & Ny-Alesund mainland, Longyearbyen & Ny-Alesund abroad, Barentsburg and Pyramiden
# Svalbard Longyear: Airport on Isfjorden
# Ny-Alesund: Main village in Kongsfjorden

## Greenland
# Sermersooq: Municipality for Nuup Kangerlua
# Nuuk: Main city in Nuup Kangerlua, also an airport
# Qeqertalik: Municipality for Disko bay
# Avannaata: Municipality that borders on Disko Bay (relevant for demographics, fish landings, etc.)
# Qeqertarsuaq: Main city in Disko Bay (?)
# Aasiaat: Port on southern edge of Disko Bay
# Ilulissat: Port on eastern edge of Disko Bay, also an airport
# Qasigiannguit: Port on eastern edge of Disko Bay
# Uummannaq: City North of Disko Bay (possibly relevant for fish landings etc.)
# Kangaatsiaq: Port south of Disko Bay (possibly relevant for fish landings etc.)
# Outside municipalities: Young Sound appears to fall outside of a municipality


## Link sites --------------------------------------------------------------

# The list of sites created to help with linking
# full_site_list <- dplyr::select(full_ALL, site) |> distinct() |>
#   dplyr::rename(site_alt = site) |> arrange(site_alt) |> 
#   mutate(site = case_when(site_alt %in% long_site_names$site ~ site_alt, TRUE ~ NA),
#          site = case_when(site_alt == "EU" ~ "EU",
#                           site_alt %in% c("Svalbard", "svalbard", "sval", "east ice", "west ice", "SvalbardTransit") ~ "sval",
#                           site_alt %in% c("Norway", "Barents Sea", "Barents sea", "barents sea", "nor", "NorwegianWaters") ~ "nor",
#                           site_alt %in% c("Grønlund", "green",
#                                           # "West- Eastgreenland", "Westgreenland",
#                                           # "east ice", "west ice",
#                                           "All Greenland") ~ "green",
#                           site_alt %in% c("Kongsfjorden", "Ny-Alesund") ~ "kong",
#                           site_alt %in% c("Longyearbyen & Ny-Alesund", "Longyearbyen & Ny-Alesund mainland",
#                                           "Longyearbyen & Ny-Alesund abroad", "Barentsburg and Pyramiden") ~ "is", # NB: This is an intentional choice
#                           site_alt %in% c("Svalbard Longyear", "Isfjorden") ~ "is",
#                           site_alt %in% c("Storfjorden") ~ "stor",
#                           site_alt %in% c("Lakselv", "Lakselv Banak", "Honningsvåg Valan", "Honningsvåg",
#                                           "Troms og Finnmark - Romsa ja Finnmárku") ~ "por",
#                           site_alt %in% c("Nuuk", "Kommuneqarfik Sermersooq",
#                                           # "Kommuneqarfik Sermersooq Øst", # NB: Intentionally not choosing this one
#                                           "Kommuneqarfik Sermersooq Vest") ~ "nuup",
#                           site_alt %in% c("Qeqertarsuaq", "Avannaata Kommunia", 
#                                           "Avannaata Kommunia and Kommune Qeqertalik", 
#                                           "Kommune Qeqertalik", "Aasiaat",
#                                           "Ilulissat", "Ilulissat (*)", "Qasigiannguit",
#                                           "Uummannaq", "Kangaatsiaq", "Disko Bay") ~ "disko",
#                           site_alt %in% c("Outside municipalities") ~ "young",
#                           TRUE ~ site)) |> 
#   left_join(long_site_names, by = "site") |> 
#   mutate(site_long = case_when(site == "EU" ~ "EU", site == "green" ~ "Greenland",
#                                site == "nor" ~ "Norway", site == "sval" ~ "Svalbard", TRUE ~ site_long)) |> 
#   dplyr::select(site, site_long, site_alt)
# write_csv(full_site_list, "metadata/full_site_list.csv")


## Add new sites ---------------------------------------------------------

# write_csv(full_var_list, "metadata/full_var_list.csv")

# Site list for province etc. conversions
full_site_list <- read_csv("metadata/full_site_list.csv")

# Manually add new sites
# full_site_list <- rbind(full_site_list,
#                        data.frame(site = "green",
#                                   site_long = "Greenland",
#                                   site_alt = c("Westgreenland", "Eastgreenland", "West- Eastgreenland"))) |>
#   distinct() |> arrange(site, site_long, site_alt)
# write_csv(full_site_list, "metadata/full_site_list.csv")


## Remove sites ------------------------------------------------------------

# Manually remove sites
# full_site_list <- full_site_list |>
#   # mutate(site_alt = case_when(is.na(site_long) &
#   #                               grepl("Westgreenland|Eastgreenland|West- Eastgreenland", site_alt) ~ as.character(NA),
#   #                             TRUE ~ site_alt)) |>
#   mutate(site_alt = case_when(site == "stor" & site_long == "Isfjorden" ~ as.character(NA), TRUE ~ site_alt)) |> 
#   filter(!is.na(site_alt))
# write_csv(full_site_list, "metadata/full_site_list.csv")

# Check for duplicates
# site_dup <- full_site_list |> 
#   summarise(count = n(), .by = "site_alt")


# Variables ---------------------------------------------------------------

# Full category -> driver -> variable list
# NB: This was first generated a posteriori from v1.0 of the data product
# It was then updated vor v1.4 in the 'Driver conversion' section of 'code/data_product.R'
full_var_list <- read_csv("metadata/full_var_list.csv")


## Manually add variables -------------------------------------------------

# full_var_list <- rbind(full_var_list,
#                        data.frame(category = c("chem"),
#                                   driver = c("nutrients"),
#                                   variable = c("SiO4 [µmol kg-1]", "PO4 [µmol kg-1]"))) |>
#   distinct() |> arrange(category, driver, variable)
# write_csv(full_var_list, "metadata/full_var_list.csv")

# Check that no obvious errors were committed
# length(unique(full_var_list$category)) # Should be 5
# length(unique(full_var_list$driver)) # Should be 14


## Add variables from object ----------------------------------------------

# Add variables via an entire dataset
# NB: Change as necessary
# part_var_list <- pg_kong_bio |> dplyr::select(category, variable) |> distinct() |>
#   # filter(grepl("Cruise passengers|Quota|Trawlers", variable)) |>
#   # mutate(driver = case_when(grepl("\\[presence|\\[present", variable) ~ "spp rich", TRUE ~ "biomass")) |>
#   # mutate(driver = case_when(grepl("\\[prop", variable) ~ "spp rich", TRUE ~ "biomass")) |>
#   # mutate(driver = case_when(grepl("Cruise passengers", variable) ~ "tourism", TRUE ~ "fisheries")) |>
#   mutate(driver = "biomass") |>
#   dplyr::select(category, driver, variable)
# full_var_list <- rbind(full_var_list, part_var_list) |>
#   distinct() |> arrange(category, driver, variable)
# write_csv(full_var_list, "metadata/full_var_list.csv")

# Check for duplicates
# dup_var <- summarise(full_var_list, count = n(), .by = "variable") |> filter(count > 1)
# check_var <- filter(full_var_list, variable %in% dup_var$variable)


## Correct variables ------------------------------------------------------

# full_var_list <- full_var_list |>
#   # mutate(variable = str_replace(variable, " -total [n]", " - total [n]"))
#   # mutate(category = case_when(variable == "Fv/Fm" ~ "bio", TRUE ~ category),
#   #        driver = case_when(variable == "Fv/Fm" ~ "prim prod", TRUE ~ driver)) |> 
#   mutate(driver = case_when(grepl("C/Chl a", variable) ~ "prim prod", TRUE ~ driver)) |> 
#   distinct()
# write_csv(full_var_list, "metadata/full_var_list.csv")


## Remove a specific variables --------------------------------------------

# full_var_list <- filter(full_var_list, !grepl("-total", variable))
# write_csv(full_var_list, "metadata/full_var_list.csv")


## Remove many variables --------------------------------------------------

# full_var_list <- filter(full_var_list, !grepl(paste0(LETTERS,". ", collapse = "|"), variable))
# write_csv(full_var_list, "metadata/full_var_list.csv")


## Fix cat or driver ------------------------------------------------------

# full_var_list <- filter(full_var_list, !grepl("carb chem", driver))
# write_csv(full_var_list, "metadata/full_var_list.csv")


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
long_names <- data.frame(category = c("cryo", "cryo", "cryo", "phys", "phys", "phys",
                                      "chem", "chem", "bio", "bio", "bio", "soc", "soc", "soc"),
                         category_long = c("cryosphere", "cryosphere", "cryosphere", "physics", "physics", "physics",
                                           "chemistry", "chemistry", "biology", "biology", "biology",
                                           "social", "social", "social"),
                         driver = c("sea ice", "glacier", "runoff", "sea temp", "salinity", "light",
                                    "carb", "nutrients", "prim prod", "biomass", "spp rich", 
                                    "gov", "tourism", "fisheries"),
                         driver_long = c("sea ice", "glacier mass balance", "terrestrial runoff",
                                         "seawater temperature", "salinity", "light",
                                         "carbonate chemistry", "nutrients",
                                         "primary production", "biomass", "species richness",
                                         "governance", "tourism", "fisheries")) %>% 
  mutate(category_long = factor(category_long,
                                levels = c("cryosphere", "physics", "chemistry", "biology", "social")),
         driver_long = factor(driver_long, 
                              levels = c("sea ice", "glacier mass balance", "terrestrial runoff",
                                         "seawater temperature", "salinity", "light",
                                         "carbonate chemistry", "nutrients",
                                         "primary production", "biomass", "species richness",
                                         "governance",  "tourism", "fisheries")))

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


# PANGAEA processing times ------------------------------------------------

# # 1
# as.Date("2022-06-14")-as.Date("2022-03-25")-1
# # 2
# as.Date("2023-02-08")-as.Date("2022-10-28")-6
# # 3
# as.Date("2023-01-03")-as.Date("2022-11-17")-3
# # 4
# as.Date("2023-03-17")-as.Date("2023-02-09")-12
# # 5
# as.Date("2023-03-20")-as.Date("2023-03-01")-0
# # 6
# as.Date("2023-04-19")-as.Date("2023-03-03")-8
# # 7
# as.Date("2023-08-31")-as.Date("2023-05-04")-1
# # 8
# as.Date("2023-06-29")-as.Date("2023-06-19")-0
# # 9
# as.Date("2023-12-07")-as.Date("2023-08-04")-48
# # 10
# as.Date("2024-01-02")-as.Date("2023-08-07")-0 # NB: Ongoing
# # 11
# as.Date("2023-11-08")-as.Date("2023-08-07")-31
# # 12
# as.Date("2023-10-18")-as.Date("2023-08-07")-15
# # 13
# as.Date("2023-11-27")-as.Date("2023-10-16")-9
# # 14
# as.Date("2023-11-27")-as.Date("2023-10-16")-9 # NB: These are not duplicates
# # 15
# as.Date("2023-12-05")-as.Date("2023-10-16")-14
# # 16
# as.Date("2023-11-27")-as.Date("2023-10-16")-7
# # 17
# as.Date("2024-01-02")-as.Date("2023-10-31")-0 # NB: Ongoing
# # 18
# as.Date("2024-01-02")-as.Date("2023-10-31")-0 # NB: Ongoing
# # 19
# as.Date("2024-01-02")-as.Date("2023-10-31")-0 # NB: Ongoing
# # 20
# as.Date("2024-01-02")-as.Date("2023-10-31")-0 # NB: Ongoing
# # 21
# as.Date("2024-01-02")-as.Date("2023-11-14")-0 # NB: Ongoing
# 
# # My delays
# time_me <- c(1, 6, 3, 12, 0, 8, 1, 0, 48, 0, 31, 15, 9, 9, 14, 7, 0, 0, 0, 0, 0)
# min(time_me); median(time_me); mean(time_me); max(time_me)
# 
# # Processing times
# time_PG <- c(80, 97, 44, 24, 19, 39, 118, 10, 77, 148, 62, 57, 33, 33, 36, 35, 63, 63, 63, 63, 49)
# min(time_PG); median(time_PG); mean(time_PG); max(time_PG)


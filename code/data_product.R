# code/data_product.R
# This script houses the code used to create data products from the many disparate files


# Setup -------------------------------------------------------------------

# Libraries
source("code/functions.R")
library(tidync)
library(stringi)
library(raster)

# Remove scientific notation
options(scipen = 9999)

# Set Timezone to UTC
Sys.setenv(TZ = "UTC")

# Re-run full data collection pipeline
# system.time(
# source("code/data_collection.R")
# )

# PANGAEA files
pg_files <- dir("data/pg_data/", pattern = "pg_", full.names = T)

# Bounding boxes
bbox_EU <- c(-60, 60, 63, 90)
bbox_kong <- c(11, 12.69, 78.86, 79.1)
bbox_is <- c(13.62, 17.14, 78.03, 78.71)
bbox_ingle <- c(18.15, 18.79, 77.87, 78.05)
bbox_young <- c(-22.367917, -19.907644, 74.210137, 74.624304)
bbox_disko <- c(-55.56, -49.55, 68.22, 70.5)
bbox_nuup <- c(-53.32, -48.93, 64.01, 64.8)
bbox_por <- c(24.5, 27, 70, 71.2)

# Quick filtering function
# Manual tweaks will still be required after running this
pg_quick_filter <- function(file_name, bbox){
  pg_dat <- data.table::fread(file_name)
  if("Longitude" %in% colnames(pg_dat)){
    pg_res <- pg_dat %>% 
      dplyr::rename(lon = Longitude, lat = Latitude) %>% 
      filter(lon >= bbox[1], lon <= bbox[2],
             lat >= bbox[3], lat <= bbox[4]) %>% 
      janitor::remove_empty("cols")
  } else{
    pg_res <- NULL
  }
  rm(pg_dat); gc()
  return(pg_res)
}

# Function for melting columns related to a specific driver
pg_var_melt <- function(pg_clean, key_words, var_word){
  # Message of which columns were melted
  sub_cols <- colnames(pg_clean)[colnames(pg_clean) %in% unique(key_words)]
  sub_cols <- sub_cols[!sub_cols %in% c("date_accessed", "URL", "citation", "lon", "lat", "date", "depth")]
  print(sub_cols)
  if(length(sub_cols) == 0) stop("No variables for this driver group") 
  
  # Subset and melt data.frame
  pg_melt <- pg_clean %>% 
    dplyr::select("date_accessed", "URL", "citation", "lon", "lat", "date", "depth", all_of(sub_cols)) %>% 
    pivot_longer(cols = all_of(sub_cols), names_to = paste0("var_name"), values_to = "value") %>% 
    mutate(var_type = var_word) %>% 
    filter(!is.na(value)) %>%
    distinct() %>% 
    dplyr::select(date_accessed:depth, var_type, var_name, value)
}

# Function for melting individual files from other data sources
single_file_var_melt <- function(){
  
}


# European Arctic ---------------------------------------------------------

## PG product --------------------------------------------------------------

# There is no EU PANGAEA product because the bits and pieces were given to the individual PG site files 

## Full product ------------------------------------------------------------

# No products are currently planned to be made for the EU Arctic
# Rather access the existing files directly: ~/pCloud/EU_Arctic/...
# Though now thinking about it perhaps it would be better to create a combined product
# from which the site specific products may draw data

# Bathymetry data
# EU_GEBCO # Not on pCloud

# Zooplankton biodiversity
# 1995-2008-zooplankton-biodiversity.tsv
EU_zooplankton

# Ice modelling output
# EU_ice # Not on pCloud

# CTD data from YMER cruise in 1980
# 77YM19800811.exc.csv
EU_YMER

# CTD data for Arctic
# Codispoti_Arctic_Nutrients_Submission_11-11-2010.csv
EU_Codispoti

# Ice core samples for protist presence
# protists\
EU_protists

# CTD data from NCEI Accession 9700302
# 9700302.2.2.tar.gz # This appears to be some sort of proprietary data format...
EU_NCEI_1989

# GRDC river discharge data
# grdc_arctichycos_stations.xlsx
EU_GRDC

# Combine and save
full_product_EU <- rbind()
data.table::fwrite(full_product_EU, "~/pCloudDrive/FACE-IT_data/EU_arctic/full_product_EU.csv")
save(full_product_EU, file = "~/pCloudDrive/FACE-IT_data/EU_arctic/full_product_EU.RData")
rm(list = grep("EU_",names(.GlobalEnv),value = TRUE)); gc()


# Svalbard ----------------------------------------------------------------

## PG product --------------------------------------------------------------

# There is no PG product for Svalbard
# Rather, all PG products are loaded for each site to get any shared data points


## Full product ------------------------------------------------------------

# The intention here is to create a product from which data for other sites may be accessed
## Glacial topography + thickness
# cumulative-mass-balance-for-glaciers-in-svalbard.csv; 
# austre-broggerbreen-mass-balance.csv; 
# etonbreen-austfonna-mass-balance.csv; 
# kongsvegen-mass-balance.csv; 
# kronebreenholtedahlfonna-mass-balance.csv; 
# midtre-lovenbreen-mass-balance.csv
sval_glacier_mass

# Glacier area outlines
# glacier_area/
# sval_glacier_area # Not working with shape files of geomorphology

# Tidal glacier fronts
# tidewater/
# sval_tidal_glacier_front # Not working with shape files of geomorphology

# Marine terminating glacier fronts
# glacier_fronts/
# sval_marine_glacier_front # Not working with shape files of geomorphology

# Surface meteorology
# N-ICE_metData_v2.nc; 
# N-ICE_metData_QC.py;
# README_N-ICE_metData_v2.txt
sval_surface_met

# UNIS database
## NB: This is really slow due to file size
# ncdf4::nc_open("~/pCloudDrive/FACE-IT_data/svalbard/CTD_all_1876-2019.nc")
sval_UNIS_nc_dat <- ncdf4::nc_open("~/pCloudDrive/FACE-IT_data/svalbard/CTD_all_1876-2019.nc")
sval_UNIS_TEMP <- CTD_to_long(sval_UNIS_nc_dat, "TEMP"); gc()
sval_UNIS_PSAL <- CTD_to_long(sval_UNIS_nc_dat, "PSAL"); gc()
sval_UNIS_CNDC <- CTD_to_long(sval_UNIS_nc_dat, "CNDC"); gc()
sval_UNIS_database <- full_join(sval_UNIS_TEMP, sval_UNIS_PSAL, by = c("lon", "lat", "date", "depth")) %>% 
  full_join(sval_UNIS_CNDC, by = c("lon", "lat", "date", "depth")) %>% 
  pivot_longer(temp:cndc, names_to = "var_name", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(URL = "https://data.npolar.no/dataset/39d9f0f9-af12-420c-a879-10990df2e22d",
         citation = "Skogseth, R., Ellingsen, P., Berge, J., Cottier, F., Falk-Petersen, S., Ivanov, B., … Vader, A. (2019). UNIS hydrographic database [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/unis-hydrography",
         units = case_when(var_name == "temp" ~ "°C",
                           var_name == "psal" ~ "1e-3",
                           var_name == "cndc" ~ "S m-1"),
         var_name = paste0(var_name," [", units,"]"),
         var_type = "phys",
         date_accessed = as.Date("2021-03-12")) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value); gc()
rm(sval_UNIS_nc_dat, sval_UNIS_TEMP, sval_UNIS_PSAL, sval_UNIS_CNDC); gc()

# Seabird database
# sval_seabird_database # Not on pCloud

# Protection of sites
# sval_protection # Not on pCloud

# N-ICE21015 many data products
# sval_NICE # Not on pCloud

# Fast ice persistence
# svalbard_fastice_persistency_2014.tif; svalbard_fastice_persistency_2015.tif; svalbard_fastice_persistency_2016.tif
# sval_fast # Not working with shape files of geomorphology

# Biogeochemistry
# 2009-2013-pigments-api-v1.tsv; 2010-2013-nutrients-api-v1.tsv
sval_biogeochemistry <- bind_rows(read_delim("~/pCloudDrive/FACE-IT_data/svalbard/2009-2013-pigments-api-v1.tsv", delim = "\t"),
                                  read_delim("~/pCloudDrive/FACE-IT_data/svalbard/2010-2013-nutrients-api-v1.tsv", delim = "\t")) %>% 
  dplyr::rename(date = eventDate, lon = decimalLongitude, lat = decimalLatitude) %>% 
  mutate(depth = case_when(!is.na(minimumDepthInMeters) ~ (minimumDepthInMeters+maximumDepthInMeters)/2,
                           TRUE ~ maximumDepthInMeters),
         date = as.Date(date)) %>% 
  dplyr::select(lon, lat, date, depth, chlorophyll_a, phaeopigment, nox:nitrate_stddev) %>% 
  pivot_longer(chlorophyll_a:nitrate_stddev, names_to = "var_name", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(var_type = case_when(var_name %in% c("chlorophyll_a", "phaeopigment") ~ "bio",
                              TRUE ~ "chem"),
         date_accessed = as.Date("2021-02-11"),
         URL = "https://data.npolar.no/dataset/c9de2d1f-54c1-49ca-b58f-a04cf5decca5",
         citation = "Norwegian Polar Institute (2020). Marine biogeochemistry [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2020.c9de2d1f") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)

# Combine and save
full_product_sval <- rbind(sval_UNIS_database, sval_biogeochemistry)
data.table::fwrite(full_product_sval, "~/pCloudDrive/FACE-IT_data/svalbard/full_product_sval.csv")
save(full_product_sval, file = "~/pCloudDrive/FACE-IT_data/svalbard/full_product_sval.RData")
rm(list = grep("sval_",names(.GlobalEnv),value = TRUE)); gc()


# Kongsfjorden ------------------------------------------------------------

## PG product --------------------------------------------------------------

# Load pg kong files
system.time(
  pg_kong_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_kong)
) # 57 seconds

# Test problem files
# pg_test <- pg_data(doi = "10.1594/PANGAEA.868371")
# pg_test <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.909130")
# pg_test <- pg_kong_sub %>% 
#   filter(URL == "https://doi.org/10.1594/PANGAEA.868371") %>% 
#   dplyr::select(contains("depth"), everything()) %>% 
#   # mutate_all(~na_if(., '')) %>% 
#   janitor::remove_empty("cols")

# More testing
# colnames(pg_kong_sub)
# test1 <- pg_kong_sub %>% 
#   dplyr::select(URL, `T air (1) [°C]`) %>% 
#   na.omit()
  
# Process Kongsfjorden bbox PANGAEA data
pg_kong_clean <- pg_kong_sub %>% 
  # dplyr::select(contains(c("Qz")), everything()) %>%  # Look at specific problem columns
  # dplyr::select(contains(c("press", "depth", "elev", "lon", "lat")), everything()) %>%  # Look at depth columns
  # dplyr::select(contains(c("date")), everything()) %>%  # Look at date columns
  # Manually remove problematic files- no need
  # Manually remove problematic columns
  # dplyr::select(-"File start date/time", -"File stop date/time") %>%
  mutate_all(~na_if(., '')) %>%
  janitor::remove_empty("cols") %>%
  # Manage lon/lat columns - no need
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = ifelse(date == "", NA, date),
         date = case_when(is.na(date) & !is.na(`Date/time start`) ~ `Date/time start`,
                          TRUE ~ date),
         date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = case_when(!is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Depth [m]`) ~ as.numeric(`Depth [m]`),
                           !is.na(`Press [dbar]`) ~ as.numeric(`Press [dbar]`))) %>%
  mutate(depth = case_when(is.na(depth) & !is.na(`Elevation [m]`) ~ -`Elevation [m]`,
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -`Elevation [m a.s.l.]`,
                           TRUE ~ depth)) %>% 
  # dplyr::select(depth, everything())
  # Remove unwanted columns
  dplyr::select(-"Longitude 2", -"Latitude 2",
                -"Date/time start", -"Date/time end", -"Date",
                -"Press [dbar]",
                -contains(c("Depth ", "Elev ", "Elevation "))) %>%
  # Finish up
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(7:length(.)), as.numeric) %>% 
  janitor::remove_empty("cols")
colnames(pg_kong_clean)

## Individual category data.frames
# Cryosphere
pg_kong_Cryosphere <- pg_var_melt(pg_kong_clean, query_Cryosphere$pg_col_name, "cryo")
# Physical
pg_kong_Physical <- pg_var_melt(pg_kong_clean, query_Physical$pg_col_name, "phys")
# Carbonate chemistry
pg_kong_Chemistry <- pg_var_melt(pg_kong_clean, query_Chemistry$pg_col_name, "chem")
# Biology
pg_kong_Biology <- pg_var_melt(pg_kong_clean, query_Biology$pg_col_name, "bio") # 0 values
# Social
pg_kong_Social <- pg_var_melt(pg_kong_clean, query_Social$pg_col_name, "soc") # 0 values

# Stack them together
pg_kong_ALL <- rbind(pg_kong_Cryosphere, pg_kong_Physical, pg_kong_Chemistry)
data.table::fwrite(pg_kong_ALL, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_ALL.csv")
save(pg_kong_ALL, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_ALL.RData")

# Load data to investigate
# pg_kong_ALL <- data.table::fread("~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_ALL.csv")

# Check that all columns were used
colnames(pg_kong_clean)[!colnames(pg_kong_clean) %in% unique(pg_kong_ALL$var_name)]

# Count per grid cell
pg_kong_ALL %>% 
  select(-URL, -citation) %>% 
  mutate(lon = round(lon, 2),
         lat = round(lat, 2)) %>% 
  group_by(lon, lat) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  ggplot(aes(x = lon, y = lat)) +
  borders(fill = "grey30") +
  geom_tile(aes(fill = count)) +
  coord_quickmap(xlim = c(bbox_kong[1:2]), 
                 ylim = c(bbox_kong[3:4])) +
  labs(x = NULL, y = NULL)

# Average temperature over depth
pg_kong_ALL %>% 
  select(-URL, -citation) %>% 
  filter(!is.na(depth),
         var_type == "phys") %>% 
  mutate(depth = round(depth, -1),
         year = lubridate::year(date)) %>%
  # filter(value > -20, value < 10) %>% 
  group_by(depth, year) %>% 
  dplyr::summarise(value = mean(value, na.rm = T),
                   count = n(), .groups = "drop") %>% 
  ggplot(aes(x = year, y = -depth)) +
  geom_tile(aes(fill = value, colour = count), size = 1) +
  scale_colour_distiller(palette = "Reds", direction = 1) +
  scale_fill_viridis_c() +
  coord_cartesian(expand = F) +
  labs(x = NULL, y = "Depth (m)", fill = "Value")

# Tables of value names
table(pg_kong_ALL$var_type)
table(pg_kong_ALL$var_name, pg_kong_ALL$var_type)

# Clean up
rm(list = grep("pg_kong",names(.GlobalEnv),value = TRUE)); gc()


## Full product ------------------------------------------------------------

# Load PG file
load("~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_ALL.RData")

# Process individual files
## Sea ice cover
kong_sea_ice_inner <- read_csv("~/pCloudDrive/FACE-IT_data/kongsfjorden/Kongsfjorden_sea_ice_cover_data.csv", na = "999") %>% 
  pivot_longer(February:June, names_to = "month", values_to = "value") %>% 
  mutate(month = match(month, month.name),
         date = as.Date(paste0(Year,"-",month,"-01")),
         lon = NA, lat = NA, depth = NA,
         var_name = "ice cover [%]",
         var_type = "cryo",
         date_accessed = as.Date("2021-02-11"),
         URL = "https://data.npolar.no/dataset/74c7b236-b94d-48c5-a665-ffcd54e8e1b7",
         citation = "Gerland, S., & Pavlova, O. (2020). Sea ice coverage in inner Kongsfjorden, Svalbard, 2003-2019, version 1.0 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2020.74c7b236") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)

## Zooplankton abundance and species
kong_zoo_data_1 <- read_csv("~/pCloudDrive/FACE-IT_data/kongsfjorden/kf_zooplankton_sampling_meta.csv")
kong_zoo_data_2 <- read_csv("~/pCloudDrive/FACE-IT_data/kongsfjorden/kf_zooplankton_species_meta.csv")
kong_zoo_data_3 <- read_csv("~/pCloudDrive/FACE-IT_data/kongsfjorden/kf_zooplankton_abundance_data.csv")
kong_zoo_data <- kong_zoo_data_3 %>% 
  pivot_longer(CALfinM:SCYPZlar, names_to = "sps", values_to = "value") %>% 
  left_join(kong_zoo_data_1, by = c("X1" = "id")) %>% 
  left_join(kong_zoo_data_2, by = c("sps" = "id")) %>% 
  dplyr::rename(lon = longitude, lat = latitude) %>% 
  mutate(value = value*biomass_conv, # Need to check that this conversion is correct
         var_name = case_when(!is.na(stage) ~ paste0(species," [",stage,"]"), TRUE ~ species),
         var_type = "bio",
         date_accessed = as.Date("2021-02-11"),
         URL = "https://data.npolar.no/dataset/94b29b16-b03b-47d7-bfbc-1c3c4f7060d2",
         citation = "Hop H, Wold A, Vihtakari M, Daase M, Kwasniewski S, Gluchowska M, Lischka S, Buchholz F, Falk-Petersen S (2019) Zooplankton in Kongsfjorden (1996-2016) in relation to climate change. In: The ecosystem of Kongsfjorden, Svalbard (eds. Hop H, Wiencke C), Advances in Polar Ecology, Springer Verlag.") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, var_type, var_name, value) %>% 
  summarise(depth = (from+to)/2, .groups = "drop") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)
rm(kong_zoo_data_1, kong_zoo_data_2, kong_zoo_data_3); gc()

## Protist species and nutrients and Chla
kong_protist_nutrient_chla_1 <- read_csv("~/pCloudDrive/FACE-IT_data/kongsfjorden/Metadata_Kongsfjorden2009-2014_Hegseth et al.csv") %>% 
  mutate(`Sampling date` = str_replace_all(`Sampling date`, "[.]", "/"),
         Longitude = as.character(Longitude), Latitude = as.character(Latitude),
         Latitude = as.numeric(gsub("^(.{2})(.*)$", "\\1.\\2", Latitude)),
         Longitude = if_else(substring(Longitude, 1, 1) != "1", 
                             as.numeric(gsub("^(.{1})(.*)$", "\\1.\\2", Longitude)),
                             as.numeric(gsub("^(.{2})(.*)$", "\\1.\\2", Longitude))))
kong_protist_nutrient_chla_2 <- read_csv("~/pCloudDrive/FACE-IT_data/kongsfjorden/Protist_abundance_Kongsfjorden2009-2013_Hegseth et al.csv") %>% 
  dplyr::select(Cruise:Year, Taxon_full, `Abundance (Cells L-1)`) %>% 
  pivot_wider(names_from = Taxon_full, values_from = `Abundance (Cells L-1)`, values_fn = mean)
kong_protist_nutrient_chla_3 <- read_csv("~/pCloudDrive/FACE-IT_data/kongsfjorden/Nutrients&Chla_Kongsfjorden2009-2014_Hegseth et al.csv", na = c("NA", "na"))
kong_protist_nutrient_chla <- kong_protist_nutrient_chla_1 %>% 
  left_join(kong_protist_nutrient_chla_3, by = c("SampleID" = "CHLA_sampleID", "Station", "Year", "Depth")) %>% 
  left_join(kong_protist_nutrient_chla_3, by = c("SampleID" = "NUTRIENT_sampleID", "Station", "Year", "Depth", "Cruise",
                                                 "P", "NO2", "NO3", "Si", "NH4", "Chla")) %>% 
  left_join(kong_protist_nutrient_chla_2, by = c("SampleID")) %>% 
  # The date formatting is a bit of a struggle
  mutate(date = as.Date(`Sampling date`, tryFormats = c("%m/%d/%Y", "%Y%m%d"))) %>% 
  mutate(date = case_when(is.na(date) ~ as.Date(`Sampling date`, format = "%d%m%Y"), TRUE ~ date)) %>% 
  mutate(date = case_when(is.na(date) ~ as.Date(`Sampling date`, format = "%d/%m/%Y"), TRUE ~ date)) %>% 
  mutate(date = case_when(is.na(date) ~ as.Date(`Sampling date`, format = "%Y-%m-%d %H:%M"), TRUE ~ date)) %>% 
  dplyr::rename(lon = Longitude, lat = Latitude, depth = Depth.x) %>%
  dplyr::select(lon, lat, date, depth, P:Chla, `Dinobryon spp. cyst`:`Heterocapsa  sp.`) %>%
  pivot_longer(P:`Heterocapsa  sp.`, names_to = "var_name", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(var_type = case_when(var_name %in% c("P", "NO2", "NO3", "Si", "NH4") ~ "chem", # May want to include "Chla
                              TRUE ~ "bio"),
         date_accessed = as.Date("2021-02-11"),
         URL = "https://data.npolar.no/dataset/2bff82dc-22b9-41c0-8348-220e7d6ca4f4",
         citation = "Hegseth EN, Assmy P, Wiktor JM, Wiktor Jr. JM, Kristiansen S, Leu E, Tverberg V, Gabrielsen TM, Skogseth R and Cottier F (2019) Phytoplankton Seasonal Dynamics in Kongsfjorden, Svalbard and the Adjacent Shelf. In: The ecosystem of Kongsfjorden, Svalbard (eds. Hop H, Wiencke C), Advances in Polar Ecology, Springer Verlag.") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)
rm(kong_protist_nutrient_chla_1, kong_protist_nutrient_chla_2, kong_protist_nutrient_chla_3); gc()

## CTD sampling data
# ncdf4::nc_open("~/pCloudDrive/FACE-IT_data/kongsfjorden/Kongsfjorden_ctd_1906_2017.nc")
kong_CTD_nc_dat <- ncdf4::nc_open("~/pCloudDrive/FACE-IT_data/kongsfjorden/Kongsfjorden_ctd_1906_2017.nc")
kong_CTD_TEMP <- CTD_to_long(kong_CTD_nc_dat, "TEMP")
kong_CTD_PSAL <- CTD_to_long(kong_CTD_nc_dat, "PSAL")
kong_CTD_CNDC <- CTD_to_long(kong_CTD_nc_dat, "CNDC")
kong_CTD_database <- left_join(kong_CTD_TEMP, kong_CTD_PSAL, by = c("lon", "lat", "date", "depth")) %>% 
  left_join(kong_CTD_CNDC, by = c("lon", "lat", "date", "depth")) %>% 
  pivot_longer(temp:cndc, names_to = "var_name", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(URL = "https://data.npolar.no/dataset/074a215c-d1df-47a9-bea7-e0fcc37273c6",
         citation = "Skogseth, R., Tverberg, V., Walczowski, W., & Sundfjord, A. (2019). Kongsfjorden Transect CTD data 1906-2016 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.074a215c",
         units = case_when(var_name == "temp" ~ "°C",
                           var_name == "psal" ~ "1e-3",
                           var_name == "cndc" ~ "S m-1"),
         var_name = paste0(var_name," [", units,"]"),
         var_type = "phys",
         date_accessed = as.Date("2021-02-11")) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)
rm(kong_CTD_nc_dat, kong_CTD_TEMP, kong_CTD_PSAL, kong_CTD_CNDC); gc()

## CO2 data
kong_CTD_CO2 <- read_csv("~/pCloudDrive/FACE-IT_data/kongsfjorden/Kongsfjorden_Marine_CO2_system_2012_to_2014.csv") %>% 
  dplyr::rename(date = `yyyy-mm-dd`, lon = Longitude, lat = Latitude, depth = `Depth [m]`) %>% 
  dplyr::select(date:`DIC [µmol/kg]`, -`Bot.Depth [m]`) %>% 
  pivot_longer(Salinity:`DIC [µmol/kg]`, names_to = "var_name", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(date = as.Date(date),
         var_type = case_when(var_name == "Salinity" ~ "phys",
                              var_name == "Temperature [C]" ~ "phys",
                              var_name == "AT [µmol/kg]" ~ "chem",
                              var_name == "DIC [µmol/kg]" ~ "chem"),
         date_accessed = as.Date("2021-02-11"),
         URL = "https://data.npolar.no/dataset/e53eae53-147a-45df-b473-917bb5ba1ed4",
         citation = "Fransson, A., & Chierici, M. (2019). Marine CO2 system data for the Svalbard fjord Kongsfjorden and the West-Spitsbergen shelf in July 2012-2014 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.e53eae53") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)

## Glacial topography + thickness
kong_glacier_info_1 <- load_utm("~/pCloudDrive/FACE-IT_data/kongsfjorden/TIGRIF_DEM_ice_surface_150m_v1.tif")
kong_glacier_info_2 <- load_utm("~/pCloudDrive/FACE-IT_data/kongsfjorden/TIGRIF_DEM_ice_thickness_150m_1.tif")
kong_glacier_info_3 <- load_utm("~/pCloudDrive/FACE-IT_data/kongsfjorden/TIGRIF_DEM_subglacial_elevation_150m_v1.tif")
kong_glacier_info_4 <- read_delim("~/pCloudDrive/FACE-IT_data/kongsfjorden/TIGRIF_radarprofiles_2004_2016_v1.txt", delim = "\t")[,1:5]
coordinates(kong_glacier_info_4) <- ~x+y 
proj4string(kong_glacier_info_4) <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs" # Matches other similar files
kong_glacier_info_4 <- spTransform(kong_glacier_info_4, CRS("+proj=longlat +datum=WGS84"))
kong_glacier_info_4 <- as.data.frame(kong_glacier_info_4) %>% 
  dplyr::rename(lon = x, lat = y, thick = thick., bed = bed.) %>% 
  dplyr::select(lon, lat, everything())
kong_glacier_info <- full_join(kong_glacier_info_4, kong_glacier_info_1, by = c("lon", "lat")) %>% 
  full_join(kong_glacier_info_2, by = c("lon", "lat")) %>% 
  full_join(kong_glacier_info_3, by = c("lon", "lat")) %>% 
  pivot_longer(surf:TIGRIF_DEM_subglacial_elevation_150m_v1, names_to = "var_name", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(date = as.Date(NA), depth = as.numeric(NA),
         var_type = "cryo",
         date_accessed = as.Date("2021-02-11"),
         URL = "https://data.npolar.no/dataset/702ca4a7-7d02-462c-8cbd-2d80d0e977a1",
         citation = "Lindbäck, K., Kohler, J., Pettersson, R., Nuth, C., Langley, K., Messerli, A., … Brandt, O. (2018). Subglacial topography, ice thickness, and bathymetry of Kongsfjorden, northwestern Svalbard [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2017.702ca4a7") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)
rm(kong_glacier_info_1, kong_glacier_info_2, kong_glacier_info_3, kong_glacier_info_4); gc()

## Kongsvegen weather station
kong_weather_station <- read_delim("~/pCloudDrive/FACE-IT_data/kongsfjorden/Kongsvegen-weather.tsv", delim = "\t", na = "null") %>% 
  mutate(date = as.Date(timestamp), .keep = "unused") %>% 
  pivot_longer(sw_out_wpm2_avg:ws_2_wvc1, names_to = "var_name", values_to = "value") %>% 
  mutate(lon = NA, lat = NA, depth = NA, 
         var_type =  "phys",
         date_accessed = as.Date("2021-03-02"),
         URL = "https://data.npolar.no/dataset/5dc31930-0922-4483-a1df-6f48af9e371b",
         citation = "Kohler, J., Hudson, S. R., & Obleitner, F. (2017). Automatic weather station data from Kongsvegen, Ny-Ålesund [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2017.5dc31930") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)

## GFI mooring
# tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/1883_RCM_645_QC.nc")
# ncdf4::nc_open("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/0687_RCM_2761_QC.nc")
kong_mooring_GFI_units <- distinct(rbind(ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/1883_RCM_645_QC.nc")$variable,
                                         ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/1766_RCM_10907_QC.nc")$variable,
                                         ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/0712_RCM_12347_QC.nc")$variable))
kong_mooring_GFI_1 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/1883_RCM_645_QC.nc")) %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/1883_RCM_645_QC.nc"), "S"))) %>% 
  mutate(date = as.Date(as.POSIXct(time*86400, origin = "2014-08-30 12:00:00", tz = "UTC")), 
         URL = "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/KF_201408/", .keep = "unused")
kong_mooring_GFI_2 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/1884_RCM_8003_QC.nc")) %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/1884_RCM_8003_QC.nc"), "S"))) %>% 
  mutate(date = as.Date(as.POSIXct(time*86400, origin = "2014-08-29 23:00:00", tz = "UTC")), 
         URL = "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/KF_201408/", .keep = "unused")
kong_mooring_GFI_3 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/1885_RCM_646_QC.nc")) %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/1885_RCM_646_QC.nc"), "S"))) %>% 
  mutate(date = as.Date(as.POSIXct(time*86400, origin = "2014-08-30 12:00:00", tz = "UTC")), 
         URL = "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/KF_201408/", .keep = "unused")
kong_mooring_GFI_4 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/1766_RCM_10907_QC.nc")) %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/1766_RCM_10907_QC.nc"), "S"))) %>% 
  mutate(date = as.Date(as.POSIXct(time*86400, origin = "2013-08-31 04:00:00", tz = "UTC")), 
         URL = "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201308_A/", .keep = "unused")
kong_mooring_GFI_5 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/1856_RCM_645_QC.nc")) %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/1856_RCM_645_QC.nc"), "S"))) %>% 
  mutate(date = as.Date(as.POSIXct(time*86400, origin = "2013-08-31 13:00:01", tz = "UTC")), 
         URL = "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201308_A/", .keep = "unused")
kong_mooring_GFI_6 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/1857_RCM_646_QC.nc")) %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/1857_RCM_646_QC.nc"), "S"))) %>% 
  mutate(date = as.Date(as.POSIXct(time*86400, origin = "2013-08-31 13:00:01", tz = "UTC")), 
         URL = "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201308_A/", .keep = "unused")
kong_mooring_GFI_7 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/0712_RCM_12347_QC.nc")) %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/0712_RCM_12347_QC.nc"), "S"))) %>% 
  mutate(date = as.Date(as.POSIXct(time*86400, origin = "2012-09-08 20:30:00", tz = "UTC")), 
         URL = "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201209_D/", .keep = "unused")
kong_mooring_GFI_8 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/1848_RCM_645_QC.nc")) %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/1848_RCM_645_QC.nc"), "S"))) %>% 
  mutate(date = as.Date(as.POSIXct(time*86400, origin = "2012-09-09 13:00:00", tz = "UTC")), 
         URL = "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201209_D/", .keep = "unused")
kong_mooring_GFI_9 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/1849_RCM_646_QC.nc")) %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/1849_RCM_646_QC.nc"), "S"))) %>% 
  mutate(date = as.Date(as.POSIXct(time*86400, origin = "2012-09-09 13:00:00", tz = "UTC")), 
         URL = "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201209_D/", .keep = "unused")
kong_mooring_GFI_10 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/0706_RCM_10907_QC.nc")) %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/0706_RCM_10907_QC.nc"), "S"))) %>% 
  mutate(date = as.Date(as.POSIXct(time*86400, origin = "2011-09-10 09:30:00", tz = "UTC")), 
         URL = "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201109_A/", .keep = "unused")
kong_mooring_GFI_11 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/1844_RCM_645_QC.nc")) %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/1844_RCM_645_QC.nc"), "S"))) %>% 
  mutate(date = as.Date(as.POSIXct(time*86400, origin = "2011-09-11 07:37:00", tz = "UTC")), 
         URL = "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201109_A/", .keep = "unused")
kong_mooring_GFI_12 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/1845_RCM_646_QC.nc")) %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/1845_RCM_646_QC.nc"), "S"))) %>% 
  mutate(date = as.Date(as.POSIXct(time*86400, origin = "2011-09-11 07:30:00", tz = "UTC")), 
         URL = "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201109_A/", .keep = "unused")
kong_mooring_GFI_13 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/0686_RCM_8003_QC.nc")) %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/0686_RCM_8003_QC.nc"), "S"))) %>% 
  mutate(date = as.Date(as.POSIXct(time*86400, origin = "2010-09-12 15:00:00", tz = "UTC")), 
         URL = "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201009_A/", .keep = "unused")
kong_mooring_GFI_14 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/0687_RCM_2761_QC.nc")) %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI/0687_RCM_2761_QC.nc"), "S"))) %>% 
  mutate(date = as.Date(as.POSIXct(time*86400, origin = "2010-09-12 15:00:00", tz = "UTC")), 
         URL = "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201009_A/", .keep = "unused")
kong_mooring_GFI <- bind_rows(kong_mooring_GFI_1, kong_mooring_GFI_2, kong_mooring_GFI_3, kong_mooring_GFI_4, kong_mooring_GFI_5,
                              kong_mooring_GFI_6, kong_mooring_GFI_7, kong_mooring_GFI_8, kong_mooring_GFI_9, kong_mooring_GFI_10,
                              kong_mooring_GFI_11, kong_mooring_GFI_12, kong_mooring_GFI_13, kong_mooring_GFI_14) %>% 
  dplyr::select(URL, lon, lat, date, depth, everything()) %>% 
  pivot_longer(temp:speed, names_to = "var_name", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  left_join(kong_mooring_GFI_units, by = c("var_name" = "name")) %>% 
  mutate(units = case_when(units == "Celsius" ~ "°C", units == "degree" ~ "°", TRUE ~ units),
         citation = NA,
         var_type = "phys",
         var_name = paste0(var_name, " [", units,"]"),
         date_accessed = as.Date("2021-08-04")) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)
rm(kong_mooring_GFI_units, kong_mooring_GFI_1, kong_mooring_GFI_2, kong_mooring_GFI_3, kong_mooring_GFI_4, kong_mooring_GFI_5,
   kong_mooring_GFI_6, kong_mooring_GFI_7, kong_mooring_GFI_8, kong_mooring_GFI_9, kong_mooring_GFI_10, kong_mooring_GFI_11,
   kong_mooring_GFI_12, kong_mooring_GFI_13, kong_mooring_GFI_14); gc()

# Combine and save
full_product_kong <- rbind(pg_kong_ALL, kong_sea_ice_inner, kong_zoo_data, kong_protist_nutrient_chla, # kong_glacier_info,
                           kong_CTD_database, kong_CTD_CO2, kong_weather_station, kong_mooring_GFI)
data.table::fwrite(full_product_kong, "~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.csv")
save(full_product_kong, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")
rm(list = grep("kong_",names(.GlobalEnv),value = TRUE)); gc()

# Search product for specific authors
# load("~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")

# Philipp Fischer ferry box data - There are a couple of months of data for 2014
# kong_fischer <- full_product_kong %>% 
  # filter(grepl("Fischer", citation))

# Popova carbonate chemistry model data - No data present
# kong_popova <- full_product_kong %>% 
  # filter(grepl("Popova", citation))

# Clean up
# rm(kong_fischer, kong_popova)


# Isfjorden ---------------------------------------------------------------

## PG product --------------------------------------------------------------

# Load pg is files
system.time(
  pg_is_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_is)
) # 56 seconds

# Test problem files
# pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
# pg_test <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.925759")
# pg_test <- pg_test_dl("10.1594/PANGAEA.909130")

# Remove unneeded columns
pg_is_clean <- pg_is_sub %>% 
  # dplyr::select(contains(c("date")), everything()) %>%  # Look at date columns
  # dplyr::select(contains(c("press", "depth", "elev")), everything()) %>%  # Look at specific problem columns
  # Manually remove problematic files
  # filter(!URL %in% c("https://doi.org/10.1594/PANGAEA.900501", "https://doi.org/10.1594/PANGAEA.786375",
                     # "https://doi.org/10.1594/PANGAEA.847003","https://doi.org/10.1594/PANGAEA.867207")) %>% 
  # Manually remove problematic columns
  # dplyr::select(-"File start date/time", -"File stop date/time") %>%
  # mutate(date_accessed = as.Date(date_accessed)) %>% 
  mutate_all(~na_if(., '')) %>% 
  janitor::remove_empty("cols") %>% 
  # Manage lon/lat columns - no need
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = ifelse(date == "", NA, date),
         date = case_when(is.na(date) & !is.na(`Sampling date`) ~ `Sampling date`,
                          date == 2008 ~ "2008-01-01", TRUE ~ date),
         date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = case_when(!is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Depth [m]`) ~ as.numeric(`Depth [m]`),
                           !is.na(`Press [dbar]`) ~ as.numeric(`Press [dbar]`))) %>%
  mutate(depth = case_when(is.na(depth) & !is.na(`Elevation [m]`) ~ -`Elevation [m]`,
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -`Elevation [m a.s.l.]`,
                           TRUE ~ depth)) %>% 
  # dplyr::select(depth, everything())
  # Remove unused meta columns
  dplyr::select(-contains(c("Longitude", "Latitude", "Depth ", "Press"))) %>%
  # Finish up
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(7:length(.)), as.numeric) %>% 
  janitor::remove_empty("cols")
colnames(pg_is_clean)

## Individual category data.frames
# Cryosphere
pg_is_Cryosphere <- pg_var_melt(pg_is_clean, query_Cryosphere$pg_col_name, "cryo")
# Physical
pg_is_Physical <- pg_var_melt(pg_is_clean, query_Physical$pg_col_name, "phys")
# Carbonate chemistry
pg_is_Chemistry <- pg_var_melt(pg_is_clean, query_Chemistry$pg_col_name, "chem")
# Biology
pg_is_Biology <- pg_var_melt(pg_is_clean, query_Biology$pg_col_name, "bio")
# Social
pg_is_Social <- pg_var_melt(pg_is_clean, query_Social$pg_col_name, "soc") # empty

# Stack them together
pg_is_ALL <- rbind(pg_is_Cryosphere, pg_is_Physical, pg_is_Chemistry, pg_is_Biology)
data.table::fwrite(pg_is_ALL, "~/pCloudDrive/FACE-IT_data/isfjorden/pg_is_ALL.csv")
save(pg_is_ALL, file = "~/pCloudDrive/FACE-IT_data/isfjorden/pg_is_ALL.RData")

# Check that all columns were used
colnames(pg_is_clean)[!colnames(pg_is_clean) %in% unique(pg_is_ALL$var_name)]

# Clean up
rm(list = grep("pg_is",names(.GlobalEnv),value = TRUE)); gc()


## Full product ------------------------------------------------------------

# Load PG file
load("~/pCloudDrive/FACE-IT_data/isfjorden/pg_is_ALL.RData")

# Process individual files
## Mouth mooring North
# tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_N/IN1516.nc")
# as.data.frame(ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_N/IN1516.nc")$attribute$global)
is_mooring_N_units <- ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_N/IN1516.nc")$variable
is_mooring_N_1 <- tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_N/IN1516.nc") %>% hyper_tibble() %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_N/IN1516.nc"), "D2"))) %>% 
  mutate(URL = "https://data.npolar.no/dataset/111aca43-7f5c-4c15-9f31-dcd3214dbfcb",
         citation = "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - North (I-N) during 31 Aug 2015 to 12 Aug 2016 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.111aca43")
is_mooring_N_2 <- tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_N/IN1617.nc") %>% hyper_tibble() %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_N/IN1617.nc"), "D2"))) %>% 
  mutate(URL = "https://data.npolar.no/dataset/3078f619-9955-4a7f-9316-fab598fec382",
         citation = "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - North (I-N) during 15 Oct 2016 to 2 Oct 2017 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.3078f619")
is_mooring_N_3 <- tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_N/IN1718.nc") %>% hyper_tibble() %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_N/IN1718.nc"), "D2"))) %>% 
  mutate(URL = "https://data.npolar.no/dataset/e9106051-6c44-4849-9d62-04e4a82f1ca9",
         citation = "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - North (I-N) during 5 Oct 2017 to 24 Aug 2018 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.e9106051")
is_mooring_N <- rbind(is_mooring_N_1, is_mooring_N_2, is_mooring_N_3) %>% 
  mutate(date = as.Date(as.POSIXct(TIME*86400, origin = "1950-01-01", tz = "UTC")), .keep = "unused") %>% 
  dplyr::rename(lon = LONGITUDE, lat = LATITUDE, depth = MPRES) %>% 
  pivot_longer(CDNC:VVEL, names_to = "var_name", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  left_join(is_mooring_N_units, by = c("var_name" = "name")) %>% 
  mutate(units = case_when(units == "degree_Celsius" ~ "°C", TRUE ~ units),
         var_name = paste0(var_name, " [", units,"]"),
         var_type = "phys",
         date_accessed = as.Date("2021-04-15")) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)
rm(is_mooring_N_units, is_mooring_N_1, is_mooring_N_2, is_mooring_N_3); gc()

## Mouth mooring South
# tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS0506.nc")
# as.data.frame(ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS0506.nc")$attribute$global)
is_mooring_S_URL <- c("https://data.npolar.no/dataset/176eea39-7d99-49d7-a082-b18acf42850c", "https://data.npolar.no/dataset/a1239ca3-79e6-4284-bba5-38028358994a", 
                      "https://data.npolar.no/dataset/064a09b7-f590-4448-810e-3f287b182dd2", "https://data.npolar.no/dataset/b0e473c4-b5b9-4ebc-96eb-411d47f1d850", 
                      "https://data.npolar.no/dataset/2be7bdee-c899-45b8-901b-9ec5baa9397a", "https://data.npolar.no/dataset/a247e9a9-4b62-4149-bbf4-83df3576a7c4", 
                      "https://data.npolar.no/dataset/6813ce6d-bdc9-4375-a310-679e074bee6b", "https://data.npolar.no/dataset/11b7e849-e53d-40d8-909b-13e29c7971a0", 
                      "https://data.npolar.no/dataset/21838303-c9a0-4fc4-aac3-f537b37356df", "https://data.npolar.no/dataset/cd7a2f7c-abed-4284-b7c5-a9ff43c89afc", 
                      "https://data.npolar.no/dataset/54dcd0c9-b863-41b1-a72b-0827099ad2b0")
is_mooring_S_citation <- c("Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during September 2005 to September 2006 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.176eea39", 
                           "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during September 2006 to September 2007 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.a1239ca3", 
                           "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the outer Isfjorden - South (I-S) during September 2007 to January 2008 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.064a09b7", 
                           "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 9 Sep 2010 to 3 Sep 2011 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.b0e473c4", 
                           "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 8 Sep 2011 to 3 Sep 2012 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.2be7bdee", 
                           "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 6 Sep 2012 to 28 Aug 2013 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.a247e9a9", 
                           "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 2 Sep 2013 to 26 Aug 2014 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.6813ce6d", 
                           "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 31 Aug 2014 to 24 Aug 2015 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.11b7e849", 
                           "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 31 Aug 2015 to 12 Aug 2016 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.21838303", 
                           "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 19 Aug 2016 to 2 Oct 2017 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.cd7a2f7c", 
                           "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from the Isfjorden Mouth - South (I-S) during 5 Oct 2017 to 25 Aug 2018 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.54dcd0c9")
is_mooring_S_units <- ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS1617_ADCP.nc")$variable
is_mooring_S_1 <- tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS0506.nc") %>% hyper_tibble() %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS0506.nc"), "D2"))) %>% 
  mutate(URL = is_mooring_S_URL[1], citation = is_mooring_S_citation[1])
is_mooring_S_2 <- tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS0607.nc") %>% hyper_tibble() %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS0607.nc"), "D2"))) %>% 
  mutate(URL = is_mooring_S_URL[2], citation = is_mooring_S_citation[2])
is_mooring_S_3 <- tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS0708.nc") %>% hyper_tibble() %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS0708.nc"), "D2"))) %>% 
  mutate(URL = is_mooring_S_URL[3], citation = is_mooring_S_citation[3])
is_mooring_S_4 <- tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS1011.nc") %>% hyper_tibble() %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS1011.nc"), "D2"))) %>% 
  mutate(URL = is_mooring_S_URL[4], citation = is_mooring_S_citation[4])
is_mooring_S_5 <- tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS1112.nc") %>% hyper_tibble() %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS1112.nc"), "D2"))) %>% 
  mutate(URL = is_mooring_S_URL[5], citation = is_mooring_S_citation[5])
is_mooring_S_6 <- tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS1213.nc") %>% hyper_tibble() %>%
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS1213.nc"), "D2"))) %>% 
  mutate(URL = is_mooring_S_URL[6], citation = is_mooring_S_citation[6])
is_mooring_S_7 <- tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS1314.nc") %>% hyper_tibble() %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS1314.nc"), "D2"))) %>% 
  mutate(URL = is_mooring_S_URL[7], citation = is_mooring_S_citation[7])
is_mooring_S_8 <- tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS1415.nc") %>% hyper_tibble() %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS1415.nc"), "D2"))) %>% 
  mutate(URL = is_mooring_S_URL[8], citation = is_mooring_S_citation[8])
is_mooring_S_9_ADCP <- tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS1516_ADCP.nc") %>% hyper_tibble() %>%
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS1516_ADCP.nc"), "D2"))) %>% 
  mutate(URL = is_mooring_S_URL[9], citation = is_mooring_S_citation[9])
is_mooring_S_9 <- tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS1516.nc") %>% hyper_tibble() %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS1516.nc"), "D2"))) %>% 
  mutate(URL = is_mooring_S_URL[9], citation = is_mooring_S_citation[9])
is_mooring_S_10_ADCP <- tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS1617_ADCP.nc") %>% hyper_tibble() %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS1617_ADCP.nc"), "D2"))) %>% 
  mutate(URL = is_mooring_S_URL[10], citation = is_mooring_S_citation[10])
is_mooring_S_10 <- tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS1617.nc") %>% hyper_tibble() %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS1617.nc"), "D2"))) %>% 
  mutate(URL = is_mooring_S_URL[10], citation = is_mooring_S_citation[10])
is_mooring_S_11 <- tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS1718.nc") %>% hyper_tibble() %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S/IS1718.nc"), "D2"))) %>% 
  mutate(URL = is_mooring_S_URL[11], citation = is_mooring_S_citation[11])
is_mooring_S <- bind_rows(is_mooring_S_1, is_mooring_S_2, is_mooring_S_3, is_mooring_S_4, is_mooring_S_5, is_mooring_S_6, is_mooring_S_7,
                      is_mooring_S_8, is_mooring_S_9_ADCP, is_mooring_S_9, is_mooring_S_10_ADCP, is_mooring_S_10, is_mooring_S_11) %>% 
  mutate(date = as.Date(as.POSIXct(TIME*86400, origin = "1950-01-01", tz = "UTC")), .keep = "unused") %>% 
  dplyr::rename(lon = LONGITUDE, lat = LATITUDE, depth = MPRES) %>% 
  dplyr::select(URL, citation, lon, lat, date, depth, everything(), -STATION, -FDEP) %>% 
  pivot_longer(TEMP:WVEL, names_to = "var_name", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  left_join(is_mooring_S_units, by = c("var_name" = "name")) %>% 
  mutate(units = case_when(units == "degree_Celsius" ~ "°C", TRUE ~ units),
         var_name = paste0(var_name, " [", units,"]"),
         var_type = "phys",
         date_accessed = as.Date("2021-04-15")) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)
rm(is_mooring_S_units, is_mooring_S_1, is_mooring_S_2, is_mooring_S_3, is_mooring_S_4, is_mooring_S_5, is_mooring_S_6, is_mooring_S_7,
   is_mooring_S_8, is_mooring_S_9_ADCP, is_mooring_S_9, is_mooring_S_10_ADCP, is_mooring_S_10, is_mooring_S_11,
   is_mooring_S_URL, is_mooring_S_citation); gc()

## Mooring IFO
is_mooring_IFO_units <- rbind(ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_IFO/IFO1617.nc")$variable,
                              ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_IFO/IFO1617_ADCP.nc")$variable) %>% distinct()
is_mooring_IFO <- tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_IFO/IFO1617.nc") %>% hyper_tibble() %>% 
  bind_rows(hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_IFO/IFO1617_ADCP.nc"))) %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_IFO/IFO1617.nc"), "D2"))) %>% 
  mutate(date = as.Date(as.POSIXct(TIME*86400, origin = "1950-01-01", tz = "UTC")), .keep = "unused") %>% 
  dplyr::rename(lon = LONGITUDE, lat = LATITUDE, depth = MPRES) %>% 
  dplyr::select(lon, lat, date, depth, everything(), -STATION, -FDEP) %>% 
  pivot_longer(DEN:WVEL, names_to = "var_name", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  left_join(is_mooring_IFO_units, by = c("var_name" = "name")) %>% 
  mutate(units = case_when(units == "degree_Celsius" ~ "°C", TRUE ~ units),
         URL = "https://data.npolar.no/dataset/7718a106-5d13-42d9-bb79-1d2adf0f51c4",
         citation = "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from Isfjorden online mooring (IFO) during 30 Sep 2016 to 11 Mar 2017 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.7718a106",
         var_name = paste0(var_name, " [", units,"]"),
         var_type = "phys",
         date_accessed = as.Date("2021-04-15")) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)
rm(is_mooring_IFO_units); gc()

## North mouth mooring GFI
# tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_N/1954_RCM_1318_QC.nc")
# ncdf4::nc_open("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_N/1954_RCM_1318_QC.nc")
is_mooring_GFI_N_units <- distinct(rbind(ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_N/1954_RCM_1318_QC.nc")$variable,
                                         ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_N/1955_RCM_645_QC.nc")$variable,
                                         ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_N/1955_RCM_645_QC.nc")$variable))
is_mooring_GFI_N_1 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_N/1954_RCM_1318_QC.nc")) %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_N/1954_RCM_1318_QC.nc"), "S")))
is_mooring_GFI_N_2 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_N/1955_RCM_645_QC.nc")) %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_N/1955_RCM_645_QC.nc"), "S")))
is_mooring_GFI_N_3 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_N/1956_RCM_646_QC.nc")) %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_N/1956_RCM_646_QC.nc"), "S")))
is_mooring_GFI_N <- bind_rows(is_mooring_GFI_N_1, is_mooring_GFI_N_2, is_mooring_GFI_N_3) %>% 
  mutate(date = as.Date(time, origin = "2015-08-31"), .keep = "unused") %>% 
  pivot_longer(temp:oxy, names_to = "var_name", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  left_join(is_mooring_GFI_N_units, by = c("var_name" = "name")) %>% 
  mutate(units = case_when(units == "Celsius" ~ "°C", units == "degree" ~ "°", TRUE ~ units),
         URL = "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201508_Isfjorden/",
         citation = "Skogseth R., Olivier L.L.A., Nilsen F., Falck E., Fraser N., Tverberg V., Ledang A.B., Vader A., Jonassen M.O., Søreide J., Cottier F., Berge J., Ivanov B.V., and Falk-Petersen S. (2019). Variability and decadal trends in the Isfjorden (Svalbard) ocean climate and circulation - a thermometer for climate change in the Arctic Ocean, Submitted to Progress in Oceanography",
         var_type = case_when(var_name == "oxy" ~ "chem",
                              TRUE ~ "phys"),
         var_name = paste0(var_name, " [", units,"]"),
         date_accessed = as.Date("2021-04-15")) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)
rm(is_mooring_GFI_N_units, is_mooring_GFI_N_1, is_mooring_GFI_N_2, is_mooring_GFI_N_3); gc()

## North mouth mooring GFI
# tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_S/1841_RCM_464_QC.nc")
# ncdf4::nc_open("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_S/1841_RCM_464_QC.nc")
is_mooring_GFI_S_units <- distinct(rbind(ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_S/0688_RCM_784_QC.nc")$variable,
                                         ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_S/0710_RCM_783_QC.nc")$variable,
                                         ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_S/0711_RCM_784_QC.nc")$variable,
                                         ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_S/1841_RCM_464_QC.nc")$variable))
is_mooring_GFI_S_1 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_S/0688_RCM_784_QC.nc")) %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_S/0688_RCM_784_QC.nc"), "S"))) %>% 
  mutate(date = as.Date(time, origin = "2010-09-09"),
         URL = "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201009_B/", .keep = "unused")
is_mooring_GFI_S_2 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_S/0710_RCM_783_QC.nc")) %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_S/0710_RCM_783_QC.nc"), "S"))) %>% 
  mutate(date = as.Date(time, origin = "2012-09-06"),
         URL = "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201209_A/", .keep = "unused")
is_mooring_GFI_S_3 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_S/0711_RCM_784_QC.nc")) %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_S/0711_RCM_784_QC.nc"), "S"))) %>% 
  mutate(date = as.Date(time, origin = "2012-09-06"),
         URL = "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201209_A/", .keep = "unused")
is_mooring_GFI_S_4 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_S/1841_RCM_464_QC.nc")) %>% 
  cbind(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_S/1841_RCM_464_QC.nc"), "S"))) %>% 
  mutate(date = as.Date(time, origin = "2012-09-06"),
         URL = "ftp://ftp.nmdc.no/nmdc/UIB/Currents/moorings/SV_201209_A/", .keep = "unused")
is_mooring_GFI_S <- bind_rows(is_mooring_GFI_S_1, is_mooring_GFI_S_2, is_mooring_GFI_S_3, is_mooring_GFI_S_4) %>% 
  dplyr::select(URL, lon, lat, date, depth, everything()) %>% 
  pivot_longer(temp:oxy, names_to = "var_name", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  left_join(is_mooring_GFI_S_units, by = c("var_name" = "name")) %>% 
  mutate(units = case_when(units == "Celsius" ~ "°C", units == "degree" ~ "°", TRUE ~ units),
         citation = NA,
         var_type = case_when(var_name == "oxy" ~ "chem",
                              TRUE ~ "phys"),
         var_name = paste0(var_name, " [", units,"]"),
         date_accessed = as.Date("2021-08-04")) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)
rm(is_mooring_GFI_S_units, is_mooring_GFI_S_1, is_mooring_GFI_S_2, is_mooring_GFI_S_3, is_mooring_GFI_S_4); gc()

## CO2 station at Tempelfjorden
is_CO2_tempelfjorden <- read_csv("~/pCloudDrive/FACE-IT_data/isfjorden/Marine_CO2_system_data_from_Tempelfjorden_2015_to_2017.csv") %>% 
  dplyr::rename(lon = `Longitude [°]`, lat = `Latitude [°]`, depth = `CTD Pressure [dbar]`) %>% 
  mutate(date = as.Date(paste0(Year,"-",Month,"-",Day)), .keep = "unused") %>% 
  dplyr::select(lon, lat, date, depth, `Temperature [ITS-90, deg C]`:`pHT in situ (temperature correction using EP TA)`) %>% 
  pivot_longer(`Temperature [ITS-90, deg C]`:`pHT in situ (temperature correction using EP TA)`, names_to = "var_name", values_to = "value") %>% 
  mutate(URL = "https://sios-svalbard.org/metsis/search?fulltext=isfjorden&start_date=&end_date=&is_parent=All",
         citation = NA,
         var_type = case_when(var_name == "Salinity [PSU]" ~ "phys",
                              var_name == "Temperature [ITS-90, deg C]" ~ "phys",
                              var_name == "TA [µmol/kg]" ~ "chem",
                              var_name == "pHT in situ" ~ "chem",
                              var_name == "EP TA [µmol/kg]" ~ "chem",
                              var_name == "pHT in situ (temperature correction using EP TA)" ~ "chem"),
         date_accessed = as.Date("2021-04-14")) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)

## CO2 station at IsA
is_CO2_IsA <- read_csv("~/pCloudDrive/FACE-IT_data/isfjorden/Marine_CO2_system_data_at_the_IsA_Station_2015_to_2017.csv") %>% 
  dplyr::rename(lon = `Longitude [°]`, lat = `Latitude [°]`, depth = `CTD Pressure [dbar]`) %>% 
  mutate(date = as.Date(paste0(Year,"-",Month,"-",Day)), .keep = "unused") %>% 
  dplyr::select(lon, lat, date, depth, `Temperature [ITS-90, deg C]`:`pHT in situ (temperature correction using EP TA)`) %>% 
  pivot_longer(`Temperature [ITS-90, deg C]`:`pHT in situ (temperature correction using EP TA)`, names_to = "var_name", values_to = "value") %>% 
  mutate(URL = "https://sios-svalbard.org/metsis/search?fulltext=isfjorden&start_date=&end_date=&is_parent=All",
         citation = NA,
         var_type = case_when(var_name == "Salinity [PSU]" ~ "phys",
                              var_name == "Temperature [ITS-90, deg C]" ~ "phys",
                              var_name == "TA [µmol/kg]" ~ "chem",
                              var_name == "pHT in situ" ~ "chem",
                              var_name == "EP TA [µmol/kg]" ~ "chem",
                              var_name == "pHT in situ (temperature correction using EP TA)" ~ "chem"),
         date_accessed = as.Date("2021-04-14")) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)

## Chlorophyl station at IsA
# tidync("~/pCloudDrive/FACE-IT_data/isfjorden/chl_a/IsA_Svalbard_Chlorophyll_A_2011_2019_GFF.nc")
# as.data.frame(ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/chl_a/IsA_Svalbard_Chlorophyll_A_2011_2019_GFF.nc")$attribute$global)
is_Chla_IsA_units <- rbind(ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/chl_a/IsA_Svalbard_Chlorophyll_A_2011_2019_10um.nc")$variable,
                           ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/chl_a/IsA_Svalbard_Chlorophyll_A_2011_2019_GFF.nc")$variable) %>% distinct()
is_Chla_IsA_1 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/chl_a/IsA_Svalbard_Chlorophyll_A_2011_2019_10um.nc")) %>% mutate(data = "10um")
is_Chla_IsA_2 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/chl_a/IsA_Svalbard_Chlorophyll_A_2011_2019_GFF.nc")) %>% mutate(data = "GFF")
is_Chla_IsA <- rbind(is_Chla_IsA_1, is_Chla_IsA_2) %>% 
  dplyr::rename(depth = Depth) %>% 
  mutate(date = as.Date(`Days since 1st jan 2011`, origin = "2011-01-01"), .keep = "unused") %>% 
  pivot_longer(`Chlorophyll A`:Phaeophytin, names_to = "var_name", values_to = "value") %>% 
  left_join(is_Chla_IsA_units, by = c("var_name" = "name")) %>% 
  mutate(URL = "https://sios-svalbard.org/metsis/search?fulltext=isfjorden&start_date=&end_date=&is_parent=All",
         citation = NA,
         lon = 15.52992, lat = 78.26105,
         var_name = paste0(var_name," - ",data," [", units,"]"),
         var_type = "bio",
         date_accessed = as.Date("2021-04-16")) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)
rm(is_Chla_IsA_units, is_Chla_IsA_1, is_Chla_IsA_2); gc()

## Isfjord radio meteorological station
# ncdf4::nc_open("~/pCloudDrive/FACE-IT_data/isfjorden/SN99790.nc")
is_met_radio <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/SN99790.nc")) %>% 
  mutate(across(everything(), ~replace(., . == 9969209968386869046778552952102584320, NA)),
         date = as.Date(as.POSIXct(time, origin = "1970-01-01"))) %>% 
  pivot_longer(air_temperature_2m:air_pressure_at_sea_level_qnh, names_to = "var_name", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(URL = "https://thredds.met.no/thredds/catalog/met.no/observations/stations/catalog.html?dataset=met.no/observations/stations/SN99790.nc",
         citation = NA, depth = NA,
         lon = 13.619200, lat = 78.062500,
         units = case_when(var_name == "relative_humidity" ~ "1",
                           var_name == "surface_air_pressure_2m" ~ "Pa",
                           var_name == "air_temperature_2m" ~ "K",
                           var_name == "wind_from_direction_10m" ~ "°",
                           var_name == "wind_speed_10m" ~ "m s-1",
                           var_name == "air_pressure_at_sea_level" ~ "Pa",
                           var_name == "air_pressure_at_sea_level_qnh" ~ "hPa"),
         var_name = paste0(var_name," [", units,"]"),
         var_type = "phys",
         date_accessed = as.Date("2021-04-14")) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)

## Airport meteorological station
# ncdf4::nc_open("~/pCloudDrive/FACE-IT_data/isfjorden/SN99840.nc")
is_met_airport <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/SN99840.nc")) %>% 
  mutate(across(everything(), ~replace(., . == 9969209968386869046778552952102584320, NA)),
         date = as.Date(as.POSIXct(time, origin = "1970-01-01"))) %>% 
  pivot_longer(air_temperature_2m:air_pressure_at_sea_level_qnh, names_to = "var_name", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(URL = "https://thredds.met.no/thredds/catalog/met.no/observations/stations/catalog.html?dataset=met.no/observations/stations/SN99840.nc",
         citation = NA, depth = NA,
         lon = 15.501500, lat = 78.245300,
         units = case_when(var_name == "relative_humidity" ~ "1",
                           var_name == "surface_air_pressure_2m" ~ "Pa",
                           var_name == "air_temperature_2m" ~ "K",
                           var_name == "wind_from_direction_10m" ~ "°",
                           var_name == "wind_speed_10m" ~ "m s-1",
                           var_name == "air_pressure_at_sea_level" ~ "Pa",
                           var_name == "air_pressure_at_sea_level_qnh" ~ "hPa"),
         var_name = paste0(var_name," [", units,"]"),
         var_type = "phys",
         date_accessed = as.Date("2021-08-04")) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)

## Pyramiden radio meteorological station
# ncdf4::nc_open("~/pCloudDrive/FACE-IT_data/isfjorden/SN99880.nc")
is_met_pyramiden <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/SN99880.nc")) %>% 
  mutate(across(everything(), ~replace(., . == 9969209968386869046778552952102584320, NA)),
         date = as.Date(as.POSIXct(time, origin = "1970-01-01"))) %>% 
  pivot_longer(air_temperature_2m:air_pressure_at_sea_level_qnh, names_to = "var_name", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(URL = "https://thredds.met.no/thredds/catalog/met.no/observations/stations/catalog.html?dataset=met.no/observations/stations/SN99880.nc",
         citation = NA, depth = NA,
         lon = 16.360300, lat = 78.655700,
         units = case_when(var_name == "relative_humidity" ~ "1",
                           var_name == "surface_air_pressure_2m" ~ "Pa",
                           var_name == "air_temperature_2m" ~ "K",
                           var_name == "wind_from_direction_10m" ~ "°",
                           var_name == "wind_speed_10m" ~ "m s-1",
                           var_name == "air_pressure_at_sea_level" ~ "Pa",
                           var_name == "air_pressure_at_sea_level_qnh" ~ "hPa"),
         var_name = paste0(var_name," [", units,"]"),
         var_type = "phys",
         date_accessed = as.Date("2021-08-04")) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)

# Combine and save
full_product_is <- rbind(pg_is_ALL, is_mooring_N, is_mooring_S, is_mooring_IFO, is_mooring_GFI_N, is_mooring_GFI_S,
                         is_CO2_tempelfjorden, is_CO2_IsA, is_Chla_IsA, is_met_airport, is_met_pyramiden)
data.table::fwrite(full_product_is, "~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.csv")
save(full_product_is, file = "~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.RData")
rm(list = grep("is_",names(.GlobalEnv),value = TRUE)); gc()


## Test visuals ------------------------------------------------------------

is_phys <- full_product_is %>% 
  filter(var_type == "phys")

is_pH <- full_product_is %>% 
  filter(grepl("pH",var_name))

is_temp <- is_phys %>% 
  filter(grepl("°C",var_name))
is_temp %>% 
  filter(depth >= 0) %>% 
  group_by(date, depth) %>% 
  summarise(value = mean(value, na.rm = T)) %>% 
  ggplot(aes(x = date, y = depth)) +
  geom_point(aes(colour = value)) +
  scale_y_reverse()

is_temp_ph <- left_join(is_temp, is_pH, by = c("date_accessed", "URL", "citation", "lon", "lat", "date", "depth"))
is_temp_ph <- rbind(is_temp, is_pH)
is_temp_ph %>% 
  filter(!is.na(value.y)) %>% 
  ggplot(aes(x = value.x, y = value.y)) +
  geom_point() +
  scale_y_reverse()


# Inglefieldbukta ---------------------------------------------------------

## PG product --------------------------------------------------------------

# Load pg ingle files
system.time(
  pg_ingle_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_ingle)
) # 57 seconds


# Test problem files
# pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
# pg_test <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.778258")

# Remove unneeded columns
pg_ingle_clean <- pg_ingle_sub %>% 
  # Manually remove problematic files - No issues
  # Manually remove problematic columns - No issues
  mutate_all(~na_if(., '')) %>% 
  janitor::remove_empty("cols") %>% 
  # Manage lon/lat columns - No issues
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = NA) %>% 
  # Remove unwanted columns - not needed
  # Finish up
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(7:length(.)), as.numeric) #%>% 
  # janitor::remove_empty("cols")
colnames(pg_ingle_clean)

## Individual category data.frames
# Cryosphere
pg_ingle_Cryosphere <- pg_var_melt(pg_ingle_clean, query_Cryosphere$pg_col_name, "cryo") # empty
# Physical
pg_ingle_Physical <- pg_var_melt(pg_ingle_clean, query_Physical$pg_col_name, "phys")
# Carbonate chemistry
pg_ingle_Chemistry <- pg_var_melt(pg_ingle_clean, query_Chemistry$pg_col_name, "chem") # empty
# Biology
pg_ingle_Biology <- pg_var_melt(pg_ingle_clean, query_Biology$pg_col_name, "bio") # empty
# Social
pg_ingle_Social <- pg_var_melt(pg_ingle_clean, query_Social$pg_col_name, "soc") # empty

# Stack them together
pg_ingle_ALL <- rbind(pg_ingle_Physical)
data.table::fwrite(pg_ingle_ALL, "~/pCloudDrive/FACE-IT_data/inglefieldbukta/pg_ingle_ALL.csv")
save(pg_ingle_ALL, file = "~/pCloudDrive/FACE-IT_data/inglefieldbukta/pg_ingle_ALL.RData")

# Check that all columns were used
colnames(pg_ingle_clean)[!colnames(pg_ingle_clean) %in% unique(pg_ingle_ALL$var_name)]

# Clean up
rm(list = grep("pg_ingle",names(.GlobalEnv),value = TRUE)); gc()


## Full product ------------------------------------------------------------

# Load PG data
load("~/pCloudDrive/FACE-IT_data/inglefieldbukta/pg_ingle_ALL.RData")

# Combine and save
full_product_ingle <- rbind(pg_ingle_ALL)
data.table::fwrite(full_product_ingle, "~/pCloudDrive/FACE-IT_data/inglefieldbukta/full_product_ingle.csv")
save(full_product_ingle, file = "~/pCloudDrive/FACE-IT_data/inglefieldbukta/full_product_ingle.RData")
rm(list = grep("ingle_",names(.GlobalEnv),value = TRUE)); gc()


# Young Sound -------------------------------------------------------------

## PG product --------------------------------------------------------------

# Load pg young files
system.time(
  pg_young_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_young)
) # 57 seconds

# Test problem files
# pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
# pg_test <- pg_dl_proc(pg_doi = pg_young_all$doi[32])

# Remove unneeded columns
pg_young_clean <- pg_young_sub %>% 
  # dplyr::select(contains(c("date", "lon", "lat")), everything()) %>%  # Look at meta columns
  # dplyr::select(contains(c("depth", "press", "bathy", "elev")), everything()) %>%  # Look at depth columns
  # Manually remove problematic files - no need
  # Manually remove problematic columns - no need
  mutate_all(~na_if(., '')) %>% 
  janitor::remove_empty("cols") %>% 
  # Manage lon/lat columns - no need
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = ifelse(date == "", NA, date),
         date = case_when(date == "2002-05" ~ "2002-05-01",
                          is.na(date) & !is.na(Date) ~ as.character(Date),
                          TRUE ~ date),
         date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = case_when(!is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Bathy depth [m]`) ~ as.numeric(`Bathy depth [m]`))) %>%
  mutate(depth = case_when(is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -`Elevation [m a.s.l.]`,
                           TRUE ~ depth)) %>% 
  # dplyr::select(depth, everything())
  # Remove unwanted columns
  dplyr::select(-"Longitude 2", -"Latitude 2",
                -"Date/time end",
                -contains(c("Elevation ", "Depth "))) %>%
  # Finish up
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(7:length(.)), as.numeric) %>% 
  janitor::remove_empty("cols")
colnames(pg_young_clean)

## Individual category data.frames
# Cryosphere
pg_young_Cryosphere <- pg_var_melt(pg_young_clean, query_Cryosphere$pg_col_name, "cryo")
# Physical
pg_young_Physical <- pg_var_melt(pg_young_clean, query_Physical$pg_col_name, "phys")
# Carbonate chemistry
pg_young_Chemistry <- pg_var_melt(pg_young_clean, query_Chemistry$pg_col_name, "chem")
# Biology
pg_young_Biology <- pg_var_melt(pg_young_clean, query_Biology$pg_col_name, "bio")
# Social
pg_young_Social <- pg_var_melt(pg_young_clean, query_Social$pg_col_name, "soc") # empty

# Stack them together
pg_young_ALL <- rbind(pg_young_Cryosphere, pg_young_Physical, pg_young_Chemistry, pg_young_Biology)
data.table::fwrite(pg_young_ALL, "~/pCloudDrive/FACE-IT_data/young_sound/pg_young_ALL.csv")
save(pg_young_ALL, file = "~/pCloudDrive/FACE-IT_data/young_sound/pg_young_ALL.RData")

# Check that all columns were used
colnames(pg_young_clean)[!colnames(pg_young_clean) %in% unique(pg_young_ALL$var_name)]

# Clean up
rm(list = grep("pg_young",names(.GlobalEnv),value = TRUE)); gc()


## Full product ------------------------------------------------------------

# Load PG product
load("~/pCloudDrive/FACE-IT_data/young_sound/pg_young_ALL.RData")

# Combine and save
full_product_young <- rbind(pg_young_ALL)
data.table::fwrite(full_product_young, "~/pCloudDrive/FACE-IT_data/young_sound/full_product_young.csv")
save(full_product_young, file = "~/pCloudDrive/FACE-IT_data/young_sound/full_product_young.RData")
rm(list = grep("young_",names(.GlobalEnv),value = TRUE)); gc()


# Disko Bay ---------------------------------------------------------------

## PG product --------------------------------------------------------------

# Load pg disko files
system.time(
  pg_disko_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_disko)
) # 57 seconds

# Test problem files
# pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
# pg_test <- pg_dl_proc(pg_doi = pg_disko_all$doi[32])
# rev(colnames(pg_disko_clean))

# Remove unneeded columns
pg_disko_clean <- pg_disko_sub %>% 
  # dplyr::select(contains(c("date", "lon", "lat")), everything()) %>%  # Look at meta columns
  # dplyr::select(contains(c("depth", "press", "bathy", "elev")), everything()) %>%  # Look at depth columns
  # Manually remove problematic files- no need
  # Manually remove problematic columns - these problems are dealt with later
  # Switching out '' doesn't work due to an unidentified problem column
  # mutate_all(~na_if(., '')) %>% 
  # janitor::remove_empty("cols")# %>%
  # Manage lon/lat columns - no need
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = as.character(date)) %>%
  mutate(date = ifelse(date == "", NA, date),
         date = case_when(is.na(date) & !is.na(`Date/time start`) ~ as.character(`Date/time start`),
                          is.na(date) & !is.na(Date) ~ as.character(Date),
                          TRUE ~ date),
         date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = case_when(!is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Bathy depth [m]`) ~ as.numeric(`Bathy depth [m]`),
                           !is.na(`Depth [m]`) ~ as.numeric(`Depth [m]`))) %>%
  mutate(depth = case_when(is.na(depth) & !is.na(`Elevation [m]`) ~ -as.numeric(`Elevation [m]`),
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -as.numeric(`Elevation [m a.s.l.]`),
                           is.na(depth) & !is.na(`Surf elev [m]`) ~ -as.numeric(`Surf elev [m]`),
                           is.na(depth) & !is.na(`Elev mean [m a.s.l.]`) ~ -as.numeric(`Elev mean [m a.s.l.]`),
                           TRUE ~ depth)) %>%
  # dplyr::select(depth, everything())
  # Remove unwanted columns
  # dplyr::select(-contains(c("Longitude ", "Latitude ", "Elev "))) %>%
  # Finish up
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(7:length(.)), as.numeric) %>% 
  janitor::remove_empty("cols")
colnames(pg_disko_clean)

## Individual category data.frames
# Cryosphere
pg_disko_Cryosphere <- pg_var_melt(pg_disko_clean, query_Cryosphere$pg_col_name, "cryo")
# Physical
pg_disko_Physical <- pg_var_melt(pg_disko_clean, query_Physical$pg_col_name, "phys")
# Carbonate chemistry
pg_disko_Chemistry <- pg_var_melt(pg_disko_clean, query_Chemistry$pg_col_name, "chem")
# Biology
pg_disko_Biology <- pg_var_melt(pg_disko_clean, query_Biology$pg_col_name, "bio")
# Social
pg_disko_Social <- pg_var_melt(pg_disko_clean, query_Social$pg_col_name, "soc") # empty

# Stack them together
pg_disko_ALL <- rbind(pg_disko_Cryosphere, pg_disko_Physical, pg_disko_Chemistry, pg_disko_Biology)
data.table::fwrite(pg_disko_ALL, "~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_ALL.csv")
save(pg_disko_ALL, file = "~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_ALL.RData")

# Check that all columns were used
colnames(pg_disko_clean)[!colnames(pg_disko_clean) %in% unique(pg_disko_ALL$var_name)]

# Clean up
rm(list = grep("pg_disko",names(.GlobalEnv),value = TRUE)); gc()


## Full product ------------------------------------------------------------

# Load PG product
load("~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_ALL.RData")

# Combine and save
full_product_disko <- rbind(pg_disko_ALL)
data.table::fwrite(full_product_disko, "~/pCloudDrive/FACE-IT_data/disko_bay/full_product_disko.csv")
save(full_product_disko, file = "~/pCloudDrive/FACE-IT_data/disko_bay/full_product_disko.RData")
rm(list = grep("disko_",names(.GlobalEnv),value = TRUE)); gc()

# Nuup Kangerlua ----------------------------------------------------------

## PG product --------------------------------------------------------------

# Load pg young files
system.time(
  pg_nuup_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_nuup)
) # 57 seconds

# Test problem files
# pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
# pg_test <- pg_dl_proc(pg_doi = pg_nuup_all$doi[32])

# Remove unneeded columns
pg_nuup_clean <- pg_nuup_sub %>% 
  # dplyr::select(contains(c("date", "lon", "lat")), everything()) %>%  # Look at meta columns
  # dplyr::select(contains(c("depth", "press", "bathy", "elev")), everything()) %>%  # Look at depth columns
  # Manually remove problematic files - no need
  mutate_all(~na_if(., '')) %>%
  janitor::remove_empty("cols") %>%
  # Manage lon/lat columns - no issues
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = case_when(is.na(date) & !is.na(Date) ~ as.character(Date),
                          date %in% seq(2000, 2009) ~ paste0(date,"-01-01"),
                          # is.na(date) & !is.na(`Sampling date`) ~ `Sampling date`, # There is an issue with these values
                          TRUE ~ date),
         # date = as.character(date),
         date = ifelse(date == "", NA, date),
         date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = case_when(!is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Bathy depth [m]`) ~ as.numeric(`Bathy depth [m]`))) %>%
  mutate(depth = case_when(is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -as.numeric(`Elevation [m a.s.l.]`),
                           is.na(depth) & !is.na(`Surf elev [m]`) ~ -as.numeric(`Surf elev [m]`),
                           is.na(depth) & !is.na(`Elevation [m]`) ~ -as.numeric(`Elevation [m]`),
                           TRUE ~ depth)) %>% 
    # dplyr::select(depth, everything())
  # Remove unwanted columns  # Manually remove problematic columns
  # dplyr::select(-contains(c("Date/", "Depth ", "Elevation ", "Elev ", "Press ", "Longitude ", "Latitude "))) %>%
  # Finish up
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(7:length(.)), as.numeric) %>% 
  janitor::remove_empty("cols")
colnames(pg_nuup_clean)

## Individual category data.frames
# Cryosphere
pg_nuup_Cryosphere <- pg_var_melt(pg_nuup_clean, query_Cryosphere$pg_col_name, "cryo")
# Physical
pg_nuup_Physical <- pg_var_melt(pg_nuup_clean, query_Physical$pg_col_name, "phys")
# Carbonate chemistry
pg_nuup_Chemistry <- pg_var_melt(pg_nuup_clean, query_Chemistry$pg_col_name, "chem")
# Biology
pg_nuup_Biology <- pg_var_melt(pg_nuup_clean, query_Biology$pg_col_name, "bio")
# Social
pg_nuup_Social <- pg_var_melt(pg_nuup_clean, query_Social$pg_col_name, "soc") # empty

# Stack them together
pg_nuup_ALL <- rbind(pg_nuup_Cryosphere, pg_nuup_Physical, pg_nuup_Chemistry, pg_nuup_Biology)
data.table::fwrite(pg_nuup_ALL, "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/pg_nuup_ALL.csv")
save(pg_nuup_ALL, file = "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/pg_nuup_ALL.RData")

# Check that all columns were used
colnames(pg_nuup_clean)[!colnames(pg_nuup_clean) %in% unique(pg_nuup_ALL$var_name)]

# Clean up
rm(list = grep("pg_nuup",names(.GlobalEnv),value = TRUE)); gc()


## Full product ------------------------------------------------------------

# Load PG product
load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/pg_nuup_ALL.RData")

# Combine and save
full_product_nuup <- rbind(pg_nuup_ALL)
data.table::fwrite(full_product_nuup, "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/full_product_nuup.csv")
save(full_product_nuup, file = "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/full_product_nuup.RData")
rm(list = grep("nuup_",names(.GlobalEnv),value = TRUE)); gc()


# Porsangerfjorden --------------------------------------------------------

## PG product --------------------------------------------------------------

# Load pg is files
system.time(
  pg_por_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_por)
) # 57 seconds

# Test problem files
# pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
# pg_test <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.869680")

# Remove unneeded columns
pg_por_clean <- pg_por_sub %>% 
  # dplyr::select(contains(c("date", "lon", "lat")), everything()) %>%  # Look at meta columns
  dplyr::select(contains(c("depth", "press", "bathy", "elev")), everything()) %>%  # Look at depth columns
  # Manually remove problematic files
  # filter(!URL %in% c("https://doi.org/10.1594/PANGAEA.900501", "https://doi.org/10.1594/PANGAEA.786375",
  #                    "https://doi.org/10.1594/PANGAEA.847003","https://doi.org/10.1594/PANGAEA.867207",
  #                    "https://doi.org/10.1594/PANGAEA.867215", "https://doi.org/10.1594/PANGAEA.907926",
  #                    "https://doi.org/10.1594/PANGAEA.869680")) %>% 
  # Manually remove problematic columns
  # dplyr::select(-"Date/Time (The date and time the entry w...)", -"Date/Time (Moscow time)",
                # -"Date/Time (local time)") %>%
  mutate_all(~na_if(., '')) %>% 
  janitor::remove_empty("cols") %>% 
  # Manage lon/lat columns - no need
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = ifelse(date == "", NA, date),
         # mutate(date = case_when(is.na(date) & !is.na(Date) ~ as.character(Date), TRUE ~ date)), # There is an issue with these values
         date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = case_when(!is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Press [dbar]`) ~ as.numeric(`Press [dbar]`))) %>%
  mutate(depth = case_when(is.na(depth) & !is.na(`Elevation [m]`) ~ -as.numeric(`Elevation [m]`),
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -as.numeric(`Elevation [m a.s.l.]`),
                           TRUE ~ depth)) %>% 
    # dplyr::select(depth, everything())
  # Remove unwanted columns
  # dplyr::select(-"Longitude e", -"Latitude e", -"Press [dbar]", -contains(c("Depth ", "Elevation "))) %>%
  # Finish up
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(7:length(.)), as.numeric) %>% 
  janitor::remove_empty("cols")
colnames(pg_por_clean)

## Individual category data.frames
# Cryosphere
pg_por_Cryosphere <- pg_var_melt(pg_por_clean, query_Cryosphere$pg_col_name, "cryo")
# Physical
pg_por_Physical <- pg_var_melt(pg_por_clean, query_Physical$pg_col_name, "phys")
# Carbonate chemistry
pg_por_Chemistry <- pg_var_melt(pg_por_clean, query_Chemistry$pg_col_name, "chem")
# Biology
pg_por_Biology <- pg_var_melt(pg_por_clean, query_Biology$pg_col_name, "bio") # empty
# Social
pg_por_Social <- pg_var_melt(pg_por_clean, query_Social$pg_col_name, "soc") # empty

# Stack them together
pg_por_ALL <- rbind(pg_por_Cryosphere, pg_por_Physical, pg_por_Chemistry)
data.table::fwrite(pg_por_ALL, "~/pCloudDrive/FACE-IT_data/porsangerfjorden/pg_por_ALL.csv")
save(pg_por_ALL, file = "~/pCloudDrive/FACE-IT_data/porsangerfjorden/pg_por_ALL.RData")

# Check that all columns were used
colnames(pg_por_clean)[!colnames(pg_por_clean) %in% unique(pg_por_ALL$var_name)]

# Clean up
rm(list = grep("pg_por",names(.GlobalEnv),value = TRUE)); gc()

## Full product ------------------------------------------------------------

# Load PG product
load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/pg_por_ALL.RData")

# Combine and save
full_product_por <- rbind(pg_por_ALL)
data.table::fwrite(full_product_por, "~/pCloudDrive/FACE-IT_data/porsangerfjorden/full_product_por.csv")
save(full_product_por, file = "~/pCloudDrive/FACE-IT_data/porsangerfjorden/full_product_por.RData")
rm(list = grep("por_",names(.GlobalEnv),value = TRUE)); gc()


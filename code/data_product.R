# code/data_product.R
# This script houses the code used to create data products from the many disparate files

# TODO: Investigate PG columns that should be numeric but aren't
# e.g. `[NO3]- [µmol/l]` for pg_nuup_clean
# Provide lists of variables that were removed


# Setup -------------------------------------------------------------------

# Libraries
source("code/functions.R")
source("code/key_drivers.R")
library(tidync)
library(stringi)
library(raster)
library(circular) # For calculating mean daily wind direction from degree values

# Re-run full data collection pipeline
# system.time(
# source("code/data_collection.R")
# )

# PANGAEA files
pg_files <- dir("data/pg_data", pattern = "pg_", full.names = T)

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
# This file is loaded to get a summary of the data
pg_EU_files <- dir("data/pg_data", pattern = "pg_EU", full.names = T)
system.time(
  pg_EU <- plyr::ldply(pg_EU_files, pg_quick_filter, bbox = bbox_EU)
) # 70 seconds
length(unique(pg_EU$citation))
rm(pg_EU); gc()


## Full product ------------------------------------------------------------

# No products are currently planned to be made for the EU Arctic
# Rather access the existing files directly: ~/pCloud/EU_Arctic/...
# Though now thinking about it perhaps it would be better to create a combined product
# from which the site specific products may draw data

# Bathymetry data
# EU_GEBCO # Not on pCloud

# Zooplankton biodiversity
# 1995-2008-zooplankton-biodiversity.tsv
# EU_zooplankton

# Ice modelling output
# EU_ice # Not on pCloud

# CTD data from YMER cruise in 1980
# 77YM19800811.exc.csv
# EU_YMER

# CTD data for Arctic
# Codispoti_Arctic_Nutrients_Submission_11-11-2010.csv
# EU_Codispoti

# Ice core samples for protist presence
# protists\
# EU_protists

# CTD data from NCEI Accession 9700302
# 9700302.2.2.tar.gz # This appears to be some sort of proprietary data format...
# EU_NCEI_1989

# GRDC river discharge data
# grdc_arctichycos_stations.xlsx
# EU_GRDC

# Carb chem Arctic model output
# Outputs are stored at the Centre for Environmental Data Analysis's (CEDA) JASMIN servers:/gws/nopw/j04/nemo_vol2/ROAM. 
# JASMIN's web address is https://www.jasmin.ac.uk and its access point https://www.jasmin.ac.uk/users/access/. 
# Note that non-UK users may need to demonstrate association with NERC, ESA, the EC or a non-profit.  
EU_Popova <- read_delim("~/pCloudDrive/FACE-IT_data/EU_arctic/Arctic_model_output_acid.dat", 
                        delim = " ", col_names = c("Year", "SST", "ice_extent_March", 
                                                   "ice_extent_September", "MLD", "DIC", "pH"))

# SOCAT data
EU_SOCAT <- read_rds("~/pCloudDrive/FACE-IT_data/socat/SOCATv2021.rds") %>%  
  dplyr::rename(lon = `longitude [dec.deg.E]`, lat = `latitude [dec.deg.N]`,
                depth = `sample_depth [m]`, value = `pCO2water_SST_wet [uatm]`) %>% 
  filter(lat >= 63, value >= 0) %>% 
  mutate(lon = case_when(lon >= 180 ~ lon-360, TRUE ~ lon)) %>% 
  filter(lon <= 60, lon >= -60) %>% 
  unite(yr, mon, day, sep = "-", remove = T, col = "date") %>% 
  mutate(date = as.Date(date),
         var_name = "pCO2water_SST_wet [uatm]",
         var_type = "chem",
         date_accessed = as.Date("2021-08-06"),
         URL = "https://www.socat.info",
         citation = "Bakker, D. C. E., Pfeil, B. Landa, C. S., Metzl, N., O’Brien, K. M., Olsen, A., Smith, K., Cosca, C., Harasawa, S., Jones, S. D., Nakaoka, S., Nojiri, Y., Schuster, U., Steinhoff, T., Sweeney, C., Takahashi, T., Tilbrook, B., Wada, C., Wanninkhof, R., Alin, S. R., Balestrini, C. F., Barbero, L., Bates, N. R., Bianchi, A. A., Bonou, F., Boutin, J., Bozec, Y., Burger, E. F., Cai, W.-J., Castle, R. D., Chen, L., Chierici, M., Currie, K., Evans, W., Featherstone, C., Feely, R. A., Fransson, A., Goyet, C., Greenwood, N., Gregor, L., Hankin, S., Hardman-Mountford, N. J., Harlay, J., Hauck, J., Hoppema, M., Humphreys, M. P., Hunt, C. W., Huss, B., Ibánhez, J. S. P., Johannessen, T., Keeling, R., Kitidis, V., Körtzinger, A., Kozyr, A., Krasakopoulou, E., Kuwata, A., Landschützer, P., Lauvset, S. K., Lefèvre, N., Lo Monaco, C., Manke, A., Mathis, J. T., Merlivat, L., Millero, F. J., Monteiro, P. M. S., Munro, D. R., Murata, A., Newberger, T., Omar, A. M., Ono, T., Paterson, K., Pearce, D., Pierrot, D., Robbins, L. L., Saito, S., Salisbury, J., Schlitzer, R., Schneider, B., Schweitzer, R., Sieger, R., Skjelvan, I., Sullivan, K. F., Sutherland, S. C., Sutton, A. J., Tadokoro, K., Telszewski, M., Tuma, M., Van Heuven, S. M. A. C., Vandemark, D., Ward, B., Watson, A. J., Xu, S. (2016) A multi-decade record of high quality fCO2 data in version 3 of the Surface Ocean CO2 Atlas (SOCAT). Earth System Science Data 8: 383-413. doi:10.5194/essd-8-383-2016.") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)
save(EU_SOCAT, file = "~/pCloudDrive/FACE-IT_data/EU_arctic/SOCAT_EU.RData")
# load("~/pCloudDrive/FACE-IT_data/EU_arctic/SOCAT_EU.RData")

# GLODAP data
EU_GLODAP <- read_rds("~/pCloudDrive/FACE-IT_data/glodap/GLODAP_bottle.rds") %>% 
  `colnames<-`(gsub("G2","",colnames(.))) %>% 
  dplyr::rename(lon = longitude, lat = latitude) %>% 
  filter(lon <= 60, lon >= -60, lat >= 63) %>% 
  unite(year, month, day, sep = "-", remove = T, col = "date") %>% 
  mutate(date = as.Date(date)) %>% 
  # NB: The counting error columns were removed here. As well as all flag and QC columns.
  dplyr::select(lon, lat, date, depth, temperature, theta, salinity, oxygen, aou, nitrate, nitrite, silicate, 
                phosphate, tco2, talk, fco2, fco2temp, phts25p0, phtsinsitutp, cfc11, pcfc11, cfc12, pcfc12, 
                cfc113, pcfc113, ccl4, pccl4, sf6, psf6, c13, c14, h3, he3, he, neon, o18, toc, doc, don, tdn, chla) %>% 
  pivot_longer(temperature:chla, names_to = "var_name", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(var_type = case_when(var_name %in% c("temperature", "theta", "salinity") ~ "phys", TRUE ~ "chem"),
         var_name = case_when(var_name %in% c("temperature", "theta", "fco2temp") ~ paste0(var_name," [°C]"),
                              var_name %in% c("oxygen", "aou", "nitrate", "nitrite", "silicate", 
                                              "phosphate", "tco2", "talk") ~ paste0(var_name," [μmol kg-1]"),
                              var_name %in% c("fco2") ~ paste0(var_name," [μatm]"),
                              var_name %in% c("cfc11", "cfc12", "cfc113", "ccl4") ~ paste0(var_name," [pmol kg-1]"),
                              var_name %in% c("sf6") ~ paste0(var_name," [fmol kg-1]"),
                              var_name %in% c("pcfc11", "pcfc12", "pcfc113", "pccl4", "psf6") ~ paste0(var_name," [ppt]"),
                              var_name %in% c("c13", "c14", "o18") ~ paste0(var_name," [‰]"),
                              var_name %in% c("h3") ~ paste0(var_name," [TU]"),
                              var_name %in% c("he3") ~ paste0(var_name," [%]"),
                              var_name %in% c("he", "neon") ~ paste0(var_name," [nmol kg-1]"),
                              var_name %in% c("toc", "doc", "don", "tdn") ~ paste0(var_name," [μmol L-1 d]"),
                              var_name %in% c("chla") ~ paste0(var_name," [μg kg-1 d]"),
                              TRUE ~ var_name),
         date_accessed = as.Date("2021-08-06"),
         URL = "https://www.glodap.info",
         citation = "Olsen, A., R. M. Key, S. van Heuven, S. K. Lauvset, A. Velo, X. Lin, C. Schirnick, A. Kozyr, T. Tanhua, M. Hoppema, S. Jutterström, R. Steinfeldt, E. Jeansson, M. Ishii, F. F. Pérez and T. Suzuki. The Global Ocean Data Analysis Project version 2 (GLODAPv2) – an internally consistent data product for the world ocean, Earth Syst. Sci. Data, 8, 297–323, 2016, doi:10.5194/essd-8-297-2016
         Key, R.M., A. Olsen, S. van Heuven, S. K. Lauvset, A. Velo, X. Lin, C. Schirnick, A. Kozyr, T. Tanhua, M. Hoppema, S. Jutterström, R. Steinfeldt, E. Jeansson, M. Ishi, F. F. Perez, and T. Suzuki. 2015. Global Ocean Data Analysis Project, Version 2 (GLODAPv2), ORNL/CDIAC-162, ND-P093. Carbon Dioxide Information Analysis Center, Oak Ridge National Laboratory, US Department of Energy, Oak Ridge, Tennessee. doi:10.3334/CDIAC/OTG.NDP093_GLODAPv2") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)
save(EU_GLODAP, file = "~/pCloudDrive/FACE-IT_data/EU_arctic/GLODAP_EU.RData")
# load("~/pCloudDrive/FACE-IT_data/EU_arctic/GLODAP_EU.RData")

# Greenland fjord CTD casts
EU_green_fjords <- read_csv("~/pCloudDrive/FACE-IT_data/EU_arctic/LAKO_2018_SBE25_CTD_profiles.csv") %>% 
  mutate(date = as.Date(paste0(year,"-",month,"-",day)))

# Combine and save
# full_product_EU <- rbind()
# data.table::fwrite(full_product_EU, "~/pCloudDrive/FACE-IT_data/EU_arctic/full_product_EU.csv")
# save(full_product_EU, file = "~/pCloudDrive/FACE-IT_data/EU_arctic/full_product_EU.RData")
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
# sval_glacier_mass

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
# sval_surface_met

# UNIS database
## NB: This is really slow due to file size
# ncdf4::nc_open("~/pCloudDrive/FACE-IT_data/svalbard/CTD_all_1876-2019.nc")
sval_UNIS_nc_dat <- ncdf4::nc_open("~/pCloudDrive/FACE-IT_data/svalbard/CTD_all_1876-2019.nc")
# sval_UNIS_OWNER <- distinct(data.frame(ncdf4::ncvar_get(sval_UNIS_nc_dat, varid = "OWNER")))
# sval_UNIS_CRUISE <- distinct(data.frame(ncdf4::ncvar_get(sval_UNIS_nc_dat, varid = "CRUISE")))
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
# Look at UNIS contributors

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

# Svalbard population counts
sval_pop <- read_delim("~/pCloudDrive/FACE-IT_data/svalbard/svalbard_population_stats.csv", delim = "\t") %>% 
  dplyr::select(-contents) %>% 
  rename_at(.vars = vars(ends_with("H1")), .funs = list(~gsub("H1", "", .))) %>% 
  pivot_longer(`1990`:`2021`, names_to = "year", values_to = "value") %>% 
  mutate(settlement = case_when(grepl("Resident", settlement) ~ "Longyearbyen & Ny-Alesund mainland",
                                grepl("abroad", settlement) ~ "Longyearbyen & Ny-Alesund abroad",
                                TRUE ~ settlement)) %>% 
  mutate(date = as.Date(paste0(year,"-01-01")),
         date_accessed = as.Date("2021-09-29"),
         URL = "https://www.ssb.no/en/befolkning/folketall/statistikk/befolkningen-pa-svalbard",
         citation = "Statistics Norway. www.ssb.no. Accessed 2021-09-29",
         var_type = "soc", var_name = paste0("pop [",settlement,"]"),
         depth = NA, lon = NA, lat = NA, .keep = "unused") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)
write_csv(sval_pop, "~/pCloudDrive/FACE-IT_data/svalbard/svalbard_population_stats_full.csv")

# Svalbard tourist arrivals
# NB: The historic camping data are only available per year
# So the monthly data are averaged to years to be the same
sval_tour_arrival_hist <- read_csv("~/pCloudDrive/FACE-IT_data/svalbard/svalbard_tourist_arrivals_historic.csv") %>% 
  dplyr::rename(May = Mai, Oct = Okt, Dec = Des) %>%
  pivot_longer(Jan:Dec) %>% 
  mutate(month = match(name, month.abb),
         type = "Hotels and similar establishments",
         residence = "Total") %>% 
  mutate(date = as.Date(paste0(Year,"-",month,"-01")), .keep = "unused") %>% 
  dplyr::select(type, residence, date, value) %>% 
  # manually copied from: https://en.visitsvalbard.com/dbimgs/StatistikkfraVisitSvalbardASper2018forweb.pdf
  rbind(data.frame(type = "Camping sites (annual)",
                   residence = "Total",
                   date = as.Date(c("2016-12-31", "2017-12-31", "2018-12-31")),
                   value = c(862, 779, 612))) %>% 
  mutate(URL = "https://en.visitsvalbard.com/dbimgs/StatistikkfraVisitSvalbardASper2018forweb.pdf")
sval_tour_arrival <- read_delim("~/pCloudDrive/FACE-IT_data/svalbard/svalbard_tourist_arrivals.csv", delim = "\t") %>% 
  dplyr::rename(type = `type of accommodation`, residence = `country of residence`) %>% 
  pivot_longer(`2020M01`:`2021M07`) %>% # NB: This will expand as the months go by
  separate(name, into = c("year", "month"), sep = "M") %>% 
  mutate(date = as.Date(paste0(year,"-",month,"-01")),
         URL = "https://www.ssb.no/en/statbank/table/12896", .keep = "unused") %>% 
  # group_by(URL, type, residence, year) %>% 
  # summarise(value = sum(value, na.rm = T), .groups = "drop") %>% 
  bind_rows(sval_tour_arrival_hist) %>% 
  mutate(date_accessed = as.Date("2021-09-30"),
         citation = "Statistics Norway. www.ssb.no. Accessed 2021-09-30",
         var_type = "soc", var_name = paste0("arrival [",type," - ",residence,"]"),
         depth = NA, lon = NA, lat = NA, .keep = "unused") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)
write_csv(sval_tour_arrival, "~/pCloudDrive/FACE-IT_data/svalbard/svalbard_tourist_arrivals_full.csv")

# Svalbard guest nights
sval_guest_night_hist <- read_csv("~/pCloudDrive/FACE-IT_data/svalbard/svalbard_guest_nights_historic.csv") %>% 
  dplyr::rename(May = Mai, Oct = Okt, Dec = Des) %>%
  pivot_longer(Jan:Dec) %>% 
  mutate(month = match(name, month.abb),
         type = "Hotels and similar establishments",
         residence = "Total") %>% 
  mutate(date = as.Date(paste0(Year,"-",month,"-01")), .keep = "unused") %>% 
  dplyr::select(type, residence, date, value) %>% 
  # manually copied from: https://en.visitsvalbard.com/dbimgs/StatistikkfraVisitSvalbardASper2018forweb.pdf
  rbind(data.frame(type = "Camping sites (annual)",
                   residence = "Total",
                   date = as.Date(c("2016-12-31", "2017-12-31", "2018-12-31")),
                   value = c(2778, 2323, 2007))) %>% 
  mutate(URL = "https://en.visitsvalbard.com/dbimgs/StatistikkfraVisitSvalbardASper2018forweb.pdf")
sval_guest_night <- read_delim("~/pCloudDrive/FACE-IT_data/svalbard/svalbard_guest_nights.csv", delim = "\t", na = ".") %>% 
  dplyr::rename(type = `type of accommodation`, residence = `country of residence`) %>% 
  pivot_longer(`2019M01`:`2021M08`) %>% # NB: This will expand as the months go by
  separate(name, into = c("year", "month"), sep = "M") %>% 
  mutate(date = as.Date(paste0(year,"-",month,"-01")),
         URL = "https://www.ssb.no/en/statbank/table/12892", .keep = "unused") %>% 
  # group_by(URL, type, residence, date) %>% 
  # summarise(value = sum(value, na.rm = T), .groups = "drop") %>% 
  bind_rows(sval_guest_night_hist) %>% 
  mutate(date_accessed = as.Date("2021-09-30"),
         citation = "Statistics Norway. www.ssb.no. Accessed 2021-09-30",
         var_type = "soc", var_name = paste0("guest night [",type," - ",residence,"]"),
         depth = NA, lon = NA, lat = NA, .keep = "unused") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)
write_csv(sval_guest_night, "~/pCloudDrive/FACE-IT_data/svalbard/svalbard_guest_nights_full.csv")

# AIS data
sval_AIS <- read_csv("~/pCloudDrive/FACE-IT_data/svalbard/AIS_aggregated.csv") %>% 
  pivot_longer(`Nautical miles`:`Average speed (knots)`, names_to = "var", values_to = "value") %>% 
  mutate(date = as.Date(paste0(Year,"-12-31")),
         depth = 0, # It may be better to list this as NA 
         lon = NA, lat = NA, 
         date_accessed = as.Date("2020-09-30"),
         var_name = paste0(Area," [",var,"]"),
         var_type = case_when(grepl("co2|nox|sox", var_name, ignore.case = T) ~ "chem",
                              grepl("PM", var_name, ignore.case = T) ~ "phys", TRUE ~ "soc"),
         URL = "Received directly from Morten Simonsen",
         citation = "Simonsen, M., Walnum, H. J., & Gössling, S. (2018). Model for estimation of fuel consumption of cruise ships. Energies, 11(5), 1059.") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)

## SOCAT
### NB: EU_SOCAT loaded in EU full product section
sval_SOCAT <- EU_SOCAT %>% 
  filter(lon >= bbox_sval[1], lon <= bbox_sval[2],
         lat >= bbox_sval[3], lat <= bbox_sval[4])
save(sval_SOCAT, file = "~/pCloudDrive/FACE-IT_data/svalbard/SOCAT_sval.RData")

## GLODAP
### NB: EU_GLODAP loaded in EU full product section
sval_GLODAP <- EU_GLODAP %>% 
  filter(lon >= bbox_sval[1], lon <= bbox_sval[2],
         lat >= bbox_sval[3], lat <= bbox_sval[4])
save(sval_GLODAP, file = "~/pCloudDrive/FACE-IT_data/svalbard/GLODAP_sval.RData")

# Combine and save
full_product_sval <- rbind(sval_UNIS_database, sval_biogeochemistry, sval_pop, sval_tour_arrival, sval_guest_night, 
                           sval_AIS, sval_SOCAT, sval_GLODAP) %>% distinct()
data.table::fwrite(full_product_sval, "~/pCloudDrive/FACE-IT_data/svalbard/full_product_sval.csv")
save(full_product_sval, file = "~/pCloudDrive/FACE-IT_data/svalbard/full_product_sval.RData")
save(full_product_sval, file = "data/full_data/full_product_sval.RData")
rm(list = grep("sval_",names(.GlobalEnv),value = TRUE)); gc()


# Kongsfjorden ------------------------------------------------------------

## PG product --------------------------------------------------------------

# Load pg kong files
system.time(
  pg_kong_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_kong)
) # 70 seconds

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
                -"Date/time start", -"Date/time end",
                -"Press [dbar]",
                -contains(c("Depth ", "Elev ", "Elevation "))) %>%
  # Finish up
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(7:length(.)), as.numeric) %>% 
  janitor::remove_empty("cols")
# colnames(pg_kong_clean)

## Individual category data.frames
# Cryosphere
pg_kong_Cryosphere <- pg_var_melt(pg_kong_clean, query_Cryosphere$pg_col_name, "cryo")
# Physical
pg_kong_Physical <- pg_var_melt(pg_kong_clean, query_Physical$pg_col_name, "phys")
# Carbonate chemistry
pg_kong_Chemistry <- pg_var_melt(pg_kong_clean, query_Chemistry$pg_col_name, "chem")
# Biology
# pg_kong_Biology <- pg_var_melt(pg_kong_clean, query_Biology$pg_col_name, "bio") # 0 values
# Social
# pg_kong_Social <- pg_var_melt(pg_kong_clean, query_Social$pg_col_name, "soc") # 0 values

# Stack them together
pg_kong_ALL <- rbind(pg_kong_Cryosphere, pg_kong_Physical, pg_kong_Chemistry)
data.table::fwrite(pg_kong_ALL, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_ALL.csv")
save(pg_kong_ALL, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_ALL.RData")

# Load data to investigate
# pg_kong_ALL <- data.table::fread("~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_ALL.csv")

# Check that all columns were used
# colnames(pg_kong_clean)[!colnames(pg_kong_clean) %in% unique(pg_kong_ALL$var_name)]

# Clean up
rm(list = grep("pg_kong",names(.GlobalEnv),value = TRUE)); gc()


## Full product ------------------------------------------------------------

# Load full Svalbard file
if(!exists("full_product_sval")) load("data/full_data/full_product_sval.RData")

# Load PG file
if(!exists("pg_kong_ALL")) load("~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_ALL.RData")

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
kong_zoo_data <- read_csv("~/pCloudDrive/FACE-IT_data/kongsfjorden/kf_zooplankton_abundance_data.csv") %>% 
  pivot_longer(CALfinM:SCYPZlar, names_to = "sps", values_to = "value") %>%
  dplyr::rename("id" = "...1") %>% 
  left_join(read_csv("~/pCloudDrive/FACE-IT_data/kongsfjorden/kf_zooplankton_sampling_meta.csv"), by = c("id")) %>% 
  left_join(read_csv("~/pCloudDrive/FACE-IT_data/kongsfjorden/kf_zooplankton_species_meta.csv"), by = c("sps" = "id")) %>% 
  dplyr::rename(lon = longitude, lat = latitude) %>% 
  mutate(value = value*biomass_conv, # Need to check that this conversion is correct
         var_name = case_when(!is.na(stage) ~ paste0(species," [",stage,"]"), TRUE ~ species),
         var_type = "bio",
         date_accessed = as.Date("2021-02-11"),
         URL = "https://data.npolar.no/dataset/94b29b16-b03b-47d7-bfbc-1c3c4f7060d2",
         citation = "Hop H, Wold A, Vihtakari M, Daase M, Kwasniewski S, Gluchowska M, Lischka S, Buchholz F, Falk-Petersen S (2019) Zooplankton in Kongsfjorden (1996-2016) in relation to climate change. In: The ecosystem of Kongsfjorden, Svalbard (eds. Hop H, Wiencke C), Advances in Polar Ecology, Springer Verlag.") %>% 
  filter(!is.na(value)) %>%
  group_by(date_accessed, URL, citation, lon, lat, date, var_type, var_name, value) %>% 
  summarise(depth = (from+to)/2, .groups = "drop") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop")

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
  distinct() %>%
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
         date = as.Date(date),
         depth = as.numeric(depth),
         units = case_when(var_name == "temp" ~ "°C",
                           var_name == "psal" ~ "1e-3",
                           var_name == "cndc" ~ "S m-1"),
         var_name = paste0(var_name," [", units,"]"),
         var_type = "phys",
         date_accessed = as.Date("2021-02-11")) %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop")
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
## NB: Not included in final project as we aren't including grided(ish) data
# Code deleted for tidiness on 2021-10-12

## Kongsvegen weather station
kong_weather_station <- read_delim("~/pCloudDrive/FACE-IT_data/kongsfjorden/Kongsvegen-weather.tsv", delim = "\t", na = "null") %>% 
  mutate(date = as.Date(timestamp), .keep = "unused") %>% 
  group_by(date) %>% 
  summarise(sw_out_wpm2_avg = mean(sw_out_wpm2_avg, na.rm = T),
            at_2_avg = mean(at_2_avg, na.rm = T),
            sw_in_wpm2_avg = mean(sw_in_wpm2_avg, na.rm = T),
            lw_in_corr_wpm2_avg = mean(lw_in_corr_wpm2_avg, na.rm = T),
            rh_2_avg = mean(rh_2_avg, na.rm = T),
            lw_out_corr_wpm2_avg = mean(lw_out_corr_wpm2_avg, na.rm = T),
            ws_2_wvc1 = mean(ws_2_wvc1, na.rm = T),
            ws_2_wvc2 = as.numeric(round(mean.circular(circular(ws_2_wvc2, units = "degrees"), na.rm = T)))) %>% 
  mutate(ws_2_wvc2 = case_when(ws_2_wvc2 < 0 ~ ws_2_wvc2 + 360, TRUE ~ ws_2_wvc2)) %>% 
  pivot_longer(sw_out_wpm2_avg:ws_2_wvc1, names_to = "var_name", values_to = "value") %>% 
  mutate(lon = 13.15, lat = 78.78, depth = NA, 
         var_type =  "phys",
         date_accessed = as.Date("2021-03-02"),
         URL = "https://data.npolar.no/dataset/5dc31930-0922-4483-a1df-6f48af9e371b",
         citation = "Kohler, J., Hudson, S. R., & Obleitner, F. (2017). Automatic weather station data from Kongsvegen, Ny-Ålesund [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2017.5dc31930") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)

## GFI mooring
kong_mooring_GFI <- plyr::ldply(dir("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI", full.names = T), load_GFI, .parallel = T) %>% 
  mutate(date_accessed = as.Date("2021-08-04"), .before = 1)

## Ferry box data
kong_ferry <- readRDS("~/pCloudDrive/FACE-IT_data/kongsfjorden/d_all.rds") %>%
  dplyr::select(date, alp:at_calc, co2_air:depth, diff_sal_fb_insitu_9m:k600, nh4:ws) %>% 
  pivot_longer(cols = c(-"date", -"depth"), names_to = "var_name", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(lon = 11.920027777777777, lat = 78.93065833333334,
         var_type = case_when(grepl("co2|ph_|phint|phEXT", var_name, ignore.case = T) ~ "chem",
                              var_name %in% c("at", "at_calc", "nh4", "NH4", "NO2","no3", "NO3", 
                                              "no3no2", "NO3NO2", "po4", "PO4", "si", "Si", "k",
                                              "ta_inst") ~ "chem",
                              TRUE ~ "phys"), 
         URL = "File provided by Jean-Pierre Gattuso",
         date_accessed = as.Date("2021-08-12"),
         citation = "Gattuso, J.-P., Alliouane S., Fischer P. & Gattuso J.-P., in prep. Multiyear, high-frequency time series of the carbonate system in a coastal high Arctic station (Spitsbergen)") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)

## SAMS mooring data
kong_mooring_SAMS <- plyr::ldply(dir("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_SAMS/", full.names = T), load_SAMS, .parallel = T) %>% 
  mutate(date_accessed = as.Date("2021-10-21"), .before = 1)

## Ny-Alesund ship arrivals
kong_ship_arrivals <- read_csv("~/pCloudDrive/FACE-IT_data/kongsfjorden/kong_ship_arrivals.csv") %>% 
  pivot_longer(January:December, names_to = "month", values_to = "value") %>% 
  mutate(var_name = case_when(type == "PAX" ~ "Tourist arrivals [count]",
                              type != "PAX" ~ paste0("Vessels [",type,"]")),
         var_type = "soc",
         month = match(month, month.name),
         date = as.Date(paste0(year,"-",month,"-01")),
         depth = NA, lon = 11.92, lat = 78.93,
         URL = "https://port.kingsbay.no/statistics/",
         date_accessed = as.Date("2021-10-18"),
         citation = "Havenstrøm, E. (2021). Port calls in Kings Bay. https://port.kingsbay.no/statistics") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)

## SOCAT
### NB: EU_SOCAT loaded in EU full product section
kong_SOCAT <- EU_SOCAT %>% 
  filter(lon >= bbox_kong[1], lon <= bbox_kong[2],
         lat >= bbox_kong[3], lat <= bbox_kong[4])
save(kong_SOCAT, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/SOCAT_kong.RData")

## GLODAP
### NB: EU_GLODAP loaded in EU full product section
### NB: There are no GLODAP data in Kongsfjorden
kong_GLODAP <- EU_GLODAP %>% 
  filter(lon >= bbox_kong[1], lon <= bbox_kong[2],
         lat >= bbox_kong[3], lat <= bbox_kong[4])

# Combine and save
full_product_kong <- rbind(pg_kong_ALL, kong_sea_ice_inner, kong_zoo_data, kong_protist_nutrient_chla, # kong_glacier_info,
                           kong_CTD_database, kong_CTD_CO2, kong_weather_station, kong_mooring_GFI, 
                           kong_ferry, kong_mooring_SAMS, kong_ship_arrivals, kong_SOCAT) %>% 
  rbind(filter(full_product_sval, lon >= bbox_kong[1], lon <= bbox_kong[2], lat >= bbox_kong[3], lat <= bbox_kong[4])) %>% 
  rbind(filter(full_product_sval, grepl("Kongsfjorden", var_name))) %>% distinct()
data.table::fwrite(full_product_kong, "~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.csv")
save(full_product_kong, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")
save(full_product_kong, file = "data/full_data/full_product_kong.RData")
rm(list = grep("kong_",names(.GlobalEnv),value = TRUE)); gc()

# Search product for specific authors
# if(!exists("full_product_kong")) load("~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")

# Simple checks
# full_product_kong %>% filter(grepl("Jentzsch", citation))

# Philipp Fischer ferry box data - There are a couple of months of data for 2014
# kong_fischer <- full_product_kong %>% filter(grepl("Fischer", citation))

# Popova carbonate chemistry model data - No data present
# kong_popova <- full_product_kong %>% filter(grepl("Popova", citation))

# Clean up
# rm(kong_fischer, kong_popova)


## Model product -----------------------------------------------------------

## NB: These files are available on pCloud at: pCloudDrive/FACE-IT_data/model/
## I load them from a local folder here for speed and convenience
# model_kong <- load_model("kongsfjorden_rcp")

## Test visuals
# model_summary(model_kong, "Kongsfjorden")


# Isfjorden ---------------------------------------------------------------

## PG product --------------------------------------------------------------

# Load pg is files
system.time(
  pg_is_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_is)
) # 65 seconds

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
# colnames(pg_is_clean)

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
# pg_is_Social <- pg_var_melt(pg_is_clean, query_Social$pg_col_name, "soc") # empty

# Stack them together
pg_is_ALL <- rbind(pg_is_Cryosphere, pg_is_Physical, pg_is_Chemistry, pg_is_Biology)
data.table::fwrite(pg_is_ALL, "~/pCloudDrive/FACE-IT_data/isfjorden/pg_is_ALL.csv")
save(pg_is_ALL, file = "~/pCloudDrive/FACE-IT_data/isfjorden/pg_is_ALL.RData")
save(pg_is_ALL, file = "data/pg_data/pg_is_ALL.RData")

# Check that all columns were used
# colnames(pg_is_clean)[!colnames(pg_is_clean) %in% unique(pg_is_ALL$var_name)]

# Clean up
rm(list = grep("pg_is",names(.GlobalEnv),value = TRUE)); gc()


## Full product ------------------------------------------------------------

# Load full Svalbard file
if(!exists("full_product_sval")) load("data/full_data/full_product_sval.RData")

# Load PG file
if(!exists("pg_is_ALL")) load("~/pCloudDrive/FACE-IT_data/isfjorden/pg_is_ALL.RData")

# Process individual files
## Mouth mooring North
is_mooring_N <- plyr::ldply(dir("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_N", full.names = T), load_is_mooring, .parallel = T); gc()

## Mouth mooring South
is_mooring_S <- plyr::ldply(dir("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S", full.names = T), load_is_mooring, .parallel = T); gc()

## Mooring IFO
is_mooring_IFO_units <- distinct(rbind(ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_IFO/IFO1617.nc")$variable,
                                       ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_IFO/IFO1617_ADCP.nc")$variable))
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
  group_by(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name) %>% 
  summarise(value = round(mean(value, na.rm = T), 6), .groups = "drop")
rm(is_mooring_IFO_units); gc()

## North mouth mooring GFI
is_mooring_GFI_N <- plyr::ldply(dir("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_N", full.names = T), load_GFI, .parallel = T) %>% 
  mutate(date_accessed = as.Date("2021-04-15"), .before = 1)

## North mouth mooring GFI
is_mooring_GFI_S <- plyr::ldply(dir("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_S", full.names = T), load_GFI, .parallel = T) %>% 
  mutate(date_accessed = as.Date("2021-08-04"), .before = 1)

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
  group_by(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name) %>% 
  summarise(value = round(mean(value, na.rm = T), 6), .groups = "drop")

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
  group_by(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name) %>% 
  summarise(value = round(mean(value, na.rm = T), 6), .groups = "drop")

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
         citation = "University Centre in Svalbard (2020).ISA_Svalbard_Chlorophyll_A_2011_2019 [Data set]. Norstore. https://doi.org/10.11582/2020.00063",
         lon = 15.52992, lat = 78.26105,
         var_name = paste0(var_name," - ",data," [", units,"]"),
         var_type = "bio",
         date_accessed = as.Date("2021-04-16")) %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name) %>% 
  summarise(value = round(mean(value, na.rm = T), 6), .groups = "drop")
rm(is_Chla_IsA_units, is_Chla_IsA_1, is_Chla_IsA_2); gc()

## Isfjord radio meteorological station
is_met_radio <- load_met_NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/SN99790.nc") %>% mutate(date_accessed = as.Date("2021-04-14"), .before = 1)

## Airport meteorological station
is_met_airport <- load_met_NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/SN99840.nc") %>% mutate(date_accessed = as.Date("2021-08-04"), .before = 1)

## Pyramiden radio meteorological station
is_met_pyramiden <- load_met_NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/SN99880.nc") %>% mutate(date_accessed = as.Date("2021-08-04"), .before = 1)

## Ship AIS data
is_AIS_2017 <- read_csv("~/pCloudDrive/FACE-IT_data/isfjorden/AIS/AIS_2017.csv") %>% mutate(year = 2017)
is_AIS_2019 <- read_csv("~/pCloudDrive/FACE-IT_data/isfjorden/AIS/AIS_2019.csv") %>% mutate(year = 2019)
is_AIS <- rbind(is_AIS_2017, is_AIS_2019) %>% 
  dplyr::select(Name, ShipName, Month, everything()) %>% 
  pivot_longer(`Number of trips`:`CO2 emissions in port (tonnes)`, names_to = "var", values_to = "value") %>% 
  mutate(date = as.Date(paste0(year,"-",Month,"-01")),
         depth = 0, # It may be better to list this as NA 
         lon = NA, lat = NA, 
         date_accessed = as.Date("2020-09-30"),
         var_name = paste0(ShipName," [",var,"]"),
         var_type = case_when(grepl("co2|nox|sox", var_name, ignore.case = T) ~ "chem",
                              grepl("PM", var_name, ignore.case = T) ~ "phys", TRUE ~ "soc"),
         URL = "Received directly from Morten Simonsen",
         citation = "Simonsen, M., Walnum, H. J., & Gössling, S. (2018). Model for estimation of fuel consumption of cruise ships. Energies, 11(5), 1059.") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)
rm(is_AIS_2017, is_AIS_2019); gc()

# Raw AIS data
"~/pCloud"

## Tourist ship arrival data
is_ship_arrivals <- read_csv("~/pCloudDrive/FACE-IT_data/isfjorden/is_ship_arrivals.csv") %>% 
  pivot_longer(`2007`:`2019`, names_to = "date", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(var_name = paste0(type," [",name,"]"),
         var_type = "soc",
         date = as.Date(paste0(date,"-12-31")),
         depth = NA, lon = 15.60, lat = 78.23,
         URL = "https://portlongyear.no/statistics-of-port-longyear-2007-2012-2019/",
         date_accessed = as.Date("2021-10-25"),
         citation = "Port of Longyearbyen (2020). Statistics of Port Longyear 2007, 2012-2019. https://portlongyear.no/statistics-of-port-longyear-2007-2012-2019/") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)

## SOCAT
### NB: EU_SOCAT loaded in EU full product section
is_SOCAT <- EU_SOCAT %>% 
  filter(lon >= bbox_is[1], lon <= bbox_is[2],
         lat >= bbox_is[3], lat <= bbox_is[4])
save(is_SOCAT, file = "~/pCloudDrive/FACE-IT_data/isfjorden/SOCAT_is.RData")

## GLODAP
### NB: EU_GLODAP loaded in EU full product section
is_GLODAP <- EU_GLODAP %>% 
  filter(lon >= bbox_is[1], lon <= bbox_is[2],
         lat >= bbox_is[3], lat <= bbox_is[4])
save(is_GLODAP, file = "~/pCloudDrive/FACE-IT_data/isfjorden/GLODAP_is.RData")

# Combine and save
full_product_is <- rbind(pg_is_ALL, is_mooring_N, is_mooring_S, is_mooring_IFO, is_mooring_GFI_N, is_mooring_GFI_S,
                         is_CO2_tempelfjorden, is_CO2_IsA, is_Chla_IsA, is_met_radio, is_met_airport, is_met_pyramiden, 
                         is_AIS, is_ship_arrivals, is_SOCAT, is_GLODAP) %>% 
  rbind(filter(full_product_sval, lon >= bbox_is[1], lon <= bbox_is[2], lat >= bbox_is[3], lat <= bbox_is[4])) %>% 
  rbind(filter(full_product_sval, grepl("Isfjorden", var_name))) %>% distinct()
data.table::fwrite(full_product_is, "~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.csv")
save(full_product_is, file = "~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.RData")
save(full_product_is, file = "data/full_data/full_product_is.RData")
rm(list = grep("is_",names(.GlobalEnv),value = TRUE)); gc()
# if(!exists("full_product_is")) load("~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.RData")


## Test visuals ------------------------------------------------------------

# is_phys <- full_product_is %>% 
#   filter(var_type == "phys")

# is_pH <- full_product_is %>% 
#   filter(grepl("pH",var_name))

# is_temp <- is_phys %>% 
#   filter(grepl("°C",var_name))

# is_temp %>% 
#   filter(depth >= 0) %>% 
#   group_by(date, depth) %>% 
#   summarise(value = mean(value, na.rm = T)) %>% 
#   ggplot(aes(x = date, y = depth)) +
#   geom_point(aes(colour = value)) +
#   scale_y_reverse()

# is_temp_ph <- left_join(is_temp, is_pH, by = c("date_accessed", "URL", "citation", "lon", "lat", "date", "depth"))
# is_temp_ph <- rbind(is_temp, is_pH)
# is_temp_ph %>% 
#   filter(!is.na(value.y)) %>% 
#   ggplot(aes(x = value.x, y = value.y)) +
#   geom_point() +
#   scale_y_reverse()


# Storfjorden -------------------------------------------------------------

## PG product --------------------------------------------------------------

# Load pg ingle files
system.time(
  pg_stor_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_stor)
) # 64 seconds

# Test problem files
# pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
# pg_test <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.778258")

# Remove unneeded columns
pg_stor_clean <- pg_stor_sub %>% 
  # dplyr::select(contains(c("press", "depth", "elev", "lon", "lat")), everything()) %>%  # Look at depth columns
  # Manually remove problematic files - No issues
  # Manually remove problematic columns - No issues
  mutate_all(~na_if(., '')) %>% 
  janitor::remove_empty("cols") %>% 
  # Manage lon/lat columns - No issues
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = case_when(!is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Depth [m]`) ~ as.numeric(`Depth [m]`),
                           !is.na(`Press [dbar]`) ~ as.numeric(`Press [dbar]`))) %>%
  mutate(depth = case_when(is.na(depth) & !is.na(`Elevation [m]`) ~ -`Elevation [m]`,
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -`Elevation [m a.s.l.]`,
                           TRUE ~ depth)) %>% 
  # Remove unwanted columns
  dplyr::select(-"Press [dbar]",- "Depth water [m]",- "Depth bot [m]",
                -"Bathy depth interp/grid [m]",
                -contains(c("Elev ", "Elevation ", "Latitude", "Longitude"))) %>%
  # Finish up
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(7:length(.)), as.numeric) %>% 
  janitor::remove_empty("cols")
# colnames(pg_stor_clean)

## Individual category data.frames
# Cryosphere
pg_stor_Cryosphere <- pg_var_melt(pg_stor_clean, query_Cryosphere$pg_col_name, "cryo")
# Physical
pg_stor_Physical <- pg_var_melt(pg_stor_clean, query_Physical$pg_col_name, "phys")
# Carbonate chemistry
pg_stor_Chemistry <- pg_var_melt(pg_stor_clean, query_Chemistry$pg_col_name, "chem")
# Biology
# pg_stor_Biology <- pg_var_melt(pg_stor_clean, query_Biology$pg_col_name, "bio") # empty
# Social
# pg_stor_Social <- pg_var_melt(pg_stor_clean, query_Social$pg_col_name, "soc") # empty

# Stack them together
pg_stor_ALL <- rbind(pg_stor_Cryosphere, pg_stor_Physical, pg_stor_Chemistry)
data.table::fwrite(pg_stor_ALL, "~/pCloudDrive/FACE-IT_data/storfjorden/pg_stor_ALL.csv")
save(pg_stor_ALL, file = "~/pCloudDrive/FACE-IT_data/storfjorden/pg_stor_ALL.RData")

# Check that all columns were used
# colnames(pg_stor_clean)[!colnames(pg_stor_clean) %in% unique(pg_stor_ALL$var_name)]

# Clean up
rm(list = grep("pg_stor",names(.GlobalEnv),value = TRUE)); gc()


## Full product ------------------------------------------------------------

# Load full Svalbard file
if(!exists("full_product_sval")) load("data/full_data/full_product_sval.RData")

# Load PG file
if(!exists("pg_stor_ALL")) load("~/pCloudDrive/FACE-IT_data/storfjorden/pg_stor_ALL.RData")

# Process individual files
## Light data
### NB: No columns with key drivers: CDOM, icam_anap, Perkins_ap
### NB: pressure [dbar] used here as no depth data available
stor_light_CTD <- read_csv("~/pCloudDrive/FACE-IT_data/storfjorden/optical_properties/acs_fdom_ctd.csv") %>% 
  dplyr::select(`Lat [deg_N]`, `Lon [deg_E]`, Year, Month, Day, `Pressure [dbar]`, `Temp [degC]`, `Sal [PSU]`) %>% 
  dplyr::rename(lat = `Lat [deg_N]`, lon = `Lon [deg_E]`, depth = `Pressure [dbar]`, `Temp [°C]` = `Temp [degC]`) %>% 
  pivot_longer(`Temp [°C]`:`Sal [PSU]`, names_to = "var_name", values_to = "value") %>% 
  mutate(date = as.Date(paste0(Year,"-",Month,"-",Day)),
         date_accessed = as.Date("2022-01-21"),
         var_type = "phys",
         URL = "https://data.npolar.no/dataset/e6974f73-99bb-46d8-b06d-d00287b91729",
         citation = "Petit, T., Granskog, M. A., Hamre, B., Kowalczuk, P., & Röttgers, R. (2022). Inherent optical properties of waters in Storfjorden (Svalbard) in summer 2020 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2022.e6974f73") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value) %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")

## SOCAT
### NB: EU_SOCAT loaded in EU full product section
stor_SOCAT <- EU_SOCAT %>% 
  filter(lon >= bbox_stor[1], lon <= bbox_stor[2],
         lat >= bbox_stor[3], lat <= bbox_stor[4])
save(stor_SOCAT, file = "~/pCloudDrive/FACE-IT_data/storfjorden/SOCAT_stor.RData")

## GLODAP
### NB: EU_GLODAP loaded in EU full product section
stor_GLODAP <- EU_GLODAP %>% 
  filter(lon >= bbox_stor[1], lon <= bbox_stor[2],
         lat >= bbox_stor[3], lat <= bbox_stor[4])
save(stor_GLODAP, file = "~/pCloudDrive/FACE-IT_data/storfjorden/GLODAP_stor.RData")

# Combine and save
full_product_stor <- rbind(pg_stor_ALL, stor_light_CTD, stor_SOCAT, stor_GLODAP) %>% 
  rbind(filter(full_product_sval, lon >= bbox_stor[1], lon <= bbox_stor[2], lat >= bbox_stor[3], lat <= bbox_stor[4])) %>% 
  rbind(filter(full_product_sval, grepl("Storfjorden", var_name))) %>% distinct()
data.table::fwrite(full_product_stor, "~/pCloudDrive/FACE-IT_data/storfjorden/full_product_stor.csv")
save(full_product_stor, file = "~/pCloudDrive/FACE-IT_data/storfjorden/full_product_stor.RData")
save(full_product_stor, file = "data/full_data/full_product_stor.RData")
rm(list = grep("stor_",names(.GlobalEnv),value = TRUE)); gc()
# if(!exists("full_product_stor")) load("~/pCloudDrive/FACE-IT_data/storfjorden/full_product_stor.RData")


# Young Sound -------------------------------------------------------------

## PG product --------------------------------------------------------------

# Load pg young files
system.time(
  pg_young_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_young)
) # 65 seconds

# Test problem files
# pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
# pg_test <- pg_dl_proc(pg_doi = pg_young_all$doi[32])

# Remove unneeded columns
pg_young_clean <- pg_young_sub %>% 
  dplyr::select(contains(c("date", "lon", "lat")), everything()) %>%  # Look at meta columns
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
                          # is.na(date) & !is.na(Date) ~ as.character(Date),
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
# colnames(pg_young_clean)

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
# pg_young_Social <- pg_var_melt(pg_young_clean, query_Social$pg_col_name, "soc") # empty

# Stack them together
pg_young_ALL <- rbind(pg_young_Cryosphere, pg_young_Physical, pg_young_Chemistry, pg_young_Biology)
data.table::fwrite(pg_young_ALL, "~/pCloudDrive/FACE-IT_data/young_sound/pg_young_ALL.csv")
save(pg_young_ALL, file = "~/pCloudDrive/FACE-IT_data/young_sound/pg_young_ALL.RData")

# Check that all columns were used
# colnames(pg_young_clean)[!colnames(pg_young_clean) %in% unique(pg_young_ALL$var_name)]

# Clean up
rm(list = grep("pg_young",names(.GlobalEnv),value = TRUE)); gc()


## Full product ------------------------------------------------------------

# Load PG product
if(!exists("pg_young_ALL")) load("~/pCloudDrive/FACE-IT_data/young_sound/pg_young_ALL.RData")

# Primary production data
holding_station_idx <- read_csv("~/pCloudDrive/FACE-IT_data/young_sound/Holding_etal_2019_data/CTD_biochem/YS_2014_CTD_biochem.csv") %>% 
  dplyr::rename(lon = LONG_DD, lat = LAT_DD) %>% 
  dplyr::select(lon, lat, station) %>% 
  distinct()
holding_CTD_biochem <- read_csv("~/pCloudDrive/FACE-IT_data/young_sound/Holding_etal_2019_data/CTD_biochem/YS_2014_CTD_biochem.csv") %>% 
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>% 
  dplyr::rename(lon = LONG_DD, lat = LAT_DD) %>% 
  dplyr::select(lon, lat, date, depth, z_mix, z_photo, temp:SiO4, -real_depth) %>% 
  pivot_longer(z_mix:SiO4, names_to = "var_name", values_to = "value")
holding_CTD_profiles <- read_csv("~/pCloudDrive/FACE-IT_data/young_sound/Holding_etal_2019_data/CTD_biochem/YS_2014_SBE19plus_CTD_profiles_ALL.csv") %>% 
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>% 
  dplyr::rename(lon = LONG_DD, lat = LAT_DD) %>% 
  dplyr::select(lon, lat, date, depth, temp:SoundVelocit_m_s, -real_depth) %>% 
  pivot_longer(temp:SoundVelocit_m_s, names_to = "var_name", values_to = "value")
holding_PI <- read_csv("~/pCloudDrive/FACE-IT_data/young_sound/Holding_etal_2019_data/PI_parameters/YS_2014_PI_parameters.csv") %>%
  mutate(date = as.Date(paste0(year,"-",month,"-",day)), .keep = "unused") %>% 
  pivot_longer(pm_chl:ik, names_to = "var_name", values_to = "value") %>% 
  left_join(holding_station_idx, by = "station") %>% 
  dplyr::select(lon, lat, date, depth, var_name, value)
holding_ChlA <- read_csv("~/pCloudDrive/FACE-IT_data/young_sound/Holding_etal_2019_data/Chl_a/YS_2014_Chl_Fractions.csv") %>% 
  mutate(date = as.Date(paste0(year,"-",month,"-",day)), .keep = "unused") %>% 
  pivot_longer(chla_GFF_conc:TOTAL_chla_area, names_to = "var_name", values_to = "value") %>% 
  left_join(holding_station_idx, by = "station") %>% 
  dplyr::select(lon, lat, date, depth, var_name, value)
holding_PP <- read_csv("~/pCloudDrive/FACE-IT_data/young_sound/Holding_etal_2019_data/Primary_production/YS_2014_PP_Fractions.csv") %>% 
  mutate(date = as.Date(paste0(year,"-",month,"-",day)), .keep = "unused") %>% 
  pivot_longer(PP_plus_10_frac:TOTAL_PP_area, names_to = "var_name", values_to = "value") %>% 
  left_join(holding_station_idx, by = "station") %>% 
  dplyr::select(lon, lat, date, depth, var_name, value)
young_prim_prod <- rbind(holding_CTD_biochem, holding_CTD_profiles, holding_PI, holding_ChlA, holding_PP) %>% 
  filter(!is.na(value)) %>% 
  mutate(URL = "https://zenodo.org/record/5572041#.YW_Lc5uxU5m",
         citation = "Holding, Johnna M, Markager, Stiig, Juul-Pedersen, Thomas, Paulsen, Maria L, Møller, Eva F, & Sejr, Mikael K. (2021). Dataset from Holding et al. (2019) Seasonal and spatial patterns of primary production in a high latitude fjord [Data set]. Zenodo. https://doi.org/10.5281/zenodo.5572041",
         date_accessed = as.Date("2021-10-20"),
         var_type = case_when(var_name %in% c("z_mix", "z_photo", "strat_index", "temp", "conductivity", 
                                              "turbidity", "PAR", "salinity", "pot_temp", "sigmaT_kg_m3",
                                              "density_kg_m3", "SoundVelocit_m_s") ~ "phys",
                              var_name %in% c("nitracline", "oxygen_umol_kg", "oxygen_corrected_umol_kg", 
                                              "NH4", "NO2", "NO3", "NO2_NO3", "PO4", "SiO4") ~ "chem",
                              TRUE ~ "bio"),
         var_name = case_when(var_name == "pm_chl" ~ "pm_chl [g C g-1 Chl h-1]",
                              var_name == "alpha_chl" ~ "alpha_chl [g C g-1 Chl mol-1 photons m2]",
                              var_name == "ik" ~ "ik [μmol photons m-2 s-1]",
                              var_name %in% c("PP_area_plus_10_frac", "PP_area_GFF_frac", 
                                              "PP_area_disolv_frac") ~ paste0(var_name," [mg C m-2 day-1]"),
                              var_name %in% c("z_mix", "z_photo", "fluor_max", "chl_max", "nitracline") ~ paste0(var_name," [m]"),
                              var_name %in% c("temp", "pot_temp") ~ paste0(var_name," [°C]"),
                              var_name == "conductivity" ~ "conductivity [S/m]",
                              var_name == "salinity" ~ "salinity [PSU]",
                              var_name == "turbidity" ~ "turbidity [FTU]",
                              var_name == "PAR" ~ "PAR [µmol m-2 s-1]",
                              var_name %in% c("oxygen_umol_kg", "oxygen_corrected_umol_kg") ~ paste0(var_name," [µmol kg-1]"),
                              var_name %in% c("sigmaT_kg_m3", "density_kg_m3") ~ paste0(var_name," [kg m-3]"),
                              var_name == "SoundVelocit_m_s" ~ "SoundVelocit_m_s [m/s]",
                              var_name == "chl_flu" ~ "chl_flu [µg chl m-3]",
                              var_name == "pp_vol" ~ "pp_vol [mg C m-3 day-1]",
                              var_name == "pp_chla" ~ "pp_chla [mg C µg Chla-1 m–3 day-1]",
                              var_name %in% c("NH4", "NO2", "NO3", "NO2_NO3", "PO4", "SiO4") ~ paste0(var_name," [µmol L-1]"),
                              TRUE ~ var_name)) %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop")
rm(list = grep("holding_",names(.GlobalEnv),value = TRUE)); gc()

## SOCAT
### NB: EU_SOCAT loaded in EU full product section
### NB: No SOCAT data in Young Sound
young_SOCAT <- EU_SOCAT %>% 
  filter(lon >= bbox_young[1], lon <= bbox_young[2],
         lat >= bbox_young[3], lat <= bbox_young[4])

## GLODAP
### NB: EU_GLODAP loaded in EU full product section
### NB: No GLODAP data in Young Sound
young_GLODAP <- EU_GLODAP %>% 
  filter(lon >= bbox_young[1], lon <= bbox_young[2],
         lat >= bbox_young[3], lat <= bbox_young[4])

# Combine and save
full_product_young <- rbind(pg_young_ALL, young_prim_prod) %>% distinct()
data.table::fwrite(full_product_young, "~/pCloudDrive/FACE-IT_data/young_sound/full_product_young.csv")
save(full_product_young, file = "~/pCloudDrive/FACE-IT_data/young_sound/full_product_young.RData")
save(full_product_young, file = "data/full_data/full_product_young.RData")
rm(list = grep("young_",names(.GlobalEnv),value = TRUE)); gc()
# if(!exists("full_product_young")) load("~/pCloudDrive/FACE-IT_data/young_sound/full_product_young.RData")


# Disko Bay ---------------------------------------------------------------

## PG product --------------------------------------------------------------

# Load pg files and subset to Disko Bay
system.time(
  pg_disko_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_disko)
) # 65 seconds

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
# colnames(pg_disko_clean)

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
# pg_disko_Social <- pg_var_melt(pg_disko_clean, query_Social$pg_col_name, "soc") # empty

# Stack them together
pg_disko_ALL <- rbind(pg_disko_Cryosphere, pg_disko_Physical, pg_disko_Chemistry, pg_disko_Biology)
data.table::fwrite(pg_disko_ALL, "~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_ALL.csv")
save(pg_disko_ALL, file = "~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_ALL.RData")

# Check that all columns were used
# colnames(pg_disko_clean)[!colnames(pg_disko_clean) %in% unique(pg_disko_ALL$var_name)]

# Clean up
rm(list = grep("pg_disko",names(.GlobalEnv),value = TRUE)); gc()


## Full product ------------------------------------------------------------

# Load PG product
if(!exists("pg_disko_ALL")) load("~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_ALL.RData")

# Biochemistry CTD cruise
# SANNA_2016_SBE19plus_CTD_profiles.nc
disko_CTD_ChlA_var <- ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/disko_bay/SANNA_2016_SBE19plus_CTD_profiles.nc")$variable
disko_CTD_ChlA <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/disko_bay/SANNA_2016_SBE19plus_CTD_profiles.nc")) %>% 
  left_join(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/disko_bay/SANNA_2016_SBE19plus_CTD_profiles.nc"), "D0,D1")), 
            by = c("stations", "trajectory")) %>% 
  dplyr::rename(lon = longitude, lat = latitude, depth = z_ctd) %>% 
  pivot_longer(salinity:sound_velocity, names_to = "var_name", values_to = "value") %>% 
  filter(!is.na(value), value != -9999) %>% 
  left_join(disko_CTD_ChlA_var[,c("name", "units")], by = c("var_name" = "name")) %>% 
  mutate(date = as.Date(as.POSIXct(time, origin = "1970-01-01")),
         date_accessed = as.Date("2021-10-20"),
         URL = "https://zenodo.org/record/4062024#.YW_TOJuxU5l",
         citation = "Carlson, Daniel F., Holding, Johnna M., Bendtsen, Jørgen, Markager, Stiig, Møller, Eva F., Meire, Lorenz, Rysgaard, Søren, Dalsgaard, Tage, & Sejr, Mikael K. (2020). CTD Profiles from the R/V Sanna cruise to Northwest Greenland fjords, August 11-31, 2016 [Data set]. Zenodo. https://doi.org/10.5281/zenodo.4062024",
         var_type = case_when(TRUE ~ "phys"),
         units = case_when(units == "practical_salinity_units" ~ "PSU",
                           units == "degrees_Celsius" ~ "°C",
                           TRUE ~ units),
         var_name = paste0(var_name," [",units,"]"), .keep = "unused") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)
rm(disko_CTD_ChlA_var); gc()

## SOCAT
### NB: EU_SOCAT loaded in EU full product section
disko_SOCAT <- EU_SOCAT %>% 
  filter(lon >= bbox_disko[1], lon <= bbox_disko[2],
         lat >= bbox_disko[3], lat <= bbox_disko[4])
save(disko_SOCAT, file = "~/pCloudDrive/FACE-IT_data/disko_bay/SOCAT_disko.RData")

## GLODAP
### NB: EU_GLODAP loaded in EU full product section
disko_GLODAP <- EU_GLODAP %>% 
  filter(lon >= bbox_disko[1], lon <= bbox_disko[2],
         lat >= bbox_disko[3], lat <= bbox_disko[4])
save(disko_GLODAP, file = "~/pCloudDrive/FACE-IT_data/disko_bay/GLODAP_disko.RData")

# Combine and save
full_product_disko <- rbind(pg_disko_ALL, disko_CTD_ChlA, disko_SOCAT, disko_GLODAP) %>% distinct()
data.table::fwrite(full_product_disko, "~/pCloudDrive/FACE-IT_data/disko_bay/full_product_disko.csv")
save(full_product_disko, file = "~/pCloudDrive/FACE-IT_data/disko_bay/full_product_disko.RData")
save(full_product_disko, file = "data/full_data/full_product_disko.RData")
rm(list = grep("disko_",names(.GlobalEnv),value = TRUE)); gc()
# if(!exists("full_product_disko")) load("~/pCloudDrive/FACE-IT_data/disko_bay/full_product_disko.RData")


# Nuup Kangerlua ----------------------------------------------------------

## PG product --------------------------------------------------------------

# Load pg Nuup Kangerlua files
system.time(
  pg_nuup_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_nuup)
) # 65 seconds

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
  mutate(date = case_when(date %in% seq(2000, 2009) ~ paste0(date,"-01-01"),
                          # is.na(date) & !is.na(Date) ~ as.character(Date),
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
# colnames(pg_nuup_clean)

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
# pg_nuup_Social <- pg_var_melt(pg_nuup_clean, query_Social$pg_col_name, "soc") # empty

# Stack them together
pg_nuup_ALL <- rbind(pg_nuup_Cryosphere, pg_nuup_Physical, pg_nuup_Chemistry, pg_nuup_Biology)
data.table::fwrite(pg_nuup_ALL, "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/pg_nuup_ALL.csv")
save(pg_nuup_ALL, file = "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/pg_nuup_ALL.RData")

# Check that all columns were used
# colnames(pg_nuup_clean)[!colnames(pg_nuup_clean) %in% unique(pg_nuup_ALL$var_name)]

# Clean up
rm(list = grep("pg_nuup",names(.GlobalEnv),value = TRUE)); gc()


## Full product ------------------------------------------------------------

# Load PG product
if(!exists("pg_nuup_ALL")) load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/pg_nuup_ALL.RData")

## SOCAT
### NB: EU_SOCAT loaded in EU full product section
nuup_SOCAT <- EU_SOCAT %>% 
  filter(lon >= bbox_nuup[1], lon <= bbox_nuup[2],
         lat >= bbox_nuup[3], lat <= bbox_nuup[4])
save(nuup_SOCAT, file = "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/SOCAT_nuup.RData")

## GLODAP
### NB: EU_GLODAP loaded in EU full product section
### NB: No GLODAP data in Nuup Kangerlua
nuup_GLODAP <- EU_GLODAP %>% 
  filter(lon >= bbox_nuup[1], lon <= bbox_nuup[2],
         lat >= bbox_nuup[3], lat <= bbox_nuup[4])

# Combine and save
full_product_nuup <- rbind(pg_nuup_ALL, nuup_SOCAT) %>% distinct()
data.table::fwrite(full_product_nuup, "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/full_product_nuup.csv")
save(full_product_nuup, file = "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/full_product_nuup.RData")
save(full_product_nuup, file = "data/full_data/full_product_nuup.RData")
rm(list = grep("nuup_",names(.GlobalEnv),value = TRUE)); gc()
if(!exists("full_product_nuup")) load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/full_product_nuup.RData")


# Porsangerfjorden --------------------------------------------------------

## PG product --------------------------------------------------------------

# Load pg is files
system.time(
  pg_por_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_por)
) # 65 seconds

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
# colnames(pg_por_clean)

## Individual category data.frames
# Cryosphere
pg_por_Cryosphere <- pg_var_melt(pg_por_clean, query_Cryosphere$pg_col_name, "cryo")
# Physical
pg_por_Physical <- pg_var_melt(pg_por_clean, query_Physical$pg_col_name, "phys")
# Carbonate chemistry
pg_por_Chemistry <- pg_var_melt(pg_por_clean, query_Chemistry$pg_col_name, "chem")
# Biology
# pg_por_Biology <- pg_var_melt(pg_por_clean, query_Biology$pg_col_name, "bio") # empty
# Social
# pg_por_Social <- pg_var_melt(pg_por_clean, query_Social$pg_col_name, "soc") # empty

# Stack them together
pg_por_ALL <- rbind(pg_por_Cryosphere, pg_por_Physical, pg_por_Chemistry)
data.table::fwrite(pg_por_ALL, "~/pCloudDrive/FACE-IT_data/porsangerfjorden/pg_por_ALL.csv")
save(pg_por_ALL, file = "~/pCloudDrive/FACE-IT_data/porsangerfjorden/pg_por_ALL.RData")

# Check that all columns were used
# colnames(pg_por_clean)[!colnames(pg_por_clean) %in% unique(pg_por_ALL$var_name)]

# Clean up
rm(list = grep("pg_por",names(.GlobalEnv),value = TRUE)); gc()


## Full product ------------------------------------------------------------

# Load PG product
if(!exists("pg_por_ALL")) load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/pg_por_ALL.RData")

## Series of GFI moorings
por_mooring_GFI <- plyr::ldply(dir("~/pCloudDrive/FACE-IT_data/porsangerfjorden/mooring_GFI", full.names = T), load_GFI, .parallel = T) %>% 
  mutate(date_accessed = as.Date("2021-08-11"), .before = 1)

## Sea ice extent for Norwegian fjords
por_sea_ice <- read_delim("~/pCloudDrive/FACE-IT_data/porsangerfjorden/12d_ice-extent.txt", delim = "\t") %>% 
  filter(`...2` == "Porsangerfjord") %>% 
  pivot_longer(`2001-02-02`:`2019-06-26`, names_to = "date", values_to = "value") %>%
  mutate(date = as.Date(date),
         date_accessed = as.Date("2021-09-08"),
         URL = "https://zenodo.org/record/4133926#.YTikO1uxU5l",
         citation = "Megan O'Sadnick, Chris Petrich, Camilla Brekke, & Jofrid Skardhamar. (2020). Ice extent in Norwegian fjords, 2001-2019 [Data set]. In Annals of Glaciology (1.0.1, Number Sea Ice at the Interface). Zenodo. https://doi.org/10.5281/zenodo.4133926",
         lon = NA, lat = NA, depth = NA,
         var_type = "cryo", var_name = "ice area [km2]") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, var_type, var_name, value)

## Hydrographic data
por_hydro <- plyr::ldply(1952:2013, load_nor_hydro, date_accessed = as.Date("2021-09-08"))

## SOCAT
### NB: EU_SOCAT loaded in EU full product section
por_SOCAT <- EU_SOCAT %>% 
  filter(lon >= bbox_por[1], lon <= bbox_por[2],
         lat >= bbox_por[3], lat <= bbox_por[4])
save(por_SOCAT, file = "~/pCloudDrive/FACE-IT_data/porsangerfjorden/SOCAT_por.RData")

## GLODAP
### NB: EU_GLODAP loaded in EU full product section
### NB: No GLODAP data in Porsangerfjorden
por_GLODAP <- EU_GLODAP %>% 
  filter(lon >= bbox_por[1], lon <= bbox_por[2],
         lat >= bbox_por[3], lat <= bbox_por[4])

# Combine and save
full_product_por <- rbind(pg_por_ALL, por_mooring_GFI, por_sea_ice, por_hydro, por_SOCAT) %>% distinct()
data.table::fwrite(full_product_por, "~/pCloudDrive/FACE-IT_data/porsangerfjorden/full_product_por.csv")
save(full_product_por, file = "~/pCloudDrive/FACE-IT_data/porsangerfjorden/full_product_por.RData")
save(full_product_por, file = "data/full_data/full_product_por.RData")
rm(list = grep("por_",names(.GlobalEnv),value = TRUE)); gc()
if(!exists("full_product_por")) load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/full_product_por.RData")


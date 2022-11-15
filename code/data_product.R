# code/data_product.R
# This script houses the code used to create data products from the many disparate files

# TODO: Investigate PG columns that should be numeric but aren't
# e.g. `[NO3]- [µmol/l]` for pg_nuup_clean
# Provide lists of variables that were removed
# Correct PANGAEA values with `t [°C]` to be type 'cryo' as this is the unit for ice/snow temperature


# Setup -------------------------------------------------------------------

# Libraries
source("code/functions.R")
source("code/key_drivers.R")
library(stringi)
library(pxweb)

# Re-run full data collection pipeline
# system.time(
# source("code/data_collection.R")
# )

# PANGAEA files
pg_files <- dir("data/pg_data", pattern = "pg_", full.names = T)

# Quick filtering function
# Manual tweaks will still be required after running this
# NB: Values with no lon/lat must be removed at this step to prevent data bleed across sites
pg_quick_filter <- function(file_name, bbox){
  pg_dat <- data.table::fread(file_name, nThread = 15)
  if("Longitude" %in% colnames(pg_dat)){
    pg_res <- pg_dat %>% dplyr::rename(lon = Longitude, lat = Latitude) %>% 
      filter(Error == "") %>% 
      filter(lon >= bbox[1], lon <= bbox[2],
             lat >= bbox[3], lat <= bbox[4]) %>% 
      janitor::remove_empty("cols") %>% 
      distinct()
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
    pivot_longer(cols = all_of(sub_cols), names_to = paste0("variable"), values_to = "value") %>% 
    mutate(category = var_word) %>% 
    filter(!is.na(value)) %>%
    distinct() %>% 
    # dplyr::select(date_accessed:depth, category, variable, value)
    group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>%
    summarise(value = mean(value, na.rm = T), .groups = "drop")
  return(pg_melt)
}

# Function for melting individual files from other data sources
# single_file_var_melt <- function(){}


# European Arctic ---------------------------------------------------------

## PG product --------------------------------------------------------------

# There is no EU PANGAEA product because the bits and pieces were given to the individual PG site files
# This file is only loaded to get a summary of the data
# pg_EU_files <- dir("data/pg_data", pattern = "pg_EU", full.names = T)
# system.time(
#   pg_EU <- plyr::ldply(pg_EU_files, pg_quick_filter, bbox = bbox_EU)
# ) # 70 seconds
# length(unique(pg_EU$citation))
# rm(pg_EU); gc()


## Full product ------------------------------------------------------------

# Bathymetry data
## Not on pCloud as this is a large file hosted on a well known website
# EU_GEBCO

# Ice modelling output
## Not on pCloud, too large to download
# EU_ice

# CTD data from NCEI Accession 9700302
## 9700302.2.2.tar.gz # This appears to be some sort of proprietary data format...
# EU_NCEI_1989

# CTD data from Ichthyo research
## NB: Server was down when attempting to access these data on 2022-05-25
# EU_icthyo <- "~/pCloudDrive/restricted_data/PolarData/"

# EU MET station data
# TODO: Develop code to automatically download and process these MET data
# https://thredds.met.no/thredds/catalog/met.no/observations/stations/catalog.html
# https://frost.met.no/index.html

# Zooplankton biodiversity
EU_zooplankton <- read_delim("~/pCloudDrive/FACE-IT_data/EU_arctic/1995-2008-zooplankton-biodiversity.tsv", delim = "\t") %>%
  dplyr::rename(lon = decimalLongitude, lat = decimalLatitude, date = eventDate, depth = minimumDepthInMeters) %>% 
  dplyr::select(lon, lat, date, depth, organismQuantity, organismQuantityType, scientificName, 
                lifeStage, identificationQualifier, sizeGroup, sizeGroupOperator) %>%
  mutate(date = as.Date(date),
         variable = case_when(!is.na(identificationQualifier) ~ paste0(scientificName," ",identificationQualifier), TRUE ~ scientificName),
         variable = case_when(!is.na(lifeStage) ~ paste0(variable," - ",lifeStage), TRUE ~ variable),
         variable = case_when(!is.na(sizeGroup) ~ paste0(variable,"; ",sizeGroup," ",sizeGroupOperator), TRUE ~ variable),
         variable = paste0(variable," [",organismQuantityType,"]"),
         category = "bio",
         value = organismQuantity,
         date_accessed = as.Date("2021-02-11"),
         URL = "https://data.npolar.no/dataset/9167dae8-cab2-45b3-9cea-ad69541b0448",
         citation = "Norwegian Polar Institute (2020). Marine zooplankton and icefauna biodiversity [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2020.9167dae8") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# CTD data from YMER cruise in 1980
## Note that the first row after the header contains the units:
## METERS, DBARS, ITS-90, PSS-78, flag, UMOL/KG, flag, UMOL/KG, flag, UMOL/KG, flag, UMOL/KG, flag, TU, TU, flag, O/OO, flag, DEGC
EU_YMER <- read_csv("~/pCloudDrive/FACE-IT_data/EU_arctic/77YM19800811.exc.csv", skip = 36) %>%
  dplyr::rename(lon = LONGITUDE, lat = LATITUDE, date = DATE, bot_depth = DEPTH, depth = CTDPRS,
                temp = CTDTMP, sal = CTDSAL, O2 = OXYGEN, SiO4 = SILCAT, NO3 = NITRAT, PO4 = PHSPHT) %>% 
  dplyr::select(date:sal, O2, SiO4, NO3, PO4, TRITUM, TRITER, DELO18, THETA) %>% 
  slice(-1) %>% # Remove row containing units
  pivot_longer(temp:THETA, names_to = "variable", values_to = "value") %>%
  mutate(date = lubridate::ymd(date),
         category = case_when(variable %in% c("O2", "SiO4", "NO3", "PO4") ~ "chem", TRUE ~ "phys"),
         variable = case_when(variable == "temp" ~ "temp [ITS-90]",
                              variable == "sal" ~ "sal [PSS-78]",
                              variable %in% c("O2", "SiO4", "NO3", "PO4") ~ paste0(variable," [µmol/kg]"),
                              variable %in% c("TRITUM", "TRITER") ~ paste0(variable," [TU]"),
                              variable == "DELO18" ~ "DELO18 [0/00]",
                              variable == "THETA" ~ "THETA [°C]"),
         value = as.numeric(value),
         depth = as.numeric(depth),
         date_accessed = as.Date("2021-04-15"),
         URL = "https://data.npolar.no/dataset/9167dae8-cab2-45b3-9cea-ad69541b0448",
         citation = "Norwegian Polar Institute (2020). Marine zooplankton and icefauna biodiversity [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2020.9167dae8") %>% 
  filter(value != -999) %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value) %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# CTD data for Arctic
## NB: The documentation does not give the volume of sampling for nutrients (i.e. litres (l) or kilograms (kg))
## But searching through a 1976 paper that they reference it appears to be in litres
EU_Codispoti <- read_csv("~/pCloudDrive/FACE-IT_data/EU_arctic/Codispoti_Arctic_Nutrients_Submission_11-11-2010.csv") %>%
  dplyr::rename(lon = Longitude, lat = Latitude, date = Date, depth = z, temp = `T`, sal = Sal) %>% 
  dplyr::select(date, lat:NO3, NO2) %>% 
  pivot_longer(temp:NO2, names_to = "variable", values_to = "value") %>%
  filter(value != -999) %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         category = case_when(variable %in% c("PO4", "NO2", "NO3") ~ "chem", TRUE ~ "phys"),
         variable = case_when(variable == "temp" ~ "temp [°C]",
                              variable == "sal" ~ "sal [PSU]",
                              variable %in% c("PO4", "NO2", "NO3") ~ paste0(variable," [µmol/l]")),
         date_accessed = as.Date("2021-04-15"),
         URL = "https://www.nodc.noaa.gov/archive/arc0034/0072133/",
         citation = "Multiple. See References section in: https://www.nodc.noaa.gov/archive/arc0034/0072133/1.1/data/1-data/ReadMe_Codispoti_ArcticNuts.pdf") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# Ice core samples for protist presence
## NB: Units for PAR data are not given so it is assumed that they are [µmol m-2 s-1]
## NB: Some ice thickness data is available but the values are not documented so I have not included them here
EU_protists <- read_delim("~/pCloudDrive/FACE-IT_data/EU_arctic/protists/sea-ice protist percentage.csv", delim = ";") %>%
  dplyr::rename(lon = longitude, lat = latitude, PAR = par) %>% 
  dplyr::select(lon, lat, date, depth, PAR, `Acanthostomella norvegica`:`Uronema marinum`) %>% 
  pivot_longer(PAR:`Uronema marinum`, names_to = "variable", values_to = "value") %>%
  mutate(date = as.Date(date, format = "%d.%m.%Y"),
         category = case_when(variable == "PAR" ~ "phys", TRUE ~ "bio"),
         variable = case_when(variable == "PAR" ~ "PAR [µmol m-2 s-1]", TRUE ~ paste0(variable," [% total]")),
         date_accessed = as.Date("2021-04-19"),
         URL = "https://data.npolar.no/dataset/a3111a68-3126-4acd-a64a-d9be46c80842",
         citation = "Hop, H., Vithakari, M., Bluhm, B. A., Assmy, P., Poulin, M., Peeken, I., Gradinger, R., & Melnikov, I. A. (2020). Sea-ice protist in the Arctic Ocean from the 1980s to 2010s [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2020.a3111a68.") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# Carb chem Arctic model output
## Outputs are stored at the Centre for Environmental Data Analysis's (CEDA) JASMIN servers:/gws/nopw/j04/nemo_vol2/ROAM. 
## JASMIN's web address is https://www.jasmin.ac.uk and its access point https://www.jasmin.ac.uk/users/access/. 
## Getting access to this full dataset proved to be nearly impossible and was scrapped after months of effort
## NB: No units were included with these data
EU_Popova <- read_delim("~/pCloudDrive/FACE-IT_data/EU_arctic/Arctic_model_output_acid.dat", delim = " ", 
                        col_names = c("Year", "SST", "ice_extent_March", "ice_extent_September", "MLD", "DIC", "pH")) %>% 
  pivot_longer(SST:pH, names_to = "variable", values_to = "value") %>% 
  mutate(date = case_when(variable == "ice_extent_March" ~ as.Date(paste0(Year,"-03-01")),
                          variable == "ice_extent_September" ~ as.Date(paste0(Year,"-09-01")),
                          TRUE ~ as.Date(paste0(Year,"-12-31"))),
         category = case_when(variable %in% c("SST", "MLD") ~ "phys",
                              variable %in% c("DIC", "pH") ~ "chem",
                              TRUE ~ "cryo"),
         variable = case_when(variable == "SST" ~ "temp [°C]",
                              grepl("ice_extent", variable) ~ "ice extent [km^2]",
                              variable == "MLD" ~ "MLD [m]",
                              variable == "DIC" ~ "DIC [µmol/kg]",
                              TRUE ~ variable),
         lon = NA, lat = NA, depth = NA,
         date_accessed = as.Date("2021-08-11"),
         URL = "File was received directly from J-P Gattuso",
         citation = "Popova, E. E., Yool, A., Aksenov, Y., Coward, A. C., & Anderson, T. R. (2014). Regional variability of acidification in the Arctic: a sea of contrasts. Biogeosciences, 11(2), 293-308.") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# Greenland fjord CTD casts
## NB: The units are found in the README by following the URL
EU_green_fjords <- read_csv("~/pCloudDrive/FACE-IT_data/EU_arctic/LAKO_2018_SBE25_CTD_profiles.csv") %>% 
  dplyr::rename(lon = LONG, lat = LAT) %>% 
  mutate(date = as.Date(paste0(year,"-",month,"-",day))) %>% 
  dplyr::select(lon, lat, date, depth, pressure, salinity:par) %>% 
  pivot_longer(pressure:par, names_to = "variable", values_to = "value") %>% 
  mutate(category = case_when(variable == "fluorescence" ~ "bio", TRUE ~ "phys"),
         variable = case_when(variable == "pressure" ~ "pres [db]",
                              variable == "temperature" ~ "temp [°C]",
                              variable == "salinity" ~ "sal [PSU]",
                              variable == "fluorescence" ~ "fluor",
                              variable == "turbidity" ~ "turbidity [FTU]",
                              variable == "conductivity" ~ "conductivity [S/m]",
                              variable == "par" ~ "PAR [µmol m-2 s-1]"),
         # Remove impossible negative values
         value = case_when(variable != "temp [°C]" & value < 0 ~ as.numeric(NA), TRUE ~ value),
         date_accessed = as.Date("2021-10-20"),
         URL = "https://zenodo.org/record/5572329#.Yo5IrzlBw5n",
         citation = "Holding, Johnna M, Carlson, Daniel F, Meire, Lorenz, Stuart-Lee, Alice, Møller, Eva F, Markager, Lund-Hansen, Lars C, Stedmon, Colin, Britsch, Eik, & Sejr, Mikael K. (2021). CTD Profiles from the HDMS Lauge Koch cruise to East Greenland fjords, August 2018 [Data set]. Zenodo. https://doi.org/10.5281/zenodo.5572329") %>% 
  filter(!is.na(value)) %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# IMR species observations
EU_IMR_spp_obs <- read_delim("~/pCloudDrive/FACE-IT_data/EU_arctic/IMR/EU_occurrence.txt", delim = "\t") %>% 
  unite(year, month, day, sep = "-", remove = T, col = "date") %>% 
  dplyr::rename(lon = decimalLongitude, lat = decimalLatitude, variable = scientificName) %>% 
  dplyr::select(lon, lat, date, variable) %>% 
  distinct() %>% 
  filter(lon <= 60, lon >= -60, lat >= 60) %>% 
  mutate(date = as.Date(date),
         depth = NA,
         variable = paste0(variable," [presence]"),
         category = "bio",
         date_accessed = as.Date("2022-11-14"),
         URL = "https://gbif.imr.no/ipt/resource?r=imr",
         citation = "Sagen, H., Morvik, A. (2015). Observations of marine species [Data set]. GBIF.") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>%
  summarise(value = 1, .groups = "drop")

# SOCAT data
EU_SOCAT <- read_rds("~/pCloudDrive/FACE-IT_data/socat/SOCATv2022.rds") %>%  
  dplyr::rename(lon = `longitude [dec.deg.E]`, lat = `latitude [dec.deg.N]`,
                depth = `sample_depth [m]`, value = `pCO2water_SST_wet [uatm]`) %>% 
  filter(lat >= 63, value >= 0) %>% 
  mutate(lon = case_when(lon >= 180 ~ lon-360, TRUE ~ lon)) %>% 
  filter(lon <= 60, lon >= -60) %>% 
  unite(yr, mon, day, sep = "-", remove = T, col = "date") %>% 
  mutate(date = as.Date(date),
         variable = "pCO2water_SST_wet [uatm]",
         category = "chem",
         date_accessed = as.Date("2021-08-06"),
         URL = "https://www.socat.info",
         citation = "Bakker, D. C. E., Pfeil, B. Landa, C. S., Metzl, N., O’Brien, K. M., Olsen, A., Smith, K., Cosca, C., Harasawa, S., Jones, S. D., Nakaoka, S., Nojiri, Y., Schuster, U., Steinhoff, T., Sweeney, C., Takahashi, T., Tilbrook, B., Wada, C., Wanninkhof, R., Alin, S. R., Balestrini, C. F., Barbero, L., Bates, N. R., Bianchi, A. A., Bonou, F., Boutin, J., Bozec, Y., Burger, E. F., Cai, W.-J., Castle, R. D., Chen, L., Chierici, M., Currie, K., Evans, W., Featherstone, C., Feely, R. A., Fransson, A., Goyet, C., Greenwood, N., Gregor, L., Hankin, S., Hardman-Mountford, N. J., Harlay, J., Hauck, J., Hoppema, M., Humphreys, M. P., Hunt, C. W., Huss, B., Ibánhez, J. S. P., Johannessen, T., Keeling, R., Kitidis, V., Körtzinger, A., Kozyr, A., Krasakopoulou, E., Kuwata, A., Landschützer, P., Lauvset, S. K., Lefèvre, N., Lo Monaco, C., Manke, A., Mathis, J. T., Merlivat, L., Millero, F. J., Monteiro, P. M. S., Munro, D. R., Murata, A., Newberger, T., Omar, A. M., Ono, T., Paterson, K., Pearce, D., Pierrot, D., Robbins, L. L., Saito, S., Salisbury, J., Schlitzer, R., Schneider, B., Schweitzer, R., Sieger, R., Skjelvan, I., Sullivan, K. F., Sutherland, S. C., Sutton, A. J., Tadokoro, K., Telszewski, M., Tuma, M., Van Heuven, S. M. A. C., Vandemark, D., Ward, B., Watson, A. J., Xu, S. (2016) A multi-decade record of high quality fCO2 data in version 3 of the Surface Ocean CO2 Atlas (SOCAT). Earth System Science Data 8: 383-413. doi:10.5194/essd-8-383-2016.") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop")
# save(EU_SOCAT, file = "~/pCloudDrive/FACE-IT_data/EU_arctic/SOCAT_EU.RData")
# load("~/pCloudDrive/FACE-IT_data/EU_arctic/SOCAT_EU.RData")

# GLODAP data
EU_GLODAP <- read_csv("~/pCloudDrive/FACE-IT_data/glodap/GLODAPv2.2022_Merged_Master_File.csv") %>% 
  `colnames<-`(gsub("G2","",colnames(.))) %>% 
  dplyr::rename(lon = longitude, lat = latitude) %>% 
  filter(lon <= 60, lon >= -60, lat >= 63) %>% 
  unite(year, month, day, sep = "-", remove = T, col = "date") %>% 
  mutate(date = as.Date(date)) %>% 
  # NB: The counting error columns were removed here. As well as all flag and QC columns.
  dplyr::select(lon, lat, date, depth, temperature, theta, salinity, oxygen, aou, nitrate, nitrite, silicate, 
                phosphate, tco2, talk, fco2, fco2temp, phts25p0, phtsinsitutp, cfc11, pcfc11, cfc12, pcfc12, 
                cfc113, pcfc113, ccl4, pccl4, sf6, psf6, c13, c14, h3, he3, he, neon, o18, toc, doc, don, tdn, chla) %>% 
  pivot_longer(temperature:chla, names_to = "variable", values_to = "value") %>% 
  filter(!is.na(value), value != -9999) %>% 
  mutate(category = case_when(variable %in% c("temperature", "theta", "salinity") ~ "phys", TRUE ~ "chem"),
         variable = case_when(variable %in% c("temperature", "theta", "fco2temp") ~ paste0(variable," [°C]"),
                              variable %in% c("oxygen", "aou", "nitrate", "nitrite", "silicate", 
                                              "phosphate", "tco2", "talk") ~ paste0(variable," [μmol kg-1]"),
                              variable %in% c("fco2") ~ paste0(variable," [μatm]"),
                              variable %in% c("cfc11", "cfc12", "cfc113", "ccl4") ~ paste0(variable," [pmol kg-1]"),
                              variable %in% c("sf6") ~ paste0(variable," [fmol kg-1]"),
                              variable %in% c("pcfc11", "pcfc12", "pcfc113", "pccl4", "psf6") ~ paste0(variable," [ppt]"),
                              variable %in% c("c13", "c14", "o18") ~ paste0(variable," [‰]"),
                              variable %in% c("h3") ~ paste0(variable," [TU]"),
                              variable %in% c("he3") ~ paste0(variable," [%]"),
                              variable %in% c("he", "neon") ~ paste0(variable," [nmol kg-1]"),
                              variable %in% c("toc", "doc", "don", "tdn") ~ paste0(variable," [μmol L-1 d]"),
                              variable %in% c("chla") ~ paste0(variable," [μg kg-1 d]"),
                              TRUE ~ variable),
         date_accessed = as.Date("2022-10-19"),
         URL = "https://www.glodap.info",
         citation = "Lauvset, S. K., Lange, N., Tanhua, T., Bittig, H. C., Olsen, A., Kozyr, A., Álvarez, M., Becker, S., Brown, P. J., Carter, B. R., Cotrim da Cunha, L., Feely, R. A., van Heuven, S., Hoppema, M., Ishii, M., Jeansson, E., Jutterström, S., Jones, S. D., Karlsen, M. K., Lo Monaco, C., Michaelis, P., Murata, A., Pérez, F. F., Pfeil, B., Schirnick, C., Steinfeldt, R., Suzuki, T., Tilbrook, B., Velo, A., Wanninkhof, R., Woosley, R. J., and Key, R. M.: An updated version of the global interior ocean biogeochemical data product, GLODAPv2.2021, Earth Syst. Sci. Data, 13, 5565–5589, https://doi.org/10.5194/essd-13-5565-2021, 2021. ") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop")
# save(EU_GLODAP, file = "~/pCloudDrive/FACE-IT_data/EU_arctic/GLODAP_EU.RData")
# load("~/pCloudDrive/FACE-IT_data/EU_arctic/GLODAP_EU.RData")

# Combine and save
full_product_EU <- rbind(EU_zooplankton, EU_YMER, EU_Codispoti, EU_protists, EU_Popova, EU_green_fjords,
                         EU_IMR_spp_obs, EU_SOCAT, EU_GLODAP) %>% mutate(site = "EU")
data.table::fwrite(full_product_EU, "~/pCloudDrive/FACE-IT_data/EU_arctic/full_product_EU.csv")
save(full_product_EU, file = "~/pCloudDrive/FACE-IT_data/EU_arctic/full_product_EU.RData")
save(full_product_EU, file = "data/full_data/full_product_EU.RData")
rm(list = grep("EU_",names(.GlobalEnv),value = TRUE)); gc()


# Svalbard ----------------------------------------------------------------

## PG product --------------------------------------------------------------

# There is no PG product for Svalbard
# Rather, all PG products are loaded for each site to get any shared data points


## Full product ------------------------------------------------------------

# Load full EU file
if(!exists("full_product_EU")) load("data/full_data/full_product_EU.RData")

# Glacier mass balance
## NB: Updated annually in May
sval_MOSJ_cmb <- read_delim("~/pCloudDrive/FACE-IT_data/svalbard/cumulative-mass-balance-for-glaciers-in-svalbard.csv", delim = ";") %>% mutate(site = `Series name`, `Series name` = "cumulative mass balance")
sval_MOSJ_austre <- read_delim("~/pCloudDrive/FACE-IT_data/svalbard/austre-broggerbreen-mass-balance.csv", delim = ";") %>% mutate(site = "Austre Brøggerbreen")
sval_MOSJ_etonbreen <- read_delim("~/pCloudDrive/FACE-IT_data/svalbard/etonbreen-austfonna-mass-balance.csv", delim = ";") %>% mutate(site = "Etonbreen (Austfonna)")
sval_MOSJ_kongsvegen <- read_delim("~/pCloudDrive/FACE-IT_data/svalbard/kongsvegen-mass-balance.csv", delim = ";") %>% mutate(site = "Kongsvegen")
sval_MOSJ_kronebreen <- read_delim("~/pCloudDrive/FACE-IT_data/svalbard/kronebreenholtedahlfonna-mass-balance.csv", delim = ";") %>% mutate(site = "Kronebreen/Holtedahlfonna")
sval_MOSJ_midtre <- read_delim("~/pCloudDrive/FACE-IT_data/svalbard/midtre-lovenbreen-mass-balance.csv", delim = ";") %>% mutate(site = "Midtre Lovénbreen")
sval_MOSJ_glacier_mass <- bind_rows(sval_MOSJ_cmb, sval_MOSJ_austre, sval_MOSJ_etonbreen, sval_MOSJ_kongsvegen, sval_MOSJ_kronebreen, sval_MOSJ_midtre) %>% 
  pivot_longer(`1967`:`2020`, names_to = "year") %>% 
  dplyr::rename(variable = `Series name`) %>% 
  mutate(date = as.Date(paste0(year,"-12-31")),
         lon = case_when(site == "Austre Brøggerbreen" ~ 11.8438992,
                         site == "Kongsvegen" ~ 12.5523049,
                         site == "Midtre Lovénbreen" ~ 12.004464,
                         grepl("Kronebreen", site) ~ 13.264755,
                         grepl("Etonbreen", site) ~ 22.693686),
         lat = case_when(site == "Austre Brøggerbreen" ~ 78.9163196,
                         site == "Kongsvegen" ~ 78.8504786,
                         site == "Midtre Lovénbreen" ~ 78.824676,
                         grepl("Kronebreen", site) ~ 78.990132,
                         grepl("Etonbreen", site) ~ 79.705398),
         variable = case_when(grepl("with ", site) ~ paste0(variable, " with calving"),
                              grepl("without ", site) ~ paste0(variable, " without calving"),
                              TRUE ~ variable),
         variable = paste0(variable," [",Unit,"]"),
         URL = "https://www.mosj.no/en/climate/land/mass-balance-glaciers.html",
         citation = "Kohler, J. & Moholdt, G. (2021) Mass balance of Svalbard glaciers [Dataset]. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). Accessed: 2022-04-26. https://www.mosj.no/en/climate/land/mass-balance-glaciers.html",
         category = "cryo", depth = NA,
         date_accessed = as.Date("2022-04-28")) %>% 
  filter(!is.na(value)) %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop")
rm(sval_MOSJ_cmb, sval_MOSJ_austre, sval_MOSJ_etonbreen, sval_MOSJ_kongsvegen, sval_MOSJ_kronebreen, sval_MOSJ_midtre); gc()

# Glacier mass balance from Nature publication
## https://www.nature.com/articles/s41586-021-04314-4#data-availability
sval_Nature_glacier_mass_base <- read_csv("~/pCloudDrive/FACE-IT_data/svalbard/Geyman_et_al_Svalbard_glacier_data_final.csv", skip = 1)
sval_Nature_glacier_mass_latlon <- convert_epsg(x = sval_Nature_glacier_mass_base$X_center, 
                                                y = sval_Nature_glacier_mass_base$Y_center, epsg1 = "epsg:32633")
sval_Nature_glacier_mass <- left_join(sval_Nature_glacier_mass_base, sval_Nature_glacier_mass_latlon, 
                                      by = c("X_center" = "x", "Y_center" = "y")) %>% 
  dplyr::select(lon, lat, isTidewater:`mass_change_method2_oneSigma_1990_2010 (kg)`) %>% 
  pivot_longer(isTidewater:`mass_change_method2_oneSigma_1990_2010 (kg)`, names_to = "variable") %>% 
  mutate(variable = gsub("\\(m)", "[m]", variable),
         variable = gsub("\\(degree days)", "[degree days]", variable),
         variable = gsub("\\(mwe yr-1)", "[mwe/yr]", variable),
         variable = gsub("\\(m yr-1)", "[m/yr]", variable),
         variable = gsub("\\(m2)", "[m^2]", variable),
         variable = gsub("\\(m3)", "[m^3]", variable),
         variable = gsub("\\(kg)", "[kg]", variable),
         variable = gsub("\\(C)", "[°C]", variable),
         date = case_when(grepl("_1936 ", variable) ~ as.Date("1936-12-31"),
                          grepl("_1957 ", variable) ~ as.Date("1957-12-31"),
                          grepl("_1990 ", variable) ~ as.Date("1990-12-31"),
                          grepl("_2010 ", variable) ~ as.Date("1936-12-31"),
                          grepl("_2100_", variable) ~ as.Date("2100-12-31"),
                          grepl("slope_1936", variable) ~ as.Date("1936-12-31"),
                          grepl("slope_1990", variable) ~ as.Date("1990-12-31"),
                          grepl("slope_2010", variable) ~ as.Date("2010-12-31"),
                          grepl("1990_with", variable) ~ as.Date("1990-12-31"),
                          grepl("2010_with", variable) ~ as.Date("2010-12-31")),
         URL = "https://data.npolar.no/dataset/f6afca5c-6c95-4345-9e52-cfe2f24c7078",
         citation = "Geyman, E., van Pelt, W., Maloof, A., Aas, H. F., & Kohler, J. (2021). 1936/1938 DEM of Svalbard [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2021.f6afca5c",
         category = "cryo", depth = NA,
         date_accessed = as.Date("2022-05-10")) %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop")
rm(sval_Nature_glacier_mass_base, sval_Nature_glacier_mass_latlon); gc()

# Glacier area outlines
# glacier_area/
# sval_glacier_area # Not working with shape files of geomorphology

# Tidal glacier fronts
# tidewater/
# sval_tidal_glacier_front # Not working with shape files of geomorphology

# Marine terminating glacier fronts
# glacier_fronts/
# sval_marine_glacier_front # Not working with shape files of geomorphology

# Tidewater glacier ablation
# ncdf4::nc_open("~/pCloudDrive/FACE-IT_data/svalbard/Sval_Fronts_data.nc")
sval_tidewater_ablation <- tidync("~/pCloudDrive/FACE-IT_data/svalbard/Sval_Fronts_data.nc") %>% 
  hyper_tibble() %>% 
  dplyr::rename(lon = longitude, lat = latitude) %>% 
  pivot_longer(thickness1:trajectory, names_to = "variable") %>% 
  mutate(date1 = as.Date(t1, origin = "0000-01-01"), 
         date2 = as.Date(t2, origin = "0000-01-01"),
         # NB: These values are generally taken over a one year period
         # Here we take the end date as the date value vor each datum
         date = date2,
         # NB: Not processing uncertainty values
         variable = case_when(variable == "thickness1" ~ "glacier ice thickness front [m]",
                              variable == "thickness2" ~ "glacier ice thickness flux gate [m]",
                              variable == "area_rate" ~ "glacier ice area rate [km2 yr-1]",
                              variable == "velocity" ~ "glacier ice surface velocity [m yr-1]",
                              variable == "mass_rate" ~ "glacier mass balance front [Gt yr-1]",
                              variable == "discharge" ~ "glacier mass balance calving [Gt yr-1]",
                              variable == "front_abl" ~ "glacier mass balance ablation [Gt yr-1]"),
         URL = "https://data.npolar.no/dataset/d1d08fac-622a-4ba6-a911-f369b4fb67a0",
         citation = " Moholdt, G., Maton, J., & Kohler, J. (2021). Frontal ablation of Svalbard tidewater glaciers [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2021.d1d08fac",
         category = "cryo", depth = NA,
         date_accessed = as.Date("2022-04-26")) %>% 
  filter(date2 > "1901-01-01", !is.na(variable)) %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# Surface meteorology
# See 'README_N-ICE_metData_v2.txt' for detailed info
# ncdf4::nc_open("~/pCloudDrive/FACE-IT_data/svalbard/N-ICE_metData_v2.nc")
sval_NICE <- tidync("~/pCloudDrive/FACE-IT_data/svalbard/N-ICE_metData_v2.nc") %>% 
  hyper_tibble() %>% 
  dplyr::rename(lon = longitude, lat = latitude) %>% 
  unite(year, month, day, sep = "-", remove = TRUE, col = date) %>% 
  dplyr::select(lon, lat, date, everything(), -second, -minute, -hour, -time, -unix_time,
                -wind_speed_10m_flag, -wind_from_direction_10m_flag, # _flag: don't need to know about wind data interpolation status
                -air_temperature_fill, -air_pressure_at_sea_level_fill) %>% # _fill: data seem problematic
  group_by(lon, lat, date) %>% 
  summarise(air_pressure_at_sea_level = mean(air_pressure_at_sea_level, na.rm = T),
            air_temperature_2m = mean(air_temperature_2m, na.rm = T),
            air_temperature_4m = mean(air_temperature_4m, na.rm = T),
            air_temperature_10m = mean(air_temperature_10m, na.rm = T),
            relative_humidity_2m = mean(relative_humidity_2m, na.rm = T),
            relative_humidity_4m = mean(relative_humidity_4m, na.rm = T),
            relative_humidity_10m = mean(relative_humidity_10m, na.rm = T),
            relative_humidity_ice_2m = mean(relative_humidity_ice_2m, na.rm = T),
            relative_humidity_ice_4m = mean(relative_humidity_ice_4m, na.rm = T),
            relative_humidity_ice_10m = mean(relative_humidity_ice_10m, na.rm = T),
            wind_speed_2m = mean(wind_speed_2m, na.rm = T),
            wind_speed_4m = mean(wind_speed_4m, na.rm = T),
            wind_speed_10m = mean(wind_speed_10m, na.rm = T),
            wind_from_direction_2m = as.numeric(round(mean.circular(circular(wind_from_direction_2m, units = "degrees"), na.rm = T))),
            wind_from_direction_4m = as.numeric(round(mean.circular(circular(wind_from_direction_4m, units = "degrees"), na.rm = T))),
            wind_from_direction_10m = as.numeric(round(mean.circular(circular(wind_from_direction_10m, units = "degrees"), na.rm = T))), 
            .groups = "drop") %>% 
  mutate(wind_from_direction_2m = case_when(wind_from_direction_2m < 0 ~ wind_from_direction_2m + 360, TRUE ~ wind_from_direction_2m),
         wind_from_direction_4m = case_when(wind_from_direction_4m < 0 ~ wind_from_direction_4m + 360, TRUE ~ wind_from_direction_4m),
         wind_from_direction_10m = case_when(wind_from_direction_10m < 0 ~ wind_from_direction_10m + 360, TRUE ~ wind_from_direction_10m)) %>% 
  pivot_longer(air_pressure_at_sea_level:wind_from_direction_10m, names_to = "variable") %>% 
  filter(!is.na(value)) %>% 
  mutate(URL = "https://data.npolar.no/dataset/056a61d1-d089-483a-a256-081de4f3308d",
         citation = "Hudson, S. R., Cohen, L., & Walden, V. (2015). N-ICE2015 surface meteorology [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2015.056a61d1",
         category = "phys",
         depth = case_when(grepl("_at_sea_level", variable) ~ 0,
                           grepl("_2m", variable) ~ -2,
                           grepl("_4m" , variable) ~ -4,
                           grepl("_10m" , variable) ~ -10),
         variable = str_remove(variable, "_at_sea_level|_2m|_4m|_10m"),
         variable = case_when(grepl("air_temperature", variable) ~ "TTT [°C]",
                              grepl("air_pressure", variable) ~ "air_pressure [hPa]",
                              grepl("relative_humidity", variable) ~ paste0(variable," [%]"),
                              grepl("wind_speed", variable) ~ "wind_speed [m/s]",
                              grepl("wind_from_direction", variable) ~ "wind_from_direction [°]",
                              TRUE ~ variable),
         value = case_when(variable == "TTT [°C]" ~ value-273.15, TRUE ~ value), 
         date_accessed = as.Date("2021-02-11")) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)

# UNIS database
## NB: This takes a long time due to file size
# ncdf4::nc_open("~/pCloudDrive/FACE-IT_data/svalbard/CTD_all_1876-2019.nc")
sval_UNIS_nc_dat <- ncdf4::nc_open("~/pCloudDrive/FACE-IT_data/svalbard/CTD_all_1876-2019.nc")
# sval_UNIS_OWNER <- distinct(data.frame(ncdf4::ncvar_get(sval_UNIS_nc_dat, varid = "OWNER")))
# sval_UNIS_CRUISE <- distinct(data.frame(ncdf4::ncvar_get(sval_UNIS_nc_dat, varid = "CRUISE")))
sval_UNIS_TEMP <- CTD_to_long(sval_UNIS_nc_dat, "TEMP"); gc()
sval_UNIS_PSAL <- CTD_to_long(sval_UNIS_nc_dat, "PSAL"); gc()
sval_UNIS_CNDC <- CTD_to_long(sval_UNIS_nc_dat, "CNDC"); gc()
sval_UNIS_database <- full_join(sval_UNIS_TEMP, sval_UNIS_PSAL, by = c("lon", "lat", "date", "depth")) %>% 
  full_join(sval_UNIS_CNDC, by = c("lon", "lat", "date", "depth")) %>% 
  pivot_longer(temp:cndc, names_to = "variable", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(URL = "https://data.npolar.no/dataset/39d9f0f9-af12-420c-a879-10990df2e22d",
         citation = "Skogseth, R., Ellingsen, P., Berge, J., Cottier, F., Falk-Petersen, S., Ivanov, B., … Vader, A. (2019). UNIS hydrographic database [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/unis-hydrography",
         units = case_when(variable == "temp" ~ "°C",
                           variable == "psal" ~ "1e-3",
                           variable == "cndc" ~ "S m-1"),
         variable = paste0(variable," [", units,"]"),
         category = "phys",
         date_accessed = as.Date("2021-03-12")) %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value); gc()
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop"); gc()
rm(sval_UNIS_nc_dat, sval_UNIS_TEMP, sval_UNIS_PSAL, sval_UNIS_CNDC); gc()

# Seabird database
# sval_seabird_database # Not on pCloud

# Protection of sites
# https://data.npolar.no/dataset/117acc0c-7d6e-5c58-bb80-bb220780f406
# Not available online
# sval_protection

# N-ICE21015 many data products
# https://data.npolar.no/dataset/7f7e56d0-9e70-4363-b37d-17915e09a935
# Files listed at the bottom of the page
# Generally these files are outside of any study region
# sval_NICE

# Fast ice persistence
# svalbard_fastice_persistency_2014.tif; svalbard_fastice_persistency_2015.tif; svalbard_fastice_persistency_2016.tif
# sval_fast # Not working with shape files of geomorphology

# Biogeochemistry
sval_biogeochemistry <- bind_rows(read_delim("~/pCloudDrive/FACE-IT_data/svalbard/2009-2013-pigments-api-v1.tsv", delim = "\t"),
                                  read_delim("~/pCloudDrive/FACE-IT_data/svalbard/2010-2013-nutrients-api-v1.tsv", delim = "\t")) %>% 
  dplyr::rename(date = eventDate, lon = decimalLongitude, lat = decimalLatitude) %>% 
  mutate(depth = case_when(!is.na(minimumDepthInMeters) ~ (minimumDepthInMeters+maximumDepthInMeters)/2,
                           TRUE ~ maximumDepthInMeters),
         date = as.Date(date)) %>% 
  dplyr::select(lon, lat, date, depth, chlorophyll_a, phaeopigment, nox:nitrate_stddev) %>% 
  pivot_longer(chlorophyll_a:nitrate_stddev, names_to = "variable", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(category = case_when(variable %in% c("chlorophyll_a", "phaeopigment") ~ "bio",
                              TRUE ~ "chem"),
         variable = case_when(variable == "chlorophyll_a" ~ "Chla [µg/l]",  
                              variable == "nox" ~ "diss_oxygen [mg/l]",
                              variable == "nox_stddev" ~ "diss_oxygen stddev [mg/l]",
                              variable == "phosphate" ~ "PO4 [µmol/l]",
                              variable == "phosphate_stddev" ~ "PO4 stddev [µmol/l]", 
                              variable == "silicate" ~ "SiO4 [µmol/l]",
                              variable == "silicate_stddev" ~ "SiO4 stddev [µmol/l]",
                              variable == "nitrite" ~ "NO2 [µmol/l]",
                              variable == "nitrite_stddev" ~ "NO2 stddev [µmol/l]",
                              variable == "nitrate" ~ "NO3 [µmol/l]",
                              variable == "nitrate_stddev" ~ "NO3 stddev [µmol/l]",
                              variable == "ammonium" ~ "NH4 [µmol/l]", 
                              # "phaeopigment"
                              TRUE ~ variable),
         date_accessed = as.Date("2021-02-11"),
         URL = "https://data.npolar.no/dataset/c9de2d1f-54c1-49ca-b58f-a04cf5decca5",
         citation = "Norwegian Polar Institute (2020). Marine biogeochemistry [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2020.c9de2d1f") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop")

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
         category = "soc", variable = paste0("pop [",settlement,"]"),
         depth = NA, lon = NA, lat = NA, .keep = "unused") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
# write_csv(sval_pop, "~/pCloudDrive/FACE-IT_data/svalbard/svalbard_population_stats_full.csv")

# Svalbard tourist arrivals
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
  # group_by(URL, type, residence, year) %>% # NB: Use this to create annual averages. Not currently desired.
  # summarise(value = sum(value, na.rm = T), .groups = "drop") %>% 
  bind_rows(sval_tour_arrival_hist) %>% 
  mutate(date_accessed = as.Date("2021-09-30"),
         citation = "Statistics Norway. www.ssb.no. Accessed 2021-09-30",
         category = "soc", variable = paste0("arrival [",type," - ",residence,"]"),
         depth = NA, lon = NA, lat = NA, .keep = "unused") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
# write_csv(sval_tour_arrival, "~/pCloudDrive/FACE-IT_data/svalbard/svalbard_tourist_arrivals_full.csv")

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
  # group_by(URL, type, residence, date) %>%  # NB: Use this to create annual averages. Not currently desired.
  # summarise(value = sum(value, na.rm = T), .groups = "drop") %>% 
  bind_rows(sval_guest_night_hist) %>% 
  mutate(date_accessed = as.Date("2021-09-30"),
         citation = "Statistics Norway. www.ssb.no. Accessed 2021-09-30",
         category = "soc", variable = paste0("guest night [",type," - ",residence,"]"),
         depth = NA, lon = NA, lat = NA, .keep = "unused") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
# write_csv(sval_guest_night, "~/pCloudDrive/FACE-IT_data/svalbard/svalbard_guest_nights_full.csv")

# AIS data
sval_AIS <- read_csv("~/pCloudDrive/FACE-IT_data/svalbard/AIS_aggregated.csv") %>% 
  pivot_longer(`Nautical miles`:`Average speed (knots)`, names_to = "var", values_to = "value") %>% 
  mutate(date = as.Date(paste0(Year,"-12-31")),
         depth = 0, # It may be better to list this as NA 
         lon = NA, lat = NA, 
         date_accessed = as.Date("2020-09-30"),
         variable = paste0(Area," [",var,"]"),
         # TODO: Are ship CO2 emissions for social or chemical data?
         category = case_when(grepl("co2|nox|sox", variable, ignore.case = T) ~ "chem",
                              grepl("PM", variable, ignore.case = T) ~ "phys", TRUE ~ "soc"),
         URL = "Received directly from Morten Simonsen",
         citation = "Simonsen, M., Walnum, H. J., & Gössling, S. (2018). Model for estimation of fuel consumption of cruise ships. Energies, 11(5), 1059.") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)

# Combine and save
full_product_sval <- rbind(sval_MOSJ_glacier_mass, sval_Nature_glacier_mass, 
                           sval_tidewater_ablation, sval_NICE, sval_UNIS_database, sval_biogeochemistry,
                           sval_pop, sval_tour_arrival, sval_guest_night, sval_AIS) %>% 
  rbind(filter(dplyr::select(full_product_EU, -site), lon >= bbox_sval[1], lon <= bbox_sval[2], lat >= bbox_sval[3], lat <= bbox_sval[4])) %>% 
  distinct() %>% mutate(site = "sval")
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
  # Manually remove problematic files - no need
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
pg_kong_ALL <- rbind(pg_kong_Cryosphere, pg_kong_Physical, pg_kong_Chemistry) %>% mutate(site = "kong")
data.table::fwrite(pg_kong_ALL, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_ALL.csv")
save(pg_kong_ALL, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_ALL.RData")

# Load data to investigate
# pg_kong_ALL <- data.table::fread("~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_ALL.csv")

# Check that all columns were used
# colnames(pg_kong_clean)[!colnames(pg_kong_clean) %in% unique(pg_kong_ALL$variable)]

# Clean up
rm(list = grep("pg_kong",names(.GlobalEnv),value = TRUE)); gc()


## Full product ------------------------------------------------------------

# Load full Svalbard file
## NB: This contains the full EU Arctic data
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
         variable = "ice cover [%]",
         category = "cryo",
         date_accessed = as.Date("2021-02-11"),
         URL = "https://data.npolar.no/dataset/74c7b236-b94d-48c5-a665-ffcd54e8e1b7",
         citation = "Gerland, S., & Pavlova, O. (2020). Sea ice coverage in inner Kongsfjorden, Svalbard, 2003-2019, version 1.0 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2020.74c7b236") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)

## Sea ice cover from shape files
### NB: Not loaded as these are shape files
# kong_sea_ice_shp <- read_delim("~/pCloudDrive/FACE-IT_data/kongsfjorden/IceMap_KF_2003to2021.zip")

## Zooplankton abundance and species
kong_zoo_data <- read_csv("~/pCloudDrive/FACE-IT_data/kongsfjorden/kf_zooplankton_abundance_data.csv") %>% 
  pivot_longer(CALfinM:SCYPZlar, names_to = "sps", values_to = "value") %>%
  dplyr::rename("id" = "...1") %>% 
  left_join(read_csv("~/pCloudDrive/FACE-IT_data/kongsfjorden/kf_zooplankton_sampling_meta.csv"), by = c("id")) %>% 
  left_join(read_csv("~/pCloudDrive/FACE-IT_data/kongsfjorden/kf_zooplankton_species_meta.csv"), by = c("sps" = "id")) %>% 
  dplyr::rename(lon = longitude, lat = latitude) %>% 
  mutate(variable = case_when(!is.na(stage) ~ paste0(species," (",stage,")"), TRUE ~ species),
         # value = value*biomass_conv, # This changes the values from ind/m3 to biomass; see Hop et al. 2019
         variable = paste0(variable, " [ind/m3]"),
         category = "bio",
         date_accessed = as.Date("2021-02-11"),
         URL = "https://data.npolar.no/dataset/94b29b16-b03b-47d7-bfbc-1c3c4f7060d2",
         citation = "Hop H, Wold A, Vihtakari M, Daase M, Kwasniewski S, Gluchowska M, Lischka S, Buchholz F, Falk-Petersen S (2019) Zooplankton in Kongsfjorden (1996-2016) in relation to climate change. In: The ecosystem of Kongsfjorden, Svalbard (eds. Hop H, Wiencke C), Advances in Polar Ecology, Springer Verlag.") %>% 
  filter(!is.na(value)) %>%
  group_by(date_accessed, URL, citation, lon, lat, date, category, variable, value) %>% 
  summarise(depth = (from+to)/2, .groups = "drop") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>%
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
  mutate(Taxon_full = paste0(Taxon_full," [cells/l]")) %>% 
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
  dplyr::select(lon, lat, date, depth, P:Chla, `Dinobryon spp. cyst [cells/l]`:`Heterocapsa  sp. [cells/l]`) %>%
  pivot_longer(P:`Heterocapsa  sp. [cells/l]`, names_to = "variable", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(category = case_when(variable %in% c("P", "NO2", "NO3", "Si", "NH4") ~ "chem", # May want to include "Chla
                              TRUE ~ "bio"),
         variable = case_when(variable == "P" ~ "P [µmol/l]", 
                              variable == "NO2" ~ "NO2 [µmol/l]", 
                              variable == "NO3" ~ "NO3 [µmol/l]", 
                              variable == "Si" ~ "Si [µmol/l]", 
                              variable == "NH4" ~ "NH4 [µmol/l]", 
                              variable == "Chla" ~ "Chla [µg/l]",
                              TRUE ~ variable),
         date_accessed = as.Date("2021-02-11"),
         URL = "https://data.npolar.no/dataset/2bff82dc-22b9-41c0-8348-220e7d6ca4f4",
         citation = "Hegseth EN, Assmy P, Wiktor JM, Wiktor Jr. JM, Kristiansen S, Leu E, Tverberg V, Gabrielsen TM, Skogseth R and Cottier F (2019) Phytoplankton Seasonal Dynamics in Kongsfjorden, Svalbard and the Adjacent Shelf. In: The ecosystem of Kongsfjorden, Svalbard (eds. Hop H, Wiencke C), Advances in Polar Ecology, Springer Verlag.") %>% 
  distinct() %>%
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop")
rm(kong_protist_nutrient_chla_1, kong_protist_nutrient_chla_2, kong_protist_nutrient_chla_3); gc()

## CTD sampling data
# ncdf4::nc_open("~/pCloudDrive/FACE-IT_data/kongsfjorden/Kongsfjorden_ctd_1906_2017.nc")
kong_CTD_nc_dat <- ncdf4::nc_open("~/pCloudDrive/FACE-IT_data/kongsfjorden/Kongsfjorden_ctd_1906_2017.nc")
kong_CTD_TEMP <- CTD_to_long(kong_CTD_nc_dat, "TEMP")
kong_CTD_PSAL <- CTD_to_long(kong_CTD_nc_dat, "PSAL")
kong_CTD_CNDC <- CTD_to_long(kong_CTD_nc_dat, "CNDC")
kong_CTD_database <- left_join(kong_CTD_TEMP, kong_CTD_PSAL, by = c("lon", "lat", "date", "depth")) %>% 
  left_join(kong_CTD_CNDC, by = c("lon", "lat", "date", "depth")) %>% 
  pivot_longer(temp:cndc, names_to = "variable", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(URL = "https://data.npolar.no/dataset/074a215c-d1df-47a9-bea7-e0fcc37273c6",
         citation = "Skogseth, R., Tverberg, V., Walczowski, W., & Sundfjord, A. (2019). Kongsfjorden Transect CTD data 1906-2016 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.074a215c",
         date = as.Date(date),
         depth = as.numeric(depth),
         units = case_when(variable == "temp" ~ "°C",
                           variable == "psal" ~ "1e-3",
                           variable == "cndc" ~ "S m-1"),
         variable = paste0(variable," [", units,"]"),
         category = "phys",
         date_accessed = as.Date("2021-02-11")) %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop")
rm(kong_CTD_nc_dat, kong_CTD_TEMP, kong_CTD_PSAL, kong_CTD_CNDC); gc()

## CO2 data
kong_CTD_CO2 <- read_csv("~/pCloudDrive/FACE-IT_data/kongsfjorden/Kongsfjorden_Marine_CO2_system_2012_to_2014.csv") %>% 
  dplyr::rename(date = `yyyy-mm-dd`, lon = Longitude, lat = Latitude, depth = `Depth [m]`, `Temp [°C]` = `Temperature [C]`) %>% 
  dplyr::select(date:`DIC [µmol/kg]`, -`Bot.Depth [m]`) %>% 
  pivot_longer(Salinity:`DIC [µmol/kg]`, names_to = "variable", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(date = as.Date(date),
         category = case_when(variable == "Salinity" ~ "phys",
                              variable == "Temp [°C]" ~ "phys",
                              variable == "AT [µmol/kg]" ~ "chem",
                              variable == "DIC [µmol/kg]" ~ "chem"),
         date_accessed = as.Date("2021-02-11"),
         URL = "https://data.npolar.no/dataset/e53eae53-147a-45df-b473-917bb5ba1ed4",
         citation = "Fransson, A., & Chierici, M. (2019). Marine CO2 system data for the Svalbard fjord Kongsfjorden and the West-Spitsbergen shelf in July 2012-2014 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.e53eae53") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)

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
  pivot_longer(sw_out_wpm2_avg:ws_2_wvc1, names_to = "variable", values_to = "value") %>% 
  mutate(lon = 13.15, lat = 78.78, depth = NA, 
         category =  "phys",
         date_accessed = as.Date("2021-03-02"),
         URL = "https://data.npolar.no/dataset/5dc31930-0922-4483-a1df-6f48af9e371b",
         citation = "Kohler, J., Hudson, S. R., & Obleitner, F. (2017). Automatic weather station data from Kongsvegen, Ny-Ålesund [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2017.5dc31930") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)

## GFI mooring
kong_mooring_GFI <- plyr::ldply(dir("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_GFI", full.names = T), load_GFI, .parallel = T) %>% 
  mutate(date_accessed = as.Date("2021-08-04"), .before = 1)

## Ferry box data
kong_ferry <- readRDS("~/pCloudDrive/FACE-IT_data/kongsfjorden/kong_ferry.rds") %>%
  dplyr::rename(date = datetime, depth = `pressure [dbar]`) %>% 
  pivot_longer(`s_insitu [unit]`:`pH_sf [total scale]`, names_to = "variable") %>% 
  filter(!is.na(value)) %>%
  mutate(lon = 11.920027777777777, lat = 78.93065833333334,
         date = as.Date(date),
         depth = case_when(grepl("_fb", variable) ~ 11,
                           grepl("_11m", variable) ~ 11,
                           TRUE ~ depth),
         category = case_when(grepl("co2|ph_|phint|phEXT", variable, ignore.case = T) ~ "chem",
                              variable %in% c("at", "at_calc", "nh4", "NH4", "NO2","no3", "NO3", 
                                              "no3no2", "NO3NO2", "po4", "PO4", "si", "Si", "k",
                                              "ta_inst") ~ "chem",
                              TRUE ~ "phys"), 
         URL = "File provided by Jean-Pierre Gattuso",
         date_accessed = as.Date("2021-08-12"),
         citation = "Gattuso, J.-P., Alliouane S., Fischer P. & Gattuso J.-P., in prep. Multiyear, high-frequency time series of the carbonate system in a coastal high Arctic station (Spitsbergen)") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")

## SAMS mooring data
kong_mooring_SAMS <- plyr::ldply(dir("~/pCloudDrive/FACE-IT_data/kongsfjorden/mooring_SAMS/", full.names = T), load_SAMS, .parallel = T) %>% 
  mutate(date_accessed = as.Date("2021-10-21"), .before = 1) %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")

## Ny-Alesund ship arrivals
kong_ship_arrivals <- read_csv("~/pCloudDrive/FACE-IT_data/kongsfjorden/kong_ship_arrivals.csv") %>% 
  pivot_longer(January:December, names_to = "month", values_to = "value") %>% 
  mutate(variable = case_when(type == "PAX" ~ "Tourist arrivals [count]",
                              type != "PAX" ~ paste0("Vessels [",type,"]")),
         category = "soc",
         month = match(month, month.name),
         date = as.Date(paste0(year,"-",month,"-01")),
         depth = NA, lon = 11.92, lat = 78.93,
         URL = "https://port.kingsbay.no/statistics/",
         date_accessed = as.Date("2021-10-18"),
         citation = "Havenstrøm, E. (2021). Port calls in Kings Bay. https://port.kingsbay.no/statistics") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop") 

## Bremen data
# Daten/ELUV
# Daten/Licht
# Daten/Temperatur
# ELUV data not loaded because they are from an experiment
# "~/pCloudDrive/restricted_data/Bremen/Daten/ELUV", "~/pCloudDrive/restricted_data/Bremen/Daten/Licht",
# "~/pCloudDrive/restricted_data/Bremen/Daten/Temperatur/EluvFjordTemperatur.csv"
# The code used to load these files may be found in the history before 2022-03-23

# DATEN4/CTD
kong_CTD_DATEN4 <- map_dfr(dir("~/pCloudDrive/restricted_data/Bremen/DATEN4/CTD", full.names = T), load_CTD_DATEN) %>% 
  dplyr::rename(depth = `press [dbar]`, `Temp [°C]` = `temp [°C]`) %>% 
  filter(`sal [ppt]` >= 1, depth >= 0) %>% # Sketchy surface values
  mutate(date = as.Date(date)) %>%
  group_by(lon, lat, date, depth) %>% 
  # NB: Changes from 0 - 360 to -180 - 180
  mutate(`dir [°]` = as.numeric(mean.circular(circular(`dir [°]`, units = "degrees")))) %>% 
  pivot_longer(`Temp [°C]`:`dir [°]`, names_to = "variable") %>% 
  filter(!is.na(value)) %>% 
  mutate(category = "phys",
         URL = NA,
         date_accessed = as.Date("2022-03-02"),
         citation = "Bischof, K., Hanelt, D., TuÈg, H., Karsten, U., Brouwer, P. E., & Wiencke, C. (1998). Acclimation of brown algal photosynthesis to ultraviolet radiation in Arctic coastal waters (Spitsbergen, Norway). Polar Biology, 20(6), 388-395.") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop") 
                           
# DATEN4/MINI-PAM
# NB: Not loaded as it only appears to have PAR values that look incorrect.

# LICHT
kong_LICHT <- read_csv("~/pCloudDrive/restricted_data/Bremen/LICHT/combined.csv") %>% 
  dplyr::rename(`PAR [µmol m-2 s-1]` = PAR) %>% 
  mutate(lon = case_when(site == "Blomstrand" ~ 12.0444,
                         site == "Hansneset" ~ 11.9872),
         lat = case_when(site == "Blomstrand" ~ 78.9611,
                         site == "Hansneset" ~ 78.9958)) %>% 
  dplyr::select(lon, lat, date, depth, UVA, UVB, `PAR [µmol m-2 s-1]`) %>% 
  pivot_longer(UVA:`PAR [µmol m-2 s-1]`, names_to = "variable") %>% 
  filter(!is.na(value)) %>% 
  mutate(date = as.Date(date, format = "%d/%m/%y"),
         category = "phys",
         URL = NA,
         date_accessed = as.Date("2022-03-02"),
         citation = "Bischof, K., Hanelt, D., TuÈg, H., Karsten, U., Brouwer, P. E., & Wiencke, C. (1998). Acclimation of brown algal photosynthesis to ultraviolet radiation in Arctic coastal waters (Spitsbergen, Norway). Polar Biology, 20(6), 388-395.") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# Light Data
kong_light_Laeseke <- read_csv("~/pCloudDrive/restricted_data/Bremen/Light Data/Laeseke_light_data.csv", skip = 9) %>% 
  mutate(date = as.Date("2015-08-17"), lon = 11.9872, lat = 78.9958) %>% 
  dplyr::rename(depth = `Depth [m]`) %>% 
  dplyr::select(lon, lat, date, depth, `UV-B [mW*m^2]`, `UV-A [mW*m^2]`, `PAR [mW*m^2]`) %>% 
  pivot_longer(`UV-B [mW*m^2]`:`PAR [mW*m^2]`, names_to = "variable") %>% 
  mutate(category = "phys",
         variable = str_replace(variable, "mW", "W"),
         value = value/1000, # Convert to W*m^2
         # Valu of ~2.5 for conversion taken from Morel and Smith (1974): https://aslopubs.onlinelibrary.wiley.com/doi/abs/10.4319/lo.1974.19.4.0591
         value = case_when(grepl("PAR", variable) ~ value*2.5, TRUE ~ value), # Convert to `PAR [µmol m-2 s-1]`
         variable = case_when(grepl("PAR", variable) ~ "PAR [µmol m-2 s-1]", TRUE ~ variable),
         URL = NA,
         date_accessed = as.Date("2022-03-02"),
         citation = "Laeseke, P., Bartsch, I., & Bischof, K. (2019). Effects of kelp canopy on underwater light climate and viability of brown algal spores in Kongsfjorden (Spitsbergen). Polar Biology, 42(8), 1511-1527.") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# Light Data/Lydia...
# Not loaded as these data are measurements of difference in PAR in and out of the river outflow near Ny-Alesund
# While interesting, these data do not have lon/lat coords so are difficult to incorporate with everything else

## Light data from Inka
# NB: The sensors get dirty throughout the year so values after the winter dark period are best to remove
kong_light_Inka_1 <- read_csv("~/pCloudDrive/restricted_data/Inka_PAR/PAR_Hansneset_KelpForest_10M_above kelp canopy_July2012-June2013_final.csv") %>% 
  slice(1:15999) %>% mutate(date = as.Date(date, format = "%d/%m/%Y"), depth = 10) %>% 
  dplyr::select(-`...4`) %>% pivot_longer(`PAR [umol m-2 s-1]`, names_to = "variable")
kong_light_Inka_2 <- read_csv("~/pCloudDrive/restricted_data/Inka_PAR/PAR_Hansneset_KelpForest_15M_3Jul2012-13Jun2013_final.csv") %>% 
  slice(1:16074) %>% mutate(date = as.Date(date, format = "%d/%m/%Y"), depth = 15) %>% pivot_longer(`PAR [umol m-2 s-1]`, names_to = "variable")
klib <- read_csv("~/pCloudDrive/restricted_data/Inka_PAR/PAR_Hansneset_KelpForest_All_Depths_4July-31July2012_final.csv") 
kong_light_Inka_3 <- rbind(data.frame(date = klib$date, time = klib$time...2,
                                      value = klib$`2.3 UNTEN PAR [µmol m-2 s-1]`, variable = "2.3 UNTEN PAR [µmol m-2 s-1]"),
                           data.frame(date = klib$date, time = klib$time...4,
                                      value = klib$`1.7 OBEN PAR [µmol m-2 s-1]`, variable = "1.7 OBEN PAR [µmol m-2 s-1]"),
                           data.frame(date = klib$date, time = klib$time...6,
                                      value = klib$`4.2 OBEN PAR [µmol m-2 s-1]`, variable = "4.2 OBEN PAR [µmol m-2 s-1]"),
                           data.frame(date = klib$date, time = klib$time...8,
                                      value = klib$`4.8 UNTEN PAR [µmol m-2 s-1]`, variable = "4.8 UNTEN PAR [µmol m-2 s-1]"),
                           data.frame(date = klib$date, time = klib$time...10,
                                      value = klib$`9.2 OBEN PAR [µmol m-2 s-1]`, variable = "9.2 OBEN PAR [µmol m-2 s-1]"),
                           data.frame(date = klib$date, time = klib$time...12,
                                      value = klib$`9.8 UNTEN PAR [µmol m-2 s-1]`, variable = "9.8 UNTEN PAR [µmol m-2 s-1]"),
                           data.frame(date = klib$date, time = klib$time...14,
                                      value = klib$`14.8 PAR [µmol m-2 s-1]`, variable = "14.8 PAR [µmol m-2 s-1]")) %>% 
  mutate(depth = case_when(grepl("14.8", variable) ~ 14.8, grepl("1.7", variable) ~ 1.7, grepl("2.3", variable) ~ 2.3, 
                           grepl("4.2", variable) ~ 4.2, grepl("4.8", variable) ~ 4.8, grepl("9.2", variable) ~ 9.2, grepl("9.8", variable) ~ 9.8),
         date = as.Date(date, format = "%d/%m/%Y"),
         variable = case_when(grepl("OBEN", variable) ~ "PAR above canopy [µmol m-2 s-1]",
                              grepl("UNTEN", variable) ~ "PAR below canopy [µmol m-2 s-1]",
                              TRUE ~ "PAR [µmol m-2 s-1]")) %>% 
  filter(!is.na(time)) # Missing time is also 0 values
kong_light_Inka_hourly <- bind_rows(kong_light_Inka_1, kong_light_Inka_2, kong_light_Inka_3) %>% 
  mutate(note = case_when(grepl("above", variable) ~ "Above canopy",
                              grepl("below", variable) ~ "Below canopy",
                              TRUE ~ "No canopy"),
         `longitude [°E]` = 11.9872, `latitude [°N]` = 78.9958,
         date = paste(date, time, sep = "T")) %>% arrange(depth) %>% 
  dplyr::rename(`PAR [µmol m-2 s-1]` = value, `depth [m]` = depth, `date/time [UTC+0]` = date) %>% 
  dplyr::select(`longitude [°E]`, `latitude [°N]`, `date/time [UTC+0]`, `depth [m]`, `PAR [µmol m-2 s-1]`, note)
# write_delim(kong_light_Inka_hourly, "~/pCloudDrive/restricted_data/Inka_PAR/Bartsch_PAR_Hansneset.csv", delim = "\t")
# kong_light_Inka_PG <- read_delim("~/Downloads/Kongsfjorden_Hansneset_PAR.tab", delim = "\t", skip = 20) # The published data
kong_light_Inka <- bind_rows(kong_light_Inka_1, kong_light_Inka_2, kong_light_Inka_3) %>% 
  mutate(lon = 11.9872, lat = 78.9958,
         variable = case_when(variable == "PAR [umol m-2 s-1]" ~ "PAR [µmol m-2 s-1]", TRUE ~ variable),
         category = "phys", 
         date_accessed = as.Date("2022-03-24"),
         URL = "Received directly from Inka Bartsch", 
         citation = "Bartsch, I., Paar, M., Fredriksen, S., Schwanitz, M., Daniel, C., Hop, H., & Wiencke, C. (2016). Changes in kelp forest biomass and depth distribution in Kongsfjorden, Svalbard, between 1996–1998 and 2012–2014 reflect Arctic warming. Polar Biology, 39(11), 2021-2036.") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")
rm(kong_light_Inka_1, kong_light_Inka_2, kong_light_Inka_3, klib, kong_light_Inka_hourly); gc()

# PAR data from Dieter Hanelt
## NB: The coords that are slightly different from Hansneset are a bit of a guess RE advise from Dieter
## NB: There are two different sampling profiles on the same day with the same coords provided
## This is because the time of day of sampling is different
kong_PAR_Dieter <- read_csv("~/pCloudDrive/FACE-IT_data/kongsfjorden/Messung_Hansneset_PAR.csv") %>% 
  dplyr::select(-Air, - Ratio) %>% dplyr::rename(value = UW) %>% 
  mutate(variable = "PAR [umol m-2 s-1]",
         category = "phys", 
         date_accessed = as.Date("2022-04-19"),
         URL = "Received directly from Dieter Hanelt", 
         citation = "Pavlov, A. K., Leu, E., Hanelt, D., Bartsch, I., Karsten, U., Hudson, S. R., ... & Granskog, M. A. (2019). The underwater light climate in Kongsfjorden and its ecological implications. In The ecosystem of Kongsfjorden, Svalbard (pp. 137-170). Springer, Cham.") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# Light and kelp data from Sarina's 2022 paper
## NB: Not added to meta-database or data product; waiting for manuscript publication and PANGAEA publication
kong_NiedzKelp <- read_csv("~/pCloudDrive/restricted_data/Niedzwiedz/dataKelp.csv") %>% 
  fill(Length, Width, Stipe.Length, Area.discs, µmol.0, µmol.0.h.cm, µmol.24, µmol.24.h.cm, 
       Comp.irr, Comp.irr.log, Chla.cm.tR, Acc.cm.tR, Acc.Chla.tR, N.Perc, C.Perc, CN) %>% 
  mutate(µmol.0 = round(µmol.0, 2), 
         µmol.0.h.cm = round(µmol.0.h.cm, 2), 
         Comp.irr = round(Comp.irr, 2), 
         Chla.cm = round(Chla.cm, 2), 
         Chla.cm.tR = round(Chla.cm.tR, 2), 
         Acc.cm = round(Acc.cm, 2), 
         Acc.cm.tR = round(Acc.cm.tR, 2),
         Acc.Chla = round(Acc.Chla, 2), 
         Acc.Chla.tR = round(Acc.Chla.tR, 2), 
         CN = round(CN, 2)) %>% 
  dplyr::rename(`Experiment day` = Exp.Day, `Treat temp [°C]` = Temperature,
                `phylloid length [cm]` = Length, `phylloid width [cm]` = Width, `cauloid length [cm]` = Stipe.Length,
                `disc area [cm-2]` = Area.discs, `FW [g]` = FW, `DW [g]` = DW, `Fv/Fm` = Fv.Fm,
                `O2 at 0 µmol photons m-2 s-1 [µmol l-1 s-1]` = µmol.0,
                `O2 at 0 µmol photons m-2 s-1 [µmol l-1 h-1 cm-2]` = µmol.0.h.cm,
                `O2 at 24 µmol photons m-2 s-1 [µmol l-1 s-1]` = µmol.24,
                `O2 at 24 µmol photons m-2 s-1 [µmol l-1 h-1 cm-2]` = µmol.24.h.cm,
                `Compensation E [mol m-2 s-1]` = Comp.irr,
                `Compensation E [log(mol m-2 s-1)]` = Comp.irr.log,
                `Chl a [µg cm-2]` = Chla.cm, `Chl a mean [µg cm-2]` = Chla.cm.tR,
                `Pigm acc [µg cm-2]` = Acc.cm, `Pigm acc mean [µg cm-2]` = Acc.cm.tR,
                `Pigm acc/chl a [µg cm-2]` = Acc.Chla, `Pigm acc/chl a mean [µg cm-2]` = Acc.Chla.tR,
                `N [%]` = N.Perc, `C [%]` = C.Perc) %>% 
  mutate(Species = case_when(Species == "Slat" ~ "Saccharina latissima",
                             Species == "Aesc" ~ "Alaria esculenta"))
write_delim(kong_NiedzKelp, "~/pCloudDrive/restricted_data/Niedzwiedz/dataKelp_PG.csv", delim = "\t")
kong_NiedzLight <- read_csv("~/pCloudDrive/restricted_data/Niedzwiedz/dataLight.csv")
kong_NiedzLight_PG <- kong_NiedzLight %>% 
  dplyr::rename(`longitude [°E]`= Longitude, `latitude [°N]` = Latitude, `depth [m]` = Depth,
                `PAR [µmol m-2 s-1]` = PAR, `PAR [log(µmol m-2 s-1)]`= `log(PAR)`,
                `UV-A [µmol m-2 s-1]` = UV.A, `UV-B [µmol m-2 s-1]` = UV.B, 
                `E [µmol m-2 s-1]` = Surface.irr, Sal = Surface.Salinity) %>% 
  mutate(DateTime = DateTime-7200) %>% # Correct from Svalbard (UTC+2) to UTC+0
  separate(DateTime, into = c("Date", "Time"), sep = " ") %>% 
  mutate(`date/time [UTC+0]` = paste(Date, Time, sep = "T")) %>%
  dplyr::select(Station, `latitude [°N]`, `longitude [°E]`, `date/time [UTC+0]`, `depth [m]`,
                `E [µmol m-2 s-1]`, `PAR [µmol m-2 s-1]`, `PAR [log(µmol m-2 s-1)]`, 
                `UV-A [µmol m-2 s-1]`, `UV-B [µmol m-2 s-1]`, Sal) %>% 
  group_by(Station, `latitude [°N]`, `longitude [°E]`) %>% 
  arrange(`depth [m]`, .by_group = TRUE) %>% ungroup()
write_delim(kong_NiedzLight_PG, "~/pCloudDrive/restricted_data/Niedzwiedz/dataLight_PG.csv", delim = "\t")

# Combine and save
full_product_kong <- rbind(dplyr::select(pg_kong_ALL, -site), 
                           kong_sea_ice_inner, kong_zoo_data, kong_protist_nutrient_chla, # kong_glacier_info,
                           kong_CTD_database, kong_CTD_CO2, kong_weather_station, kong_mooring_GFI, 
                           kong_ferry, kong_mooring_SAMS, kong_ship_arrivals, kong_CTD_DATEN4, kong_LICHT,
                           kong_light_Laeseke, kong_light_Inka, kong_PAR_Dieter) %>% 
  rbind(filter(dplyr::select(full_product_sval, -site), lon >= bbox_kong[1], lon <= bbox_kong[2], lat >= bbox_kong[3], lat <= bbox_kong[4])) %>% 
  rbind(filter(dplyr::select(full_product_sval, -site), grepl("Kongsfjorden", citation))) %>% distinct() %>% mutate(site = "kong")
data.table::fwrite(full_product_kong, "~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.csv")
save(full_product_kong, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")
save(full_product_kong, file = "data/full_data/full_product_kong.RData")
plyr::l_ply(unique(full_product_kong$category), save_category, .parallel = T,
            df = full_product_kong, data_type = "full", site_name = "kong")
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
pg_is_ALL <- rbind(pg_is_Cryosphere, pg_is_Physical, pg_is_Chemistry, pg_is_Biology) %>% mutate(site = "is")
data.table::fwrite(pg_is_ALL, "~/pCloudDrive/FACE-IT_data/isfjorden/pg_is_ALL.csv")
save(pg_is_ALL, file = "~/pCloudDrive/FACE-IT_data/isfjorden/pg_is_ALL.RData")

# Check that all columns were used
# colnames(pg_is_clean)[!colnames(pg_is_clean) %in% unique(pg_is_ALL$variable)]

# Clean up
rm(list = grep("pg_is",names(.GlobalEnv),value = TRUE)); gc()


## Full product ------------------------------------------------------------

# Load full Svalbard file
## NB: This contains the full EU Arctic data
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
  pivot_longer(DEN:WVEL, names_to = "variable", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  left_join(is_mooring_IFO_units, by = c("variable" = "name")) %>% 
  mutate(units = case_when(units == "degree_Celsius" ~ "°C", TRUE ~ units),
         URL = "https://data.npolar.no/dataset/7718a106-5d13-42d9-bb79-1d2adf0f51c4",
         citation = "Skogseth, R., & Ellingsen, P. G. (2019). Mooring data from Isfjorden online mooring (IFO) during 30 Sep 2016 to 11 Mar 2017 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.7718a106",
         category = case_when(variable %in% c("OXY", "OXYS") ~ "chem", TRUE ~ "phys"),
         variable = paste0(variable, " [", units,"]"),
         date_accessed = as.Date("2021-04-15")) %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 6), .groups = "drop")
rm(is_mooring_IFO_units); gc()

## North mouth mooring GFI
is_mooring_GFI_N <- plyr::ldply(dir("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_N", full.names = T), load_GFI, .parallel = T) %>% 
  mutate(date_accessed = as.Date("2021-04-15"), .before = 1)

## South mouth mooring GFI
is_mooring_GFI_S <- plyr::ldply(dir("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_GFI_S", full.names = T), load_GFI, .parallel = T) %>% 
  mutate(date_accessed = as.Date("2021-08-04"), .before = 1)

## CO2 station at Tempelfjorden
is_CO2_tempelfjorden <- read_csv("~/pCloudDrive/FACE-IT_data/isfjorden/Marine_CO2_system_data_from_Tempelfjorden_2015_to_2017.csv") %>% 
  dplyr::rename(lon = `Longitude [°]`, lat = `Latitude [°]`, depth = `CTD Pressure [dbar]`) %>% 
  mutate(date = as.Date(paste0(Year,"-",Month,"-",Day)), .keep = "unused") %>% 
  dplyr::select(lon, lat, date, depth, `Temperature [ITS-90, deg C]`:`pHT in situ (temperature correction using EP TA)`) %>% 
  pivot_longer(`Temperature [ITS-90, deg C]`:`pHT in situ (temperature correction using EP TA)`, names_to = "variable", values_to = "value") %>% 
  mutate(URL = "http://metadata.nmdc.no/metadata-api/landingpage/35a23dd3ac46065c0b6cb86fcdd30e98",
         citation = "Ylva Ericson, UNIS, Eva Falck, UNIS, Agneta Fransson NPOLAR, and Melissa Chierici, IMR and UNIS (2019) Marine CO2 system data from Tempelfjorden, Svalbard, 2015-2017 https://doi.org/10.21335/NMDC-656799113",
         category = case_when(variable == "Salinity [PSU]" ~ "phys",
                              variable == "Temperature [ITS-90, deg C]" ~ "phys",
                              variable == "TA [µmol/kg]" ~ "chem",
                              variable == "pHT in situ" ~ "chem",
                              variable == "EP TA [µmol/kg]" ~ "chem",
                              variable == "pHT in situ (temperature correction using EP TA)" ~ "chem"),
         date_accessed = as.Date("2021-04-14")) %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 6), .groups = "drop")

## CO2 station at IsA
is_CO2_IsA <- read_csv("~/pCloudDrive/FACE-IT_data/isfjorden/Marine_CO2_system_data_at_the_IsA_Station_2015_to_2017.csv") %>% 
  dplyr::rename(lon = `Longitude [°]`, lat = `Latitude [°]`, depth = `CTD Pressure [dbar]`) %>% 
  mutate(date = as.Date(paste0(Year,"-",Month,"-",Day)), .keep = "unused") %>% 
  dplyr::select(lon, lat, date, depth, `Temperature [ITS-90, deg C]`:`pHT in situ (temperature correction using EP TA)`) %>% 
  pivot_longer(`Temperature [ITS-90, deg C]`:`pHT in situ (temperature correction using EP TA)`, names_to = "variable", values_to = "value") %>% 
  mutate(URL = "http://metadata.nmdc.no/metadata-api/landingpage/1e5ae6511b1c22a2f8d00aac50c32eb5",
         citation = "Ylva Ericson, UNIS, Eva Falck, UNIS, and Melissa Chierici, IMR and UNIS. (2019) Marine CO2 system data from the IsA Station, Svalbard, 2015-2017 https://doi.org/10.21335/NMDC-80568951",
         category = case_when(variable == "Salinity [PSU]" ~ "phys",
                              variable == "Temperature [ITS-90, deg C]" ~ "phys",
                              variable == "TA [µmol/kg]" ~ "chem",
                              variable == "pHT in situ" ~ "chem",
                              variable == "EP TA [µmol/kg]" ~ "chem",
                              variable == "pHT in situ (temperature correction using EP TA)" ~ "chem"),
         date_accessed = as.Date("2021-04-14")) %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 6), .groups = "drop")

## Chlorophyl station at IsA
# tidync("~/pCloudDrive/FACE-IT_data/isfjorden/chl_a/IsA_Svalbard_Chlorophyll_A_2011_2019_GFF.nc")
# as.data.frame(ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/chl_a/IsA_Svalbard_Chlorophyll_A_2011_2019_GFF.nc")$attribute$global)
is_Chla_IsA_units <- rbind(ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/chl_a/IsA_Svalbard_Chlorophyll_A_2011_2019_10um.nc")$variable,
                           ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/chl_a/IsA_Svalbard_Chlorophyll_A_2011_2019_GFF.nc")$variable) %>% distinct() %>% 
  mutate(units = case_when(units == "Micrograms per liter" ~ "µg/l", 
                           units == "Millilitres" ~ "ml", TRUE ~ units))
is_Chla_IsA_1 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/chl_a/IsA_Svalbard_Chlorophyll_A_2011_2019_10um.nc")) %>% mutate(data = "10um")
is_Chla_IsA_2 <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/isfjorden/chl_a/IsA_Svalbard_Chlorophyll_A_2011_2019_GFF.nc")) %>% mutate(data = "GFF")
is_Chla_IsA <- rbind(is_Chla_IsA_1, is_Chla_IsA_2) %>% 
  dplyr::rename(depth = Depth) %>% 
  mutate(date = as.Date(`Days since 1st jan 2011`, origin = "2011-01-01"), .keep = "unused") %>% 
  pivot_longer(`Chlorophyll A`:Phaeophytin, names_to = "variable", values_to = "value") %>% 
  left_join(is_Chla_IsA_units, by = c("variable" = "name")) %>% 
  mutate(URL = "https://archive.sigma2.no/pages/public/datasetDetail.jsf?id=10.11582/2020.00063",
         citation = "University Centre in Svalbard (2020).ISA_Svalbard_Chlorophyll_A_2011_2019 [Data set]. Norstore. https://doi.org/10.11582/2020.00063",
         lon = 15.52992, lat = 78.26105,
         variable = paste0(variable," - ",data," [", units,"]"),
         category = "bio",
         date_accessed = as.Date("2021-04-16")) %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 6), .groups = "drop")
rm(is_Chla_IsA_units, is_Chla_IsA_1, is_Chla_IsA_2); gc()

## Isfjorden radio meteorological station
is_met_radio <- load_met_NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/SN99790.nc") %>% mutate(date_accessed = as.Date("2021-04-14"), .before = 1)

## Airport meteorological station
is_met_airport <- load_met_NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/SN99840.nc") %>% mutate(date_accessed = as.Date("2021-08-04"), .before = 1)

## Pyramiden radio meteorological station
is_met_pyramiden <- load_met_NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/SN99880.nc") %>% mutate(date_accessed = as.Date("2021-08-04"), .before = 1)

## Ship AIS data
# Rather combine all of the ships into one variable
is_AIS_2017 <- read_csv("~/pCloudDrive/FACE-IT_data/isfjorden/AIS/AIS_2017.csv") %>% mutate(year = 2017)
is_AIS_2019 <- read_csv("~/pCloudDrive/FACE-IT_data/isfjorden/AIS/AIS_2019.csv") %>% mutate(year = 2019)
is_AIS <- rbind(is_AIS_2017, is_AIS_2019) %>% 
  dplyr::select(Name, ShipName, Month, everything()) %>% 
  group_by(year, Month) %>% 
  summarise(`trips [n]` = sum(`Number of trips`),
            `gross weight [sum]` = sum(`Gross weight`),
            `gross weight [mean]` = round(mean(`Gross weight`)),
            `built year [mean]` = round(mean(BuiltYear)),
            `berths [n; sum]` = sum(Berths),
            `berths [n; mean]` = round(mean(Berths)),
            `speed [mean]` = round(mean(Speed), 2),
            `nautical miles [sum]` = sum(`Nautical miles`),
            `nautical miles [mean]` = round(mean(`Nautical miles`), 2),
            `duration [hours; sum]` = sum(`Duration (hours)`),
            `duration [hours; mean]` = round(mean(`Duration (hours)`), 2),
            `duration in port [hours; sum]` = sum(`Duration In port (hours)`),
            `duration in port [hours; mean]` = round(mean(`Duration In port (hours)`), 1),
            `Total fuel [tonnes; sum]` = sum(`Total fuel (tonnes)`),
            `Total fuel [tonnes; mean]` = round(mean(`Total fuel (tonnes)`), 2),
            `Fuel propulsion [tonnes; sum]` = sum(`Fuel propulsion (tonnes)`),
            `Fuel propulsion [tonnes; mean]` = round(mean(`Fuel propulsion (tonnes)`), 2),
            `Fuel in port [tonnes; sum]` = sum(`Fuel in port (tonnes)`),
            `Fuel in port [tonnes; mean]` = round(mean(`Fuel in port (tonnes)`), 2),
            `Power total [GWh; sum]` = sum(`Power total (GWh)`),
            `Power total [GWh; mean]` = round(mean(`Power total (GWh)`), 1),
            `Power in port [GWh; sum]` = sum(`Power in port (GWh)`),
            `Power in port [GWh; mean]` = round(mean(`Power in port (GWh)`), 3),
            `CO2 emissions total [tonnes; sum]` = sum(`CO2 emissions total (tonnes)`),
            `CO2 emissions total [tonnes; mean]` = round(mean(`CO2 emissions total (tonnes)`), 2),
            `CO2 emissions in port [tonnes; sum]` = sum(`CO2 emissions in port (tonnes)`),
            `CO2 emissions in port [tonnes; mean]` = round(mean(`CO2 emissions in port (tonnes)`), 2),
            `NOx emissions total [tonnes; sum]` = sum(`NOx emissions total (tonnes)`),
            `NOx emissions total [tonnes; mean]` = round(mean(`NOx emissions total (tonnes)`), 4),
            `NOx emissions in port [tonnes; sum]` = sum(`NOx emissions in port (tonnes)`),
            `NOx emissions in port [tonnes; mean]` = round(mean(`NOx emissions in port (tonnes)`), 4),
            `SOx emissions total [tonnes; sum]` = sum(`SOx emissions total (tonnes)`),
            `SOx emissions total [tonnes; mean]` = round(mean(`SOx emissions total (tonnes)`), 4),
            `SOx emissions in port [tonnes; sum]` = sum(`SOx emissions in port (tonnes)`),
            `SOx emissions in port [tonnes; mean]` = round(mean(`SOx emissions in port (tonnes)`), 4),
            `PM emissions total [tonnes; sum]` = sum(`PM emissions total (tonnes)`),
            `PM emissions total [tonnes; mean]` = round(mean(`PM emissions total (tonnes)`), 4),
            `PM emissions in port [tonnes; sum]` = sum(`PM emissions in port (tonnes)`),
            `PM emissions in port [tonnes; mean]` = round(mean(`PM emissions in port (tonnes)`), 4), .groups = "drop") %>% 
  pivot_longer(`trips [n]`:`PM emissions in port [tonnes; mean]`, names_to = "variable", values_to = "value") %>%
  mutate(date = as.Date(paste0(year,"-",Month,"-01")),
         depth = 0, # It may be better to list this as NA 
         lon = NA, lat = NA, 
         date_accessed = as.Date("2020-09-30"),
         # variable = paste0(ShipName," [",var,"]"), # No longer using individual ships
         # category = case_when(grepl("co2|nox|sox", variable, ignore.case = T) ~ "chem",
                              # grepl("PM", variable, ignore.case = T) ~ "phys", TRUE ~ "soc"),
         category = "soc", # Rather I think these should all be classified as social data
         URL = "Received directly from Morten Simonsen",
         citation = "Simonsen, M., Walnum, H. J., & Gössling, S. (2018). Model for estimation of fuel consumption of cruise ships. Energies, 11(5), 1059.") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
rm(is_AIS_2017, is_AIS_2019); gc()

# Raw AIS data
# NB: Not used
# load("~/pCloudDrive/FACE-IT_data/isfjorden/AIS/is_AIS_raw.RData")

## Tourist ship arrival data
is_ship_arrivals <- read_csv("~/pCloudDrive/FACE-IT_data/isfjorden/is_ship_arrivals.csv") %>% 
  pivot_longer(`2007`:`2019`, names_to = "date", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(variable = paste0(type," [",name,"]"),
         category = "soc",
         date = as.Date(paste0(date,"-12-31")),
         depth = NA, lon = 15.60, lat = 78.23,
         URL = "https://portlongyear.no/statistics-of-port-longyear-2007-2012-2019/",
         date_accessed = as.Date("2021-10-25"),
         citation = "Port of Longyearbyen (2020). Statistics of Port Longyear 2007, 2012-2019. https://portlongyear.no/statistics-of-port-longyear-2007-2012-2019/") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)

# Combine and save
full_product_is <- rbind(dplyr::select(pg_is_ALL, -site), 
                         is_mooring_N, is_mooring_S, is_mooring_IFO, is_mooring_GFI_N, is_mooring_GFI_S,
                         is_CO2_tempelfjorden, is_CO2_IsA, is_Chla_IsA, is_met_radio, is_met_airport, is_met_pyramiden, 
                         is_AIS, is_ship_arrivals) %>% 
  rbind(filter(dplyr::select(full_product_sval, -site), lon >= bbox_is[1], lon <= bbox_is[2], lat >= bbox_is[3], lat <= bbox_is[4])) %>% 
  rbind(filter(dplyr::select(full_product_sval, -site), grepl("Isfjorden", variable))) %>% # Shipping data 
  rbind(filter(dplyr::select(full_product_sval, -site), grepl("Isfjorden", citation))) %>% distinct() %>% mutate(site = "is")
data.table::fwrite(full_product_is, "~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.csv")
save(full_product_is, file = "~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.RData")
save(full_product_is, file = "data/full_data/full_product_is.RData")
plyr::l_ply(unique(full_product_is$category), save_category, .parallel = T,
            df = full_product_is, data_type = "full", site_name = "is")
rm(list = grep("is_",names(.GlobalEnv),value = TRUE)); gc()
# if(!exists("full_product_is")) load("~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.RData")


## Test visuals ------------------------------------------------------------

# is_phys <- full_product_is %>% 
#   filter(category == "phys")

# is_pH <- full_product_is %>% 
#   filter(grepl("pH",variable))

# is_temp <- is_phys %>% 
#   filter(grepl("°C",variable))

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
pg_stor_ALL <- rbind(pg_stor_Cryosphere, pg_stor_Physical, pg_stor_Chemistry) %>% mutate(site = "stor")
data.table::fwrite(pg_stor_ALL, "~/pCloudDrive/FACE-IT_data/storfjorden/pg_stor_ALL.csv")
save(pg_stor_ALL, file = "~/pCloudDrive/FACE-IT_data/storfjorden/pg_stor_ALL.RData")

# Check that all columns were used
# colnames(pg_stor_clean)[!colnames(pg_stor_clean) %in% unique(pg_stor_ALL$variable)]

# Clean up
rm(list = grep("pg_stor",names(.GlobalEnv),value = TRUE)); gc()


## Full product ------------------------------------------------------------

# Load full Svalbard file
## NB: This contains the full EU Arctic data
if(!exists("full_product_sval")) load("data/full_data/full_product_sval.RData")

# Load PG file
if(!exists("pg_stor_ALL")) load("~/pCloudDrive/FACE-IT_data/storfjorden/pg_stor_ALL.RData")

# Process individual files
## Light data
### NB: No columns with key drivers: CDOM, icam_aphy, icam_anap, Perkins_ap, O18
### NB: pressure [dbar] used here as no depth data available
stor_light_CTD <- read_csv("~/pCloudDrive/FACE-IT_data/storfjorden/optical_properties/acs_fdom_ctd.csv") %>% 
  dplyr::select(`Lat [deg_N]`, `Lon [deg_E]`, Year, Month, Day, `Pressure [dbar]`, `Temp [degC]`, `Sal [PSU]`) %>% 
  dplyr::rename(lat = `Lat [deg_N]`, lon = `Lon [deg_E]`, depth = `Pressure [dbar]`, `Temp [°C]` = `Temp [degC]`) %>% 
  pivot_longer(`Temp [°C]`:`Sal [PSU]`, names_to = "variable", values_to = "value") %>% 
  mutate(date = as.Date(paste0(Year,"-",Month,"-",Day)),
         date_accessed = as.Date("2022-01-21"),
         category = "phys",
         URL = "https://data.npolar.no/dataset/e6974f73-99bb-46d8-b06d-d00287b91729",
         citation = "Petit, T., Granskog, M. A., Hamre, B., Kowalczuk, P., & Röttgers, R. (2022). Inherent optical properties of waters in Storfjorden (Svalbard) in summer 2020 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2022.e6974f73") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value) %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# Combine and save
full_product_stor <- rbind(dplyr::select(pg_stor_ALL, -site), 
                           stor_light_CTD) %>% 
  rbind(filter(dplyr::select(full_product_sval, -site), lon >= bbox_stor[1], lon <= bbox_stor[2], lat >= bbox_stor[3], lat <= bbox_stor[4])) %>% 
  rbind(filter(dplyr::select(full_product_sval, -site), grepl("Storfjorden", variable))) %>% # Shipping data 
  rbind(filter(dplyr::select(full_product_sval, -site), grepl("Storfjorden", citation))) %>% distinct() %>% mutate(site = "stor")
data.table::fwrite(full_product_stor, "~/pCloudDrive/FACE-IT_data/storfjorden/full_product_stor.csv")
save(full_product_stor, file = "~/pCloudDrive/FACE-IT_data/storfjorden/full_product_stor.RData")
save(full_product_stor, file = "data/full_data/full_product_stor.RData")
plyr::l_ply(unique(full_product_stor$category), save_category, .parallel = T,
            df = full_product_stor, data_type = "full", site_name = "stor")
rm(list = grep("stor_",names(.GlobalEnv),value = TRUE)); gc()
# if(!exists("full_product_stor")) load("~/pCloudDrive/FACE-IT_data/storfjorden/full_product_stor.RData")



# Greenland ---------------------------------------------------------------

## PG product -------------------------------------------------------------

# There is no PG product for Greenland


## Full product -----------------------------------------------------------

# National statistics
## Income
green_income_json <- 
  pxweb_get(url = "https://bank.stat.gl:443/api/v1/en/Greenland/IN/IN20/INXPI101.px",
            query = "data/JSON/pxapi-api_table_INXPI101.px.json")
green_income <- as.data.frame(green_income_json, column.name.type = "text", variable.value.type = "text") %>% 
  dplyr::rename(value = `Income for persons (14 years +)`) %>% 
  mutate(site = case_when(grepl("Qeqertalik", municipality) ~ "disko",
                          grepl("Sermersooq", municipality) ~ "nuup"),
         variable = paste0(`type of income`," - ",gender, "[DKK]"),
         date = as.Date(paste0(time,"-12-31")),
         date_accessed = as.Date(Sys.Date()),
         category = "gov",
         URL = "https://bank.stat.gl:443/api/v1/en/Greenland/IN/IN20/INXPI101.px",
         citation = px_cite(green_income_json),
         lon = NA, lat = NA, depth = NA) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
rm(green_income_json)

# Combine and save
full_product_green <- rbind(green_income)
data.table::fwrite(full_product_green, "~/pCloudDrive/FACE-IT_data/greenland/full_product_green.csv")
save(full_product_green, file = "~/pCloudDrive/FACE-IT_data/greenland/full_product_green.RData")
save(full_product_green, file = "data/full_data/full_product_green.RData")
plyr::l_ply(unique(full_product_green$category), save_category, .parallel = T,
            df = full_product_green, data_type = "full", site_name = "green") # NB: Site name requires some consideration
rm(list = grep("green_",names(.GlobalEnv),value = TRUE)); gc()


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
  # dplyr::select(contains(c("date", "lon", "lat")), everything()) %>%  # Look at meta columns
  dplyr::select(contains(c("depth", "press", "bathy", "elev")), everything()) %>%  # Look at depth columns
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
  mutate(depth = case_when(!is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`))) %>%
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
# pg_young_Biology <- pg_var_melt(pg_young_clean, query_Biology$pg_col_name, "bio") # empty
# Social
# pg_young_Social <- pg_var_melt(pg_young_clean, query_Social$pg_col_name, "soc") # empty

# Stack them together
pg_young_ALL <- rbind(pg_young_Cryosphere, pg_young_Physical, pg_young_Chemistry) %>% mutate(site = "young")
data.table::fwrite(pg_young_ALL, "~/pCloudDrive/FACE-IT_data/young_sound/pg_young_ALL.csv")
save(pg_young_ALL, file = "~/pCloudDrive/FACE-IT_data/young_sound/pg_young_ALL.RData")

# Check that all columns were used
# colnames(pg_young_clean)[!colnames(pg_young_clean) %in% unique(pg_young_ALL$variable)]

# Clean up
rm(list = grep("pg_young",names(.GlobalEnv),value = TRUE)); gc()


## Full product ------------------------------------------------------------

# Load full EU file
if(!exists("full_product_EU")) load("data/full_data/full_product_EU.RData")

# Load PG product
if(!exists("pg_young_ALL")) load("~/pCloudDrive/FACE-IT_data/young_sound/pg_young_ALL.RData")

# Primary production data
holding_station_idx <- read_csv("~/pCloudDrive/FACE-IT_data/young_sound/Holding_etal_2019_data/CTD_biochem/YS_2014_CTD_biochem.csv") %>% 
  dplyr::rename(lon = LONG_DD, lat = LAT_DD) %>% 
  dplyr::select(lon, lat, station) %>% distinct()
holding_CTD_biochem <- read_csv("~/pCloudDrive/FACE-IT_data/young_sound/Holding_etal_2019_data/CTD_biochem/YS_2014_CTD_biochem.csv") %>% 
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>% 
  dplyr::rename(lon = LONG_DD, lat = LAT_DD) %>% 
  dplyr::select(lon, lat, date, depth, z_mix, z_photo, temp:SiO4, -real_depth) %>% 
  pivot_longer(z_mix:SiO4, names_to = "variable", values_to = "value")
holding_CTD_profiles <- read_csv("~/pCloudDrive/FACE-IT_data/young_sound/Holding_etal_2019_data/CTD_biochem/YS_2014_SBE19plus_CTD_profiles_ALL.csv") %>% 
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>% 
  dplyr::rename(lon = LONG_DD, lat = LAT_DD) %>% 
  dplyr::select(lon, lat, date, depth, temp:SoundVelocit_m_s, -real_depth) %>% 
  pivot_longer(temp:SoundVelocit_m_s, names_to = "variable", values_to = "value")
holding_PI <- read_csv("~/pCloudDrive/FACE-IT_data/young_sound/Holding_etal_2019_data/PI_parameters/YS_2014_PI_parameters.csv") %>%
  mutate(date = as.Date(paste0(year,"-",month,"-",day)), .keep = "unused") %>% 
  pivot_longer(pm_chl:ik, names_to = "variable", values_to = "value") %>% 
  left_join(holding_station_idx, by = "station") %>% 
  dplyr::select(lon, lat, date, depth, variable, value)
holding_ChlA <- read_csv("~/pCloudDrive/FACE-IT_data/young_sound/Holding_etal_2019_data/Chl_a/YS_2014_Chl_Fractions.csv") %>% 
  mutate(date = as.Date(paste0(year,"-",month,"-",day)), .keep = "unused") %>% 
  pivot_longer(chla_GFF_conc:TOTAL_chla_area, names_to = "variable", values_to = "value") %>% 
  left_join(holding_station_idx, by = "station") %>% 
  dplyr::select(lon, lat, date, depth, variable, value)
holding_PP <- read_csv("~/pCloudDrive/FACE-IT_data/young_sound/Holding_etal_2019_data/Primary_production/YS_2014_PP_Fractions.csv") %>% 
  mutate(date = as.Date(paste0(year,"-",month,"-",day)), .keep = "unused") %>% 
  pivot_longer(PP_plus_10_frac:TOTAL_PP_area, names_to = "variable", values_to = "value") %>% 
  left_join(holding_station_idx, by = "station") %>% 
  dplyr::select(lon, lat, date, depth, variable, value)
young_prim_prod <- rbind(holding_CTD_biochem, holding_CTD_profiles, holding_PI, holding_ChlA, holding_PP) %>% 
  filter(!is.na(value)) %>% 
  mutate(URL = "https://zenodo.org/record/5572041#.YW_Lc5uxU5m",
         citation = "Holding, Johnna M, Markager, Stiig, Juul-Pedersen, Thomas, Paulsen, Maria L, Møller, Eva F, & Sejr, Mikael K. (2021). Dataset from Holding et al. (2019) Seasonal and spatial patterns of primary production in a high latitude fjord [Data set]. Zenodo. https://doi.org/10.5281/zenodo.5572041",
         date_accessed = as.Date("2021-10-20"),
         category = case_when(variable %in% c("z_mix", "z_photo", "strat_index", "temp", "conductivity", 
                                              "turbidity", "PAR", "salinity", "pot_temp", "sigmaT_kg_m3",
                                              "density_kg_m3", "SoundVelocit_m_s") ~ "phys",
                              variable %in% c("nitracline", "oxygen_umol_kg", "oxygen_corrected_umol_kg", 
                                              "NH4", "NO2", "NO3", "NO2_NO3", "PO4", "SiO4") ~ "chem",
                              TRUE ~ "bio"),
         variable = case_when(variable == "pm_chl" ~ "pm_chl [g C g-1 Chl h-1]",
                              variable == "alpha_chl" ~ "alpha_chl [g C g-1 Chl mol-1 photons m2]",
                              variable == "ik" ~ "ik [μmol photons m-2 s-1]",
                              variable %in% c("PP_area_plus_10_frac", "PP_area_GFF_frac", 
                                              "PP_area_disolv_frac") ~ paste0(variable," [mg C m-2 day-1]"),
                              variable %in% c("z_mix", "z_photo", "fluor_max", "chl_max", "nitracline") ~ paste0(variable," [m]"),
                              variable %in% c("temp", "pot_temp") ~ paste0(variable," [°C]"),
                              variable == "conductivity" ~ "conductivity [S/m]",
                              variable == "salinity" ~ "salinity [PSU]",
                              variable == "turbidity" ~ "turbidity [FTU]",
                              variable == "PAR" ~ "PAR [µmol m-2 s-1]",
                              variable %in% c("oxygen_umol_kg", "oxygen_corrected_umol_kg") ~ paste0(variable," [µmol kg-1]"),
                              variable %in% c("sigmaT_kg_m3", "density_kg_m3") ~ paste0(variable," [kg m-3]"),
                              variable == "SoundVelocit_m_s" ~ "SoundVelocit_m_s [m/s]",
                              variable == "chl_flu" ~ "chl_flu [µg chl m-3]",
                              variable %in% c("chla_GFF_conc", "chla_plus_10_conc") ~ paste0(variable," [µg Chla m-3]"),
                              variable %in% c("chla_area_GFF_frac", "chla_area_plus_10_frac") ~ paste0(variable," [mg Chla m-2]"),
                              variable == "pp_vol" ~ "pp_vol [mg C m-3 day-1]",
                              variable == "pp_chla" ~ "pp_chla [mg C µg Chla-1 m–3 day-1]",
                              variable %in% c("NH4", "NO2", "NO3", "NO2_NO3", "PO4", "SiO4") ~ paste0(variable," [µmol/l]"),
                              TRUE ~ variable)) %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop")
rm(list = grep("holding_",names(.GlobalEnv),value = TRUE)); gc()

# Combine and save
full_product_young <- rbind(dplyr::select(pg_young_ALL, -site), 
                            young_prim_prod) %>% 
  rbind(filter(dplyr::select(full_product_EU, -site), lon >= bbox_young[1], lon <= bbox_young[2], lat >= bbox_young[3], lat <= bbox_young[4])) %>% 
  rbind(filter(dplyr::select(full_product_EU, -site), grepl("Young Sound", citation))) %>% distinct() %>% mutate(site = "young")
data.table::fwrite(full_product_young, "~/pCloudDrive/FACE-IT_data/young_sound/full_product_young.csv")
save(full_product_young, file = "~/pCloudDrive/FACE-IT_data/young_sound/full_product_young.RData")
save(full_product_young, file = "data/full_data/full_product_young.RData")
plyr::l_ply(unique(full_product_young$category), save_category, .parallel = T,
            df = full_product_young, data_type = "full", site_name = "young")
rm(list = grep("young_",names(.GlobalEnv),value = TRUE)); gc()
# if(!exists("full_product_young")) load("~/pCloudDrive/FACE-IT_data/young_sound/full_product_young.RData")


## GEM ---------------------------------------------------------------------

# NB: Other radiation files are available but we are just using net radiation here
# A lot of wind data were downloaded but not used

# Mooring with many variables
## NB: These data are part of an upcoming publication
## NB: Not yet added to meta-database. Waiting for publication.
## Therefore a lot of the information is still forthcoming
young_mooring_multi <- read_csv("~/pCloudDrive/restricted_data/Young_Sound/RBR_Sedimentfaelde_2108-19_SN080360.csv") %>% 
  dplyr::select(-`Average of PAR`) %>% # Rather using the corrected PAR values
  dplyr::rename(date = Date, 
                `cndc [mS/cm]` = `Average of Conductivity`,
                `sal [PSU]` = `Average of Salinity`, 
                `press [dbar]` = `Average of Pressure`,
                `temp [°C]` = `Average of Temperature`, 
                `chlA [µg/l]` = `Average of Chlorophyll a`,
                `oxygen [µmol/l]` = `Average of Dissolved O₂ concentration`,
                `PAR [µMol/m²/s]` = `PAR corrected`,
                `turbidity [NTU]` = `Average of Turbidity`, 
                `depth` = `Average of Depth`) %>% 
  pivot_longer(`cndc [mS/cm]`:`turbidity [NTU]`, names_to = "variable") %>% 
  mutate(category = case_when(variable == "chlA [µg/l]" ~ "bio",
                              variable == "oxygen [µmol/l]" ~ "chem",
                              TRUE ~ "phys"),
         lon = NA, lat = NA, # NB: Still need this information from Mikael or from when the publication comes out
         depth = round(depth, 1),
         date = as.Date(date, format = "%B %d, %Y"),
         URL = "Received directly from Mikael Sejr",
         date_accessed = as.Date("2021-12-01"),
         citation = "Singh, R., Belanger, S., ... Sejr, M. (2022) In prep.") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# Sea ice free period per year
young_GEM_sea_ice_open_water <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Sea_ice_conditions_Open_water_duration.csv", delim = "\t") %>% 
  dplyr::rename(value = `Open Water duration`) %>% 
  mutate(variable = "Open water [annual days]",
         category = "cryo",
         date = as.Date(paste0(Year,"-12-31")),
         depth = NA, lon = -20.57, lat = 74.47,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/R3QB-7Q71",
         date_accessed = as.Date("2022-02-03"),
         citation = "Open water duration. Sea ice conditions MarineBasis Zackenberg. doi: 10.17897/R3QB-7Q71") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)

# Sea ice breakup
young_GEM_sea_ice_breakup <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Sea_ice_conditions_Sea_ice_breakup.csv", delim = "\t") %>% 
  dplyr::rename(value = `Start open water`) %>% 
  mutate(variable = "Open water [start date]",
         category = "cryo",
         date = value,
         value = as.integer(date),
         depth = NA, lon = -20.57, lat = 74.47,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/SM1Q-6A72",
         date_accessed = as.Date("2022-02-03"),
         citation = "Sea ice breakup. Sea ice conditions MarineBasis Zackenberg. doi: 10.17897/SM1Q-6A72") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)

# Sea ice formation
young_GEM_sea_ice_formation <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Sea_ice_conditions_Sea_ice_formation.csv", delim = "\t") %>% 
  dplyr::rename(value = `End open water`) %>% 
  mutate(variable = "Open water [end date]",
         category = "cryo",
         date = value,
         value = as.integer(date),
         depth = NA, lon = -20.57, lat = 74.47,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/5MNP-KX83",
         date_accessed = as.Date("2022-02-03"),
         citation = "Sea ice formation. Sea ice conditions MarineBasis Zackenberg. doi: 10.17897/5MNP-KX83") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)

# Sea ice thickness
young_GEM_sea_ice_thickness <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Sea_ice_conditions_Sea_ice_thickness.csv", delim = "\t") %>% 
  dplyr::rename(value = `Sea ice thickness (cm)`, date = Date) %>% 
  mutate(variable = "Sea ice thickness [cm]",
         category = "cryo",
         depth = NA, lon = -20.25, lat = 74.31,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/H5D5-TZ32",
         date_accessed = as.Date("2022-02-03"),
         citation = "Sea ice thickness. Sea ice conditions MarineBasis Zackenberg. doi: 10.17897/H5D5-TZ32") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# Sea ice thickness
young_GEM_sea_ice_snow_thickness <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Sea_ice_conditions_Snow_thickness.csv", delim = "\t") %>% 
  dplyr::rename(value = `Snow thickness (cm)`, date = Date) %>% 
  mutate(variable = "Sea ice snow thickness [cm]",
         category = "cryo",
         depth = NA, lon = -20.25, lat = 74.31,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/B77X-BT16",
         date_accessed = as.Date("2022-02-03"),
         citation = "Sea ice snow thickness. Sea ice conditions MarineBasis Zackenberg. doi: 10.17897/B77X-BT16") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# Water column CTD
## NB: PAR units not given, but they look like [µmol m-2 s-1]
young_GEM_CTD_water_column <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Water_column_CTD_measurements.csv", delim = "\t") %>% 
  dplyr::rename(date = Date, `depth` = `Pressure, db`, `temp [°C]` = `Temperature, C`, 
                sal = Salinity, `density [kg m-3]` = `Water Density, kg m-3`, fluor = `Water Fluorescence`, 
                PAR = `Water, PAR`, turbidity = `Water turbidity`, `Tpot [°C]` = `Potential temperature, C`,
                `oxygen [µmol/kg]` = `Water oxygen content, µmol/kg`) %>% 
  pivot_longer(`temp [°C]`:`oxygen [µmol/kg]`, names_to = "variable") %>% 
  filter(!is.na(value)) %>% 
  mutate(category = case_when(variable == "fluor" ~ "bio",
                              variable == "oxygen [µmol/kg]" ~ "chem",
                              TRUE ~ "phys"),
         lon = -20.57, lat = 74.47,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/CG48-0H12",
         date_accessed = as.Date("2022-02-03"),
         citation = "CTD measurements Water column. Water column MarineBasis Zackenberg. doi: 10.17897/CG48-0H12") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# Mooring CTD
young_GEM_CTD_mooring <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Water_column_Mooring_CTD_measurements.csv", delim = "\t") %>% 
  dplyr::rename(date = DATE, depth = `PRESSURE (db)`, `temp [°C]` = `TEMPERATURE (°C)`, `sal [PSU]` = `Salinity (PSU)`) %>% 
  pivot_longer(`temp [°C]`:`sal [PSU]`, names_to = "variable") %>% 
  mutate(category = "phys",
         lon = -20.27883, lat = 74.31515,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/8GPS-CE70",
         date_accessed = as.Date("2022-02-03"),
         citation = "CTD measurements mooring. Water column MarineBasis Zackenberg. doi: 10.17897/8GPS-CE70") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# Sill CTD
young_GEM_CTD_sill <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Water_column_Sill_CTD_measurements.csv") %>% 
  dplyr::rename(date = Date, `depth` = `Pressure, db`, `temp [°C]` = `Temperature, C`, 
                sal = Salinity, `density [kg m-3]` = `Water Density, kg m-3`, fluor = `Water Fluorescence`, 
                PAR = `Water, PAR`, turbidity = `Water turbidity`, `Tpot [°C]` = `Potential temperature, C`,
                `oxygen [µmol/kg]` = `Water oxygen content, µmol/kg`) %>% 
  pivot_longer(`temp [°C]`:`oxygen [µmol/kg]`, names_to = "variable") %>%
  filter(value != -9999) %>% 
  mutate(category = case_when(variable == "fluor" ~ "bio",
                              variable == "oxygen [µmol/kg]" ~ "chem",
                              TRUE ~ "phys"),
         lon = -20.57, lat = 74.47,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/B4E2-N060",
         date_accessed = as.Date("2022-02-03"),
         citation = "Boone, W., Rysgaard, S., Carlson, D. F., Meire, L., Kirillov, S., Mortensen, J., ... & Sejr, M. K. (2018). Coastal freshening prevents fjord bottom water renewal in Northeast Greenland: A mooring study from 2003 to 2015. Geophysical Research Letters, 45(6), 2726-2733") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# Bottom CTD measurements
young_GEM_CTD_bottom <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Water_column_Bottom_CTD_measurements.csv") %>% 
  dplyr::rename(date = Date, `depth` = `Pressure, db`, `temp [°C]` = `Temperature, C`, 
                sal = Salinity, `density [kg m-3]` = `Water Density, kg m-3`, fluor = `Water Fluorescence`, 
                PAR = `Water, PAR`, turbidity = `Water turbidity`, `Tpot [°C]` = `Potential temperature, C`,
                `oxygen [µmol/kg]` = `Water oxygen content, µmol/kg`) %>% 
  pivot_longer(`temp [°C]`:`oxygen [µmol/kg]`, names_to = "variable") %>%
  filter(value != -9999) %>% 
  mutate(category = case_when(variable == "fluor" ~ "bio",
                              variable == "oxygen [µmol/kg]" ~ "chem",
                              TRUE ~ "phys"),
         lon = -20.57, lat = 74.47,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/J1J5-W960",
         date_accessed = as.Date("2022-05-03"),
         citation = "Boone, W., Rysgaard, S., Carlson, D. F., Meire, L., Kirillov, S., Mortensen, J., ... & Sejr, M. K. (2018). Coastal freshening prevents fjord bottom water renewal in Northeast Greenland: A mooring study from 2003 to 2015. Geophysical Research Letters, 45(6), 2726-2733") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# Phytoplankton relative species composition
young_GEM_phyto_sp <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Water_column_Phytoplankton_Relative_Species_Composition.csv", delim = "\t") %>%
  dplyr::rename(date = Date, variable = `Species name`, value = `% of total cells`) %>% 
  mutate(category = "bio",
         variable = paste0(variable," [%]"),
         lon = -20.57, lat = 74.47, depth = 0,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/MN5J-K414",
         date_accessed = as.Date("2022-02-03"),
         citation = "Phytoplankton Relative Species Composition (%). Water column MarineBasis Zackenberg. doi: 10.17897/MN5J-K414") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# DIC concentration
young_GEM_DIC <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Water_column_Water_DIC_Concentration_æmol_kg.csv", delim = "\t") %>% 
  dplyr::rename(date = Date, depth = Depth, value = `Dissolved inorganic carbon (DIC), µmol kg-1`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "chem",
         variable = "DIC [µmol/kg]",
         lon = -20.57, lat = 74.47,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/7FSW-D577",
         date_accessed = as.Date("2022-02-03"),
         citation = "Water DIC Concentration (µmol/kg). Water column MarineBasis Zackenberg. doi: 10.17897/7FSW-D577") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# pCO2 in the water column
young_GEM_pCO2 <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Water_column_Water_pCO2.csv", delim = "\t") %>% 
  dplyr::rename(date = Date, lat_char = lat, lon_char = long, depth = `Depth, m`, value = `pCO2, µatm`) %>% 
  mutate(category = "chem",
         variable = "pCO2 [µatm]", # NB: This may need volume units
         lon_clean = str_replace(str_remove(lon_char, "’W|'W"), "°", " "), 
         lat_clean = str_replace(str_remove(lat_char, "’N|'N"), "°", " "), 
         lon = -as.numeric(measurements::conv_unit(lon_clean, "deg_dec_min", "dec_deg")),
         lat = as.numeric(measurements::conv_unit(lat_clean, "deg_dec_min", "dec_deg")),
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/A8J4-AF12",
         date_accessed = as.Date("2022-02-03"),
         citation = "Water pCO2. Water column MarineBasis Zackenberg. doi: 10.17897/A8J4-AF12") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# Phosphate concentration
young_GEM_phosphate <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Water_column_Water_Phosphate_Concentration.csv", delim = "\t") %>% 
  dplyr::rename(date = Date, depth = Depth, value = `PO4, µM`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "chem",
         variable = "PO4 [µmol/l]",
         lon = -20.57, lat = 74.47,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/KQ4Z-SD22",
         date_accessed = as.Date("2022-02-03"),
         citation = "Water Phosphate Concentration (µmol/L). Water column MarineBasis Zackenberg. doi: 10.17897/KQ4Z-SD22") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 6), .groups = "drop")

# Silicate concentration
young_GEM_silicate <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Water_column_Water_Silicate_Concentration.csv", delim = "\t") %>% 
  dplyr::rename(date = Date, depth = Depth, value = `Si, µM`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "chem",
         variable = "Si [µmol/l]",
         lon = -20.57, lat = 74.47,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/GFHW-ZT42",
         date_accessed = as.Date("2022-02-03"),
         citation = "Water Silicate Concentration (µmol/L). Water column MarineBasis Zackenberg. doi: 10.17897/GFHW-ZT42") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T),  .groups = "drop")

# Silicate concentration
young_GEM_TA <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Water_column_Water_TA_Concentration_æmol_kg.csv", delim = "\t") %>% 
  dplyr::rename(date = Date, depth = Depth, value = `Total Alkalinity (TA), µmol kg-1`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "chem",
         variable = "TA [µmol/kg]",
         lon = -20.57, lat = 74.47,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/6H8H-WM93",
         date_accessed = as.Date("2022-02-03"),
         citation = "Water TA Concentration (µmol/kg). Water column MarineBasis Zackenberg. doi: 10.17897/6H8H-WM93") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# Zooplankton abundance
young_GEM_zoo_sp <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Water_column_Zooplankton_Abundance.csv", delim = "\t") %>% 
  dplyr::rename(date = Date, value = `Abundance/m2`) %>% 
  mutate(category = "bio",
         variable = paste0(`Species name`," ",`Stage/sex`," [Abundance/m2]"),
         lon = -20.57, lat = 74.47, depth = 0,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/D3C5-AJ13",
         date_accessed = as.Date("2022-02-03"),
         citation = "Zooplankton Species Composition (individuals/m2). Water column MarineBasis Zackenberg. doi: 10.17897/D3C5-AJ13") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T)), .groups = "drop")

# Air temperature at 2 m
young_GEM_air_temp_2m <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Air_temperature_Air_temperature_200cm_@_30min_sample_DegreesC.csv", delim = "\t") %>% 
  dplyr::rename(date = Date, value = `Air temperature, 200cm - 30min average (°C)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "phys",
         variable = "TTT [°C]",
         lon = -20.55208333, lat = 74.47191667, depth = -2,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/G5WS-0W04",
         date_accessed = as.Date("2022-04-28"),
         citation = "Air temperature, 200cm - 30min sample (°C). Air temperature ClimateBasis Zackenberg. doi: 10.17897/G5WS-0W04") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 2), .groups = "drop")

# Air temperature at 7 m
young_GEM_air_temp_7m <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Air_temperature_Air_temperature_750cm_@_60min_sample_DegreesC.csv", delim = "\t") %>% 
  dplyr::rename(date = Date, value = `Air temperature, 750cm - 60min average (°C)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "phys",
         variable = "TTT [°C]",
         lon = -20.55208333, lat = 74.47191667, depth = -7.5,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/9V7J-Z845",
         date_accessed = as.Date("2022-04-28"),
         citation = "Air temperature, 750cm - 60min sample (°C). Air temperature ClimateBasis Zackenberg. doi: 10.17897/9V7J-Z845") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 2), .groups = "drop")

# Precipitation
young_GEM_precip <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Precipitation_Precipitation_accumulated_mm.csv") %>% 
  dplyr::rename(date = Date, value = `Precipitation accumulated (mm)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "phys",
         lon = -20.55208333, lat = 74.47191667, depth = NA,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/KVVQ-BE46",
         date_accessed = as.Date("2022-04-28"),
         citation = "Precipitation accumulated - 60min (mm) . Precipitation ClimateBasis Zackenberg. doi: 10.17897/KVVQ-BE46") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category) %>% 
  summarise(`precipitation [mm/h]` = round(mean(value, na.rm = T), 2),
            `precipitation [mm/day]` = sum(value, na.rm = T), .groups = "drop") %>% 
  pivot_longer(`precipitation [mm/h]`:`precipitation [mm/day]`, names_to = "variable")

# Snow fall
young_GEM_snow_fall <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Precipitation_Snow_depth_m.csv") %>% 
  dplyr::rename(date = Date, value = `Snow depth (m)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "cryo",
         # variable = "Snow depth [m]",
         lon = -20.55208333, lat = 74.47191667, depth = -44,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/7RVV-Z412",
         date_accessed = as.Date("2022-04-28"),
         citation = "Snow depth - 180min sample (m). Precipitation ClimateBasis Zackenberg. doi: 10.17897/7RVV-Z412") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category) %>% 
  summarise(`snow fall [m/h]` = round(mean(value, na.rm = T), 2),
            `snow fall [m/day]` = sum(value, na.rm = T), .groups = "drop") %>% 
  pivot_longer(`snow fall [m/h]`:`snow fall [m/day]`, names_to = "variable")

# Air pressure
young_GEM_air_press <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Pressure_Air_pressure_@_200cm_60min_sample_hPa.csv") %>% 
  dplyr::rename(date = Date, value = `Air pressure, 200cm - 60min sample (hPa)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "phys",
         variable = "Air pressure [hPa]",
         lon = -20.55208333, lat = 74.47191667, depth = -46,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/WG9V-3J16",
         date_accessed = as.Date("2022-04-28"),
         citation = "Air pressure, 200cm - 60min sample (hPa). Pressure ClimateBasis Zackenberg. doi: 10.17897/WG9V-3J16") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 2), .groups = "drop")

# Net radiation
young_GEM_qnet <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Radiation_Net_radiation_CNR1_@_200_cm_5min_average_W_m.csv") %>% 
  dplyr::rename(date = Date, value = `NR (W/m2)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "phys",
         variable = "Qnet [W/m2]",
         lon = -20.55208333, lat = 74.47191667, depth = -46,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/ZNNM-V349",
         date_accessed = as.Date("2022-04-28"),
         citation = "Net radiation (CNR1), 200 cm - 5min average (W/m2). Radiation ClimateBasis Zackenberg. doi: 10.17897/ZNNM-V349") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 2), .groups = "drop")

# River discharge
young_GEM_river_dis <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Zackenberg_River_Hydrometric_data_Discharge_at_a_cross_section_of_the_river_m3_s.csv") %>% 
  dplyr::rename(date = Date, value = `Q (m3/s)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "cryo",
         variable = "Q [m3/s]",
         lon = -20.57675, lat = 74.47069444, depth = NA,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/A308-6075",
         date_accessed = as.Date("2022-04-28"),
         citation = "Discharge at a cross section of the river (m3/s). Zackenberg River Hydrometric data ClimateBasis Zackenberg. doi: 10.17897/A308-6075") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 2), .groups = "drop")

# PAR over land
young_GEM_PAR_land <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Radiation_Photosyntetic_active_radiation_@_200_cm_5min_average_æmol_sec_m.csv") %>% 
  dplyr::rename(date = Date, value = `PAR (µmol/sec/m2)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "phys",
         variable = "PAR [µmol/sec/m2]",
         lon = -20.55208333, lat = 74.47191667, depth = -46,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/XRDQ-N135",
         date_accessed = as.Date("2022-04-28"),
         citation = "Photosyntetic active radiation, 200 cm - 5min sample (µmol/sec/m2). Radiation ClimateBasis Zackenberg. doi: 10.17897/XRDQ-N135") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 2), .groups = "drop")

# Glacier ice surface mass balance
# Jikes, whoever created this coords file needs some help
# Somehow `2013-04-28 12:00:00 stake 13` refuses to change...
## NB: Daily averages are not created of the ablation values here because many datum are missing lon/lat
## By creating daily means it clumps these values together, which is incorrect to do
## After some reflection it was decided to keep the value smissing lon/lat because they still have date values
## Meaning that they are useful when constructing a time series of change when creating averages over the study site
young_GEM_gmb_coords <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Glacier_ice_GPS_sites.csv") %>% 
  mutate(site_name = str_replace(site_name, "s", "stake "),
         lon = case_when(lon %in% c(-99.99, -9999) ~ as.numeric(NA), 
                         lon > 100 ~ lon/10000000, 
                         lon > 0 ~ -lon,
                         lon > -1 ~ lon *100,
                         TRUE ~ lon),
         lat = case_when(lat %in% c(-99.99, -9999) ~ as.numeric(NA), 
                         lat < 70 ~ lat*100, TRUE ~ lat),
         alt = case_when(alt == -9999 ~ as.numeric(NA), TRUE ~ alt))
young_GEM_gmb <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Glacier_ice_Surface_mass_balance.csv") %>% 
  left_join(young_GEM_gmb_coords, by = c("date", "time", "site_name")) %>% 
  filter(ablation != -9999) %>% 
  dplyr::rename(value = ablation) %>% 
  mutate(category = "cryo",
         variable = "ablation [m w.e.]",
         depth = -alt,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/SNZB-ZA54",
         date_accessed = as.Date("2022-04-28"),
         citation = "Surface mass balance. Glacier ice GlacioBasis Zackenberg. doi: 10.17897/SNZB-ZA54") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  # NB: Do NOT create daily averages for this file
  # group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  # summarise(value = mean(value, na.rm = T), .groups = "drop")
rm(young_GEM_gmb_coords); gc()

# Snow depth
## NB: Three different sites are given but only one set of coords is listed on the meta-data page
## so I am averaging the three sites here...
young_GEM_snow_depth <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Snow_cover_Snow_sonic_ranger_height.csv") %>% 
  dplyr::select(-h_snow_M_qual, -h_snow_S_qual, -h_snow_T_qual) %>% 
  pivot_longer(h_snow_M:h_snow_T) %>% 
  mutate(category = "cryo",
         variable = "snow depth [m]",
         lon = -21.6000003814697, lat = 74.6500015258789, depth = NA,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/8GMN-WJ32",
         date_accessed = as.Date("2022-04-28"),
         citation = "Snow height observations at Zack-M, -S and -T Snow cover. Snow cover GlacioBasis Zackenberg. doi: 10.17897/8GMN-WJ32") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 6), .groups = "drop")

# Chl A
young_GEM_chla <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Water_column_Pigment_concentration.csv") %>% 
  dplyr::rename(date = Date, depth = `DEPTH (M)`, `Chl a [µg/l]` = `Chl a (µg-l)`, `Phaeopigments [µg/l]` = `Phaeopigments (µg-l)`) %>%
  pivot_longer(`Chl a [µg/l]`:`Phaeopigments [µg/l]`, names_to = "variable") %>% 
  filter(!is.na(value)) %>% 
  mutate(lon = -as.numeric(measurements::conv_unit(`LONGITUDE (DDM)`, "deg_dec_min", "dec_deg")),
         lat = as.numeric(measurements::conv_unit(`LATITUDE (DDM)`, "deg_dec_min", "dec_deg")),
         category = "bio",
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/DS37-V333",
         date_accessed = as.Date("2022-04-28"),
         citation = "Pigment concentration: Collected using Niskin Bottle Water Sampler. Water column MarineBasis Zackenberg. doi: 10.17897/DS37-V333") %>%
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 6), .groups = "drop")

# Nitrate plus nitrite concentrations
young_GEM_nitrate_nitrite <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Water_column_Water_Nitrate_Nitrite_Concentration.csv") %>% 
  dplyr::rename(date = Date, depth = Depth, value = `Nitrate+nitrite (NOx), µM`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "chem",
         variable = "nitrate+nitrite [µmol/l]",
         lon = -20.57, lat = 74.47,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/N626-SX63",
         date_accessed = as.Date("2022-04-28"),
         citation = "Water Nitrate+Nitrite Concentration (µmol/L). Water column MarineBasis Zackenberg. doi: 10.17897/N626-SX63") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 6), .groups = "drop")

# Combine and save
young_GEM <- rbind(young_mooring_multi, young_GEM_sea_ice_open_water, young_GEM_sea_ice_breakup, young_GEM_sea_ice_formation, 
                   young_GEM_sea_ice_thickness, young_GEM_sea_ice_snow_thickness, young_GEM_CTD_water_column, young_GEM_CTD_mooring, 
                   young_GEM_CTD_sill, young_GEM_CTD_bottom, young_GEM_phyto_sp, young_GEM_DIC, young_GEM_pCO2, young_GEM_phosphate, 
                   young_GEM_silicate, young_GEM_TA, young_GEM_air_temp_2m, young_GEM_air_temp_7m, young_GEM_precip, young_GEM_snow_fall, 
                   young_GEM_air_press, young_GEM_qnet, young_GEM_river_dis, young_GEM_PAR_land, young_GEM_gmb, young_GEM_snow_depth, 
                   young_GEM_chla, young_GEM_nitrate_nitrite) %>% mutate(site = "young")
save(young_GEM, file = "data/restricted/young_GEM.RData"); save(young_GEM, file = "~/pCloudDrive/restricted_data/GEM/young/young_GEM.RData")
rm(list = grep("young_GEM",names(.GlobalEnv),value = TRUE)); rm(young_mooring_multi); gc()


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
# pg_disko_Biology <- pg_var_melt(pg_disko_clean, query_Biology$pg_col_name, "bio") # empty
# Social
# pg_disko_Social <- pg_var_melt(pg_disko_clean, query_Social$pg_col_name, "soc") # empty

# Stack them together
pg_disko_ALL <- rbind(pg_disko_Cryosphere, pg_disko_Physical, pg_disko_Chemistry) %>% mutate(site = "disko")
data.table::fwrite(pg_disko_ALL, "~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_ALL.csv")
save(pg_disko_ALL, file = "~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_ALL.RData")

# Check that all columns were used
# colnames(pg_disko_clean)[!colnames(pg_disko_clean) %in% unique(pg_disko_ALL$variable)]

# Clean up
rm(list = grep("pg_disko",names(.GlobalEnv),value = TRUE)); gc()


## Full product ------------------------------------------------------------

# Load full EU file
if(!exists("full_product_EU")) load("data/full_data/full_product_EU.RData")

# Load PG product
if(!exists("pg_disko_ALL")) load("~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_ALL.RData")

# Biochemistry CTD cruise
# SANNA_2016_SBE19plus_CTD_profiles.nc
disko_CTD_ChlA_var <- ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/disko_bay/SANNA_2016_SBE19plus_CTD_profiles.nc")$variable
disko_CTD_ChlA <- hyper_tibble(tidync("~/pCloudDrive/FACE-IT_data/disko_bay/SANNA_2016_SBE19plus_CTD_profiles.nc")) %>% 
  left_join(hyper_tibble(activate(tidync("~/pCloudDrive/FACE-IT_data/disko_bay/SANNA_2016_SBE19plus_CTD_profiles.nc"), "D0,D1")), 
            by = c("stations", "trajectory")) %>% 
  dplyr::rename(lon = longitude, lat = latitude, depth = z_ctd) %>% 
  pivot_longer(salinity:sound_velocity, names_to = "variable", values_to = "value") %>% 
  filter(!is.na(value), value != -9999) %>% 
  left_join(disko_CTD_ChlA_var[,c("name", "units")], by = c("variable" = "name")) %>% 
  mutate(date = as.Date(as.POSIXct(time, origin = "1970-01-01")),
         date_accessed = as.Date("2021-10-20"),
         URL = "https://zenodo.org/record/4062024#.YW_TOJuxU5l",
         citation = "Carlson, Daniel F., Holding, Johnna M., Bendtsen, Jørgen, Markager, Stiig, Møller, Eva F., Meire, Lorenz, Rysgaard, Søren, Dalsgaard, Tage, & Sejr, Mikael K. (2020). CTD Profiles from the R/V Sanna cruise to Northwest Greenland fjords, August 11-31, 2016 [Data set]. Zenodo. https://doi.org/10.5281/zenodo.4062024",
         category = case_when(variable %in% c("oxygen_sat", "oxygen_mmkg") ~ "chem", 
                              variable %in% c("chl_fluor") ~ "bio", TRUE ~ "phys"),
         units = case_when(units == "practical_salinity_units" ~ "PSU",
                           units == "degrees_Celsius" ~ "°C",
                           units == "mS_per_cm" ~ "mS/cm",
                           units == "microgram_per_liter" ~ "µg/l",
                           units == "millimoles_per_square_meter" ~ "mmol/m-2",
                           units == "percent" ~ "%",
                           units == "micro_moles_per_kg" ~ "mmol/kg",
                           units == "kg_per_cubic_meter" ~ "kg/m-3",
                           units == "meters_per_second" ~ "m/s",
                           TRUE ~ units),
         variable = paste0(variable," [",units,"]"), .keep = "unused") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
rm(disko_CTD_ChlA_var); gc()

# Combine and save
full_product_disko <- rbind(dplyr::select(pg_disko_ALL, -site), 
                            disko_CTD_ChlA) %>% 
  rbind(filter(dplyr::select(full_product_EU, -site), lon >= bbox_disko[1], lon <= bbox_disko[2], lat >= bbox_disko[3], lat <= bbox_disko[4])) %>% 
  rbind(filter(dplyr::select(full_product_EU, -site), grepl("Disko", citation))) %>% distinct() %>% mutate(site = "disko")
data.table::fwrite(full_product_disko, "~/pCloudDrive/FACE-IT_data/disko_bay/full_product_disko.csv")
save(full_product_disko, file = "~/pCloudDrive/FACE-IT_data/disko_bay/full_product_disko.RData")
save(full_product_disko, file = "data/full_data/full_product_disko.RData")
plyr::l_ply(unique(full_product_disko$category), save_category, .parallel = T,
            df = full_product_disko, data_type = "full", site_name = "disko")
rm(list = grep("disko_",names(.GlobalEnv),value = TRUE)); gc()
# if(!exists("full_product_disko")) load("~/pCloudDrive/FACE-IT_data/disko_bay/full_product_disko.RData")


## GEM ---------------------------------------------------------------------

# NB: Several undownloaded wind data files as these are not currently to be used in the review paper

# Open water CTD
## NB: These single lon/lat values seem dubious
## They do not seem to agree with the similar data from Zenodo
## PAR units given by Thomas Juul-Pedersen as the original file does not have them
disko_GEM_CTD_open_water <- read_delim("~/pCloudDrive/restricted_data/GEM/disko/Disko_Data_Water_column_CTD_measurements.csv", delim = "\t") %>% 
  dplyr::rename(date = Date, depth = `depSM: Depth (salt water, m)`, `temp [°C]` = `tv268C: Temperature (IPTS-68, deg C)`,
                `cond [S/m]` = `c0S/m: Conductivity (S/m)`, `sal [PSU]` = `sal00: Salinity, Practical (PSU)`,
                `density [kg/m^3]` = `density00: Density (density, kg/m^3)`, `press [psi]` = `prdE: Pressure, Strain Gauge (psi)`, 
                `oxygen a [mg/l]` = `sbeox0Mg/L: Oxygen, SBE 43 (mg/l) a`, `oxygen b [mg/l]` = `sbeox0Mg/L: Oxygen, SBE 43 (mg/l) b`,
                `oxygen [ml/l]` = `sbeox0ML/L: Oxygen, SBE 43 (ml/l)`, `oxygen [% sat]` = `sbeox0PS: Oxygen, SBE 43 (% saturation)`,
                fluor = `flSP: Fluorescence, Seapoint`, `turbidity [FTU]` = `seaTurbMtr: Turbidity, Seapoint (FTU)`,
                `PAR [µmol photons m-2 sec-1]` = `par: PAR/Irradiance, Biospherical/Licor`) %>% 
  pivot_longer(`temp [°C]`:`PAR [µmol photons m-2 sec-1]`, names_to = "variable") %>% 
  filter(value != -9999) %>% 
  mutate(category = case_when(variable == "fluor" ~ "bio",
                              variable %in% c("oxygen a [mg/l]", "oxygen b [mg/l]",
                                              "oxygen [ml/l]", "oxygen [% sat]") ~ "chem",
                              TRUE ~ "phys"),
         lon = -53.51, lat = 69.251,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/WH30-HT61",
         date_accessed = as.Date("2022-02-03"),
         citation = "CTD measurements. Water column MarineBasis Disko. doi: 10.17897/WH30-HT61") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 6), .groups = "drop")

# Precipitation
disko_GEM_precip <- read_delim("~/pCloudDrive/restricted_data/GEM/disko/Disko_Data_Precipitation_Precipitation_60min_sample_mm.csv") %>% 
  dplyr::rename(date = Date, value = `PRE (mm)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "phys",
         lon = -53.5288581848145, lat = 69.2441711425781, depth = NA,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/KQ4Z-SD22",
         date_accessed = as.Date("2022-04-28"),
         citation = "Precipitation, sampled once per hour. Precipitation ClimateBasis Disko. doi: 10.17897/KQ4Z-SD22") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category) %>% 
  summarise(`precipitation [mm/h]` = round(mean(value, na.rm = T), 2),
            `precipitation [mm/day]` = sum(value, na.rm = T), .groups = "drop") %>% 
  pivot_longer(`precipitation [mm/h]`:`precipitation [mm/day]`, names_to = "variable")

# Air pressure
disko_GEM_air_press <- read_delim("~/pCloudDrive/restricted_data/GEM/disko/Disko_Data_Pressure_Air_pressure_60min_sample_hPa.csv") %>% 
  dplyr::rename(date = Date, value = `Pressure (hPa)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "phys",
         variable = "Air pressure [hPa]",
         lon = -53.5288581848145, lat = 69.2441711425781, depth = -2,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/BQKG-JJ88",
         date_accessed = as.Date("2022-04-28"),
         citation = "Atmospheric pressure - sampled once per hour. Pressure ClimateBasis Disko. doi: 10.17897/BQKG-JJ88") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 2), .groups = "drop")

# Shortwave radiation at 2 m
disko_GEM_swr <- read_delim("~/pCloudDrive/restricted_data/GEM/disko/Disko_Data_Radiation_Short_wave_incoming_radiation_@_200_cm_5min_average_W_m2.csv") %>% 
  dplyr::rename(date = Date, value = `SRI (W/m2)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "phys",
         variable = "Shortwave radiation (incoming) [W/m2]",
         lon = -53.5288581848145, lat = 69.2441711425781, depth = -2,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/0TSZ-ZV79",
         date_accessed = as.Date("2022-04-28"),
         citation = "Incoming shortwave radition, measured at 200 cm above ground, averaged over 5 min interval with time label at end of bin. Radiation ClimateBasis Disko. doi: 10.17897/0TSZ-ZV79") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 2), .groups = "drop")

# Snow depth
## NB: These might be precipitation values...
disko_GEM_snow1 <- read_delim("~/pCloudDrive/restricted_data/GEM/disko/Disko_Data_Snow_depth_Snow_depth_3min_average_every_hour_m.csv") %>% 
  dplyr::rename(date = Date, value = `SD (m)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "cryo",
         variable = "snow depth [m]",
         lon = -53.5288581848145, lat = 69.2441711425781, depth = NA,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/FDJZ-6X91",
         date_accessed = as.Date("2022-04-28"),
         citation = "Snow depth measured by ultrasonic ranger. Snow depth ClimateBasis Disko. doi: 10.17897/FDJZ-6X91") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 3), .groups = "drop")

# Snow depth 
disko_GEM_snow2 <- read_delim("~/pCloudDrive/restricted_data/GEM/disko/Disko_Data_Snow_cover_Snow_sonic_ranger_height.csv") %>% 
  dplyr::rename(value = h_snow_A) %>% 
  filter(value != -9999) %>% 
  mutate(category = "cryo",
         variable = "snow depth [m]",
         lon = -53.51, lat = 69.251, depth = NA,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/VF9W-CE81",
         date_accessed = as.Date("2022-05-03"),
         citation = "Snow height observations at LYN-A. Snow cover GlacioBasis Disko. doi: 10.17897/VF9W-CE81") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 3), .groups = "drop")

# Glacier ice mass balance
# NB: There are multiple sites, but only on set of lon/lat given
# It was decided to create daily means of these values...
disko_GEM_gmb <- read_delim("~/pCloudDrive/restricted_data/GEM/disko/Disko_Data_Glacier_ice_Surface_mass_balance.csv") %>% 
  filter(ablation != -9999) %>% 
  dplyr::rename(value = ablation) %>% 
  mutate(category = "cryo",
         variable = "ablation [m w.e.]",
         lon = -53.51, lat = 69.251, depth = NA,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/QXKF-G654",
         date_accessed = as.Date("2022-05-03"),
         citation = "Ablation stake readings in the ablation zone. Glacier ice GlacioBasis Disko. doi: 10.17897/QXKF-G654") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 2), .groups = "drop")

# Glacier ice temperature
disko_GEM_glacier_ice <- read_delim("~/pCloudDrive/restricted_data/GEM/disko/Disko_Data_Glacier_ice_AWS_LYN_A.csv") %>% 
  filter(qual != -9999) %>% 
  dplyr::select(lon, lat, date, alt, h_ice:ice_T8) %>% 
  pivot_longer(h_ice:ice_T8, names_to = "variable") %>% 
  filter(value != -9999) %>% 
  mutate(category = "cryo",
         variable = case_when(variable == "h_ice" ~ "ice depth [m]",
                              grepl("ice_", variable) ~ paste0(variable," [°C]")),
         lon = -53.51, lat = 69.251, depth = -alt,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/JQ7G-3G55",
         date_accessed = as.Date("2022-05-03"),
         citation = "Ice temperature profile at station LYN-A. Glacier ice GlacioBasis Disko. doi: 10.17897/JQ7G-3G55") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 3), .groups = "drop")

# Air temperature at 2 m
disko_GEM_air_temp_2m <- read_delim("~/pCloudDrive/restricted_data/GEM/disko/Disko_Data_Temperature_Air_temperature_@_200_cm_60min_sample_DegreesC.csv") %>% 
  dplyr::rename(date = Date, value = `AT (°C)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "phys",
         variable = "TTT [°C]",
         lon = -53.5288581848145, lat = 69.2441711425781, depth = -2,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/19SC-2708",
         date_accessed = as.Date("2022-04-28"),
         citation = "Air temperature at 200 cm above ground, sampled once per hour. Temperature ClimateBasis Disko. doi: 10.17897/19SC-2708") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 3), .groups = "drop")

# Air temperature at 7.5 m
disko_GEM_air_temp_7m <- read_delim("~/pCloudDrive/restricted_data/GEM/disko/Disko_Data_Temperature_Air_temperature_@_750_cm_60min_sample_DegreesC.csv") %>% 
  dplyr::rename(date = Date, value = `AT (°C)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "phys",
         variable = "TTT [°C]",
         lon = -53.5288581848145, lat = 69.2441711425781, depth = -7.5,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/71Q9-KP53",
         date_accessed = as.Date("2022-04-28"),
         citation = "Air temperature at 750 cm above ground, sampled once per hour. Temperature ClimateBasis Disko. doi: 10.17897/71Q9-KP53") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 3), .groups = "drop")

# Cruise CTD data
## NB: Longitude values corrected for - sign
disko_GEM_CTD_cruise <- read_delim("~/pCloudDrive/restricted_data/GEM/disko/Disko_Data_Water_column_Disko_Bay_Cruise_2019_CTD_measurements.csv") %>% 
  rename(date = Date, lat = Latitude, lon = Longitude, depth = `depSM: Depth (salt water, m)`, `temp [°C]` = `tv268C: Temperature (IPTS-68, deg C)`,
         `conductivity [S/m]` = `c0S/m: Conductivity (S/m)`, `salinity [PSU]` = `sal00: Salinity, Practical (PSU)`, 
         `density [kg/m3]` = `density00: Density (density, kg/m^3)`, `pressure [psi]` = `prdE: Pressure, Strain Gauge (psi)`,
         fluorescence = `flSP: Fluorescence, Seapoint`, `turbidity [FTU]` = `seaTurbMtr: Turbidity, Seapoint (FTU)`, 
         `PAR [µmol photons m2/sec]` = `par: PAR/Irradiance, Biospherical/Licor`) %>% 
  pivot_longer(`temp [°C]`:`PAR [µmol photons m2/sec]`, names_to = "variable") %>% 
  mutate(category = case_when(variable == "fluorescence" ~ "bio", TRUE ~ "phys"),
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/VB94-Y512",
         date_accessed = as.Date("2022-04-28"),
         lon = -lon,
         citation = "Disko Bay Cruise 2019, CTD measurements. Water column MarineBasis Disko. doi: 10.17897/VB94-Y512") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")

# Historic temperature and salinity data
## NB: Longitude values corrected for - sign
disko_GEM_historic_ts <- read_delim("~/pCloudDrive/restricted_data/GEM/disko/Disko_Data_Water_column_Historic_temperature_and_salinity_1924_to_2010.csv") %>% 
  dplyr::rename(date = Date, depth = `Depth (m)`, `temp [°C]` = `Temperature (deg C)`, `salinity [PSU]` = `Salinity(psu)`, 
                lat = `Latitude (degrees_north)`, lon = `Longitude (degrees_east)`) %>% 
  dplyr::select(lon, lat, date, depth, `temp [°C]`, `salinity [PSU]`, `Sigma-t`) %>% 
  pivot_longer(`temp [°C]`:`Sigma-t`, names_to = "variable") %>% 
  filter(!is.na(value)) %>% 
  mutate(category = "phys",
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/62VX-AX79",
         date_accessed = as.Date("2022-04-28"),
         lon = -lon,
         citation = "Historic temperature and salinity, 1924 to 2010. Water column MarineBasis Disko. doi: 10.17897/62VX-AX79") %>% 
    # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 6), .groups = "drop")

# Combine and save
disko_GEM <- rbind(disko_GEM_CTD_open_water, disko_GEM_precip, disko_GEM_air_press, disko_GEM_swr, disko_GEM_snow1, disko_GEM_snow2, 
                   disko_GEM_gmb, disko_GEM_glacier_ice, disko_GEM_air_temp_2m, disko_GEM_air_temp_7m, disko_GEM_CTD_cruise, 
                   disko_GEM_historic_ts) %>% mutate(site = "disko")
save(disko_GEM, file = "data/restricted/disko_GEM.RData"); save(disko_GEM, file = "~/pCloudDrive/restricted_data/GEM/disko/disko_GEM.RData")
rm(list = grep("disko_GEM",names(.GlobalEnv),value = TRUE)); gc()


# Nuup Kangerlua ----------------------------------------------------------

## PG product --------------------------------------------------------------

# Load pg Nuup Kangerlua files
system.time(
  pg_nuup_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_nuup)
) # 91 seconds

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
  mutate(date = case_when(nchar(date) == 4 ~ paste0(date,"-01-01"),
                          nchar(date) == 7 ~ paste0(date,"-01-01"),
                          # is.na(date) & !is.na(Date) ~ as.character(Date),
                          # is.na(date) & !is.na(`Sampling date`) ~ `Sampling date`, # There is an issue with these values
                          TRUE ~ date),
         # date = as.character(date),
         date = ifelse(date == "", NA, date),
         date = as.Date(gsub("T.*", "", date))) %>%
         # date = as.Date(date)) %>%
  # Manage depth column
  mutate(depth = case_when(!is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`))) %>%
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
# pg_nuup_Biology <- pg_var_melt(pg_nuup_clean, query_Biology$pg_col_name, "bio") # empty
# Social
# pg_nuup_Social <- pg_var_melt(pg_nuup_clean, query_Social$pg_col_name, "soc") # empty

# Stack them together
pg_nuup_ALL <- rbind(pg_nuup_Cryosphere, pg_nuup_Physical, pg_nuup_Chemistry) %>% mutate(site = "nuup")
data.table::fwrite(pg_nuup_ALL, "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/pg_nuup_ALL.csv")
save(pg_nuup_ALL, file = "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/pg_nuup_ALL.RData")

# Check that all columns were used
# colnames(pg_nuup_clean)[!colnames(pg_nuup_clean) %in% unique(pg_nuup_ALL$variable)]

# Clean up
rm(list = grep("pg_nuup",names(.GlobalEnv),value = TRUE)); gc()


## Full product ------------------------------------------------------------

# Load full EU file
if(!exists("full_product_EU")) load("data/full_data/full_product_EU.RData")

# Load PG product
if(!exists("pg_nuup_ALL")) load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/pg_nuup_ALL.RData")

# Combine and save
full_product_nuup <- rbind(dplyr::select(pg_nuup_ALL, -site)) %>% 
  rbind(filter(dplyr::select(full_product_EU, -site), lon >= bbox_nuup[1], lon <= bbox_nuup[2], lat >= bbox_nuup[3], lat <= bbox_nuup[4])) %>% 
  rbind(filter(dplyr::select(full_product_EU, -site), grepl("Nuup|Nuuk", citation))) %>% distinct() %>% mutate(site = "nuup")
data.table::fwrite(full_product_nuup, "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/full_product_nuup.csv")
save(full_product_nuup, file = "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/full_product_nuup.RData")
save(full_product_nuup, file = "data/full_data/full_product_nuup.RData")
plyr::l_ply(unique(full_product_nuup$category), save_category, .parallel = T,
            df = full_product_nuup, data_type = "full", site_name = "nuup")
rm(list = grep("nuup_",names(.GlobalEnv),value = TRUE)); gc()
# if(!exists("full_product_nuup")) load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/full_product_nuup.RData")


## GEM ---------------------------------------------------------------------

# NB: Several undownloaded wind data files as these are not currently to be used in the review paper
# Other radiation files are available but we are just using net radiation here

# Open water CTD
nuup_GEM_CTD_open_water <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/Nuuk_Data_Water_column_CTD_measurements.csv", delim = "\t") %>% 
  dplyr::rename(date = Date, lon = Longitude, lat = Latitude, `depth` = `Pressure, db`, `temp [°C]` = `Temperature, C`, 
                sal = Salinity, `density [kg m-3]` = `Density, Sigma-theta, kg m-3`, fluor = `Fluorescence`, 
                `PAR [µmol photons m2/sec]` = `PAR, µmol photons m-2 sec-1`, `turbidity [FTU]` = `Turbidity, FTU`, 
                `Tpot [°C]` = `Potential temperature, C`, `oxygen [µmol/kg]` = `Oxygen content, µmol kg-1`) %>% 
  pivot_longer(`temp [°C]`:`oxygen [µmol/kg]`, names_to = "variable") %>% 
  filter(!is.na(value)) %>% 
  mutate(category = case_when(variable == "fluor" ~ "bio",
                              variable == "oxygen [µmol/kg]" ~ "chem",
                              TRUE ~ "phys"),
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/KMEK-TK21",
         date_accessed = as.Date("2022-02-03"),
         citation = "CTD measurements. Water column MarineBasis Nuuk. doi: 10.17897/KMEK-TK21") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 6), .groups = "drop")

# Primary production
nuup_GEM_pp <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/Nuuk_Data_Water_column_Particulate_Pelagic_Primary_Production_mg_C_m2_d.csv", delim = "\t") %>% 
  dplyr::rename(date = DATE, depth = DEPTH, `PP [mg C/m3/d]` = `Water PP`) %>% pivot_longer(`PP [mg C/m3/d]`, names_to = "variable") %>% 
  mutate(category = "bio",
         lon = -51.883333, lat = 64.116667,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/TQQV-VJ76",
         date_accessed = as.Date("2022-02-03"),
         citation = "Particulate Pelagic Primary Production (mg C/m2/d). Water column MarineBasis Nuuk. doi: 10.17897/TQQV-VJ76") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)

# Phytoplankton relative species composition
nuup_GEM_phyto_sp <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/Nuuk_Data_Water_column_Phytoplankton_Relative_Species_Composition_Percent.csv", delim = "\t") %>% 
  dplyr::rename(date = DATE, variable = SPECIES, value = Phytopl) %>% 
  mutate(category = "bio",
         variable = paste0(variable," [%]"),
         value = as.numeric(value),
         lon = -51.883333, lat = 64.116667, depth = 0,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/Y3A4-9D86",
         date_accessed = as.Date("2022-02-03"),
         citation = "Phytoplankton Relative Species Composition (%). Water column MarineBasis Nuuk. doi: 10.17897/Y3A4-9D86") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 6), .groups = "drop")

# Silicate concentration
nuup_GEM_silicate <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/Nuuk_Data_Water_column_Silicate_Concentration_æmol_L.csv", delim = "\t") %>% 
  dplyr::rename(date = Date, lat = Latitude, lon = Longitude, depth = `Depth, m`, value = `Si(OH)4, µM`) %>% 
  mutate(category = "chem",
         variable = "Si(OH)4 [µM/l]",
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/VVQP-F862",
         date_accessed = as.Date("2022-02-03"),
         citation = "Monthly measurements of the concentration of silicate (Si(OH)4). Water column MarineBasis Nuuk. doi: 10.17897/VVQP-F862") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>%
  summarise(value = round(mean(value, na.rm = T), 6), .groups = "drop")

# ChlA concentrations
nuup_GEM_ChlA <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/Nuuk_Data_Water_column_Water_Chlorophyll_a_Concentration_æg_L.csv", delim = "\t") %>% 
  dplyr::rename(date = DATE, depth = DEPTH, value = `WATER CHL A`) %>% 
  mutate(category = "bio",
         variable = "Chl a [µg/l]",
         lon = -51.883333, lat = 64.116667,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/1QK2-6B74",
         date_accessed = as.Date("2022-02-03"),
         citation = "Water Chlorophyll a Concentration (µg/L). Water column MarineBasis Nuuk. doi: 10.17897/1QK2-6B74") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>%
  summarise(value = round(mean(value, na.rm = T), 8), .groups = "drop")

# Precipitation
nuup_GEM_precip <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/Nuuk_Data_Precipitation_Precipitation_accumulated_mm.csv") %>% 
  dplyr::rename(date = Date, value = `Precipitation accumulated (mm)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "phys",
         variable = "precipitation [mm/h]",
         lon = -51.34305568, lat = 64.133055, depth = NA,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/SXJ8-WA79",
         date_accessed = as.Date("2022-04-28"),
         citation = "Precipitation accumulated - 60min (mm). Precipitation ClimateBasis Nuuk. doi: 10.17897/SXJ8-WA79") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category) %>% 
  summarise(`precipitation [mm/h]` = round(mean(value, na.rm = T), 2),
            `precipitation [mm/day]` = sum(value, na.rm = T), .groups = "drop") %>% 
  pivot_longer(`precipitation [mm/h]`:`precipitation [mm/day]`, names_to = "variable")

# Snow depth
nuup_GEM_snow <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/Nuuk_Data_Precipitation_Snow_depth_m.csv") %>% 
  dplyr::rename(date = Date, value = `SD (m)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "cryo",
         variable = "Snow depth [m]",
         lon = -51.34305568, lat = 64.133055, depth = NA,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/19YH-DB49",
         date_accessed = as.Date("2022-04-28"),
         citation = "Snow depth - 3min average every 180min (m). Precipitation ClimateBasis Nuuk. doi: 10.17897/19YH-DB49") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 3), .groups = "drop")

# Air pressure
nuup_GEM_air_press <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/Nuuk_Data_Pressure_Air_pressure_@_150_cm_30min_average_hPa.csv") %>% 
  dplyr::rename(date = Date, value = `Air pressure, 150 cm - 30min average (hPa)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "phys",
         variable = "Air pressure [hPa]",
         lon = -51.34305568, lat = 64.133055, depth = -1.5,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/EG71-TC03",
         date_accessed = as.Date("2022-04-28"),
         citation = "Air pressure, 150 cm - 30min average (hPa). Pressure ClimateBasis Nuuk. doi: 10.17897/EG71-TC03") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 2), .groups = "drop")

# Net radiation at 2 m
nuup_GEM_qnet <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/Nuuk_Data_Radiation_Net_radiation_@_200_cm_5min_average_W_m2.csv") %>% 
  dplyr::rename(date = Date, value = `NR (W/m2)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "phys",
         variable = "Net radiation (incoming) [W/m2]",
         lon = -51.34305568, lat = 64.133055, depth = -2,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/P4DC-7863",
         date_accessed = as.Date("2022-04-28"),
         citation = "Net radiation (CNR1), 200 cm (W/m2) calculated from QCed radiation components. Radiation ClimateBasis Nuuk. doi: 10.17897/P4DC-7863") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 2), .groups = "drop")

# Air temperature at 2 m
nuup_GEM_air_temp_2m <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/Nuuk_Data_Temperature_Air_temperature_@_200_cm_30min_average_DegreesC.csv") %>% 
  dplyr::rename(date = Date, value = `Air temperature, 200 cm - 30min average (°C)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "phys",
         variable = "TTT [°C]",
         lon = -51.34305568, lat = 64.133055, depth = -2,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/PGN3-7597",
         date_accessed = as.Date("2022-04-28"),
         citation = "Air temperature, 200 cm - 30min average (°C). Temperature ClimateBasis Nuuk. doi: 10.17897/PGN3-7597") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 2), .groups = "drop")

# Air temperature at 10 m
nuup_GEM_air_temp_10m <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/Nuuk_Data_Temperature_Air_temperature_@_1000_cm_30min_average_DegreesC.csv") %>% 
  dplyr::rename(date = Date, value = `Air temperature, 1000 cm - 30min average (°C)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "phys",
         variable = "TTT [°C]",
         lon = -51.34305568, lat = 64.133055, depth = -10,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/969D-S292",
         date_accessed = as.Date("2022-04-28"),
         citation = "Air temperature, 1000 cm - 30min average (°C). Temperature ClimateBasis Nuuk. doi: 10.17897/969D-S292") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 2), .groups = "drop")

# Air temperature at 2 m at Kobbefjord
nuup_GEM_air_temp_2m_Kobbefjord <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/Nuuk_Data_River_hydrology_Air_temperature_@_200_cm_30min_average_DegreesC.csv") %>% 
  dplyr::rename(date = Date, value = `AT (°C)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "phys",
         variable = "TTT [°C]",
         lon = -51.38077775, lat = 64.133111, depth = -2,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/QWJG-TB45",
         date_accessed = as.Date("2022-04-28"),
         citation = "Kobbefjord hydrometric station air temperature - 30m average (°C). River hydrology ClimateBasis Nuuk. doi: 10.17897/QWJG-TB45") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 2), .groups = "drop")

# River discharge at Kingigtorssuaq
nuup_GEM_Kingigtorssuaq <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/Nuuk_Data_River_hydrology_Discharge_@_river_Kingigtorssuaq_m3_s.csv") %>% 
  dplyr::rename(date = Date, value = `Q (m3/s)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "cryo",
         variable = "Q [m3/s]",
         lon = -51.5795, lat = 64.138722, depth = NA,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/VWS5-4688",
         date_accessed = as.Date("2022-04-28"),
         citation = "Discharge at river Kingigtorssuaq (m3/s). River hydrology ClimateBasis Nuuk. doi: 10.17897/VWS5-4688") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 4), .groups = "drop")

# River discharge at Kobbefjord
nuup_GEM_Kobbefjord <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/Nuuk_Data_River_hydrology_Discharge_@_river_Kobbefjord_m3_s.csv") %>% 
  dplyr::rename(date = Date, value = `Q (m3/s)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "cryo",
         variable = "Q [m3/s]",
         lon = -51.38077775, lat = 64.133111, depth = NA,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/H2MR-PP28",
         date_accessed = as.Date("2022-04-28"),
         citation = "Discharge at river Kobbefjord (m3/s). River hydrology ClimateBasis Nuuk. doi: 10.17897/H2MR-PP28") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 4), .groups = "drop")

# River discharge at Oriartorfik
nuup_GEM_Oriartorfik <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/Nuuk_Data_River_hydrology_Discharge_@_river_Oriartorfik_m3_s.csv") %>% 
  dplyr::rename(date = Date, value = `Q (m3/s)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "cryo",
         variable = "Q [m3/s]",
         lon = -51.40661103, lat = 64.171861, depth = NA,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/VHDT-PX65",
         date_accessed = as.Date("2022-04-28"),
         citation = "Discharge at river Oriartorfik (m3/s). River hydrology ClimateBasis Nuuk. doi: 10.17897/VHDT-PX65") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 4), .groups = "drop")

# River discharge at Teqinngalip
nuup_GEM_Teqinngalip <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/Nuuk_Data_River_hydrology_Discharge_@_river_Teqinngalip_m3_s.csv") %>% 
  dplyr::rename(date = Date, value = `Q (m3/s)`) %>% 
  filter(value != -9999) %>% 
  mutate(category = "cryo",
         variable = "Q [m3/s]",
         lon = -51.54838892, lat = 64.158611, depth = NA,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/DFVW-4R58",
         date_accessed = as.Date("2022-04-28"),
         citation = "Discharge at river Teqinngalip (m3/s). River hydrology ClimateBasis Nuuk. doi: 10.17897/DFVW-4R58") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 4), .groups = "drop")

# Nitrate and nitrite concentrations
nuup_GEM_nitrate_nitrite <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/Nuuk_Data_Water_column_Nitrate_Nitrite_Concentration_цmol_L.csv") %>% 
  dplyr::rename(date = Date, depth = `Depth, m`, value = `NOx, µM`, lon = Longitude, lat = Latitude) %>% 
  mutate(category = "chem",
         variable = "nitrate+nitrite [µmol/l]",
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/3NQX-FA50",
         date_accessed = as.Date("2022-04-28"),
         citation = "Monthly measurements of the concentration of Nox, which is Nitrate (NO3) plus Nitrite (NO2). Water column MarineBasis Nuuk. doi: 10.17897/3NQX-FA50") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)

# Fish larvae presence 2006-2013
## NB: These values have erroneously been given Zackenberg coordinates on the GEM database
nuup_GEM_fish_larvae <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/Nuuk_Data_Water_column_Fish_Larvae_Species_Composition_individuals_m.csv") %>% 
  dplyr::rename(date = DATE, variable = SPECIES, value = `FISH LARV`) %>% 
  mutate(category = "bio",
         variable = paste0(variable," [individuals/m3]"),
         value = as.numeric(value),
         lon = -51.54838892, lat = 64.158611, depth = 0, # NB: Coordinates are the general estimate of Nuuk as proper coords not provided
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/SBG4-YH51",
         date_accessed = as.Date("2022-04-28"),
         citation = "Fish Larvae Species Composition (individuals/m3). Water column MarineBasis Nuuk. doi: 10.17897/SBG4-YH51") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 8), .groups = "drop")

# Leaf growth of Saccharina latissima (g)
## NB: These values have erroneously been given Zackenberg coordinates on the GEM database
## Decided to create daily averages from replicate data
nuup_GEM_Slat_g <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/Nuuk_Data_Benthic_vegetation_Leaf_Growth_of_Saccharina_latissima_g_C_yr.csv") %>% 
  dplyr::rename(date = DATE, value = `KELP BLADE GROWTH - BIOMASS`) %>% 
  mutate(category = "bio",
         variable = paste0("S. latissima blade growth (",LOCATION,") [g C/year]"),
         lon = -51.54838892, lat = 64.158611, depth = NA, # NB: Coordinates are the general estimate of Nuuk as proper coords not provided
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/EWVJ-KX92",
         date_accessed = as.Date("2022-04-28"),
         citation = "Leaf Growth of Saccharina latissima (g C/yr). Benthic vegetation MarineBasis Nuuk. doi: 10.17897/EWVJ-KX92") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 3), .groups = "drop")

# Leaf growth of Saccharina latissima (cm)
## NB: These values have erroneously been given Zackenberg coordinates on the GEM database
## Decided to create daily averages from replicate data
nuup_GEM_Slat_cm <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/Nuuk_Data_Benthic_vegetation_Leaf_Growth_of_Saccharina_latissima_cm_yr.csv") %>% 
  dplyr::rename(date = DATE, value = `KELP BLADE GROWTH - LENGTH`) %>%
  mutate(category = "bio",
         variable = paste0("S. latissima blade growth (",LOCATION,") [cm/year]"),
         lon = -51.54838892, lat = 64.158611, depth = NA, # NB: Coordinates are the general estimate of Nuuk as proper coords not provided
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/TDEK-WQ82",
         date_accessed = as.Date("2022-04-28"),
         citation = "Leaf Growth of Saccharina latissima (cm/yr). Benthic vegetation MarineBasis Nuuk. doi: 10.17897/TDEK-WQ82") %>% 
  # dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = round(mean(value, na.rm = T), 1), .groups = "drop")

# Ascophyllum nodosum tips (g)
nuup_GEM_Anod_g <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/Nuuk_Data_Benthic_Vegetation_Ascophyllum_nodosum_segment_biomass.csv") %>% 
  dplyr::rename(date = Date, lon = Longitude, lat = Latitude, `[Avg g dw]` = `Avg (g dw)`, 
                `[Std g dw]` = `Std (g dw)`, `[n]` = N, `[SE g dw]` = `SE (g dw)`) %>%
  pivot_longer(`[Avg g dw]`:`[SE g dw]`, names_to = "units") %>% 
  mutate(category = "bio",
         variable = paste0("A. nodosum tip growth (",`Segment (year)`," year segment - ",Site," ",Tidalzone," Tidal Zone) ",units),
         lon = -51.38, lat = 64.13, depth = NA,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/KEMY-JJ24",
         date_accessed = as.Date("2022-04-28"),
         citation = "Annual (2012-2021) sampling of 20 Ascophyllum nodosum tips. Benthic vegetation MarineBasis Nuuk. doi: 10.17897/KEMY-JJ24") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)

# Ascophyllum nodosum tips (cm)
nuup_GEM_Anod_cm <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/Nuuk_Data_Benthic_Vegetation_Ascophyllum_nodosum_segment_length.csv") %>% 
  dplyr::rename(date = Date, lon = Longitude, lat = Latitude, `[Avg cm]` = `Avg (cm)`, 
                `[Std cm]` = `Std (cm)`, `[n]` = N, `[SE cm]` = `SE (cm)`) %>%
  pivot_longer(`[Avg cm]`:`[SE cm]`, names_to = "units") %>% 
  mutate(category = "bio",
         variable = paste0("A. nodosum tip growth (",`Segment (year)`," year segment - ",Site," ",Tidalzone," Tidal Zone) ",units),
         lon = -51.38, lat = 64.13, depth = NA,
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/4NJK-ZV13",
         date_accessed = as.Date("2022-04-28"),
         citation = "Annual (2012-2021) sampling of 20 Ascophyllum nodosum tips. Benthic vegetation MarineBasis Nuuk. doi: 10.17897/4NJK-ZV13") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)

# Combine and save
nuup_GEM <- rbind(nuup_GEM_CTD_open_water, nuup_GEM_pp, nuup_GEM_phyto_sp, nuup_GEM_silicate, nuup_GEM_ChlA, nuup_GEM_precip, nuup_GEM_snow,
                  nuup_GEM_air_press, nuup_GEM_qnet, nuup_GEM_air_temp_2m, nuup_GEM_air_temp_10m, nuup_GEM_air_temp_2m_Kobbefjord, 
                  nuup_GEM_Kingigtorssuaq, nuup_GEM_Kobbefjord, nuup_GEM_Oriartorfik, nuup_GEM_Teqinngalip, nuup_GEM_nitrate_nitrite,
                  nuup_GEM_fish_larvae, nuup_GEM_Slat_g, nuup_GEM_Slat_cm, nuup_GEM_Anod_g, nuup_GEM_Anod_cm) %>% mutate(site = "nuup")
save(nuup_GEM, file = "data/restricted/nuup_GEM.RData"); save(nuup_GEM, file = "~/pCloudDrive/restricted_data/GEM/nuup/nuup_GEM.RData")
rm(list = grep("nuup_GEM",names(.GlobalEnv),value = TRUE)); gc()


# Norway ------------------------------------------------------------------

## PG product -------------------------------------------------------------

# There is no PG product for Norway


## Full product -----------------------------------------------------------

# National statistics

# Porsangerfjorden ---------------------------------------------------------

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
  # dplyr::select(contains(c("depth", "press", "bathy", "elev")), everything()) %>%  # Look at depth columns
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
pg_por_ALL <- rbind(pg_por_Cryosphere, pg_por_Physical, pg_por_Chemistry) %>% mutate(site = "por")
data.table::fwrite(pg_por_ALL, "~/pCloudDrive/FACE-IT_data/porsangerfjorden/pg_por_ALL.csv")
save(pg_por_ALL, file = "~/pCloudDrive/FACE-IT_data/porsangerfjorden/pg_por_ALL.RData")

# Check that all columns were used
# colnames(pg_por_clean)[!colnames(pg_por_clean) %in% unique(pg_por_ALL$variable)]

# Clean up
rm(list = grep("pg_por",names(.GlobalEnv),value = TRUE)); gc()


## Full product ------------------------------------------------------------

# Load full EU file
if(!exists("full_product_EU")) load("data/full_data/full_product_EU.RData")

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
         category = "cryo", variable = "ice area [km2]") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)

## Hydrographic data
por_hydro <- plyr::ldply(1952:2013, load_nor_hydro, date_accessed = as.Date("2021-09-08"))

## IMR red king crab survey data
por_IMR_kingcrab_count <- read_delim("~/pCloudDrive/FACE-IT_data/porsangerfjorden/IMR/dwca-imr_kingcrab-v1.2/measurementorfact.txt")
por_IMR_kingcrab <- read_delim("~/pCloudDrive/FACE-IT_data/porsangerfjorden/IMR/dwca-imr_kingcrab-v1.2/occurrence.txt") %>% 
  left_join(por_IMR_kingcrab_count, by = "id") %>% 
  unite(year, month, day, sep = "-", remove = T, col = "date") %>% 
  dplyr::rename(lon = decimalLongitude, lat = decimalLatitude, variable = scientificName, value = measurementValue) %>% 
  mutate(variable = paste0(variable," [count]")) %>% 
  rowwise() %>% 
  mutate(depth = mean(c(minimumDepthInMeters, maximumDepthInMeters), na.rm = T)) %>% 
  dplyr::select(lon, lat, date, depth, variable, value) %>%
  distinct() %>% 
  filter(lon >= bbox_por[1], lon <= bbox_por[2], lat >= bbox_por[3], lat >= bbox_por[4]) %>% 
  mutate(date = as.Date(date),
         depth = case_when(is.na(depth) ~ as.numeric(NA), TRUE ~ depth),
         date_accessed = as.Date("2022-11-14"),
         URL = "https://gbif.imr.no/ipt/resource?r=imr_kingcrab",
         citation = "Hjelset, Ann Merete; Institute of Marine Research, Norway (2017): Red king crab survey data from Finnmark Northern Norway in the period 1994 -2016 http://gbif.imr.no/ipt/resource?id=imr_kingcrab/v1.2.xml",
         category = "bio") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
rm(por_IMR_kingcrab_count); gc()

# Combine and save
full_product_por <- rbind(dplyr::select(pg_por_ALL, -site), 
                          por_mooring_GFI, por_sea_ice, por_hydro, por_IMR_kingcrab) %>% 
  rbind(filter(dplyr::select(full_product_EU, -site), lon >= bbox_por[1], lon <= bbox_por[2], lat >= bbox_por[3], lat <= bbox_por[4])) %>% 
  rbind(filter(dplyr::select(full_product_EU, -site), grepl("Porsanger", citation))) %>% distinct() %>% mutate(site = "por")
save(full_product_por, file = "~/pCloudDrive/FACE-IT_data/porsangerfjorden/full_product_por.RData")
save(full_product_por, file = "data/full_data/full_product_por.RData")
plyr::l_ply(unique(full_product_por$category), save_category, .parallel = T,
            df = full_product_por, data_type = "full", site_name = "por")
rm(list = grep("por_",names(.GlobalEnv),value = TRUE)); gc()
# if(!exists("full_product_por")) load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/full_product_por.RData")


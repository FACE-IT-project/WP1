# code/data_product.R
# This script houses the code used to create data products from the many disparate files

# NB: If new PANGAEA data are downloaded, it is necessary to go through the 
# column checking (e.g. date, depth, lon/lat) process again for each PG product section
# There is a lot of commented out code to help facilitate this process

# TODO: For v1.4: meta-data; Make dummy entries for all of the commented out links below
# that have drivers/variables etc. when possible so that their meta-data can be harvested and
# put into the new automagic database.


# Setup -------------------------------------------------------------------

# Libraries
source("code/functions.R")
source("code/key_drivers.R")
source("users/calin/code/formulas.R")
library(stringi)
library(pxweb)
library(clock)

# Re-run full data collection pipeline
# system.time(
# source("code/data_collection.R")
# )

# For processing below
clean_cols <- c("date_accessed", "URL", "citation", "site", "lon", "lat", "date", "depth", "spp_name", "spp_value")

# PANGAEA files
pg_files <- dir("data/pg_data", pattern = "pg_", full.names = T)

# PANGAEA metadata files
pg_meta_files <- map_dfr(dir("metadata", all.files = T, full.names = T, pattern = "_doi.csv"), load_pg) |> 
  dplyr::select(-Error, -parent_doi)


# Site oriented data products ---------------------------------------------

## European Arctic ---------------------------------------------------------

### PG product --------------------------------------------------------------

# There is no EU PANGAEA product
# Moving towards v2.0 the scraping of EU data from PANGAEA was abandoned
# in favour of scraping at each site specifically

### Species ----------------------------------------------------------------

# EU (east) harp seal population
EU_epagr_population <- read.csv("~/pCloudDrive/FACE-IT_data/EU_arctic/production-of-pups-and-e.csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`Modelled.production.of.pups`, `Modelled.total.stock.size`, `Survey.counts.of.pups`)) %>% 
  mutate(date_accessed = as.Date("2023-04-14"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/harp-seal/",
         citation = "Institute of Marine Research (2022). Production of pups and estimated population size for harp seal in the East Ice. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/harp-seal.html",
         lon = NA, lat = NA, depth = NA,
         Species = "Pagophilus groenlandicus",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " ", str_replace_all(tolower(name),"\\."," ")," [n]"),
         category = "bio",
         driver ="biomass",
         site = "east ice",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))

# EU (west) harp seal population
EU_wpagr_population <- read.csv("~/pCloudDrive/FACE-IT_data/EU_arctic/production-of-pups-and-e.csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`Modelled.production.of.pups`, `Modelled.total.stock.size`, `Survey.counts.of.pups`)) %>% 
  mutate(date_accessed = as.Date("2023-04-14"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/harp-seal/",
         citation = "Institute of Marine Research (2022). Production of pups and estimated population size for harp seal in the West Ice. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/harp-seal.html",
         lon = NA, lat = NA, depth = NA,
         Species = "Pagophilus groenlandicus",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " ", str_replace_all(tolower(name),"\\."," ")," [n]"),
         category = "bio",
         driver ="biomass",
         site = "west ice",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))

# barents polar cod biomass
barents_polar_cod <- read.csv("~/pCloudDrive/FACE-IT_data/svalbard/biomass-of-polar-cod-in.csv", sep = ";", dec = ",") %>% 
  mutate(date_accessed = as.Date("2023-04-11"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/biomass-of-polar-cod-in-the-barents-sea/", 
         citation = "Institute of Marine Research (2022). Biomass of polar cod in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/polar-cod.html", 
         lon = NA, lat = NA, depth = NA, 
         Species = "Boreogadus saida",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " [10^6 kg]"),
         category = "bio",
         driver ="biomass",
         site = "barents sea", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::rename(value = Biomass) %>% 
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))

# barents capelin stock
barents_capelin_stock <- read.csv("~/pCloudDrive/FACE-IT_data/svalbard/capelin-stock-in-the-bar.csv",sep = ";" , dec = ",") %>% 
  pivot_longer(cols = c(`Mature.stock`, `Immature.stock`)) %>% 
  mutate(date_accessed = as.Date("2023-04-12"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/capelin-stock-in-the-barents-sea/", 
         citation = "Institute of Marine Research (2022). Capelin stock in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/capelin.html", 
         lon = NA, lat = NA, depth = NA, 
         Species = "Mallotus villosus",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " ", str_replace(tolower(name),"\\."," ") ," [10^6 kg]"),
         category = "bio",
         driver ="biomass",
         site = "barents sea", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))

# barents golden redfish population
barents_golden_redfish_population <- read_delim("~/pCloudDrive/FACE-IT_data/svalbard/stock-of-golden-redfish.csv") %>% 
  pivot_longer(cols = c(`Mature stock`, `Immature stock`)) %>% 
  mutate(date_accessed = as.Date("2023-04-12"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/golden-redfish-stock-in-the-barents-sea/", 
         citation = "Institute of Marine Research (2023). Stock of golden redfish in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: https://mosj.no/en/indikator/fauna/marine-fauna/golden-redfish-stock-in-the-barents-sea/", 
         lon = NA, lat = NA, depth = NA, 
         Species = "Sebastes norvegicus",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " ", tolower(name) ," [10^3 kg]"),
         category = "bio",
         driver ="biomass",
         site = "barents sea", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))

# barents beaked redfish population
barents_beaked_redfish_population <- read.csv("~/pCloudDrive/FACE-IT_data/svalbard/stock-of-beaked-redfish.csv", sep = ";") %>% 
  pivot_longer(cols = c(`Mature.stock`, `Immature.stock`)) %>% 
  mutate(date_accessed = as.Date("2023-04-12"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/bestanden-av-snabeluer-i-barentshavet/", 
         citation = "Institute of Marine Research (2022). Stock of beaked redfish in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/deep-sea-redfish.html", 
         lon = NA, lat = NA, depth = NA, 
         Species = "Sebastes mentella",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " ", str_replace(tolower(name),"\\."," ") ," [10^6 kg]"),
         category = "bio",
         driver ="biomass",
         site = "barents sea", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))

# barents northeast arctic cod population
barents_northeast_cod_population <- read.csv("~/pCloudDrive/FACE-IT_data/svalbard/stock-of-northeast-arcti.csv", sep = ";") %>% 
  pivot_longer(cols = c(`Immature.stock`, `Spawning.stock`)) %>% 
  mutate(date_accessed = as.Date("2023-04-12"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/stock-of-northeast-arctic-cod/", 
         citation = "Institute of Marine Research (2022). Stock of Northeast Arctic cod in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/northeast-arctic-cod.html", 
         lon = NA, lat = NA, depth = NA, 
         Species = "Gadus morhua",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " ", str_replace(tolower(name),"\\."," ") ," [10^6 kg]"),
         category = "bio",
         driver ="biomass",
         site = "barents sea", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))

# barents young herring population
barents_young_herring_population <- read.csv("~/pCloudDrive/FACE-IT_data/svalbard/biomass-index-for-young.csv", sep = ";", dec = ",") %>% 
  pivot_longer(cols = c(`X1.year.olds`, `X2.year.old`, `X3.year.old`)) %>%
  mutate(date_accessed = as.Date("2023-04-13"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/bestanden-av-ungsild-i-barentshavet/",
         citation = "Institute of Marine Research (2022). Biomass index for young herring 1–3 years in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/young-herring-population.html",
         lon = NA, lat = NA, depth = NA,
         Species = "Clupea harengus",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " ", substr(str_replace_all(tolower(name),"\\."," "),2, 11)," [n]"),
         category = "bio",
         driver ="biomass",
         site = "barents sea",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))

# Combine and save
EU_species <- rbind(EU_epagr_population,
                    EU_wpagr_population,
                    barents_polar_cod, 
                    barents_beaked_redfish_population, 
                    barents_capelin_stock, 
                    barents_golden_redfish_population, 
                    barents_northeast_cod_population, 
                    barents_young_herring_population)
save(EU_species, file = "~/pCloudDrive/FACE-IT_data/EU_arctic/EU_species.RData")
write_csv(EU_species, file = "~/pCloudDrive/FACE-IT_data/EU_arctic/EU_species.csv")
rm(list = grep("EU_|barents_",names(.GlobalEnv),value = TRUE)); gc()


### Wild data ---------------------------------------------------------------

# Bathymetry data
EU_GEBCO <- data.frame(date_accessed = NA, 
                       URL = "https://www.gebco.net/", 
                       citation = "GEBCO Compilation Group (2021). GEBCO 2021 Grid. https://doi.org/10.5285/c6612cbe-50b3-0cff-e053-6c86abc09f8f", 
                       site = "EU", 
                       lon = NA, 
                       lat = NA, 
                       date = NA, 
                       depth = NA,
                       category = "phys", 
                       driver = "bathymetry", 
                       variable = "bathymetry")

# Bathymetry data
EU_IBCAO <- data.frame(date_accessed = NA, 
                       URL = "https://www.gebco.net/data_and_products/gridded_bathymetry_data/arctic_ocean/", 
                       citation = "Jakobsson, M., Mayer, L. A., Bringensparr, C., Castro, C. F., Mohammad, R., Johnson, P., ... & Zinglersen, K. B. (2020). The international bathymetric chart of the Arctic Ocean version 4.0. Scientific data, 7(1), 176.", 
                       site = "EU", 
                       lon = NA, 
                       lat = NA, 
                       date = NA, 
                       depth = NA,
                       category = "phys", 
                       driver = "bathymetry", 
                       variable = NA)

# Ice modelling output
EU_ice <- data.frame(date_accessed = NA, 
                     URL = "https://data.npolar.no/dataset/881e423a-b6f6-4c1a-8000-d802adf03a66", 
                     citation = "Liston, G., Merkouriadi, I., & Granskog, M. A. (2019). Snow-ice, snow depth and ice thickness from HIGHTSI modeling with ice motion in the Arctic Ocean in the period 1980 to 2016 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2019.881e423a", 
                     site = "EU", 
                     lon = NA, 
                     lat = NA, 
                     date = NA, 
                     depth = NA,
                     category = "cryo", 
                     driver = "sea ice", 
                     variable = c("sea ice thickness","sea ice snow cover depth"))

# CTD data from NCEI Accession 9700302
EU_NCEI_1989 <- data.frame(date_accessed = NA, 
                           URL = "https://accession.nodc.noaa.gov/9700302", 
                           citation = "Adrov, Nickoli; Murmansk Marine Biological Institute (MMBI) (2010). Physical profile data collected from bottle casts in the Barents, Greenland, and Norweigan Seas from 1989-06-01 to 1989-09-09 (NCEI Accession 9700302). [indicate subset used]. NOAA National Centers for Environmental Information. Dataset. https://accession.nodc.noaa.gov/9700302. Accessed [date].", 
                           site = "EU", 
                           lon = NA, 
                           lat = NA, 
                           date = NA, 
                           depth = NA,
                           category = "phys", 
                           driver = c("temp", "salinity"), 
                           variable = c("temp [°C]", "salinity"))
                           

# CTD data from Greenland Sea icthyoplankton cruise
## NB: Server was down when attempting to access these data on 2022-05-25
# EU_icthyo <- "~/pCloudDrive/restricted_data/PolarData/"
EU_icthyo <- data.frame(date_accessed = NA, 
                        URL = "https://www.polardata.ca/pdcsearch/PDCSearch.jsp?doi_id=13251", 
                        citation = "Bouchard, C., Chawarski, J., Geoffroy, A., & Agersted, M. (2021). Hydroacoustic data EK60 Greenland Sea August-September 2017. Waterloo, Canada: Canadian Cryospheric Information Network (CCIN). (Unpublished Data).", 
                        site = "EU", 
                        lon = NA, 
                        lat = NA, 
                        date = NA, 
                        depth = NA,
                        category = c("phys", "phys", "bio"), 
                        driver = c("temp", "salinity", "biomass"), 
                        variable = c("temp [°C]", "salinity", "fish presence"))

# Cryosphere
## Sea ice concentration
# 25 km, 1978 - 2019: daily
# ftp://sidads.colorado.edu/pub/DATASETS/NOAA/G02202_V3/north/daily/
# 6.25 km, possibly 10 m, 2002 - 2021: daily
# https://seaice.uni-bremen.de/data/amsr2/

## Randolph Glacier Inventory
# https://nsidc.org/data/nsidc-0770/versions/6


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
# code/data_processing.R
load("~/pCloudDrive/FACE-IT_data/EU_arctic/SOCAT_EU.RData")

# GLODAP data
# code/data_processing.R
load("~/pCloudDrive/FACE-IT_data/EU_arctic/GLODAP_EU.RData")

# Combine and save
EU_wild <- rbind(EU_zooplankton, EU_YMER, EU_Codispoti, EU_protists, EU_Popova, EU_green_fjords,
                 EU_IMR_spp_obs, EU_SOCAT, EU_GLODAP) |> mutate(site = "EU") |> 
  left_join(full_var_list, by = c("category", "variable")) |> 
  dplyr::select(date_accessed, URL, citation, site, lon, lat, date, depth, category, driver, variable, value)
data.table::fwrite(EU_wild, "~/pCloudDrive/FACE-IT_data/EU_arctic/EU_wild.csv")
save(EU_wild, file = "~/pCloudDrive/FACE-IT_data/EU_arctic/EU_wild.RData")
rm(list = grep("EU_",names(.GlobalEnv),value = TRUE)); gc()

# Or if only one new file etc. was added:
# if(!exists("EU_wild")) load("~/pCloudDrive/FACE-IT_data/EU_arctic/EU_wild.RData")
# EU_wild <- rbind(new_data, EU_wild) |> distinct()
# data.table::fwrite(EU_wild, "~/pCloudDrive/FACE-IT_data/EU_arctic/EU_wild.csv")
# save(EU_wild, file = "~/pCloudDrive/FACE-IT_data/EU_arctic/EU_wild.RData")
# rm(list = grep("EU_",names(.GlobalEnv),value = TRUE)); gc()


### Full product ------------------------------------------------------------

# Load PANGAEA
## NB: There are intentionally none

# Load species
if(!exists("EU_species")) load("~/pCloudDrive/FACE-IT_data/EU_arctic/EU_species.RData")

# Load wild data
if(!exists("EU_wild")) load("~/pCloudDrive/FACE-IT_data/EU_arctic/EU_wild.RData")

# Combine and save
full_product_EU <- rbind(EU_species, EU_wild) |> distinct()
data.table::fwrite(full_product_EU, "~/pCloudDrive/FACE-IT_data/EU_arctic/full_product_EU.csv")
save(full_product_EU, file = "~/pCloudDrive/FACE-IT_data/EU_arctic/full_product_EU.RData")
save(full_product_EU, file = "data/full_data/full_product_EU.RData")
rm(list = grep("EU_",names(.GlobalEnv),value = TRUE)); gc()


## Svalbard ----------------------------------------------------------------

### PG product --------------------------------------------------------------

# There is no PG product for Svalbard
# Rather, all PG products are loaded for each site to get any shared data points


### Species ----------------------------------------------------------------

# Svalbard ivory gull population
sval_ivory_gull_population <- read.csv("~/pCloudDrive//FACE-IT_data/svalbard/the-number-of-breeding-p.csv", sep = ";") %>% # read the csv
  mutate(date_accessed = as.Date("2023-04-12"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/ivory-gull/", 
         citation = "Norwegian Polar Institute (2022). The number of breeding pairs of ivory gulls in Svalbard. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/ismaake.html", 
         lon = NA, lat = NA, depth = NA, 
         Species = "Pagophila eburnea",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp," breeding population [%]"),
         category = "bio",
         driver = "biomass",
         site = "svalbard", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::rename(value = Svalbard) %>% 
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))

# Svalbard walrus population
sval_walrus_population <- read_delim("~/pCloudDrive/FACE-IT_data/svalbard/walrus-population-in-sva.csv") %>% 
  pivot_longer(cols = c(`Walrus estimated numbers`, `Walrus population aerial counts`)) %>% 
  mutate(date_accessed = as.Date("2023-04-13"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/walrus/", 
         citation = "Norwegian Polar Institute (2022). Walrus population in Svalbard. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/walrus-population.html", 
         lon = NA, lat = NA, depth = NA, 
         Species = "Odobenus marinus",
         nomsp = map(Species, latin_eng),
         type = case_when(name == "Walrus estimated numbers"~"estimated",
                          name == "Walrus population aerial counts"~"aerial survey"),
         variable = paste0(nomsp, " ", type, " [n]"),
         category = "bio",
         driver = "biomass",
         site = "svalbard", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))

# Svalbard (north and west) calanus population by size
sval_nw_calanus_mm_population <- read.csv("~/pCloudDrive/FACE-IT_data/svalbard/average-biomass-of-zoopl.csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`X0.18.mm`, `X1.0.mm`, `X2.0.mm`, `Total`)) %>%
  filter(!name == "Total") %>%
  mutate(date_accessed = as.Date("2023-04-13"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/zooplankton-biomass-in-the-barents-sea/",
         citation = "Institute of Marine Research (2023). Average biomass of zooplankton in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/zooplankton-biomass.html",
         lon = NA, lat = NA, depth = NA,
         Species = "Zooplankton",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " ", substr(stri_replace_last(tolower(name)," ", regex = "[.]"), 2, 10)," [g/m²]"),
         category = "bio",
         driver = "biomass",
         site = "svalbard",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))

# Svalbard (north and west) calanus population tot
sval_nw_calanus_tot_population <- read.csv("~/pCloudDrive/FACE-IT_data/svalbard/average-biomass-of-zoopl.csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`X0.18.mm`, `X1.0.mm`, `X2.0.mm`, `Total`)) %>%
  filter(name == "Total") %>%
  mutate(date_accessed = as.Date("2023-04-13"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/zooplankton-biomass-in-the-barents-sea/",
         citation = "Institute of Marine Research (2023). Average biomass of zooplankton in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/zooplankton-biomass.html",
         lon = NA, lat = NA, depth = NA,
         Species = "Zooplankton",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " [g/m²]"),
         category = "bio",
         driver = "biomass",
         site = "svalbard",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))

# Svalbard (south and east) calanus population by size
sval_se_calanus_mm_population <- read.csv("~/pCloudDrive/FACE-IT_data/svalbard/average-biomass-of-zoopl (2).csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`X0.18.mm`, `X1.0.mm`, `X2.0.mm`, `Total`)) %>%
  filter(!name == "Total") %>%
  mutate(date_accessed = as.Date("2023-04-13"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/zooplankton-biomass-in-the-barents-sea/",
         citation = "Institute of Marine Research (2023). Average biomass of zooplankton in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/zooplankton-biomass.html",
         lon = NA, lat = NA, depth = NA,
         Species = "Zooplankton",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " ", substr(stri_replace_last(tolower(name)," ", regex = "[.]"), 2, 10)," [g/m²]"),
         category = "bio",
         driver = "biomass",
         site = "svalbard",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))

# Svalbard (south and east) calanus population tot
sval_se_calanus_tot_population <- read.csv("~/pCloudDrive/FACE-IT_data/svalbard/average-biomass-of-zoopl (2).csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`X0.18.mm`, `X1.0.mm`, `X2.0.mm`, `Total`)) %>%
  filter(name == "Total") %>%
  mutate(date_accessed = as.Date("2023-04-13"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/zooplankton-biomass-in-the-barents-sea/",
         citation = "Institute of Marine Research (2023). Average biomass of zooplankton in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/zooplankton-biomass.html",
         lon = NA, lat = NA, depth = NA,
         Species = "Zooplankton",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " [g/m²]"),
         category = "bio",
         driver = "biomass",
         site = "svalbard",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))

# Svalbard kittiwake population
sval_kittiwake_population <- read.csv("~/pCloudDrive/FACE-IT_data/svalbard/black-legged-kittiwake-p.csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`Fuglehuken`, `Bjørnøya`, `Grumant`, `Sofiekammen`, `Ossian.Sars`, `Tschermakfjellet`, `Alkhornet`, `Amsterdamya`)) %>%
  filter(name == "Bjørnøya") %>% 
  mutate(date_accessed = as.Date("2023-04-13"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/black-legged-kittiwake/",
         citation = "Norwegian Polar Institute (2022). Black-legged kittiwake population size, as percentage of the average in the colony. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/black-legged-kittiwake.html",
         lon = NA, lat = NA, depth = NA,
         Species = "Rissa tridactyla",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " population [% average in the colony]"),
         category = "bio",
         driver = "biomass",
         site = "svalbard",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))

# Svalbard Brünnich’s guillemot population
sval_brguillemot_population <- read.csv("~/pCloudDrive/FACE-IT_data/svalbard/brnnichs-guillemot-breed.csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`Diabas`, `Alkhornet`, `Sofiekammen`, `Grumant`, `Tschermakfjellet`, `Fuglehuken`, `Ossian.Sarsfjellet`, `Bjørnøya..southern.part`, `Bjørnøya..Evjebukta`, `Jan.Mayen`)) %>%
  filter(name == "Fuglehuken") %>% 
  mutate(date_accessed = as.Date("2023-04-14"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/brunnichs-guillemot/",
         citation = "Norwegian Polar Institute (2022). Brünnich’s guillemot breeding populations, percentage of colony average. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/brunnichs-guillemot.html",
         lon = NA, lat = NA, depth = NA,
         Species = "Uria lomvia",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " breeding population [%]"),
         category = "bio",
         driver = "biomass",
         site = "svalbard",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))

# Svalbard hooded seal population
sval_cycr_population <- read.csv("~/pCloudDrive/FACE-IT_data/svalbard/population-size-of-hoode.csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`Modelled.production.of.pups`, `Modelled.total.stock.size`, `Survey.counts.of.pups`)) %>% 
  mutate(date_accessed = as.Date("2023-04-14"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/hooded-seal/",
         citation = "Institute of Marine Research (2022). Population size of hooded seals in the West Ice. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/hooded-seal.html",
         lon = NA, lat = NA, depth = NA,
         Species = "Cystophora cristata",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp," ", str_replace_all(tolower(name),"\\."," ")," [n]"),
         category = "bio",
         driver ="biomass",
         site = "svalbard",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))

# Svalbard (east) harp seal population
sval_cycr_population <- read.csv("~/pCloudDrive/FACE-IT_data/svalbard/population-size-of-hoode.csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`Modelled.production.of.pups`, `Modelled.total.stock.size`, `Survey.counts.of.pups`)) %>% 
  mutate(date_accessed = as.Date("2023-04-14"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/hooded-seal/",
         citation = "Institute of Marine Research (2022). Population size of hooded seals in the West Ice. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/hooded-seal.html",
         lon = NA, lat = NA, depth = NA,
         Species = "Cystophora cristata",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp," ", str_replace_all(tolower(name),"\\."," ")," [n]"),
         category = "bio",
         driver ="biomass",
         site = "svalbard",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))

# Combine and save
sval_species <- rbind(sval_ivory_gull_population, 
                      sval_nw_calanus_mm_population,
                      sval_nw_calanus_tot_population,
                      sval_se_calanus_mm_population,
                      sval_se_calanus_tot_population,
                      sval_walrus_population,
                      sval_brguillemot_population)
save(sval_species, file = "~/pCloudDrive/FACE-IT_data/svalbard/sval_species.RData")
write_csv(sval_species, file = "~/pCloudDrive/FACE-IT_data/svalbard/sval_species.csv")
rm(list = grep("sval_",names(.GlobalEnv),value = TRUE)); gc()


### Wild data ---------------------------------------------------------------


# Tidal glacier fronts
sval_tidal_glacier_front <- data.frame(date_accessed = NA, 
                                       URL = "https://www.gebco.net/", 
                                       citation = "GEBCO Compilation Group (2021). GEBCO 2021 Grid. https://doi.org/10.5285/c6612cbe-50b3-0cff-e053-6c86abc09f8f", 
                                       site = "EU", 
                                       lon = NA, 
                                       lat = NA, 
                                       date = NA, 
                                       depth = NA,
                                       category = "phys", 
                                       driver = "bathymetry", 
                                       variable = "bathymetry")
                                       
                                       type = "Geospatial",
                                       data_name = "Glacier: tidal front",
                                       date_range = "2015 - 2019",
                                       # lon_range = "9 - 36",
                                       # lat_range = "76 - 81",
                                       # depth_range = NA,
                                       # file_name = '<a onclick="alert(\'tidewater/\');">1 folder, 9 files</a>',
                                       URL = '<a target="_blank" rel="noopener noreferrer" href="https://data.npolar.no/dataset/7cd67b1a-1b9b-4dfd-b7a1-f9469597ed4d">NPDC</a>',
                                       reference = '<a onclick="alert(\'Kohler, J., König, M., Nuth, C., & Villaflor, G. (2018). Svalbard tidewater glacier front database [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2018.7cd67b1a\');">Kohler et al. (2018)</a>',
                                       note = NA)

# Marine terminating glacier fronts
sval_marine_glacier_front <- data.frame(type = "Geospatial",
                                        data_name = "Glacier: marine front",
                                        date_range = "2008 - 2020",
                                        # lon_range = "10 - 34",
                                        # lat_range = "76 - 81",
                                        # depth_range = NA,
                                        # file_name = '<a onclick="alert(\'glacier_fronts/\');">1 folder, 10 files</a>',
                                        URL = '<a target="_blank" rel="noopener noreferrer" href="https://data.npolar.no/dataset/d60a919a-9cc8-4048-9686-df81bfdc2338">NPDC</a>',
                                        reference = '<a onclick="alert(\'Moholdt, G., Maton, J., Majerska, M., & Kohler, J. (2021). Annual frontlines of marine-terminating glaciers on Svalbard [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2021.d60a919a\');">Moholdt et al. (2021)</a>',
                                        note = NA)

# Surface meteorology
sval_surface_met <- data.frame(type = "Cruise",
                               data_name = "Air: temperature, pressure; relative humidity; wind: speed, direction",
                               date_range = "2015",
                               # lon_range = "2.9 - 29.9",
                               # lat_range = "78.1 - 83.3",
                               # depth_range = NA,
                               # file_name = '<a onclick="alert(\'N-ICE_metData_v2.nc; N-ICE_metData_QC.py; README_N-ICE_metData_v2.txt\');">3 files</a>',
                               URL = '<a target="_blank" rel="noopener noreferrer" href="https://data.npolar.no/dataset/056a61d1-d089-483a-a256-081de4f3308d">NPDC</a>',
                               reference = '<a onclick="alert(\'Hudson, S. R., Cohen, L., & Walden, V. (2015). N-ICE2015 surface meteorology [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2015.056a61d1\');">Hudson & Walden (2015)</a>', 
                               note = '<a onclick="alert(\'The N-ICE Atmospheric Forcing work package team requests that users of these data: 1) Contact both  Stephen Hudson (Stephen.Hudson@npolar.no) and Lana Cohen (Lana.Cohen@npolar.no) to discuss your specific uses of the data, and 2) Include the requested_acknowledgment in any presentations or publications: The authors acknowledge support from Stephen Hudson and Lana Cohen at the Norwegian Polar Institute and Von P. Walden at Washington State University for use of the N-ICE2015 dataset.\');">Requirements for use</a>')

# Seabird database
sval_seabird_database <- data.frame(type = "Database",
                                    data_name = "Seabird Colonies",
                                    date_range = "1880 - 2020",
                                    # lon_range = "9 - 35",
                                    # lat_range = "74 - 81",
                                    # depth_range = NA,
                                    # file_name = '<a onclick="alert(\'See the website\');">0 files</a>',
                                    URL = '<a target="_blank" rel="noopener noreferrer" href="https://data.npolar.no/dataset/fd4fd3aa-7249-53c9-9846-6e28c5a42587">NPDC</a>',
                                    reference = '<a onclick="alert(\'Strøm, H., Descamps, S., & Bakken, V. . (2008). Seabird Colonies by the Barents Sea, White Sea and Kara Sea [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2008.fd4fd3aa\');">Strøm et al. (2008)</a>',
                                    note = NA)

# Protection of sites
sval_protection <- data.frame(type = "Database",
                              data_name = "Protection of geological sites",
                              date_range = "1993 - 2020",
                              # lon_range = "20 - 80",
                              # lat_range = "45 - 81",
                              # depth_range = NA,
                              # file_name = '<a onclick="alert(\'See the website\');">0 files</a>',
                              URL = '<a target="_blank" rel="noopener noreferrer" href="https://data.npolar.no/dataset/117acc0c-7d6e-5c58-bb80-bb220780f406">NPDC</a>',
                              reference = '<a onclick="alert(\'Blomeier, D. P. G., & Hjelle, A. (2008). Protection of geological sites [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2008.117acc0c\');">Blomeier et al. (2008)</a>',
                              note = NA)

# Fast ice persistence
sval_fast <- data.frame(type = "Geospatial",
                        data_name = "Fast ice: duration",
                        date_range = "2014 - 2016",
                        # lon_range = " 8.7 - 28.3",
                        # lat_range = "76.4 - 80.7",
                        # depth_range = NA,
                        # file_name = '<a onclick="alert(\'svalbard_fastice_persistency_2014.tif; svalbard_fastice_persistency_2015.tif; svalbard_fastice_persistency_2016.tif\');">1 folder, 3 files</a>',
                        URL = '<a target="_blank" rel="noopener noreferrer"href="https://data.npolar.no/dataset/33d631d3-051e-403d-8600-78a30b767ed3">NPDC</a>',
                        reference = '<a onclick="alert(\'Itkin, M. (2017). Svalbard Fjords Fast Ice Persistence [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2017.33d631d3\');">Itkin (2017)</a>',
                        note = NA)

# svalbard ivory gull population
sval_paeb <- data.frame(type = "In situ",
                        data_name = "Biomass: Pagophila eburnea (ivory gull)",
                        date_range = "2009 to 2021",
                        URL = '<a target="_blank" rel="noopener noreferrer" href="https://mosj.no/en/indikator/fauna/marine-fauna/ivory-gull/">MOSJ</a>',
                        reference = '<a onclick="alert(\'Norwegian Polar Institute (2022). The number of breeding pairs of ivory gulls in Svalbard. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/ismaake.html\');">NPI (2022)</a>',
                        note = NA)

# svalbard walrus population
sval_odma <- data.frame(type = "In situ",
                        data_name = "Biomass: Odobenus marinus (walrus)",
                        date_range = "1980 to 2018",
                        URL = '<a target="_blank" rel="noopener noreferrer" href="https://mosj.no/en/indikator/fauna/marine-fauna/walrus/">MOSJ</a>',
                        reference = '<a onclick="alert(\'Norwegian Polar Institute (2022). Walrus population in Svalbard. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/walrus-population.html\');">NPI (2022)</a>',
                        note = NA)

# svalbard calanus population by size (north and west)
sval_nwmmcalanus <- data.frame(type = "In situ",
                               data_name = "Biomass: Calanus species size (north and west)",
                               date_range = "2009 to 2021",
                               URL = '<a target="_blank" rel="noopener noreferrer" href="https://mosj.no/en/indikator/fauna/marine-fauna/zooplankton-biomass-in-the-barents-sea/">MOSJ</a>',
                               reference = '<a onclick="alert(\'Institute of Marine Research (2023). Average biomass of zooplankton in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/zooplankton-biomass.html\');">IMR (2023)</a>',
                               note = NA)

# svalbard calanus population all (north and west)
sval_nwtotcalanus <- data.frame(type = "In situ",
                                data_name = "Biomass: Calanus species (north and west)",
                                date_range = "2009 to 2021",
                                URL = '<a target="_blank" rel="noopener noreferrer" href="https://mosj.no/en/indikator/fauna/marine-fauna/zooplankton-biomass-in-the-barents-sea/">MOSJ</a>',
                                reference = '<a onclick="alert(\'Institute of Marine Research (2023). Average biomass of zooplankton in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/zooplankton-biomass.html\');">IMR (2023)</a>',
                                note = NA)

# svalbard calanus population by size (south and east)
sval_semmcalanus <- data.frame(type = "In situ",
                               data_name = "Biomass: Calanus species size (south and east)",
                               date_range = "1997 to 2021",
                               URL = '<a target="_blank" rel="noopener noreferrer" href="https://mosj.no/en/indikator/fauna/marine-fauna/zooplankton-biomass-in-the-barents-sea/">MOSJ</a>',
                               reference = '<a onclick="alert(\'Institute of Marine Research (2023). Average biomass of zooplankton in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/zooplankton-biomass.html\');">IMR (2023)</a>',
                               note = NA)

# svalbard calanus population all (south and east)
sval_setotcalanus <- data.frame(type = "In situ",
                                data_name = "Biomass: Calanus species (south and east)",
                                date_range = "1997 to 2021",
                                URL = '<a target="_blank" rel="noopener noreferrer" href="https://mosj.no/en/indikator/fauna/marine-fauna/zooplankton-biomass-in-the-barents-sea/">MOSJ</a>',
                                reference = '<a onclick="alert(\'Institute of Marine Research (2023). Average biomass of zooplankton in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/zooplankton-biomass.html\');">IMR (2023)</a>',
                                note = NA)

# svalbard kittiwake population
sval_ritr <- data.frame(type = "In situ",
                        data_name = "Biomass: Rissa tridactyla (black-legged kittiwake)",
                        date_range = "1988 to 2021",
                        URL = '<a target="_blank" rel="noopener noreferrer" href="https://mosj.no/en/indikator/fauna/marine-fauna/black-legged-kittiwake//">MOSJ</a>',
                        reference = '<a onclick="alert(\'Norwegian Polar Institute (2022). Black-legged kittiwake population size, as percentage of the average in the colony. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/black-legged-kittiwake.html\');">NPI (2022)</a>',
                        note = NA)

# svalbard brünnich’s guillemot population
sval_urlo <- data.frame(type = "In situ",
                        data_name = "Biomass: Uria lomvia (brünnich’s guillemot)",
                        date_range = "1988 to 2021",
                        URL = '<a target="_blank" rel="noopener noreferrer" href="https://mosj.no/en/indikator/fauna/marine-fauna/brunnichs-guillemot/">MOSJ</a>',
                        reference = '<a onclick="alert(\'Norwegian Polar Institute (2022). Brünnich’s guillemot breeding populations, percentage of colony average. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/brunnichs-guillemot.html\');">NPI (2022)</a>',
                        note = NA)


# Glacier Area Outlines
# https://data.npolar.no/dataset/89f430f8-862f-11e2-8036-005056ad0004

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
                                                y = sval_Nature_glacier_mass_base$Y_center, epsg1 = "epsg:32633") |> distinct()
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
         # Here we take the end date as the date value for each datum
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
# NB: I was creating daily means from these data, but they are underway and so there is only one value per lon/lat combo
sval_NICE <- tidync("~/pCloudDrive/FACE-IT_data/svalbard/N-ICE_metData_v2.nc") |> hyper_tibble() |>  
  dplyr::rename(lon = longitude, lat = latitude) |> 
  unite(year, month, day, sep = "-", remove = TRUE, col = date) |>  
  mutate(date = as.Date(date)) |> 
  dplyr::select(lon, lat, date, everything(),
                -second, -minute, -hour, -time, -unix_time,
                -wind_speed_10m_flag, -wind_from_direction_10m_flag, # _flag: don't need to know about wind data interpolation status
                -air_temperature_fill, -air_pressure_at_sea_level_fill) |> # _fill: data seem problematic
  pivot_longer(wind_from_direction_4m:relative_humidity_4m, names_to = "variable") |>  
  filter(!is.na(value)) |>
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
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop"); gc()
rm(sval_UNIS_nc_dat, sval_UNIS_TEMP, sval_UNIS_PSAL, sval_UNIS_CNDC); gc()

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
         depth = NA,
         lon = NA, lat = NA, 
         date_accessed = as.Date("2020-09-30"),
         variable = paste0(Area," [",var,"]"),
         category = case_when(grepl("co2|nox|sox", variable, ignore.case = T) ~ "chem",
                              grepl("PM", variable, ignore.case = T) ~ "phys", TRUE ~ "soc"),
         URL = "Received directly from Morten Simonsen",
         citation = "Simonsen, M., Walnum, H. J., & Gössling, S. (2018). Model for estimation of fuel consumption of cruise ships. Energies, 11(5), 1059.") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)

# Combine and save
sval_wild <- rbind(sval_MOSJ_glacier_mass, sval_Nature_glacier_mass, 
                   sval_tidewater_ablation, sval_NICE, sval_UNIS_database, sval_biogeochemistry,
                   sval_pop, sval_tour_arrival, sval_guest_night, sval_AIS) |> 
  mutate(site = "sval") |> distinct() |> 
  left_join(full_var_list, by = c("category", "variable")) |> 
  dplyr::select(date_accessed, URL, citation, site, lon, lat, date, depth, category, driver, variable, value)
data.table::fwrite(sval_wild, "~/pCloudDrive/FACE-IT_data/svalbard/sval_wild.csv")
save(sval_wild, file = "~/pCloudDrive/FACE-IT_data/svalbard/sval_wild.RData")
rm(list = grep("sval_",names(.GlobalEnv),value = TRUE)); gc()

# See EU example if only one file is being added


### Full product ------------------------------------------------------------

# Load full EU file
if(!exists("full_product_EU")) load("data/full_data/full_product_EU.RData")

# Subset to Svalbard bbox and references for data missing lon/lat coords
sval_EU_sub <- filter_site_plural("sval", full_product_EU)

# Load PANGAEA
## NB: There is intentionally no PANGAEA file

# Load species
if(!exists("sval_species")) load("~/pCloudDrive/FACE-IT_data/svalbard/sval_species.RData")

# Load wild data
if(!exists("sval_wild")) load("~/pCloudDrive/FACE-IT_data/svalbard/sval_wild.RData")

# Combine and save
full_product_sval <- rbind(sval_EU_sub, sval_species, sval_wild) |> mutate(site = "sval") |> distinct()
data.table::fwrite(full_product_sval, "~/pCloudDrive/FACE-IT_data/svalbard/full_product_sval.csv")
save(full_product_sval, file = "~/pCloudDrive/FACE-IT_data/svalbard/full_product_sval.RData")
save(full_product_sval, file = "data/full_data/full_product_sval.RData")
rm(list = grep("sval_",names(.GlobalEnv),value = TRUE)); gc()


## Kongsfjorden ------------------------------------------------------------

### PG product --------------------------------------------------------------

# Load pg kong files
system.time(
  pg_kong_sub <- plyr::ldply(pg_files, pg_site_filter, site_name = "kong")
) # 64 seconds

# Test problem files
# pg_test <- pg_data(doi = "10.1594/PANGAEA.868371")
# pg_test <- pg_data(doi = "10.1594/PANGAEA.896828")
# pg_test <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.896828")
# pg_test <- pg_kong_sub %>% 
#   filter(URL == "https://doi.org/10.1594/PANGAEA.868371") %>% 
#   dplyr::select(contains("depth"), everything()) %>% 
#   # mutate_all(~na_if(., '')) %>% 
#   janitor::remove_empty("cols")
# pg_kong <- read_csv("data/pg_data/pg_kong.csv")
# pg_test <- filter(pg_kong, URL == "https://doi.pangaea.de/10.1594/PANGAEA.896828")

# More testing
# colnames(pg_kong_sub)
# test1 <- pg_kong_sub %>% 
#   dplyr::select(URL, `T air (1) [°C]`) %>% 
#   na.omit()
  
# Process Kongsfjorden PANGAEA data
pg_kong_clean <- pg_kong_sub |> 
  # dplyr::select(contains(c("Qz")), everything()) |> # Look at specific problem columns
  # dplyr::select(contains(c("date", "lon", "lat")), everything()) |> # Look at date columns
  # dplyr::select(contains(c("depth", "press", "elev")), everything()) |> # Look at depth columns
  # Remove empty columns
  mutate_if(is.character, ~na_if(., '')) |>
  janitor::remove_empty("cols") |>
  # Manually remove problematic files - no need
  # Manually remove problematic columns - no need
  # Manage lon/lat columns - no need
  # Manage date column
  dplyr::rename(date = `Date/Time`) |> 
  mutate(date = ifelse(date == "", NA, date),
         date = case_when(is.na(date) & !is.na(Date) ~ as.character(Date),
                          is.na(date) & !is.na(`Date/time start`) ~ as.character(`Date/time start`),
                          is.na(date) & !is.na(`Date/time end`) ~ as.character(`Date/time end`),
                          TRUE ~ date)) |> 
  mutate(date = as.Date(gsub("T.*", "", date))) |> 
  # Manage depth column
  # TODO: Rather have depth columns filled in by progressively more distant choices from a pre-made list of depth column names
  dplyr::rename(`Depth [m]1` = `Depth water [m] (water depth from ETOPO1, if >...)`) |> 
  mutate(depth = case_when(!is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Depth [m]`) ~ as.numeric(`Depth [m]`),
                           !is.na(`Depth [m]1`) ~ as.numeric(`Depth [m]1`),
                           !is.na(`Depth water [m] (top)`) ~ as.numeric(`Depth water [m] (top)`),
                           !is.na(`Depth [m] (negative = above surface)`) ~ as.numeric(`Depth [m] (negative = above surface)`))) |> 
  mutate(depth = case_when(is.na(depth) & !is.na(`Press [dbar]`) ~ as.numeric(`Press [dbar]`),
                           is.na(depth) & !is.na(`Elevation [m]`) ~ -as.numeric(`Elevation [m]`),
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -as.numeric(`Elevation [m a.s.l.]`),
                           TRUE ~ depth)) |> 
  # dplyr::select(depth, everything())
  # Remove unwanted columns
  # dplyr::select(-contains(c("Elev ", "Elevation "))) |>
  # Finish up
  left_join(pg_meta_files, by = c("meta_idx", "site")) |> 
  dplyr::select(date_accessed, URL, citation, site, lon, lat, date, depth, everything()) |> 
  dplyr::rename(`Sal [PSU]` = `Sal ([PSU])`) |> 
  # NB: This must be changed manually when new data are loaded
  mutate(across(!dplyr::all_of(clean_cols), as.numeric)) |>  
  janitor::remove_empty("cols") |> 
  # NB: Site exists earlier to reflect data from different files for metadata joining
  mutate(site = "kong")
# colnames(pg_kong_clean)

# Individual category data.frames
pg_kong_cryo <- pg_var_melt(pg_kong_clean, query_cryo)
pg_kong_phys <- pg_var_melt(pg_kong_clean, query_phys)
pg_kong_chem <- pg_var_melt(pg_kong_clean, query_chem)
pg_kong_bio <- pg_var_melt(pg_kong_clean, query_bio)
pg_kong_soc <- pg_var_melt(pg_kong_clean, query_soc) # 0 values

# Stack them together
pg_kong_ALL <- rbind(pg_kong_cryo, pg_kong_phys, pg_kong_chem, pg_kong_bio, pg_kong_soc)
data.table::fwrite(pg_kong_ALL, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_ALL.csv")
save(pg_kong_ALL, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_ALL.RData")

# Check that all columns were used
# TODO: PAR and some other variables are being missed here
colnames(pg_kong_clean)[!gsub("\\] \\(.*", "\\]", colnames(pg_kong_clean)) %in% unique(pg_kong_ALL$variable)]

# Clean up
rm(list = grep("pg_kong",names(.GlobalEnv),value = TRUE)); gc()

# Load data to investigate
# pg_kong_ALL <- data.table::fread("~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_ALL.csv")


### Species ----------------------------------------------------------------

# kong glaucous gull population
kong_glaucous_gull_population <- read_delim("~/pCloudDrive/FACE-IT_data/kongsfjorden/glaucous-gull-population.csv") %>% 
  mutate(date_accessed = as.Date("2023-04-11"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/glaucous-gull/", 
         citation = "Norwegian Polar Institute (2022). Glaucous gull population, as percentage of the average in the colony. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/glaucous-gull.html", 
         lon = NA, lat = NA, depth = NA, 
         Species = "Larus hyperboreus",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " breeding population [%]"),
         category = "bio",
         driver ="biomass",
         site = "kong", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::rename(value = Kongsfjorden) %>% 
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))

# kong eiders
kong_eiders_stock <- read.csv("~/pCloudDrive/FACE-IT_data/kongsfjorden/breeding-population-of-c.csv", sep = ";") %>% 
  mutate(date_accessed = as.Date("2023-04-12"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/common-eider/", 
         citation = "Norwegian Polar Institute (2022). Breeding population of common eiders in Kongsfjorden, number of breeding pairs. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/common-eider.html", 
         lon = NA, lat = NA, depth = NA, 
         Species = "Somateria mollissima borealis",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " breeding pairs [n]"),
         category = "bio",
         driver ="biomass",
         site = "kong", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::rename(value = Common.eider) %>% 
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))

# kong seabird
kong_seabird <- read.csv("~/pCloudDrive/FACE-IT_data/kongsfjorden/Descamps_Strom_Ecology_data.csv", sep = ",", skip = 3, header = TRUE) %>%
  janitor::remove_empty(which = "cols") %>% 
  filter(Colony == "Kongsfjorden") %>% 
  mutate(date_accessed = as.Date("2023-04-12"), 
         URL = "https://data.npolar.no/dataset/0ea572cd-1e4c-47a3-b2a5-5d7cc75aaeb4", 
         citation = "Descamps, S., & Strøm, H. (2021). Seabird monitoring data from Svalbard, 2009-2018 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2021.0ea572cd", 
         lon = NA, lat = NA, depth = NA, 
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " colony count [n]"),
         category = "bio",
         driver ="biomass",
         site = "kong", 
         date = as.Date(paste0(YR,"-12-31"))) %>% 
  dplyr::rename(value = Count) %>% 
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))

# kong calanus population
kong_calanus_population <- read.csv("~/pCloudDrive/FACE-IT_data/kongsfjorden/calanus-species-composit.csv", sep = ";", dec = ",") %>% 
  pivot_longer(cols = c(`Proportion.of.Atlantic.species`, `Proportion.of.Arctic.species`)) %>% 
  mutate(date_accessed = as.Date("2023-04-13"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/zooplankton-species-composition-in-kongsfjorden/", 
         citation = "Norwegian Polar Institute (2022). Calanus species composition in Kongsfjorden. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/zooplankton-species-composition.html", 
         lon = NA, lat = NA, depth = NA, 
         Species = substr(str_replace_all(tolower(name),"\\."," "),15, 30),
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " [%]"),
         category = "bio",
         driver ="biomass",
         site = "kong", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))

# kong  kittiwake population
kong_kittiwakke_population <- read.csv("~/pCloudDrive/FACE-IT_data/svalbard/black-legged-kittiwake-p.csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`Fuglehuken`, `Bjørnøya`, `Grumant`, `Sofiekammen`, `Ossian.Sars`, `Tschermakfjellet`, `Alkhornet`, `Amsterdamya`)) %>%
  filter(name == "Ossian.Sars") %>% 
  mutate(date_accessed = as.Date("2023-04-13"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/black-legged-kittiwake/",
         citation = "Norwegian Polar Institute (2022). Black-legged kittiwake population size, as percentage of the average in the colony. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/black-legged-kittiwake.html",
         lon = NA, lat = NA, depth = NA,
         Species = "Rissa tridactyla",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " population [% average in the colony]"),
         category = "bio",
         driver ="biomass",
         site = "kong",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))


# kong Brünnich’s guillemot population
kong_brguillemot_population <- read.csv("~/pCloudDrive/FACE-IT_data/svalbard/brnnichs-guillemot-breed.csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`Diabas`, `Alkhornet`, `Sofiekammen`, `Grumant`, `Tschermakfjellet`, `Fuglehuken`, `Ossian.Sarsfjellet`, `Bjørnøya..southern.part`, `Bjørnøya..Evjebukta`, `Jan.Mayen`)) %>%
  filter(name == "Ossian.Sarsfjellet") %>% 
  mutate(date_accessed = as.Date("2023-04-14"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/brunnichs-guillemot/",
         citation = "Norwegian Polar Institute (2022). Brünnich’s guillemot breeding populations, percentage of colony average. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/brunnichs-guillemot.html",
         lon = NA, lat = NA, depth = NA,
         Species = "Uria lomvia",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " breeding population [%]"),
         category = "bio",
         driver ="biomass",
         site = "kong",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))

# Combine and save
kong_species <- rbind(kong_glaucous_gull_population, 
                      kong_eiders_stock,
                      kong_seabird, 
                      kong_calanus_population,
                      kong_kittiwakke_population,
                      kong_brguillemot_population)
save(kong_species, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/kong_species.RData")
write_csv(kong_species, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/kong_species.csv")
rm(list = grep("kong_",names(.GlobalEnv),value = TRUE)); gc()


### Wild data ---------------------------------------------------------------

# Sea ice cover shape files
kong_sea_ice_shp <- data.frame(type = "Survey",
                               data_name = "Sea ice: cover",
                               date_range = "2003 - 2021",
                               # lon_range = NA,
                               # lat_range = NA,
                               # depth_range = "surface",
                               # file_name = '<a onclick="alert(\'Kongsfjorden_sea_ice_cover_data.csv\');">1 file</a>',
                               URL = '<a target="_blank" rel="noopener noreferrer" href="https://data.npolar.no/dataset/d6d31f5b-8413-42b4-9736-db88d55816dc">NPDC</a>',
                               reference = '<a onclick="alert(\'Gerland, S., Pavlova, O., Marnela, M., Divine, D., Kohler, J., Renner, A. H., & Skoglund, A. (2022). Sea ice extent variability in Kongsfjorden, Svalbard during 2003-2021, based on visual observations from the mountain Zeppelinfjellet. [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2022.d6d31f5b\');">Gerland et al. (2022)</a>',
                               note = NA)

# Glacial topography + thickness
kong_glacier_info <- data.frame(type = "",
                                data_name = "Glacier: surface, thickness, elevation",
                                date_range = "2004 - 2016",
                                # lon_range = "10 - 14", 
                                # lat_range = "78.8 - 79.2",
                                # depth_range = NA,
                                # file_name = '<a onclick="alert(\'TIGRIF_DEM_ice_surface_150m_v1.tif; TIGRIF_DEM_ice_thickness_150m_1.tif; TIGRIF_DEM_subglacial_elevation_150m_v1.tif; TIGRIF_radarprofiles_2004_2016_v1.txt\');">4 files</a>',
                                URL = '<a target="_blank" rel="noopener noreferrer" href="https://data.npolar.no/dataset/702ca4a7-7d02-462c-8cbd-2d80d0e977a1">NPDC</a>',
                                reference = '<a onclick="alert(\'Lindbäck, K., Kohler, J., Pettersson, R., Nuth, C., Langley, K., Messerli, A., … Brandt, O. (2018). Subglacial topography, ice thickness, and bathymetry of Kongsfjorden, northwestern Svalbard [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2017.702ca4a7\');">Lindbäck et al. (2018)</a>',
                                note = NA)

# Ferry box data
kong_ferry <- data.frame(type = "Mooring",
                         data_name = "Salinity; Seawater temperature: surface, mid, bottom; Total alkalinity; pCO2; pH ",
                         date_range = "2015 - 2021",
                         # lon_range = "11.92", 
                         # lat_range = "78.93",
                         # depth_range = "0 - 12",
                         # file_name = '<a onclick="alert(\'kong_ferry.rds\');">1 file</a>',
                         URL = '<a onclick="alert(\'https://doi.pangaea.de/10.1594/PANGAEA.960131\');">NA</a>',
                         reference = '<a onclick="alert(\'Gattuso, Jean-Pierre; Alliouane, Samir; Fischer, Philipp (2023): High-frequency, year-round time series of the carbonate chemistry in a high-Arctic fjord (Svalbard) v2. PANGAEA, https://doi.org/10.1594/PANGAEA.960131\');">Gattuso et al. (2023)</a>',
                         note = NA)

# MOSJ cruise data
kong_MOSJ <- data.frame(type = "Cruise",
                        data_name = "PAR",
                        date_range = NA,
                        # lon_range = "?", lat_range = "?", depth_range = "?",
                        # file_name = '<a onclick="alert(\'\');">0 files</a>',
                        URL = '<a onclick="alert(\'\');">NA</a>',
                        reference = '<a onclick="alert(\'\');">NA</a>',
                        note = "MOSJ cruise data were not able to be found.")

# Collection of PAR data published in Pavlov et al. 2019
kong_pavlov <- data.frame(type = "In situ",
                          data_name = "PAR",
                          date_range = NA,
                          # lon_range = "?", lat_range = "?", depth_range = "?",
                          # file_name = '<a onclick="alert(\'\');">0 files</a>',
                          URL = '<a onclick="alert(\'\');">NA</a>',
                          reference = '<a onclick="alert(\'\');">NA</a>',
                          note = "Pavlov et al. 2019 data are not publicly available.")

# PAR data collected as part of Kai's work in Kongsfjorden
kong_bischoff <- data.frame(type = "In situ",
                            data_name = "PAR",
                            date_range = "2021",
                            # lon_range = "?", lat_range = "?", depth_range = "?",
                            # file_name = '<a onclick="alert(\'\');">0 files</a>',
                            URL = '<a onclick="alert(\'\');">NA</a>',
                            reference = '<a onclick="alert(\'\');">NA</a>',
                            note = "Field data collected by Kai's team may be available in 2022.")

# PAR data collected as part of a 2012/13 experiment by Inka
## NB: Currently being processed into PANGAEA
kong_bartsch <- data.frame(type = "In situ",
                           data_name = "PAR",
                           date_range = "2012 - 2013", 
                           # lon_range = "?", lat_range = "?", depth_range = "?",
                           # file_name = '<a onclick="alert(\'\');">0 files</a>',
                           URL = '<a target="_blank" rel="noopener noreferrer" href="https://doi.pangaea.de/10.1594/PANGAEA.945341">PANGAEA</a>',
                           reference = '<a onclick="alert(\'Bartsch, I., Paar, M., Fredriksen, S., Schwanitz, M., Daniel, C., Hop, H., & Wiencke, C. (2016). Changes in kelp forest biomass and depth distribution in Kongsfjorden, Svalbard, between 1996–1998 and 2012–2014 reflect Arctic warming. Polar Biology, 39(11), 2021-2036.\');">Bartsch et al. (2016)</a>',
                           note = NA)


# PAR data from Dieter Hanelt
kong_hanelt <- data.frame(type = "In situ",
                          data_name = "PAR",
                          date_range = "2012",
                          # lon_range = "?", lat_range = "?", depth_range = "?",
                          # file_name = '<a onclick="alert(\'\');">0 files</a>',
                          URL = '<a onclick="alert(\'Received directly from Dieter Hanelt\');">NA</a>',
                          reference = '<a onclick="alert(\'Pavlov, A. K., Leu, E., Hanelt, D., Bartsch, I., Karsten, U., Hudson, S. R., ... & Granskog, M. A. (2019). The underwater light climate in Kongsfjorden and its ecological implications. In The ecosystem of Kongsfjorden, Svalbard (pp. 137-170). Springer, Cham.\');">Pavlov et al. (2019)</a>',
                          note = NA)

# Cruise data sampling water along the marine terminating edge of glaciers
kong_TWICE <- data.frame(type = "Cruise",
                         data_name = "Salinity, glacier: terminating edge",
                         date_range = NA,
                         # lon_range = "?", lat_range = "?", depth_range = "?",
                         # file_name = '<a onclick="alert(\'\');">0 files</a>',
                         URL = '<a onclick="alert(\'\');">NA</a>',
                         reference = '<a onclick="alert(\'\');">NA</a>',
                         note = "TWICE cruise data not able to be sourced.")

# Macroalgae data from multiple surveys/studies
kong_macroalgae <- data.frame(type = "Survey",
                              data_name = "Macroalgae: presence, abundance; primary production",
                              date_range = "1996/98 and 2012-14",
                              # lon_range = "?", lat_range = "?", depth_range = "?",
                              # file_name = '<a onclick="alert(\'\');">0 files</a>',
                              URL = '<a onclick="alert(\'\');">NA</a>',
                              reference = '<a onclick="alert(\'\');">NA</a>',
                              note = "Data may be available in 2023.")

# Macroalgae data from multiple surveys/studies
kong_benthic <- data.frame(type = "Survey",
                           data_name = "Benthic invertebrates: presence, abundance, biomass",
                           date_range = "1996/98 and 2012-14", 
                           # lon_range = "?", lat_range = "?", depth_range = "?",
                           # file_name = '<a onclick="alert(\'\');">0 files</a>',
                           URL = '<a onclick="alert(\'\');">NA</a>',
                           reference = '<a onclick="alert(\'\');">NA</a>',
                           note = "Data may be available in 2023.")

# kong glaucous gull population
kong_lahy <- data.frame(type = "In situ",
                        data_name = "Biomass: Larus hyperboreus (glaucous gull)",
                        date_range = "2005 to 2021",
                        URL = '<a target="_blank" rel="noopener noreferrer" href="https://mosj.no/en/indikator/fauna/marine-fauna/glaucous-gull/">MOSJ</a>',
                        reference = '<a onclick="alert(\'Norwegian Polar Institute (2022). Glaucous gull population, as percentage of the average in the colony. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/glaucous-gull.html\');">NPI (2022)</a>',
                        note = NA)

# kong common eiders population
kong_somobo <- data.frame(type = "In situ",
                          data_name = "Biomass: Somateria mollissima borealis (common eider)",
                          date_range = "1981 to 2022",
                          URL = '<a target="_blank" rel="noopener noreferrer" href="https://mosj.no/en/indikator/fauna/marine-fauna/common-eider/">MOSJ</a>',
                          reference = '<a onclick="alert(\'Norwegian Polar Institute (2022). Breeding population of common eiders in Kongsfjorden, number of breeding pairs. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/common-eider.html\');">NPI (2022)</a>',
                          note = NA)

# kong seabird population
kong_seabirds <- data.frame(type = "In situ",
                            data_name = "Biomass: seabirds",
                            date_range = "2011 to 2018",
                            URL = '<a target="_blank" rel="noopener noreferrer" href="https://data.npolar.no/dataset/0ea572cd-1e4c-47a3-b2a5-5d7cc75aaeb4">NPDC</a>',
                            reference = '<a onclick="alert(\'Descamps, S., & Strøm, H. (2021). Seabird monitoring data from Svalbard, 2009-2018 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2021.0ea572cd\');">Descamps and Strøm (2021)</a>',
                            note = NA)

# kong kittiwake population
kong_ritr <- data.frame(type = "In situ",
                        data_name = "Biomass: Rissa tridactyla (black-legged kittiwake)",
                        date_range = "1988 to 2021",
                        URL = '<a target="_blank" rel="noopener noreferrer" href="https://mosj.no/en/indikator/fauna/marine-fauna/black-legged-kittiwake//">MOSJ</a>',
                        reference = '<a onclick="alert(\'Norwegian Polar Institute (2022). Black-legged kittiwake population size, as percentage of the average in the colony. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/black-legged-kittiwake.html\');">NPI (2022)</a>',
                        note = NA)

# kong brünnich’s guillemot population
kong_urlo <- data.frame(type = "In situ",
                        data_name = "Biomass: Uria lomvia (brünnich’s guillemot)",
                        date_range = "1988 to 2021",
                        URL = '<a target="_blank" rel="noopener noreferrer" href="https://mosj.no/en/indikator/fauna/marine-fauna/brunnichs-guillemot/">MOSJ</a>',
                        reference = '<a onclick="alert(\'Norwegian Polar Institute (2022). Brünnich’s guillemot breeding populations, percentage of colony average. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/brunnichs-guillemot.html\');">NPI (2022)</a>',
                        note = NA)

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
         depth = (from+to)/2,
         variable = paste0(variable, " [ind/m3]"),
         category = "bio",
         date_accessed = as.Date("2021-02-11"),
         URL = "https://data.npolar.no/dataset/94b29b16-b03b-47d7-bfbc-1c3c4f7060d2",
         citation = "Hop H, Wold A, Vihtakari M, Daase M, Kwasniewski S, Gluchowska M, Lischka S, Buchholz F, Falk-Petersen S (2019) Zooplankton in Kongsfjorden (1996-2016) in relation to climate change. In: The ecosystem of Kongsfjorden, Svalbard (eds. Hop H, Wiencke C), Advances in Polar Ecology, Springer Verlag.") %>% 
  filter(!is.na(value)) %>%
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
  # NB: Not necessary to summarise()
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value)
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
# NB: This is now on PANGAEA, and has been updated in the meta-database
# https://doi.pangaea.de/10.1594/PANGAEA.960131

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

# Combine and save
kong_wild <- rbind(kong_sea_ice_inner, kong_zoo_data, kong_protist_nutrient_chla,
                   kong_CTD_database, kong_CTD_CO2, kong_weather_station, kong_mooring_GFI,
                   kong_mooring_SAMS, kong_ship_arrivals, kong_CTD_DATEN4, kong_LICHT,
                   kong_light_Laeseke, kong_PAR_Dieter) |> mutate(site = "kong") |> distinct() |> 
  left_join(full_var_list, by = c("category", "variable")) |> 
  dplyr::select(date_accessed, URL, citation, site, lon, lat, date, depth, category, driver, variable, value)
data.table::fwrite(kong_wild, "~/pCloudDrive/FACE-IT_data/kongsfjorden/kong_wild.csv")
save(kong_wild, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/kong_wild.RData")
rm(list = grep("kong_",names(.GlobalEnv),value = TRUE)); gc()

# See EU example if only one file is being added


### Full product ------------------------------------------------------------

# Load full Svalbard file
## NB: This contains the full EU Arctic data
if(!exists("full_product_sval")) load("data/full_data/full_product_sval.RData")

# Subset to Kongsfjorden bbox and references for data missing lon/lat coords
kong_sval_sub <- filter_site_plural("kong")

# Load PG file
if(!exists("pg_kong_ALL")) load("~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_ALL.RData")

# Load species
if(!exists("kong_species")) load("~/pCloudDrive/FACE-IT_data/kongsfjorden/kong_species.RData")

# Load wild data
if(!exists("kong_wild")) load("~/pCloudDrive/FACE-IT_data/kongsfjorden/kong_wild.RData")


# Combine and save
full_product_kong <- rbind(kong_sval_sub, pg_kong_ALL, kong_species, kong_wild) |> mutate(site = "kong") |> distinct()
data.table::fwrite(full_product_kong, "~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.csv")
save(full_product_kong, file = "~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")
save(full_product_kong, file = "data/full_data/full_product_kong.RData")
save_data(full_product_kong)
rm(list = grep("kong_",names(.GlobalEnv),value = TRUE)); gc()

# Search product for specific authors
# if(!exists("full_product_kong")) load("~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")

# Simple checks
# full_product_kong %>% filter(grepl("Jentzsch", citation))


## Isfjorden ---------------------------------------------------------------

### PG product --------------------------------------------------------------

# Load pg is files
# TODO: There is a lot of room for improvement in this process
system.time(
  pg_is_sub <- plyr::ldply(pg_files, pg_site_filter, site_name = "is")
) # 421 seconds - RAM limited
gc()

# Test problem files
# pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
# pg_test <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.925759")
# pg_test <- pg_test_dl("10.1594/PANGAEA.909130")
# pg_test <- pg_test_dl(pg_doi = "10.1594/PANGAEA.950472")

# Remove unneeded columns
pg_is_clean <- pg_is_sub |> 
  # dplyr::select(contains(c("date", "lon", "lat")), everything()) |> # Look at date columns
  # Remove empty columns - NB: Due to the size of the dataframe this is not possible...
  # mutate_if(is.character, ~na_if(., '')) |>
  # janitor::remove_empty("cols") |>
  # Manually remove problematic files - no need
  # Manually remove problematic columns - no need
  # Manage lon/lat columns - no need
  # Manage date column
  dplyr::rename(date = `Date/Time`) |>
  mutate(date = ifelse(date == "", NA, date),
         date = case_when(is.na(date) & !is.na(Date) ~ as.character(Date),
                          is.na(date) & !is.na(`Date/time start`) ~ as.character(`Date/time start`),
                          is.na(date) & !is.na(`Date/time end`) ~ as.character(`Date/time end`),
                          is.na(date) & !is.na(`Sampling date`) ~ as.character(`Sampling date`),
                          TRUE ~ date),
         date = ifelse(date == "", NA, date)) |>
  mutate(date = case_when(nchar(date) == 4 ~ paste0(date,"-01-01"),
                          nchar(date) == 7 ~ paste0(date,"-01-01"),
                          TRUE ~ date)) |>
  mutate(date = as.Date(gsub("T.*", "", date))); gc()

# Manage depth column
pg_is_clean <- pg_is_clean |> 
  # dplyr::select(contains(c("depth", "press", "elev")), everything()) |> # Look at specific columns
  dplyr::rename(`Depth [m]1` = `Depth water [m] (water depth from ETOPO1, if >...)`,
                `Depth [m]2` = `Depth water [m] (corresponds to CTD event; mea...)`,
                `Elevation [m a.s.l.]1` = `Elevation [m a.s.l.] (ELEVATION of CTD event, CTD/R...)`) |>
  mutate(depth = case_when(!is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Depth [m]`) ~ as.numeric(`Depth [m]`),
                           !is.na(`Depth [m]1`) ~ as.numeric(`Depth [m]1`),
                           !is.na(`Depth [m]2`) ~ as.numeric(`Depth [m]2`))) |>
  mutate(depth = case_when(is.na(depth) & !is.na(`Depth top [m]`) ~ as.numeric(`Depth top [m]`),
                           is.na(depth) & !is.na(`Press [dbar]`) ~ as.numeric(`Press [dbar]`),
                           is.na(depth) & !is.na(`Elevation [m]`) ~ -as.numeric(`Elevation [m]`),
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -as.numeric(`Elevation [m a.s.l.]`),
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]1`) ~ -as.numeric(`Elevation [m a.s.l.]1`),
                           TRUE ~ depth)); gc()
# Finish up
pg_is_clean <- pg_is_clean |> 
  dplyr::select(-"Depth [m]1", -"Depth [m]2", -"Elevation [m a.s.l.]1",
                -"Depth water [m]", -"Depth [m]", -"Depth [m]1", -"Depth [m]2",
                -"Depth top [m]", -"Press [dbar]", -"Elevation [m]", -"Elevation [m a.s.l.]",
                -contains(c("MAGT", "MAAT", # Ground temperatures
                            "RelWindDir", "RelWindSp", "RelWindGust"))) |> # Wind values
  left_join(pg_meta_files, by = c("meta_idx", "site")) |> 
  dplyr::filter(!URL %in% c("https://doi.org/10.1594/PANGAEA.56770")) |> # Firn line elevation data
  dplyr::select(date_accessed, URL, citation, site, lon, lat, date, depth, everything()) |> 
  mutate(across(!dplyr::all_of(clean_cols), as.numeric)) |>  
  # janitor::remove_empty("cols") |> # File is too large for this
  # NB: Site exists earlier to reflect data from different files for metadata joining
  mutate(site = "is"); gc()
rm(pg_is_sub); gc()

# NB: This is a bad idea. Rather make the data cleaner before getting to this step.
# system.time(
# save(pg_is_clean, file = "data/restricted/pg_is_clean.RData")
# ) # xxx seconds
# system.time(
# load("data/restricted/pg_is_clean.RData")
# ) # xxx seconds
# colnames(pg_is_clean)

# Individual category data.frames
pg_is_cryo <- pg_var_melt(pg_is_clean, query_cryo); gc()
pg_is_phys <- pg_var_melt(pg_is_clean, query_phys); gc()
pg_is_chem <- pg_var_melt(pg_is_clean, query_chem); gc()
pg_is_bio <- pg_var_melt(pg_is_clean, query_bio); gc()
pg_is_soc <- pg_var_melt(pg_is_clean, query_soc); gc() # empty

# Clean for saving
rm(pg_is_clean); gc()

# Stack them together
pg_is_ALL <- rbind(pg_is_cryo, pg_is_phys, pg_is_chem, pg_is_bio, pg_is_soc)
data.table::fwrite(pg_is_ALL, "~/pCloudDrive/FACE-IT_data/isfjorden/pg_is_ALL.csv")
save(pg_is_ALL, file = "~/pCloudDrive/FACE-IT_data/isfjorden/pg_is_ALL.RData")

# Check that all columns were used
# TODO: Follow up on some of these
colnames(pg_is_clean)[!gsub("\\] \\(.*", "\\]", colnames(pg_is_clean)) %in% unique(pg_kong_ALL$variable)]

# Clean up
rm(list = grep("pg_is",names(.GlobalEnv),value = TRUE)); gc()


### Species ----------------------------------------------------------------

# is  kittiwake population
is_kittiwakke_population <- read.csv("~/pCloudDrive/FACE-IT_data/svalbard/black-legged-kittiwake-p.csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`Fuglehuken`, `Bjørnøya`, `Grumant`, `Sofiekammen`, `Ossian.Sars`, `Tschermakfjellet`, `Alkhornet`, `Amsterdamya`)) %>%
  filter(name == "Tschermakfjellet"| name == "Alkhornet") %>% 
  mutate(date_accessed = as.Date("2023-04-13"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/black-legged-kittiwake/",
         citation = "Norwegian Polar Institute (2022). Black-legged kittiwake population size, as percentage of the average in the colony. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/black-legged-kittiwake.html",
         lon = NA, lat = NA, depth = NA,
         Species = "Rissa tridactyla",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " population [% average in the colony]"),
         category = "bio",
         driver ="biomass",
         site = "is",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))

# is Brünnich’s guillemot population
is_brguillemot_population <- read.csv("~/pCloudDrive/FACE-IT_data/svalbard/brnnichs-guillemot-breed.csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`Diabas`, `Alkhornet`, `Sofiekammen`, `Grumant`, `Tschermakfjellet`, `Fuglehuken`, `Ossian.Sarsfjellet`, `Bjørnøya..southern.part`, `Bjørnøya..Evjebukta`, `Jan.Mayen`)) %>%
  filter(name == "Diabas"|name == "Tschermakfjellet"|name == "Alkhornet") %>% 
  mutate(date_accessed = as.Date("2023-04-14"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/brunnichs-guillemot/",
         citation = "Norwegian Polar Institute (2022). Brünnich’s guillemot breeding populations, percentage of colony average. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/brunnichs-guillemot.html",
         lon = NA, lat = NA, depth = NA,
         Species = "Uria lomvia",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " breeding population [%]"),
         category = "bio",
         driver ="biomass",
         site = "svalbard",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))

# Combine and save
is_species <- rbind(is_kittiwakke_population,
                    is_brguillemot_population)
save(is_species, file = "~/pCloudDrive/FACE-IT_data/isfjorden/is_species.RData")
write_csv(is_species, file = "~/pCloudDrive/FACE-IT_data/isfjorden/is_species.csv")
rm(list = grep("is_",names(.GlobalEnv),value = TRUE)); gc()


### Wild data ---------------------------------------------------------------

# CO2 station at IsA
is_IsA_CO2 <- data.frame(type = "CTD",
                         data_name = "Sea temperature; salinity; TA; pH; EP TA",
                         date_range = "2015 - 2017",
                         # lon_range = "15.5", 
                         # lat_range = "78.3",
                         # depth_range = "1 - 91",
                         # file_name = '<a onclick="alert(\'Marine_CO2_system_data_at_the_IsA_Station_2015_to_2017.xlsx; Marine_CO2_system_data_at_the_IsA_Station_2015_to_2017.csv\');"2 files</a>',
                         URL = '<a target="_blank" rel="noopener noreferrer" href="http://metadata.nmdc.no/metadata-api/landingpage/1e5ae6511b1c22a2f8d00aac50c32eb5">NMDC</a>',
                         reference = '<a onclick="alert(\'Ylva Ericson, UNIS, Eva Falck, UNIS, and Melissa Chierici, IMR and UNIS. (2019) Marine CO2 system data from the IsA Station, Svalbard, 2015-2017 https://doi.org/10.21335/NMDC-80568951\');">Ericson et al. (2019)</a>',
                         note = NA)

# Chlorophyll station at IsA
is_IsA_Chla <- data.frame(type = "Niskin bottle",
                          data_name = "ChlA",
                          date_range = "2011 - 2019",
                          # lon_range = "15.5", lat_range = "78.3",
                          # depth_range = "0 - 980",
                          # file_name = '<a onclick="alert(\'chl_a: IsA_Svalbard_Chlorophyll_A_2011_2019_10um.nc; IsA_Svalbard_Chlorophyll_A_2011_2019_GFF.nc\');">1 folder, 2 files</a>',
                          URL = '<a onclick="alert(\'https://archive.sigma2.no/pages/public/datasetDetail.jsf?id=10.11582/2020.00063; https://ns9999k.webs.sigma2.no/10.11582_2020.00063/IsA_Svalbard_Chlorophyll_A_2011_2019_10um.nc; https://ns9999k.webs.sigma2.no/10.11582_2020.00063/IsA_Svalbard_Chlorophyll_A_2011_2019_GFF.nc\');">NIRD</a>',
                          reference = '<a onclick="alert(\'University Centre in Svalbard (2020). ISA_Svalbard_Chlorophyll_A_2011_2019 [Data set]. Norstore. https://doi.org/10.11582/2020.00063\');">UCS (2020)</a>',
                          note = NA)

# Light related data at the IsA station
is_IsA_light <- data.frame(type = "",
                           data_name = "PAR; suspended matter: organic, mineral",
                           date_range = "2012 - ?",
                           # lon_range = "15.5", lat_range = "78.3",
                           # depth_range = "0 - 15",
                           # file_name = '<a onclick="alert(\' \');">0 files</a>',
                           URL = NA, 
                           reference = '<a onclick="alert(\'No reference available yet.\');">Vader et al. (In prep.)</a>',
                           note = "Data are not yet publicly available.")

# Biology related data at the IsA station
is_IsA_bio <- data.frame(type = "Niskin bottle",
                         data_name = "Nutrients: nitrate, nitrite, phosphate, silicate; DNA; phytoplankton: species, abundance; ChlA concentration; protists; bacteria",
                         date_range = "2011 - ?",
                         # lon_range = "15.5", lat_range = "78.3",
                         # depth_range = "0, 15, and 75",
                         # file_name = '<a onclick="alert(\' \');">0 files</a>',
                         URL = NA,
                         reference = '<a onclick="alert(\'No reference available yet.\');">Vader et al. (In prep.)</a>',
                         note = "Data are not yet publicly available.")

# Biology related data at the BAB station
is_BAB_bio <- data.frame(type = "Niskin bottle",
                         data_name = "Nutrients: nitrate, nitrite, phosphate, silicate; DNA; phytoplankton: species, abundance; ChlA concentration; protists",
                         date_range = "2020 - ?",
                         # lon_range = "16.7", lat_range = "78.7",
                         # depth_range = "0, 15, and 150",
                         # file_name = '<a onclick="alert(\' \');">0 files</a>',
                         URL = NA,
                         reference = '<a onclick="alert(\'No reference available yet.\');">Vader et al. (In prep.)</a>',
                         note = "Data are not yet publicly available.")

# Zooplankton data
is_zoo <- data.frame(type = "Multinet",
                     data_name = "Zooplankton: species, abundance, biomass",
                     date_range = "2001 - ?",
                     # lon_range = "14.0 - 16.7", 
                     # lat_range = "78.1 - 78.7",
                     # depth_range = "?",
                     # file_name = '<a onclick="alert(\' \');">0 files</a>',
                     URL = NA,
                     reference = '<a onclick="alert(\'No reference available yet.\');">Søreide et al. (In prep.)</a>',
                     note = "Data are not yet publicly available.")

# Social data for Isfjorden
is_social <- data.frame(type = "Statistics",
                        data_name = "Game landings; local management; national statistics; tourist vessels: mileage; tourist arrivals",
                        date_range = NA, 
                        # lon_range = NA, lat_range = NA, depth_range = NA,
                        # file_name = '<a onclick="alert(\'\');">0 files</a>',
                        URL = NA, reference = NA,
                        note = "Data are still being pursued.")

# is kittiwake population
is_ritr <- data.frame(type = "In situ",
                      data_name = "Biomass: Rissa tridactyla (black-legged kittiwake)",
                      date_range = "1988 to 2021",
                      URL = '<a target="_blank" rel="noopener noreferrer" href="https://mosj.no/en/indikator/fauna/marine-fauna/black-legged-kittiwake//">MOSJ</a>',
                      reference = '<a onclick="alert(\'Norwegian Polar Institute (2022). Black-legged kittiwake population size, as percentage of the average in the colony. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/black-legged-kittiwake.html\');">NPI (2022)</a>',
                      note = NA)

# is brünnich’s guillemot population
is_urlo <- data.frame(type = "In situ",
                      data_name = "Biomass: Uria lomvia (brünnich’s guillemot)",
                      date_range = "1988 to 2021",
                      URL = '<a target="_blank" rel="noopener noreferrer" href="https://mosj.no/en/indikator/fauna/marine-fauna/brunnichs-guillemot/">MOSJ</a>',
                      reference = '<a onclick="alert(\'Norwegian Polar Institute (2022). Brünnich’s guillemot breeding populations, percentage of colony average. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/brunnichs-guillemot.html\');">NPI (2022)</a>',
                      note = NA)


# Process individual files
## Mouth mooring North
is_mooring_N <- plyr::ldply(dir("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_N", full.names = T), load_is_mooring, .parallel = T); gc()

## Mouth mooring South
is_mooring_S <- plyr::ldply(dir("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_S", full.names = T), load_is_mooring, .parallel = T); gc()

## Mooring IFO
is_mooring_IFO_units <- rbind(ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_IFO/IFO1617.nc")$variable,
                              ncdump::NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/mooring_IFO/IFO1617_ADCP.nc")$variable) |> 
  dplyr::select(-id) |> distinct()
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
is_met_radio <- load_met_NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/SN99790.nc") %>% 
  mutate(date_accessed = as.Date("2021-04-14"), .before = 1)

## Airport meteorological station
is_met_airport <- load_met_NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/SN99840.nc") %>% 
  mutate(date_accessed = as.Date("2021-08-04"), .before = 1)

## Pyramiden radio meteorological station
is_met_pyramiden <- load_met_NetCDF("~/pCloudDrive/FACE-IT_data/isfjorden/SN99880.nc") %>% 
  mutate(date_accessed = as.Date("2021-08-04"), .before = 1)

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
is_wild <- rbind(is_mooring_N, is_mooring_S, is_mooring_IFO, is_mooring_GFI_N, is_mooring_GFI_S,
                 is_CO2_tempelfjorden, is_CO2_IsA, is_Chla_IsA, is_met_radio, is_met_airport, is_met_pyramiden, 
                 is_AIS, is_ship_arrivals) |> mutate(site = "is") |>
  left_join(full_var_list, by = c("category", "variable")) |> 
  dplyr::select(date_accessed, URL, citation, site, lon, lat, date, depth, category, driver, variable, value)
data.table::fwrite(is_wild, "~/pCloudDrive/FACE-IT_data/isfjorden/is_wild.csv")
save(is_wild, file = "~/pCloudDrive/FACE-IT_data/isfjorden/is_wild.RData")
rm(list = grep("is_",names(.GlobalEnv),value = TRUE)); gc()


### Full product ------------------------------------------------------------

# Load full Svalbard file
## NB: This contains the full EU Arctic data
if(!exists("full_product_sval")) load("data/full_data/full_product_sval.RData")

# Subset to Isfjorden bbox and references for data missing lon/lat coords
is_sval_sub <- filter_site_plural("is")

# Load PG file
if(!exists("pg_is_ALL")) load("~/pCloudDrive/FACE-IT_data/isfjorden/pg_is_ALL.RData")

# Load species
if(!exists("is_species")) load("~/pCloudDrive/FACE-IT_data/isfjorden/is_species.RData")

# Load wild data
if(!exists("is_wild")) load("~/pCloudDrive/FACE-IT_data/isfjorden/is_wild.RData")

# Combine and save
full_product_is <- rbind(is_sval_sub, pg_is_ALL, is_species, is_wild) |> mutate(site = "is") |> distinct()
data.table::fwrite(full_product_is, "~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.csv")
save(full_product_is, file = "~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.RData")
save(full_product_is, file = "data/full_data/full_product_is.RData")
save_data(full_product_is)
rm(list = grep("is_",names(.GlobalEnv),value = TRUE)); gc()
# if(!exists("full_product_is")) load("~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.RData")


## Storfjorden -------------------------------------------------------------

### PG product --------------------------------------------------------------

# Load pg stor files
system.time(
  pg_stor_sub <- plyr::ldply(pg_files, pg_site_filter, site_name = "stor")
) # 21 seconds

# Test problem files
# pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
# pg_test <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.778258")

# Remove unneeded columns
pg_stor_clean <- pg_stor_sub |> 
  # dplyr::select(contains(c("date", "lon", "lat")), everything()) |> # Look at meta columns
  # dplyr::select(contains(c("press", "depth", "elev", "lon", "lat")), everything()) |> # Look at depth columns
  # Remove empty columns
  mutate_if(is.character, ~na_if(., '')) |>
  janitor::remove_empty("cols") |>
  # Manually remove problematic files - No issues
  # Manually remove problematic columns - No issues
  # Manage lon/lat columns - No issues
  # Manage date column
  dplyr::rename(date = `Date/Time`) |> 
  mutate(date = case_when(is.na(date) & nchar(`Sampling date`) == 9 ~ sapply(str_split(`Sampling date`, "-"), "[[", 1),
                          TRUE ~ date),
         date = case_when(nchar(date) == 4 ~ paste0(date,"-01-01"),
                          nchar(date) == 7 ~ paste0(date,"-01"),
                          TRUE ~ date)) |> 
  mutate(date = as.Date(gsub("T.*", "", date))) |> 
  # Manage depth column
  dplyr::rename(`Depth [m]1` = `Depth water [m] (water depth from ETOPO1, if >...)`) |> 
  mutate(depth = case_when(!is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Depth [m]`) ~ as.numeric(`Depth [m]`),
                           !is.na(`Depth [m]1`) ~ as.numeric(`Depth [m]1`))) |> 
  mutate(depth = case_when(is.na(depth) & !is.na(`Press [dbar]`) ~ as.numeric(`Press [dbar]`),
                           is.na(depth) & !is.na(`Elevation [m]`) ~ -as.numeric(`Elevation [m]`),
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -as.numeric(`Elevation [m a.s.l.]`),
                           TRUE ~ depth)) |> 
  # dplyr::select(depth, everything())
  # Remove unwanted columns
  # dplyr::select(-contains(c("Elev ", "Elevation ", "Latitude", "Longitude"))) |> 
  # Finish up
  left_join(pg_meta_files, by = c("meta_idx", "site")) |> 
  dplyr::select(date_accessed, URL, citation, site, lon, lat, date, depth, everything()) |> 
  # NB: This must be changed manually when new data are loaded
  mutate(across(!dplyr::all_of(clean_cols), as.numeric)) |>  
  janitor::remove_empty("cols") |> 
  # NB: Site exists earlier to reflect data from different files for metadata joining
  mutate(site = "stor")
# colnames(pg_stor_clean)

# Individual category data.frames
pg_stor_cryo <- pg_var_melt(pg_stor_clean, query_cryo)
pg_stor_phys <- pg_var_melt(pg_stor_clean, query_phys)
pg_stor_chem <- pg_var_melt(pg_stor_clean, query_chem)
pg_stor_bio <- pg_var_melt(pg_stor_clean, query_bio)
pg_stor_soc <- pg_var_melt(pg_stor_clean, query_soc) # empty

# Stack them together
pg_stor_ALL <- rbind(pg_stor_cryo, pg_stor_phys, pg_stor_chem, pg_stor_bio, pg_stor_soc)
data.table::fwrite(pg_stor_ALL, "~/pCloudDrive/FACE-IT_data/storfjorden/pg_stor_ALL.csv")
save(pg_stor_ALL, file = "~/pCloudDrive/FACE-IT_data/storfjorden/pg_stor_ALL.RData")

# Check that all columns were used
colnames(pg_stor_clean)[!gsub("\\] \\(.*", "\\]", colnames(pg_stor_clean)) %in% unique(pg_stor_ALL$variable)]

# Clean up
rm(list = grep("pg_stor",names(.GlobalEnv),value = TRUE)); gc()


### Wild data ---------------------------------------------------------------

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
stor_wild <- rbind(stor_light_CTD) |> mutate(site = "stor") |> distinct() |> 
  left_join(full_var_list, by = c("category", "variable")) |> 
  dplyr::select(date_accessed, URL, citation, site, lon, lat, date, depth, category, driver, variable, value)
data.table::fwrite(stor_wild, "~/pCloudDrive/FACE-IT_data/storfjorden/stor_wild.csv")
save(stor_wild, file = "~/pCloudDrive/FACE-IT_data/storfjorden/stor_wild.RData")
rm(list = grep("stor_",names(.GlobalEnv),value = TRUE)); gc()


### Full product ------------------------------------------------------------

# Load full Svalbard file
## NB: This contains the full EU Arctic data
if(!exists("full_product_sval")) load("data/full_data/full_product_sval.RData")

# Subset to Storfjorden bbox and references for data missing lon/lat coords
stor_sval_sub <- filter_site_plural("stor")

# Load PG file
if(!exists("pg_stor_ALL")) load("~/pCloudDrive/FACE-IT_data/storfjorden/pg_stor_ALL.RData")

# Load species
## NB: There are none for this site

# Load wild data
if(!exists("stor_wild")) load("~/pCloudDrive/FACE-IT_data/storfjorden/stor_wild.RData")

# Combine and save
full_product_stor <- rbind(stor_sval_sub, pg_stor_ALL, stor_wild) |> mutate(site = "stor") |> distinct()
data.table::fwrite(full_product_stor, "~/pCloudDrive/FACE-IT_data/storfjorden/full_product_stor.csv")
save(full_product_stor, file = "~/pCloudDrive/FACE-IT_data/storfjorden/full_product_stor.RData")
save(full_product_stor, file = "data/full_data/full_product_stor.RData")
save_data(full_product_stor)
rm(list = grep("stor_",names(.GlobalEnv),value = TRUE)); gc()
# if(!exists("full_product_stor")) load("~/pCloudDrive/FACE-IT_data/storfjorden/full_product_stor.RData")


## Greenland ---------------------------------------------------------------

# NB: There is only this one full product step for Greenland
# These are all social data

# National statistics
## Income
green_income_json <- 
  pxweb_get(url = "https://bank.stat.gl:443/api/v1/en/Greenland/IN/IN20/INXPI101.px",
            query = "data/JSON/pxapi-api_table_INXPI101.px.json")
green_income <- as.data.frame(green_income_json, 
                              column.name.type = "text", variable.value.type = "text") %>% 
  dplyr::rename(value = `Income for persons (14 years +)`, site = municipality) %>% 
  mutate(variable = paste0(`type of income`," - ",gender, " [DKK]"),
         date = as.Date(paste0(time,"-12-31")), date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = green_income_json$url,
         citation = px_cite(green_income_json)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site)

## Monthly employment and income
green_employment_json <- 
  pxweb_get(url = "https://bank.stat.gl:443/api/v1/en/Greenland/AR/AR30/ARXBFB3.px",
            query = "data/JSON/pxapi-api_table_ARXBFB3.px.json")
green_employment <- as.data.frame(green_employment_json, 
                                  column.name.type = "text", variable.value.type = "text") %>% 
  dplyr::rename(value = `Main employment for permanent residents`, long_var = `inventory variable`,
                site = municipality) %>% 
  mutate(long_var = case_when(grepl("Number of main", long_var) ~ paste0(" - main employment - ", gender," [n/month]"),
                              grepl("Average monthly", long_var) ~ paste0(" - income - ",gender," [DKK/month]")),
         variable = paste0(industry, long_var),
         date = as.Date(paste0(time,"-12-31")), date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = green_employment_json$url,
         citation = px_cite(green_employment_json)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site)

## Unemployment
green_unemployment_json <- 
  pxweb_get(url = "https://bank.stat.gl:443/api/v1/en/Greenland/AR/AR40/ARXLED3.px",
            query = "data/JSON/pxapi-api_table_ARXLED3.px.json")
green_unemployment <- as.data.frame(green_unemployment_json, 
                                    column.name.type = "text", variable.value.type = "text") %>% 
  dplyr::rename(value = `Unemployment among permanent residents aged 18-65 years`, site = district) %>% 
  mutate(variable = paste0("Unemployed - ",gender, " [n]"),
         date = as.Date(paste0(time,"-12-31")), date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = green_unemployment_json$url,
         citation = px_cite(green_unemployment_json)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site)

## Population
green_pop_json <- 
  pxweb_get(url = "https://bank.stat.gl:443/api/v1/en/Greenland/BE/BE01/BEXSAT1.PX",
            query = "data/JSON/pxapi-api_table_BEXSAT1.PX.json")
green_pop <- as.data.frame(green_pop_json,
                           column.name.type = "text", variable.value.type = "text") %>% 
  dplyr::rename(value = `Population and population growth`) %>% 
  mutate(site = "green",
         variable = case_when(type == "Number"~ "Population [n]",
                              type == "Growth" ~ "Population growth [n]",
                              type == "Growth in percent" ~ "Population growth [%]"),
         date = as.Date(paste0(time,"-12-31")), date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = green_pop_json$url,
         citation = px_cite(green_pop_json)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site)

## Cruise passengers
green_cruise_passenger_json <-
  pxweb_get(url = "https://bank.stat.gl:443/api/v1/en/Greenland/TU/TU10/TUXKRH.px",
            query = "data/JSON/pxapi-api_table_TUXKRH.px.json")
green_cruise_passenger <- as.data.frame(green_cruise_passenger_json,
                                        column.name.type = "text", variable.value.type = "text") %>% 
  dplyr::rename(value = `Number of cruise passengers for each harbour`, site = port) %>% 
  mutate(variable = case_when(month == "Total" ~ "Cruise passengers - total [n]",
                              TRUE ~ "Cruise passengers [n]"),
         month_int = case_when(month == "Total" ~ 12,
                               TRUE ~ as.numeric(match(month, month.name))),
         date = as.Date(invalid_resolve(set_day(year_month_day_parse(paste0(time,"-",month_int),
                                                             format = "%Y-%m", precision = "month"), 31), invalid = "previous")),
         date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = green_cruise_passenger_json$url,
         citation = px_cite(green_cruise_passenger_json)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site)

## Cruise arrivals
green_cruise_count_json <- 
  pxweb_get(url = "https://bank.stat.gl:443/api/v1/en/Greenland/TU/TU10/TUXKRK.px",
          query = "data/JSON/pxapi-api_table_TUXKRK.px.json")
green_cruise_count <- as.data.frame(green_cruise_count_json,
                                    column.name.type = "text", variable.value.type = "text") %>% 
  dplyr::rename(value = `Number of cruise passengers for each harbour`) %>% 
  mutate(site = "green",
         variable = case_when(unit == "Number of cruises" ~ 
                                paste0("Cruise capacity (",capacity,") arrivals [n]"),
                              unit == "Passengers" ~ 
                                paste0("Cruise capacity (",capacity,") passengers [n]")),
         month_int = case_when(month == "Total" ~ 12,
                               TRUE ~ as.numeric(match(month, month.name))),
         date = as.Date(invalid_resolve(set_day(year_month_day_parse(paste0(time,"-",month_int),
                                                                     format = "%Y-%m", precision = "month"), 31), invalid = "previous")),
         date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = green_cruise_passenger_json$url,
         citation = px_cite(green_cruise_passenger_json)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site)

## Cruise passenger nationality
green_cruise_nation_json <- 
  pxweb_get(url = "https://bank.stat.gl:443/api/v1/en/Greenland/TU/TU10/TUXKRL.px",
            query = "data/JSON/pxapi-api_table_TUXKRL.px.json")
green_cruise_nation <- as.data.frame(green_cruise_nation_json,
                                    column.name.type = "text", variable.value.type = "text") %>% 
  dplyr::rename(value = `Nationality of cruise passengers`) %>% 
  mutate(site = "green",
         variable = case_when(month == "Total" ~ paste0("Cruise passengers - ",nation," -total [n]"),
                              TRUE ~ paste0("Cruise passengers - ",nation," [n]")),
         month_int = case_when(month == "Total" ~ 12,
                               TRUE ~ as.numeric(match(month, month.name))),
         date = as.Date(invalid_resolve(set_day(year_month_day_parse(paste0(time,"-",month_int),
                                                                     format = "%Y-%m", precision = "month"), 31), invalid = "previous")),
         date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = green_cruise_nation_json$url,
         citation = px_cite(green_cruise_nation_json)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site)

## Air passenger arrivals
green_air_passenger_json <- 
  pxweb_get(url = "https://bank.stat.gl:443/api/v1/en/Greenland/TU/TU20/TUXUPAX.px",
            query = "data/JSON/pxapi-api_table_TUXUPAX.px.json")
green_air_passenger <- as.data.frame(green_air_passenger_json,
                                     column.name.type = "text", variable.value.type = "text") %>% 
  dplyr::rename(value = `Number of international passengers`, site = airport) %>% 
  mutate(variable = case_when(month == "Total" ~ "Airport arrivals - total [n]",
                              TRUE ~ "Airport arrivals [n]"),
         month_int = case_when(month == "Total" ~ 12,
                               TRUE ~ as.numeric(match(month, month.name))),
         date = as.Date(invalid_resolve(set_day(year_month_day_parse(paste0(time,"-",month_int),
                                                                     format = "%Y-%m", precision = "month"), 31), invalid = "previous")),
         date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = green_air_passenger_json$url,
         citation = px_cite(green_air_passenger_json)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site)

## Overnight stays
green_guests_json <- 
  pxweb_get(url = "https://bank.stat.gl:443/api/v1/en/Greenland/TU/TU30/TUXHOT.px",
            query = "data/JSON/pxapi-api_table_TUXHOT.px.json")
green_guests <- as.data.frame(green_guests_json,
                              column.name.type = "text", variable.value.type = "text") %>% 
  dplyr::rename(value = `Overnight stays`, site = region) %>% 
  mutate(variable = case_when(month == "Total" ~ paste0(unit," - total [n]"),
                              TRUE ~ paste0(unit," [n]")),
         month_int = case_when(month == "Total" ~ 12,
                               TRUE ~ as.numeric(match(month, month.name))),
         date = as.Date(invalid_resolve(set_day(year_month_day_parse(paste0(time,"-",month_int),
                                                                     format = "%Y-%m", precision = "month"), 31), invalid = "previous")),
         date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = green_guests_json$url,
         citation = px_cite(green_guests_json)) %>% 
  filter(!is.na(value)) %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable, site) %>% 
  summarise(value = sum(value, na.rm = T), .groups = "drop")

## Sled dogs
green_dogs_json <- 
  pxweb_get(url = "https://bank.stat.gl:443/api/v1/en/Greenland/TU/TUX01.px",
            query = "data/JSON/pxapi-api_table_TUX01.px.json")
green_dogs <- as.data.frame(green_dogs_json,
                            column.name.type = "text", variable.value.type = "text") %>% 
  dplyr::rename(value = `Number of sled dogs`, site = district) %>% 
  mutate(variable = case_when(site == "Total" ~ "Sled dogs - total [n]",
                              TRUE ~ "Sled dogs [n]"),
         date = as.Date(paste0(time,"-12-31")),
         date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = green_dogs_json$url,
         citation = px_cite(green_dogs_json)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site)

## Domestic landings
green_landings_domestic_json <- 
  pxweb_get(url = "https://bank.stat.gl:443/api/v1/en/Greenland/FI/FI10/FIX012.px",
            query = "data/JSON/pxapi-api_table_FIX012.px.json")
green_landings_domestic <- as.data.frame(green_landings_domestic_json,
                                         column.name.type = "text", variable.value.type = "text") %>% 
  dplyr::rename(value = `Total landings of fish and shellfish`, site = district) %>% 
  mutate(variable = paste0(species," - ",`vessel type`," - ",`fishing segment`," [",enhed,"]"),
         month_int = as.numeric(match(month, month.name)),
         date = as.Date(invalid_resolve(set_day(year_month_day_parse(paste0(time,"-",month_int),
                                                                     format = "%Y-%m", precision = "month"), 31), invalid = "previous")),
         date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = green_landings_domestic_json$url,
         citation = px_cite(green_landings_domestic_json)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site)

## International landings
green_landings_inter_json <- 
  pxweb_get(url = "https://bank.stat.gl:443/api/v1/en/Greenland/FI/FI10/FIX010.px",
            query = "data/JSON/pxapi-api_table_FIX010.px.json")
green_landings_inter <- as.data.frame(green_landings_inter_json,
                                      column.name.type = "text", variable.value.type = "text") %>% 
  dplyr::rename(value = `Total catch of fish and shellfish in ton and type of vessels`, site = area) %>% 
  mutate(variable = paste0(species," - ",`vessel type`," - ",`fishsegm`," - ",nation," - ",quarter," [Tonnes]"),
         month_int = case_when(quarter == "Quarter 1" ~ 3,
                               quarter == "Quarter 2" ~ 6,
                               quarter == "Quarter 3" ~ 9,
                               quarter == "Quarter 4" ~ 12),
         date = as.Date(invalid_resolve(set_day(year_month_day_parse(paste0(time,"-",month_int),
                                                                     format = "%Y-%m", precision = "month"), 31), invalid = "previous")),
         date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = green_landings_inter_json$url,
         citation = px_cite(green_landings_inter_json)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site)

## International landings
green_fish_price_json <- 
  pxweb_get(url = "https://bank.stat.gl:443/api/v1/en/Greenland/FI/FI60/FIX009.px",
            query = "data/JSON/pxapi-api_table_FIX009.px.json")
green_fish_price <- as.data.frame(green_fish_price_json,
                                  column.name.type = "text", variable.value.type = "text") %>% 
  dplyr::rename(value = `Index of average kilo prices of selected fish products (2010 = 100)`) %>% 
  mutate(site = "green",
         value = case_when(time == 2010 ~ 100, TRUE ~ value),
         species = str_replace(species, ",", " -"),
         variable = case_when(quarter == "Average" ~ paste0("Average kilo price - ",species," - total [index]"),
                              TRUE ~ paste0("Average kilo price - ",species," [index]")),
         month_int = case_when(quarter == "1st quarter" ~ 3,
                               quarter == "2nd quarter" ~ 6,
                               quarter == "3rd quarter" ~ 9,
                               quarter == "4th quarter" ~ 12,
                               quarter == "Average" ~ 12),
         date = as.Date(invalid_resolve(set_day(year_month_day_parse(paste0(time,"-",month_int),
                                                                     format = "%Y-%m", precision = "month"), 31), invalid = "previous")),
         date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = green_fish_price_json$url,
         citation = px_cite(green_fish_price_json)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site)

## International landing quotas
green_quotas_json <- 
  pxweb_get(url = "https://bank.stat.gl:443/api/v1/en/Greenland/FI/FI10/FIXKVOT.px",
            query = "data/JSON/pxapi-api_table_FIXKVOT.px.json")
green_quotas <- as.data.frame(green_quotas_json,
                              column.name.type = "text", variable.value.type = "text") %>% 
  dplyr::rename(value = `Fish quotas for offshore- and coastal fisheries for Greenland and other countries`, site = area) %>% 
  mutate(variable = paste0("Quota - ",species," - ",`fishplace`," - ",`nation`," [Tonnes]"),
         date = as.Date(paste0(time,"-12-31")),
         date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = green_quotas_json$url,
         citation = px_cite(green_quotas_json)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site)

## Domestic quota advice
green_quota_advice_json <- 
  pxweb_get(url = "https://bank.stat.gl:443/api/v1/en/Greenland/FI/FI70/FIX021.px",
            query = "data/JSON/pxapi-api_table_FIX021.px.json")
green_quota_advice <- as.data.frame(green_quota_advice_json,
                                    column.name.type = "text", variable.value.type = "text") %>% 
  dplyr::rename(value = `Advice on permitted catches of fish`, site = area) %>% 
  mutate(form = case_when(form == "Kvota" ~ "Quota", TRUE ~ form),
         variable = paste0(form," - ",species," [pieces]"),
         date = as.Date(paste0(time,"-12-31")),
         date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = green_quota_advice_json$url,
         citation = px_cite(green_quota_advice_json)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site)

## Domestic quota advice
green_fish_exports_json <- 
  pxweb_get(url = "https://bank.stat.gl:443/api/v1/en/Greenland/IE/IEXEXPMND.px",
            query = "data/JSON/pxapi-api_table_IEXEXPMND.px.json")
green_fish_exports <- as.data.frame(green_fish_exports_json,
                                    column.name.type = "text", variable.value.type = "text") %>% 
  dplyr::rename(value = `Exports of fishproducts`) %>% 
  mutate(site = "green",
         product = str_replace(product, "Exports,", ""),
         product = str_replace(product, ",", " -"),
         variable = case_when(month == "Total" ~ paste0("Export - ",product," - total [",unit,"]"),
                              TRUE ~ paste0("Export - ",product," [",unit,"]")),
         month_int = case_when(month == "Total" ~ 12,
                               TRUE ~ as.numeric(match(month, month.name))),
         date = as.Date(invalid_resolve(set_day(year_month_day_parse(paste0(time,"-",month_int),
                                                                     format = "%Y-%m", precision = "month"), 31), invalid = "previous")),
         date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = green_fish_exports_json$url,
         citation = px_cite(green_fish_exports_json)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site)

# Combine and save
# TODO: Fix issues with broken links above
full_product_green <- rbind(green_income, green_employment, green_unemployment, green_pop,
                            green_cruise_passenger, green_cruise_count, green_cruise_nation,
                            green_air_passenger, green_guests, green_dogs, green_landings_domestic,
                            green_landings_inter, green_fish_price, green_quotas, green_quota_advice,
                            green_fish_exports) |>
  left_join(full_var_list, by = c("category", "variable")) |> 
  dplyr::select(date_accessed, URL, citation, site, lon, lat, date, depth, category, driver, variable, value)
data.table::fwrite(full_product_green, "~/pCloudDrive/FACE-IT_data/greenland/full_product_green.csv")
save(full_product_green, file = "~/pCloudDrive/FACE-IT_data/greenland/full_product_green.RData")
save(full_product_green, file = "data/full_data/full_product_green.RData")
save_data(full_product_green) # NB: Shouldn't save anything
rm(list = grep("green_",names(.GlobalEnv),value = TRUE)); gc()


## Young Sound -------------------------------------------------------------

### PG product --------------------------------------------------------------

# Load pg young files
system.time(
  pg_young_sub <- plyr::ldply(pg_files, pg_site_filter, site_name = "young")
) # 27 seconds

# Test problem files
# pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
# pg_test <- pg_dl_proc(pg_doi = pg_young_all$doi[32])

# Remove unneeded columns
pg_young_clean <- pg_young_sub |> 
  # dplyr::select(contains(c("date", "lon", "lat")), everything()) |> # Look at meta columns
  # dplyr::select(contains(c("depth", "press", "bathy", "elev")), everything()) |> # Look at depth columns
  # Remove empty columns
  mutate_if(is.character, ~na_if(., '')) |> 
  janitor::remove_empty("cols") |> 
  # Manually remove problematic files - no need
  # Manually remove problematic columns - no need
  # Manage lon/lat columns - no need
  # Manage date column
  dplyr::rename(date = `Date/Time`) |> 
  mutate(date = case_when(is.na(date) & !is.na(`Date/time start`) ~ `Date/time start`,
                          is.na(date) & nchar(`Sampling date`) == 9 ~ sapply(str_split(`Sampling date`, "-"), "[[", 1),
                          TRUE ~ date),
         date = case_when(nchar(date) == 4 ~ paste0(date,"-01-01"),
                          nchar(date) == 7 ~ paste0(date,"-01-01"),
                          TRUE ~ date),
         date = ifelse(date == "", NA, date)) |> 
  mutate(date = as.Date(gsub("T.*", "", date))) |> 
  # Manage depth column
  # dplyr::rename(`Depth [m]1` = `Depth water [m] (water depth from ETOPO1, if >...)`) |> 
  mutate(depth = case_when(!is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Depth [m]`) ~ as.numeric(`Depth [m]`))) |> #,
                           # !is.na(`Depth [m]1`) ~ as.numeric(`Depth [m]1`))) |> 
  mutate(depth = case_when(is.na(depth) & !is.na(`Depth top [m]`) ~ as.numeric(`Depth top [m]`),
                           is.na(depth) & !is.na(`Elevation [m]`) ~ -as.numeric(`Elevation [m]`),
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -as.numeric(`Elevation [m a.s.l.]`),
                           is.na(depth) & !is.na(`Elevation [m a.s.l.] (GLWD)`) ~ -as.numeric(`Elevation [m a.s.l.] (GLWD)`),
                           TRUE ~ depth)) |>  
  # dplyr::select(depth, everything())
  # Remove unwanted columns
  # dplyr::select(-"Longitude 2", -"Latitude 2",
  #               -contains(c("Date/", "Elevation ", "Press "))) |>
  # Finish up
  left_join(pg_meta_files, by = c("meta_idx", "site")) |> 
  dplyr::select(date_accessed, URL, citation, site, lon, lat, date, depth, everything()) |> 
  # NB: This must be changed manually when new data are loaded
  mutate(across(!dplyr::all_of(clean_cols), as.numeric)) |>  
  janitor::remove_empty("cols") |> 
  # NB: Site exists earlier to reflect data from different files for metadata joining
  mutate(site = "young")
# colnames(pg_young_clean)

# Individual category data.frames
pg_young_cryo <- pg_var_melt(pg_young_clean, query_cryo)
pg_young_phys <- pg_var_melt(pg_young_clean, query_phys)
pg_young_chem <- pg_var_melt(pg_young_clean, query_chem)
pg_young_bio <- pg_var_melt(pg_young_clean, query_bio)
pg_young_soc <- pg_var_melt(pg_young_clean, query_soc) # empty

# Stack them together
pg_young_ALL <- rbind(pg_young_cryo, pg_young_phys, pg_young_chem, pg_young_bio, pg_young_soc)
data.table::fwrite(pg_young_ALL, "~/pCloudDrive/FACE-IT_data/young_sound/pg_young_ALL.csv")
save(pg_young_ALL, file = "~/pCloudDrive/FACE-IT_data/young_sound/pg_young_ALL.RData")

# Check that all columns were used
# TODO: Follow up on some of these
colnames(pg_young_clean)[!gsub("\\] \\(.*", "\\]", colnames(pg_young_clean)) %in% unique(pg_young_ALL$variable)]

# Clean up
rm(list = grep("pg_young",names(.GlobalEnv),value = TRUE)); gc()


### Wild data ---------------------------------------------------------------

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
young_wild <- rbind(young_prim_prod) |> mutate(site = "young") |> distinct() |> 
  left_join(full_var_list, by = c("category", "variable")) |> 
  dplyr::select(date_accessed, URL, citation, site, lon, lat, date, depth, category, driver, variable, value)
data.table::fwrite(young_wild, "~/pCloudDrive/FACE-IT_data/young_sound/young_wild.csv")
save(young_wild, file = "~/pCloudDrive/FACE-IT_data/young_sound/young_wild.RData")
rm(list = grep("young_",names(.GlobalEnv),value = TRUE)); gc()


### GEM ---------------------------------------------------------------------

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
## After some reflection it was decided to keep the values missing lon/lat because they still have date values
## Meaning that they are useful when constructing a time series of change when creating averages over the study site
young_GEM_gmb_coords <- read_delim("~/pCloudDrive/restricted_data/GEM/young/Zackenberg_Data_Glacier_ice_GPS_sites.csv") %>% 
  mutate(site_name = str_replace(site_name, "s", "stake "),
         lon = case_when(lon %in% c(-99.99, -9999) ~ as.numeric(NA), 
                         lon > 100 ~ lon/10000000, 
                         lon > 0 ~ -lon,
                         lon > -1 ~ lon*100,
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
                   young_GEM_chla, young_GEM_nitrate_nitrite) |> mutate(site = "young") |>
  left_join(full_var_list, by = c("category", "variable")) |> 
  dplyr::select(date_accessed, URL, citation, site, lon, lat, date, depth, category, driver, variable, value)
save(young_GEM, file = "data/restricted/young_GEM.RData"); save(young_GEM, file = "~/pCloudDrive/restricted_data/GEM/young/young_GEM.RData")
rm(list = grep("young_GEM",names(.GlobalEnv),value = TRUE)); rm(young_mooring_multi); gc()


### GEM species ------------------------------------------------------------

# Bird breeding phenology nests eggs
## Have NA value
young_bird_nests_eggs <- read_delim("~/pCloudDrive/restricted_data/GEM/young/View_BioBasis_Zackenberg_Data_Birds_Bird_breeding_phenology__nests170420231421385886.csv", 
                                    na = c("9999-01-01","-9999"), 
                                    col_types = "iccnnDDiiicc") %>%
  convert_UTM_deg(utm_zone = 27) %>%
  pivot_longer(cols = c(`FirstEggDate`, `HatchingDate`)) %>% 
  dplyr::filter(name == "FirstEggDate") %>% 
  dplyr::rename(date_egg = value) %>%
  mutate(date_enfonction = ifelse(is.na(date_egg), 
                                  as.Date(paste0(Year,"-12-31")), 
                                  as.Date(date_egg)),
         date_bon = as.Date(date_enfonction),
         date_accessed = as.Date("2023-04-17"),
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/5S51-HE52",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         # lon = NA, lat = NA, # No longer necessary
         depth = NA,
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp," eggs laid [n]"),
         category = "bio",
         driver ="biomass",
         site = "young",
         value = EggsLaid) %>%
  dplyr::rename(date = date_bon) %>%
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value)

# Bird breeding phenology nests hatching
## Have NA/0 value
young_bird_nests_hatch <- read_delim("~/pCloudDrive/restricted_data/GEM/young/View_BioBasis_Zackenberg_Data_Birds_Bird_breeding_phenology__nests170420231421385886.csv",
                                     na = c("9999-01-01","-9999"), 
                                     col_types = "iccnnDDiiicc") %>%
  convert_UTM_deg(utm_zone = 27) %>%
  pivot_longer(cols = c(`FirstEggDate`, 
                        `HatchingDate`)) %>% 
  dplyr::filter(name == "HatchingDate") %>% 
  dplyr::rename(date_hatch = value) %>%
  # add date
  mutate(date_enfonction = ifelse(is.na(date_hatch), 
                                  as.Date(paste0(Year,"-12-31")), 
                                  as.Date(date_hatch)),
         date_bon = as.Date(date_enfonction),
         date_accessed = as.Date("2023-04-17"),
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/5S51-HE52",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         # lon = NA, lat = NA, # No longer necessary
         depth = NA,
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp," eggs hatched [n]"),
         category = "bio",
         driver ="biomass",
         site = "young",
         value = PulliHatched) %>%
  dplyr::rename(date = date_bon) %>%
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))

# Bird abundance
young_bird_abundance <- read_delim("~/pCloudDrive/restricted_data/GEM/young/View_BioBasis_Zackenberg_Data_Birds_Breeding_bird_abundance170420231423146922.csv",
                                   col_types = "icinncc",
                                   na = "-9999") %>%
  convert_UTM_deg(utm_zone = 27) %>%
  mutate(date_accessed = as.Date("2023-04-17"),
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/1Z6Z-FQ32",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         depth = NA,
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp," [presence]"),
         category = "bio",
         driver ="biomass",
         date = as.Date(paste0(Year,"-12-31")),
         site = "young",
         value = 1) %>%
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value)

# Bird breeding phenology broods
## Have NA value
young_bird_broods <- read_delim("~/pCloudDrive/restricted_data/GEM/young/View_BioBasis_Zackenberg_Data_Birds_Bird_breeding_phenology__broods210420231531510758.csv",
                                    na = c("9999-01-01","-9999","#REF!","01/01/9999")) %>%
  convert_UTM_deg(utm_zone = 27) %>%
  dplyr::rename(date_egg = FirstEggDate) %>%
  mutate(date_enfonction = ifelse(is.na(date_egg), 
                                  as.Date(paste0(Year,"-12-31")), 
                                  as.Date(date_egg)),
         date_bon = as.Date(date_enfonction),
         date_accessed = as.Date("2023-04-21"),
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/YPNZ-VX08",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         # lon = NA, lat = NA, # No longer necessary
         depth = NA,
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp," eggs laid [n]"),
         category = "bio",
         driver = "biomass",
         site = "young",
         value = Accuracy) %>%
  dplyr::rename(date = date_bon) %>%
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value)

# Combine and save
young_species_GEM <- rbind(young_bird_nests_eggs, 
                           young_bird_nests_hatch,
                           young_bird_abundance,
                           young_bird_broods)
save(young_species_GEM, file = "~/pCloudDrive/restricted_data/GEM/young/young_species_GEM.RData")
write_csv(young_species_GEM, file = "~/pCloudDrive/restricted_data/GEM/young/young_species_GEM.csv")
rm(list = grep("young_",names(.GlobalEnv),value = TRUE))


### Full product ------------------------------------------------------------

# Load full EU file
if(!exists("full_product_EU")) load("data/full_data/full_product_EU.RData")

# Subset to Young Sound bbox and references for data missing lon/lat coords
young_EU_sub <- filter_site_plural("young", full_product_EU)

# Load Greenland data
## NB: These are all social data, and Young Sound lays outside of any provinces

# Load GEM data and create shadow
if(!exists("young_GEM")) load("~/pCloudDrive/restricted_data/GEM/young/young_GEM.RData")

# Create shadow
young_GEM_shadow <- shadow(young_GEM)

# Load GEM species data
if(!exists("young_species_GEM")) load("~/pCloudDrive/restricted_data/GEM/young/young_species_GEM.RData")

# Create shadow
young_species_GEM_shadow <- shadow(young_species_GEM)

# Load PG product
if(!exists("pg_young_ALL")) load("~/pCloudDrive/FACE-IT_data/young_sound/pg_young_ALL.RData")

# Load species data
## No non-GEM species data

# Load wild data
if(!exists("young_wild")) load("~/pCloudDrive/FACE-IT_data/young_sound/young_wild.RData")

# Combine and save
full_product_young <- rbind(young_EU_sub, young_GEM_shadow, young_species_GEM_shadow,
                            pg_young_ALL, young_wild) |> mutate(site = "young") |> distinct()
data.table::fwrite(full_product_young, "~/pCloudDrive/FACE-IT_data/young_sound/full_product_young.csv")
save(full_product_young, file = "~/pCloudDrive/FACE-IT_data/young_sound/full_product_young.RData")
save(full_product_young, file = "data/full_data/full_product_young.RData")
save_data(full_product_young)
rm(list = grep("young_",names(.GlobalEnv),value = TRUE)); gc()
# if(!exists("full_product_young")) load("~/pCloudDrive/FACE-IT_data/young_sound/full_product_young.RData")


## Disko Bay ---------------------------------------------------------------

### PG product --------------------------------------------------------------

# Load pg files and subset to Disko Bay
system.time(
  pg_disko_sub <- plyr::ldply(pg_files, pg_site_filter, site_name = "disko")
) # 27 seconds

# Test problem files
# pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
# pg_test <- pg_dl_proc(pg_doi = pg_disko_all$doi[32])

# Remove unneeded columns
pg_disko_clean <- pg_disko_sub |> 
  # dplyr::select(contains(c("date", "lon", "lat")), everything()) |> # Look at meta columns
  # dplyr::select(contains(c("depth", "press", "bathy", "elev")), everything()) |> # Look at depth columns
  # Remove empty columns
  mutate_if(is.character, ~na_if(., '')) |> 
  janitor::remove_empty("cols") |> 
  # Manually remove problematic files- no need
  # Manually remove problematic columns - no need
  # Manage lon/lat columns - no need
  # Manage date column
  dplyr::rename(date = `Date/Time`) |> 
  mutate(date = case_when(is.na(date) & !is.na(`Date/time start`) ~ `Date/time start`, 
                          is.na(date) & !is.na(`Date/time end`) ~ `Date/time end`,
                          is.na(date) & nchar(`Sampling date`) == 9 ~ sapply(str_split(`Sampling date`, "-"), "[[", 1),
                          is.na(date) & nchar(`Sampling date`) == 4 ~ `Sampling date`,
                          TRUE ~ date),
         date = case_when(nchar(date) == 4 ~ paste0(date,"-01-01"),
                          nchar(date) == 7 ~ paste0(date,"-01-01"),
                          TRUE ~ date),
         date = ifelse(date == "", NA, date)) |> 
  mutate(date = as.Date(gsub("T.*", "", date))) |>
  # Manage depth column
  mutate(depth = case_when(!is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Depth [m]`) ~ as.numeric(`Depth [m]`),
                           !is.na(`Depth [m] (maximum)`) ~ as.numeric(`Depth [m] (maximum)`),
                           !is.na(`Depth [m] (mbsf)`) ~ as.numeric(`Depth [m] (mbsf)`),
                           !is.na(`Depth [m] (in section/core)`) ~ as.numeric(`Depth [m] (in section/core)`),
                           !is.na(`Depth water [m] (min)`) ~ as.numeric(`Depth water [m] (min)`),
                           !is.na(`Depth water [m] (salt water)`) ~ as.numeric(`Depth water [m] (salt water)`))) |> 
  mutate(depth = case_when(is.na(depth) & !is.na(`Depth water [m] (approximate/target depth)`) ~ as.numeric(`Depth water [m] (approximate/target depth)`),
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -as.numeric(`Elevation [m a.s.l.]`),
                           is.na(depth) & !is.na(`Elevation [m a.s.l.] (Origin data)`) ~ -as.numeric(`Elevation [m a.s.l.] (Origin data)`),
                           is.na(depth) & !is.na(`Surf elev [m]`) ~ -as.numeric(`Surf elev [m]`),
                           TRUE ~ depth)) |> 
  # dplyr::select(depth, everything())
  # Remove unwanted columns
  dplyr::select(-contains(c("Longitude ", "Latitude ", "Elev "))) %>%
  # Finish up
  left_join(pg_meta_files, by = c("meta_idx", "site")) |> 
  dplyr::select(date_accessed, URL, citation, site, lon, lat, date, depth, everything()) |> 
  # NB: This must be changed manually when new data are loaded
  mutate(across(!dplyr::all_of(clean_cols), as.numeric)) |>  
  janitor::remove_empty("cols") |> 
  # NB: Site exists earlier to reflect data from different files for metadata joining
  mutate(site = "disko")
# colnames(pg_disko_clean)

# Individual category data.frames
pg_disko_cryo <- pg_var_melt(pg_disko_clean, query_cryo)
pg_disko_phys <- pg_var_melt(pg_disko_clean, query_phys)
pg_disko_chem <- pg_var_melt(pg_disko_clean, query_chem)
pg_disko_bio <- pg_var_melt(pg_disko_clean, query_bio)
pg_disko_soc <- pg_var_melt(pg_disko_clean, query_soc) # empty

# Stack them together
pg_disko_ALL <- rbind(pg_disko_cryo, pg_disko_phys, pg_disko_chem, pg_disko_bio, pg_disko_soc)
data.table::fwrite(pg_disko_ALL, "~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_ALL.csv")
save(pg_disko_ALL, file = "~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_ALL.RData")

# Check that all columns were used
# TODO: Follow up on some of these
colnames(pg_disko_clean)[!gsub("\\] \\(.*", "\\]", colnames(pg_disko_clean)) %in% unique(pg_disko_ALL$variable)]

# Clean up
rm(list = grep("pg_disko",names(.GlobalEnv),value = TRUE)); gc()


### Wild data ---------------------------------------------------------------

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
disko_wild <- rbind(disko_CTD_ChlA) |> mutate(site = "disko") |>
  left_join(full_var_list, by = c("category", "variable")) |> 
  dplyr::select(date_accessed, URL, citation, site, lon, lat, date, depth, category, driver, variable, value)
data.table::fwrite(disko_wild, "~/pCloudDrive/FACE-IT_data/disko_bay/disko_wild.csv")
save(disko_wild, file = "~/pCloudDrive/FACE-IT_data/disko_bay/disko_wild.RData")
rm(list = grep("disko_",names(.GlobalEnv),value = TRUE)); gc()


### GEM ---------------------------------------------------------------------

# NB: Several undownloaded wind data files as these were not used in the review paper

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
                   disko_GEM_historic_ts) |> mutate(site = "disko") |> 
  left_join(full_var_list, by = c("category", "variable")) |> 
  dplyr::select(date_accessed, URL, citation, site, lon, lat, date, depth, category, driver, variable, value)
save(disko_GEM, file = "data/restricted/disko_GEM.RData"); save(disko_GEM, file = "~/pCloudDrive/restricted_data/GEM/disko/disko_GEM.RData")
rm(list = grep("disko_GEM",names(.GlobalEnv),value = TRUE)); gc()


### Full product ------------------------------------------------------------

# Load full EU file
if(!exists("full_product_EU")) load("data/full_data/full_product_EU.RData")

# Subset to Disko Bay bbox and references for data missing lon/lat coords
disko_EU_sub <- filter_site_plural("disko", full_product_EU)

# Load Greenland data
if(!exists("full_product_green")) load("data/full_data/full_product_green.RData")

# Subset to relevant Disko Bay site names
# TODO: Still need to implement this

# Load GEM data and create shadow
if(!exists("disko_GEM")) load("~/pCloudDrive/restricted_data/GEM/disko/disko_GEM.RData")

# Create shadow
disko_GEM_shadow <- shadow(disko_GEM)

# Load GEM species data
## No GEM species data... that seems odd...

# Create shadow
# disko_species_GEM_shadow <- shadow(disko_species_GEM)

# Load PG product
if(!exists("pg_disko_ALL")) load("~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_ALL.RData")

# Load species data
## No non-GEM species data

# Load wild data
if(!exists("disko_wild")) load("~/pCloudDrive/FACE-IT_data/disko_bay/disko_wild.RData")

# Combine and save
full_product_disko <- rbind(disko_EU_sub, disko_GEM_shadow, pg_disko_ALL, disko_wild) |> mutate(site = "disko") |> distinct()
data.table::fwrite(full_product_disko, "~/pCloudDrive/FACE-IT_data/disko_bay/full_product_disko.csv")
save(full_product_disko, file = "~/pCloudDrive/FACE-IT_data/disko_bay/full_product_disko.RData")
save(full_product_disko, file = "data/full_data/full_product_disko.RData")
save_data(full_product_disko)
rm(list = grep("disko_",names(.GlobalEnv),value = TRUE)); gc()
# if(!exists("full_product_disko")) load("~/pCloudDrive/FACE-IT_data/disko_bay/full_product_disko.RData")


## Nuup Kangerlua ----------------------------------------------------------

### PG product --------------------------------------------------------------

# Load pg Nuup Kangerlua files
system.time(
  pg_nuup_sub <- plyr::ldply(pg_files, pg_site_filter, site_name = "nuup")
) # 27 seconds

# Test problem files
# pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
# pg_test <- pg_dl_proc(pg_doi = pg_nuup_all$doi[32])

# Remove unneeded columns
pg_nuup_clean <- pg_nuup_sub |> 
  # Look at specific meta columns
  # dplyr::select(contains(c("date", "lon", "lat")), everything()) |> # Look at meta columns
  # dplyr::select(contains(c("depth", "press", "bathy", "elev")), everything()) |> # Look at depth columns
  # Remove empty columns
  mutate_if(is.character, ~na_if(., '')) |> 
  janitor::remove_empty("cols") |> 
  # Manually remove problematic files - no need
  # Manage lon/lat columns - no issues
  # Manage date column -  no additional date column
  dplyr::rename(date = `Date/Time`) |> 
  mutate(date = case_when(nchar(date) == 4 ~ paste0(date,"-01-01"),
                          nchar(date) == 7 ~ paste0(date,"-01-01"),
                          TRUE ~ date),
         date = ifelse(date == "", NA, date)) |> 
  mutate(date = as.Date(gsub("T.*", "", date))) |>
  # Manage depth column
  dplyr::rename(`Elevation [m.a.s.l.]` = `Elevation [m a.s.l.] (= m.a.s.L(Fix) + (Mid(Fix)-Mi...)`) |> 
  mutate(depth = case_when(!is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Depth water [m] (min)`) ~ as.numeric(`Depth water [m] (min)`),
                           !is.na(`Depth water [m] (salt water)`) ~ as.numeric(`Depth water [m] (salt water)`))) |> 
  mutate(depth = case_when(is.na(depth) & !is.na(`Press [dbar]`) ~ as.numeric(`Press [dbar]`),
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -as.numeric(`Elevation [m a.s.l.]`),
                           is.na(depth) & !is.na(`Surf elev [m]`) ~ -as.numeric(`Surf elev [m]`),
                           is.na(depth) & !is.na(`Elevation [m]`) ~ -as.numeric(`Elevation [m]`),
                           is.na(depth) & !is.na(`Elevation [m.a.s.l.]`) ~ -as.numeric(`Elevation [m.a.s.l.]`),
                           TRUE ~ depth)) |> 
  # dplyr::select(depth, everything())
  # Remove unwanted columns  # Manually remove problematic columns
  dplyr::select(-contains(c("Date/", "Depth ", "Elevation ", "Elev ", "Press ", "Longitude ", "Latitude "))) %>%
  # Finish up
  left_join(pg_meta_files, by = c("meta_idx", "site")) |> 
  dplyr::select(date_accessed, URL, citation, site, lon, lat, date, depth, everything()) |> 
  # NB: This must be changed manually when new data are loaded
  mutate(across(!dplyr::all_of(clean_cols), as.numeric)) |>  
  janitor::remove_empty("cols") |> 
  # NB: Site exists earlier to reflect data from different files for metadata joining
  mutate(site = "nuup")
# colnames(pg_nuup_clean)

# Individual category data.frames
pg_nuup_cryo <- pg_var_melt(pg_nuup_clean, query_cryo)
pg_nuup_phys <- pg_var_melt(pg_nuup_clean, query_phys)
pg_nuup_chem <- pg_var_melt(pg_nuup_clean, query_chem)
pg_nuup_bio <- pg_var_melt(pg_nuup_clean, query_bio)
pg_nuup_soc <- pg_var_melt(pg_nuup_clean, query_soc) # empty

# Stack them together
pg_nuup_ALL <- rbind(pg_nuup_cryo, pg_nuup_phys, pg_nuup_chem, pg_nuup_bio, pg_nuup_soc)
data.table::fwrite(pg_nuup_ALL, "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/pg_nuup_ALL.csv")
save(pg_nuup_ALL, file = "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/pg_nuup_ALL.RData")

# Check that all columns were used
# TODO: Follow up on some of these
colnames(pg_nuup_clean)[!gsub("\\] \\(.*", "\\]", colnames(pg_nuup_clean)) %in% unique(pg_nuup_ALL$variable)]

# Clean up
rm(list = grep("pg_nuup",names(.GlobalEnv),value = TRUE)); gc()


### Wild data ---------------------------------------------------------------

# NB: Currently there are none
## Everything comes straight from PANGAEA or GEM


### GEM ---------------------------------------------------------------------

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
nuup_GEM <- rbind(nuup_GEM_CTD_open_water, nuup_GEM_pp, nuup_GEM_phyto_sp, nuup_GEM_silicate, nuup_GEM_ChlA, 
                  nuup_GEM_precip, nuup_GEM_snow, nuup_GEM_air_press, nuup_GEM_qnet, nuup_GEM_air_temp_2m, 
                  nuup_GEM_air_temp_10m, nuup_GEM_air_temp_2m_Kobbefjord, nuup_GEM_Kingigtorssuaq, nuup_GEM_Kobbefjord, 
                  nuup_GEM_Oriartorfik, nuup_GEM_Teqinngalip, nuup_GEM_nitrate_nitrite, nuup_GEM_fish_larvae, 
                  nuup_GEM_Slat_g, nuup_GEM_Slat_cm, nuup_GEM_Anod_g, nuup_GEM_Anod_cm) |> mutate(site = "nuup") |> 
  left_join(full_var_list, by = c("category", "variable")) |> 
  dplyr::select(date_accessed, URL, citation, site, lon, lat, date, depth, category, driver, variable, value)
save(nuup_GEM, file = "data/restricted/nuup_GEM.RData"); save(nuup_GEM, file = "~/pCloudDrive/restricted_data/GEM/nuup/nuup_GEM.RData")
rm(list = grep("nuup_GEM",names(.GlobalEnv),value = TRUE)); gc()


### GEM species ------------------------------------------------------------

# Bird presence
## Manque : Species
nuup_bird_nb <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/View_BioBasis_Nuuk_Data_Birds_Passerine_bird_abundance170420231432285653.csv") %>% 
  left_join(nuup_bird_coords, by = "Point") |> 
  mutate(date_accessed = as.Date("2023-04-17"),
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/DRTB-PY74",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         depth = NA,
         nomsp = map(Species, latin_eng),
         age = case_when(Age == "J"~"juvenile", 
                         Age == "A"~"adult",
                         Age == "UK"~"unknown"),
         gender = case_when(Gender == "M"~"male", 
                            Gender == "F"~"female",
                            Gender == "UK"~"unknown"),
         variable = paste0(nomsp, " ", age, " ", gender," [n]"),
         category = "bio",
         driver = "biomass",
         site = "nuup",
         value = Number) %>%
  dplyr::rename(date = Date) %>%
  dplyr::group_by(date_accessed, URL, citation, site, lon, lat, date, depth, category, driver, variable) %>% 
  # summarise data by day and variable
  dplyr::summarise(value = sum(value), .groups = "drop") %>% 
  filter(!value == 0)

# Seabird counting
nuup_seabird_count <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/View_MarineBasis_Nuuk_Data_Seabirds_Seabird_species_counts_per_colony17042023154835389.csv",
                                 na = c("NULL","-1", "2017-07-00")) %>% 
  filter(!is.na(Date)) %>% 
  filter(!is.na(Latin)) %>%
  filter(!is.na(MinNumbers)) %>%
  mutate(date_accessed = as.Date("2023-04-17"), 
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/WKFK-SS31", 
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Greenland Institute of Natural Resources, Nuuk, Greenland in collaboration with Department of Bioscience, Aarhus University, Denmark and University of Copenhagen, Denmark.", 
         lon = Longitude, 
         lat = Latitude, 
         depth = NA, 
         nomsp = map(Latin, latin_eng),
         variable = paste0(nomsp, " [n]"),
         category = "bio",
         driver = "biomass",
         site = "nuup", 
         date = as.Date(Date),
         moy = ceiling((MinNumbers+MaxNumbers)/2), # calculates the average between the minimum and maximum values
         value = ifelse(!is.na(moy), moy, MinNumbers)) %>% 
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value)

# Seabird presence
nuup_seabird_presence <- read_delim("~/pCloudDrive/restricted_data/GEM/nuup/View_MarineBasis_Nuuk_Data_Seabirds_Seabird_species_counts_per_colony17042023154835389.csv",
                                    na = "2017-07-00") %>% 
  filter(!is.na(Latin)) %>%
  mutate(date_accessed = as.Date("2023-04-17"), 
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/WKFK-SS31", 
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Greenland Institute of Natural Resources, Nuuk, Greenland in collaboration with Department of Bioscience, Aarhus University, Denmark and University of Copenhagen, Denmark.", 
         lon = Longitude, 
         lat = Latitude, 
         depth = NA, 
         nomsp = map(Latin, latin_eng),
         variable = paste0(nomsp, " [presence]"),
         category = "bio",
         driver ="biomass",
         site = "nuup", 
         date = as.Date(Date),
         value = ifelse(MinNumbers == 0, 0, 1)) %>% # change values to 1 if presence and 0 if absence
  filter(!nomsp == "NA") %>% 
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value)

# Marine mammal
## Not all the data of the set have been used
nuup_mmam_count <- read_delim("~/pCloudDrive//restricted_data/GEM/nuup/View_MarineBasis_Nuuk_Data_Marine_mammals_Identification_of_Humpback_Whales_individuals_year170420231542310109.csv") %>%
  mutate(date_accessed = as.Date("2023-04-17"), 
         URL = "https://data.g-e-m.dk/datasets?doi=10.17897/13YN-1209", 
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Greenland Institute of Natural Resources, Nuuk, Greenland in collaboration with Department of Bioscience, Aarhus University, Denmark and University of Copenhagen, Denmark.", 
         lon = NA, 
         lat = NA, 
         depth = NA, 
         Species = "Megaptera novaeangliae",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " [n]"),
         category = "bio",
         driver ="biomass",
         site = "nuup", 
         date = as.Date(paste0(YEAR,"-12-31"))) %>% 
  dplyr::rename(value = INDIVIDUALS) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, site, category, driver, variable, lon, lat, date, depth, value)

# Combine and save
nuup_species_GEM <- rbind(nuup_bird_nb, 
                          nuup_seabird_count, 
                          nuup_seabird_presence, 
                          nuup_mmam_count)
save(nuup_species_GEM, file = "~/pCloudDrive/restricted_data/GEM/nuup/nuup_species_GEM.RData")
write_csv(nuup_species_GEM, file = "~/pCloudDrive/restricted_data/GEM/nuup/nuup_species_GEM.csv")
rm(list = grep("nuup_",names(.GlobalEnv),value = TRUE))


### Full product ------------------------------------------------------------

# Load full EU file
if(!exists("full_product_EU")) load("data/full_data/full_product_EU.RData")

# Subset to Disko Bay bbox and references for data missing lon/lat coords
nuup_EU_sub <- filter_site_plural("nuup", full_product_EU)

# Load Greenland data
if(!exists("full_product_green")) load("data/full_data/full_product_green.RData")

# Subset to relevant Disko Bay site names
# TODO: Still need to implement this

# Load GEM data and create shadow
if(!exists("nuup_GEM")) load("~/pCloudDrive/restricted_data/GEM/nuup/nuup_GEM.RData")

# Create shadow
nuup_GEM_shadow <- shadow(nuup_GEM)

# Load GEM species data
if(!exists("nuup_species_GEM")) load("~/pCloudDrive/restricted_data/GEM/nuup/nuup_species_GEM.RData")

# Create shadow
nuup_species_GEM_shadow <- shadow(nuup_species_GEM)

# Load PG product
if(!exists("pg_nuup_ALL")) load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/pg_nuup_ALL.RData")

# Load species data
## No non-GEM species data

# Load wild data
## There are none

# Combine and save
full_product_nuup <- rbind(nuup_EU_sub, nuup_GEM_shadow, nuup_species_GEM_shadow, pg_nuup_ALL) |> mutate(site = "nuup") |> distinct()
data.table::fwrite(full_product_nuup, "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/full_product_nuup.csv")
save(full_product_nuup, file = "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/full_product_nuup.RData")
save(full_product_nuup, file = "data/full_data/full_product_nuup.RData")
save_data(full_product_nuup)
rm(list = grep("nuup_",names(.GlobalEnv),value = TRUE)); gc()
# if(!exists("full_product_nuup")) load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/full_product_nuup.RData")


## Norway ------------------------------------------------------------------

# NB: There is only the full data product for Norway
# This is a collection of social data

# National statistics
## Salmon exports
nor_salmon_exports_json <- 
  pxweb_get(url = "https://data.ssb.no/api/v0/en/table/03024/",
            query = "data/JSON/ssbapi_table_03024.json")
nor_salmon_exports <- as.data.frame(nor_salmon_exports_json,
                                    column.name.type = "text", variable.value.type = "text") %>% 
  dplyr::rename(Tonnes = `Weight (tonnes)`, `NOK/kg` = `Price per kilo (NOK)`) %>% 
  pivot_longer(cols = c(Tonnes, `NOK/kg`), names_to = "unit", values_to = "value") %>% 
  mutate(site = "nor",
         `commodity group` = str_replace(`commodity group`, ",", " -"),
         variable = paste0("Export - ",`commodity group`," [",unit,"]"),
         week = paste0(str_replace(week, "U","-"),"-1"),
         date = as.Date(week,"%Y-%U-%u"),
         date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = nor_salmon_exports_json$url,
         citation = px_cite(nor_salmon_exports_json)) %>% 
  mutate(date = case_when(is.na(date) ~ as.Date(paste0(substr(week, start = 1, stop = 4), "-12-31")),
                          TRUE ~ date)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site)

## Fish exports
nor_fish_exports_json <- 
  pxweb_get(url = "https://data.ssb.no/api/v0/en/table/08818/",
            query = "data/JSON/ssbapi_table_08818.json")
nor_fish_exports <- as.data.frame(nor_fish_exports_json,
                                  column.name.type = "text", variable.value.type = "text") %>% 
  dplyr::rename(value = Value, var = `commodity group`) %>% 
  separate(month, into = c("year", "month"), sep = "M") %>% 
  mutate(site = "nor",
         var = case_when(grepl("Fish, fresh", var) ~ "Fish - fresh or frozen",
                         grepl("Fish, dried", var) ~ "Fish - dried or salted or smoked",
                         grepl("whether in shell or", var) ~ "Other - fresh or frozen or dried or salted or smoked",
                         grepl("Fish, crustaceans", var) ~ "Fish or Other - prepared or preserved"),
         variable = paste0("Export - ",var," [NOK 1,000]"),
         date = as.Date(paste0(year,"-",month,"-01"))+months(1)-days(1),
         date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = nor_fish_exports_json$url,
         citation = px_cite(nor_fish_exports_json)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site)

## Birth rate
# https://www.ssb.no/en/befolkning/fodte-og-dode/statistikk/fodte

## Gender equality
# https://www.ssb.no/en/befolkning/likestilling/statistikk/indikatorer-for-kjonnslikestilling-i-kommunene

## Population projections
# https://www.ssb.no/en/befolkning/befolkningsframskrivinger

## Population
# https://www.ssb.no/en/statbank/table/01222 - More detailed stats
# https://www.ssb.no/en/statbank/table/06913 - Pop stats back to 1951 
# NB: This fails to be coerced to a data.frame
# nor_pop_json <- 
#   pxweb_get(url = "https://data.ssb.no/api/v0/en/table/05212/",
#             query = "data/JSON/ssbapi_table_05212.json")
# nor_pop <- as.data.frame(nor_pop_json, column.name.type = "text", variable.value.type = "text")
nor_pop_male_json <- 
  pxweb_get(url = "https://data.ssb.no/api/v0/en/table/05277/",
            query = "data/JSON/ssbapi_table_05277_male.json")
nor_pop_female_json <- 
  pxweb_get(url = "https://data.ssb.no/api/v0/en/table/05277/",
            query = "data/JSON/ssbapi_table_05277_female.json")
nor_pop <- rbind(as.data.frame(nor_pop_male_json),
                      as.data.frame(nor_pop_female_json)) %>% 
  dplyr::rename(value = Population, site = region) %>% 
  mutate(variable = paste0("Population - ",age," - ",sex," [n]"),
         date = as.Date(paste0(year,"-12-31")),
         date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = nor_pop_male_json$url,
         citation = px_cite(nor_pop_male_json)) %>% 
  filter(!is.na(value)) %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable, site) %>% 
  # NB: There appear to be three measurements per year.
  # So we choose the highest pop value in a given year
  summarise(value = max(value, na.rm = T), .groups = "drop")

## Svalbard population
sval_pop_json <- 
  pxweb_get(url = "https://data.ssb.no/api/v0/en/table/07429/",
            query = "data/JSON/ssbapi_table_07429.json")
sval_pop <- as.data.frame(sval_pop_json) %>% 
  dplyr::rename(value = Persons) %>% 
  separate(`half year`, into = c("year", "half"), sep = "H") %>% 
  mutate(site = "Longyearbyen & Ny-Alesund",
         variable = paste0("Population - ",age," - ",sex," [n]"),
         date = case_when(half == "1" ~ as.Date(paste0(year,"-06-30")),
                          half == "2" ~ as.Date(paste0(year,"-12-31"))),
         date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = sval_pop_json$url,
         citation = px_cite(sval_pop_json)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site)

## MPAs
nor_MPA_json <- 
  pxweb_get(url = "https://data.ssb.no/api/v0/en/table/08936/",
            query = "data/JSON/ssbapi_table_08936.json")
nor_MPA <- as.data.frame(nor_MPA_json) %>% 
  dplyr::rename(`[km^2]` = `Protected area at sea (km²)`, `[n]` = `Number of protected areas total`, site = region) %>% 
  pivot_longer(col = c(`[km^2]`, `[n]`), names_to = "unit", values_to = "value") %>% 
  mutate(variable = paste0(`protection purpose`," ",unit),
         date = as.Date(paste0(year,"-12-31")),
         date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = nor_MPA_json$url,
         citation = px_cite(nor_MPA_json)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site)

## Waste per household
# https://www.ssb.no/en/statbank/table/12241
# https://www.ssb.no/en/statbank/table/13136

## Air passengers
nor_air_passenger_json <- 
  pxweb_get(url = "https://data.ssb.no/api/v0/en/table/08507/",
            query = "data/JSON/ssbapi_table_08507.json")
nor_air_passenger <- as.data.frame(nor_air_passenger_json) %>% 
  dplyr::rename(value = Passengers, site = airport, 
                var = `passenger group`, var2 = `domestic/international flights`) %>% 
  separate(month, into = c("year", "month"), sep = "M") %>% 
  mutate(var = case_when(grepl("Total passengers", var) ~ "Passengers - total",
                         grepl("arrival", var) ~ "Passengers - arrival",
                         grepl("departure", var) ~ "Passengers - departure"),
         var2 = case_when(grepl("Domestic and international", var2) ~ "All flights", TRUE ~ var2),
         variable = paste0(var," - ",var2," [n]"),
         date = as.Date(paste0(year,"-",month,"-01"))+months(1)-days(1),
         date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = nor_air_passenger_json$url,
         citation = px_cite(nor_air_passenger_json)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site)

## Transport - for consideration
# https://www.ssb.no/en/transport-og-reiseliv/landtransport/statistikk/innenlandsk-transport
# https://www.ssb.no/en/statbank/table/08923
# https://www.ssb.no/en/statbank/table/09495

## Employment - No fisheries specific data
# https://www.ssb.no/en/statbank/table/12817
# https://www.ssb.no/en/statbank/table/13472

## Svalbard employment sector %
sval_employ_perc_json <- 
  pxweb_get(url = "https://data.ssb.no/api/v0/en/table/09715/",
            query = "data/JSON/ssbapi_table_09715.json")
sval_employ_perc <- as.data.frame(sval_employ_perc_json) %>% 
  dplyr::rename(value = `Share of total employment on Svalbard`, variable = sector) %>% 
  mutate(site = "sval",
         variable = paste0("Employment - ",variable," [% share]"),
         date = as.Date(paste0(year,"-12-31")),
         date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = nor_MPA_json$url,
         citation = px_cite(nor_MPA_json)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site)

## Unemployment
# https://www.ssb.no/en/statbank/table/11587
# https://www.ssb.no/en/statbank/table/08930

## Earnings
# https://www.ssb.no/en/statbank/table/11419

## Tourism - Nothing clearly needed
# https://www.ssb.no/en/nasjonalregnskap-og-konjunkturer/nasjonalregnskap/statistikk/satellittregnskap-for-turisme
# https://www.ssb.no/en/statbank/table/10638

## Accommodation
nor_accommodation_json <- 
  pxweb_get(url = "https://data.ssb.no/api/v0/en/table/12892/",
            query = "data/JSON/ssbapi_table_12892.json")
nor_accommodation <- as.data.frame(nor_accommodation_json) %>% 
  dplyr::rename(value = `Guest nights`, site = region, 
                var1 = `type of accommodation`, var2 = `country of residence`) %>% 
  separate(month, into = c("year", "month"), sep = "M") %>% 
  mutate(var2 = str_replace(var2, ",", " -"),
         variable = paste0("Guest nights - ",var1," - ",var2," [n]"),
         date = as.Date(paste0(year,"-",month,"-01"))+months(1)-days(1),
         date_accessed = as.Date(Sys.Date()),
         category = "soc", lon = NA, lat = NA, depth = NA, URL = nor_accommodation_json$url,
         citation = px_cite(nor_accommodation_json)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site)

# Fisheries data
## IMR red king crab survey data
nor_IMR_kingcrab_count <- read_delim("~/pCloudDrive/FACE-IT_data/norway/IMR/dwca-imr_kingcrab-v1.2/measurementorfact.txt")
nor_IMR_kingcrab <- read_delim("~/pCloudDrive/FACE-IT_data/norway/IMR/dwca-imr_kingcrab-v1.2/occurrence.txt") %>% 
  left_join(nor_IMR_kingcrab_count, by = "id") %>% 
  unite(year, month, day, sep = "-", remove = T, col = "date") %>% 
  dplyr::rename(lon = decimalLongitude, lat = decimalLatitude, variable = scientificName, value = measurementValue) %>% 
  mutate(variable = paste0(variable," [count]")) %>% 
  rowwise() %>% 
  mutate(depth = mean(c(minimumDepthInMeters, maximumDepthInMeters), na.rm = T)) %>% 
  dplyr::select(lon, lat, date, depth, variable, value) %>%
  distinct() %>% 
  mutate(site = "Barents Sea",
         date = as.Date(date),
         depth = case_when(is.na(depth) ~ as.numeric(NA), TRUE ~ depth),
         date_accessed = as.Date("2022-11-14"),
         URL = "https://gbif.imr.no/ipt/resource?r=imr_kingcrab",
         citation = "Hjelset, Ann Merete; Institute of Marine Research, Norway (2017): Red king crab survey data from Finnmark Northern Norway in the period 1994 -2016 http://gbif.imr.no/ipt/resource?id=imr_kingcrab/v1.2.xml",
         category = "bio") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable, site) %>% 
  summarise(value = sum(value, na.rm = T), .groups = "drop")
rm(nor_IMR_kingcrab_count); gc()

## Polar cod biomass in the Barents Sea
nor_MOSJ_pcod <- read_delim("~/pCloudDrive/FACE-IT_data/norway/biomass-of-polar-cod-in-the-barents-sea.csv", ";") %>% 
  pivot_longer(cols = `1986`:`2021`, values_to = "value", names_to = "year") %>% 
  distinct() %>% 
  mutate(site = "Barents Sea",
         variable = "Biomass - Polar Cod [10^6 kg]",
         date = as.Date(paste0(year,"-12-31")),
         depth = NA, lon = NA, lat = NA,
         date_accessed = as.Date("2022-11-17"),
         URL = "https://www.mosj.no/en/fauna/marine/polar-cod.html",
         citation = "Institute of Marine Research (2022). Biomass of polar cod in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/polar-cod.html",
         category = "bio") %>% 
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, category, variable, value, site)

# Combine and save
full_product_nor <- rbind(nor_salmon_exports, nor_fish_exports, nor_pop, sval_pop, nor_MPA, 
                          nor_air_passenger, sval_employ_perc, nor_accommodation, 
                          nor_IMR_kingcrab, nor_MOSJ_pcod) |>
  left_join(full_var_list, by = c("category", "variable")) |> 
  dplyr::select(date_accessed, URL, citation, site, lon, lat, date, depth, category, driver, variable, value)
data.table::fwrite(full_product_nor, "~/pCloudDrive/FACE-IT_data/norway/full_product_nor.csv")
save(full_product_nor, file = "~/pCloudDrive/FACE-IT_data/norway/full_product_nor.RData")
save(full_product_nor, file = "data/full_data/full_product_nor.RData")
save_data(full_product_nor) # NB: Shouldn't save anything because no matching site names to FACE-IT 7
rm(list = grep("nor_|sval_",names(.GlobalEnv),value = TRUE)); gc()


## Porsangerfjorden ---------------------------------------------------------

### PG product --------------------------------------------------------------

# Load pg is files
system.time(
  pg_por_sub <- plyr::ldply(pg_files, pg_site_filter, site_name = "por")
) # 24 seconds

# Test problem files
# pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
# pg_test <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.869680")

# Remove unneeded columns
pg_por_clean <- pg_por_sub |> 
  # dplyr::select(contains(c("date", "lon", "lat")), everything()) |> # Look at meta columns
  # dplyr::select(contains(c("depth", "press", "bathy", "elev")), everything()) |> # Look at depth columns
  # Remove empty columns
  mutate_if(is.character, ~na_if(., '')) |> 
  janitor::remove_empty("cols") |> 
  # Manually remove problematic columns - no need
  # Manage lon/lat columns - no need
  # Manage date column
  dplyr::rename(date = `Date/Time`) |> 
  mutate(date = ifelse(date == "", NA, date),
         date = case_when(nchar(date) == 4 ~ paste0(date,"-01-01"),
                          nchar(date) == 7 ~ paste0(date,"-01"),
                          # is.na(date) & !is.na(`Date/time start`) ~ as.character(`Date/time start`), 
                          is.na(date) & !is.na(`Date/time end`) ~ as.character(`Date/time end`),
                          TRUE ~ date)) |> 
  mutate(date = as.Date(gsub("T.*", "", date))) |> 
  # Manage depth column
  dplyr::rename(depth = `Depth water [m]`) |> 
  mutate(depth = as.numeric(depth),
         depth = case_when(is.na(depth) & !is.na(`Depth [m]`) ~ as.numeric(`Depth [m]`),
                           is.na(depth) & !is.na(`Press [dbar]`) ~ as.numeric(`Press [dbar]`),
                           TRUE ~ depth)) |> 
  mutate(depth = case_when(is.na(depth) & !is.na(`Elevation [m]`) ~ -as.numeric(`Elevation [m]`),
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -as.numeric(`Elevation [m a.s.l.]`),
                           TRUE ~ depth)) |> 
  # dplyr::select(depth, everything())
  # Remove unwanted columns
  dplyr::select(-contains(c("Depth ", "Elevation ", "Press "))) |> 
  # Finish up
  left_join(pg_meta_files, by = c("meta_idx", "site")) |> 
  dplyr::select(date_accessed, URL, citation, site, lon, lat, date, depth, everything(), -meta_idx) |> 
  # Manually remove problematic files - no need
  mutate(across(!dplyr::all_of(clean_cols), as.numeric)) |>  
  janitor::remove_empty("cols") |> 
  # NB: Site exists earlier to reflect data from different files for metadata joining
  mutate(site = "por")
# colnames(pg_por_clean)

# Individual category data.frames
pg_por_cryo <- pg_var_melt(pg_por_clean, query_cryo)
pg_por_phys <- pg_var_melt(pg_por_clean, query_phys)
pg_por_chem <- pg_var_melt(pg_por_clean, query_chem)
pg_por_bio <- pg_var_melt(pg_por_clean, query_bio)
pg_por_soc <- pg_var_melt(pg_por_clean, query_soc) # empty

# Stack them together
pg_por_ALL <- rbind(pg_por_cryo, pg_por_phys, pg_por_chem, pg_por_bio, pg_por_soc)
data.table::fwrite(pg_por_ALL, "~/pCloudDrive/FACE-IT_data/porsangerfjorden/pg_por_ALL.csv")
save(pg_por_ALL, file = "~/pCloudDrive/FACE-IT_data/porsangerfjorden/pg_por_ALL.RData")

# Check that all columns were used
# TODO: Follow up on some of these
colnames(pg_por_clean)[!gsub("\\] \\(.*", "\\]", colnames(pg_por_clean)) %in% unique(pg_por_ALL$variable)]

# Clean up
rm(list = grep("pg_por",names(.GlobalEnv),value = TRUE)); gc()


### Species data ------------------------------------------------------------

# NB: Currently there are none


### Wild data ---------------------------------------------------------------

# NorKyst-800 physical model
por_NorKyst <- data.frame(date_accessed = NA, 
                          URL = "https://thredds.met.no/thredds/fou-hi/fou-hi.html", 
                          citation = "Albretsen, J., Sperrevik, A. K., Staalstrøm, A., Sandvik, A. D., Vikebø, F., & Asplin, L. (2011). NorKyst-800 Rapport nr. 1: Brukermanual og tekniske beskrivelser.", 
                          site = "por", 
                          lon = NA, 
                          lat = NA, 
                          date = NA, 
                          depth = NA,
                          category = c("phys", "cryo"), 
                          driver = c("sea temp", "sea ice"), 
                          variable = c("temp [°C]", "sea ice thickness"))

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

# Combine and save
por_wild <- rbind(por_mooring_GFI, por_sea_ice, por_hydro) |> mutate(site = "por") |> distinct() |> 
  left_join(full_var_list, by = c("category", "variable")) |> 
  dplyr::select(date_accessed, URL, citation, site, lon, lat, date, depth, category, driver, variable, value)
data.table::fwrite(por_wild, "~/pCloudDrive/FACE-IT_data/porsangerfjorden/por_wild.csv")
save(por_wild, file = "~/pCloudDrive/FACE-IT_data/porsangerfjorden/por_wild.RData")
rm(list = grep("por_",names(.GlobalEnv),value = TRUE)); gc()


### Full product ------------------------------------------------------------

# Load full EU file
if(!exists("full_product_EU")) load("data/full_data/full_product_EU.RData")

# Subset to Porsangerfjorden bbox and references for data missing lon/lat coords
por_EU_sub <- filter_site_plural("por", full_product_EU)

# Load full Norway file
if(!exists("full_product_nor")) load("data/full_data/full_product_nor.RData")

# Subset to Porsangerfjorden bbox and references for data missing lon/lat coords
# TODO: Improve this to catch and convert provence etc. site names
por_nor_sub <- filter_site_plural("por", full_product_nor)

# Load PG product
if(!exists("pg_por_ALL")) load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/pg_por_ALL.RData")

# Load species data
## Currently none

# Load wild data
if(!exists("por_wild")) load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/por_wild.RData")

# Combine and save
full_product_por <- rbind(por_EU_sub, por_nor_sub, pg_por_ALL, por_wild) |> mutate(site = "por") |> distinct()
data.table::fwrite(full_product_por, "~/pCloudDrive/FACE-IT_data/porsangerfjorden/full_product_por.csv")
save(full_product_por, file = "~/pCloudDrive/FACE-IT_data/porsangerfjorden/full_product_por.RData")
save(full_product_por, file = "data/full_data/full_product_por.RData")
save_data(full_product_por)
rm(list = grep("por_",names(.GlobalEnv),value = TRUE)); gc()
# if(!exists("full_product_por")) load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/full_product_por.RData")


# Driver oriented data products -------------------------------------------

# It is here that the data are oriented by drivers rather than site

# NB: Individual clean_*_all.csv are available at ~/WP1/data/full_data/


## Site data ---------------------------------------------------------------
# Re-load site data a necessary

# FACE-IT collected data
if(!exists("full_product_kong")) load("~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")
if(!exists("full_product_is")) load("~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.RData")
if(!exists("full_product_stor")) load("~/pCloudDrive/FACE-IT_data/storfjorden/full_product_stor.RData")
if(!exists("full_product_young")) load("~/pCloudDrive/FACE-IT_data/young_sound/full_product_young.RData")
if(!exists("full_product_disko")) load("~/pCloudDrive/FACE-IT_data/disko_bay/full_product_disko.RData")
if(!exists("full_product_nuup")) load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/full_product_nuup.RData")
if(!exists("full_product_por")) load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/full_product_por.RData")

# Extra data by site name only in EU, sval, green, and nor files
if(!exists("full_product_EU")) load("~/pCloudDrive/FACE-IT_data/EU_arctic/full_product_EU.RData")
if(!exists("full_product_sval")) load("~/pCloudDrive/FACE-IT_data/svalbard/full_product_sval.RData")
if(!exists("full_product_green")) load("~/pCloudDrive/FACE-IT_data/greenland/full_product_green.RData")
if(!exists("full_product_nor")) load("~/pCloudDrive/FACE-IT_data/norway/full_product_nor.RData")

# GEM data
if(!exists("young_GEM")) load("~/pCloudDrive/restricted_data/GEM/young/young_GEM.RData")
if(!exists("disko_GEM")) load("~/pCloudDrive/restricted_data/GEM/disko/disko_GEM.RData")
if(!exists("nuup_GEM")) load("~/pCloudDrive/restricted_data/GEM/nuup/nuup_GEM.RData")

# GEM species data
if(!exists("young_species_GEM")) load("~/pCloudDrive/restricted_data/GEM/young/young_species_GEM.RData")
if(!exists("nuup_species_GEM")) load("~/pCloudDrive/restricted_data/GEM/nuup/nuup_species_GEM.RData")

# Combine everything into one mega-file for ease of use below
# This also ensures a project-wide standard has been maintained
full_ALL <- rbind(full_product_kong, full_product_is, full_product_stor,
                  full_product_young, full_product_disko, full_product_nuup,
                  full_product_por,
                  full_product_EU, full_product_sval, full_product_green, full_product_nor,
                  young_GEM, disko_GEM, nuup_GEM,
                  young_species_GEM, nuup_species_GEM) |> 
  mutate(type = "in situ")


## Cryosphere --------------------------------------------------------------

# Find cryosphere category data with no assigned driver
miss_cryo <- cat_driver_miss(full_ALL, "cryo")

# NB: Actively choosing not to include Geyman et al. variables with clean cryo data
# This is because they represent large time spans per value, so aren't well suited here
miss_cryo <- filter(miss_cryo, !grepl("Geyman", citation))

# TODO: Think about how to scan the full data for missing drivers
# and create a system that reincorporates them into full_var_list
# Or perhaps rather use this as a catch and then go back above and 
# correct the variable names to match with existing variables
# And when that doesn't work, then add new ones to 'full_var_list'

# Separate out missing drivers into groups
# miss_sea_ice <- filter(miss_cryo)


### Sea ice ----------------------------------------------------------------

## Notes on strange variables
# https://doi.org/10.1594/PANGAEA.935267; bi [code] = ice of land origin, ci [code] = sea ice concentration, zi [code] = ice situation
# https://doi.org/10.1594/PANGAEA.269605; t [°C] = temperature ice/snow
# https://doi.org/10.1594/PANGAEA.269619; DI [code] = bearing of principal ice edge, l_s [code] = type of ice accretion
# https://doi.org/10.1594/PANGAEA.935267; EsEs [m] = sea ice thickness, EsEs acc [cm] = thickness of ice accretion
# https://doi.org/10.1594/PANGAEA.896581; RGI 6.0 ID = Randolph glacier inventory, SWE [m] = snow water equivalent, SWE unc [m] - Uncertainty
# https://doi.org/10.1594/PANGAEA.869294; IP [km**3/day] = sea ice production
# https://doi.org/10.1594/PANGAEA.908494; SIC d [months/a] = Sea ice cover duration NB: This file is a good candidate for checking pipeline errors
# https://doi.org/10.1594/PANGAEA.815951; Glac w [km] = Glacier width
# https://doi.org/10.1594/PANGAEA.59224; IRD [arbitrary units] = Ice rafted debris, general

# Get sea ice data from full datasets
clean_sea_ice <- filter(full_ALL, driver == "sea ice") |> 
  filter(depth >= 0 | is.na(depth)) |> # For now it is necessary to reappoint some data here to the glacier file
  mutate(depth = NA, type = "in situ") |>  # Deeper depths are bottom depths and should be converted to NA
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) |> 
  distinct()

## Not a lot of common sea ice data between sites
## The gridded data sea ice cover will be the best comparison between sites
## Created in code/data_processing.R
load("data/analyses/ice_4km_proc.RData")
ice_4km_prop_wide <- plyr::ddply(ice_4km_proc, c("site"), ice_cover_prop, .parallel = T)

# Calculate trends
# NB: While interesting, this is too detailed to be included in the clean data
# ice_4km_trend <- plyr::ddply(dplyr::rename(ice_4km_prop, val = mean_prop), c("site", "month"), trend_calc, .parallel = T)
# ice_4km_trend_long <- ice_4km_trend %>% 
#   pivot_longer(trend:sd_val, names_to = "variable") %>% 
#   mutate(variable = case_when(variable == "trend" ~ paste0("sea ice cover ",month," [annual proportion trend]"),
#                               variable == "p.value" ~ paste0("sea ice cover ",month," [annual proportion trend p-value]"),
#                               variable == "mean_val" ~ paste0("sea ice cover ",month," [mean proportion]"),
#                               variable == "sd_val" ~ paste0("sea ice cover ",month," [SD proportion]"))) %>% 
#   dplyr::select(-month)

# Combine with other clean data
ice_4km_prop <- ice_4km_prop_wide %>%
  dplyr::rename(value = mean_prop) %>% 
  dplyr::select(date, value, site) %>% 
  mutate(variable = "sea ice cover [proportion]",
         type = "MASIE", depth = NA, lon = NA, lat = NA,
         category = "cryo", driver = "sea ice",
         date_accessed = as.Date("2022-04-26"),
         URL = "https://doi.org/10.7265/N5GT5K3K",
         citation = "U.S. National Ice Center and National Snow and Ice Data Center. Compiled by F. Fetterer, M. Savoie, S. Helfrich, and P. Clemente-Colón. (2010), updated daily. Multisensor Analyzed Sea Ice Extent - Northern Hemisphere (MASIE-NH), Version 1. 4km resolution. Boulder, Colorado USA. NSIDC: National Snow and Ice Data Center. doi: https://doi.org/10.7265/N5GT5K3K.") |> 
  dplyr::select(date_accessed, URL, citation, site, type, category, driver, variable, lon, lat, date, depth, value)
save(ice_4km_prop, file = "data/analyses/ice_4km_prop.RData")

# Bind together
clean_sea_ice <- rbind(clean_sea_ice, ice_4km_prop_long) %>% distinct()
rm(ice_4km_proc, ice_4km_prop, ice_4km_prop_long); gc(); print(unique(clean_sea_ice$variable))

# Calculate sea ice breakup and formation dates
## Not sure if this is useful/comparable for all the different sites. e.g. Young Sound vs. Disko Bay
## Consider calculating open water days


### Glacier -----------------------------------------------------------------

# Get height data from sea ice driver
# TODO: Fix this above so this step isn't necessary
# Or perhaps keep it as it is another approach to parsing sea ice and glacier ice data from each other
clean_glacier_sea_ice <- filter(full_ALL, driver == "sea ice", depth < 0) |> mutate(driver = "glacier")
clean_glacier <- filter(full_ALL, driver == "glacier") |> rbind(clean_glacier_sea_ice) |> mutate(type = "in situ") |> 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) |> 
  distinct()
rm(clean_glacier_sea_ice); gc(); print(unique(clean_glacier$variable))

# NB: Not using this legacy code at the moment, this was for the data paper
# Summary analyses
# summary_glacier <- review_summary(clean_glacier)
# Plot results
# review_summary_plot(summary_glacier, "glacier")


### Runoff ------------------------------------------------------------------

# Pedro Duarte has contacted a colleague to get Kongsfjorden area river discharge data

# GRDC river discharge data
## NB: These are restricted data so they are not added to 'full_product_EU'
# lta_discharge = long-term average discharge, cubic metre per sec
# r_vol_yr = mean annual volume, cubic kilometre
# r_height_yr	= mean annual runoff depth, mm
EU_GRDC <- read_csv("~/pCloudDrive/restricted_data/GRDC/grdc_arctichycos_stations.csv")
site_GRDC <- map_dfr(dir("~/pCloudDrive/restricted_data/GRDC", pattern = "Cmd.txt", full.names = T), load_GRDC)

# Get river data from GRDC database
FACE_IT_GRDC <- site_GRDC %>% 
  mutate(site = case_when(lon >= bbox_kong[1] & lon <= bbox_kong[2] & lat >= bbox_kong[3] & lat <= bbox_kong[4] ~ "kong",
                          lon >= bbox_is[1] & lon <= bbox_is[2] & lat >= bbox_is[3] & lat <= bbox_is[4] ~ "is",
                          lon >= bbox_stor[1] & lon <= bbox_stor[2] & lat >= bbox_stor[3] & lat <= bbox_stor[4] ~ "stor",
                          lon >= bbox_young[1] & lon <= bbox_young[2] & lat >= bbox_young[3] & lat <= bbox_young[4] ~ "young",
                          lon >= bbox_disko[1] & lon <= bbox_disko[2] & lat >= bbox_disko[3] & lat <= bbox_disko[4] ~ "disko",
                          lon >= bbox_nuup[1] & lon <= bbox_nuup[2] & lat >= bbox_nuup[3] & lat <= bbox_nuup[4] ~ "nuup",
                          lon >= bbox_por[1] & lon <= bbox_por[2] & lat >= bbox_por[3] & lat <= bbox_por[4] ~ "por")) %>% 
  filter(!is.na(site)) %>% 
  pivot_longer(`Q [m3/s]`, names_to = "variable") %>% 
  mutate(date_accessed = as.Date("2022-06-13"), type = "in situ", category = "cryo", driver = "runoff",
         URL = "https://www.bafg.de/GRDC/EN/04_spcldtbss/41_ARDB/ardb_node.html", 
         citation = "Arctic Region Discharge Data (2021). The Global Runoff Data Centre, 56068 Koblenz, Germany") %>% 
  dplyr::select(date_accessed, URL, citation, site, type, category, driver, variable, lon, lat, date, depth, value)

# Get all river discharge data from full/GEM products and combine with GRDC
clean_runoff <- filter(full_ALL, driver == "runoff") |> mutate(type = "in situ") |> 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) |> 
  distinct() |> rbind(FACE_IT_GRDC)
rm(EU_GRDC, site_GRDC, FACE_IT_GRDC); gc(); print(unique(clean_runoff$variable))


## Physics ----------------------------------------------------------------

# TODO: Assign missing variables to drivers
# Find physical category data with no assigned driver
miss_phys <- cat_driver_miss(full_ALL, "phys")

# Many of these will be very simple
# Perhaps better to go back through datasets above and correct the variables there


### Sea temp ----------------------------------------------------------------

# TODO: Look into temperature values above 20°C
# TODO: Still need to clean up these values

# Load OISST and CCI data
# Prepared in code/data_processing.R
if(!exists("OISST_all")) load("data/analyses/OISST_all.RData")
if(!exists("CCI_all")) load("data/analyses/CCI_all.RData")

## Notes on temperature variables
# Remove air, CO2, and pH related temperature values
# TTT is air temperature from cruise data on PANGAEA. e.g. https://doi.pangaea.de/10.1594/PANGAEA.326679
# MAAT + MAGT = ground temperatures e.g. https://doi.pangaea.de/10.1594/PANGAEA.808512
# MAT = mean annual temperature e.g. https://doi.pangaea.de/10.1594/PANGAEA.907818
# Remove overly processed variables (e.g. average summer SST)
# Remove slightly different variables
# tequ = temperature at equilibrium; ~+0.6°C than the corresponding water temp
# e.g. https://doi.pangaea.de/10.1594/PANGAEA.849863
# T intern [°C] = internal temperature; ~+0.03°C than the corresponding water temp
# e.g. https://doi.pangaea.de/10.1594/PANGAEA.930028
# Removing tpot (Potential temperature) is a potentially controversial decision...
# t [°C] = ground/snow temperatures e.g. https://doi.pangaea.de/10.1594/PANGAEA.930472
# T tech [°C] + T cal [°C] = Temperatures from an experiment e.g. https://doi.pangaea.de/10.1594/PANGAEA.847626
clean_sea_temp <- filter(full_ALL, driver == "sea temp") |> mutate(type = "in situ") |> 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) |> 
  distinct() |> rbind(OISST_all, CCI_all) |> depth_bin_average() |> 
  mutate(variable = case_when(variable == "Temp [°C]" ~ "temp [°C]", TRUE ~ variable)) |> 
  filter(variable == "temp [°C]")
rm(OISST_all, CCI_all); gc(); print(unique(clean_sea_temp$variable))


### Salinity ---------------------------------------------------------------

# TODO: A couple of odd values

## Notes on salinity
# Remove Sal [mg/l]
# Remove overly processed variables
# sal interp e.g. https://doi.org/10.1594/PANGAEA.877869
# Remove glacial drainage land stations
# variable = case_when(variable %in% c("s_fb [unit]") ~ "sal_pco2", # Salinity for pCO2 analyses
clean_salinity <- filter(full_ALL, driver == "salinity") |> mutate(type = "in situ") |> filter(value > 0) |> 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) |> 
  distinct() |> depth_bin_average()
print(unique(clean_salinity$variable))


### Light ------------------------------------------------------------------

# TODO: Look into variables

# Get all PAR+UV data
clean_light <- filter(full_ALL, driver == "light") |> mutate(type = "in situ") |> filter(value > 0) |> 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) |> 
  distinct()
  #        !grepl("volt", variable)) %>%
  # mutate(value = case_when(str_detect(variable, "mmol") ~ value/1000, TRUE ~ value),
  #        variable = case_when(str_detect(variable, "PAR|par") ~ "PAR [µmol m-2 s-1]",
  #                             str_detect(variable, "UVA") ~ "UV-A [W*m^2]", # TODO: Keep or remove?
  #                             str_detect(variable, "UVB") ~ "UV-B [W*m^2]",
  #                             TRUE ~ variable), driver = "light")
print(unique(clean_light$variable))


## Chemistry ---------------------------------------------------------------

# TODO: Assign missing variables to drivers
# Find physical category data with no assigned driver
miss_chem <- cat_driver_miss(full_ALL, "chem")


### Carb -------------------------------------------------------------------

# TODO: Sort out the variable conversions etc.
# Bring DIC back into dataset
# pH is not always the same, there are different scales with differences of up to 0.2
## It requires expert knowledge and review of each citation to determine the provenence of the pH scale...
# Difference in measured vs calculated pCO2, and difference in SST and normalised temperature

# From Liqing Jiang:
# I like the idea of adding the carbon parameter pair used to conduct the CO2 system calculation to the variable name. 
# After all, they could have different associated uncertainties. 
# For data submission purposes, please feel free to use these new names as you suggested. 

# Keep pCO2_calc as a separate variable because they can't be taken as absolutely the same
# Same for PCO2water_SST_wet
# Can use SeaCarb to transform fco2 to pCO2
# Note that there are duplicates from GLODAP and the underlying files downloaded via PANGAEA
# But this is actually a good thing as it allows us to acknowledge specific contributors,
# which is something that the GLODAP product requests that we do.
clean_carb <- filter(full_ALL, driver == "carb") |>  
  filter(!variable %in% c("pH_dur [total scale]", "ph_s_sf_t_insi [total scale]", 
                          "ph_s_dur_t_fb [total scale]", "pH_calc [total scale]", "phts25p0")) |> 
  mutate(value = case_when(variable == "AT [mmol(eq)/l]" ~ value*1000, TRUE ~ value), # Convert to µmol/l
         variable = case_when(variable %in% c("AT [mmol(eq)/l]", "AT [µmol/kg]",
                                              "talk [μmol kg-1]") ~ "TA [µmol/kg]",
                              variable %in% c("pco2 [uatm]") ~ "pCO2 [µatm]", 
                              variable %in% c("pCO2water_SST_wet [uatm]") ~ "pCO2water_SST_wet [µatm]",
                              variable %in% c("pco2_calc [uatm]") ~ "pCO2_calc [µatm]",
                              variable %in% c("pH_sf [total scale]", "pHT in situ", "phtsinsitutp") ~ "pH in situ [total scale]",
                              variable %in% c("pH") ~ "pH [unknown scale]",
                              TRUE ~ variable),
         type = "in situ") |> filter(value > 0) |> 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) |> 
  distinct()
# unique(clean_carb$variable[str_detect(clean_carb$variable, "ph|pH")]) # Double check that only pH values are screened this way
# unique(clean_carb$variable)
# test_df <- dplyr::select(clean_carb, citation, variable) %>% distinct() %>% filter(str_detect(variable, "ph|pH"))
# test_df <- filter(clean_carb, variable == "phtsinsitutp")
print(unique(clean_carb$variable))


### Nutrients ---------------------------------------------------------------

# TODO: Create report showing difference in GLODAP l and kg values

## Notes on nutrients
# [µmol/l] is the same as [µg-at/l]
# [µmol/l] vs [μmol kg-1] are different, a conversion should be made between them, but they appear to be used interchangeably
# Keep Nitrate + Nitrite
# Same same:
  # - [NO2]- vs NO2
  # - PO4 vs [PO4]3-

# Get all nutrient data
clean_nutrients <- filter(full_ALL, driver == "nutrients") %>% 
  filter(value > 0) %>%
  filter(variable != "NO3 [µmol/kg]") %>% # TODO: Fix this conversion in data_product.R seacarb::rho() see help file
  # Change GLODAP variable to match PANGAEA standard 
  mutate(variable = case_when(variable == "nitrate [μmol kg-1]" ~ "NO3 [µmol/l]",   
                              variable == "nitrite [μmol kg-1]" ~ "NO2 [µmol/l]",   
                              variable == "silicate [μmol kg-1]" ~ "SiO4 [µmol/l]",
                              variable == "phosphate [μmol kg-1]" ~ "PO4 [µmol/l]",
                              TRUE ~ variable),
         # Convert other variable names to a single standard
         variable = case_when(variable %in% c("[NO3]- [µmol/l]",
                                              # "NO3 [µmol/kg]", # Possible units issue
                                              "NO3 [µg-at/l]") ~ "NO3 [µmol/l]", 
                              variable %in% c("[PO4]3- [µmol/l]", "PO4 [µg-at/l]") ~ "PO4 [µmol/l]",
                              variable %in% c("[NH4]+ [µmol/l]", "[NH4]+ [µg-at/l]") ~ "NH4 [µmol/l]",
                              variable %in% c("[NO2]- [µmol/l]", "[NO2]- [µg-at/l]") ~ "NO2 [µmol/l]",
                              variable %in% c("nitrate+nitrite [µmol/l]", "[NO3]- + [NO2]- [µmol/l]",
                                              "NO2_NO3 [µmol/l]") ~ "NO3+NO2 [µmol/l]",
                              TRUE ~ variable),
         type = "in situ") |> filter(value > 0) |> 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) |> 
  distinct()
# unique(clean_nutrients$variable)
# test_df <- filter(clean_nutrients, variable == "NO3 [µmol/l]")
print(unique(clean_nutrients$variable))


## Biology -----------------------------------------------------------------

# TODO: Assign missing variables to drivers
# Find physical category data with no assigned driver
miss_bio <- cat_driver_miss(full_ALL, "bio")


### Primary production ------------------------------------------------------

# TODO: Look into making PP conversion calculations with existing data 

# Notes on primary production data
# Phaeopygments etc are not measures of PP, don't need fluorescence either
# [10um] vs [GFF] are different methods and both are valid.
# Must keep the difference between them documented.
# Collect all ChlA data
# https://zenodo.org/record/5572041#.YW_Lc5uxU5m: chl_flu [µg chl m-3] = chlorophyll a calculated from fluorescence profile

# Compile
clean_prim_prod <- filter(full_ALL, driver == "prim prod") |> filter(value > 0) |> 
  mutate(variable = case_when(variable %in% c("chlA [µg/l]", "Chl a [µg/l]") ~ "Chla [µg/l]", 
                              variable == "Chlorophyll A - 10um [µg/l]" ~ "Chla - 10um [µg/l]",
                              variable == "Chlorophyll A - GFF [µg/l]" ~ "Chla - GFF [µg/l]",
                              TRUE ~ variable),
         type = "in situ") |> filter(value > 0) |> 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) |> 
  distinct()
# unique(clean_pp$variable)
# unique(clean_pp$variable[str_detect(clean_pp$variable, "PP|pp")]) # Double check that only pH values are screened this way
# test_df <- filter(clean_pp, variable == "TOTAL_chla_area")
print(unique(clean_prim_prod$variable))


### Biomass -----------------------------------------------------------------

# TODO: Check this for lot's of variables in Young Sound: https://zenodo.org/record/5572041#.YW_Lc5uxU5m
# TODO: Look into creating phytoplankton biomass conversion using Chl a data

# Get all biomass variables
clean_biomass <- filter(full_ALL, driver == "biomass") |> 
  filter(!variable  %in% c("chlA [µg/l]", "Chla [µg/l]"), !grepl("\\[\\%\\]", variable)) |> 
  filter(!grepl("blade growth", variable), !grepl("tip growth", variable)) |> # NB: Decided to remove growth data
  filter(!grepl("\\[presence\\]", variable)) |> # Presence data used in species richness driver
  mutate(variable = str_replace(variable, "individuals\\/m3", "ind\\/m3"), 
         variable = str_replace(variable, "Biomass - ", ""),
         type = "in situ") |> filter(value > 0) |> 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) |> 
  distinct()
print(unique(clean_biomass$variable))


### Species richness ------------------------------------------------------

# The usefulness of this value will be adversely affected by how deep into the taxonomy a researcher has gone in one site vs another
# E.g. by giving all species, or just grouping by a larger taxa
# So don't use these comparisons in the data paper
# Just describe the data

# Get all species variables
clean_spp_rich <- filter(full_ALL, driver == "biomass") |> 
  filter(!variable  %in% c("chlA [µg/l]", "Chla [µg/l]")) |>  
  filter(value > 0) |> 
  # Fix sp. and spp.
  mutate(variable = str_replace(variable, " sp | spp | spp. ", " sp. "),
         # Remove life stages
         variable = str_replace(variable, " \\(CI\\)| \\(CII\\)| \\(CIII\\)| \\(CIV\\)| \\(CV\\)|
                                | \\(CI-CIII\\)| \\(CI-CV\\)|
                                | - AF| - AM| - CI| - CII| - CIII| - CIV| - CV", ""),
         variable = str_replace(variable, "-CIII|-CV|\\/AM", ""),
         variable = str_replace(variable, "longiremisI|longiremisII|longiremisV", "longiremis"),
         variable = str_replace(variable, " - juvenile| - cypris| - facetotecta| - nauplii| - adult|
                                | - zoea| - veliger| - parasitic nauplii| - larvae| - ova| - medusae|
                                | - megalopa| - pilidium| non.det| not det.", ""),
         # Remove units
         variable = str_replace(variable, " \\[ind\\/m3\\]", ""),
         variable = str_replace(variable, " \\[\\%\\]", ""),
         variable = str_replace(variable, " \\[cells\\/l]", ""),
         variable = str_replace(variable, " \\[individuals\\/m3\\]", ""),
         variable = str_replace(variable, " \\[count\\]", ""),
         variable = str_replace(variable, " 30-40um| 40-50um| 50-60um| 70-80um", ""),
         # Remove other specifications
         variable = str_replace(variable, " \\(veliger\\)| \\(AF\\)| \\(cypris\\)| \\(nauplii\\)|
                                | \\(AF/AM\\)| \\(furcilia\\)| \\(AM\\)| \\(calyptopis\\)| \\(larvae\\)|
                                | \\(secondary larvae\\)| \\(trochophora\\)| \\(metatrochophora\\)| \\(medusae\\)|
                                | \\(mitraria\\)| \\(adult\\)| \\(zoea\\)| \\(megalopa\\)| \\(pilidium\\)|
                                | \\(veliger \\(incl. Margarites and Velutina\\)\\)| \\(coxiella form\\)| \\(GG6\\)|
                                |cyst| > 10um| 3-7um| < 10um|  5-10um|  Non det. 5-10um| >7um| ~3um| 10-20um|
                                | non det.| non det| Non det.| indet.| 1| 2| 3| 4| 5| 30-40um| 20-30um|
                                |0-30um|0-40um| indet.0-50um| cf. normanii| cf. Cerinula - larvae| - larvae", ""),
         # Fix double spacing
         variable = str_replace(variable, "  ", " "),
         # Other small scale fixes
         variable = str_replace(variable, "finmarchicusI|finmarchicusII|finmarchicusV", "finmarchicus"),
         variable = str_replace(variable, "glacialisI|glacialisII|glacialisV", "glacialis"),
         variable = str_replace(variable, "; lt|; gte|;0 gte|; gt", ""),
         variable = str_replace(variable, "sp.0-40um|sp.0-30um", "sp."),
         variable = str_replace(variable, "sp.3", 'sp.')) |> 
  # Fixes by species
  mutate(variable = case_when(str_detect(variable, "A. nodosum") ~ "A. nodosum", 
                              str_detect(variable, "S. latissima") ~ "S. latissima", 
                              str_detect(variable, "CiliophoraNon") ~ "Ciliophora",
                              str_detect(variable, "Calanus finmarchicus") ~ "Calanus finmarchicus",
                              str_detect(variable, "Calanus glacialis") ~ "Calanus glacialis",
                              str_detect(variable, "Calanus hyperboreus") ~ "Calanus hyperboreus",
                              str_detect(variable, "Metridia longa") ~ "Metridia longa",
                              str_detect(variable, "Metridia lucens") ~ "Metridia lucens",
                              str_detect(variable, "Neoscolecithrix farrani") ~ "Neoscolecithrix farrani",
                              str_detect(variable, "Navicula sp") ~ "Navicula sp.",
                              str_detect(variable, "Nitzschia sp.") ~ "Nitzschia sp.",
                              str_detect(variable, "Pennate.") ~ "Pennate diatoms",
                              str_detect(variable, "Pseudocalanus minutus") ~ "Pseudocalanus minutus",
                              str_detect(variable, "Pseudocalanus acuspes") ~ "Pseudocalanus acuspes",
                              str_detect(variable, "Pseudocalanus sp.") ~ "Pseudocalanus sp.",
                              str_detect(variable, "Scolecithricella minor") ~ "Scolecithricella minor",
                              str_detect(variable, "Polar cod") ~ "Boreogadus saida",
                              TRUE ~ variable)) |> 
  # More specific fixes
  mutate(variable = case_when(variable == "Acartia longiremisI" ~ "Acartia longiremis", 
                              variable == "AetideidaeV" ~ "Aetideidae", 
                              TRUE ~ variable)) |>  
  # Remove unidentified things
  mutate(variable = case_when(variable %in% c("Centric diatoms not determined", "centric diatoms not determined",
                                              "Cell 7 domek", "Centric diatoms not det.") ~ as.character(NA),
                              TRUE ~ variable)) |> 
  mutate(variable = paste0(variable," [presence]"), value = 1,
         driver = "spp rich", type = "in situ") |>  
  filter(!is.na(variable)) %>% arrange(variable) |> 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) |> 
  distinct()

# From the cleaned up data create a species count variable
# From here the species are combined into counts - the names are therefore lost
spp_count <- clean_spp_rich |> 
  group_by(lon, lat, date, depth, category, driver, site, type) |> 
  summarise(value = as.numeric(n()), .groups = "drop") |> 
  mutate(variable = "spp count [n]",
         date_accessed = as.Date(Sys.Date()), 
         URL = "None", citation = "Value derived for FACE-IT dataset") |> 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) |> 
  distinct()

# Combine and clean up
clean_spp_rich <- rbind(clean_spp_rich, spp_count) |> distinct()
rm(spp_count); gc(); print(unique(clean_biomass$variable))


## Social ------------------------------------------------------------------

# NB: There is quite a lot of data in the social category for provinces/cities etc.
# that are outside of the seven FACE-IT study sites.
# It was unclear what was to be done with these data so they were included in the v1.0 dataset.
# For future versions we may possibly remove all data for settlements etc. not within the seven study sites

### Relevant sites

## Norway
# Troms og Finnmark: Province(s) for Porsangerfjorden
# Lakselv: Main city for Porsangerfjorden (?)
# Lakselv Banak + Honningsvåg Valan: Airports on Porsangerfjorden 

## Svalbard
# Svalbard: Province for Svalbard
# Longyearbyen: Main city in Isfjorden
# Svalbard Longyear: Airport on Isfjorden
# Ny-Alesund: Main village in Kongsfjorden

## Greenland
# Sermersooq: Municipality for Nuup Kangerlua
# Nuuk: Main city in Nuup Kangerlua, also an airport
# Qeqertalik: Municipality for Disko bay
# Avannaata: Municipality that borders pn Disko Bay (relevant for demographics, fish landings, etc.)
# Qeqertarsuaq: Main city in Disko Bay (?)
# Aasiaat: Port on southern edge of Disko Bay
# Ilulissat: Port on eastern edge of Disko Bay, also an airport
# Qasigiannguit: Port on eastern edge of Disko Bay
# Uummannaq: City North of Disko Bay (possibly relevant for fish landings etc.)
# Kangaatsiaq: Port south of Disko Bay (possibly relevant for fish landings etc.)
# Outside municipalities: Young Sound appears to fall outside of a municipality

# TODO: Assign missing variables to drivers
# Find physical category data with no assigned driver
miss_soc <- cat_driver_miss(full_ALL, "soc")


### Governance --------------------------------------------------------------

# Get all governance data
clean_gov <- filter(full_ALL, driver == "gov") |> mutate(type = "in situ") |> 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) |> 
  distinct()
print(unique(clean_gov$variable))


### Tourism ----------------------------------------------------------------

# Get tourism variables
clean_tourism <- filter(full_ALL, driver == "tourism", !is.na(value)) |> mutate(type = "in situ") |> 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) |> 
  distinct()
print(unique(clean_tourism$variable))


### Fisheries ---------------------------------------------------------------

# NB: Ship traffic is included here as it is mostly due to industry and not tourism

# Get shipping variables
clean_fisheries <- filter(full_ALL, driver == "fisheries") |> 
  filter(!grepl("\\[Month Trips\\]", variable)) |> # NB: It is unclear what exactly these are
  mutate(type = "in situ") |> arrange(variable) |> 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) |> 
  distinct()
# unique(clean_fisheries$category)
# unique(clean_fisheries$driver)
# unique(clean_fisheries$variable)
print(unique(clean_fisheries$variable))


## Save clean data ---------------------------------------------------------

# Combine and select columns to match final standard
clean_all <- rbind(clean_sea_ice, clean_glacier, clean_runoff,
                   clean_sea_temp, clean_salinity, clean_light,
                   clean_carb, clean_nutrients,
                   clean_prim_prod, clean_biomass, clean_spp_rich,
                   clean_gov, clean_tourism, clean_fisheries) |> distinct()

# Save all data in one file
save(clean_all, file = "data/analyses/clean_all.RData")
save(clean_all, file = "~/pCloudDrive/FACE-IT_data/clean_all.RData")

# Save all data by driver/site
save_data(df = clean_all, data_type = "clean")


## Meta-data ---------------------------------------------------------------

if(!exists("clean_all")) load("data/analyses/clean_all.RData")

# Harvest meta-data to be used in the meta-database

# Site, type, driver, date (year range), source, citation, date accessed
clean_all_meta <- clean_all |> 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, date) |> 
  mutate(date = year(date)) |> 
  group_by(date_accessed, URL, citation, type, site, category, driver) |> 
  mutate(year_min = min(date), year_max = max(date)) |> dplyr::select(-date) |> 
  ungroup() |>  distinct() |> 
  left_join(long_cat_names, by = "category") |> left_join(long_driver_names, by = "driver") |> 
  group_by(date_accessed, URL, citation, type, site, year_min, year_max) |> 
  summarise(category = toString(unique(category_long)), driver = toString(unique(driver_long)), .groups = "drop")
save(clean_all_meta, file = "data/analyses/clean_all_meta.RData")


## References --------------------------------------------------------------

if(!exists("clean_all")) load("data/analyses/clean_all.RData")

# Save just the references
all_ref <- dplyr::select(full_ALL, citation) |> distinct()
save(all_ref, file = "data/analyses/all_ref.RData")


## Summary -----------------------------------------------------------------

# NB: Not currently running the data summaries
# This was done for the v1.0 data paper
# Combine analysed data
# all_meta <- rbind(summary_sea_ice$monthly, summary_glacier$monthly, summary_runoff$monthly,
#                   summary_sea_temp$monthly, summary_sal$monthly, summary_light$monthly,
#                   summary_carb$monthly, summary_nutrients$monthly, 
#                   summary_pp$monthly, summary_biomass$monthly, summary_spp_rich$monthly, 
#                   summary_gov$monthly, summary_tourism$monthly, summary_fisheries$monthly)
# save(all_meta, file = "data/analyses/all_meta.RData")
# load("data/analyses/all_meta.RData")


## PANGAEA file ------------------------------------------------------------

# TODO: Consider splitting these out into metadata lookup tables

# Load all clean data
# clean_all <- map_dfr(dir("data/full_data", pattern = "clean", full.names = T), read_csv)
if(!exists("clean_all")) load("data/analyses/clean_all.RData")

# Leave an NA shadow so users know the data exist and where to find them
data_shadow <- "g-e-m|GRDC|Received directly from Mikael Sejr"
data_shadow_df <- shadow(clean_all)

# Prep for PANGAEA standard
FACE_IT_v1.3 <- clean_all |> 
  # Remove shadow data
  filter(!grepl(data_shadow, URL)) |> 
  # Convert to PANGAEA date standard
  rbind(data_shadow_df) |> 
  dplyr::rename(`date/time [UTC+0]` = date, `depth [m]` = depth,
                `longitude [°E]` = lon, `latitude [°N]` = lat) |> 
  mutate(`date/time [UTC+0]` = case_when(!is.na(`date/time [UTC+0]`) ~ paste0(`date/time [UTC+0]`,"T00:00:00"), 
                                         TRUE ~ as.character(`date/time [UTC+0]`)),
         citation = str_replace_all(citation, ";", "."))

# Double check data shadows have been applied correctly
shadow_test <- filter(FACE_IT_v1.3, grepl(data_shadow, URL))
rm(shadow_test); gc()

# Save as .csv
# NB: write_csv_arrow not currently working, file too large
write_csv(FACE_IT_v1.3, "~/pCloudDrive/FACE-IT_data/FACE_IT_v1.3.csv")
write_csv(FACE_IT_v1.3, "data/full_data/FACE_IT_v1.3.csv")

# TODO: Address multiples
# Cryo data
FACE_IT_v1.3_cryo <- filter(FACE_IT_v1.3, category == "cryo") |>
  pivot_wider(names_from = variable, values_from = value, values_fn = mean)
write_delim(FACE_IT_v1.3_cryo, "~/pCloudDrive/FACE-IT_data/FACE_IT_v1.3_cryo.csv", delim = ";")

# Phys data
FACE_IT_v1.3_phys <- filter(FACE_IT_v1.3, category == "phys") |> 
  pivot_wider(names_from = variable, values_from = value, values_fn = mean)
write_delim(FACE_IT_v1.3_phys, "~/pCloudDrive/FACE-IT_data/FACE_IT_v1.3_phys.csv", delim = ";")

# Chem data
FACE_IT_v1.3_chem <- filter(FACE_IT_v1.3, category == "chem") |> 
  pivot_wider(names_from = variable, values_from = value)
write_delim(FACE_IT_v1.3_chem, "~/pCloudDrive/FACE-IT_data/FACE_IT_v1.3_chem.csv", delim = ";")

# Bio data
FACE_IT_v1.3_bio <- filter(FACE_IT_v1.3, category == "bio") |> 
  pivot_wider(names_from = variable, values_from = value, values_fn = mean)
write_delim(FACE_IT_v1.3_bio, "~/pCloudDrive/FACE-IT_data/FACE_IT_v1.3_bio.csv", delim = ";")

# Soc data
FACE_IT_v1.3_soc <- filter(FACE_IT_v1.3, category == "soc") |> 
  pivot_wider(names_from = variable, values_from = value, values_fn = mean)
write_delim(FACE_IT_v1.3_soc, "~/pCloudDrive/FACE-IT_data/FACE_IT_v1.3_soc.csv", delim = ";")


## Additional cleaning -----------------------------------------------------
# Another layer of cleaning for smoother top-level comparative analyses

# NB: This is not done for the data uploaded to PANGAEA as it removes complexity from the dataset
# that may be of interest to some users.

# Load all clean data
# clean_all <- map_dfr(dir("data/full_data", pattern = "clean", full.names = T), read_csv)
if(!exists("clean_all")) load("data/analyses/clean_all.RData")

# Clean/remove some variables for better comparisons
clean_all_cryo <- filter(clean_all, category == "cryo") %>% 
  filter(!str_detect(variable, "cover Jan|cover Feb|cover Mar|cover Apr|cover May|cover Jun|
                     |cover Jul|cover Aug|cover Sep|cover Oct|cover Nov|cover Dec|
                     |end date|start date|Snow|snow|
                     |_1936|_1990|_2010")) # Add these back in if possible
clean_all_phys <- filter(clean_all, category == "phys") %>% 
  filter(!grepl("UV-A", variable)) %>% # Don't want this for comparisons... maybe remove completely...
  filter(!grepl("emissions", variable)) # Removing ship emissions for the moment
clean_all_chem <- filter(clean_all, category == "chem") %>% 
  filter(!grepl("pH \\[unknown scale\\]", variable)) %>% # Only want known pH scale data for comparisons
  filter(!grepl("emissions", variable)) # Removing ship emissions for the moment
clean_all_biomass <- filter(clean_all, driver == "biomass") %>% # Just get sum of all species counts per sample... not ideal
  mutate(variable = case_when(str_detect(variable, "ind\\/m3") ~ "spp count [ind/m3]", 
                              str_detect(variable, "cells\\/l") ~ "spp count [cells/l]",
                              TRUE ~ variable)) %>% 
  group_by(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth) %>% 
  summarise(value = sum(value, na.rm = T), .groups = "drop")
clean_all_bio <- filter(clean_all, category == "bio") %>% 
  filter(driver == "prim prod" |variable == "spp count [n]") %>% # Remove everything except the species count value created above 
  rbind(clean_all_biomass); rm(clean_all_biomass)
clean_all_gov <- filter(clean_all, driver == "gov") %>% 
  mutate(variable = case_when(str_detect(variable, "Population - ") ~ "Population [n]",
                              str_detect(variable, "Taxable income - ") ~ "Taxable income [DKK]",
                              str_detect(variable, "All industries - main employment - Total") ~ "Employement [n/month]",
                              str_detect(variable, "All industries - income - Total") ~ "Income [DKK/month]",
                              str_detect(variable, "Unemployed - Total ") ~ "Unemployed [n]",
                              str_detect(variable, "pop \\[Longyearbyen & Ny-Alesund") ~ "Population [n]",
                              str_detect(variable, "pop \\[Barentsburg and Pyramiden") ~ "Population [n]",
                              str_detect(variable, "pop \\[Hornsund") ~ "Population [n]",
                              TRUE ~ variable)) %>% 
  filter(!str_detect(variable, "Taxable income - |- main employment| - income - |Unemployed - |Employment - ")) %>%
  filter(!is.na(variable), !is.na(value)) %>% 
  group_by(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth) %>% 
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>% arrange(variable)
clean_all_tourism <- filter(clean_all, driver == "tourism") %>% 
  mutate(variable = case_when(variable %in% c("Calls [Cruise boats (overseas)]", "Calls [Tourist boats (expedition cruise)]",
                                              "Calls [Day trip boats (local boats)]", "Calls [Day trip boats (12 PAX RIB mm)]",
                                              "Calls [Pleasure boats (Sail charter engine)]") ~ "Calls - tourism [n]",
                              variable == "Guest nights - Total - Total [n]" ~ "Guest nights [n]",
                              variable == "Cruise capacity (Total) arrivals [n]" ~ "Cruise capacity arrivals [n]",
                              variable == "Cruise capacity (Total) passengers [n]" ~ "Cruise capacity passengers [n]",
                              str_detect(variable, "Passengers") ~ "Passengers [n]",
                              str_detect(variable, "arrival") ~ "Arrivals [n]",
                              str_detect(variable, "Days in port") ~ "Days in port - tourism [count]",
                              str_detect(variable, "guest night ") ~ "Guest nights [n]",
                              str_detect(variable, "Vessels ") ~ "Vessels [n]",
                              TRUE ~ variable)) %>% 
  # Remove some unwanted ship values like duration in the fjord
  filter(!str_detect(variable, "Duration|duration|Fuel|gross weight|Month trips|emissions|
                     |Number of trips pr year|Average speed|Power|Total fuel|Tonnage|
                     |Month Trips|Number of ships|(annual)|Cruise passengers - |
                     |Guest nights - |Cruise capacity \\(|- total \\[n\\]")) %>% # These are annual values or are otherwise accounted for
  # Select sum rather than mean values
  mutate(variable = case_when(str_detect(variable, "mean") ~ as.character(NA),
                              str_detect(variable, "; sum") ~ gsub("; sum", "", variable),
                              TRUE ~ variable)) %>% 
  filter(!is.na(variable), !is.na(value)) %>% 
  group_by(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth) %>% 
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>% arrange(variable)
clean_all_fisheries <- filter(clean_all, driver == "fisheries") %>% 
  mutate(variable = case_when(variable %in% c("Calls [Cargo boats]", "Calls [Teaching / research]",
                                              "Calls [Fishing boats]", "Calls [Navy / Coast Guard]",
                                              "Calls [Polar / Nordsyssel]", "Calls [Pilot boat]",
                                              "Calls [Other vessels]") ~ "Calls - commercial [n]",
                              variable == "Export -  total [1,000 DKK]" ~ "Export [1,000 DKK]",
                              variable == "Export -  total [Tonnes]" ~ "Export [Tonnes]",
                              str_detect(variable, "Advice - ") ~ "Advice [pieces]",
                              str_detect(variable, "Quarter ") ~ as.character(NA),
                              str_detect(variable, " - Coastal \\[1,000 DKK\\]") ~ "Landings - coastal [1,000 DKK]",
                              str_detect(variable, " - Coastal \\[Tonnes\\]") ~ "Landings - coastal [Tonnes]",
                              str_detect(variable, " - Offshore \\[1,000 DKK\\]") ~ "Landings - offshore [1,000 DKK]",
                              str_detect(variable, " - Offshore \\[Tonnes\\]") ~ "Landings - offshore [Tonnes]",
                              str_detect(variable, "Average kilo price - ") ~ "Average kilo price [index]",
                              str_detect(variable, "Quota - ") ~ "Quota [Tonnes]",
                              str_detect(variable, "Catch - ") ~ "Catch [pieces]",
                              str_detect(variable, "Vessels ") ~ "Vessels [n]",
                              TRUE ~ variable)) %>% 
  # Remove some unwanted ship values like duration in the fjord
  filter(!str_detect(variable, "Duration|duration|Fuel|gross weight|Month trips|emissions|
                     |Number of trips pr year|Average speed|Power|Total fuel|Tonnage|
                     |Month Trips|Number of ships|Export - ")) %>% # These are annual values or are otherwise accounted for
  # Select sum rather than mean values
  mutate(variable = case_when(str_detect(variable, "mean") ~ as.character(NA),
                              str_detect(variable, "; sum") ~ gsub("; sum", "", variable),
                              TRUE ~ variable)) %>% 
  filter(!is.na(variable), !is.na(value)) %>% 
  group_by(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth) %>% 
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>% arrange(variable)

# Combine
clean_all_clean <- clean_all_cryo %>% 
  rbind(clean_all_phys) %>% 
  rbind(clean_all_chem) %>%
  rbind(clean_all_bio) %>% 
  rbind(clean_all_gov) %>% 
  rbind(clean_all_tourism) %>% 
  rbind(clean_all_fisheries)
rm(clean_all_cryo, clean_all_phys, clean_all_chem, clean_all_bio, clean_all_gov, clean_all_tourism, clean_all_fisheries); gc()
save(clean_all_clean, file = "data/analyses/clean_all_clean.RData")


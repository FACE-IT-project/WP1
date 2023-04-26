# Code/nuup_species_gem.R
# restricted data of nuup from the GEM website


# Set up ------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(ggridges)
library(ggpubr)
library(stringi)
library(pangaear)
library(rgbif)


# Formula -----------------------------------------------------------------
# Change name to latin and english name
source('users/calin/code/formulas.R')


# Data --------------------------------------------------------------------
# Bird presence
## Manque : Species
nuup_bird_presence <- read_delim("P:/restricted_data/GEM/nuup/View_BioBasis_Nuuk_Data_Birds_Passerine_bird_abundance170420231432285653.csv") %>% 
  mutate(date_accessed = as.Date("2023-04-17"),
         URL = "https://doi.org/10.17897/DRTB-PY74",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         lonP = Point,
         latP = Point,
         lon = case_when(lonP == "A"~64.134685,
                         lonP == "B"~64.135155,
                         lonP == "C"~64.134592,
                         lonP == "D"~64.13239,
                         lonP == "E"~64.131052,
                         lonP == "F"~64.129385,
                         lonP == "G"~64.131761,
                         lonP == "H"~64.132669,
                         lonP == "I"~64.134509,
                         lonP == "J"~64.135639,
                         lonP == "K"~64.133636,
                         lonP == "L"~64.132841,
                         lonP == "M"~64.131031),
         lat = case_when(lonP == "A"~-51.385105,
                         lonP == "B"~-51.391187,
                         lonP == "C"~-51.396234,
                         lonP == "D"~-51.39359,
                         lonP == "E"~-51.38916,
                         lonP == "F"~-51.37833,
                         lonP == "G"~-51.379398,
                         lonP == "H"~-51.374116,
                         lonP == "I"~-51.363874,
                         lonP == "J"~-51.355553,
                         lonP == "K"~-51.344558,
                         lonP == "L"~-51.336278,
                         lonP == "M"~-51.326204), 
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
         driver ="biomass",
         type = "in situ",
         site = "nuup",
         value = Number) %>%
  dplyr::rename(date = Date) %>%
  dplyr::group_by(date_accessed, URL, citation, lon, lat, depth, date, variable, category, driver, type, site) %>% 
  dplyr::summarise(sum(value)) %>% 
  dplyr::rename(value = `sum(value)`) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!value == 0)

# Seabird counting
nuup_seabird_count <- read_delim("P:/restricted_data/GEM/nuup/View_MarineBasis_Nuuk_Data_Seabirds_Seabird_species_counts_per_colony17042023154835389.csv",
                                 na = c("NULL","-1", "2017-07-00")) %>% 
  filter(!is.na(Date)) %>% 
  filter(!is.na(Latin)) %>%
  filter(!is.na(MinNumbers)) %>%
  mutate(date_accessed = as.Date("2023-04-17"), 
         URL = "https://doi.org/10.17897/WKFK-SS31", 
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Greenland Institute of Natural Resources, Nuuk, Greenland in collaboration with Department of Bioscience, Aarhus University, Denmark and University of Copenhagen, Denmark.", 
         lon = Longitude, 
         lat = Latitude, 
         depth = NA, 
         nomsp = map(Latin, latin_eng),
         variable = paste0(nomsp, " [n]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "nuup", 
         date = as.Date(Date),
         moy = ceiling((MinNumbers+MaxNumbers)/2),
         value = ifelse(!is.na(moy), moy, MinNumbers)) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value)


# Seabird presence
nuup_seabird_presence <- read_delim("P:/restricted_data/GEM/nuup/View_MarineBasis_Nuuk_Data_Seabirds_Seabird_species_counts_per_colony17042023154835389.csv",
                                    na ="2017-07-00") %>% 
  filter(!is.na(Latin)) %>%
  mutate(date_accessed = as.Date("2023-04-17"), 
         URL = "https://doi.org/10.17897/WKFK-SS31", 
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Greenland Institute of Natural Resources, Nuuk, Greenland in collaboration with Department of Bioscience, Aarhus University, Denmark and University of Copenhagen, Denmark.", 
         lon = Longitude, 
         lat = Latitude, 
         depth = NA, 
         nomsp = map(Latin, latin_eng),
         variable = paste0(nomsp, " [presence]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "nuup", 
         date = as.Date(Date),
         value = ifelse((MinNumbers == 0), 0, 1)) %>% 
  filter(!nomsp == "NA") %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value)


# marin mammal
nuup_mmam_count <- read_delim("P:/restricted_data/GEM/nuup/View_MarineBasis_Nuuk_Data_Marine_mammals_Identification_of_Humpback_Whales_individuals_year170420231542310109.csv") %>%
  pivot_longer(cols = c(`INDIVIDUALS`, `NEW INDIVIDUALS`, `RECATCH IND`)) %>%
  mutate(date_accessed = as.Date("2023-04-17"), 
         URL = "https://doi.org/10.17897/13YN-1209", 
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Greenland Institute of Natural Resources, Nuuk, Greenland in collaboration with Department of Bioscience, Aarhus University, Denmark and University of Copenhagen, Denmark.", 
         lon = NA, 
         lat = NA, 
         depth = NA, 
         Species = "Megaptera novaeangliae",
         nomsp = map(Species, latin_eng),
         type = case_when(name == "INDIVIDUALS"~"sighted",
                          name == "NEW INDIVIDUALS"~"new individual sighted",
                          name == "RECATCH IND"~"individual already sighted"),
         variable = paste0(nomsp, " ", type, " [n]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "nuup", 
         date = as.Date(paste0(YEAR,"-12-31"))) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value)



# PANGEA data -------------------------------------------------------------



nuup_search01 <- pg_search(query = 'nuuk species')
nuup_data01 <- pg_data(doi = nuup_search01$doi[1])
nuup_0101 <- nuup_data01[[1]]$data

nuup_search02 <- pg_search(query = 'Saha, Atal; Hauser, Lorenz; Hedeholm, Rasmus; Planque, Benjamin')
nuup_data02 <- pg_data(doi = nuup_search02$doi[1])
nuup_0202 <- nuup_data02[[2]]$data


gbif_search01 <- occ_search(scientificName = "Saccharina latissima")
gbif_data01 <- gbif_search01$data


ggplot(data = gbif_data01, aes (x = decimalLongitude, y = decimalLatitude)) + 
  borders()+
  geom_point(aes(color = year(eventDate)))

# Data set ----------------------------------------------------------------

nuup_species_GEM <- rbind(nuup_bird_presence, nuup_seabird_count, nuup_seabird_presence, nuup_mmam_count)

save(nuup_species_GEM, file = "users/calin/data/nuup_species_GEM.RData")

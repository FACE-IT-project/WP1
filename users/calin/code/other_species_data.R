# Code/other_species_data.R
# restricted data of young from the GEM website


# Set up ------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(ggridges)
library(ggpubr)
library(stringi)


# Formula -----------------------------------------------------------------
# Change name to latin and english name
source('users/calin/code/formulas.R')


# Data --------------------------------------------------------------------

## LAKES -------------------------------------------------------------------
# Phytoplankton biovolume
## Manque : lon,lat et Species
young_phyto_biovolume <- read_delim("P:/restricted_data/GEM/young/View_BioBasis_Zackenberg_Data_Lakes_Phytoplankton170420231440184919.csv",
                                    na = c("-1.000","-9999")) %>% 
  mutate(date_accessed = as.Date("2023-04-17"),
         URL = "https://doi.org/10.17897/B15M-2E46",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         lon = NA, lat = NA, depth = NA,
         variable = paste0(Taxon," (phytoplancton) in ", Lake ," lake biolume [mm3/L]"),
         category = "bio",
         driver ="biomass",
         date = Date,
         type = "in situ",
         site = "young",
         value = PhytoBiovolume) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))

# Phytoplankton individuals
## Manque : lon,lat et Species
young_phyto_number <- read_delim("P:/restricted_data/GEM/young/View_BioBasis_Zackenberg_Data_Lakes_Phytoplankton170420231440184919.csv",
                                 na = c("-1.000","-9999", "-1")) %>% 
  mutate(date_accessed = as.Date("2023-04-17"),
         URL = "https://doi.org/10.17897/B15M-2E46",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         lon = NA, lat = NA, depth = NA,
         variable = paste0(Taxon, " (phytoplancton) in ", Lake ," lake [n/mL]"),
         category = "bio",
         driver ="biomass",
         date = Date,
         type = "in situ",
         site = "young",
         value = NumberPer_mLiter) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))


# Zooplankton individuals LS lake
## Have stage to change ????
young_zoo_number_LSlake <- read_delim("P:/restricted_data/GEM/young/View_BioBasis_Zackenberg_Data_Lakes_Zooplankton170420231442261928.csv") %>% 
  filter(Lake == "Langemandssø (LS)"| Lake == "Langemandssø") %>%
  mutate(date_accessed = as.Date("2023-04-17"),
         URL = "https://doi.org/10.17897/VWPC-B466",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         lon = 74.50113, 
         lat = -20.60272, 
         depth = NA,
         variable = paste0(tolower(Stage), " ", Taxon, " (zooplancton) [n/L]"),
         category = "bio",
         driver ="biomass",
         date = Date,
         type = "in situ",
         site = "young",
         value = NumberPerLiter) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) #%>%
# filter(!is.na(value))

# Zooplankton individuals SS lake
## Have stage to change ????
young_zoo_number_SSlake <- read_delim("P:/restricted_data/GEM/young/View_BioBasis_Zackenberg_Data_Lakes_Zooplankton170420231442261928.csv") %>% 
  filter(!Lake == "Langemandssø (LS)"| !Lake == "Langemandssø") %>%
  mutate(date_accessed = as.Date("2023-04-17"),
         URL = "https://doi.org/10.17897/VWPC-B466",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         lon = 74.49871, 
         lat = -20.60252, 
         depth = NA,
         variable = paste0(tolower(Stage), " ", Taxon, " (zooplancton) [n/L]"),
         category = "bio",
         driver ="biomass",
         date = Date,
         type = "in situ",
         site = "young",
         value = NumberPerLiter) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) #%>%
# filter(!is.na(value))


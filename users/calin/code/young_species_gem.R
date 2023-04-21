# Code/young_species_gem.R
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

## YOUNG -------------------------------------------------------------------
# Bird breeding phenology nests eggs
## Manque : lon,lat et Species
## Have NA value
young_bird_nests_eggs <- read_delim("P:/restricted_data/GEM/young/View_BioBasis_Zackenberg_Data_Birds_Bird_breeding_phenology__nests170420231421385886.csv", 
                                    na = c("9999-01-01","-9999"), 
                                    col_types = "iccnnDDiiicc") %>%
  pivot_longer(cols = c(`FirstEggDate`, 
                        `HatchingDate`)) %>% 
  dplyr::filter(name == "FirstEggDate") %>% 
  dplyr::rename(date_egg = value) %>%
  mutate(date_enfonction = ifelse(is.na(date_egg), 
                                  as.Date(paste0(Year,"-12-31")), 
                                  as.Date(date_egg)),
         date_bon = as.Date(date_enfonction, origin),
         date_accessed = as.Date("2023-04-17"),
         URL = "https://doi.org/10.17897/5S51-HE52",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         lon = NA, lat = NA, depth = NA,
         nomsp = "FORMULA IN PROGRESS",
         variable = paste0(nomsp," eggs laid [n]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "young",
         value = EggsLaid) %>%
  dplyr::rename(date = date_bon) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value)# %>%
  # filter(!is.na(value))

# Bird breeding phenology nests hatching
## Manque : lon,lat et Species
## Have NA value
young_bird_nests_hatch <- read_delim("P:/restricted_data/GEM/young/View_BioBasis_Zackenberg_Data_Birds_Bird_breeding_phenology__nests170420231421385886.csv", 
                                    na = c("9999-01-01","-9999"), 
                                    col_types = "iccnnDDiiicc") %>%
  pivot_longer(cols = c(`FirstEggDate`, 
                        `HatchingDate`)) %>% 
  dplyr::filter(name == "HatchingDate") %>% 
  dplyr::rename(date_hatch = value) %>%
  mutate(date_enfonction = ifelse(is.na(date_hatch), 
                                  as.Date(paste0(Year,"-12-31")), 
                                  as.Date(date_hatch)),
         date_bon = as.Date(date_enfonction, origin),
         date_accessed = as.Date("2023-04-17"),
         URL = "https://doi.org/10.17897/5S51-HE52",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         lon = NA, lat = NA, depth = NA,
         nomsp = "FORMULA IN PROGRESS",
         variable = paste0(nomsp," eggs hatched [n]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "young",
         value = PulliHatched) %>%
  dplyr::rename(date = date_bon) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) #%>%
# filter(!is.na(value))


# Bird abundance
## Manque : lon,lat et Species
## Have NA value
young_bird_abundance <- read_delim("P:/restricted_data/GEM/young/View_BioBasis_Zackenberg_Data_Birds_Breeding_bird_abundance170420231423146922.csv") %>%
  mutate(date_accessed = as.Date("2023-04-17"),
         URL = "https://doi.org/10.17897/1Z6Z-FQ32",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         lon = NA, lat = NA, depth = NA,
         nomsp = "FORMULA IN PROGRESS",
         variable = paste0(nomsp," [presence]"),
         category = "bio",
         driver ="biomass",
         date = as.Date(paste0(Year,"-12-31")),
         type = "in situ",
         site = "young",
         value = 1) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) #%>%
  # filter(!is.na(value))

  
# Phytoplankton biovolume
## Manque : lon,lat et Species
## Have NA value
young_phyto_biovolume <- read_delim("P:/restricted_data/GEM/young/View_BioBasis_Zackenberg_Data_Lakes_Phytoplankton170420231440184919.csv",
                                    na = c("-1.000","-9999")) %>% 
  mutate(date_accessed = as.Date("2023-04-17"),
         URL = "https://doi.org/10.17897/B15M-2E46",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         lon = NA, lat = NA, depth = NA,
         nomsp = "FORMULA IN PROGRESS",
         variable = paste0(Taxon," (phytoplancton) in ", Lake ," lake biolume [mm3/L]"),
         category = "bio",
         driver ="biomass",
         date = Date,
         type = "in situ",
         site = "young",
         value = PhytoBiovolume) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) #%>%
# filter(!is.na(value))

# Phytoplankton individuals
## Manque : lon,lat et Species
## Have NA value
young_phyto_number <- read_delim("P:/restricted_data/GEM/young/View_BioBasis_Zackenberg_Data_Lakes_Phytoplankton170420231440184919.csv",
                                 na = c("-1.000","-9999")) %>% 
  mutate(date_accessed = as.Date("2023-04-17"),
         URL = "https://doi.org/10.17897/B15M-2E46",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         lon = NA, lat = NA, depth = NA,
         nomsp = "FORMULA IN PROGRESS",
         variable = paste0(Taxon, " (phytoplancton) in ", Lake ," lake [n/mL]"),
         category = "bio",
         driver ="biomass",
         date = Date,
         type = "in situ",
         site = "young",
         value = NumberPer_mLiter) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) #%>%
# filter(!is.na(value))


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
         nomsp = "FORMULA IN PROGRESS",
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
         nomsp = "FORMULA IN PROGRESS",
         variable = paste0(tolower(Stage), " ", Taxon, " (zooplancton) [n/L]"),
         category = "bio",
         driver ="biomass",
         date = Date,
         type = "in situ",
         site = "young",
         value = NumberPerLiter) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) #%>%
# filter(!is.na(value))


# Bird breeding phenology broods
## Manque : lon,lat et Species
## Have NA value
young_bird_broods <- read_delim("P:/restricted_data/GEM/young/View_BioBasis_Zackenberg_Data_Birds_Bird_breeding_phenology__broods17042023135647252.csv", 
                                    na = c("9999-01-01","-9999"), 
                                    col_types = "iccnnDic") %>%
  dplyr::rename(date_egg = FirstEggDate) %>%
  mutate(date_enfonction = ifelse(is.na(date_egg), 
                                  as.Date(paste0(Year,"-12-31")), 
                                  as.Date(date_egg)),
         date_bon = as.Date(date_enfonction, origin),
         date_accessed = as.Date("2023-04-17"),
         URL = "https://doi.org/10.17897/YPNZ-VX08",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         lon = NA, lat = NA, depth = NA,
         nomsp = "FORMULA IN PROGRESS",
         variable = paste0(nomsp," eggs laid [n]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "young",
         value = Accuracy) %>%
  dplyr::rename(date = date_bon) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value)# %>%
# filter(!is.na(value))


























  




## NUUP --------------------------------------------------------------------
# Bird presence
# plot A
## Manque : Species
## Have NA value
nuup_bird_presence_A <- read_delim("P:/restricted_data/GEM/nuup/View_BioBasis_Nuuk_Data_Birds_Passerine_bird_abundance170420231432285653.csv") %>% 
  filter(Point == "A") %>%
  mutate(date_accessed = as.Date("2023-04-17"),
         URL = "https://doi.org/10.17897/DRTB-PY74",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         lon = 64.134685, 
         lat = -51.385105, 
         depth = NA,
         nomsp = "FORMULA IN PROGRESS",
         gender = case_when(Gender == "M"~"male", 
                            Gender == "F"~"female",
                            Gender == "UK"~"unknown"),
         variable = paste0(gender, " ", nomsp," [n]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "nuup",
         value = Number) %>%
  dplyr::rename(date = Date) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value)# %>%filter(!is.na(value))
# plot B
## Manque : Species
## Have NA value
nuup_bird_presence_B <- read_delim("P:/restricted_data/GEM/nuup/View_BioBasis_Nuuk_Data_Birds_Passerine_bird_abundance170420231432285653.csv") %>% 
  filter(Point == "B") %>%
  mutate(date_accessed = as.Date("2023-04-17"),
         URL = "https://doi.org/10.17897/DRTB-PY74",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         lon = 64.135155, 
         lat = -51.391187, 
         depth = NA,
         nomsp = "FORMULA IN PROGRESS",
         gender = case_when(Gender == "M"~"male", 
                            Gender == "F"~"female",
                            Gender == "UK"~"unknown"),
         variable = paste0(gender, " ", nomsp," [n]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "nuup",
         value = Number) %>%
  dplyr::rename(date = Date) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value)# %>%filter(!is.na(value))




















































# OTHER -------------------------------------------------------------------
young_data <- rbind(young_bird_nests_eggs, young_bird_nests_hatch,
                    young_bird_abundance,
                    young_phyto_biovolume,
                    young_phyto_number,
                    young_zoo_number_LSlake,
                    young_zoo_number_SSlake,
                    young_bird_broods
)

#   dplyr::rename(nomSpecies = Species) %>% 
#   apply(young_bird_nests, margin = 2, FUN = nom_latin_com())
# # 
# 
# nom_latin_com(nomSpecies = Species)
# 
# test1 <- nom_latin_com(young_bird_nests$Species)
# 
# 
# 


# # ivory gull population
# young_bird_brood <- read_delim("P:/restricted_data/GEM/young/View_BioBasis_Zackenberg_Data_Birds_Bird_breeding_phenology__broods17042023135647252.csv") %>%
#   
#   convert_UTM_deg(utm_zone = 27)
# 
# 
# young_bird_brood_coord <- young_bird_brood %>% 
#   dplyr::select(Easting, Northing) %>% 
#   distinct() %>% 
#   filter(Easting >= 0)
# 
# young_bird_brood_dd <- 
#   


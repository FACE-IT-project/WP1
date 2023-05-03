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
# Bird breeding phenology nests eggs
## Have NA value
young_bird_nests_eggs <- read_delim("P:/restricted_data/GEM/young/View_BioBasis_Zackenberg_Data_Birds_Bird_breeding_phenology__nests170420231421385886.csv", 
                                    na = c("9999-01-01","-9999"), 
                                    col_types = "iccnnDDiiicc") %>%
  convert_UTM_deg(utm_zone = 27) #%>%
  pivot_longer(cols = c(`FirstEggDate`, 
                        `HatchingDate`)) %>% 
  dplyr::filter(name == "FirstEggDate") %>% 
  dplyr::rename(date_egg = FirstEggDate) %>%
  mutate(date_enfonction = ifelse(is.na(date_egg), 
                                  as.Date(paste0(Year,"-12-31")), 
                                  as.Date(date_egg)),
         date_bon = as.Date(date_enfonction, origin),
         date_accessed = as.Date("2023-04-17"),
         URL = "https://doi.org/10.17897/5S51-HE52",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         # lon = NA, lat = NA, # No longer necessary
         depth = NA,
         nomsp = map(Species, latin_eng),
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
## Have NA/0 value
young_bird_nests_hatch <- read_delim("P:/restricted_data/GEM/young/View_BioBasis_Zackenberg_Data_Birds_Bird_breeding_phenology__nests170420231421385886.csv", 
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
         date_bon = as.Date(date_enfonction, origin),
         date_accessed = as.Date("2023-04-17"),
         URL = "https://doi.org/10.17897/5S51-HE52",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         # lon = NA, lat = NA, # No longer necessary
         depth = NA,
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp," eggs hatched [n]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "young",
         value = PulliHatched) %>%
  dplyr::rename(date = date_bon) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))


# Bird abundance
young_bird_abundance <- read_delim("P:/restricted_data/GEM/young/View_BioBasis_Zackenberg_Data_Birds_Breeding_bird_abundance170420231423146922.csv",
                                   col_types = "icinncc",
                                   na = "-9999") %>%
  convert_UTM_deg(utm_zone = 27) %>%
  mutate(date_accessed = as.Date("2023-04-17"),
         URL = "https://doi.org/10.17897/1Z6Z-FQ32",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         # lon = NA, lat = NA, # No longer necessary
         depth = NA,
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp," [presence]"),
         category = "bio",
         driver ="biomass",
         date = as.Date(paste0(Year,"-12-31")),
         type = "in situ",
         site = "young",
         valuez = 1) %>%
  dplyr::group_by(date_accessed, URL, citation, lon, lat, depth, date, variable, category, driver, type, site) %>%
  dplyr::summarise(sum(valuez)) %>%
  mutate(value = 1) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) #%>%
  # filter(!is.na(value))


# Bird breeding phenology broods
## Have NA value
young_bird_broods <- read_delim("P:/restricted_data/GEM/young/View_BioBasis_Zackenberg_Data_Birds_Bird_breeding_phenology__broods210420231531510758.csv",
                                na = c("9999-01-01","-9999","#REF!","01/01/9999")) %>%
  convert_UTM_deg(utm_zone = 27) %>%
  dplyr::rename(date_egg = FirstEggDate) %>%
  mutate(date_enfonction = ifelse(is.na(date_egg), 
                                  as.Date(paste0(Year,"-12-31")), 
                                  as.Date(date_egg)),
         date_bon = as.Date(date_enfonction, origin),
         date_accessed = as.Date("2023-04-21"),
         URL = "https://doi.org/10.17897/YPNZ-VX08",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         # lon = NA, lat = NA, # No longer necessary
         depth = NA,
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp," eggs laid [n]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "young",
         value = Accuracy) %>%
  dplyr::rename(date = date_bon) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value)# %>%
# filter(!is.na(value))







# data set ----------------------------------------------------------------

young_species_GEM <- rbind(young_bird_nests_eggs, 
                    young_bird_nests_hatch,
                    young_bird_abundance,
                    young_bird_broods)

save(young_species_GEM, file = "users/calin/data/young_species_GEM.RData")


young_species_GEM_analysis <- rbind(young_bird_nests_eggs, young_bird_broods)

save(young_species_GEM, file = "users/calin/data/young_species_GEM_analysis.RData")

# OTHER -------------------------------------------------------------------



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


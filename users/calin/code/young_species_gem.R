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
         nomsp = map(nomSpecies, latin_eng),
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
         nomsp = map(nomSpecies, latin_eng),
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
         nomsp = map(nomSpecies, latin_eng),
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
         nomsp = map(nomSpecies, latin_eng),
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
         nomsp = map(nomSpecies, latin_eng),
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
         nomsp = map(nomSpecies, latin_eng),
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
         nomsp = map(nomSpecies, latin_eng),
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
## Manque : Species
## Have NA value
nuup_bird_presence <- read_delim("P:/restricted_data/GEM/nuup/View_BioBasis_Nuuk_Data_Birds_Passerine_bird_abundance170420231432285653.csv") #%>% 
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
         nomsp = map(nomSpecies, latin_eng),
         age = case_when(Age == "J"~"juvenile", 
                         Age == "A"~"adult",
                         Age == "UK"~"unknown"),
         gender = case_when(Gender == "M"~"male", 
                            Gender == "F"~"female",
                            Gender == "UK"~"unknown"),
         variable = paste0(age, " ", gender, " ", nomsp," [n]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "nuup",
         value = Number) %>%
  dplyr::rename(date = Date) %>%
  dplyr::group_by(date_accessed, URL, citation, lon, lat, depth, date, variable, category, driver, type, site) %>% 
  dplyr::summarise(sum(value)) %>% 
  dplyr::rename(value = `sum(value)`) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value)




































# data set ----------------------------------------------------------------

young_data <- rbind(young_bird_nests_eggs, young_bird_nests_hatch,
                    young_bird_abundance,
                    young_phyto_biovolume,
                    young_phyto_number,
                    young_zoo_number_LSlake,
                    young_zoo_number_SSlake,
                    young_bird_broods)

nuup_data <- rbind(nuup_bird_presence)

gem_data <- rbind(young_data, nuup_data) 


gem_data_annual <- gem_data %>% 
  mutate(year = year(date)) %>% 
  summarise(annual_count = n(), .by = c(year, site))

gem_data_species_summury <- gem_data %>% 
  mutate(year = year(date)) %>% 
  dplyr::select(year, variable, site) %>% 
  distinct() %>% 
  summarise(annual_species_count = n(), .by = c(year, site))

gem_data_set_summury <- gem_data %>% 
  mutate(year = year(date)) %>% 
  dplyr::select(year, URL, site) %>% 
  distinct() %>% 
  summarise(annual_set_count = n(), .by = c(year, site))



# Figures -----------------------------------------------------------------
gem_data04 <- ggplot(gem_data_set_summury, aes(x = year, y = annual_set_count)) + 
  geom_bar(aes(fill = site), stat = 'Identity', position = 'dodge')


gem_data03 <- ggplot(gem_data_species_summury, aes(x = year, y = annual_species_count)) + 
  geom_bar(aes(fill = site), stat = 'Identity', position = 'dodge')


gem_data02 <- ggplot(gem_data_annual, aes(x = year, y = annual_count)) + 
  geom_bar(aes(fill = site), stat = 'Identity', position = 'dodge')

gem_data04


gem_data01 <- ggplot(gem_data, aes(x = date, y = site, fill = site)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none", axis.text = element_text(size = 9)) +
  labs(x = NULL, y = NULL)


arctic_data2 <- ggplot(data = gem_data, aes(x = date, y = value)) +
  geom_point(aes(color = variable)) + 
  facet_wrap(~variable, scales = "free_y") +
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL)


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


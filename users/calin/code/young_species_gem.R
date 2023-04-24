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
## Manque : lon,lat
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
## Have NA/0 value
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
## Manque : lon,lat
young_bird_abundance <- read_delim("P:/restricted_data/GEM/young/View_BioBasis_Zackenberg_Data_Birds_Breeding_bird_abundance170420231423146922.csv",
                                   col_types = "icinncc",
                                   na = "-9999") %>%
  mutate(date_accessed = as.Date("2023-04-17"),
         URL = "https://doi.org/10.17897/1Z6Z-FQ32",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         lon = NA, lat = NA, depth = NA,
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

  
# Phytoplankton biovolume
## Manque : lon,lat et Species
## LAKES
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
## LAKES
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
## LAKES
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
## LAKES
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


# Bird breeding phenology broods
## Manque : lon,lat
## Have NA value
young_bird_broods <- read_delim("P:/restricted_data/GEM/young/View_BioBasis_Zackenberg_Data_Birds_Bird_breeding_phenology__broods210420231531510758.csv",
                                na = c("9999-01-01","-9999","#REF!","01/01/9999")) %>%
  dplyr::rename(date_egg = FirstEggDate) %>%
  mutate(date_enfonction = ifelse(is.na(date_egg), 
                                  as.Date(paste0(Year,"-12-31")), 
                                  as.Date(date_egg)),
         date_bon = as.Date(date_enfonction, origin),
         date_accessed = as.Date("2023-04-21"),
         URL = "https://doi.org/10.17897/YPNZ-VX08",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         lon = NA, lat = NA, depth = NA,
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

young_data <- rbind(young_bird_nests_eggs, 
                    young_bird_nests_hatch,
                    young_bird_abundance,
                    young_phyto_biovolume,
                    young_phyto_number,
                    young_zoo_number_LSlake,
                    young_zoo_number_SSlake,
                    young_bird_broods)

nuup_data <- rbind(nuup_bird_presence)

gem_data <- rbind(young_data, nuup_data) 

EU_arctic_data



ECC_data <- rbind(gem_data, EU_arctic_data) %>% 
  mutate(lieu = case_when(site == "barents sea"~"barents sea",
                          site == "east ice"~"barents sea",
                          site == "is"~"is",
                          site == "kong"~"kong",
                          site == "nuup"~"nuup",
                          site == "svalbard"~"svalbard",
                          site == "west ice"~"barents sea",
                          site == "young"~"young"))



ECC_data_annual <- ECC_data %>% 
  mutate(year = year(date)) %>% 
  summarise(annual_count = n(), .by = c(year, lieu))


ECC_data_annual_type <- ECC_data %>% 
  mutate(year = year(date),
         fish = case_when(grepl("|FIS|", variable, fixed = TRUE) == TRUE~"Fish"),
         mammal = case_when(grepl("|MAM|", variable, fixed = TRUE) == TRUE~"Mammal"),
         bird = case_when(grepl("|BIR|", variable, fixed = TRUE) == TRUE~"Bird",
                          grepl("|bir|", variable, fixed = TRUE) == TRUE~"Bird"),
         zoo = case_when(grepl("Zoo", variable, fixed = TRUE) == TRUE~"Zooplankton",
                         grepl("|ZOO|", variable, fixed = TRUE) == TRUE~"Zooplankton",
                         grepl("zoo", variable, fixed = TRUE) == TRUE~"Zooplankton"),
         phyto = case_when(grepl("phyto", variable, fixed = TRUE) == TRUE~"Phytoplankton"),
         classification = case_when(fish == "Fish"~"fish",
                        mammal == "Mammal" ~"mammal",
                        bird == "Bird"~"bird",
                        zoo == "Zooplankton"~"zooplankton",
                        phyto == "Phytoplankton"~"phytoplankton")) %>%
  dplyr::select(year, classification, lieu) %>% 
  summarise(annual_type_count = n(), .by = c(year, classification))

ECC_data_annual_variable <- ECC_data %>% 
  mutate(year = year(date)) %>%
  dplyr::select(year, variable, lieu) %>% 
  summarise(annual_type_count = n(), .by = variable)


ECC_data_type <- ECC_data %>% 
  mutate(year = year(date),
         fish = case_when(grepl("|FIS|", variable, fixed = TRUE) == TRUE~"Fish"),
         mammal = case_when(grepl("|MAM|", variable, fixed = TRUE) == TRUE~"Mammal"),
         bird = case_when(grepl("|BIR|", variable, fixed = TRUE) == TRUE~"Bird",
                          grepl("|bir|", variable, fixed = TRUE) == TRUE~"Bird"),
         zoo = case_when(grepl("Zoo", variable, fixed = TRUE) == TRUE~"Zooplankton",
                         grepl("|ZOO|", variable, fixed = TRUE) == TRUE~"Zooplankton",
                         grepl("zoo", variable, fixed = TRUE) == TRUE~"Zooplankton"),
         phyto = case_when(grepl("phyto", variable, fixed = TRUE) == TRUE~"Phytoplankton"),
         classification = case_when(fish == "Fish"~"fish",
                                    mammal == "Mammal" ~"mammal",
                                    bird == "Bird"~"bird",
                                    zoo == "Zooplankton"~"zooplankton",
                                    phyto == "Phytoplankton"~"phytoplankton")) %>%
  dplyr::select(year, classification, value) %>% 
  summarise(type_count = n(), .by = classification)


ECC_data_species_summury <- ECC_data %>% 
  mutate(year = year(date)) %>% 
  dplyr::select(year, variable, lieu) %>% 
  distinct() %>% 
  summarise(annual_species_count = n(), .by = c(year, lieu))

ECC_data_set_summury <- ECC_data %>% 
  mutate(year = year(date)) %>% 
  dplyr::select(year, URL, lieu) %>% 
  distinct() %>% 
  summarise(annual_set_count = n(), .by = c(year, lieu))



# Figures -----------------------------------------------------------------
ECC_data01 <- ggplot(ECC_data_annual, aes(x = year, y = annual_count)) + 
  geom_bar(aes(fill = lieu), stat = 'Identity', position = 'dodge') +
  labs(y = "data [n]", title = "Data by site and by year", x = NULL, fill = "site") +
  theme(legend.position = c(0.20,0.80)) +
  scale_fill_brewer(palette="Set3")
ECC_data01
ggsave(ECC_data01, file = 'users/calin/figures/ECC_data01.png')

ECC_data02 <- ggplot(ECC_data_species_summury, aes(x = year, y = annual_species_count)) + 
  geom_bar(aes(fill = lieu), stat = 'Identity', position = 'dodge') +
  labs(y = "species [n]", x = NULL) +
  theme(legend.position = c(0.20,0.80)) +
  scale_fill_brewer(palette="Set3")
ECC_data02
ggsave(ECC_data02, file = 'users/calin/figures/ECC_data02.png')


ECC_data03 <- ggplot(ECC_data_set_summury, aes(x = year, y = annual_set_count)) + 
  geom_bar(aes(fill = lieu), stat = 'Identity', position = 'dodge') +
  labs(y = "set [n]", title = "Data by set and by year", x = NULL) +
  theme(legend.position = c(0.20,0.80)) +
  scale_fill_brewer(palette="Set2")
ECC_data03

ECC_data04 <- ggplot(ECC_data_annual_type, aes(x = year, y = annual_type_count)) + 
  geom_bar(aes(fill = classification), stat = 'Identity', position = 'dodge') +
  labs(y = "data [n]", title = "Data by classification and by year", x = NULL) +
  theme(legend.position = c(0.20,0.80)) +
  scale_fill_brewer(palette="Set2")
ECC_data04

ECC_data05 <- ggplot(ECC_data_type, aes(x="", y = type_count, fill=classification)) +
  geom_bar(stat="identity", width=1, color="white") +
  labs(fill = "species groups") +
  coord_polar("y", start=0)+
  theme_void() + # remove background, grid, numeric labels
  scale_fill_brewer(palette="Set2")
ECC_data05
ggsave(ECC_data05, file = 'users/calin/figures/ECC_data05.png')


ECC_data06 <- ggplot(ECC_data, aes(x = year(date), y = lieu, fill = lieu)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none", axis.text = element_text(size = 9)) +
  labs(x = NULL, y = NULL) +
  scale_fill_brewer(palette="Set3")
ECC_data06
ggsave(ECC_data06, file = 'users/calin/figures/ECC_data06.png')






gem_data04 <- ggplot(gem_data_set_summury, aes(x = year, y = annual_count)) + 
  geom_bar(aes(fill = lieu), stat = 'Identity', position = 'dodge')


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


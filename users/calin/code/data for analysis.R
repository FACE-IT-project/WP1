# code/data_for_analysis
# Species data cleaning for the analysis script

# Set up ------------------------------------------------------------------
source('users/calin/code/formulas.R')

library(tidyverse)
library(janitor)
library(stringi)


load('users/calin/data/EU_arctic_data.RData') # firstsetdata data -> svalbard data
load('users/calin/data/young_species_GEM.RData') # young_species_gem data -> young GEM data
load('users/calin/data/nuup_species_GEM.RData') # nuup_species_gem data -> nuup GEM data
load('users/calin/data/nuup_bird_nb_GEM.RData') # nuup_species_gem data -> nuup GEM data
load('users/calin/data/nuup_seabird_count_GEM.RData') # nuup_species_gem data -> nuup GEM data
load('users/calin/data/young_bird_nests_hatch_GEM.RData') 
load('users/calin/data/young_bird_broods_GEM.RData')

Data_ori <- rbind(EU_arctic_data, young_species_GEM, nuup_species_GEM)


young_species_GEM


NUUP_1_n <- nuup_bird_nb_GEM %>% 
  # transform( nuup_bird_nb_GEM, TimeSeries_id = as.numeric(factor(`URL`))) %>% 
  # filter(!grepl('presence', variable)) %>% # Create the TimeSeries_id by URL
  mutate(Speci = substr(gsub("\\(.*", "", variable), start = 7, stop = 80), # Take the latin name
         Taxon = stri_replace_last(Speci, replacement = "", regex = " "), # Delete the last 'space'
         Year = year(date),
         Site = "nuup",
         TimeSeries_id = 1) %>% 
  dplyr::group_by(Site, TimeSeries_id, Year, Taxon) %>%
  dplyr::summarise(sum(value)) %>% 
  dplyr::rename(Density = `sum(value)`) %>%
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density)

NUUP_2_n <- nuup_seabird_count_GEM %>% 
  # transform( nuup_bird_nb_GEM, TimeSeries_id = as.numeric(factor(`URL`))) %>% 
  # filter(!grepl('presence', variable)) %>% # Create the TimeSeries_id by URL
  mutate(Speci = substr(gsub("\\(.*", "", variable), start = 7, stop = 80), # Take the latin name
         Taxon = stri_replace_last(Speci, replacement = "", regex = " "), # Delete the last 'space'
         Year = year(date),
         Site = "nuup",
         TimeSeries_id = 2) %>% 
  dplyr::group_by(Site, TimeSeries_id, Year, Taxon) %>%
  dplyr::summarise(sum(value)) %>% 
  dplyr::rename(Density = `sum(value)`) %>%
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density)


YOUNG_1_n <- young_bird_nests_hatch_GEM %>% 
  filter(!is.na(value)) %>%
  mutate(Speci = substr(gsub("\\(.*", "", variable), start = 7, stop = 80), # Take the latin name
         Taxon = stri_replace_last(Speci, replacement = "", regex = " "), # Delete the last 'space'
         Year = year(date),
         Site = "young",
         TimeSeries_id = 3) %>% 
  dplyr::group_by(Site, TimeSeries_id, Year, Taxon) %>%
  dplyr::summarise(sum(value)) %>% 
  dplyr::rename(Density = `sum(value)`) %>%
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density)


YOUNG_2_n <- young_bird_broods_GEM %>% 
  filter(!is.na(value)) %>%
  mutate(Speci = substr(gsub("\\(.*", "", variable), start = 7, stop = 80), # Take the latin name
         Taxon = stri_replace_last(Speci, replacement = "", regex = " "), # Delete the last 'space'
         Year = year(date),
         Site = "young",
         TimeSeries_id = 4) %>% 
  dplyr::group_by(Site, TimeSeries_id, Year, Taxon) %>%
  dplyr::summarise(sum(value)) %>% 
  dplyr::rename(Density = `sum(value)`) %>%
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density)


        
# Site = site name, TimeSeries_id = unique identifier for the time series, Year = survey year, Taxon = taxon name, Density = total density or biomass or number of individual of that taxon for that year.
































# Data --------------------------------------------------------------------
# Site = site name, TimeSeries_id = unique identifier for the time series, Year = survey year, Taxon = taxon name, Density = total density or biomass or number of individual of that taxon for that year. 

# svalbard ivory gull population [n]
A_sva_iv_gull_pop <- read.csv("P:/FACE-IT_data/svalbard/the-number-of-breeding-p.csv", 
                                           sep = ";") %>% # read the csv
  mutate(Taxon = "Pagophila eburnea",
         TimeSeries_id = 1,
         Site = "svalbard", 
         Year = year(as.Date(paste0(Category,"-12-31")))) %>% 
  dplyr::rename(Density = Svalbard) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))


# svalbard walrus population [n]
A_sva_walrus_pop <- read_delim("P:/FACE-IT_data/svalbard/walrus-population-in-sva.csv") %>% 
  mutate(Taxon = "Odobenus marinus",
         TimeSeries_id = 2,
         Site = "svalbard", 
         Year = year(as.Date(paste0(Category,"-12-31")))) %>% 
  dplyr::rename(Density = `Walrus population aerial counts`) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))


# svalbard (north and west) calanus population by size [g/m²]
A_sva_calanus_nw_pop <- read.csv("P:/FACE-IT_data/svalbard/average-biomass-of-zoopl.csv", sep = ";", dec = ",") %>%
  mutate(Taxon = "Calanus",
         TimeSeries_id = 3,
         Site = "svalbard", 
         Year = year(as.Date(paste0(Category,"-12-31")))) %>% 
  dplyr::rename(Density = `Total`) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))


# svalbard (south and east) calanus population by size [g/m²]
A_sva_calanus_se_pop <- read.csv("P:/FACE-IT_data/svalbard/average-biomass-of-zoopl (2).csv", sep = ";", dec = ",") %>%
  mutate(Taxon = "Calanus",
         TimeSeries_id = 4,
         Site = "svalbard", 
         Year = year(as.Date(paste0(Category,"-12-31")))) %>% 
  dplyr::rename(Density = `Total`) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))


# svalbard  kittiwake population [%]
A_sva_kitti_pop <- read.csv("P:/FACE-IT_data/svalbard/black-legged-kittiwake-p.csv", sep = ";", dec = ",") %>%
  mutate(Taxon = "Rissa tridactyla",
         TimeSeries_id = 5,
         Site = "svalbard", 
         Year = year(as.Date(paste0(Category,"-12-31")))) %>% 
  dplyr::rename(Density = `Bjørnøya`) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))


# svalbard Brünnich’s guillemot population [%]
A_sva_brg_pop <- read.csv("P:/FACE-IT_data/svalbard/brnnichs-guillemot-breed.csv", sep = ";", dec = ",") %>%
  mutate(Taxon = "Uria lomvia",
         TimeSeries_id = 6,
         Site = "svalbard", 
         Year = year(as.Date(paste0(Category,"-12-31")))) %>% 
  dplyr::rename(Density = `Fuglehuken`) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))


# svalbard Hooded seal population [n]
A_sva_cycr_pop <- read.csv("P:/FACE-IT_data/svalbard/population-size-of-hoode.csv", sep = ";", dec = ",") %>%
  mutate(Taxon = "Cystophora cristata",
         TimeSeries_id = 7,
         Site = "svalbard", 
         Year = year(as.Date(paste0(Category,"-12-31")))) %>% 
  dplyr::rename(Density = `Modelled.total.stock.size`) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))



## Kong data ---------------------------------------------------------------

# kong glaucous gull population [%]
A_kong_gl_gull_pop <- read_delim("P:/FACE-IT_data/kongsfjorden/glaucous-gull-population.csv") %>%
  mutate(Taxon = "Larus hyperboreus",
         TimeSeries_id = 8,
         Site = "kong", 
         Year = year(as.Date(paste0(Category,"-12-31")))) %>% 
  dplyr::rename(Density = `Kongsfjorden`) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))


# kong eiders [n]
A_kong_eider_pop <- read.csv("P:/FACE-IT_data/kongsfjorden/breeding-population-of-c.csv", sep = ";") %>%
  mutate(Taxon = "Somateria mollissima",
         TimeSeries_id = 9,
         Site = "kong", 
         Year = year(as.Date(paste0(Category,"-12-31")))) %>% 
  dplyr::rename(Density = `Common.eider`) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))

# kong seabird [n]
A_kong_seabird <- read.csv("P:/FACE-IT_data/kongsfjorden/Descamps_Strom_Ecology_data.csv", sep = ",", skip = 3, header = TRUE) %>%
  remove_empty(which = "cols") %>% 
  filter(Colony == "Kongsfjorden") %>%
  mutate(Taxon = "Larus hyperboreus",
         TimeSeries_id = 10,
         Site = "kong", 
         Year = YR) %>% 
  dplyr::rename(Density = `Count`) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))

# kong calanus population [%]
A_kong_calanus_pop <- read.csv("P:/FACE-IT_data/kongsfjorden/calanus-species-composit.csv", sep = ";", dec = ",") %>%
  dplyr::rename(`Calanus finmarchicus` = `Proportion.of.Atlantic.species`) %>%
  dplyr::rename(`Calanus glacialis` = `Proportion.of.Arctic.species`) %>%
  pivot_longer(cols = c(`Calanus finmarchicus`, `Calanus glacialis`)) %>% 
  mutate(Taxon = name,
         TimeSeries_id = 11,
         Site = "kong", 
         Year = year(as.Date(paste0(Category,"-12-31")))) %>% 
  dplyr::rename(Density = `value`) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))


# kong  kittiwake population [%]
A_kong_kitti_pop <- read.csv("P:/FACE-IT_data/svalbard/black-legged-kittiwake-p.csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`Fuglehuken`, `Bjørnøya`, `Grumant`, `Sofiekammen`, `Ossian.Sars`, `Tschermakfjellet`, `Alkhornet`, `Amsterdamya`)) %>%
  filter(name == "Ossian.Sars") %>% 
  mutate(Taxon = "Rissa tridactyla",
         TimeSeries_id = 12,
         Site = "kong", 
         Year = year(as.Date(paste0(Category,"-12-31")))) %>% 
  dplyr::rename(Density = `value`) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))


# kong Brünnich’s guillemot population [%]
A_kong_brg_pop <- read.csv("P:/FACE-IT_data/svalbard/brnnichs-guillemot-breed.csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`Diabas`, `Alkhornet`, `Sofiekammen`, `Grumant`, `Tschermakfjellet`, `Fuglehuken`, `Ossian.Sarsfjellet`, `Bjørnøya..southern.part`, `Bjørnøya..Evjebukta`, `Jan.Mayen`)) %>%
  filter(name == "Ossian.Sarsfjellet") %>% 
  mutate(Taxon = "Uria lomvia",
         TimeSeries_id = 13,
         Site = "kong", 
         Year = year(as.Date(paste0(Category,"-12-31")))) %>% 
  dplyr::rename(Density = `value`) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))


# barents polar cod biomass [10^6 kg]
A_ba_polar_cod <- read.csv("P:/FACE-IT_data/svalbard/biomass-of-polar-cod-in.csv", sep = ";", dec = ",") %>% 
  mutate(Taxon = "Boreogadus saida",
         TimeSeries_id = 14,
         Site = "barents", 
         Year = year(as.Date(paste0(Category,"-12-31")))) %>% 
  dplyr::rename(Density = `Biomass`) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))


# barents capelin stock [10^6 kg]
A_ba_cap_st <- read.csv("P:/FACE-IT_data/svalbard/capelin-stock-in-the-bar.csv",sep = ";" , dec = ",") %>% 
  mutate(Biomass = `Mature.stock`+`Immature.stock`, 
         Taxon = "Mallotus villosus",
         TimeSeries_id = 15,
         Site = "barents", 
         Year = year(as.Date(paste0(Category,"-12-31")))) %>% 
  dplyr::rename(Density = `Biomass`) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))


# barents golden redfish population [10^3 kg]
A_ba_gfish_pop <- read_delim("P:/FACE-IT_data/svalbard/stock-of-golden-redfish.csv") %>% 
  mutate(Biomass = `Mature stock`+`Immature stock`, 
         Taxon = "Sebastes norvegicus",
         TimeSeries_id = 16,
         Site = "barents", 
         Year = year(as.Date(paste0(Category,"-12-31")))) %>% 
  dplyr::rename(Density = `Biomass`) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))
 

# barents beaked redfish population [10^6 kg]
A_ba_bfish_pop <- read.csv("P:/FACE-IT_data/svalbard/stock-of-beaked-redfish.csv", sep = ";") %>% 
  mutate(Biomass = `Mature.stock`+`Immature.stock`, 
         Taxon = "Sebastes mentella",
         TimeSeries_id = 17,
         Site = "barents", 
         Year = year(as.Date(paste0(Category,"-12-31")))) %>% 
  dplyr::rename(Density = `Biomass`) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))


# barents northeast arctic cod population [10^6 kg]
A_ba_ncod_pop <- read.csv("P:/FACE-IT_data/svalbard/stock-of-northeast-arcti.csv", sep = ";") %>% 
  mutate(Biomass = `Immature.stock` + `Spawning.stock`, 
         Taxon = "Gadus morhua",
         TimeSeries_id = 18,
         Site = "barents", 
         Year = year(as.Date(paste0(Category,"-12-31")))) %>% 
  dplyr::rename(Density = `Biomass`) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))


# barents young herring population [n]
A_ba_yherring_pop <- read.csv("P:/FACE-IT_data/svalbard/biomass-index-for-young.csv", sep = ";", dec = ",") %>% 
  mutate(Biomass = `X1.year.olds` + `X2.year.old` + `X3.year.old`, 
         Taxon = "Clupea harengus",
         TimeSeries_id = 19,
         Site = "barents", 
         Year = year(as.Date(paste0(Category,"-12-31")))) %>% 
  dplyr::rename(Density = `Biomass`) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))

# Bird breeding phenology nests eggs [n]
A_yo_b_eggs <- read_delim("P:/restricted_data/GEM/young/View_BioBasis_Zackenberg_Data_Birds_Bird_breeding_phenology__nests170420231421385886.csv", 
                          na = c("9999-01-01","-9999"), 
                          col_types = "iccnnDDiiicc") %>%
  dplyr::group_by(Year, Species) %>%
  dplyr::summarise(sum(EggsLaid)) %>%
  dplyr::rename(Density = `sum(EggsLaid)`) %>% 
  mutate(Site = "young",
         TimeSeries_id = 20,
         Taxon = paste0(`Species`, " eggs")) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))


# Bird presence [n]
A_nu_bird_nb <- read_delim("P:/restricted_data/GEM/nuup/View_BioBasis_Nuuk_Data_Birds_Passerine_bird_abundance170420231432285653.csv") %>% 
  mutate(Year = year(as.Date(paste0(Date,"-12-31")))) %>% 
  dplyr::group_by(Year, Species) %>%
  dplyr::summarise(sum(Number)) %>%
  dplyr::rename(Density = `sum(Number)`) %>%
  mutate(Site = "nuup",
         TimeSeries_id = 21,
         Taxon = case_when(Species == "LB"~"Calcarius lapponicus", 
                           Species == "NW"~"Oenanthe oenanthe", 
                           Species == "RP"~"Carduelis flammea", 
                           Species == "SB"~"Plectrophenax nivalis")) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))


# Seabird counting [n]
A_nu_seabird <- read_delim("P:/restricted_data/GEM/nuup/View_MarineBasis_Nuuk_Data_Seabirds_Seabird_species_counts_per_colony17042023154835389.csv",
                                 na = c("NULL","-1", "2017-07-00")) %>% 
  filter(!is.na(Date)) %>% 
  filter(!is.na(Latin)) %>%
  filter(!is.na(MinNumbers)) %>%
  mutate(moy = ceiling((MinNumbers+MaxNumbers)/2),
         value = ifelse(!is.na(moy), moy, MinNumbers),
         Year = year(Date)) %>% 
  dplyr::group_by(Year, Latin) %>%
  dplyr::summarise(sum(value)) %>%
  dplyr::rename(Density = `sum(value)`) %>%
  dplyr::rename(Taxon = `Latin`) %>%
  mutate(Site = "nuup",
         TimeSeries_id = 22) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))


# marin mammal [n]
A_nu_mmam <- read_delim("P:/restricted_data/GEM/nuup/View_MarineBasis_Nuuk_Data_Marine_mammals_Identification_of_Humpback_Whales_individuals_year170420231542310109.csv") %>%
  mutate(Taxon = "Megaptera novaeangliae",
         TimeSeries_id = 23,
         Site = "nuup") %>% 
  dplyr::rename(Year = `YEAR`) %>%
  dplyr::rename(Density = INDIVIDUALS) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))





data_n_for_analysis <- rbind(A_sva_iv_gull_pop, 
                             A_sva_walrus_pop,
                             A_sva_cycr_pop, 
                             #A_kong_eider_pop, 
                             #A_kong_seabird, 
                             #A_ba_yherring_pop,
                             A_yo_b_eggs,
                             A_nu_bird_nb,
                             A_nu_seabird,
                             A_nu_mmam) 
  # dplyr::group_by(Year, Taxon, Site) %>%
  # dplyr::summarise(count = n())

data_100_for_analysis <- rbind(A_sva_kitti_pop, 
                               A_sva_brg_pop, 
                               A_kong_gl_gull_pop,
                               A_kong_calanus_pop,
                               A_kong_kitti_pop, 
                               A_kong_brg_pop)


data_gm_for_analysis <- rbind(A_sva_calanus_nw_pop, 
                              A_sva_calanus_se_pop)


data_6kg_for_analysis <- rbind(A_ba_polar_cod, 
                               A_ba_cap_st, 
                               A_ba_bfish_pop, 
                               A_ba_ncod_pop)

data_3kg_for_analysis <- rbind(A_ba_gfish_pop)


data_nuup_analysis <- rbind(A_nu_bird_nb,
                            A_nu_seabird,
                            A_nu_mmam)


















































































## Is data -----------------------------------------------------------------

# is  kittiwake population
is_kittiwakke_population <- read.csv("P:/FACE-IT_data/svalbard/black-legged-kittiwake-p.csv", sep = ";", dec = ",") %>%
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
         type = "in situ",
         site = "is",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))


# is Brünnich’s guillemot population
is_brguillemot_population <- read.csv("P:/FACE-IT_data/svalbard/brnnichs-guillemot-breed.csv", sep = ";", dec = ",") %>%
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
         type = "in situ",
         site = "svalbard",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))


# EU ----------------------------------------------------------------------
# EU (east) harp seal population
EU_epagr_population <- read.csv("P:/FACE-IT_data/EU_arctic/production-of-pups-and-e.csv", sep = ";", dec = ",") %>%
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
         type = "in situ",
         site = "east ice",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))


# EU (west) harp seal population
EU_wpagr_population <- read.csv("P:/FACE-IT_data/EU_arctic/production-of-pups-and-e (1).csv", sep = ";", dec = ",") %>%
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
         type = "in situ",
         site = "west ice",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))

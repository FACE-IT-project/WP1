# Code/firstsetdata.R
# tests data first day


# Set up ------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(ggridges)
library(ggpubr)
library(stringi)




# Data -----------------------------------------------------------
## Svalbard data -----------------------------------------------------------

# ivory gull population

svalbard_ivory_gull_population <- read.csv("P:/FACE-IT_data/svalbard/the-number-of-breeding-p.csv", sep = ";") %>% 
  mutate(date_accessed = as.Date("2023-04-12"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/ivory-gull/", 
         citation = "Norwegian Polar Institute (2022). The number of breeding pairs of ivory gulls in Svalbard. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/ismaake.html", 
         lon = NA, lat = NA, depth = NA, 
         variable = "Pagophila eburnea (ivory gull) breeding population [%]",
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "svalbard", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::rename(value = Svalbard) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))


# svalbard walrus population

svalbard_walrus_population <- read_delim("P:/FACE-IT_data/svalbard/walrus-population-in-sva.csv") %>% 
  pivot_longer(cols = c(`Walrus estimated numbers`, `Walrus population aerial counts`)) %>% 
  mutate(date_accessed = as.Date("2023-04-13"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/walrus/", 
         citation = "Norwegian Polar Institute (2022). Walrus population in Svalbard. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/walrus-population.html", 
         lon = NA, lat = NA, depth = NA, 
         type = case_when(name == "Walrus estimated numbers"~"estimated", 
                          name == "Walrus population aerial counts"~"aerial survey"),
         variable = "Odobenus marinus (walrus) [n]",
         category = "bio",
         driver ="biomass",
         site = "svalbard", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))


# svalbard (north and west) calanus population by size

svalbard_nw_calanus_mm_population <- read.csv("P:/FACE-IT_data/svalbard/average-biomass-of-zoopl.csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`X0.18.mm`, `X1.0.mm`, `X2.0.mm`, `Total`)) %>%
  filter(!name == "Total") %>%
  mutate(date_accessed = as.Date("2023-04-13"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/zooplankton-biomass-in-the-barents-sea/",
         citation = "Institute of Marine Research (2023). Average biomass of zooplankton in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/zooplankton-biomass.html",
         lon = NA, lat = NA, depth = NA,
         variable = paste0(substr(stri_replace_last(tolower(name)," ", regex = "[.]"), 2, 10)," calanus [g/m²]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "svalbard",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))


# svalbard (north and west) calanus population tot

svalbard_nw_calanus_tot_population <- read.csv("P:/FACE-IT_data/svalbard/average-biomass-of-zoopl.csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`X0.18.mm`, `X1.0.mm`, `X2.0.mm`, `Total`)) %>%
  filter(name == "Total") %>%
  mutate(date_accessed = as.Date("2023-04-13"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/zooplankton-biomass-in-the-barents-sea/",
         citation = "Institute of Marine Research (2023). Average biomass of zooplankton in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/zooplankton-biomass.html",
         lon = NA, lat = NA, depth = NA,
         variable = "calanus [g/m²]",
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "svalbard",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))


# svalbard (south and east) calanus population by size

svalbard_se_calanus_mm_population <- read.csv("P:/FACE-IT_data/svalbard/average-biomass-of-zoopl (2).csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`X0.18.mm`, `X1.0.mm`, `X2.0.mm`, `Total`)) %>%
  filter(!name == "Total") %>%
  mutate(date_accessed = as.Date("2023-04-13"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/zooplankton-biomass-in-the-barents-sea/",
         citation = "Institute of Marine Research (2023). Average biomass of zooplankton in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/zooplankton-biomass.html",
         lon = NA, lat = NA, depth = NA,
         variable = paste0(substr(stri_replace_last(tolower(name)," ", regex = "[.]"), 2, 10)," calanus [g/m²]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "svalbard",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))


# svalbard (south and east) calanus population tot

svalbard_se_calanus_tot_population <- read.csv("P:/FACE-IT_data/svalbard/average-biomass-of-zoopl (2).csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`X0.18.mm`, `X1.0.mm`, `X2.0.mm`, `Total`)) %>%
  filter(name == "Total") %>%
  mutate(date_accessed = as.Date("2023-04-13"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/zooplankton-biomass-in-the-barents-sea/",
         citation = "Institute of Marine Research (2023). Average biomass of zooplankton in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/zooplankton-biomass.html",
         lon = NA, lat = NA, depth = NA,
         variable = "calanus [g/m²]",
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "svalbard",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))


# svalbard  kittiwake population

svalbard_kittiwakke_population <- read.csv("P:/FACE-IT_data/svalbard/black-legged-kittiwake-p.csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`Fuglehuken`, `Bjørnøya`, `Grumant`, `Sofiekammen`, `Ossian.Sars`, `Tschermakfjellet`, `Alkhornet`, `Amsterdamya`)) %>%
  filter(name == "Bjørnøya") %>% 
  mutate(date_accessed = as.Date("2023-04-13"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/black-legged-kittiwake/",
         citation = "Norwegian Polar Institute (2022). Black-legged kittiwake population size, as percentage of the average in the colony. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/black-legged-kittiwake.html",
         lon = NA, lat = NA, depth = NA,
         variable = "Rissa tridactyla (kittiwake) population [% average in the colony]",
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "svalbard",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))


# svalbard Brünnich’s guillemot population

svalbard_brguillemot_population <- read.csv("P:/FACE-IT_data/svalbard/brnnichs-guillemot-breed.csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`Diabas`, `Alkhornet`, `Sofiekammen`, `Grumant`, `Tschermakfjellet`, `Fuglehuken`, `Ossian.Sarsfjellet`, `Bjørnøya..southern.part`, `Bjørnøya..Evjebukta`, `Jan.Mayen`)) %>%
  filter(name == "Fuglehuken") %>% 
  mutate(date_accessed = as.Date("2023-04-14"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/brunnichs-guillemot/",
         citation = "Norwegian Polar Institute (2022). Brünnich’s guillemot breeding populations, percentage of colony average. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/brunnichs-guillemot.html",
         lon = NA, lat = NA, depth = NA,
         variable = "Uria lomvia (brünnich’s guillemot) [% average in the colony]",
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "svalbard",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))


# svalbard Hooded seal population

  svalbard_cycr_population <- read.csv("P:/FACE-IT_data/svalbard/population-size-of-hoode.csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`Modelled.production.of.pups`, `Modelled.total.stock.size`, `Survey.counts.of.pups`)) %>% 
  mutate(date_accessed = as.Date("2023-04-14"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/hooded-seal/",
         citation = "Institute of Marine Research (2022). Population size of hooded seals in the West Ice. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/hooded-seal.html",
         lon = NA, lat = NA, depth = NA,
         variable = paste0(str_replace_all(tolower(name),"\\."," ")," hooded seal [n]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "svalbard",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))

  
  # svalbard (east) harp seal population
  
  svalbard_cycr_population <- read.csv("P:/FACE-IT_data/svalbard/population-size-of-hoode.csv", sep = ";", dec = ",") %>%
    pivot_longer(cols = c(`Modelled.production.of.pups`, `Modelled.total.stock.size`, `Survey.counts.of.pups`)) %>% 
    mutate(date_accessed = as.Date("2023-04-14"),
           URL = "https://mosj.no/en/indikator/fauna/marine-fauna/hooded-seal/",
           citation = "Institute of Marine Research (2022). Population size of hooded seals in the West Ice. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/hooded-seal.html",
           lon = NA, lat = NA, depth = NA,
           variable = paste0(str_replace_all(tolower(name),"\\."," ")," hooded seal [n]"),
           category = "bio",
           driver ="biomass",
           type = "in situ",
           site = "svalbard",
           date = as.Date(paste0(Category,"-12-31"))) %>%
    dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>%
    filter(!is.na(value))





## Kong data ---------------------------------------------------------------

# kong glaucous gull population

kong_glaucous_gull_population <- read_delim("P:/FACE-IT_data/kongsfjorden/glaucous-gull-population.csv") %>% 
  mutate(date_accessed = as.Date("2023-04-11"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/glaucous-gull/", 
         citation = "Norwegian Polar Institute (2022). Glaucous gull population, as percentage of the average in the colony. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/glaucous-gull.html", 
         lon = NA, lat = NA, depth = NA, 
         variable = "Larus hyperboreus (glaucous gull population) [% average in the colony]",
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "kong", 
         date = as.Date(paste0(Category,"-12-31"))
  ) %>% 
  dplyr::rename(value = Kongsfjorden) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))



# kong eiders

kong_eiders_stock <- read.csv("P:/FACE-IT_data/kongsfjorden/breeding-population-of-c.csv", sep = ";") %>% 
  mutate(date_accessed = as.Date("2023-04-12"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/common-eider/", 
         citation = "Norwegian Polar Institute (2022). Breeding population of common eiders in Kongsfjorden, number of breeding pairs. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/common-eider.html", 
         lon = NA, lat = NA, depth = NA, 
         variable = "Somateria mollissima borealis (common eider) breeding pairs [n]",
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "kong", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::rename(value = Common.eider) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))


# kong seabird

kong_seabird <- read.csv("P:/FACE-IT_data/kongsfjorden/Descamps_Strom_Ecology_data.csv", sep = ",", skip = 3, header = TRUE) %>%
  remove_empty() %>% 
  filter(Colony == "Kongsfjorden") %>% 
  mutate(date_accessed = as.Date("2023-04-12"), 
         URL = "https://data.npolar.no/dataset/0ea572cd-1e4c-47a3-b2a5-5d7cc75aaeb4", 
         citation = "Descamps, S., & Strøm, H. (2021). Seabird monitoring data from Svalbard, 2009-2018 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2021.0ea572cd", 
         lon = NA, lat = NA, depth = NA, 
         Species = case_when(Species == "GLGU"~"Larus hyperboreus (glaucous gull)"),
         variable = paste0(tolower(Species), " colony count [n]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "kong", 
         date = as.Date(paste0(YR,"-12-31"))) %>% 
  dplyr::rename(value = Count) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))


# kong calanus population

kong_calanus_population <- read.csv("P:/FACE-IT_data/kongsfjorden/calanus-species-composit.csv", sep = ";", dec = ",") %>% 
  pivot_longer(cols = c(`Proportion.of.Atlantic.species`, `Proportion.of.Arctic.species`)) %>% 
  mutate(date_accessed = as.Date("2023-04-13"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/zooplankton-species-composition-in-kongsfjorden/", 
         citation = "Norwegian Polar Institute (2022). Calanus species composition in Kongsfjorden. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/zooplankton-species-composition.html", 
         lon = NA, lat = NA, depth = NA, 
         variable = paste0("calanus ", substr(str_replace_all(tolower(name),"\\."," "),14, 30)," [%]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "kong", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))


# kong  kittiwake population

kong_kittiwakke_population <- read.csv("P:/FACE-IT_data/svalbard/black-legged-kittiwake-p.csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`Fuglehuken`, `Bjørnøya`, `Grumant`, `Sofiekammen`, `Ossian.Sars`, `Tschermakfjellet`, `Alkhornet`, `Amsterdamya`)) %>%
  filter(name == "Ossian.Sars") %>% 
  mutate(date_accessed = as.Date("2023-04-13"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/black-legged-kittiwake/",
         citation = "Norwegian Polar Institute (2022). Black-legged kittiwake population size, as percentage of the average in the colony. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/black-legged-kittiwake.html",
         lon = NA, lat = NA, depth = NA,
         variable = "Rissa tridactyla (kittiwake) population [% average in the colony]",
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "kong",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))


# kong Brünnich’s guillemot population

kong_brguillemot_population <- read.csv("P:/FACE-IT_data/svalbard/brnnichs-guillemot-breed.csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`Diabas`, `Alkhornet`, `Sofiekammen`, `Grumant`, `Tschermakfjellet`, `Fuglehuken`, `Ossian.Sarsfjellet`, `Bjørnøya..southern.part`, `Bjørnøya..Evjebukta`, `Jan.Mayen`)) %>%
filter(name == "Ossian.Sarsfjellet") %>% 
  mutate(date_accessed = as.Date("2023-04-14"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/brunnichs-guillemot/",
         citation = "Norwegian Polar Institute (2022). Brünnich’s guillemot breeding populations, percentage of colony average. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/brunnichs-guillemot.html",
         lon = NA, lat = NA, depth = NA,
         variable = "Uria lomvia (brünnich’s guillemot) [% average in the colony]",
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "kong",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))








## Barents data ------------------------------------------------------------

# barents polar cod biomass
barents_polar_cod <- read.csv("P:/FACE-IT_data/svalbard/biomass-of-polar-cod-in.csv", sep = ";", dec = ",") %>% 
  mutate(date_accessed = as.Date("2023-04-11"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/biomass-of-polar-cod-in-the-barents-sea/", 
         citation = "Institute of Marine Research (2022). Biomass of polar cod in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/polar-cod.html", 
         lon = NA, lat = NA, depth = NA, 
         variable = "Boreogadus saida (polar cod) [10^6 kg]",
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "barents sea", 
         date = as.Date(paste0(Category,"-12-31"))
  ) %>% 
  dplyr::rename(value = Biomass) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))


# barents capelin stock

barents_capelin_stock <- read.csv("P:/FACE-IT_data/svalbard/capelin-stock-in-the-bar.csv",sep = ";" , dec = ",") %>% 
  pivot_longer(cols = c(`Mature.stock`, `Immature.stock`)) %>% 
  mutate(date_accessed = as.Date("2023-04-12"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/capelin-stock-in-the-barents-sea/", 
         citation = "Institute of Marine Research (2022). Capelin stock in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/capelin.html", 
         lon = NA, lat = NA, depth = NA, 
         variable = paste0("Mallotus villosus (capelin) ", str_replace(tolower(name),"\\."," ") ," [10^6 kg]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "barents sea", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))


# barents golden redfish population

barents_golden_redfish_population <- read_delim("P:/FACE-IT_data/svalbard/stock-of-golden-redfish.csv") %>% 
  pivot_longer(cols = c(`Mature stock`, `Immature stock`)) %>% 
  mutate(date_accessed = as.Date("2023-04-12"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/golden-redfish-stock-in-the-barents-sea/", 
         citation = "Institute of Marine Research (2023). Stock of golden redfish in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: https://mosj.no/en/indikator/fauna/marine-fauna/golden-redfish-stock-in-the-barents-sea/", 
         lon = NA, lat = NA, depth = NA, 
         variable = paste0("Sebastes norvegicus (golden redfish) ", tolower(name) ," [10^3 kg]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "barents sea", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))



# barents beaked redfish population

barents_beaked_redfish_population <- read.csv("P:/FACE-IT_data/svalbard/stock-of-beaked-redfish.csv", sep = ";") %>% 
  pivot_longer(cols = c(`Mature.stock`, `Immature.stock`)) %>% 
  mutate(date_accessed = as.Date("2023-04-12"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/bestanden-av-snabeluer-i-barentshavet/", 
         citation = "Institute of Marine Research (2022). Stock of beaked redfish in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/deep-sea-redfish.html", 
         lon = NA, lat = NA, depth = NA, 
         variable = paste0("Sebastes mentella (beaked redfish) ", str_replace(tolower(name),"\\."," ") ," [10^6 kg]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "barents sea", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))


# barents northeast arctic cod population

barents_northeast_cod_population <- read.csv("P:/FACE-IT_data/svalbard/stock-of-northeast-arcti.csv", sep = ";") %>% 
  pivot_longer(cols = c(`Immature.stock`, `Spawning.stock`)) %>% 
  mutate(date_accessed = as.Date("2023-04-12"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/stock-of-northeast-arctic-cod/", 
         citation = "Institute of Marine Research (2022). Stock of Northeast Arctic cod in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/northeast-arctic-cod.html", 
         lon = NA, lat = NA, depth = NA, 
         variable = paste0("Gadus morhua (northeast arctic) ", str_replace(tolower(name),"\\."," ") ," [10^6 kg]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "barents sea", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))


# barents young herring population

barents_young_herring_population <- read.csv("P:/FACE-IT_data/svalbard/biomass-index-for-young.csv", sep = ";", dec = ",") %>% 
  pivot_longer(cols = c(`X1.year.olds`, `X2.year.old`, `X3.year.old`)) %>%
  mutate(date_accessed = as.Date("2023-04-13"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/bestanden-av-ungsild-i-barentshavet/",
         citation = "Institute of Marine Research (2022). Biomass index for young herring 1–3 years in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/young-herring-population.html",
         lon = NA, lat = NA, depth = NA,
         variable = paste0(substr(str_replace_all(tolower(name),"\\."," "),2, 11)," Clupea harengus (herring) [n]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "barents sea",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))








## Is data -----------------------------------------------------------------

# is  kittiwake population

is_kittiwakke_population <- read.csv("P:/FACE-IT_data/svalbard/black-legged-kittiwake-p.csv", sep = ";", dec = ",") %>%
  pivot_longer(cols = c(`Fuglehuken`, `Bjørnøya`, `Grumant`, `Sofiekammen`, `Ossian.Sars`, `Tschermakfjellet`, `Alkhornet`, `Amsterdamya`)) %>%
  filter(name == "Tschermakfjellet"| name == "Alkhornet") %>% 
  mutate(date_accessed = as.Date("2023-04-13"),
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/black-legged-kittiwake/",
         citation = "Norwegian Polar Institute (2022). Black-legged kittiwake population size, as percentage of the average in the colony. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/black-legged-kittiwake.html",
         lon = NA, lat = NA, depth = NA,
         variable = "Rissa tridactyla (kittiwake) population [% average in the colony]",
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
         variable = "Uria lomvia (brünnich’s guillemot) [% average in the colony]",
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
         variable = paste0(str_replace_all(tolower(name),"\\."," ")," harp seal [n]"),
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
         variable = paste0(str_replace_all(tolower(name),"\\."," ")," harp seal [n]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "west ice",
         date = as.Date(paste0(Category,"-12-31"))) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))

# Other -------------------------------------------------------------------




## Datasets ----------------------------------------------------------------

kong_data <- rbind(kong_glaucous_gull_population, 
                   kong_eiders_stock,
                   kong_seabird, 
                   kong_calanus_population,
                   kong_kittiwakke_population,
                   kong_brguillemot_population
                   )

barents_data <- rbind(barents_polar_cod, 
                      barents_beaked_redfish_population, 
                      barents_capelin_stock, 
                      barents_golden_redfish_population, 
                      barents_northeast_cod_population, 
                      barents_young_herring_population
                      )

svalbard_data <- rbind(svalbard_ivory_gull_population, 
                       svalbard_nw_calanus_mm_population,
                       svalbard_nw_calanus_tot_population,
                       svalbard_se_calanus_mm_population,
                       svalbard_se_calanus_tot_population,
                       svalbard_walrus_population,
                       svalbard_brguillemot_population
                       )

is_data <- rbind(is_kittiwakke_population,
                 is_brguillemot_population
                 )
EU_data <- rbind(EU_epagr_population,
                 EU_wpagr_population)

EU_arctic_data <- rbind(kong_data,
                     barents_data,
                     svalbard_data,
                     is_data,
                     EU_arctic_data
                     )

EU_arctic_data



## Save data ---------------------------------------------------------------

save(EU_arctic_data, file = "users/calin/data/EU_arctic_data.RData")

## Figures -----------------------------------------------------------------

kong_glaucous1 <- ggplot(data = kong_glaucous_gull_population, aes(x = date, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)


barents_capelin1 <- ggplot(data = barents_capelin_stock, aes(x = date,  y = value)) + 
  geom_bar(stat = "identity", aes(fill = variable)) +
  theme_bw() + 
  theme(legend.position = c(0.75,0.75), legend.background = element_rect(color = "black")) +
  labs(x = NULL, fill = NULL )

# basic example
arctic_data1 <- ggplot(arctic_data, aes(x = date, y = variable, fill = variable)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none", axis.text = element_text(size = 9)) +
  labs(x = NULL, y = NULL)

arctic_data2 <- ggplot(data = arctic_data, aes(x = date, y = value)) +
  geom_point(aes(color = variable)) + 
  facet_wrap(~variable, scales = "free_y") +
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL)


arctic_data3 <- ggarrange(arctic_data1, arctic_data2,ncol = 1, nrow = 2, labels = c("a)", "b)"),align = "hv")
arctic_data4 <- annotate_figure(arctic_data3, top = text_grob("Summary arctic data set"))
arctic_data4


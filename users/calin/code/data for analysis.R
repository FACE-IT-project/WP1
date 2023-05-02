# code/data_for_analysis
# Species data cleaning for the analysis script

# Set up ------------------------------------------------------------------
source('users/calin/code/formulas.R')


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
  filter(Colony == "Kongsfjorden")%>%
  mutate(Taxon = "Somateria mollissima",
         TimeSeries_id = 10,
         Site = "kong", 
         Year = YR) %>% 
  dplyr::rename(Density = `Count`) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))


# kong calanus population
kong_calanus_population <- read.csv("P:/FACE-IT_data/kongsfjorden/calanus-species-composit.csv", sep = ";", dec = ",") %>% 
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
         Species = "Rissa tridactyla",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " population [% average in the colony]"),
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
         Species = "Uria lomvia",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " breeding population [%]"),
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
         Species = "Boreogadus saida",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " [10^6 kg]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "barents sea", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
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
         Species = "Mallotus villosus",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " ", str_replace(tolower(name),"\\."," ") ," [10^6 kg]"),
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
         Species = "Sebastes norvegicus",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " ", tolower(name) ," [10^3 kg]"),
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
         Species = "Sebastes mentella",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " ", str_replace(tolower(name),"\\."," ") ," [10^6 kg]"),
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
         Species = "Gadus morhua",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " ", str_replace(tolower(name),"\\."," ") ," [10^6 kg]"),
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
         Species = "Clupea harengus",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " ", substr(str_replace_all(tolower(name),"\\."," "),2, 11)," [n]"),
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

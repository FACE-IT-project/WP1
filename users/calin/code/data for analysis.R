# code/data_for_analysis
# Species data cleaning for the analysis script

# Set up ------------------------------------------------------------------
source('users/calin/code/formulas.R')

library(janitor)


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
  mutate(site = "young",
         valuez = EggsLaid) %>%
  dplyr::group_by(Year, Species, site) %>%
  dplyr::summarise(sum(valuez)) %>%
  dplyr::rename(value = `sum(valuez)`) %>% 
  mutate(TimeSeries_id = 20,
         Site = "young",  
         Taxon = paste0(`Species`, " eggs")) %>% 
  dplyr::rename(Density = `value`) %>% 
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))


# Bird presence [n]
A_nu_bird_nb <- read_delim("P:/restricted_data/GEM/nuup/View_BioBasis_Nuuk_Data_Birds_Passerine_bird_abundance170420231432285653.csv") %>% 
  mutate(Year = year(as.Date(paste0(Date,"-12-31")))) %>% 
  dplyr::group_by(Year, Species) %>%
  dplyr::summarise(sum(Number)) %>%
  dplyr::rename(Density = `sum(Number)`) %>%
  mutate(Site = "nuup",
         Taxon = case_when())
  
  dplyr::rename(Density = `Biomass`) %>%
  dplyr::rename(Taxon = Species) %>%
  dplyr::select(Site, TimeSeries_id, Year, Taxon, Density) %>% 
  filter(!is.na(Density))

  
  TimeSeries_id = 21,
  Site = "nuup",
  
  
        
         site = "nuup",
         value = Number) %>%
  dplyr::rename(date = Date) %>%
  dplyr::group_by(date_accessed, URL, citation, lon, lat, depth, date, variable, category, driver, type, site) %>% 
  # summarise data by day and variable
  dplyr::summarise(sum(value)) %>% 
  dplyr::rename(value = `sum(value)`) %>%
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!value == 0)



data_n_for_analysis <- rbind(A_sva_iv_gull_pop, 
                             A_sva_walrus_pop,
                             A_sva_cycr_pop, 
                             A_kong_eider_pop, 
                             A_kong_seabird, 
                             A_ba_yherring_pop,
                             A_yo_b_eggs)

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























# Bird presence
nuup_bird_nb <- read_delim("P:/restricted_data/GEM/nuup/View_BioBasis_Nuuk_Data_Birds_Passerine_bird_abundance170420231432285653.csv") %>% 
  mutate(date_accessed = as.Date("2023-04-17"),
         URL = "https://doi.org/10.17897/DRTB-PY74",
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Department of Bioscience, Aarhus University, Denmark in collaboration with Greenland Institute of Natural Resources, Nuuk, Greenland, and Department of Biology, University of Copenhagen, Denmark",
         # gives the coordinates according to the site
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
  # summarise data by day and variable
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
         moy = ceiling((MinNumbers+MaxNumbers)/2), # calculates the average between the minimum and maximum values
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
         value = ifelse((MinNumbers == 0), 0, 1)) %>% # change values to 1 if presence and 0 if absence
  filter(!nomsp == "NA") %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value)


# marin mammal
## Not all the data of the set have been used
nuup_mmam_count <- read_delim("P:/restricted_data/GEM/nuup/View_MarineBasis_Nuuk_Data_Marine_mammals_Identification_of_Humpback_Whales_individuals_year170420231542310109.csv") %>%
  mutate(date_accessed = as.Date("2023-04-17"), 
         URL = "https://doi.org/10.17897/13YN-1209", 
         citation = "Data from the Greenland Ecosystem Monitoring Programme were provided by the Greenland Institute of Natural Resources, Nuuk, Greenland in collaboration with Department of Bioscience, Aarhus University, Denmark and University of Copenhagen, Denmark.", 
         lon = NA, 
         lat = NA, 
         depth = NA, 
         Species = "Megaptera novaeangliae",
         nomsp = map(Species, latin_eng),
         variable = paste0(nomsp, " [n]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "nuup", 
         date = as.Date(paste0(YEAR,"-12-31"))) %>% 
  dplyr::rename(value = INDIVIDUALS) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value)























































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

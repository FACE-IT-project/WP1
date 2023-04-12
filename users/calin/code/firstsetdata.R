# Code/firstsetdata.R
# tests data first day


# Set up ------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(ggridges)
library(ggpubr)


# Data --------------------------------------------------------------------


# kong glaucous gull population
# survey, specie data, 2005-2021,

kong_glaucous_gull_population <- read_delim("P:/FACE-IT_data/kongsfjorden/glaucous-gull-population.csv") %>% 
  mutate(date_accessed = as.Date("2023-04-11"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/glaucous-gull/", 
         citation = "Norwegian Polar Institute (2022). Glaucous gull population, as percentage of the average in the colony. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/glaucous-gull.html", 
         lon = NA, lat = NA, depth = NA, 
         variable = "glaucous gull population [% average in the colony]",
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "kong", 
         date = as.Date(paste0(Category,"-12-31"))
         ) %>% 
  dplyr::rename(value = Kongsfjorden) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))


# barents polar cod biomass
# survey, specie data, 1986-2021

barents_polar_cod <- read.csv("P:/FACE-IT_data/svalbard/biomass-of-polar-cod-in.csv", sep = ";", dec = ",") %>% 
  mutate(date_accessed = as.Date("2023-04-11"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/biomass-of-polar-cod-in-the-barents-sea/", 
         citation = "Institute of Marine Research (2022). Biomass of polar cod in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/polar-cod.html", 
         lon = NA, lat = NA, depth = NA, 
         variable = "polar cod [10^6 kg]",
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
# survey, specie data, 1986-2021,

barents_capelin_stock <- read.csv("P:/FACE-IT_data/svalbard/capelin-stock-in-the-bar.csv",sep = ";" , dec = ",") %>% 
  pivot_longer(cols = c(`Mature.stock`, `Immature.stock`)) %>% 
  mutate(date_accessed = as.Date("2023-04-12"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/capelin-stock-in-the-barents-sea/", 
         citation = "Institute of Marine Research (2022). Capelin stock in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/capelin.html", 
         lon = NA, lat = NA, depth = NA, 
         variable = paste0("capelin ", str_replace(tolower(name),"\\."," ") ," [10^6 kg]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "barents sea", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))

# kong eiders
# survey, specie data, 1981-2022,

kong_eiders_stock <- read.csv("P:/FACE-IT_data/kongsfjorden/breeding-population-of-c.csv", sep = ";") %>% 
  mutate(date_accessed = as.Date("2023-04-12"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/common-eider/", 
         citation = "Norwegian Polar Institute (2022). Breeding population of common eiders in Kongsfjorden, number of breeding pairs. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/common-eider.html", 
         lon = NA, lat = NA, depth = NA, 
         variable = "common eider breeding pairs [n]",
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "kong", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::rename(value = Common.eider) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))

# kong seabird
#survey, specie data, 2011-2018,

kong_seabird <- read.csv("P:/FACE-IT_data/kongsfjorden/Descamps_Strom_Ecology_data.csv", sep = ",", skip = 3, header = TRUE) %>%
  remove_empty() %>% 
  filter(Colony == "Kongsfjorden") %>% 
  mutate(date_accessed = as.Date("2023-04-12"), 
         URL = "https://data.npolar.no/dataset/0ea572cd-1e4c-47a3-b2a5-5d7cc75aaeb4", 
         citation = "Descamps, S., & Strøm, H. (2021). Seabird monitoring data from Svalbard, 2009-2018 [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2021.0ea572cd", 
         lon = NA, lat = NA, depth = NA, 
         Species = case_when(Species == "GLGU"~"glaucous gull"),
         variable = paste0(tolower(Species), " colony count [n]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "kong", 
         date = as.Date(paste0(YR,"-12-31"))) %>% 
  dplyr::rename(value = Count) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))

# golden redfish population
# survey, specie data, 1986-2019,

barents_golden_redfish_population <- read_delim("P:/FACE-IT_data/svalbard/stock-of-golden-redfish.csv") %>% 
  pivot_longer(cols = c(`Mature stock`, `Immature stock`)) %>% 
  mutate(date_accessed = as.Date("2023-04-12"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/golden-redfish-stock-in-the-barents-sea/", 
         citation = "Institute of Marine Research (2023). Stock of golden redfish in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: https://mosj.no/en/indikator/fauna/marine-fauna/golden-redfish-stock-in-the-barents-sea/", 
         lon = NA, lat = NA, depth = NA, 
         variable = paste0("golden redfish ", tolower(name) ," [10^3 kg]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "barents sea", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))

# ivory gull population
# survey, specie data, 1986-2019,

svalbard_ivory_gull_population <- read.csv("P:/FACE-IT_data/svalbard/the-number-of-breeding-p.csv", sep = ";") %>% 
  mutate(date_accessed = as.Date("2023-04-12"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/ivory-gull/", 
         citation = "Norwegian Polar Institute (2022). The number of breeding pairs of ivory gulls in Svalbard. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/ismaake.html", 
         lon = NA, lat = NA, depth = NA, 
         variable = "ivory gull breeding population [%]",
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "svalbard sea", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::rename(value = Svalbard) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))

# beaked redfish population
# survey, specie data, 1992-2020,

barents_beaked_redfish_population <- read.csv("P:/FACE-IT_data/svalbard/stock-of-beaked-redfish.csv", sep = ";") %>% 
  pivot_longer(cols = c(`Mature.stock`, `Immature.stock`)) %>% 
  mutate(date_accessed = as.Date("2023-04-12"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/bestanden-av-snabeluer-i-barentshavet/", 
         citation = "Institute of Marine Research (2022). Stock of beaked redfish in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/deep-sea-redfish.html", 
         lon = NA, lat = NA, depth = NA, 
         variable = paste0("beaked redfish ", str_replace(tolower(name),"\\."," ") ," [10^6 kg]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "barents sea", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))

# northeast arctic cod population
# survey, specie data, 1992-2020,

barents_northeast_cod_population <- read.csv("P:/FACE-IT_data/svalbard/stock-of-northeast-arcti.csv", sep = ";") %>% 
  pivot_longer(cols = c(`Immature.stock`, `Spawning.stock`)) %>% 
  mutate(date_accessed = as.Date("2023-04-12"), 
         URL = "https://mosj.no/en/indikator/fauna/marine-fauna/stock-of-northeast-arctic-cod/", 
         citation = "Institute of Marine Research (2022). Stock of Northeast Arctic cod in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/northeast-arctic-cod.html", 
         lon = NA, lat = NA, depth = NA, 
         variable = paste0("northeast arctic ", str_replace(tolower(name),"\\."," ") ," [10^6 kg]"),
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "barents sea", 
         date = as.Date(paste0(Category,"-12-31"))) %>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>% 
  filter(!is.na(value))

# young herring population
# # survey, specie data, 1992-2020,
# 
# barents_young_herring_population <- read_delim("P:/FACE-IT_data/svalbard/biomass-index-for-young.csv") #%>% 
#   pivot_longer(cols = c(`Immature.stock`, `Spawning.stock`)) %>% 
#   mutate(date_accessed = as.Date("2023-04-12"), 
#          URL = "https://mosj.no/en/indikator/fauna/marine-fauna/stock-of-northeast-arctic-cod/", 
#          citation = "Institute of Marine Research (2022). Stock of Northeast Arctic cod in the Barents Sea. Environmental monitoring of Svalbard and Jan Mayen (MOSJ). URL: http://www.mosj.no/en/fauna/marine/northeast-arctic-cod.html", 
#          lon = NA, lat = NA, depth = NA, 
#          variable = paste0("northeast arctic ", str_replace(tolower(name),"\\."," ") ," [10^6 kg]"),
#          category = "bio",
#          driver ="biomass",
#          type = "in situ",
#          site = "barents sea", 
#          date = as.Date(paste0(Category,"-12-31"))) %>% 
#   dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>% 
#   filter(!is.na(value))



# Datasets ----------------------------------------------------------------

arctic_data <- rbind(kong_seabird,kong_eiders_stock, barents_capelin_stock,barents_polar_cod, kong_glaucous_gull_population)


# Figures -----------------------------------------------------------------

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


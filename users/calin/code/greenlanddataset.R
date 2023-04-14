# Code/greenlanddataset.R
# tests data first day


# Set up ------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(ggridges)
library(ggpubr)
library(stringi)


# Data --------------------------------------------------------------------

# nuuk  seabird population

nuuk_seabird_population <- read_delim("P:/FACE-IT_data/greenland/View_MarineBasis_Nuuk_Data_Seabirds_Seabird_species_counts_per_colony140420231545035416.csv") %>%
  mutate(date_accessed = as.Date("2023-04-14"),
         URL = "    ",
         citation = "      ",
         depth = NA,
         variable = "    ",
         category = "bio",
         driver ="biomass",
         type = "in situ",
         site = "nuuk"
         ) %>%
  dplyr::rename(lon = Longitude) %>% 
  dplyr::rename(lat = Latitude) #%>% 
  dplyr::select(date_accessed, URL, citation, type, site, category, driver, variable, lon, lat, date, depth, value) %>%
  filter(!is.na(value))

# Code/species_pangea.R
# data from the PANGEA website


# Set up ------------------------------------------------------------------
library(tidyverse)
library(pangaear)

# Formula -----------------------------------------------------------------
# Change name to latin and english name
source('users/calin/code/formulas.R')

# Data --------------------------------------------------------------------

pangea_search01 <- pg_search(query = 'nuuk species')
pangea_data01 <- pg_data(doi = pangea_search01$doi[1])
pangea_0101 <- pangea_data01[[1]]$data
pangea_0102 <- pangea_data01[[2]]$data

pangea_data03 <- pg_data(doi = pangea_search01$doi[3])
pangea_0301 <- pangea_data03[[1]]$data


pangea_testlatlong <- pangea_0301 %>% 
  dplyr::select(Latitude, Longitude) %>% 
  mutate(value = 1) %>% 
  summarise(annual_type_count = n(), .by = c(Latitude, Longitude))




pangea_pangea <- ggplot(data = pangea_0102, aes (x = Longitude, y = Latitude)) + 
  borders()+
  geom_point(aes(color = "red"))
pangea_pangea




pangea_search02 <- pg_search(query = 'Saha, Atal; Hauser, Lorenz; Hedeholm, Rasmus; Planque, Benjamin')
pangea_data02 <- pg_data(doi = pangea_search02$doi[1])
pangea_0202 <- pangea_data02[[2]]$data





pangea_search03 <- pg_search(query = 'Whale sightings, group sizes and krill biomass in West Greenland in 2005')
pangea_data03 <- pg_data(doi = pangea_search03$doi[1])
pangea_0301 <- pangea_data03[[1]]$data
pangea_0302 <- pangea_data03[[2]]$data

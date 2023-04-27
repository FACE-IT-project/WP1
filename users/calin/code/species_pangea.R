# Code/species_pangea.R
# data from the PANGEA website


# Set up ------------------------------------------------------------------
library(tidyverse)
library(pangaear)

# Formula -----------------------------------------------------------------
# Change name to latin and english name
source('users/calin/code/formulas.R')

# Data --------------------------------------------------------------------

nuup_search01 <- pg_search(query = 'nuuk species')
nuup_data01 <- pg_data(doi = nuup_search01$doi[1])
nuup_0101 <- nuup_data01[[1]]$data
nuup_0102 <- nuup_data01[[2]]$data



ggplot(data = nuup_0102, aes (x = Longitude, y = Latitude)) + 
  borders()+
  geom_point(aes(color = "red"))





nuup_search02 <- pg_search(query = 'Saha, Atal; Hauser, Lorenz; Hedeholm, Rasmus; Planque, Benjamin')
nuup_data02 <- pg_data(doi = nuup_search02$doi[1])
nuup_0202 <- nuup_data02[[2]]$data

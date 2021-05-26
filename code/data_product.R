# code/data_product.R
# This script houses the code used to create data products from the many disparate files


# Setup -------------------------------------------------------------------

# Libraries
library(tidyverse)

# Bounding boxes
bbox_kong <- c(11, 12.69, 78.86, 79.1)

# Quick filtering function


# Kongsfjorden ------------------------------------------------------------

# TODO: Make a function out of the code used to create the Kong product
# This can then be applied to the other large products for all sites

# Start with large EU files
pg_EU_cruise_oceans_dat <- data.table::fread("~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Oceans.csv")
colnames(pg_EU_cruise_oceans_dat)
pg_EU_cruise_oceans_sub <- pg_EU_cruise_oceans_dat %>% 
  dplyr::rename(lon = Longitude, lat = Latitude) %>% 
  # Get only data in bounding box
  filter(lon >= bbox_kong[1], lon <= bbox_kong[2],
         lat >= bbox_kong[3], lat <= bbox_kong[4]) %>% 
  # Filter all columns with no values
  janitor::remove_empty("cols") %>% 
  # janitor::convert_to_date(contains(c("Date")))
  # dplyr::select(where(~!all(is.na(.x))))# %>%
  # select(where(~sum(!is.na(.x)) > 0)) %>% 
  # Convert all columns after Date into numeric
  # mutate_at(vars(-contains(c("URL", "citation", "Date"))), as.numeric) %>% 
  # Roughly select columns containing the key drivers
  dplyr::select("URL", "citation",
                contains(c("Date", "Longitude", "Latitude", "Depth", "Bathy", "Press", "Density", "Elevation",  
                           "Temp", "°C", "Sal", "O2", "DO", "Ice", "Snow", "Turb", "PAR", "Vel", "Direction",
                           "DIC", "DOC", "DON", "pH", "pCO2", "CaCO3", "Arg", "Cal", "NO3", "NO2", "NH3", "PO4", "Si", "TOC", "TA",
                           "Chl a", "Chl b"))) #%>% 
# Filter rows that are missing all values
  # dplyr::select(-contains(c("Sample ID", "Comment", "pen depth", "105°C", "std dev", "TiO2", "parent", 
                            # "feldspar", "Rock", "Pseudo", "DOY", "Cibici", "Device", "parva", "Amp", "sp.", "spp.", "Euphaus",
                            # "Roughness", "rhaphid", "Cluster", "Trans", "residues", "falcon", "clausi", "Site"))) %>% 


                
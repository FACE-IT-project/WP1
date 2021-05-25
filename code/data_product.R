# code/data_product.R
# This script houses the code used to create data products from the many disparate files


# Setup -------------------------------------------------------------------

# Libraries
library(tidyverse)

# Bounding boxes
bbox_kong <- c(11, 12.69, 78.86, 79.1)


# Kongsfjorden ------------------------------------------------------------

# Start with large EU files
pg_EU_cruise_oceans <- data.table::fread("~/pCloudDrive/FACE-IT_data/EU_arctic/pg_EU_cruise_Oceans.csv")
colnames(pg_EU_cruise_oceans)
pg_EU_cruise_oceans_sub <- pg_EU_cruise_oceans %>% 
  dplyr::rename(lon = Longitude, lat = Latitude) %>% 
  filter(lon >= bbox_kong[1], lon <= bbox_kong[2],
         lat >= bbox_kong[3], lat <= bbox_kong[4]) %>% 
# Convert all columns after Date into numeric
# Find the sum of these rows
# Filter all columns with no values
# Filter rows that are missing all values
  dplyr::select("URL", "citation",
                contains(c("Date", "Longitude", "Latitude", "Depth", "Bathy", "Press", "Density", "Elevation", "Direction", 
                           "Temp", "°C", "Sal", "O2", "DO", "Ice", "Snow", "Turb", "PAR", "Vel",
                           "DIC", "DOC", "DON", "pH", "CO2", "CaCO3", "NO3", "NO2", "NH3", "PO4", "Si", "TOC", "TA",
                           "Chl a", "Chl b"))) #%>% 
  # dplyr::select(-contains(c("Sample ID", "Comment", "pen depth", "105°C", "std dev", "TiO2", "parent", 
                            # "feldspar", "Rock", "Pseudo", "DOY", "Cibici", "Device", "parva", "Amp", "sp.", "spp.", "Euphaus",
                            # "Roughness", "rhaphid", "Cluster", "Trans", "residues", "falcon", "clausi", "Site"))) %>% 


                
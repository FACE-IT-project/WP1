# code/data_product.R
# This script houses the code used to create data products from the many disparate files


# Setup -------------------------------------------------------------------

# Libraries
library(tidyverse)
library(doParallel); registerDoParallel(cores = 15)

# PANGAEA files
pg_files <- dir("~/pCloudDrive/FACE-IT_data", pattern = "pg_", recursive = T, full.names = T)
pg_EU_files <- pg_files[grepl("_EU_", pg_files)]
pg_kong_files <- pg_files[grepl("_kong_", pg_files)]

# Bounding boxes
bbox_EU <- c(-60, 60, 63, 90)
bbox_kong <- c(11, 12.69, 78.86, 79.1)

# Quick filtering function
# Manual tweaks wills still be required after running this
pg_quick_filter <- function(file_name, bbox){
  pg_res <- data.table::fread(file_name) %>% 
    dplyr::rename(lon = Longitude, lat = Latitude) %>% 
    filter(lon >= bbox[1], lon <= bbox[2],
           lat >= bbox[3], lat <= bbox[4]) %>% 
    janitor::remove_empty("cols")
  gc(); return(pg_res)
}


# European Arctic ---------------------------------------------------------

# First load the large files together in one stack for use later on
# NB: This is particularly slow because the files are not local
system.time(
pg_EU_sub <- plyr::ldply(pg_EU_files, pg_quick_filter, .parallel = T, bbox = bbox_EU)
) # xxx seconds


# Kongsfjorden ------------------------------------------------------------

# Trim down EU files to kong box and prep


# Load pg kong files
system.time(
pg_kong_sub <- plyr::ldply(pg_kong_files, pg_quick_filter, bbox = bbox_kong)
) # xxx seconds

# Process kong kong and combine EU data

# bind_rows()

 #%>% 
# Filter rows that are missing all values
  # dplyr::select(-contains(c("Sample ID", "Comment", "pen depth", "105°C", "std dev", "TiO2", "parent", 
                            # "feldspar", "Rock", "Pseudo", "DOY", "Cibici", "Device", "parva", "Amp", "sp.", "spp.", "Euphaus",
                            # "Roughness", "rhaphid", "Cluster", "Trans", "residues", "falcon", "clausi", "Site"))) %>% 


# Isfjorden ---------------------------------------------------------------


# Inglefieldbukta ---------------------------------------------------------


# Young Sound -------------------------------------------------------------


# Disko Bay ---------------------------------------------------------------


# Nuup Kangerlua ----------------------------------------------------------


# Porsangerfjorden --------------------------------------------------------



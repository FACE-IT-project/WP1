# code/data_product.R
# This script houses the code used to create data products from the many disparate files


# Setup -------------------------------------------------------------------

# Libraries
library(tidyverse)
library(doParallel); registerDoParallel(cores = 15)

# PANGAEA files
pg_files <- dir("~/pCloudDrive/FACE-IT_data", pattern = "pg_", recursive = T, full.names = T)
pg_EU_files <- dir("pg_EU_data/", pattern = "pg_", recursive = T, full.names = T) # Too large to want to pull from the cloud
pg_kong_files <- pg_files[grepl("_kong_", pg_files)]

# Bounding boxes
bbox_EU <- c(-60, 60, 63, 90)
bbox_kong <- c(11, 12.69, 78.86, 79.1)

# Quick filtering function
# Manual tweaks wills still be required after running this
pg_quick_filter <- function(file_name, bbox){
  pg_dat <- data.table::fread(file_name)
  if("Longitude" %in% colnames(pg_dat)){
    pg_res <- pg_dat %>% 
      dplyr::rename(lon = Longitude, lat = Latitude) %>% 
      filter(lon >= bbox[1], lon <= bbox[2],
             lat >= bbox[3], lat <= bbox[4]) %>% 
      janitor::remove_empty("cols")
  } else{
    pg_res <- NULL
  }
  rm(pg_dat); gc()
  return(pg_res)
}


# European Arctic ---------------------------------------------------------

# First load the large files together in one stack for use later on
# NB: This is too large to load in one whack
# And I don't think we can load them individually all in memory either...
# system.time(
# pg_EU_sub <- plyr::ldply(pg_EU_files, pg_quick_filter, .parallel = T, bbox = bbox_EU)
# ) # xxx seconds


# Kongsfjorden ------------------------------------------------------------

# Load pg kong files
system.time(
pg_kong_sub <- plyr::ldply(c(pg_EU_files, pg_kong_files[-c(1, 7, 8, 14, 15, 19, 20)]), pg_quick_filter, bbox = bbox_kong)
) # 50 seconds

# Load Bick file separately and get only abiotic columns
pg_kong_Bick <- read_csv("~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_Bick.csv") %>% 
  dplyr::select(URL:citation, `Date/Time`:`Sal (at bottom, in psu)`) %>% 
  dplyr::rename(lon = Longitude, lat = Latitude)

# Remove unneeded columns
pg_kong_clean <- pg_kong_sub %>% 
  bind_rows(pg_kong_Bick) %>% 
  filter(!parent_doi %in% c("10.1594/PANGAEA.847003", "10.1594/PANGAEA.808512", "10.1594/PANGAEA.786375")) %>% 
  mutate_all(~na_if(., '')) %>% 
  janitor::remove_empty("cols") %>% 
  # Manage lon/lat columns
  mutate(lon = mean(c(lon, `Longitude 2`), na.rm = T),
         lat = mean(c(lat, `Latitude 2`), na.rm = T)) %>% 
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = ifelse(date == "", NA, date),
         date = case_when(Coverage == "June-August 2006" ~ "2006-07-01",
                          is.na(date) & !is.na(`Date/time start`) ~ `Date/time start`,
                          date == "2009-07" ~ "2009-07-01",
                          date == "2003-06" ~ "2003-06-01",
                          TRUE ~ date),
         date = gsub("T.*", "", date),
         date = as.Date(date)) %>%
  # Manage depth column
  mutate(depth = case_when(!is.na(`Press [dbar] (Pressure sensor, Digiquartz)`) ~ as.numeric(`Press [dbar] (Pressure sensor, Digiquartz)`),
                           !is.na(`Press [dbar]`) ~ as.numeric(`Press [dbar]`),
                           !is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Depth [m]`) ~ as.numeric(`Depth [m]`))) %>% 
  # Remove unwanted columns
  dplyr::select(-"ID", -"Station", - "parent_doi",
                -"Longitude 2", -"Latitude 2",
                -"Coverage", -"Date/time start", -"Date/time end",
                -"Elevation [m]", -"Elevation [m a.s.l.]", -"Depth water [m]", -"Depth [m]", -"Depth top [m]",
                -"Depth bot [m]", -"Press [dbar]", -"Press [dbar] (Pressure sensor, Digiquartz)",
                -"Visib [m]", -"Distance [m]", -"Ratio", -"C/N", -"C/Chl a",
                -contains(c("Lu_", "Ed_", "Es_", "File ", "Polygon", "Name", "Event", "URL ", "TZ ",
                            "topography", "floe", "Cloud ", "Bact", "Station", "Reference", "HNA", "LNA",
                            "Ophiopluteus", "Amphipoda", "Cyphonautes", "Tintinnopsis lata", "Sigma-theta",
                            "Lamellibranchiata", "Polychaeta", "Coelenterata", "Collection", "Fluores",
                            "Std dev", " biom ", "Replicate", "indet", "juveniles", "Length", "Density",
                            "Locality", "Local time", "Type", "PFDoDA", "Domain", "Course", "TDN",
                            "Basis", "Position", "Chrysophyta", "Sampling date", "TDP", "instrument depth",
                            "Corallinales ", "Serpulidae ", "Sample label", "Taxa", "Harpacticoida",
                            "Meiofauna", " biom ", "photo ", "Area", "nation", "Ord ", "ID ", "NOBS",
                            "[NO3]- + [NO2]", "AT [µmol/kg]", "CSC", "CO3", "fCO2",
                            "A. ", "B. ", "C. ", "D. ", "E. ", "F. ", "G. ", "H. ", "I. ", "J. ", "K. ", "L. ", "M. ",
                            "N. ", "O. ", "P. ", "Q. ", "R. ", "S. ", "T. ", "U. ", "V. ", "W. ", "X. ", "Y. ", "Z. "))) %>%
  dplyr::select(URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(6:length(.)), as.numeric) %>% 
  janitor::remove_empty("cols")

# Create guide with no empty rows
pg_kong_thin <- pg_kong_clean %>% 
  select(-c(URL:depth)) %>% 
  janitor::remove_empty("rows")

# Filter out empty rows
suppressMessages( # Don't want column name joining message
pg_kong_clean <- pg_kong_clean %>% 
  right_join(pg_kong_thin)
)

colnames(pg_kong_clean)

colnames(select(pg_kong_clean, contains("lon")))
colnames(select(pg_kong_clean, contains("lat")))
colnames(select(pg_kong_clean, contains("date")))
colnames(select(pg_kong_clean, contains(c("elev", "depth", "bathy", "press"))))

pg_kong_clean[pg_kong_clean$date == "", ]

pg_kong_lon <- select(pg_kong_clean, "URL", "parent_doi", "citation", "lon", "Longitude 2", everything())
pg_kong_lat <- select(pg_kong_clean, "URL", "parent_doi", "citation", "lat", "Latitude 2", everything())
pg_kong_date <- select(pg_kong_clean, "URL", "parent_doi", "citation", contains("date"))
pg_kong_depth <- select(pg_kong_clean, "URL", "parent_doi", "citation", contains(c("elev", "depth", "bathy", "press")), everything())

# Melt common columns
pg_kong_melt <- pg_kong_clean %>% 
  # Depth values
  pivot_longer(cols = colnames(dplyr::select(., contains(c("ice")))), 
               names_to = "ice_name", values_to = "ice") %>% 
  # Ice values
  pivot_longer(cols = colnames(dplyr::select(., contains(c("ice")))), 
               names_to = "ice_name", values_to = "ice") %>% 
  # filter(!is.na(ice)) %>% 
  # °C values
  pivot_longer(cols = colnames(dplyr::select(., contains(c("°C")))), 
               names_to = "temp_name", values_to = "temp")
  

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



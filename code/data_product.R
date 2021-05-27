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

# Function for melting columns related to a specific driver
pg_var_melt <- function(pg_clean, key_words, var_name){
  # Message of which columns were melted
  print(colnames(dplyr::select(pg_clean, contains(key_words))))
  
  # Subset and melt data.frame
  pg_melt <- pg_clean %>% 
    dplyr::select("URL", "citation", "lon", "lat", "date", "depth", contains(key_words)) %>% 
    pivot_longer(cols = colnames(dplyr::select(pg_clean, contains(key_words))), 
                 names_to = paste0(var_name,"_var"), values_to = var_name) %>% 
    filter(!is.na((!!as.name(var_name)))) %>% 
    distinct()
}


# European Arctic ---------------------------------------------------------

# No products are currently planned to be made for the EU Arctic
# Rather access the existing files directly: ~/pCloud/EU_Arctic/...


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
                -contains(c("Lu_", "Ed_", "Es_"))
                # -"Visib [m]", -"Distance [m]", -"Ratio", -"C/N", -"C/Chl a",
                # -contains(c("Lu_", "Ed_", "Es_", "File ", "Polygon", "Name", "Event", "URL ", "TZ ",
                #             "topography", "floe", "Cloud ", "Bact", "Station", "Reference", "HNA", "LNA",
                #             "Ophiopluteus", "Amphipoda", "Cyphonautes", "Tintinnopsis lata", "Sigma-theta",
                #             "Lamellibranchiata", "Polychaeta", "Coelenterata", "Collection", "Fluores",
                #             "Std dev", " biom ", "Replicate", "indet", "juveniles", "Length", "Density",
                #             "Locality", "Local time", "Type", "PFDoDA", "Domain", "Course", "TDN",
                #             "Basis", "Position", "Chrysophyta", "Sampling date", "TDP", "instrument depth",
                #             "Corallinales ", "Serpulidae ", "Sample label", "Taxa", "Harpacticoida",
                #             "Meiofauna", " biom ", "photo ", "Area", "nation", "Ord ", "ID ", "NOBS",
                #             "[NO3]- + [NO2]", "AT [µmol/kg]", "CSC", "CO3", "fCO2",
                #             "A. ", "B. ", "C. ", "D. ", "E. ", "F. ", "G. ", "H. ", "I. ", "J. ", "K. ", "L. ", "M. ",
                #             "N. ", "O. ", "P. ", "Q. ", "R. ", "S. ", "T. ", "U. ", "V. ", "W. ", "X. ", "Y. ", "Z. "))
                ) %>%
  # Finish up
  dplyr::select(URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(6:length(.)), as.numeric) %>% 
  janitor::remove_empty("cols")
colnames(pg_kong_clean)

# Individual variable data.frames
pg_kong_cryo <- pg_var_melt(pg_kong_clean, c("ice", "snow"), "cryo") %>% 
  filter(!cryo_var == "Tonicella sp. [#]")
pg_kong_temp <- pg_var_melt(pg_kong_clean, c("°C"), "temp")
pg_kong_sal <- pg_var_melt(pg_kong_clean, c("sal"), "sal")
pg_kong_O2 <- pg_var_melt(pg_kong_clean, c("O2", "DO"), "O2") %>% 
  filter(!O2_var %in% c("[NO2]- [µmol/l]", "[NO3]- + [NO2]- [µmol/l]", 
                        "CO2 [µmol/kg]", "pCO2water_SST_wet [µatm]", "fCO2water_SST_wet [µatm]",
                        "DOC [µmol/l] (High temperature catalytic ox...)", "DOC [µmol/l]"))
pg_kong_nutrient <- pg_var_melt(pg_kong_clean, c("NO3", "NO2", "NH3", "PO4", "Si"), "nutrient") %>% 
  filter(!nutrient_var %in% c("[NO3]- + [NO2]- [µmol/l]", "Chl a Prasin [µg/l]", 
                              "Sipunculidae biom wm [g/m**2]", "Floe size [code] (primary floe size)",
                              "Sigma-theta [kg/m**3]", "Visib [m]", "Density [kg/m**3]",
                              "Chl a Prasin [µg/l]", "File size [kByte]"))
pg_kong_dissolved <- pg_var_melt(pg_kong_clean, c("DIC", "DOC", "DON"), "dissolved") %>% 
  filter(!dissolved_var %in% c("TDP [µmol/l] (Acidic molybdate solution)", "C. islandica [#]"))
pg_kong_CaCO3 <- pg_var_melt(pg_kong_clean, c("CaCO3", "omega", "arg", "cal"), "CaCO3") %>% 
  filter(!CaCO3_var %in% c("Bact [#/ml] (FACSCalibur flow-cytometer (B...)", "HNA het [#/ml] (FACSCalibur flow-cytometer (B...)",
                           "LNA [#/ml] (FACSCalibur flow-cytometer (B...)", "Sal (Sensor 1, corrected, Calculated)",
                           "Sal (Sensor 2, corrected, Calculated)", "O2 [µmol/kg] (Sensor 1, not calibrated, der...)",
                           "O2 [µmol/kg] (Sensor 2, not calibrated, der...)", "PAR [µmol quanta/m**2/s] (not calibrated)",
                           "PAR [µmol quanta/m**2/s] (at surface (SPAR), not calibr...)", "Fluores [µg/l] (not calibrated, Fluorometer, ...)"))
pg_kong_CO2 <- pg_var_melt(pg_kong_clean, c("CO2"), "CO2") %>% 
  filter(!CO2_var == "fCO2water_SST_wet [µatm]")

# Check a file to ensure only correct variables remain
unique(pg_kong_CO2[,7])

# Stitch them together



# Check that all columns were used
c(pg_kong_final$a) %in% colnames(pg_kong_clean)


# Bits of code used when untangling a site product
colnames(pg_kong_clean)

colnames(select(pg_kong_clean, contains("lon")))
colnames(select(pg_kong_clean, contains("lat")))
colnames(select(pg_kong_clean, contains("date")))
colnames(select(pg_kong_clean, contains(c("elev", "depth", "bathy", "press"))))

pg_lon <- select(pg_kong_clean, "URL", "parent_doi", "citation", "lon", "Longitude 2", everything())
pg_lat <- select(pg_kong_clean, "URL", "parent_doi", "citation", "lat", "Latitude 2", everything())
pg_date <- select(pg_kong_clean, "URL", "parent_doi", "citation", contains("date"))
pg_depth <- select(pg_kong_clean, "URL", "parent_doi", "citation", contains(c("elev", "depth", "bathy", "press")), everything())



# Isfjorden ---------------------------------------------------------------


# Inglefieldbukta ---------------------------------------------------------


# Young Sound -------------------------------------------------------------


# Disko Bay ---------------------------------------------------------------


# Nuup Kangerlua ----------------------------------------------------------


# Porsangerfjorden --------------------------------------------------------



# code/data_extraction.R
# Herein the documentation of requested extractions from the FACE-IT dataset


# Setup -------------------------------------------------------------------

source("code/functions.R")

if(!exists("clean_all")) load("data/analyses/clean_all.RData")


# Extract -----------------------------------------------------------------

# For Anäis on Jan 3, 2023
## Kongsfjorden
### T°, PAR, Salinity, Chla, pH
#### depth: surface data would already be good but, if possible, data at 7/8m depth would be awesome. 
#### period and frequency: the whole year if possible, but especially spring/summer. Weekly data would be nice.
### how much points along the fjord: the idea of the eDNA project is to characterise the light/salinity gradient that we have along the fjord so 4 to 6 points would be awesome.
extract_1 <- clean_all %>% 
  filter(site == "kong",
         driver %in% c("sea temp", "salinity", "light", "prim prod", "carb"),
         variable %in% c("temp [°C]", "sal", "PAR [µmol m-2 s-1]", "pH [unknown scale]", "pH in situ [total scale]", "Chla [µg/l]"),
         depth <= 8)
# Unique coords - 7772
length(unique(paste0(extract_1$lon, extract_1$lat)))
# Unique days of sampling - 14705
length(unique(extract_1$date))

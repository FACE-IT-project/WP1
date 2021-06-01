# code/data_product.R
# This script houses the code used to create data products from the many disparate files


# Setup -------------------------------------------------------------------

# Libraries
library(tidyverse)
library(doParallel); registerDoParallel(cores = 15)

# Re-run full data collection pipeline
# system.time(
# source("code/data_collection.R")
# )

# PANGAEA files
pg_files <- dir("~/pCloudDrive/FACE-IT_data", pattern = "pg_", recursive = T, full.names = T)
pg_EU_files <- dir("data/pg_EU_data", pattern = "pg_", recursive = T, full.names = T) # Too large to want to pull from the cloud
pg_kong_files <- dir("data/pg_kong_data", pattern = "pg_", recursive = T, full.names = T)
pg_is_files <- dir("data/pg_is_data", pattern = "pg_", recursive = T, full.names = T)
pg_ingle_files <- dir("data/pg_ingle_data", pattern = "pg_", recursive = T, full.names = T)
pg_young_files <- dir("data/pg_young_data", pattern = "pg_", recursive = T, full.names = T)
pg_por_files <- dir("data/pg_por_data", pattern = "pg_", recursive = T, full.names = T)

# Bounding boxes
bbox_EU <- c(-60, 60, 63, 90)
bbox_kong <- c(11, 12.69, 78.86, 79.1)
bbox_is <- c(13.62, 17.14, 78.03, 78.71)
bbox_ingle <- c(18.15, 18.79, 77.87, 78.05)
bbox_por <- c(24.5, 27, 70, 71.2)

# Quick filtering function
# Manual tweaks will still be required after running this
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
pg_var_melt <- function(pg_clean, key_words, var_word){
  # Message of which columns were melted
  sub_cols <- colnames(dplyr::select(pg_clean, contains(key_words)))
  sub_cols <- sub_cols[!sub_cols %in% c("URL", "citation", "lon", "lat", "date", "depth")]
  print(sub_cols)
  
  # Subset and melt data.frame
  pg_melt <- pg_clean %>% 
    dplyr::select("URL", "citation", "lon", "lat", "date", "depth", all_of(sub_cols)) %>% 
    pivot_longer(cols = all_of(sub_cols), names_to = paste0("var_name"), values_to = "value") %>% 
    mutate(var_type = var_word) %>% 
    filter(!is.na(value)) %>%
    distinct() %>% 
    dplyr::select(URL:depth, var_type, var_name, value)
}


# European Arctic ---------------------------------------------------------

# No products are currently planned to be made for the EU Arctic
# Rather access the existing files directly: ~/pCloud/EU_Arctic/...


# Kongsfjorden ------------------------------------------------------------

# Load pg kong files
system.time(
pg_kong_sub <- plyr::ldply(c(pg_EU_files, pg_kong_files), pg_quick_filter, bbox = bbox_kong)
) # 170 seconds

# Test problem files
pg_test <- pg_data(doi = "10.1594/PANGAEA.868371")
pg_test <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.924281")
pg_test <- pg_kong_sub %>% 
  filter(URL == "https://doi.org/10.1594/PANGAEA.868371") %>% 
  dplyr::select(contains("depth"), everything())
  # mutate_all(~na_if(., '')) %>% 
  janitor::remove_empty("cols")

# Remove unneeded columns
pg_kong_clean <- pg_kong_sub %>% 
  # dplyr::select(contains(c("Qz")), everything()) %>%  # Look at specific problem columns
  # Manually remove problematic files
  filter(!grepl("aircraft", citation), !grepl("Aircraft", citation), 
         !grepl("flight", citation), !grepl("Flight", citation), 
         !grepl("airborne", citation), !grepl("Airborne", citation), !grepl("ACLOUD", citation),
         !parent_doi %in% c("10.1594/PANGAEA.847003", "10.1594/PANGAEA.808512", "10.1594/PANGAEA.786375"),
         !URL %in% c("https://doi.org/10.1594/PANGAEA.900501", "https://doi.org/10.1594/PANGAEA.786375",
                     "https://doi.org/10.1594/PANGAEA.847003","https://doi.org/10.1594/PANGAEA.867207")) %>% 
  # Manually remove problematic columns
  dplyr::select(-"Date/Time (of lat/long info given in thi...)", -"Date/Time (non-aequidistant time steps, ...)") %>% 
  mutate_all(~na_if(., '')) %>% 
  janitor::remove_empty("cols") %>% 
  # Manage lon/lat columns
  # mutate(lon = mean(c(lon, `Longitude 2`), na.rm = T),
         # lat = mean(c(lat, `Latitude 2`), na.rm = T)) %>% 
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = ifelse(date == "", NA, date),
         date = case_when(is.na(date) & !is.na(`Date/time start`) ~ `Date/time start`,
                          # Coverage == "June-August 2006" ~ "2006-07-01", # Removed World Glacier Atlas
                          date == "2009-07" ~ "2009-07-01",
                          date == "2003-06" ~ "2003-06-01",
                          TRUE ~ date),
         date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = case_when(!is.na(`Press [dbar] (Pressure sensor, Digiquartz)`) ~ as.numeric(`Press [dbar] (Pressure sensor, Digiquartz)`),
                           !is.na(`Press [dbar]`) ~ as.numeric(`Press [dbar]`),
                           !is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Depth [m]`) ~ as.numeric(`Depth [m]`))) %>%
  mutate(depth = case_when(is.na(depth) & !is.na(`Elevation [m]`) ~ -`Elevation [m]`,
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -`Elevation [m a.s.l.]`,
                           TRUE ~ depth)) %>% 
  # Remove unwanted columns
  dplyr::select(-"Longitude 2", -"Latitude 2",
                -"Coverage",
                -"Date/time start", -"Date/time end",
                # -"Elevation [m]", -"Elevation [m a.s.l.]", 
                # -"Depth water [m]", -"Depth [m]", -"Depth top [m]", -"Depth bot [m]",
                # -"Depth ref [m] (of ground temperature (MAGT))",
                # -"Depth [m] (depth of MAGT measurement)",                      
                # -"Depth [m] (depth of zero annual amplitude)",  
                -"Press [dbar]", -"Press [dbar] (Pressure sensor, Digiquartz)",
                -contains(c("Depth ", "Elevation ", " biom wm ", "Station"))) %>%
  # Finish up
  dplyr::select(URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(6:length(.)), as.numeric) %>% 
  janitor::remove_empty("cols")
colnames(pg_kong_clean)

# Individual variable data.frames
## Cryosphere
pg_kong_cryo <- pg_var_melt(pg_kong_clean, c("ice", "snow", "glacier", "permafrost", "floe"), "cryo") %>% 
  filter(!var_name %in% c("E. melaniceps [%]", "H. gothiceps [%]"))

## Physical
# pg_kong_bathy <- pg_var_melt(pg_kong_clean, c("bathy"), "bathy") # No files
# pg_kong_current <- pg_var_melt(pg_kong_clean, c("u ", "v ", "vel", "speed", "direction", "current"), "current") %>% # Nothing
  # filter(!var_name %in% c("TOU [mmol/m**2/day]", "Echinod larv [%]"))
pg_kong_ep <- pg_var_melt(pg_kong_clean, c("evap", "precip"), "ep")
# pg_kong_flux <- pg_var_melt(pg_kong_clean, c("Q", "flux", "latent", "sensible", "longwave", "shortwave", "radiation"), "flux") # Nothing
pg_kong_light <- pg_var_melt(pg_kong_clean, c("kd", "PAR", "light"), "light") %>% 
  filter(!var_name %in% c("Ice conc [tenths] (primary ice partial concentra...)",
                          "O2 [µmol/l] (Winkler titration (Parsons et...)",
                          "O2 sat [%] (Winkler titration (Parsons et...)"))
# pg_kong_mld <- pg_var_melt(pg_kong_clean, c("mld", "mixed"), "MLD") # Nothing
# pg_kong_river <- pg_var_melt(pg_kong_clean, c("river", "discharge"), "river") # Nothing
pg_kong_sal <- pg_var_melt(pg_kong_clean, c("sal", "psu"), "sal")
# pg_kong_sediment <- pg_var_melt(pg_kong_clean, c("sedim"), "sediment") # Nothing
# pg_kong_slp <- pg_var_melt(pg_kong_clean, c("slp"), "SLP") # Nothing
# pg_kong_suspend <- pg_var_melt(pg_kong_clean, c("pom", "pim", "som", "spm"), "suspend") # Nothing
pg_kong_temp <- pg_var_melt(pg_kong_clean, c("°C", "temp", "sst"), "temp") %>% 
  filter(!var_name %in% c("DOC [µmol/l] (High temperature catalytic ox...)", 
                          "TDN [µmol/l] (High temperature catalytic ox...)",
                          "Accuracy (Temperature measurement accur...)",
                          "fCO2water_SST_wet [µatm]", "pCO2water_SST_wet [µatm]"))
pg_kong_turb <- pg_var_melt(pg_kong_clean, c("turbidity"), "turbidity")
# pg_kong_wind <- pg_var_melt(pg_kong_clean, c("wind", "speed", "direction", "u ", "v ")) # nothing

## Carbonate chemistry
pg_kong_CaCO3 <- pg_var_melt(pg_kong_clean, c("CaCO3", "omega", "arg", "ara", "cal"), "CaCO3") %>% 
  filter(var_name %in% c("Omega Arg", "Omega Cal", "Arg [%] (High Performance Liquid Chrom...)", "Arg [%]", "Cal [%]"))
pg_kong_dissolved <- pg_var_melt(pg_kong_clean, c("DIC", "DOC", "DON"), "dissolved") %>% 
  filter(!var_name %in% c("TDP [µmol/l] (Acidic molybdate solution)", "Pseudocalanus spp. [%]", "Chalcedony fragm [%]"))
pg_kong_O2 <- pg_var_melt(pg_kong_clean, c("O2", "DO"), "O2") %>% 
  filter(!var_name %in% c("[NO2]- [µmol/l]", "[NO3]- + [NO2]- [µmol/l]", "bSiO2 [%]",
                          "pCO2water_SST_wet [µatm]", "fCO2water_SST_wet [µatm]", "CO2 [µmol/kg]",
                          "DOC [µmol/l] (High temperature catalytic ox...)", "DOC [µmol/l]",
                          "Dol [%]", "Chalcedony fragm [%]", "Pseudocalanus spp. [%]"))
pg_kong_nutrient <- pg_var_melt(pg_kong_clean, c("NO3", "NO2", "NH3", "NH4", "PO4", "Si"), "nutrient") %>% 
  filter(var_name %in% c("[NO3]- [µmol/l]", "[NO2]- [µmol/l]", "[PO4]3- [µmol/l]", "PO4 biog [%]", 
                         "Si(OH)4 [µmol/l]", "Phosph [%]", "Sil [%]"))
pg_kong_CO2 <- pg_var_melt(pg_kong_clean, c("CO2"), "CO2") %>% 
  filter(!var_name == "fCO2water_SST_wet [µatm]")
pg_kong_pH <- pg_var_melt(pg_kong_clean, c("pH", "AT", "TA"), "pH") %>% 
  filter(var_name %in% c("pH", "AT [µmol/kg] (Potentiometric titration)", "AT [µmol/kg]"))

## Biology
pg_kong_Chl <- pg_var_melt(pg_kong_clean, c("Chl"), "Chl") %>% 
  filter(!var_name %in% c("Ep-chl aggr [%]", "C/Chl a"))
## Social

# Check a file to ensure only correct variables remain
unique(pg_kong_O2$var_name)

# Stack them together
pg_kong_ALL <- rbind(pg_kong_cryo, 
                     pg_kong_ep, pg_kong_light, pg_kong_sal, pg_kong_temp, pg_kong_turb,
                     pg_kong_CaCO3, pg_kong_dissolved, pg_kong_O2, pg_kong_nutrient, pg_kong_CO2, pg_kong_pH, 
                     pg_kong_Chl)
write_csv(pg_kong_ALL, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_ALL.csv")

# Check that all columns were used
colnames(pg_kong_clean)[!colnames(pg_kong_clean) %in% unique(pg_kong_ALL$var_name)]

# Count per grid cell
pg_kong_ALL %>% 
  select(-URL, -citation) %>% 
  mutate(lon = round(lon, 2),
         lat = round(lat, 2)) %>% 
  group_by(lon, lat) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  ggplot(aes(x = lon, y = lat)) +
  borders(fill = "grey30") +
  geom_tile(aes(fill = count)) +
  coord_quickmap(xlim = c(bbox_kong[1:2]), 
                 ylim = c(bbox_kong[3:4])) +
  labs(x = NULL, y = NULL)

# Average temperature over depth
pg_kong_ALL %>% 
  select(-URL, -citation) %>% 
  filter(!is.na(depth),
         var_type == "temp") %>% 
  mutate(depth = round(depth, -1),
         year = lubridate::year(date)) %>%
  filter(value > -20, value < 10) %>% 
  group_by(depth, year) %>% 
  dplyr::summarise(value = mean(value, na.rm = T),
                   count = n(), .groups = "drop") %>% 
  ggplot(aes(x = year, y = -depth)) +
  geom_tile(aes(fill = value, colour = count), size = 2) +
  scale_colour_distiller(palette = "Reds", direction = 1) +
  scale_fill_viridis_c() +
  coord_cartesian(expand = F) +
  labs(x = NULL, y = "Depth (m)", fill = "Temp. (°C)")

# Tables of value names
table(pg_kong_ALL$var_type)
table(pg_kong_ALL$var_name, pg_kong_ALL$var_type)


# Isfjorden ---------------------------------------------------------------

# Load pg is files
system.time(
  pg_is_sub <- plyr::ldply(c(pg_EU_files, pg_kong_files, pg_is_files), pg_quick_filter, bbox = bbox_is)
) # 84 seconds

# Test problem files
pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
pg_test <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.272527")

# Remove unneeded columns
pg_is_clean <- pg_is_sub %>% 
  dplyr::select(contains(c("press", "depth", "elev")), everything()) %>%  # Look at specific problem columns
  # Manually remove problematic files
  filter(!grepl("station list", citation), !grepl("Station list", citation),
         !grepl("master tracks", citation), !grepl("Master tracks", citation),
         !grepl("aircraft", citation), !grepl("Aircraft", citation), 
         !grepl("flight", citation), !grepl("Flight", citation), 
         !grepl("airborne", citation), !grepl("Airborne", citation), !grepl("ACLOUD", citation),
         !parent_doi %in% c("10.1594/PANGAEA.847003", "10.1594/PANGAEA.808512", "10.1594/PANGAEA.786375"),
         !URL %in% c("https://doi.org/10.1594/PANGAEA.900501", "https://doi.org/10.1594/PANGAEA.786375",
                     "https://doi.org/10.1594/PANGAEA.847003","https://doi.org/10.1594/PANGAEA.867207")) %>% 
  # Manually remove problematic columns
  dplyr::select(-"Date/Time (of lat/long info given in thi...)", -"Date/Time (non-aequidistant time steps, ...)") %>% 
  mutate_all(~na_if(., '')) %>% 
  janitor::remove_empty("cols") %>% 
  # Manage lon/lat columns
  # mutate(lon = mean(c(lon, `Longitude 2`), na.rm = T),
  # lat = mean(c(lat, `Latitude 2`), na.rm = T)) %>% 
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = ifelse(date == "", NA, date),
         date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = case_when(!is.na(`Press [dbar]`) ~ as.numeric(`Press [dbar]`),
                           # !is.na(`Press [dbar] (Pressure sensor, Digiquartz)`) ~ as.numeric(`Press [dbar] (Pressure sensor, Digiquartz)`),
                           !is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Depth [m]`) ~ as.numeric(`Depth [m]`))) %>%
  mutate(depth = case_when(is.na(depth) & !is.na(`Elevation [m]`) ~ -`Elevation [m]`,
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -`Elevation [m a.s.l.]`,
                           TRUE ~ depth)) %>% 
  # Remove unwanted columns
  dplyr::select(-"Longitude 2", -"Latitude 2",
                -"Coverage",
                -"Date/Time 2",
                # -"Elevation [m]", -"Elevation [m a.s.l.]", 
                -"Depth water [m]", -"Depth [m]", -"Depth top [m]", -"Depth bot [m]",
                -"Depth ref [m] (of ground temperature (MAGT))",
                -"Depth [m] (depth of MAGT measurement)",
                -"Depth [m] (depth of zero annual amplitude)",
                -"Press [dbar]", #-"Press [dbar] (Pressure sensor, Digiquartz)",
                -contains(c("Elevation ", " biom wm ", "Station", "Rubber "))) %>%
  # Finish up
  dplyr::select(URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(6:length(.)), as.numeric) %>% 
  janitor::remove_empty("cols")
colnames(pg_is_clean)

# Individual variable data.frames
## Cryosphere
pg_is_cryo <- pg_var_melt(pg_is_clean, c("ice", "snow", "glacier", "permafrost", "floe", "Vel mag"), "cryo")

## Physical
# pg_is_bathy <- pg_var_melt(pg_is_clean, c("bathy"), "bathy") # No files
# pg_is_current <- pg_var_melt(pg_is_clean, c("vel", "speed", "direction", "current"), "current") #%>% # Nothing
# filter(!var_name %in% c("TOU [mmol/m**2/day]", "Echinod larv [%]"))
pg_is_ep <- pg_var_melt(pg_is_clean, c("evap", "precip"), "ep")
# pg_is_flux <- pg_var_melt(pg_is_clean, c("Q", "flux", "latent", "sensible", "longwave", "shortwave", "radiation"), "flux") # Nothing
# pg_is_light <- pg_var_melt(pg_is_clean, c("kd", "PAR", "light"), "light") # Nothing
# pg_is_mld <- pg_var_melt(pg_is_clean, c("mld", "mixed"), "MLD") # Nothing
# pg_is_river <- pg_var_melt(pg_is_clean, c("river", "discharge"), "river") # Nothing
pg_is_sal <- pg_var_melt(pg_is_clean, c("sal", "psu"), "sal")
# pg_is_sediment <- pg_var_melt(pg_is_clean, c("sedim"), "sediment") # Nothing
# pg_is_slp <- pg_var_melt(pg_is_clean, c("slp"), "SLP") # Nothing
# pg_is_suspend <- pg_var_melt(pg_is_clean, c("pom", "pim", "som", "spm"), "suspend") # Nothing
pg_is_temp <- pg_var_melt(pg_is_clean, c("°C", "temp", "sst"), "temp") %>% 
  filter(!var_name %in% c("Accuracy (Temperature measurement accur...)",
                          "pCO2water_SST_wet [µatm]", "fCO2water_SST_wet [µatm]",
                          "fCO2water_SST_wet [µatm] (fCO2rec, Recomputed after SOC...)"))
# pg_is_turb <- pg_var_melt(pg_is_clean, c("turbidity"), "turbidity") # Nothing
# pg_is_wind <- pg_var_melt(pg_is_clean, c("wind", "speed", "direction", "u ", "v "), "wind") # nothing

## Carbonate chemistry
pg_is_CaCO3 <- pg_var_melt(pg_is_clean, c("CaCO3", "omega", "arg", "ara", "cal"), "CaCO3") %>% 
  filter(var_name %in% c("Arg [%]", "Cal [%]"))
# pg_is_dissolved <- pg_var_melt(pg_is_clean, c("DIC", "DOC", "DON"), "dissolved") # Nothing
pg_is_O2 <- pg_var_melt(pg_is_clean, c("O2", "DO"), "O2") %>% 
  filter(var_name %in% c("O2 [µmol/l]", "O2 sat [%]"))
pg_is_nutrient <- pg_var_melt(pg_is_clean, c("NO3", "NO2", "NH3", "NH4", "PO4", "Phosph", "Si"), "nutrient") %>% 
  filter(var_name %in% c("[NO3]- [µmol/l]", "[PO4]3- [µmol/l]", "PO4 biog [%]", 
                         "Si(OH)4 [µmol/l]", "Sil [%]", "Phosph [%]"))
pg_is_CO2 <- pg_var_melt(pg_is_clean, c("CO2"), "CO2") %>% 
  filter(var_name %in% c("pCO2water_SST_wet [µatm]", "P CO2 upt Vmax [µmol/kg/s]"))
pg_is_pH <- pg_var_melt(pg_is_clean, c("pH", "AT", "TA"), "pH") %>% 
  filter(var_name %in% c("pH"))

## Biology
pg_is_Chl <- pg_var_melt(pg_is_clean, c("Chl"), "Chl") %>% 
  filter(!var_name %in% c("Ep-chl aggr [%]"))

# Check a file to ensure only correct variables remain
unique(pg_is_nutrient$var_name)

# Stack them together
pg_is_ALL <- rbind(pg_is_cryo, 
                   pg_is_ep, pg_is_sal, pg_is_temp, 
                   pg_is_CaCO3, pg_is_O2, pg_is_nutrient, pg_is_CO2, pg_is_pH, 
                   pg_is_Chl)
write_csv(pg_is_ALL, "~/pCloudDrive/FACE-IT_data/isfjorden/pg_is_ALL.csv")

# Check that all columns were used
colnames(pg_is_clean)[!colnames(pg_is_clean) %in% unique(pg_is_ALL$var_name)]


# Inglefieldbukta ---------------------------------------------------------

# Load pg is files
system.time(
  pg_ingle_sub <- plyr::ldply(c(pg_EU_files, pg_kong_files, pg_is_files, pg_ingle_files), pg_quick_filter, bbox = bbox_ingle)
) # 83 seconds


# Test problem files
pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
pg_test <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.778258")

# Remove unneeded columns
pg_ingle_clean <- pg_ingle_sub %>% 
  # dplyr::select(contains(c("date")), everything()) %>%  # Look at specific problem columns
  # Manually remove problematic files
  # filter(!grepl("station list", citation), !grepl("Station list", citation),
  #        !grepl("master tracks", citation), !grepl("Master tracks", citation),
  #        !grepl("aircraft", citation), !grepl("Aircraft", citation), 
  #        !grepl("flight", citation), !grepl("Flight", citation), 
  #        !grepl("airborne", citation), !grepl("Airborne", citation), !grepl("ACLOUD", citation),
  #        !parent_doi %in% c("10.1594/PANGAEA.847003", "10.1594/PANGAEA.808512", "10.1594/PANGAEA.786375"),
  #        !URL %in% c("https://doi.org/10.1594/PANGAEA.900501", "https://doi.org/10.1594/PANGAEA.786375",
  #                    "https://doi.org/10.1594/PANGAEA.847003","https://doi.org/10.1594/PANGAEA.867207")) %>% 
  # Manually remove problematic columns
  # dplyr::select(-"Date/Time (of lat/long info given in thi...)", -"Date/Time (non-aequidistant time steps, ...)") %>% 
  mutate_all(~na_if(., '')) %>% 
  janitor::remove_empty("cols") %>% 
  # Manage lon/lat columns
  # mutate(lon = mean(c(lon, `Longitude 2`), na.rm = T),
  # lat = mean(c(lat, `Latitude 2`), na.rm = T)) %>% 
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = ifelse(date == "", NA, date),
         date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = NA) %>% 
  # Remove unwanted columns
  dplyr::select(-"Course [deg]", -"Speed [kn]") %>%
  # Finish up
  dplyr::select(URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(6:length(.)), as.numeric) #%>% 
  # janitor::remove_empty("cols")
colnames(pg_ingle_clean)

# Individual variable data.frames
## Physical
pg_ingle_temp <- pg_var_melt(pg_ingle_clean, c("°C", "temp", "sst"), "temp")

# Check a file to ensure only correct variables remain
unique(pg_is_nutrient$var_name)

# Stack them together
pg_ingle_ALL <- rbind(pg_ingle_temp)
write_csv(pg_ingle_ALL, "~/pCloudDrive/FACE-IT_data/inglefieldbukta/pg_ingle_ALL.csv")

# Check that all columns were used
colnames(pg_ingle_clean)[!colnames(pg_ingle_clean) %in% unique(pg_ingle_ALL$var_name)]


# Young Sound -------------------------------------------------------------

# Load pg is files
system.time(
  pg_young_sub <- plyr::ldply(c(pg_EU_files, pg_kong_files, pg_young_files), pg_quick_filter, bbox = bbox_is)
) # 84 seconds

# Test problem files
pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
pg_test <- pg_dl_proc(pg_doi = pg_young_all$doi[32])

# Remove unneeded columns
pg_young_clean <- pg_young_sub %>% 
  dplyr::select(contains(c("press", "depth", "elev")), everything()) %>%  # Look at specific problem columns
  # Manually remove problematic files
  filter(!grepl("station list", citation), !grepl("Station list", citation),
         !grepl("master tracks", citation), !grepl("Master tracks", citation),
         !grepl("aircraft", citation), !grepl("Aircraft", citation), 
         !grepl("flight", citation), !grepl("Flight", citation), 
         !grepl("airborne", citation), !grepl("Airborne", citation), !grepl("ACLOUD", citation),
         !parent_doi %in% c("10.1594/PANGAEA.847003", "10.1594/PANGAEA.808512", "10.1594/PANGAEA.786375"),
         !URL %in% c("https://doi.org/10.1594/PANGAEA.900501", "https://doi.org/10.1594/PANGAEA.786375",
                     "https://doi.org/10.1594/PANGAEA.847003","https://doi.org/10.1594/PANGAEA.867207")) %>% 
  # Manually remove problematic columns
  dplyr::select(-"Date/Time (of lat/long info given in thi...)", -"Date/Time (non-aequidistant time steps, ...)") %>% 
  mutate_all(~na_if(., '')) %>% 
  janitor::remove_empty("cols") %>% 
  # Manage lon/lat columns
  # mutate(lon = mean(c(lon, `Longitude 2`), na.rm = T),
  # lat = mean(c(lat, `Latitude 2`), na.rm = T)) %>% 
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = ifelse(date == "", NA, date),
         date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = case_when(!is.na(`Press [dbar]`) ~ as.numeric(`Press [dbar]`),
                           # !is.na(`Press [dbar] (Pressure sensor, Digiquartz)`) ~ as.numeric(`Press [dbar] (Pressure sensor, Digiquartz)`),
                           !is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Depth [m]`) ~ as.numeric(`Depth [m]`))) %>%
  mutate(depth = case_when(is.na(depth) & !is.na(`Elevation [m]`) ~ -`Elevation [m]`,
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -`Elevation [m a.s.l.]`,
                           TRUE ~ depth)) %>% 
  # Remove unwanted columns
  dplyr::select(-"Longitude 2", -"Latitude 2",
                -"Coverage",
                -"Date/Time 2",
                # -"Elevation [m]", -"Elevation [m a.s.l.]", 
                -"Depth water [m]", -"Depth [m]", -"Depth top [m]", -"Depth bot [m]",
                -"Depth ref [m] (of ground temperature (MAGT))",
                -"Depth [m] (depth of MAGT measurement)",
                -"Depth [m] (depth of zero annual amplitude)",
                -"Press [dbar]", #-"Press [dbar] (Pressure sensor, Digiquartz)",
                -contains(c("Elevation ", " biom wm ", "Station", "Rubber "))) %>%
  # Finish up
  dplyr::select(URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(6:length(.)), as.numeric) %>% 
  janitor::remove_empty("cols")
colnames(pg_young_clean)

# Individual variable data.frames
## Cryosphere
pg_young_cryo <- pg_var_melt(pg_young_clean, c("ice", "snow", "glacier", "permafrost", "floe", "Vel mag"), "cryo")

## Physical
# pg_young_bathy <- pg_var_melt(pg_young_clean, c("bathy"), "bathy") # No files
# pg_young_current <- pg_var_melt(pg_young_clean, c("vel", "speed", "direction", "current"), "current") #%>% # Nothing
# filter(!var_name %in% c("TOU [mmol/m**2/day]", "Echinod larv [%]"))
pg_young_ep <- pg_var_melt(pg_young_clean, c("evap", "precip"), "ep")
# pg_young_flux <- pg_var_melt(pg_young_clean, c("Q", "flux", "latent", "sensible", "longwave", "shortwave", "radiation"), "flux") # Nothing
# pg_young_light <- pg_var_melt(pg_young_clean, c("kd", "PAR", "light"), "light") # Nothing
# pg_young_mld <- pg_var_melt(pg_young_clean, c("mld", "mixed"), "MLD") # Nothing
# pg_young_river <- pg_var_melt(pg_young_clean, c("river", "discharge"), "river") # Nothing
pg_young_sal <- pg_var_melt(pg_young_clean, c("sal", "psu"), "sal")
# pg_young_sediment <- pg_var_melt(pg_young_clean, c("sedim"), "sediment") # Nothing
# pg_young_slp <- pg_var_melt(pg_young_clean, c("slp"), "SLP") # Nothing
# pg_young_suspend <- pg_var_melt(pg_young_clean, c("pom", "pim", "som", "spm"), "suspend") # Nothing
pg_young_temp <- pg_var_melt(pg_young_clean, c("°C", "temp", "sst"), "temp") %>% 
  filter(!var_name %in% c("Accuracy (Temperature measurement accur...)",
                          "pCO2water_SST_wet [µatm]", "fCO2water_SST_wet [µatm]",
                          "fCO2water_SST_wet [µatm] (fCO2rec, Recomputed after SOC...)"))
# pg_young_turb <- pg_var_melt(pg_young_clean, c("turbidity"), "turbidity") # Nothing
# pg_young_wind <- pg_var_melt(pg_young_clean, c("wind", "speed", "direction", "u ", "v "), "wind") # nothing

## Carbonate chemistry
pg_young_CaCO3 <- pg_var_melt(pg_young_clean, c("CaCO3", "omega", "arg", "ara", "cal"), "CaCO3") %>% 
  filter(var_name %in% c("Arg [%]", "Cal [%]"))
# pg_young_dissolved <- pg_var_melt(pg_young_clean, c("DIC", "DOC", "DON"), "dissolved") # Nothing
pg_young_O2 <- pg_var_melt(pg_young_clean, c("O2", "DO"), "O2") %>% 
  filter(var_name %in% c("O2 [µmol/l]", "O2 sat [%]"))
pg_young_nutrient <- pg_var_melt(pg_young_clean, c("NO3", "NO2", "NH3", "NH4", "PO4", "Phosph", "Si"), "nutrient") %>% 
  filter(var_name %in% c("[NO3]- [µmol/l]", "[PO4]3- [µmol/l]", "PO4 biog [%]", 
                         "Si(OH)4 [µmol/l]", "Sil [%]", "Phosph [%]"))
pg_young_CO2 <- pg_var_melt(pg_young_clean, c("CO2"), "CO2") %>% 
  filter(var_name %in% c("pCO2water_SST_wet [µatm]", "P CO2 upt Vmax [µmol/kg/s]"))
pg_young_pH <- pg_var_melt(pg_young_clean, c("pH", "AT", "TA"), "pH") %>% 
  filter(var_name %in% c("pH"))

## Biology
pg_young_Chl <- pg_var_melt(pg_young_clean, c("Chl"), "Chl") %>% 
  filter(!var_name %in% c("Ep-chl aggr [%]"))

# Check a file to ensure only correct variables remain
unique(pg_young_nutrient$var_name)

# Stack them together
pg_young_ALL <- rbind(pg_young_cryo, 
                   pg_young_ep, pg_young_sal, pg_young_temp, 
                   pg_young_CaCO3, pg_young_O2, pg_young_nutrient, pg_young_CO2, pg_young_pH, 
                   pg_young_Chl)
write_csv(pg_young_ALL, "~/pCloudDrive/FACE-IT_data/isfjorden/pg_young_ALL.csv")

# Check that all columns were used
colnames(pg_young_clean)[!colnames(pg_young_clean) %in% unique(pg_young_ALL$var_name)]


# Disko Bay ---------------------------------------------------------------


# Nuup Kangerlua ----------------------------------------------------------


# Porsangerfjorden --------------------------------------------------------

# Load pg is files
system.time(
  pg_por_sub <- plyr::ldply(c(pg_EU_files, pg_kong_files, pg_is_files, pg_por_files), pg_quick_filter, bbox = bbox_por)
) # 82 seconds

# Test problem files
pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
pg_test <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.869680")

# Remove unneeded columns
pg_por_clean <- pg_por_sub %>% 
  # dplyr::select(contains(c("depth", "bathy", "press", "elev")), everything()) %>%  # Look at specific problem columns
  # Manually remove problematic files
  filter(!grepl("station list", citation), !grepl("Station list", citation),
         !grepl("master tracks", citation), !grepl("Master tracks", citation),
         !grepl("aircraft", citation), !grepl("Aircraft", citation), 
         !grepl("flight", citation), !grepl("Flight", citation), 
         !grepl("airborne", citation), !grepl("Airborne", citation), !grepl("ACLOUD", citation),
         !URL %in% c("https://doi.org/10.1594/PANGAEA.900501", "https://doi.org/10.1594/PANGAEA.786375",
                     "https://doi.org/10.1594/PANGAEA.847003","https://doi.org/10.1594/PANGAEA.867207",
                     "https://doi.org/10.1594/PANGAEA.867215", "https://doi.org/10.1594/PANGAEA.907926",
                     "https://doi.org/10.1594/PANGAEA.869680")) %>% 
  # Manually remove problematic columns
  dplyr::select(-"Date/Time (The date and time the entry w...)", -"Date/Time (Moscow time)",
                -"Date/Time (local time)") %>%
  mutate_all(~na_if(., '')) %>% 
  janitor::remove_empty("cols") %>% 
  # Manage lon/lat columns - no need
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = ifelse(date == "", NA, date),
         date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = case_when(!is.na(`Press [dbar]`) ~ as.numeric(`Press [dbar]`),
                           !is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Depth [m]`) ~ as.numeric(`Depth [m]`))) %>%
  mutate(depth = case_when(is.na(depth) & !is.na(`Elevation [m]`) ~ -as.numeric(`Elevation [m]`),
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -as.numeric(`Elevation [m a.s.l.]`),
                           TRUE ~ depth)) %>% 
  # Remove unwanted columns
  dplyr::select(-"Longitude e", -"Latitude e", -"Press [dbar]",
                -contains(c("Depth ", "Elevation "))) %>%
  # Finish up
  dplyr::select(URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(6:length(.)), as.numeric) %>% 
  janitor::remove_empty("cols")
colnames(pg_por_clean)

# Individual variable data.frames
## Cryosphere
# pg_por_cryo <- pg_var_melt(pg_por_clean, c("ice", "snow", "glacier", "permafrost", "floe", "Vel mag"), "cryo") # Nothing

## Physical
# pg_por_bathy <- pg_var_melt(pg_por_clean, c("bathy"), "bathy") # Nothing
pg_por_current <- pg_var_melt(pg_por_clean, c("vel", "speed", "direction", "cur"), "current")
pg_por_ep <- pg_var_melt(pg_por_clean, c("evap", "precip"), "ep")
# pg_por_flux <- pg_var_melt(pg_por_clean, c("Q", "flux", "latent", "sensible", "longwave", "shortwave", "radiation"), "flux") # Nothing
pg_por_light <- pg_var_melt(pg_por_clean, c("kd", "PAR", "light"), "light")
# pg_por_mld <- pg_var_melt(pg_por_clean, c("mld", "mixed"), "MLD") # Nothing
# pg_por_river <- pg_var_melt(pg_por_clean, c("river", "discharge"), "river") # Nothing
pg_por_sal <- pg_var_melt(pg_por_clean, c("sal", "psu"), "sal")
# pg_por_sediment <- pg_var_melt(pg_por_clean, c("sedim"), "sediment") # Nothing
# pg_por_slp <- pg_var_melt(pg_por_clean, c("slp"), "SLP") # Nothing
# pg_por_suspend <- pg_var_melt(pg_por_clean, c("pom", "pim", "som", "spm"), "suspend") # Nothing
pg_por_temp <- pg_var_melt(pg_por_clean, c("°C", "temp", "sst"), "temp") %>% 
  filter(!var_name %in% c("fCO2water_SST_wet [µatm]",
                          "fCO2water_SST_wet [µatm] (fCO2rec, Recomputed after SOC...)"))
# pg_por_turb <- pg_var_melt(pg_por_clean, c("turbidity"), "turbidity") # Nothing
pg_por_wind <- pg_var_melt(pg_por_clean, c("wind", "speed", "direction", "u ", "v "), "wind") %>% 
  filter(var_name == "Indicator wind [code]")

## Carbonate chemistry
# pg_por_CaCO3 <- pg_var_melt(pg_por_clean, c("CaCO3", "omega", "arg", "ara", "cal"), "CaCO3") # Nothing 
# pg_por_dissolved <- pg_var_melt(pg_por_clean, c("DIC", "DOC", "DON"), "dissolved") # Nothing
pg_por_O2 <- pg_var_melt(pg_por_clean, c("O2", "DO"), "O2") %>% 
  filter(var_name %in% c("O2 [µmol/l]", "O2 sat [%]"))
pg_por_nutrient <- pg_var_melt(pg_por_clean, c("NO3", "NO2", "NH3", "NH4", "PO4", "Phosph", "Si"), "nutrient") %>% 
  filter(!var_name %in% c("Sigma-theta [kg/m**3]"))
pg_por_CO2 <- pg_var_melt(pg_por_clean, c("CO2"), "CO2")
pg_por_pH <- pg_var_melt(pg_por_clean, c("pH", "AT", "TA"), "pH") %>% 
  filter(var_name %in% c("pH (fraction < 2mm, detection lim...)", "Ta [mg/kg]"))

## Biology
# pg_por_Chl <- pg_var_melt(pg_por_clean, c("Chl"), "Chl") # Nothing

# Check a file to ensure only correct variables remain
unique(pg_por_nutrient$var_name)

# Stack them together
pg_por_ALL <- rbind(pg_por_current, pg_por_ep, pg_por_light, pg_por_sal, pg_por_temp, pg_por_wind,
                    pg_por_O2, pg_por_nutrient, pg_por_CO2, pg_por_pH)
write_csv(pg_por_ALL, "~/pCloudDrive/FACE-IT_data/porsangerfjorden/pg_por_ALL.csv")

# Check that all columns were used
colnames(pg_por_clean)[!colnames(pg_por_clean) %in% unique(pg_por_ALL$var_name)]


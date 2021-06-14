# code/data_product.R
# This script houses the code used to create data products from the many disparate files


# Setup -------------------------------------------------------------------

# Libraries
source("code/functions.R")

# Re-run full data collection pipeline
# system.time(
# source("code/data_collection.R")
# )

# PANGAEA files
pg_files <- dir("data/pg_data/", pattern = "pg_", full.names = T)

# Bounding boxes
bbox_EU <- c(-60, 60, 63, 90)
bbox_kong <- c(11, 12.69, 78.86, 79.1)
bbox_is <- c(13.62, 17.14, 78.03, 78.71)
bbox_ingle <- c(18.15, 18.79, 77.87, 78.05)
bbox_young <- c(-22.367917, -19.907644, 74.210137, 74.624304)
bbox_disko <- c(-55.56, -49.55, 68.22, 70.5)
bbox_nuup <- c(-53.32, -48.93, 64.01, 64.8)
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
  sub_cols <- colnames(pg_clean)[colnames(pg_clean) %in% unique(key_words)]
  sub_cols <- sub_cols[!sub_cols %in% c("date_accessed", "URL", "citation", "lon", "lat", "date", "depth")]
  print(sub_cols)
  
  # Subset and melt data.frame
  pg_melt <- pg_clean %>% 
    dplyr::select("date_accessed", "URL", "citation", "lon", "lat", "date", "depth", all_of(sub_cols)) %>% 
    pivot_longer(cols = all_of(sub_cols), names_to = paste0("var_name"), values_to = "value") %>% 
    mutate(var_type = var_word) %>% 
    filter(!is.na(value)) %>%
    distinct() %>% 
    dplyr::select(date_accessed:depth, var_type, var_name, value)
}


# European Arctic ---------------------------------------------------------

# No products are currently planned to be made for the EU Arctic
# Rather access the existing files directly: ~/pCloud/EU_Arctic/...


# Kongsfjorden ------------------------------------------------------------

# Load pg kong files
system.time(
  pg_kong_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_kong)
) # 230 seconds

# Test problem files
pg_test <- pg_data(doi = "10.1594/PANGAEA.868371")
pg_test <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.909130")
pg_test <- pg_kong_sub %>% 
  filter(URL == "https://doi.org/10.1594/PANGAEA.868371") %>% 
  dplyr::select(contains("depth"), everything()) %>% 
  # mutate_all(~na_if(., '')) %>% 
  janitor::remove_empty("cols")

# More testing
colnames(pg_kong_sub)
test1 <- pg_kong_sub %>% 
  dplyr::select(URL, `T air (1) [°C]`) %>% 
  na.omit()
  
# Process Kongsfjorden bbox PANGAEA data
pg_kong_clean <- pg_kong_sub %>% 
  # dplyr::select(contains(c("Qz")), everything()) %>%  # Look at specific problem columns
  dplyr::select(contains(c("press", "depth", "elev", "lon", "lat")), everything()) %>%  # Look at depth columns
  # dplyr::select(contains(c("date")), everything()) %>%  # Look at date columns
  # Manually remove problematic files
  filter(!URL %in% c("https://doi.org/10.1594/PANGAEA.909130")) %>% 
  # filter(!URL %in% c("https://doi.org/10.1594/PANGAEA.847003", "https://doi.org/10.1594/PANGAEA.808512", 
  #                    "https://doi.org/10.1594/PANGAEA.786375", "https://doi.org/10.1594/PANGAEA.900501", 
  #                    "https://doi.org/10.1594/PANGAEA.786375", "https://doi.org/10.1594/PANGAEA.847003", 
  #                    "https://doi.org/10.1594/PANGAEA.867207")) %>% 
  # Manually remove problematic columns
  # dplyr::select(-"File start date/time", -"File stop date/time") %>%
  mutate_all(~na_if(., '')) %>%
  janitor::remove_empty("cols") %>%
  # Manage lon/lat columns
  # mutate(lon = mean(c(lon, `Longitude 2`), na.rm = T),
         # lat = mean(c(lat, `Latitude 2`), na.rm = T)) %>% 
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = ifelse(date == "", NA, date),
         date = case_when(is.na(date) & !is.na(`Date/time start`) ~ `Date/time start`,
                          date == "2009-07" ~ "2009-07-01",
                          date == "2003-06" ~ "2003-06-01",
                          TRUE ~ date),
         date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = case_when(!is.na(`Press [dbar]`) ~ as.numeric(`Press [dbar]`),
                           !is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Depth [m]`) ~ as.numeric(`Depth [m]`))) %>%
  mutate(depth = case_when(is.na(depth) & !is.na(`Elevation [m]`) ~ -`Elevation [m]`,
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -`Elevation [m a.s.l.]`,
                           TRUE ~ depth)) %>% 
  # Remove unwanted columns
  dplyr::select(-"Longitude 2", -"Latitude 2",
                -"Date/time start", -"Date/time end",
                -"Press [dbar]",
                -contains(c("Depth ", "Elevation "))) %>%
  # Finish up
  dplyr::select(URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(6:length(.)), as.numeric) %>% 
  janitor::remove_empty("cols")
colnames(pg_kong_clean)

## Individual category data.frames
# Cryosphere
pg_kong_Cryosphere <- pg_var_melt(pg_kong_clean, query_Cryosphere$pg_col_name, "cryo")
# Physical
pg_kong_Physical <- pg_var_melt(pg_kong_clean, query_Physical$pg_col_name, "phys")
# Carbonate chemistry
pg_kong_Chemistry <- pg_var_melt(pg_kong_clean, query_Chemistry$pg_col_name, "chem")
# Biology
pg_kong_Biology <- pg_var_melt(pg_kong_clean, query_Biology$pg_col_name, "bio")
# Social
pg_kong_Social <- pg_var_melt(pg_kong_clean, query_Social$pg_col_name, "soc")

# Stack them together
pg_kong_ALL <- rbind(pg_kong_Cryosphere, pg_kong_Physical, pg_kong_Chemistry, pg_kong_Biology)
data.table::fwrite(pg_kong_ALL, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_ALL.csv")

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

# Clean up
rm(list = grep("pg_kong",names(.GlobalEnv),value = TRUE)); gc()

# Isfjorden ---------------------------------------------------------------

# Load pg is files
system.time(
  pg_is_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_is)
) # 162 seconds

# Test problem files
pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
pg_test <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.925759")
pg_test <- pg_test_dl("10.1594/PANGAEA.909130")

# Remove unneeded columns
pg_is_clean <- pg_is_sub %>% 
  # dplyr::select(contains(c("date")), everything()) %>%  # Look at date columns
  # dplyr::select(contains(c("press", "depth", "elev")), everything()) %>%  # Look at specific problem columns
  # Manually remove problematic files
  # filter(!URL %in% c("https://doi.org/10.1594/PANGAEA.900501", "https://doi.org/10.1594/PANGAEA.786375",
                     # "https://doi.org/10.1594/PANGAEA.847003","https://doi.org/10.1594/PANGAEA.867207")) %>% 
  # Manually remove problematic columns
  dplyr::select(-"File start date/time", -"File stop date/time") %>%
  # mutate(date_accessed = as.Date(date_accessed)) %>% 
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
  mutate(depth = case_when(is.na(depth) & !is.na(`Elevation [m]`) ~ -`Elevation [m]`,
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -`Elevation [m a.s.l.]`,
                           TRUE ~ depth)) %>% 
  # Remove unused meta columns
  dplyr::select(-contains(c("Longitude", "Latitude", "Depth ", "Press"))) %>%
  # Finish up
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(6:length(.)), as.numeric) %>% 
  janitor::remove_empty("cols")
colnames(pg_is_clean)

## Individual category data.frames
# Cryosphere
pg_is_Cryosphere <- pg_var_melt(pg_is_clean, query_Cryosphere$pg_col_name, "cryo")
# Physical
pg_is_Physical <- pg_var_melt(pg_is_clean, query_Physical$pg_col_name, "phys")
# Carbonate chemistry
pg_is_Chemistry <- pg_var_melt(pg_is_clean, query_Chemistry$pg_col_name, "chem")
# Biology
pg_is_Biology <- pg_var_melt(pg_is_clean, query_Biology$pg_col_name, "bio")
# Social
# pg_is_Social <- pg_var_melt(pg_is_clean, query_Social$pg_col_name, "soc") # empty

# Stack them together
pg_is_ALL <- rbind(pg_is_Cryosphere, pg_is_Physical, pg_is_Chemistry, pg_is_Biology)
data.table::fwrite(pg_is_ALL, "~/pCloudDrive/FACE-IT_data/isfjorden/pg_is_ALL.csv")

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
  # Manually remove problematic files - No issues
  # Manually remove problematic columns - No issues
  mutate_all(~na_if(., '')) %>% 
  janitor::remove_empty("cols") %>% 
  # Manage lon/lat columns - No issues
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
data.table::fwrite(pg_ingle_ALL, "~/pCloudDrive/FACE-IT_data/inglefieldbukta/pg_ingle_ALL.csv")

# Check that all columns were used
colnames(pg_ingle_clean)[!colnames(pg_ingle_clean) %in% unique(pg_ingle_ALL$var_name)]


# Young Sound -------------------------------------------------------------

# Load pg young files
system.time(
  pg_young_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_young)
) # 90 seconds

# Test problem files
pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
pg_test <- pg_dl_proc(pg_doi = pg_young_all$doi[32])

# Remove unneeded columns
pg_young_clean <- pg_young_sub %>% 
  dplyr::select(contains(c("date", "lon", "lat")), everything()) %>%  # Look at specific problem columns
  # dplyr::select(contains(c("depth", "press", "bathy", "elev")), everything()) %>%  # Look at specific problem columns
  # Manually remove problematic files
  filter(!grepl("station list", citation), !grepl("Station list", citation),
         !grepl("master tracks", citation), !grepl("Master tracks", citation),
         !grepl("aircraft", citation), !grepl("Aircraft", citation), 
         !grepl("flight", citation), !grepl("Flight", citation), 
         !grepl("airborne", citation), !grepl("Airborne", citation), !grepl("ACLOUD", citation),
         !URL %in% c("https://doi.org/10.1594/PANGAEA.900501", "https://doi.org/10.1594/PANGAEA.786375",
                     "https://doi.org/10.1594/PANGAEA.847003","https://doi.org/10.1594/PANGAEA.867207")) %>% 
  # Manually remove problematic columns
  # dplyr::select(-"Date/Time (of lat/long info given in thi...)", -"Date/Time (non-aequidistant time steps, ...)") %>% 
  mutate_all(~na_if(., '')) %>% 
  janitor::remove_empty("cols") %>% 
  # Manage lon/lat columns
  # mutate(lon = mean(c(lon, `Longitude 2`), na.rm = T),
  # lat = mean(c(lat, `Latitude 2`), na.rm = T)) %>% 
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = case_when(date == "2002-05" ~ "2002-05-01"),
         date = ifelse(date == "", NA, date),
         date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = case_when(!is.na(`Bathy depth [m]`) ~ as.numeric(`Bathy depth [m]`),
                           !is.na(`Depth ref [m] (of ground temperature (MAGT))`) ~ as.numeric(`Depth ref [m] (of ground temperature (MAGT))`))) %>%
  mutate(depth = case_when(is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -`Elevation [m a.s.l.]`,
                           TRUE ~ depth)) %>% 
  # Remove unwanted columns
  dplyr::select(-"Longitude 2", -"Latitude 2",
                -"Date/Time 2", -"Date/time end",
                # -"Elevation [m]", -"Elevation [m a.s.l.]", 
                -"Bathy depth [m]", -"Depth ref [m] (of ground temperature (MAGT))",
                -contains(c("Elevation ", "[%]"))) %>%
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
# pg_young_current <- pg_var_melt(pg_young_clean, c("vel", "speed", "direction", "current"), "current") # Nothing
# pg_young_ep <- pg_var_melt(pg_young_clean, c("evap", "precip"), "ep") # Nothing
# pg_young_flux <- pg_var_melt(pg_young_clean, c("Q", "flux", "latent", "sensible", "longwave", "shortwave", "radiation"), "flux") # Nothing
# pg_young_light <- pg_var_melt(pg_young_clean, c("kd", "PAR", "light"), "light") # Nothing
# pg_young_mld <- pg_var_melt(pg_young_clean, c("mld", "mixed"), "MLD") # Nothing
# pg_young_river <- pg_var_melt(pg_young_clean, c("river", "discharge"), "river") # Nothing
pg_young_sal <- pg_var_melt(pg_young_clean, c("sal", "psu"), "sal")
# pg_young_sediment <- pg_var_melt(pg_young_clean, c("sedim"), "sediment") # Nothing
# pg_young_slp <- pg_var_melt(pg_young_clean, c("slp"), "SLP") # Nothing
# pg_young_suspend <- pg_var_melt(pg_young_clean, c("pom", "pim", "som", "spm"), "suspend") # Nothing
pg_young_temp <- pg_var_melt(pg_young_clean, c("°C", "temp", "sst"), "temp") %>% 
# pg_young_turb <- pg_var_melt(pg_young_clean, c("turbidity"), "turbidity") # Nothing
# pg_young_wind <- pg_var_melt(pg_young_clean, c("wind", "speed", "direction", "u ", "v "), "wind") # Nothing

## Carbonate chemistry
# pg_young_CaCO3 <- pg_var_melt(pg_young_clean, c("CaCO3", "omega", "arg", "ara", "cal"), "CaCO3") # Nothing
# pg_young_dissolved <- pg_var_melt(pg_young_clean, c("DIC", "DOC", "DON"), "dissolved") # Nothing
pg_young_O2 <- pg_var_melt(pg_young_clean, c("O2", "DO"), "O2")
pg_young_nutrient <- pg_var_melt(pg_young_clean, c("NO3", "NO2", "NH3", "NH4", "PO4", "Phosph", "Si"), "nutrient") %>% 
  filter(var_name %in% c("[NO3]- [µmol/l]", "[PO4]3- [µmol/l]", "Si(OH)4 [µmol/l]"))
# pg_young_CO2 <- pg_var_melt(pg_young_clean, c("CO2"), "CO2") # Nothing
# pg_young_pH <- pg_var_melt(pg_young_clean, c("pH", "AT", "TA"), "pH") # Nothing

## Biology
# pg_young_Chl <- pg_var_melt(pg_young_clean, c("Chl"), "Chl") # Nothing

# Check a file to ensure only correct variables remain
unique(pg_young_nutrient$var_name)

# Stack them together
pg_young_ALL <- rbind(pg_young_cryo,
                      pg_young_sal, pg_young_temp,
                      pg_young_O2, pg_young_nutrient)
data.table::fwrite(pg_young_ALL, "~/pCloudDrive/FACE-IT_data/young_sound/pg_young_ALL.csv")

# Check that all columns were used
colnames(pg_young_clean)[!colnames(pg_young_clean) %in% unique(pg_young_ALL$var_name)]

# Clean up
rm(list = grep("pg_young",names(.GlobalEnv),value = TRUE)); gc()


# Disko Bay ---------------------------------------------------------------

# Load pg disko files
system.time(
  pg_disko_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_disko)
) # 84 seconds

# Test problem files
pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
pg_test <- pg_dl_proc(pg_doi = pg_disko_all$doi[32])

# Remove unneeded columns
rev(colnames(pg_disko_clean))
pg_disko_clean <- pg_disko_sub %>% 
  # Manually remove problematic columns
  # dplyr::select(-"Date/Time (The date and time the entry w...)", -"Date/Time (The image date used, entered ...)",
  #               -"Date/Time (Start)", -"Date/Time (Time in)", -"Date/Time (Time out)", -"Date/Time 2",
  #               -"Dated material (planktonic foraminifera, gast...)", -"Date/Time (UTC)", -"Date/time end",
  #               -"Dated material", -"Sampling date", -"Sampling date (survey)", -"Date/Time (start)") %>%
  dplyr::select(contains(c("lon", "lat")), everything()) %>%  # Look at specific problem columns
  # dplyr::select(contains(c("depth", "press", "bathy", "elev")), everything()) %>%  # Look at specific problem columns
  # Manually remove problematic files
  # filter(!URL %in% c("https://doi.org/10.1594/PANGAEA.900501", "https://doi.org/10.1594/PANGAEA.786375",
                     # "https://doi.org/10.1594/PANGAEA.847003","https://doi.org/10.1594/PANGAEA.867207")) #%>% 

  # mutate_all(~na_if(., '')) %>% 
  # janitor::remove_empty("cols")# %>% 
  # Manage lon/lat columns
  # mutate(lon = mean(c(lon, `Longitude 2`), na.rm = T),
  # lat = mean(c(lat, `Latitude 2`), na.rm = T)) %>% 
  # Manage date column
  # mutate(`Date/Time` = case_when(!is.na(`Date/time start`) ~ `Date/time start`,
                                 # TRUE ~ `Date/Time`)) #%>% 
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = as.character(date)) %>%
  mutate(date = ifelse(date == "", NA, date),
         # date = case_when(is.na(date) & !is.na(`Date/time start`) ~ `Date/time start`, 
                          # TRUE ~ date),
         date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = case_when(!is.na(`Bathy depth [m]`) ~ as.numeric(`Bathy depth [m]`),
                           !is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Press [dbar] (calibrated using factors prov...)`) ~ 
                             as.numeric(`Press [dbar] (calibrated using factors prov...)`))) %>%
  mutate(depth = case_when(is.na(depth) & !is.na(`Elevation [m]`) ~ -as.numeric(`Elevation [m]`),
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -as.numeric(`Elevation [m a.s.l.]`),
                           is.na(depth) & !is.na(`Surf elev [m]`) ~ -as.numeric(`Surf elev [m]`),
                           is.na(depth) & !is.na(`Elev mean [m a.s.l.]`) ~ -as.numeric(`Elev mean [m a.s.l.]`),
                           TRUE ~ depth)) %>% 
  # Remove unwanted columns
  dplyr::select(-contains(c("Longitude ", "Latitude ", "Elevation ", "Elev ", "Date ", "Date/", "depth "))) %>%
  # Finish up
  dplyr::select(URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(6:length(.)), as.numeric) %>% 
  janitor::remove_empty("cols")
colnames(pg_disko_clean)

# Individual variable data.frames
## Cryosphere
pg_disko_cryo <- pg_var_melt(pg_disko_clean, c("ice", "snow", "glacier", "permafrost", "floe", "Vel mag"), "cryo")

## Physical
# pg_disko_bathy <- pg_var_melt(pg_disko_clean, c("bathy"), "bathy") # No files
pg_disko_current <- pg_var_melt(pg_disko_clean, c("vel", "speed", "direction", "current"), "current") %>% 
  filter(!var_name %in% c("Vel mag [m/a]", "I. velorum [%]"))
# pg_disko_ep <- pg_var_melt(pg_disko_clean, c("evap", "precip"), "ep") # Nothing
# pg_disko_flux <- pg_var_melt(pg_disko_clean, c("Q", "flux", "latent", "sensible", "longwave", "shortwave", "radiation"), "flux") # Nothing
pg_disko_light <- pg_var_melt(pg_disko_clean, c("kd", "PAR", "light"), "light") %>% 
  filter(!var_name %in% c("I. paradoxum [%]", "E. transparantum [%]"))
# pg_disko_mld <- pg_var_melt(pg_disko_clean, c("mld", "mixed"), "MLD") # Nothing
# pg_disko_river <- pg_var_melt(pg_disko_clean, c("river", "discharge"), "river") # Nothing
pg_disko_sal <- pg_var_melt(pg_disko_clean, c("sal", "psu"), "sal")
# pg_disko_sediment <- pg_var_melt(pg_disko_clean, c("sedim"), "sediment") # Nothing
# pg_disko_slp <- pg_var_melt(pg_disko_clean, c("slp"), "SLP") # Nothing
# pg_disko_suspend <- pg_var_melt(pg_disko_clean, c("pom", "pim", "som", "spm"), "suspend") # Nothing
pg_disko_temp <- pg_var_melt(pg_disko_clean, c("°C", "temp", "sst"), "temp") %>% 
  filter(!var_name %in% c("pCO2water_SST_wet [µatm]", "fCO2water_SST_wet [µatm]",
                          "fCO2water_SST_wet [µatm] (fCO2rec, Recomputed after SOC...)"))
# pg_disko_turb <- pg_var_melt(pg_disko_clean, c("turbidity"), "turbidity") # Nothing
# pg_disko_wind <- pg_var_melt(pg_disko_clean, c("wind", "speed", "direction", "u ", "v "), "wind") # Nothing

## Carbonate chemistry
pg_disko_CaCO3 <- pg_var_melt(pg_disko_clean, c("CaCO3", "omega", "arg", "ara", "cal"), "CaCO3") %>% 
  filter(var_name %in% c("Cal [%]", "Cal (log centered)"))
# pg_disko_dissolved <- pg_var_melt(pg_disko_clean, c("DIC", "DOC", "DON"), "dissolved") # Nothing
pg_disko_O2 <- pg_var_melt(pg_disko_clean, c("O2", "DO"), "O2") %>% 
  filter(var_name %in% c("O2 [µmol/kg] (factory calibration, derived ...)",
                         "O2 [µmol/kg] (calibrated using the WOA09 cl...)",
                         "O2 [µmol/l]", "DO [ml/l]"))
pg_disko_nutrient <- pg_var_melt(pg_disko_clean, c("NO3", "NO2", "NH3", "NH4", "PO4", "Phosph", "Si"), "nutrient") %>% 
  filter(var_name %in% c("[NO3]- [µmol/l]", "[NO3]- + [NO2]- [µmol/l]", 
                         "[NO3]- [µmol/l] (calibrated using measurements...)",
                         "[NO2]- [µmol/l]", "[PO4]3- [µmol/l]",
                         "SiO2 [%] (amorph&finegrained)",
                         "SiO2 (amorph&finegrained, log centered)",
                         "Si(OH)4 [µmol/l]", "SiO2 [%]", "Si [µmol/l]"))
pg_disko_CO2 <- pg_var_melt(pg_disko_clean, c("CO2"), "CO2") %>% 
  filter(!var_name %in% c("Flag [#] (fCO2rec_flag, WOCE quality fl...)",
                          "Flag [#] (WOCE quality flag for fCO2rec...)"))
pg_disko_pH <- pg_var_melt(pg_disko_clean, c("pH", "AT", "TA"), "pH") %>% 
  filter(var_name %in% c("pH"))

## Biology
pg_disko_Chl <- pg_var_melt(pg_disko_clean, c("Chl"), "Chl") # Nothing

# Check a file to ensure only correct variables remain
unique(pg_disko_nutrient$var_name)

# Stack them together
pg_disko_ALL <- rbind(pg_disko_cryo,
                      pg_disko_current, pg_disko_light, pg_disko_sal, pg_disko_temp,
                      pg_disko_CaCO3, pg_disko_O2, pg_disko_nutrient, pg_disko_CO2, pg_disko_pH,
                      pg_disko_Chl)
data.table::fwrite(pg_disko_ALL, "~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_ALL.csv")

# Check that all columns were used
colnames(pg_disko_clean)[!colnames(pg_disko_clean) %in% unique(pg_disko_ALL$var_name)]

# Clean up
rm(list = grep("pg_disko",names(.GlobalEnv),value = TRUE)); gc()


# Nuup Kangerlua ----------------------------------------------------------

# Load pg young files
system.time(
  pg_nuup_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_nuup)
) # 90 seconds

# Test problem files
pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
pg_test <- pg_dl_proc(pg_doi = pg_nuup_all$doi[32])

# Remove unneeded columns
pg_nuup_clean <- pg_nuup_sub %>% 
  # dplyr::select(contains(c("lon", "lat")), everything()) %>%  # Look at specific problem columns
  dplyr::select(contains(c("date")), everything()) %>%  # Look at specific problem columns
  # dplyr::select(contains(c("depth", "press", "bathy", "elev")), everything()) %>%  # Look at specific problem columns
  # Manually remove problematic files
  filter(!grepl("station list", citation), !grepl("Station list", citation),
         !grepl("master tracks", citation), !grepl("Master tracks", citation),
         !grepl("aircraft", citation), !grepl("Aircraft", citation), 
         !grepl("flight", citation), !grepl("Flight", citation), 
         !grepl("airborne", citation), !grepl("Airborne", citation),
         !grepl("soil ", citation), !grepl("Soil ", citation),
         !grepl("metadata list", citation), !grepl("Metadata list", citation), 
         !grepl("ACLOUD", citation), !grepl("land use", citation),
         !URL %in% c("https://doi.org/10.1594/PANGAEA.900501", "https://doi.org/10.1594/PANGAEA.786375",
                     "https://doi.org/10.1594/PANGAEA.847003", "https://doi.org/10.1594/PANGAEA.867207")) %>% 
  # Manage lon/lat columns - no issues
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = case_when(is.na(date) & !is.na(`Sampling date`) ~ `Sampling date`,
                          TRUE ~ date),
         date = ifelse(date == "", NA, date),
         date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = case_when(!is.na(`Bathy depth [m]`) ~ as.numeric(`Bathy depth [m]`),
                           !is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Press [dbar]`) ~ as.numeric(`Press [dbar]`),
                           !is.na(`Press [dbar] (calibrated using factors prov...)`) ~ as.numeric(`Press [dbar] (calibrated using factors prov...)`))) %>%
  mutate(depth = case_when(is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -as.numeric(`Elevation [m a.s.l.]`),
                           is.na(depth) & !is.na(`Surf elev [m]`) ~ -as.numeric(`Surf elev [m]`),
                           is.na(depth) & !is.na(`Elevation [m]`) ~ -as.numeric(`Elevation [m]`),
                           TRUE ~ depth)) %>% 
  # Remove unwanted columns  # Manually remove problematic columns
  dplyr::select(-contains(c("Date/", "Depth ", "Elevation ", "Elev ", "Press ", "Longitude ", "Latitude "))) %>%
  # Finish up
  dplyr::select(URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(6:length(.)), as.numeric) %>% 
  janitor::remove_empty("cols")
colnames(pg_nuup_clean)

# Individual variable data.frames
## Cryosphere
pg_nuup_cryo <- pg_var_melt(pg_nuup_clean, c("ice", "snow", "glacier", "permafrost", "floe", "Vel mag"), "cryo")

## Physical
# pg_nuup_bathy <- pg_var_melt(pg_nuup_clean, c("bathy"), "bathy") # No files
pg_nuup_current <- pg_var_melt(pg_nuup_clean, c("vel", "speed", "direction", "current"), "current") %>% 
  filter(!var_name %in% c("I. velorum [%]"))
# pg_nuup_ep <- pg_var_melt(pg_nuup_clean, c("evap", "precip"), "ep") # Nothing
# pg_nuup_flux <- pg_var_melt(pg_nuup_clean, c("Q", "flux", "latent", "sensible", "longwave", "shortwave", "radiation"), "flux") # Nothing
pg_nuup_light <- pg_var_melt(pg_nuup_clean, c("kd", "PAR", "light"), "light") %>% 
  filter(!var_name %in% c("I. paradoxum [%]", "E. transparantum [%]"))
# pg_nuup_mld <- pg_var_melt(pg_nuup_clean, c("mld", "mixed"), "MLD") # Nothing
# pg_nuup_river <- pg_var_melt(pg_nuup_clean, c("river", "discharge"), "river") # Nothing
pg_nuup_sal <- pg_var_melt(pg_nuup_clean, c("sal", "psu"), "sal")
# pg_nuup_sediment <- pg_var_melt(pg_nuup_clean, c("sedim"), "sediment") # Nothing
# pg_nuup_slp <- pg_var_melt(pg_nuup_clean, c("slp"), "SLP") # Nothing
# pg_nuup_suspend <- pg_var_melt(pg_nuup_clean, c("pom", "pim", "som", "spm"), "suspend") # Nothing
pg_nuup_temp <- pg_var_melt(pg_nuup_clean, c("°C", "temp", "sst"), "temp") %>%
  filter(!var_name %in% c("pCO2water_SST_wet [µatm]", "fCO2water_SST_wet [µatm]",                                   
                          "fCO2water_SST_wet [µatm] (fCO2rec, Recomputed after SOC...)"))
# pg_nuup_turb <- pg_var_melt(pg_nuup_clean, c("turbidity"), "turbidity") # Nothing
# pg_nuup_wind <- pg_var_melt(pg_nuup_clean, c("wind", "speed", "direction", "u ", "v "), "wind") # Nothing

## Carbonate chemistry
pg_nuup_CaCO3 <- pg_var_melt(pg_nuup_clean, c("CaCO3", "omega", "arg", "ara", "cal"), "CaCO3") # Nothing
# pg_nuup_dissolved <- pg_var_melt(pg_nuup_clean, c("DIC", "DOC", "DON"), "dissolved") # Nothing
pg_nuup_O2 <- pg_var_melt(pg_nuup_clean, c("O2", "DO"), "O2") %>% 
  filter(var_name %in% c("O2 [µmol/kg] (factory calibration, derived ...)",
                         "O2 [µmol/kg] (calibrated using the WOA09 cl...)",
                         "O2 [µmol/l]", "DO [ml/l]"))
pg_nuup_nutrient <- pg_var_melt(pg_nuup_clean, c("NO3", "NO2", "NH3", "NH4", "PO4", "Phosph", "Si"), "nutrient") %>% 
  filter(var_name %in% c("[NO3]- [µmol/l] (calibrated using measurements...)",
                         "[PO4]3- [µmol/l]", "Si(OH)4 [µmol/l]", "[NO3]- [µmol/l]",
                         "[NO2]- [µmol/l]", "[NO3]- + [NO2]- [µmol/l]"))
pg_nuup_CO2 <- pg_var_melt(pg_nuup_clean, c("CO2"), "CO2") %>% 
  filter(!var_name %in% c("Flag [#] (fCO2rec_flag, WOCE quality fl...)",
                          "Flag [#] (WOCE quality flag for fCO2rec...)"))
pg_nuup_pH <- pg_var_melt(pg_nuup_clean, c("pH", "AT", "TA"), "pH") %>% 
  filter(var_name %in% c("pH"))

## Biology
pg_nuup_Chl <- pg_var_melt(pg_nuup_clean, c("Chl"), "Chl")

# Check a file to ensure only correct variables remain
unique(pg_nuup_nutrient$var_name)

# Stack them together
pg_nuup_ALL <- rbind(pg_nuup_cryo,
                     pg_nuup_current, pg_nuup_light, pg_nuup_sal, pg_nuup_temp,
                     pg_nuup_CaCO3, pg_nuup_O2, pg_nuup_nutrient, pg_nuup_CO2, pg_nuup_pH,
                     pg_nuup_Chl)
data.table::fwrite(pg_nuup_ALL, "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/pg_nuup_ALL.csv")

# Check that all columns were used
colnames(pg_nuup_clean)[!colnames(pg_nuup_clean) %in% unique(pg_nuup_ALL$var_name)]

# Clean up
rm(list = grep("pg_nuup",names(.GlobalEnv),value = TRUE)); gc()


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
data.table::fwrite(pg_por_ALL, "~/pCloudDrive/FACE-IT_data/porsangerfjorden/pg_por_ALL.csv")

# Check that all columns were used
colnames(pg_por_clean)[!colnames(pg_por_clean) %in% unique(pg_por_ALL$var_name)]


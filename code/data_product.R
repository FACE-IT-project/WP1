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
  if(length(sub_cols) == 0) stop("No variables for this driver group") 
  
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
) # 57 seconds

# Test problem files
# pg_test <- pg_data(doi = "10.1594/PANGAEA.868371")
# pg_test <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.909130")
# pg_test <- pg_kong_sub %>% 
#   filter(URL == "https://doi.org/10.1594/PANGAEA.868371") %>% 
#   dplyr::select(contains("depth"), everything()) %>% 
#   # mutate_all(~na_if(., '')) %>% 
#   janitor::remove_empty("cols")

# More testing
# colnames(pg_kong_sub)
# test1 <- pg_kong_sub %>% 
#   dplyr::select(URL, `T air (1) [°C]`) %>% 
#   na.omit()
  
# Process Kongsfjorden bbox PANGAEA data
pg_kong_clean <- pg_kong_sub %>% 
  # dplyr::select(contains(c("Qz")), everything()) %>%  # Look at specific problem columns
  # dplyr::select(contains(c("press", "depth", "elev", "lon", "lat")), everything()) %>%  # Look at depth columns
  # dplyr::select(contains(c("date")), everything()) %>%  # Look at date columns
  # Manually remove problematic files- no need
  # Manually remove problematic columns
  # dplyr::select(-"File start date/time", -"File stop date/time") %>%
  mutate_all(~na_if(., '')) %>%
  janitor::remove_empty("cols") %>%
  # Manage lon/lat columns - no need
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = ifelse(date == "", NA, date),
         date = case_when(is.na(date) & !is.na(`Date/time start`) ~ `Date/time start`,
                          TRUE ~ date),
         date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = case_when(!is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Depth [m]`) ~ as.numeric(`Depth [m]`),
                           !is.na(`Press [dbar]`) ~ as.numeric(`Press [dbar]`))) %>%
  mutate(depth = case_when(is.na(depth) & !is.na(`Elevation [m]`) ~ -`Elevation [m]`,
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -`Elevation [m a.s.l.]`,
                           TRUE ~ depth)) %>% 
  # dplyr::select(depth, everything())
  # Remove unwanted columns
  dplyr::select(-"Longitude 2", -"Latitude 2",
                -"Date/time start", -"Date/time end", -"Date",
                -"Press [dbar]",
                -contains(c("Depth ", "Elev ", "Elevation "))) %>%
  # Finish up
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(7:length(.)), as.numeric) %>% 
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
pg_kong_Biology <- pg_var_melt(pg_kong_clean, query_Biology$pg_col_name, "bio") # 0 values
# Social
pg_kong_Social <- pg_var_melt(pg_kong_clean, query_Social$pg_col_name, "soc") # 0 values

# Stack them together
pg_kong_ALL <- rbind(pg_kong_Cryosphere, pg_kong_Physical, pg_kong_Chemistry)
data.table::fwrite(pg_kong_ALL, "~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_ALL.csv")

# Load data to investigate
# pg_kong_ALL <- data.table::fread("~/pCloudDrive/FACE-IT_data/kongsfjorden/pg_kong_ALL.csv")

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
         var_type == "phys") %>% 
  mutate(depth = round(depth, -1),
         year = lubridate::year(date)) %>%
  # filter(value > -20, value < 10) %>% 
  group_by(depth, year) %>% 
  dplyr::summarise(value = mean(value, na.rm = T),
                   count = n(), .groups = "drop") %>% 
  ggplot(aes(x = year, y = -depth)) +
  geom_tile(aes(fill = value, colour = count), size = 1) +
  scale_colour_distiller(palette = "Reds", direction = 1) +
  scale_fill_viridis_c() +
  coord_cartesian(expand = F) +
  labs(x = NULL, y = "Depth (m)", fill = "Value")

# Tables of value names
table(pg_kong_ALL$var_type)
table(pg_kong_ALL$var_name, pg_kong_ALL$var_type)

# Clean up
rm(list = grep("pg_kong",names(.GlobalEnv),value = TRUE)); gc()


# Isfjorden ---------------------------------------------------------------

# Load pg is files
system.time(
  pg_is_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_is)
) # 56 seconds

# Test problem files
# pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
# pg_test <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.925759")
# pg_test <- pg_test_dl("10.1594/PANGAEA.909130")

# Remove unneeded columns
pg_is_clean <- pg_is_sub %>% 
  # dplyr::select(contains(c("date")), everything()) %>%  # Look at date columns
  # dplyr::select(contains(c("press", "depth", "elev")), everything()) %>%  # Look at specific problem columns
  # Manually remove problematic files
  # filter(!URL %in% c("https://doi.org/10.1594/PANGAEA.900501", "https://doi.org/10.1594/PANGAEA.786375",
                     # "https://doi.org/10.1594/PANGAEA.847003","https://doi.org/10.1594/PANGAEA.867207")) %>% 
  # Manually remove problematic columns
  # dplyr::select(-"File start date/time", -"File stop date/time") %>%
  # mutate(date_accessed = as.Date(date_accessed)) %>% 
  mutate_all(~na_if(., '')) %>% 
  janitor::remove_empty("cols") %>% 
  # Manage lon/lat columns - no need
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = ifelse(date == "", NA, date),
         date = case_when(is.na(date) & !is.na(`Sampling date`) ~ `Sampling date`,
                          date == 2008 ~ "2008-01-01", TRUE ~ date),
         date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = case_when(!is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Depth [m]`) ~ as.numeric(`Depth [m]`),
                           !is.na(`Press [dbar]`) ~ as.numeric(`Press [dbar]`))) %>%
  mutate(depth = case_when(is.na(depth) & !is.na(`Elevation [m]`) ~ -`Elevation [m]`,
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -`Elevation [m a.s.l.]`,
                           TRUE ~ depth)) %>% 
  # dplyr::select(depth, everything())
  # Remove unused meta columns
  dplyr::select(-contains(c("Longitude", "Latitude", "Depth ", "Press"))) %>%
  # Finish up
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(7:length(.)), as.numeric) %>% 
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
pg_is_Social <- pg_var_melt(pg_is_clean, query_Social$pg_col_name, "soc") # empty

# Stack them together
pg_is_ALL <- rbind(pg_is_Cryosphere, pg_is_Physical, pg_is_Chemistry, pg_is_Biology)
data.table::fwrite(pg_is_ALL, "~/pCloudDrive/FACE-IT_data/isfjorden/pg_is_ALL.csv")

# Check that all columns were used
colnames(pg_is_clean)[!colnames(pg_is_clean) %in% unique(pg_is_ALL$var_name)]

# Clean up
rm(list = grep("pg_is",names(.GlobalEnv),value = TRUE)); gc()


# Inglefieldbukta ---------------------------------------------------------

# Load pg is files
system.time(
  pg_ingle_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_ingle)
) # 57 seconds


# Test problem files
# pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
# pg_test <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.778258")

# Remove unneeded columns
pg_ingle_clean <- pg_ingle_sub %>% 
  # Manually remove problematic files - No issues
  # Manually remove problematic columns - No issues
  mutate_all(~na_if(., '')) %>% 
  janitor::remove_empty("cols") %>% 
  # Manage lon/lat columns - No issues
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = NA) %>% 
  # Remove unwanted columns - not needed
  # Finish up
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(7:length(.)), as.numeric) #%>% 
  # janitor::remove_empty("cols")
colnames(pg_ingle_clean)

## Individual category data.frames
# Cryosphere
pg_ingle_Cryosphere <- pg_var_melt(pg_ingle_clean, query_Cryosphere$pg_col_name, "cryo") # empty
# Physical
pg_ingle_Physical <- pg_var_melt(pg_ingle_clean, query_Physical$pg_col_name, "phys")
# Carbonate chemistry
pg_ingle_Chemistry <- pg_var_melt(pg_ingle_clean, query_Chemistry$pg_col_name, "chem") # empty
# Biology
pg_ingle_Biology <- pg_var_melt(pg_ingle_clean, query_Biology$pg_col_name, "bio") # empty
# Social
pg_ingle_Social <- pg_var_melt(pg_ingle_clean, query_Social$pg_col_name, "soc") # empty

# Stack them together
pg_ingle_ALL <- rbind(pg_ingle_Physical)
data.table::fwrite(pg_ingle_ALL, "~/pCloudDrive/FACE-IT_data/inglefieldbukta/pg_ingle_ALL.csv")

# Check that all columns were used
colnames(pg_ingle_clean)[!colnames(pg_ingle_clean) %in% unique(pg_ingle_ALL$var_name)]

# Clean up
rm(list = grep("pg_ingle",names(.GlobalEnv),value = TRUE)); gc()


# Young Sound -------------------------------------------------------------

# Load pg young files
system.time(
  pg_young_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_young)
) # 57 seconds

# Test problem files
pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
pg_test <- pg_dl_proc(pg_doi = pg_young_all$doi[32])

# Remove unneeded columns
pg_young_clean <- pg_young_sub %>% 
  # dplyr::select(contains(c("date", "lon", "lat")), everything()) %>%  # Look at meta columns
  # dplyr::select(contains(c("depth", "press", "bathy", "elev")), everything()) %>%  # Look at depth columns
  # Manually remove problematic files - no need
  # Manually remove problematic columns - no need
  mutate_all(~na_if(., '')) %>% 
  janitor::remove_empty("cols") %>% 
  # Manage lon/lat columns - no need
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = ifelse(date == "", NA, date),
         date = case_when(date == "2002-05" ~ "2002-05-01",
                          is.na(date) & !is.na(Date) ~ as.character(Date),
                          TRUE ~ date),
         date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = case_when(!is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Bathy depth [m]`) ~ as.numeric(`Bathy depth [m]`))) %>%
  mutate(depth = case_when(is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -`Elevation [m a.s.l.]`,
                           TRUE ~ depth)) %>% 
  # dplyr::select(depth, everything())
  # Remove unwanted columns
  dplyr::select(-"Longitude 2", -"Latitude 2",
                -"Date/time end",
                -contains(c("Elevation ", "Depth "))) %>%
  # Finish up
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(7:length(.)), as.numeric) %>% 
  janitor::remove_empty("cols")
colnames(pg_young_clean)

## Individual category data.frames
# Cryosphere
pg_young_Cryosphere <- pg_var_melt(pg_young_clean, query_Cryosphere$pg_col_name, "cryo")
# Physical
pg_young_Physical <- pg_var_melt(pg_young_clean, query_Physical$pg_col_name, "phys")
# Carbonate chemistry
pg_young_Chemistry <- pg_var_melt(pg_young_clean, query_Chemistry$pg_col_name, "chem")
# Biology
pg_young_Biology <- pg_var_melt(pg_young_clean, query_Biology$pg_col_name, "bio")
# Social
pg_young_Social <- pg_var_melt(pg_young_clean, query_Social$pg_col_name, "soc") # empty

# Stack them together
pg_young_ALL <- rbind(pg_young_Cryosphere, pg_young_Physical, pg_young_Chemistry, pg_young_Biology)
data.table::fwrite(pg_young_ALL, "~/pCloudDrive/FACE-IT_data/young_sound/pg_young_ALL.csv")

# Check that all columns were used
colnames(pg_young_clean)[!colnames(pg_young_clean) %in% unique(pg_young_ALL$var_name)]

# Clean up
rm(list = grep("pg_young",names(.GlobalEnv),value = TRUE)); gc()


# Disko Bay ---------------------------------------------------------------

# Load pg disko files
system.time(
  pg_disko_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_disko)
) # 57 seconds

# Test problem files
# pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
# pg_test <- pg_dl_proc(pg_doi = pg_disko_all$doi[32])
# rev(colnames(pg_disko_clean))

# Remove unneeded columns
pg_disko_clean <- pg_disko_sub %>% 
  # dplyr::select(contains(c("date", "lon", "lat")), everything()) %>%  # Look at meta columns
  # dplyr::select(contains(c("depth", "press", "bathy", "elev")), everything()) %>%  # Look at depth columns
  # Manually remove problematic files- no need
  # Manually remove problematic columns - these problems are dealt with later
  # Switching out '' doesn't work due to an unidentified problem column
  # mutate_all(~na_if(., '')) %>% 
  # janitor::remove_empty("cols")# %>%
  # Manage lon/lat columns - no need
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = as.character(date)) %>%
  mutate(date = ifelse(date == "", NA, date),
         date = case_when(is.na(date) & !is.na(`Date/time start`) ~ as.character(`Date/time start`),
                          is.na(date) & !is.na(Date) ~ as.character(Date),
                          TRUE ~ date),
         date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = case_when(!is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Bathy depth [m]`) ~ as.numeric(`Bathy depth [m]`),
                           !is.na(`Depth [m]`) ~ as.numeric(`Depth [m]`))) %>%
  mutate(depth = case_when(is.na(depth) & !is.na(`Elevation [m]`) ~ -as.numeric(`Elevation [m]`),
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -as.numeric(`Elevation [m a.s.l.]`),
                           is.na(depth) & !is.na(`Surf elev [m]`) ~ -as.numeric(`Surf elev [m]`),
                           is.na(depth) & !is.na(`Elev mean [m a.s.l.]`) ~ -as.numeric(`Elev mean [m a.s.l.]`),
                           TRUE ~ depth)) %>%
  # dplyr::select(depth, everything())
  # Remove unwanted columns
  # dplyr::select(-contains(c("Longitude ", "Latitude ", "Elev "))) %>%
  # Finish up
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(7:length(.)), as.numeric) %>% 
  janitor::remove_empty("cols")
colnames(pg_disko_clean)

## Individual category data.frames
# Cryosphere
pg_disko_Cryosphere <- pg_var_melt(pg_disko_clean, query_Cryosphere$pg_col_name, "cryo")
# Physical
pg_disko_Physical <- pg_var_melt(pg_disko_clean, query_Physical$pg_col_name, "phys")
# Carbonate chemistry
pg_disko_Chemistry <- pg_var_melt(pg_disko_clean, query_Chemistry$pg_col_name, "chem")
# Biology
pg_disko_Biology <- pg_var_melt(pg_disko_clean, query_Biology$pg_col_name, "bio")
# Social
pg_disko_Social <- pg_var_melt(pg_disko_clean, query_Social$pg_col_name, "soc") # empty

# Stack them together
pg_disko_ALL <- rbind(pg_disko_Cryosphere, pg_disko_Physical, pg_disko_Chemistry, pg_disko_Biology)
data.table::fwrite(pg_disko_ALL, "~/pCloudDrive/FACE-IT_data/disko_bay/pg_disko_ALL.csv")

# Check that all columns were used
colnames(pg_disko_clean)[!colnames(pg_disko_clean) %in% unique(pg_disko_ALL$var_name)]

# Clean up
rm(list = grep("pg_disko",names(.GlobalEnv),value = TRUE)); gc()


# Nuup Kangerlua ----------------------------------------------------------

# Load pg young files
system.time(
  pg_nuup_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_nuup)
) # 57 seconds

# Test problem files
# pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
# pg_test <- pg_dl_proc(pg_doi = pg_nuup_all$doi[32])

# Remove unneeded columns
pg_nuup_clean <- pg_nuup_sub %>% 
  # dplyr::select(contains(c("date", "lon", "lat")), everything()) %>%  # Look at meta columns
  # dplyr::select(contains(c("depth", "press", "bathy", "elev")), everything()) %>%  # Look at depth columns
  # Manually remove problematic files - no need
  mutate_all(~na_if(., '')) %>%
  janitor::remove_empty("cols") %>%
  # Manage lon/lat columns - no issues
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = case_when(is.na(date) & !is.na(Date) ~ as.character(Date),
                          date %in% seq(2000, 2009) ~ paste0(date,"-01-01"),
                          # is.na(date) & !is.na(`Sampling date`) ~ `Sampling date`, # There is an issue with these values
                          TRUE ~ date),
         # date = as.character(date),
         date = ifelse(date == "", NA, date),
         date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = case_when(!is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Bathy depth [m]`) ~ as.numeric(`Bathy depth [m]`))) %>%
  mutate(depth = case_when(is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -as.numeric(`Elevation [m a.s.l.]`),
                           is.na(depth) & !is.na(`Surf elev [m]`) ~ -as.numeric(`Surf elev [m]`),
                           is.na(depth) & !is.na(`Elevation [m]`) ~ -as.numeric(`Elevation [m]`),
                           TRUE ~ depth)) %>% 
    # dplyr::select(depth, everything())
  # Remove unwanted columns  # Manually remove problematic columns
  # dplyr::select(-contains(c("Date/", "Depth ", "Elevation ", "Elev ", "Press ", "Longitude ", "Latitude "))) %>%
  # Finish up
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(7:length(.)), as.numeric) %>% 
  janitor::remove_empty("cols")
colnames(pg_nuup_clean)

## Individual category data.frames
# Cryosphere
pg_nuup_Cryosphere <- pg_var_melt(pg_nuup_clean, query_Cryosphere$pg_col_name, "cryo")
# Physical
pg_nuup_Physical <- pg_var_melt(pg_nuup_clean, query_Physical$pg_col_name, "phys")
# Carbonate chemistry
pg_nuup_Chemistry <- pg_var_melt(pg_nuup_clean, query_Chemistry$pg_col_name, "chem")
# Biology
pg_nuup_Biology <- pg_var_melt(pg_nuup_clean, query_Biology$pg_col_name, "bio")
# Social
pg_nuup_Social <- pg_var_melt(pg_nuup_clean, query_Social$pg_col_name, "soc") # empty

# Stack them together
pg_nuup_ALL <- rbind(pg_nuup_Cryosphere, pg_nuup_Physical, pg_nuup_Chemistry, pg_nuup_Biology)
data.table::fwrite(pg_nuup_ALL, "~/pCloudDrive/FACE-IT_data/nuup_kangerlua/pg_nuup_ALL.csv")

# Check that all columns were used
colnames(pg_nuup_clean)[!colnames(pg_nuup_clean) %in% unique(pg_nuup_ALL$var_name)]

# Clean up
rm(list = grep("pg_nuup",names(.GlobalEnv),value = TRUE)); gc()


# Porsangerfjorden --------------------------------------------------------

# Load pg is files
system.time(
  pg_por_sub <- plyr::ldply(pg_files, pg_quick_filter, bbox = bbox_por)
) # 57 seconds

# Test problem files
# pg_test <- pg_data(doi = "10.1594/PANGAEA.867215")
# pg_test <- pg_dl_proc(pg_doi = "10.1594/PANGAEA.869680")

# Remove unneeded columns
pg_por_clean <- pg_por_sub %>% 
  # dplyr::select(contains(c("date", "lon", "lat")), everything()) %>%  # Look at meta columns
  dplyr::select(contains(c("depth", "press", "bathy", "elev")), everything()) %>%  # Look at depth columns
  # Manually remove problematic files
  # filter(!URL %in% c("https://doi.org/10.1594/PANGAEA.900501", "https://doi.org/10.1594/PANGAEA.786375",
  #                    "https://doi.org/10.1594/PANGAEA.847003","https://doi.org/10.1594/PANGAEA.867207",
  #                    "https://doi.org/10.1594/PANGAEA.867215", "https://doi.org/10.1594/PANGAEA.907926",
  #                    "https://doi.org/10.1594/PANGAEA.869680")) %>% 
  # Manually remove problematic columns
  # dplyr::select(-"Date/Time (The date and time the entry w...)", -"Date/Time (Moscow time)",
                # -"Date/Time (local time)") %>%
  mutate_all(~na_if(., '')) %>% 
  janitor::remove_empty("cols") %>% 
  # Manage lon/lat columns - no need
  # Manage date column
  dplyr::rename(date = `Date/Time`) %>% 
  mutate(date = ifelse(date == "", NA, date),
         # mutate(date = case_when(is.na(date) & !is.na(Date) ~ as.character(Date), TRUE ~ date)), # There is an issue with these values
         date = as.Date(gsub("T.*", "", date))) %>%
  # Manage depth column
  mutate(depth = case_when(!is.na(`Depth water [m]`) ~ as.numeric(`Depth water [m]`),
                           !is.na(`Press [dbar]`) ~ as.numeric(`Press [dbar]`))) %>%
  mutate(depth = case_when(is.na(depth) & !is.na(`Elevation [m]`) ~ -as.numeric(`Elevation [m]`),
                           is.na(depth) & !is.na(`Elevation [m a.s.l.]`) ~ -as.numeric(`Elevation [m a.s.l.]`),
                           TRUE ~ depth)) %>% 
    # dplyr::select(depth, everything())
  # Remove unwanted columns
  # dplyr::select(-"Longitude e", -"Latitude e", -"Press [dbar]", -contains(c("Depth ", "Elevation "))) %>%
  # Finish up
  dplyr::select(date_accessed, URL, citation, lon, lat, date, depth, everything()) %>% 
  mutate_at(c(7:length(.)), as.numeric) %>% 
  janitor::remove_empty("cols")
colnames(pg_por_clean)

## Individual category data.frames
# Cryosphere
pg_por_Cryosphere <- pg_var_melt(pg_por_clean, query_Cryosphere$pg_col_name, "cryo")
# Physical
pg_por_Physical <- pg_var_melt(pg_por_clean, query_Physical$pg_col_name, "phys")
# Carbonate chemistry
pg_por_Chemistry <- pg_var_melt(pg_por_clean, query_Chemistry$pg_col_name, "chem")
# Biology
pg_por_Biology <- pg_var_melt(pg_por_clean, query_Biology$pg_col_name, "bio") # empty
# Social
pg_por_Social <- pg_var_melt(pg_por_clean, query_Social$pg_col_name, "soc") # empty

# Stack them together
pg_por_ALL <- rbind(pg_por_Cryosphere, pg_por_Physical, pg_por_Chemistry)
data.table::fwrite(pg_por_ALL, "~/pCloudDrive/FACE-IT_data/porsangerfjorden/pg_por_ALL.csv")

# Check that all columns were used
colnames(pg_por_clean)[!colnames(pg_por_clean) %in% unique(pg_por_ALL$var_name)]

# Clean up
rm(list = grep("pg_por",names(.GlobalEnv),value = TRUE)); gc()


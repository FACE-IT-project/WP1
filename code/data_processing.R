# code/data_processing.R
# This script is used to process data downloaded from multiple sources
# It is also used to prep FACE-IT files for PANGAEA that will not be amalgamated
# Generally this is for ex situ or lab experiments

# NB: These files are stored on pCloud
# Contact Robert Schlegel for access: robert.schlegel@imev-mer.fr


# Setup -------------------------------------------------------------------

# Start with common project code
source("code/functions.R")

# Script specific libraries
library(tidync)
library(ncdump)
library(ggOceanMaps)


# SOCAT -------------------------------------------------------------------

# Process SOCAT data into R format
SOCAT_R <- read_delim("~/pCloudDrive/FACE-IT_data/socat/SOCATv2022.tsv", delim = "\t", skip = 6976)
SOCAT_R_sub <- dplyr::select(SOCAT_R, yr, mon, day, `longitude [dec.deg.E]`, `latitude [dec.deg.N]`,
                             `sample_depth [m]`, `ETOPO2_depth [m]`, `pCO2water_SST_wet [uatm]`) %>% 
  mutate(yr = as.numeric(yr), mon = as.numeric(mon), day = as.numeric(day))
write_rds(SOCAT_R_sub, "~/pCloudDrive/FACE-IT_data/socat/SOCATv2022.rds", compress = "gz")
rm(SOCAT_R, SOCAT_R_sub); gc()


# GLODAP ------------------------------------------------------------------

# Process GLODAP data into R format
GLODAP <- read_csv("~/pCloudDrive/FACE-IT_data/glodap/GLODAPv2.2022_Merged_Master_File.csv") %>% 
  `colnames<-`(gsub("G2","",colnames(.))) %>% 
  dplyr::rename(lon = longitude, lat = latitude) %>% 
  unite(year, month, day, sep = "-", remove = T, col = "date") %>% 
  mutate(date = as.Date(date))
write_rds(GLODAP, "~/pCloudDrive/FACE-IT_data/glodap/GLODAPv2.2022.rds", compress = "gz")
rm(GLODAP); gc()


# GLODAP bottles ----------------------------------------------------------

# NetCDF info
ncdump::NetCDF(paste0(pCloud_path,"FACE-IT_data/GLODAPv2.2016b_MappedClimatologies/GLODAPv2.2016b.TCO2.nc"))$variable

# Bottle data
GLODAP_Arctic_bottle <- read_csv(paste0(pCloud_path,"FACE-IT_data/GLODAPv2.2020_Arctic_Ocean.csv")) %>% 
  `colnames<-`(sub("G2", "", colnames(.))) %>% 
  mutate(t = as.Date(paste(year, month, day, sep = "-"))) %>% 
  dplyr::rename(lon = longitude, lat = latitude, TCO2 = tco2, TAlk = talk) %>% 
  dplyr::select(lon, lat, depth, t, TCO2, TAlk, salinity, temperature) %>% 
  replace(is.na(.), NA)

# Bottle data averaged over all sites/CTD stations
GLODAP_Arctic_bottle_mean <- GLODAP_Arctic_bottle %>% 
  group_by(lon, lat, depth) %>% 
  summarise_all(mean, na.rm = T, .groups = "drop") %>%
  # mutate(TCO2 = ifelse(is.na(TCO2), NA, TCO2))
  replace(is.na(.), NA)

# NetCDF layers
GLODAP_depth <- tidync::tidync(paste0(pCloud_path,"FACE-IT_data/GLODAPv2.2016b_MappedClimatologies/GLODAPv2.2016b.TCO2.nc")) %>% 
  tidync::activate("D2") %>% 
  tidync::hyper_tibble() %>% 
  dplyr::rename(depth = Depth)
GLODAP_TCO2_grid <- tidync(paste0(pCloud_path,"FACE-IT_data/GLODAPv2.2016b_MappedClimatologies/GLODAPv2.2016b.TCO2.nc")) %>% 
  hyper_tibble() %>%
  dplyr::select(lon, lat, depth_surface, TCO2)
GLODAP_TAlk_grid <- tidync(paste0(pCloud_path,"FACE-IT_data/GLODAPv2.2016b_MappedClimatologies/GLODAPv2.2016b.TAlk.nc")) %>% 
  hyper_tibble() %>% 
  dplyr::select(lon, lat, depth_surface, TAlk)
GLODAP_sal_grid <- tidync(paste0(pCloud_path,"FACE-IT_data/GLODAPv2.2016b_MappedClimatologies/GLODAPv2.2016b.salinity.nc")) %>% 
  hyper_tibble() %>% 
  dplyr::select(lon, lat, depth_surface, salinity)
GLODAP_temp_grid <- tidync(paste0(pCloud_path,"FACE-IT_data/GLODAPv2.2016b_MappedClimatologies/GLODAPv2.2016b.temperature.nc")) %>% 
  hyper_tibble() %>% 
  dplyr::select(lon, lat, depth_surface, temperature)

# Join depth data and correct longitudes to -180 to 180
GLODAP_carb_chem_grid <- GLODAP_TCO2_grid %>% 
  full_join(GLODAP_TAlk_grid, by = c("lon", "lat", "depth_surface")) %>% 
  full_join(GLODAP_sal_grid, by = c("lon", "lat", "depth_surface")) %>% 
  full_join(GLODAP_temp_grid, by = c("lon", "lat", "depth_surface")) %>% 
  mutate(lon = ifelse(lon > 180, lon-360, lon)) %>% 
  left_join(GLODAP_depth, by = c("depth_surface")) %>% 
  dplyr::select(lon, lat, depth, TCO2, TAlk, salinity, temperature)

# Request form Jean-Pierre Gattuso for Arctic carbonate chemistry layers
# 200 metres and deeper, gridded data
# 200 metres and shallower, station data (bottle data)
GLODAP_Arctic_carb_chem <- GLODAP_Arctic_bottle_mean %>% 
  filter(depth <= 200) %>% 
  rbind(., filter(GLODAP_carb_chem_grid, depth >= 200)) %>% 
  filter(lon >= min(GLODAP_Arctic_bottle_mean$lon),
         lon <= max(GLODAP_Arctic_bottle_mean$lon),
         lat >= min(GLODAP_Arctic_bottle_mean$lat),
         lat <= max(GLODAP_Arctic_bottle_mean$lat))
write_csv(GLODAP_Arctic_carb_chem, paste0(pCloud_path,"FACE-IT_data/GLODAP_Arctic_carb_chem.csv"))


# Shape files for bathymetry ----------------------------------------------

# In order to use custom bathymetry files in ggOceanMaps one must first create 
# matching land and glacier shapefiles
# Below is a convenience function that does all of this when given just a single file
# NB: This requires that one has pCloud mounted at the root directory
prep_bathy <- function(file_name){
  
}


# Marambio dataset --------------------------------------------------------

# See "~/pCloudDrive/restricted_data/Marambio/.~lock.Marambio 2022_PANGEA_PALMARIA.xlsx"
# for metadata used below
# 1, 3, 6, 10, 15 and 21
# Load each sheet
mar_1 <- read_csv_arrow("~/pCloudDrive/restricted_data/Marambio/Marambio 2022_PANGEA_PALMARIA_1.csv")
mar_2 <- read_csv_arrow("~/pCloudDrive/restricted_data/Marambio/Marambio 2022_PANGEA_PALMARIA_2.csv")
mar_3 <- read_csv_arrow("~/pCloudDrive/restricted_data/Marambio/Marambio 2022_PANGEA_PALMARIA_3.csv")
mar_4 <- read_csv_arrow("~/pCloudDrive/restricted_data/Marambio/Marambio 2022_PANGEA_PALMARIA_4.csv")

# Process pigment file
mar_1_proc <- mar_1 %>% 
  mutate(Replicate = rep(seq(1, 3), nrow(mar_1)/3)) %>% 
  pivot_longer(t1:t6, names_to = "Day", values_to = "value") %>% 
  pivot_wider(names_from = Parameter, values_from = value)

# Process fv/fm file
mar_2_proc <- mar_2 %>% 
  mutate(Replicate = rep(seq(1, 3), nrow(mar_2)/3)) %>% 
  pivot_longer(t1:t6, names_to = "Day", values_to = "value") %>% 
  pivot_wider(names_from = Parameter, values_from = value)

# Process NPQ file
mar_3_proc <- mar_3 %>% 
  pivot_longer(HL_t1:LL_t6, names_to = "Day", values_to = "value") %>% 
  separate(Day, into = c("Treat_Light", "Day")) %>% 
  mutate(col_names = paste0("NPQ [PAM-Light Curve ",`PAM-Light Curve`," µmol photons m-2 s-1]")) %>% 
  dplyr::select(-Parameter, -`PAM-Light Curve`) %>% 
  pivot_wider(names_from = col_names, values_from = value)

# Process DPPH file
mar_4_proc <- mar_4 %>% 
  mutate(Replicate = rep(seq(1, 3), nrow(mar_4)/3)) %>% 
  pivot_longer(t1:t6, names_to = "Day", values_to = "value") %>% 
  pivot_wider(names_from = Parameter, values_from = value)

# Combine all files
join_columns <- c("Species", "Family", "Treat_Light", "Treat_SA", "Replicate", "Day")
mar_full <- left_join(mar_1_proc, mar_2_proc, by = join_columns) |>  
  left_join(mar_3_proc, by = join_columns) |>  
  left_join(mar_4_proc, by = join_columns) |>  
  mutate(Treat_Light = case_when(Treat_Light == "HL" ~ 500, Treat_Light == "LL" ~ 50),
       Day = case_when(Day  == "t1" ~ 1, Day  == "t2" ~ 3, Day  == "t3" ~ 6,
                       Day  == "t4" ~ 10, Day  == "t5" ~ 15, Day  == "t6" ~ 21)) |>  
  dplyr::select(Species, Family, Replicate, Day,  everything()) |>  
  dplyr::rename(`Repl [#]` = Replicate, `Treat light [µmol photons m-2 s-1]` = Treat_Light, `Treat sal` = Treat_SA, 
                `Chl a [µg/g]` = Chla, `Lut [µg/g]` = Lut, `β Car [µg/g]` = `β Car`, `Zea [µg/g]` = Zeax, `DPPH [µg/g]` = DPPH) |> 
  # Requested by PANGAEA
  mutate_at(12:19, ~as.character(.)) |> 
  mutate_at(12:19, ~replace_na(.,""))
write_csv(mar_full, "~/pCloudDrive/restricted_data/Marambio/Marambio_full.csv")


# Lebrun dataset ----------------------------------------------------------

Lebrun_data <- read_csv("~/pCloudDrive/restricted_data/Lebrun/Fauna_biomass_Table.csv") |> 
  dplyr::rename(DOI = Doi, `Cruise ID` = CruiseID, `Latitude [°N]` = Latitude, `Longitude [°E]` = Longitude,
                `Depth min [m]` = Min_depth, `Depth max [m]` = Max_depth, 
                `Month start` = st_month, `Month end` = end_month,
                `Year start` = st_year, `Year end` = end_year, `Day(s)` = Day,
                `Replicates [n]` = Replicates_number,
                `Biomass [AFDW g/m^2]` = Biomass_gAFDW_m2, `Biomass [AFDW g/m^2 sd]` = Biomass_gAFDW_m2_sd,
                `Biomass [C g/m^2]` = Biomass_gC_m2, `Biomass [C g/m^2 sd]` = Biomass_gC_m2_sd,
                `Biomass [ww g/m^2]` = Biomass_ww_g_m2, `Biomass [ww g/m^2 sd]` = Biomass_ww_g_m2_sd,
                `Abundance [ind/m^2]` = Abundance_ind_m2, `Abundance [ind/m^2 sd]` = Abundance_ind_m2_sd) |> 
  separate(`Day(s)`, into = c("day_start", "day_end"), sep = '-', fill = "right", remove = T, convert = T) |> 
  mutate(day_start = case_when(is.na(day_start) ~ as.integer(1), TRUE ~ day_start),
         day_end = case_when(is.na(day_end) ~ day_start, TRUE ~ day_end),
         month_start = match(`Month start`, tolower(month.name)),
         month_end = match(`Month end`, tolower(month.name))) |> 
  unite("Date/Time [start]", sep = "-", remove = T, `Year start`, month_start, day_start) |> 
  unite("Date/Time [end]", sep = "-", remove = T, `Year end`, month_end, day_end) |> 
  mutate(`Date/Time [start]` = as.Date(`Date/Time [start]`),
         `Date/Time [end]` = as.Date(`Date/Time [end]`)) |> 
  dplyr::select(-`Month start`, -`Month end`) |> 
  # Requested by PANGAEA
  mutate_at(1:26, ~as.character(.)) |> 
  mutate_at(1:26, ~replace_na(., ""))
write_csv(Lebrun_data, "~/pCloudDrive/restricted_data/Lebrun/Lebrun_data_tidy.csv")

# Test visual
ggplot(data = Lebrun_data, aes(x = `Longitude [°E]`, y = `Latitude [°N]`)) +
  borders() + geom_point()


# Gattuso dataset ---------------------------------------------------------

## NB: These times must be in UTC
# This is done automatically via source("code/functions.R") so shouldn't be necessary here
# Sys.setenv(TZ = 'UTC')
Gattuso_data <- read_csv_arrow("~/pCloudDrive/restricted_data/Gattuso/AWIPEV-CO2_v1.csv") |> 
  mutate(datetime = as.POSIXct(datetime, origin = "1970-01-01")) |> 
  separate(datetime, into = c("Date", "Time"), sep = " ") |>  
  mutate(`date/time [UTC+0]` = paste(Date, Time, sep = "T")) |> 
  dplyr::select(`date/time [UTC+0]`, everything(), -Date, -Time) |> 
  mutate_at(1:13, ~as.character(.)) |>
  mutate_at(1:13, ~replace_na(., ""))
write_csv(Gattuso_data, "~/pCloudDrive/restricted_data/Gattuso/AWIPEV-CO2_v1_tidy.csv")


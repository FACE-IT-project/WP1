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


# Bartsch data ------------------------------------------------------------

## Light data from Inka
# NB: The sensors get dirty throughout the year so values after the winter dark period are best to remove
kong_light_Inka_1 <- read_csv("~/pCloudDrive/restricted_data/Inka_PAR/PAR_Hansneset_KelpForest_10M_above kelp canopy_July2012-June2013_final.csv") %>% 
  slice(1:15999) %>% mutate(date = as.Date(date, format = "%d/%m/%Y"), depth = 10) %>% 
  dplyr::select(-`...4`) %>% pivot_longer(`PAR [umol m-2 s-1]`, names_to = "variable")
kong_light_Inka_2 <- read_csv("~/pCloudDrive/restricted_data/Inka_PAR/PAR_Hansneset_KelpForest_15M_3Jul2012-13Jun2013_final.csv") %>% 
  slice(1:16074) %>% mutate(date = as.Date(date, format = "%d/%m/%Y"), depth = 15) %>% pivot_longer(`PAR [umol m-2 s-1]`, names_to = "variable")
klib <- read_csv("~/pCloudDrive/restricted_data/Inka_PAR/PAR_Hansneset_KelpForest_All_Depths_4July-31July2012_final.csv") 
kong_light_Inka_3 <- rbind(data.frame(date = klib$date, time = klib$time...2,
                                      value = klib$`2.3 UNTEN PAR [µmol m-2 s-1]`, variable = "2.3 UNTEN PAR [µmol m-2 s-1]"),
                           data.frame(date = klib$date, time = klib$time...4,
                                      value = klib$`1.7 OBEN PAR [µmol m-2 s-1]`, variable = "1.7 OBEN PAR [µmol m-2 s-1]"),
                           data.frame(date = klib$date, time = klib$time...6,
                                      value = klib$`4.2 OBEN PAR [µmol m-2 s-1]`, variable = "4.2 OBEN PAR [µmol m-2 s-1]"),
                           data.frame(date = klib$date, time = klib$time...8,
                                      value = klib$`4.8 UNTEN PAR [µmol m-2 s-1]`, variable = "4.8 UNTEN PAR [µmol m-2 s-1]"),
                           data.frame(date = klib$date, time = klib$time...10,
                                      value = klib$`9.2 OBEN PAR [µmol m-2 s-1]`, variable = "9.2 OBEN PAR [µmol m-2 s-1]"),
                           data.frame(date = klib$date, time = klib$time...12,
                                      value = klib$`9.8 UNTEN PAR [µmol m-2 s-1]`, variable = "9.8 UNTEN PAR [µmol m-2 s-1]"),
                           data.frame(date = klib$date, time = klib$time...14,
                                      value = klib$`14.8 PAR [µmol m-2 s-1]`, variable = "14.8 PAR [µmol m-2 s-1]")) %>% 
  mutate(depth = case_when(grepl("14.8", variable) ~ 14.8, grepl("1.7", variable) ~ 1.7, grepl("2.3", variable) ~ 2.3, 
                           grepl("4.2", variable) ~ 4.2, grepl("4.8", variable) ~ 4.8, grepl("9.2", variable) ~ 9.2, grepl("9.8", variable) ~ 9.8),
         date = as.Date(date, format = "%d/%m/%Y"),
         variable = case_when(grepl("OBEN", variable) ~ "PAR above canopy [µmol m-2 s-1]",
                              grepl("UNTEN", variable) ~ "PAR below canopy [µmol m-2 s-1]",
                              TRUE ~ "PAR [µmol m-2 s-1]")) %>% 
  filter(!is.na(time)) # Missing time is also 0 values
kong_light_Inka_hourly <- bind_rows(kong_light_Inka_1, kong_light_Inka_2, kong_light_Inka_3) %>% 
  mutate(note = case_when(grepl("above", variable) ~ "Above canopy",
                          grepl("below", variable) ~ "Below canopy",
                          TRUE ~ "No canopy"),
         `longitude [°E]` = 11.9872, `latitude [°N]` = 78.9958,
         date = paste(date, time, sep = "T")) %>% arrange(depth) %>% 
  dplyr::rename(`PAR [µmol m-2 s-1]` = value, `depth [m]` = depth, `date/time [UTC+0]` = date) %>% 
  dplyr::select(`longitude [°E]`, `latitude [°N]`, `date/time [UTC+0]`, `depth [m]`, `PAR [µmol m-2 s-1]`, note)
# write_delim(kong_light_Inka_hourly, "~/pCloudDrive/restricted_data/Inka_PAR/Bartsch_PAR_Hansneset.csv", delim = "\t")
# kong_light_Inka_PG <- read_delim("~/Downloads/Kongsfjorden_Hansneset_PAR.tab", delim = "\t", skip = 20) # The published data
kong_light_Inka <- bind_rows(kong_light_Inka_1, kong_light_Inka_2, kong_light_Inka_3) %>% 
  mutate(lon = 11.9872, lat = 78.9958,
         variable = case_when(variable == "PAR [umol m-2 s-1]" ~ "PAR [µmol m-2 s-1]", TRUE ~ variable),
         category = "phys", 
         date_accessed = as.Date("2022-03-24"),
         URL = "Received directly from Inka Bartsch", 
         citation = "Bartsch, I., Paar, M., Fredriksen, S., Schwanitz, M., Daniel, C., Hop, H., & Wiencke, C. (2016). Changes in kelp forest biomass and depth distribution in Kongsfjorden, Svalbard, between 1996–1998 and 2012–2014 reflect Arctic warming. Polar Biology, 39(11), 2021-2036.") %>% 
  group_by(date_accessed, URL, citation, lon, lat, date, depth, category, variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")
rm(kong_light_Inka_1, kong_light_Inka_2, kong_light_Inka_3, klib, kong_light_Inka_hourly); gc()


# Niedzwiedz data ---------------------------------------------------------

# Light and kelp data from Sarina's 2022 paper
kong_NiedzKelp <- read_csv("~/pCloudDrive/restricted_data/Niedzwiedz/dataKelp.csv") %>% 
  fill(Length, Width, Stipe.Length, Area.discs, µmol.0, µmol.0.h.cm, µmol.24, µmol.24.h.cm, 
       Comp.irr, Comp.irr.log, Chla.cm.tR, Acc.cm.tR, Acc.Chla.tR, N.Perc, C.Perc, CN) %>% 
  mutate(µmol.0 = round(µmol.0, 2), 
         µmol.0.h.cm = round(µmol.0.h.cm, 2), 
         Comp.irr = round(Comp.irr, 2), 
         Chla.cm = round(Chla.cm, 2), 
         Chla.cm.tR = round(Chla.cm.tR, 2), 
         Acc.cm = round(Acc.cm, 2), 
         Acc.cm.tR = round(Acc.cm.tR, 2),
         Acc.Chla = round(Acc.Chla, 2), 
         Acc.Chla.tR = round(Acc.Chla.tR, 2), 
         CN = round(CN, 2)) %>% 
  dplyr::rename(`Experiment day` = Exp.Day, `Treat temp [°C]` = Temperature,
                `phylloid length [cm]` = Length, `phylloid width [cm]` = Width, `cauloid length [cm]` = Stipe.Length,
                `disc area [cm-2]` = Area.discs, `FW [g]` = FW, `DW [g]` = DW, `Fv/Fm` = Fv.Fm,
                `O2 at 0 µmol photons m-2 s-1 [µmol l-1 s-1]` = µmol.0,
                `O2 at 0 µmol photons m-2 s-1 [µmol l-1 h-1 cm-2]` = µmol.0.h.cm,
                `O2 at 24 µmol photons m-2 s-1 [µmol l-1 s-1]` = µmol.24,
                `O2 at 24 µmol photons m-2 s-1 [µmol l-1 h-1 cm-2]` = µmol.24.h.cm,
                `Compensation E [mol m-2 s-1]` = Comp.irr,
                `Compensation E [log(mol m-2 s-1)]` = Comp.irr.log,
                `Chl a [µg cm-2]` = Chla.cm, `Chl a mean [µg cm-2]` = Chla.cm.tR,
                `Pigm acc [µg cm-2]` = Acc.cm, `Pigm acc mean [µg cm-2]` = Acc.cm.tR,
                `Pigm acc/chl a [µg cm-2]` = Acc.Chla, `Pigm acc/chl a mean [µg cm-2]` = Acc.Chla.tR,
                `N [%]` = N.Perc, `C [%]` = C.Perc) %>% 
  mutate(Species = case_when(Species == "Slat" ~ "Saccharina latissima",
                             Species == "Aesc" ~ "Alaria esculenta"))
write_delim(kong_NiedzKelp, "~/pCloudDrive/restricted_data/Niedzwiedz/dataKelp_PG.csv", delim = "\t")
kong_NiedzLight <- read_csv("~/pCloudDrive/restricted_data/Niedzwiedz/dataLight.csv")
kong_NiedzLight_PG <- kong_NiedzLight %>% 
  dplyr::rename(`longitude [°E]`= Longitude, `latitude [°N]` = Latitude, `depth [m]` = Depth,
                `PAR [µmol m-2 s-1]` = PAR, `PAR [log(µmol m-2 s-1)]`= `log(PAR)`,
                `UV-A [µmol m-2 s-1]` = UV.A, `UV-B [µmol m-2 s-1]` = UV.B, 
                `E [µmol m-2 s-1]` = Surface.irr, Sal = Surface.Salinity) %>% 
  mutate(DateTime = DateTime-7200) %>% # Correct from Svalbard (UTC+2) to UTC+0
  separate(DateTime, into = c("Date", "Time"), sep = " ") %>% 
  mutate(`date/time [UTC+0]` = paste(Date, Time, sep = "T")) %>%
  dplyr::select(Station, `latitude [°N]`, `longitude [°E]`, `date/time [UTC+0]`, `depth [m]`,
                `E [µmol m-2 s-1]`, `PAR [µmol m-2 s-1]`, `PAR [log(µmol m-2 s-1)]`, 
                `UV-A [µmol m-2 s-1]`, `UV-B [µmol m-2 s-1]`, Sal) %>% 
  group_by(Station, `latitude [°N]`, `longitude [°E]`) %>% 
  arrange(`depth [m]`, .by_group = TRUE) %>% ungroup()
write_delim(kong_NiedzLight_PG, "~/pCloudDrive/restricted_data/Niedzwiedz/dataLight_PG.csv", delim = "\t")


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
  separate(`date/time [UTC+0]`, into = c("Date", "Time"), sep = " ") |>  
  mutate(`date/time [UTC+0]` = paste(Date, Time, sep = "T")) |> 
  dplyr::select(`date/time [UTC+0]`, everything(), -Date, -Time) |> 
  mutate_at(1:13, ~as.character(.)) |>
  mutate_at(1:13, ~replace_na(., ""))
write_csv(Gattuso_data, "~/pCloudDrive/restricted_data/Gattuso/AWIPEV-CO2_v1_tidy.csv")

# Version 2
Gattuso_data2 <- read_csv_arrow("~/pCloudDrive/restricted_data/Gattuso/AWIPEV-CO2_v2.csv") |> 
  separate(`date/time [UTC+0]`, into = c("Date", "Time"), sep = "T") |>
  mutate(Time = case_when(Time == "NA" ~ "00:00:00", TRUE ~ Time)) |> 
  mutate(`date/time [UTC+0]` = paste(Date, Time, sep = "T")) |> 
  dplyr::select(`date/time [UTC+0]`, everything(), -Date, -Time) |> 
  mutate_at(1:13, ~as.character(.)) |>
  mutate_at(1:13, ~replace_na(., ""))
write_csv(Gattuso_data2, "~/pCloudDrive/restricted_data/Gattuso/AWIPEV-CO2_v2_PG.csv")


# Miller dataset ----------------------------------------------------------

# Load each dataset and join
Miller_flow <- read_csv("~/pCloudDrive/restricted_data/Miller/Flow Rate.csv", na = c("NaN", -99))
Miller_O2 <- read_csv("~/pCloudDrive/restricted_data/Miller/O2.csv", na = "NaN")
Miller_sal <- read_csv("~/pCloudDrive/restricted_data/Miller/Sal.csv", na = "NaN")
Miller_temp <- read_csv("~/pCloudDrive/restricted_data/Miller/Temp.csv", na = "NaN")
Miller_data <- left_join(Miller_flow, Miller_O2, by = "DateTime") |> 
  left_join(Miller_sal, by = "DateTime") |> left_join(Miller_temp, by = "DateTime") |> 
  separate(DateTime, into = c("date", "time"), sep = " ") |> 
  mutate(date = as.Date(date, "%m/%d/%y"),
         `date/time [UTC+0]` = paste(date, time, sep = "T")) |> 
  dplyr::select(`date/time [UTC+0]`, `C0M0_Flow rate (L min-1)`:C3M2_Temp) |> 
  pivot_longer(`C0M0_Flow rate (L min-1)`:C3M2_Temp) |> 
  separate(name, into = c("replicate", "variable"), sep = "_") |> 
  pivot_wider(names_from = variable, values_from = value) |> 
  dplyr::rename(`Flow rate [L/min]` = `Flow rate (L min-1)`, `O2 [%]` = `% O2`, `Temp [°C]` = Temp)
write_csv(Miller_data, "~/pCloudDrive/restricted_data/Miller/Miller_tidy.csv")


# Lund-Hansen -------------------------------------------------------------

# Ice algae mat dataset
ice_algae_1 <- read_csv("~/pCloudDrive/restricted_data/Lund-Hansen/Algae mat/aggregat_clean.csv")
ice_algae_2 <- read_csv("~/pCloudDrive/restricted_data/Lund-Hansen/Algae mat/flux_clean.csv")
ice_algae <- bind_rows(ice_algae_1, ice_algae_2) |> 
  mutate(`date/time [UTC+0]` = paste0(Date,"T00:00:00"),
         `Ice core depth [mm]` = case_when(is.na(`Ice core depth [mm]`) ~ `Ice core depth [cm]`/10, 
                                           TRUE ~ `Ice core depth [mm]`),
         `O2 conc. [%]` = case_when(is.na(`O2 conc. [%]`) ~ round(`O2 dissolved [umol/l]`/`O2 conc. [umol/l]`, 2),
                                    TRUE ~ `O2 conc. [%]`),
         `O2 conc. [%]` = case_when(is.infinite(`O2 conc. [%]`) ~ as.numeric(NA), TRUE ~ `O2 conc. [%]`)) |> 
  dplyr::select(`date/time [UTC+0]`, Profile, `Ice core depth [mm]`, `Temp. [°C]`, `Salt conc. [%]`, `Salinity`, 
                `O2 dissolved [umol/l]`, `O2 conc. [%]`, `O2 conc. [umol/l]`) |> 
  mutate_at(1:9, ~as.character(.)) |>
  mutate_at(1:9, ~replace_na(., ""))
write_csv(ice_algae, "~/pCloudDrive/restricted_data/Lund-Hansen/Algae mat/algae_mat_PG.csv")


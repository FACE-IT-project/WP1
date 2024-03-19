# code/data_extraction.R
# Herein the documentation of requested extractions from the FACE-IT dataset


# Setup -------------------------------------------------------------------

source("code/functions.R")

if(!exists("clean_all")) load("data/analyses/clean_all.RData")

# Subset high-res coastline
if(!exists("coastline_full")) load("metadata/coastline_full_df.RData")
coastline_full_df_kong <- coastline_full_df %>% 
  filter(between(x, bbox_kong[1]-1, bbox_kong[2]+1),
         between(y, bbox_kong[3]-1, bbox_kong[4]+1))


# Extract -----------------------------------------------------------------

## Anäis ------------------------------------------------------------------

# Jan 3, 2023
## Kongsfjorden
### T°, PAR, Salinity, Chla, pH
#### depth: surface data would already be good but, if possible, data at 7/8m depth would be awesome. 
#### period and frequency: the whole year if possible, but especially spring/summer. Weekly data would be nice.
### how much points along the fjord: the idea of the eDNA project is to characterise the light/salinity gradient 
  # that we have along the fjord so 4 to 6 points would be awesome.
### Create spatial average of all data within 500 m of study sites
### Create weekly averages based on Monday start
### Finish this by Jan 19

# Load site list
lebrun_sites <- read_csv_arrow("users/lebrun/Sites.csv") %>% 
  dplyr::rename(lon = Longitude, lat = Latitude)

lebrun_base <- clean_all %>% 
  filter(site == "kong",
         date >= as.Date("2000-01-01"),
         driver %in% c("sea temp", "salinity", "light", "prim prod", "carb"),
         variable %in% c("temp [°C]", "sal", "PAR [µmol m-2 s-1]", "pH in situ [total scale]", "Chla [µg/l]"),
         between(depth, 0, 8))
# Unique coords - 6711
length(unique(paste0(lebrun_base$lon, lebrun_base$lat)))
# Unique days of sampling - 8007
length(unique(lebrun_base$date))

# Get coordinates
lebrun_coords <- lebrun_base %>% 
  dplyr::select(lon, lat) %>% distinct() %>% na.omit() %>% 
  grid_match(., lebrun_sites[,c(3,2,1)]) %>% 
  dplyr::rename(lon = lon.x, lat = lat.x, lon_site = lon.y, lat_site = lat.y)

# Get references for data used
lebrun_reference <- lebrun_base %>% 
  left_join(lebrun_coords, by = c("lon", "lat")) %>% filter(dist <= 1) %>% 
  dplyr::select(date_accessed, URL, citation) %>% distinct()
write_csv_arrow(lebrun_reference, "users/lebrun/lebrun_reference.csv")

# Filter sites within 500m and create weekly averages
lebrun_weekly <- lebrun_base %>% 
  mutate(depth = round(depth)) %>% 
  left_join(lebrun_coords, by = c("lon", "lat")) %>% 
  filter(dist <= 1) %>%
  group_by(Site, depth, year = year(date), week = week(date), variable) %>% 
  summarise(value = mean(value, na.rm = T), .groups = "drop")
write_csv_arrow(lebrun_weekly, "users/lebrun/lebrun_weekly.csv")


## Steeve -----------------------------------------------------------------

# Feb 2, 2024
## Temperature data at specific points around Tromso
coords_steeve <- data.frame(lon = c(19.11, 18.9, 17.96),
                            lat = c(69.45, 69.85, 69.60))

# Load NOAA OISST
load("data/sst_trom.RData")

# Load CCI data
# load("data/sst_CCI_trom.RData") # NB: Only to 2020-12-31

# Visualise satellite product
sst_trom |> 
  filter(t == "2022-07-01") |> 
  ggplot(aes(x = lon, y = lat)) +
  geom_raster(aes(fill = temp)) +
  scale_fill_viridis_c()

# Get NOAA grid
sst_grid <- dplyr::select(sst_trom, lon, lat) |> distinct()

# Extract nearest SST pixels
sst_coords <- grid_match(coords_steeve, sst_grid) |> 
  dplyr::rename(lon_site = lon.x, lat_site = lat.x, lon_sst = lon.y, lat_sst = lat.y)
write_csv(sst_coords, "data/analyses/sst_coords.csv")
sst_steeve <- right_join(sst_trom, sst_coords, by = c("lon" = "lon_sst", "lat" = "lat_sst"))
write_csv(sst_steeve, "data/analyses/sst_steeve.csv")

# Plot pixels
ggplot(data = sst_steeve) +
  geom_point(aes(x = lon, y = lat), shape = 1) +
  geom_point(aes(x = lon_sst, y = lat_sst), shape = 0) +
  borders() +
  coord_quickmap(xlim = bbox_trom[1:2], ylim = bbox_trom[3:4])

# Load in situ data scraped from PANGAEA
trom_data <- data.table::fread("~/pCloudDrive/FACE-IT_data/tromso/pg_trom.csv")
colnames(trom_data)
# Get meta columns
# Get °C columns
trom_temp <- trom_data |> 
  dplyr::select(date_accessed:`Press [dbar]`, `Elevation [m a.s.l.]`, `Date/time start`, `Date/time end`,
                ends_with("[°C]")) |> 
  janitor::remove_empty(which = c("rows", "cols"))
rm(trom_data); gc()
colnames(trom_temp)
data.table::fwrite(trom_temp, "~/pCloudDrive/")


## Chloe ------------------------------------------------------------------

# Load+subset ERSST data
# File locations
ERSST_files <- dir("~/pCloudDrive/FACE-IT_data/ERSST", full.names = TRUE)

# Function for loading and subsetting
ERSST_region <- function(ERSST_file, date_range, lon_range, lat_range){
  vec_file <- sapply(strsplit(ERSST_file, "/"), "[[", 7)
  vec_date_char <- sapply(strsplit(vec_file, "\\."), "[[", 3)
  vec_date <- as.Date(paste0(vec_date_char,"01"), format = "%Y%m%d")
  if(vec_date %in% seq(date_range[1], date_range[2], by = "day")){
    df_mean <- tidync(ERSST_file) |> 
      hyper_filter(lon = between(lon, lon_range[1], lon_range[2]),
                   lat = between(lat, lat_range[1], lat_range[2])) |> 
      hyper_tibble() |> 
      mutate(t = vec_date) |> 
      # summarise(sst = mean(sst), 
      # ssta = mean(ssta), # NB: Reactivate if SSTa is desired 
      # .by = t)
      dplyr::rename(temp = sst) |> 
      dplyr::select(lon, lat, t, temp)
    return(df_mean)
  }
  # rm(vec_file, vec_date_char, vec_date, df_mean)
}

# Load all data
ERSST_global <- plyr::ldply(ERSST_files, ERSST_region, .parallel = T,
                            date_range = c(as.Date("1993-01-01"), as.Date("2017-12-31")), 
                            lon_range = c(0, 360), lat_range = c(-90, 90))

# Correct lon to +-180 rather than 0-360
# This is more convenient because part of the Med is West of 0°
ERSST_global$lon <- ifelse(ERSST_global$lon > 180, ERSST_global$lon-360, ERSST_global$lon)
write_csv(ERSST_global, "~/Desktop/ERSST_global.csv")

# Test plot
filter(ERSST_global, t == "1993-01-01") |> 
  ggplot(aes(x = lon, y = lat)) +
  geom_raster(aes(fill = temp)) +
  borders() +
  scale_fill_viridis_c() +
  labs(x = NULL, y = NULL) +
  coord_quickmap(xlim = c(range(ERSST_global$lon)), 
                 ylim = c(range(ERSST_global$lat)), expand = FALSE) +
  theme(legend.position = "bottom")

# Filter for just the Med
ERSST_med <- ERSST_global |> 
  # The basic bbox
  filter(lon >= -6, lon <= 36, lat >= 30, lat <= 46) |> 
  # Remove the little Atlantic bit
  filter(!(lat > 42 & lon < 0)) |> 
  # Remove the Black Sea
  filter(!(lat > 40 & lon > 26))
write_csv(ERSST_med, "~/Desktop/ERSST_med.csv")

# Test plot
filter(ERSST_med, t == "1993-07-01") |> 
  ggplot(aes(x = lon, y = lat)) +
  geom_raster(aes(fill = temp)) +
  borders() +
  scale_fill_viridis_c() +
  labs(x = NULL, y = NULL) +
  coord_quickmap(xlim = c(-8, 38), ylim = c(28, 48), expand = FALSE) +
  theme(legend.position = "bottom")

# Global SST trend from 1997-2017
ERSST_global_mean <- ERSST_global |> 
  summarise(temp = mean(temp), .by = t)
lm(temp ~ t, ERSST_global_mean)$coefficient[2]*365
# annual rate = 0.014°C, or 0.14°C/decade

# Plot
ggplot(ERSST_global_mean, aes(x = t, y = temp)) +
  geom_line() +
  geom_smooth(method = "lm")

# Med SST trend from 1997-2017
ERSST_med_mean <- ERSST_med |> 
  summarise(temp = mean(temp), .by = t)
lm(temp ~ t, ERSST_med_mean)$coefficient[2]*365
# annual rate = 0.053°C, or 0.53°C/decade

ggplot(ERSST_med_mean, aes(x = t, y = temp)) +
  geom_line() +
  geom_smooth(method = "lm")


## D1.4 --------------------------------------------------------------------
# Create a table of social science data to include in deliverable text

if(!exists("clean_all_meta")) load("data/analyses/clean_all_meta.RData")

# Social data
clean_all_meta_social <- clean_all_meta |> 
  filter(category == "social") |> 
  dplyr::select(citation, URL, driver) |> 
  dplyr::rename(Title = citation, Link = URL, Driver = driver) |> 
  distinct() |> arrange(Title)
write_csv(clean_all_meta_social, file = "~/Desktop/clean_all_meta_social.csv")


## CLIVAR ------------------------------------------------------------------
# Panels for a MHW figure

# Load+subset ERSST data
# File locations
ERSST_files <- dir("~/pCloudDrive/FACE-IT_data/ERSST", full.names = TRUE)

# Run on all files
doParallel::registerDoParallel(cores = 7)
ERSST_NW_med <- plyr::ldply(ERSST_files, ERSST_region_mean, .parallel = T,
                            lon_range = c(0, 10), lat_range = c(40, 50)) |> 
  dplyr::select(-ssta)
write_csv(ERSST_NW_med, "~/Desktop/ERSST_NW_med.csv")

# Create annual means
ERSST_NW_med_annual <- ERSST_NW_med |> 
  mutate(year = year(t)) |> 
  summarise(sst = mean(sst), .by = "year")
write_csv(ERSST_NW_med_annual, "~/Desktop/ERSST_NW_med_annual.csv")

# Determine stats
## 90th perc of baseline 1871-1900
baseline_stats_monthly <- ERSST_NW_med |> 
  filter(t >= "1901-01-01", t <= "1930-12-01") |> 
  summarise(mean = mean(sst),
            perc_90 = quantile(sst, 0.9))
baseline_stats_annual <- ERSST_NW_med_annual |> 
  filter(year >= 1901, year <= 1930) |> 
  summarise(mean = mean(sst),
            perc_90 = quantile(sst, 0.9))

## Trend
detrend_stats_monthly <- lm_tidy(ERSST_NW_med, x_var = "t", y_var = "sst") |> mutate(slope = (slope*365.25)/12)
detrend_stats_annual <- lm_tidy(ERSST_NW_med_annual, x_var = "year", y_var = "sst")
detrend_df_monthly <- expand.grid(1854:2023, 1:12) |> 
  mutate(Var3 = "01", t = as.Date(paste(Var1, Var2, Var3, sep = "-"))) |> arrange(t) |> 
  mutate(month_idx = (1:n())-1,
         sst = ERSST_NW_med$sst,
         slope_mean = baseline_stats_monthly$mean + (detrend_stats_monthly$slope*month_idx),
         slope_90 = baseline_stats_monthly$perc_90 + (detrend_stats_monthly$slope*month_idx))
detrend_df_annual <- data.frame(year = 1854:2023) |> 
  mutate(year_idx = (1:n())-1,
         sst = ERSST_NW_med_annual$sst,
         slope_mean = baseline_stats_annual$mean + (detrend_stats_annual$slope*year_idx),
         slope_90 = baseline_stats_annual$perc_90 + (detrend_stats_annual$slope*year_idx))

# The common theme
theme_new <- theme_set(theme_minimal() +
                         theme(axis.text = element_text(size = 10, colour = "black"),
                               plot.margin = margin(t = 10, r = 30, b = 20, l = 20, unit = "pt"),
                               panel.grid.major.x = element_blank(),
                               panel.grid.minor.x = element_blank(),
                               panel.grid.minor.y = element_blank(),
                               panel.grid.major.y = element_line(linetype = "dotted", colour = "black")))

# NB: The monthly values do not plot well
# Panel a
panel_a <- ggplot(data = ERSST_NW_med_annual, aes(x = year, y = sst)) +
  geom_line(colour = "chocolate") +
  geom_ribbon(aes(ymin = baseline_stats_annual$mean, ymax = baseline_stats_annual$perc_90), alpha = 0.2,
              colour = "grey30", outline.type = "lower") +
  geom_abline(intercept = baseline_stats_annual$mean, slope = 0) +
  scale_x_continuous(expand = c(0, 0), breaks = c(seq(1860, 2020, 20))) +
  scale_y_continuous(expand = c(0, 0), limits = c(16.5, 19.5), breaks = c(seq(16.5, 19.5, 0.5))) +
  labs(y = "Ocean termperature (°C)", x = NULL) +
  theme_new
# panel_a

# Panel b
panel_b <- ggplot(data = ERSST_NW_med_annual, aes(x = year, y = sst)) +
  geom_line(colour = "chocolate") +
  geom_ribbon(data = detrend_df,
              aes(ymin = slope_mean, ymax = slope_90), alpha = 0.2,
              colour = "grey30", outline.type = "lower") +
  # geom_abline(intercept = baseline_stats$perc_90, slope = detrend_stats$slope*170) +
  # geom_abline(intercept = baseline_stats$perc_90, slope = detrend_stats$slope) +
  scale_x_continuous(expand = c(0, 0), breaks = c(seq(1860, 2020, 20))) +
  scale_y_continuous(expand = c(0, 0), limits = c(16.5, 19.5), breaks = c(seq(16.5, 19.5, 0.5))) +
  labs(y = "Ocean termperature (°C)", x = NULL) +
  theme_new
# panel_b

# Combine
fig_2 <- ggpubr::ggarrange(panel_a, panel_b, ncol = 1, nrow = 2, labels = c("a)", "b)")) +
  ggpubr::bgcolor("white")
# fig_2
ggsave("~/Desktop/CLIVAR_fig_2.png", fig_2, height = 10, width = 6)


## Nora --------------------------------------------------------------------

# Load the full spatial Kongsfjorden OISST data
load("~/pCloudDrive/FACE-IT_data/kongsfjorden/sst_kong.RData")

# Seasonal
## Mean from 1980-2000; 2001-2022
sst_analysis <- sst_kong |> 
  mutate(year = year(t),
         month = month(t)) |> 
  mutate(season = case_when(month %in% c(12,1,2) ~ "winter",
                            month %in% c(6,7,8) ~ "summer"),
         year_group = case_when(year %in% 1980:2000 ~ "first",
                                year %in% 2001:2022 ~ "second")) |>
  filter(!is.na(season), !is.na(year_group)) |> 
  summarise(min = min(temp),
            median = median(temp),
            mean = mean(temp),
            max = max(temp), .by = c("year_group", "season"))

# Annual
## Mean from 1980-2000; 2001-2022
sst_analysis <- sst_kong |> 
  mutate(year = year(t)) |> 
  mutate(year_group = case_when(year %in% 1980:2000 ~ "first",
                                year %in% 2001:2022 ~ "second")) |>
  filter(!is.na(year_group)) |> 
  summarise(min = min(temp),
            median = median(temp),
            mean = mean(temp),
            max = max(temp), .by = c("year_group"))



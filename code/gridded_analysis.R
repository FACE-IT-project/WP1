# code/gridded_analysis.R
# This script houses the code used for analysing the gridded model product produced by Morten


# Setup -------------------------------------------------------------------

# Libraries
library(tidyverse)
library(tidync)
library(doParallel); registerDoParallel(cores = 15)

# Find who is the user and define the pCloud path
if (Sys.getenv("LOGNAME") == "gattuso"){
  pCloud_path = "~/pCloud\ Drive/"
} else {
  pCloud_path = "~/pCloudDrive/" 
}


# Data --------------------------------------------------------------------

ncdump::NetCDF(paste0(pCloud_path,"FACE-IT_data/kongsfjorden/kongsfjorden_rcp2.6.nc"))
var_names <- ncdump::NetCDF(paste0(pCloud_path,"FACE-IT_data/kongsfjorden/kongsfjorden_rcp2.6.nc"))$variable
depth_data <- tidync(paste0(pCloud_path,"FACE-IT_data/kongsfjorden/kongsfjorden_rcp2.6.nc")) %>% 
  hyper_tibble()
surface_data <- tidync(paste0(pCloud_path,"FACE-IT_data/kongsfjorden/kongsfjorden_rcp2.6.nc")) %>% 
  activate("D0,D1,D2") %>% 
  hyper_tibble() %>% 
  mutate(Z = 0)
meta_data <- tidync(paste0(pCloud_path,"FACE-IT_data/kongsfjorden/kongsfjorden_rcp2.6.nc")) %>% 
  activate("D0,D1") %>% 
  hyper_tibble()
all_data <- left_join(depth_data, surface_data, by = c("X", "Y", "Z", "T")) %>% 
  left_join(meta_data, by = c("X", "Y")) %>% 
  rename(lon = Long, lat = Latt, t = T, depth = Z) %>% 
  mutate(t = as.Date(as.POSIXct(t*3600, origin = "1950-01-01 01:00"))) %>%
  dplyr::select(RMask, lon, lat, Topo, depth, t, everything(), -X, -Y)


# Visualise ---------------------------------------------------------------

all_data %>%
  mutate(lon = plyr::round_any(lon, 0.5),
         lat = plyr::round_any(lat, 0.1)) %>% 
  group_by(lon, lat) %>% 
  summarise(Temp = mean(Temp, na.rm = T), .groups = "drop") %>% 
  ggplot(aes(x = lon, y = lat)) + 
  # geom_point(aes(colour = Temp)) +
  # geom_tile(aes(fill = Temp)) +
  geom_raster(aes(fill = Temp)) +
  borders(colour = "black", size = 2) +
  geom_rect(aes(xmin = 11, xmax = 12.69, ymin = 78.86, ymax = 79.1), colour = "red", fill = NA) +
  geom_label(aes(x = 11.9009578, y = 78.9247329, label = "Ny-Ålesund")) +
  scale_fill_viridis_c() +
  coord_cartesian(xlim = range(all_data$lon), ylim = range(all_data$lat)) +
  # coord_cartesian(xlim = c(11, 12.69), ylim = c(78.86, 79.1)) +
  labs(y = "latitude (°N)", x = "longitude (°E)", fill = "temp. (°C)") +
  theme(legend.position = "bottom")
ggsave("figures/test_map.png", height = 6, width = 6)

all_data %>% 
  mutate(lonlat = paste0(lon,"_",lat,"_",depth)) %>% 
  ggplot(aes(x = t, y = Temp)) +
  geom_line(aes(group = lonlat, colour = as.factor(depth))) +
  scale_x_date(expand = c(0,0)) +
  labs(x = NULL, y = "Temperature (°C)", colour = "depth (m)")
ggsave("figures/test_ts.png", height = 4, width = 8)


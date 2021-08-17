# code/data_analysis.R
# This script contains the analyses performed on the data products created for WP1


# Setup -------------------------------------------------------------------

# Libraries
source("code/functions.R")

# Remove scientific notation
options(scipen = 9999)

# Set Timezone to UTC
Sys.setenv(TZ = "UTC")

# Bounding boxes
bbox_EU <- c(-60, 60, 63, 90)
bbox_kong <- c(11, 12.69, 78.86, 79.1)
bbox_is <- c(13.62, 17.14, 78.03, 78.71)
bbox_ingle <- c(18.15, 18.79, 77.87, 78.05)
bbox_young <- c(-22.367917, -19.907644, 74.210137, 74.624304)
bbox_disko <- c(-55.56, -49.55, 68.22, 70.5)
bbox_nuup <- c(-53.32, -48.93, 64.01, 64.8)
bbox_por <- c(24.5, 27, 70, 71.2)

# Load data ---------------------------------------------------------------

load("~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")


# Meta-statistics ---------------------------------------------------------

# Tables of value names
table(full_product_kong$var_type)
table(full_product_kong$var_name)
table(full_product_kong$var_name, full_product_kong$var_type)

# Ranges of values
range(full_product_kong$lon, na.rm = T)
range(full_product_kong$lat, na.rm = T)
range(full_product_kong$date, na.rm = T)
range(full_product_kong$depth, na.rm = T)


# Spatial summary ---------------------------------------------------------

# Count per grid cell
full_product_kong %>% 
  dplyr::select(-URL, -citation) %>% 
  mutate(lon = round(lon, 2),
         lat = round(lat, 2)) %>% 
  group_by(lon, lat) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  ggplot(aes(x = lon, y = lat)) +
  borders(fill = "grey30") +
  geom_tile(aes(fill = log10(count))) +
  coord_quickmap(xlim = c(bbox_kong[1:2]), 
                 ylim = c(bbox_kong[3:4])) +
  labs(x = NULL, y = NULL)


# Temporal summary --------------------------------------------------------



# Depth summary -----------------------------------------------------------

# Average temperature over depth
full_product_kong %>% 
  dplyr::select(-URL, -citation) %>% 
  filter(!is.na(depth),
         var_type == "phys") %>% 
  mutate(depth = round(depth, -1),
         year = lubridate::year(date)) %>%
  filter(value > -20, value < 10) %>%
  group_by(depth, year) %>% 
  dplyr::summarise(value = mean(value, na.rm = T),
                   count = n(), .groups = "drop") %>% 
  ggplot(aes(x = year, y = -depth)) +
  geom_tile(aes(fill = value, colour = count), size = 1) +
  scale_colour_distiller(palette = "Reds", direction = 1) +
  scale_fill_viridis_c() +
  coord_cartesian(expand = F) +
  labs(x = NULL, y = "Depth (m)", fill = "Value")


# Climatology -------------------------------------------------------------



# Range summary -----------------------------------------------------------



# Trend summary -----------------------------------------------------------



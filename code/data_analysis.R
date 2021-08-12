# code/data_analysis.R
# This script contains the analyses performed on the data products created for WP1


# Setup -------------------------------------------------------------------

# Libraries
source("code/functions.R")


# Load data ---------------------------------------------------------------

load("~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")


# Meta-statistics ---------------------------------------------------------

# Tables of value names
table(full_product_kong$var_type)
table(full_product_kong$var_name, full_product_kong$var_type)


# Spatial summary ---------------------------------------------------------

# Count per grid cell
full_product_kong %>% 
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


# Temporal summary --------------------------------------------------------



# Depth summary -----------------------------------------------------------

# Average temperature over depth
full_product_kong %>% 
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


# Climatology -------------------------------------------------------------



# Range summary -----------------------------------------------------------



# Trend summary -----------------------------------------------------------



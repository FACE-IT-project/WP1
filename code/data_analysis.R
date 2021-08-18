# code/data_analysis.R
# This script contains the analyses performed on the data products created for WP1
# Note that this code mostly serves as a testing area for the creation of the data summary functions
# that are then used to create the nice layout in the data_summary.Rmd page


# Setup -------------------------------------------------------------------

# Libraries
source("code/functions.R")


# Load data ---------------------------------------------------------------

load("~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")


# Meta-statistics ---------------------------------------------------------

# These need to be combined into a table via a data.frame

# Tables of value names
table(full_product_kong$var_type)
table(full_product_kong$var_name)
table(full_product_kong$var_name, full_product_kong$var_type)

# Ranges of values
range(full_product_kong$lon, na.rm = T)
range(full_product_kong$lat, na.rm = T)
range(full_product_kong$date, na.rm = T)
range(full_product_kong$depth, na.rm = T)

# Table of meta-stats
meta_table <- data.frame(table(full_product_kong$var_type)) %>% 
  pivot_wider(names_from = Var1, values_from = Freq) %>% 
  mutate(lon_min = min(full_product_kong$lon, na.rm = T),
         lon_max = max(full_product_kong$lon, na.rm = T),
         lat_min = min(full_product_kong$lat, na.rm = T),
         lat_max = max(full_product_kong$lat, na.rm = T),
         date_min = min(full_product_kong$date, na.rm = T),
         date_max = max(full_product_kong$date, na.rm = T),
         depth_min = min(full_product_kong$depth, na.rm = T),
         depth_max = max(full_product_kong$depth, na.rm = T),
         site = "Kongsfjorden")

# Or rather text ranges for compact presentation
meta_table <- data.frame(table(full_product_kong$var_type)) %>% 
  pivot_wider(names_from = Var1, values_from = Freq) %>% 
  mutate(lon = paste0(min(full_product_kong$lon, na.rm = T), " to ", max(full_product_kong$lon, na.rm = T)), 
         lat = paste0(min(full_product_kong$lat, na.rm = T), " to ", max(full_product_kong$lat, na.rm = T)),
         date = paste0(min(full_product_kong$date, na.rm = T), " to ", max(full_product_kong$date, na.rm = T)),
         depth = paste0(min(full_product_kong$depth, na.rm = T), " to ", max(full_product_kong$depth, na.rm = T)),
         site = "Kongsfjorden") %>% 
  dplyr::select(site, lon, lat, date, depth, everything())

# Create graphic table
meta_table_g <- gtable_add_grob(tableGrob(meta_table, rows = NULL),
                                grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                                t = 2, b = nrow(meta_table_g), l = 1, r = ncol(meta_table_g))
ggpubr::ggarrange(meta_table_g)


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
  geom_tile(aes(fill = count)) +
  scale_fill_viridis_c() +
  # geom_tile(aes(fill = log10(count))) + # Can look better after log scaling
  coord_quickmap(xlim = c(bbox_kong[1:2]), 
                 ylim = c(bbox_kong[3:4])) +
  labs(x = NULL, y = NULL, title = "All data at 0.01° (~10 km) resolution")

# Temperature count per grid cell
full_product_kong %>% 
  dplyr::select(-URL, -citation) %>% 
  filter(!is.na(depth),
         grepl("°C", var_name)) %>% 
  mutate(lon = round(lon, 2),
         lat = round(lat, 2)) %>% 
  group_by(lon, lat) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  ggplot(aes(x = lon, y = lat)) +
  borders(fill = "grey30") +
  geom_tile(aes(fill = count)) +
  # geom_tile(aes(fill = log10(count))) + # Can look better after log scaling
  scale_fill_viridis_c() +
  coord_quickmap(xlim = c(bbox_kong[1:2]), 
                 ylim = c(bbox_kong[3:4])) +
  labs(x = NULL, y = NULL, title = "Temperature data at 0.01° (~10 km) resolution")


# Temporal summary --------------------------------------------------------

# Count of data
full_product_kong %>% 
  dplyr::select(-URL, -citation) %>% 
  mutate(year = lubridate::year(date)) %>%
  group_by(year, var_type) %>% 
  dplyr::summarise(count = n(), .groups = "drop") %>% 
  ggplot() +
  # geom_col(aes(x = year, y = count, fill = var_type)) +
  geom_col(aes(x = year, y = log10(count), fill = var_type)) +
  coord_cartesian(expand = F) +
  labs(x = NULL, fill = "Variable", y = "Count (log10)",
       title = "Count of data points per year",
       subtitle = "Note that these values are a bit distorted because the log10 is calculated on each group individually") +
  theme(panel.border = element_rect(fill = NA, colour = "black"))


# Depth summary -----------------------------------------------------------

# Count of data at depth by var type
full_product_kong %>% 
  # filter(depth >= 0) %>%
  filter(!is.na(depth)) %>% 
  mutate(depth = round(depth, -1)) %>%
  group_by(depth, var_type) %>% 
  dplyr::summarise(count = n(), .groups = "drop") %>% 
  ggplot() +
  geom_col(aes(x = depth, y = log10(count), fill = var_type)) +
  scale_y_reverse() +
  coord_cartesian(expand = F) +
  labs(x = NULL, fill = "Variable", y = "Count (log10)",
       title = "Count of data at depth",
       subtitle = "Note that these values are a bit distorted because the log10 is calculated on each group individually") +
  theme(panel.border = element_rect(fill = NA, colour = "black"))

# Count of data at depth over time
full_product_kong %>% 
  filter(depth >= 0) %>% 
  mutate(depth = round(depth, -1),
         year = lubridate::year(date)) %>%
  group_by(depth, year) %>% 
  dplyr::summarise(count = n(), .groups = "drop") %>% 
  ggplot(aes(x = year, y = depth)) +
  # geom_tile(aes(fill = count)) +
  geom_tile(aes(fill = log10(count))) +
  scale_y_reverse() +
  # scale_colour_distiller(palette = "Reds", direction = 1) +
  scale_fill_viridis_c() +
  coord_cartesian(expand = F) +
  labs(x = NULL, y = "Depth (m)", fill = "Count\n(log10)",
       title = "Count of data at depth over time in Kongsfjord") +
  theme(panel.border = element_rect(fill = NA, colour = "black"))

# Average temperature over depth
full_product_kong %>% 
  filter(!is.na(depth),
         grepl("°C", var_name)) %>% 
  mutate(depth = round(depth, -1),
         year = lubridate::year(date)) %>%
  # filter(value > -20, value < 10) %>%
  group_by(depth, year) %>% 
  dplyr::summarise(value = mean(value, na.rm = T),
                   count = n(), .groups = "drop") %>% 
  ggplot(aes(x = year, y = depth)) +
  geom_tile(aes(fill = value, colour = count), size = 0.2) +
  scale_y_reverse() +
  scale_colour_distiller(palette = "Reds", direction = 1) +
  scale_fill_viridis_c() +
  coord_cartesian(expand = F) +
  labs(x = NULL, y = "Depth (m)", fill = "Value",
       title = "Count and average temperature at depth over time in Kongsfjord") +
  theme(panel.border = element_rect(fill = NA, colour = "black"))


# Climatology -------------------------------------------------------------

# Function that produces 12 facet panel of monthly climatologies
# Temperature
# Salinity
# Oxygen
# Ice cover

# Consider spatial interpolation

# Consider depth interpolation

# Could combine spatial and depth interpolations into a #D accessible dataset via a shiny interface


# Range summary -----------------------------------------------------------

# Somehow summarise the ranges in primary drivers...


# Trend summary -----------------------------------------------------------

# Function for creating per pixel trends in primary variables


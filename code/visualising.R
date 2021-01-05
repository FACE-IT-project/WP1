# code/visualising.R
# This script houses code used to visualise raw and processed data


# Setup -------------------------------------------------------------------

library(tidyverse)
library(ggOceanMaps)

map_base <- readRDS("metadata/map_base.Rda")


# TCO2 data ---------------------------------------------------------------

# Load data
Arctic_TCO2 <- read_csv("data/Arctic_TCO2.csv")

# The count per grid cell/station
Arctic_TCO2_count <- Arctic_TCO2 %>% 
  group_by(lon, lat) %>% 
  summarise(count = n(),
            layer = ifelse(max(depth) > 200, "grid", "bottle"), .groups = "drop")

# Using ggOceanMaps
basemap(limits = 60) +
  geom_point(data = transform_coord(Arctic_TCO2_count), aes(x = lon, y = lat))

# using base ggplot2
Arctic_TCO2_count %>%
  # filter(layer == "bottle") %>% 
  filter(layer == "grid") %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(data = map_base, aes(group = group), colour = "black", fill = "grey80") +
  geom_point(aes(colour = count)) +
  scale_colour_distiller(palette = "Greens", direction = 1) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  # coord_quickmap(ylim = c(60, 90), expand = F) +
  coord_map("ortho", orientation = c(90, 0, 0), ylim = c(60, 90)) +
  labs(x = NULL, y = NULL, title = "Count per grid cell from 200 m to 5500 m depth")

# Mean value per grid/station


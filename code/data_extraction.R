# code/data_extraction.R
# Herein the documentation of requested extractions from the FACE-IT dataset


# Setup -------------------------------------------------------------------

source("code/functions.R")

if(!exists("clean_all")) load("data/analyses/clean_all.RData")

# Subset high-res coastline
coastline_full_df_kong <- coastline_full_df %>% 
  filter(between(x, bbox_kong[1]-1, bbox_kong[2]+1),
         between(y, bbox_kong[3]-1, bbox_kong[4]+1))


# Extract -----------------------------------------------------------------

# For Anäis on Jan 3, 2023
## Kongsfjorden
### T°, PAR, Salinity, Chla, pH
#### depth: surface data would already be good but, if possible, data at 7/8m depth would be awesome. 
#### period and frequency: the whole year if possible, but especially spring/summer. Weekly data would be nice.
### how much points along the fjord: the idea of the eDNA project is to characterise the light/salinity gradient that we have along the fjord so 4 to 6 points would be awesome.
extract_1 <- clean_all %>% 
  filter(site == "kong",
         driver %in% c("sea temp", "salinity", "light", "prim prod", "carb"),
         variable %in% c("temp [°C]", "sal", "PAR [µmol m-2 s-1]", "pH [unknown scale]", "pH in situ [total scale]", "Chla [µg/l]"),
         between(depth, 0, 8))
# Unique coords - 7772
length(unique(paste0(extract_1$lon, extract_1$lat)))
# Unique days of sampling - 14705
length(unique(extract_1$date))
# Monthly clims
extract_1 %>% mutate(month = lubridate::month(date)) %>% filter(!is.na(month)) %>% 
  ggplot(aes(x = as.factor(month), y = value)) +
  geom_boxplot(aes(fill = variable), position = "dodge") +
  facet_wrap(~variable, scales = "free_y") + 
  labs(x = "Month", y = NULL, colour = "Variable") +
  theme(panel.border = element_rect(colour = "black", fill = NA))
# Time series
extract_1 %>% 
  ggplot(aes(x = date, y = value, colour = variable)) +
  geom_point() +
  facet_wrap(~variable, scales = "free_y") +
  labs(y = NULL, x = NULL, colour = "Variable") +
  theme(panel.border = element_rect(colour = "black", fill = NA))
# Depth clims
extract_1 %>% 
  mutate(depth = as.factor(round(depth))) %>% 
  ggplot(aes(x = value, y = depth)) +
  geom_boxplot(aes(fill = variable), position = "dodge") +
  labs(y = NULL, x = "Depth [m]", fill = "Variable") +
  scale_y_discrete(limits = rev) +
  facet_wrap(~variable, scales = "free_x") +
  theme(panel.border = element_rect(colour = "black", fill = NA))
# Spatial coverage
extract_1 %>%
  dplyr::select(variable, lon, lat) %>% 
  distinct() %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(data = coastline_full_df_kong, fill = "grey70", colour = "black",
               aes(x = x, y = y, group = polygon_id)) +
  geom_point(aes(colour = variable)) +
  coord_quickmap(expand = F,
                 xlim = c(bbox_kong[1], bbox_kong[2]), 
                 ylim = c(bbox_kong[3], bbox_kong[4])) +
  labs(x = NULL, y = NULL)
# Coverage by region

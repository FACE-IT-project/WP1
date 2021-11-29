# code/site_analysis.R
# This script contains code used for data analyses for specific sites


# Setup -------------------------------------------------------------------

# Libraries
source("code/functions.R")


# Load data ---------------------------------------------------------------

load("~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")
load("~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.RData")
load("~/pCloudDrive/FACE-IT_data/storfjorden/full_product_stor.RData")
load("~/pCloudDrive/FACE-IT_data/young_sound/full_product_young.RData")
load("~/pCloudDrive/FACE-IT_data/disko_bay/full_product_disko.RData")
load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/full_product_nuup.RData")
load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/full_product_por.RData")


# Isfjorden ---------------------------------------------------------------

is_var <- data.frame(var = unique(full_product_is$var_name))
grep("ice", unique(full_product_is$var_name), ignore.case = F)

"Ice extent"
"Snow density, unc [kg/m**3]"
"Snow depth, unc [m]"
"Snow line [m a.s.l.]"

is_ice_extent <- full_product_is %>% 
  filter(var_name == "Snow line [m a.s.l.]")

is_temp <- full_product_is %>% 
  filter(grepl("°C", var_name, ignore.case = F))


# Porsangerfjorden --------------------------------------------------------

# Function for calculating decadal trends
# df <- filter(por_temp_annual, lon == 25.24, lat == 70.35, depth == 0)
dec_trend_calc <- function(df){
  
  # Account for gaps
  df_complete <- df %>%
    # group_by(lon, lat, depth) %>% 
    complete(nesting(lon, lat, depth), year = seq(min(year), max(year)))
  
  # Decadal trends
  if(nrow(df_complete) >= 3){
    dec_trend_temp <- broom::tidy(lm(temp ~ year, df_complete)) %>% 
      slice(2) %>% 
      mutate(dec_trend = round(estimate*10, 3),
             p.value = round(p.value, 4)) %>% 
      dplyr::select(dec_trend, p.value)
  } else {
    dec_trend_temp <- data.frame(dec_trend = NA, p.value = NA)
  }

  # Total means
  sum_stats <- df %>% 
    summarise(temp_average = round(mean(temp, na.rm = T), 2), 
              year_start = min(year, na.rm = T), 
              year_end = max(year, na.rm = T),
              year_count = n(), .groups = "drop")
  
  # Combine and exit
  res <- cbind(dec_trend_temp, sum_stats)
  rm(df, dec_trend_temp, sum_stats); gc()
  return(res)
}

# Get temperature data
# unique(full_product_por$var_name)
por_temp <- full_product_por %>% 
  # filter(grepl("°C", var_name, ignore.case = F)) # Most temperature values are not what we are looking for
  filter(var_name == "temp [°C]") %>% 
  dplyr::rename(temp = value) %>% 
  mutate(lon = round(lon, 1), lat = round(lat, 1), 
         depth = plyr::round_any(depth, 10)) %>% 
  group_by(lon, lat, depth, date) %>% 
  summarise(temp = mean(temp, na.rm = T), .groups = "drop")

# Annual averages
por_temp_annual <- por_temp %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(lon, lat, depth, year) %>% 
  summarise(temp = mean(temp, na.rm = T), .groups = "drop") %>% 
  filter(!is.na(temp),
         year > 1959) # The historic values aren't helpful for decadal trend calculations

# Calculate decadal trends per pixel and depth
system.time(
por_dec_trend <- plyr::ddply(por_temp_annual, c("lon", "lat", "depth"), dec_trend_calc, .parallel = T)
) # 463 seconds on 12 cores
save(por_dec_trend, file = "data/analyses/por_dec_trend.RData")

# Clip coastline polygons for faster plotting
coastline_full_df_por <- coastline_full_df %>% 
  filter(x >= bbox_por[1]-10, x <= bbox_por[2]+10,
         y >= bbox_por[3]-10, y <= bbox_por[4]+10)

# Plot the results
por_dec_trend %>% 
  filter(depth <= 30, !is.na(dec_trend), dec_trend <= 5) %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(data = coastline_full_df_por, aes(x = x, y = y, group = polygon_id), 
               fill = "grey70", colour = "black") +
  # geom_tile(aes(fill = dec_trend)) + # NB: This causes RStudio to hang
  # geom_point(aes(fill = dec_trend), colour = "white", shape = 21, size = 3, stroke = 2) +
  geom_point(data = filter(por_dec_trend, depth <= 50, p.value <= 0.05, dec_trend <= 2),
             aes(fill = dec_trend), colour = "red", shape = 21, size = 3, stroke = 2) +
  scale_fill_viridis_c() +
  # scale_colour_gradient2(low = "red", mid = "white", high = "white", midpoint = 0.1) +
  coord_quickmap(xlim = c(bbox_por[1:2]), 
               ylim = c(bbox_por[3:4])) +
  facet_wrap(~depth) +
  labs(x = NULL, y = NULL, fill = "Temp. trend (°C/dec.)",
       title = "Decadal in situ temperature trends in Porsangerfjord by depth",
       subtitle = "Dots with red borders show significant trends") +
  theme(panel.border = element_rect(fill = NA, colour = "black"), 
        legend.position = "bottom")
ggsave("figures/por_dec_trends.png", height = 10, width = 8)


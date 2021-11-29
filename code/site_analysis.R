# code/site_analysis.R
# This script contains code used for data analyses for specific sites


# Setup -------------------------------------------------------------------

# Libraries
source("code/functions.R")

# Function for calculating decadal trends
## NB: This requires annual data with a 'depth' column. Add an empty one if it's missing.
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

# Plot SST means and trends
## NB: This function expects a gridded daily data.frame with columns: lon, lat, t, temp
plot_sst_grid <- function(df){
  
  # Create annual means
  df_annual <- df %>% 
    mutate(year = lubridate::year(t),
           depth = 1) %>% 
    filter(year <= 2020) %>% # Change once 2021 is complete
    group_by(depth, lon, lat, year) %>% 
    summarise(temp = mean(temp, na.rm = T), .groups = "drop") 
  
  # Get SST stats
  doParallel::registerDoParallel(cores = 7) # Change to taste
  df_dec_trend <- plyr::ddply(df_annual, c("lon", "lat", "depth"), dec_trend_calc, .parallel = T)
  
  # Clip coastline polygons for faster plotting
  coastline_sub <- coastline_full_df %>% 
    filter(x >= min(df$lon)-10, x <= max(df$lon)+10,
           y >= min(df$lat)-10, y <= max(df$lat)+10)
  
  # Create stat labels for plots
  
  # Map of SST mean per pixel SST 
  map_SST_average <- df_dec_trend %>%
    na.omit() %>%
    # filter(p.value <= 0.05) %>% 
    # mutate(temp_total_05 = quantile(temp_total, probs = 0.05),
    #        temp_total_95 = quantile(temp_total, probs = 0.95),
    #        temp_total = case_when(temp_total > temp_total_95 ~ temp_total_95,
    #                               temp_total < temp_total_05 ~ temp_total_05,
    #                               TRUE ~ temp_total)) %>% 
    ggplot() +
    geom_tile(aes(fill = temp_average, x = lon, y = lat)) +
    geom_polygon(data = coastline_sub, aes(x = x, y = y, group = polygon_id), 
                 fill = "grey70", colour = "black") +
    scale_fill_viridis_c() +
    labs(x = NULL, y = NULL, fill = "Temp. (°C)",
         title = "Average annual temperature",
         subtitle = "NOAA OISST: 1982-2020") +
    coord_quickmap(expand = F, xlim = c(min(df$lon), max(df$lon)), ylim = c(min(df$lat), max(df$lat))) +
    theme(legend.position = "bottom", 
          panel.background = element_rect(colour = "black", fill = NULL))
  # map_SST_average
  
  # Map of decadal trend per pixel
  map_SST_trend <- df_dec_trend %>%
    na.omit() %>% 
    filter(p.value <= 0.05) %>% 
    # mutate(dec_trend_05 = quantile(dec_trend, probs = 0.05),
    #        dec_trend_95 = quantile(dec_trend, probs = 0.95),
    #        dec_trend = case_when(dec_trend > dec_trend_95 ~ dec_trend_95,
    #                              dec_trend < dec_trend_05 ~ dec_trend_05,
    #                              TRUE ~ dec_trend)) %>% 
    ggplot() +
    geom_tile(aes(fill = dec_trend, x = lon, y = lat)) +
    geom_polygon(data = coastline_sub, aes(x = x, y = y, group = polygon_id), 
                 fill = "grey70", colour = "black") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
    labs(x = NULL, y = NULL, fill = "Trend (°C/dec)",
         title = "Decadal trend",
         subtitle = "NOAA OISST: 1982-2020") +
    coord_quickmap(expand = F, xlim = c(min(df$lon), max(df$lon)), ylim = c(min(df$lat), max(df$lat))) +
    theme(legend.position = "bottom", 
          panel.background = element_rect(colour = "black", fill = NULL))
  # map_SST_trend
  
  # Arrange and exit
  map_SST <- ggpubr::ggarrange(map_SST_average, map_SST_trend, nrow = 1, labels = c("A)", "B)"))
  rm(df, df_annual, df_dec_trend, coastline_sub, map_SST_average, map_SST_trend); gc()
  return(map_SST)
}

# Model surface temperature projections
## NB: This could easily be modified to include depths and other variables
plot_sst_model <- function(df){
  
  # Get annual averages for surface only
  df_annual <- df %>% 
    mutate(year = lubridate::year(date)) %>% 
    filter(depth == 0, land == 1) %>%
    group_by(proj, depth, lon, lat, year) %>% 
    summarise(temp = mean(Temp, na.rm = T), .groups = "drop") 
  
  # The mean of 2000-2020 across all pathways
  df_annual_2020 <- df_annual %>% 
    filter(year <= 2020) %>% 
    group_by(lon, lat) %>% 
    summarise(temp = mean(temp, na.rm = T), .groups = "drop") 
  
  # Get SST stats
  doParallel::registerDoParallel(cores = 15) # Change to taste
  df_dec_trend <- plyr::ddply(df_annual, c( "proj", "lon", "lat", "depth"), dec_trend_calc, .parallel = T)
  
  # Clip coastline polygons for faster plotting
  coastline_sub <- coastline_full_df %>% 
    filter(x >= min(df$lon)-10, x <= max(df$lon)+10,
           y >= min(df$lat)-10, y <= max(df$lat)+10)
  
  # Average temperatures from 2000-2020
  map_SST_average <- df_annual_2020 %>%
    ggplot() + geom_point(aes(colour = temp, x = lon, y = lat), size = 3) +
    geom_polygon(data = coastline_sub, aes(x = x, y = y, group = polygon_id), 
                 fill = "grey70", colour = "black") +
    scale_colour_viridis_c() +
    labs(x = NULL, y = NULL, colour = "Temp. (°C)",
         title = "Average annual temperature",
         subtitle = "Model: 2000-2020") +
    coord_quickmap(expand = F, xlim = c(min(df$lon), max(df$lon)), 
                   ylim = c(min(df$lat), max(df$lat))) +
    theme(legend.position = "bottom", 
          panel.background = element_rect(colour = "black", fill = NULL))
  # map_SST_average
  
  # Trends from 2000-2099 for three RCPs
  map_SST_trend <- df_dec_trend %>% 
    # filter(p.value <= 0.05) %>% 
    ggplot() + geom_point(aes(colour = dec_trend, x = lon, y = lat), size = 3) +
    geom_polygon(data = coastline_sub, aes(x = x, y = y, group = polygon_id), 
                 fill = "grey70", colour = "black") +
    scale_colour_gradient2(low = "blue", mid = "white", high = "red") +
    labs(x = NULL, y = NULL, colour = "Trend (°C/dec)",
         title = "Decadal trend",
         subtitle = "Model: 2000-2099") +
    coord_quickmap(expand = F, xlim = c(min(df$lon), max(df$lon)), ylim = c(min(df$lat), max(df$lat))) +
    facet_wrap(~proj, ncol = 1) +
    theme(legend.position = "bottom", 
          panel.background = element_rect(colour = "black", fill = NULL))
  # map_SST_trend
  
  # Arrange and exit
  map_SST <- ggpubr::ggarrange(map_SST_average, map_SST_trend, nrow = 1, labels = c("A)", "B)"), widths = c(1, 0.6))
  rm(df, df_annual, df_annual_2020, df_dec_trend, coastline_sub, map_SST_average, map_SST_trend); gc()
  return(map_SST)
}


# Load data ---------------------------------------------------------------

# FACE-IT collected data
load("~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")
load("~/pCloudDrive/FACE-IT_data/isfjorden/full_product_is.RData")
load("~/pCloudDrive/FACE-IT_data/storfjorden/full_product_stor.RData")
load("~/pCloudDrive/FACE-IT_data/young_sound/full_product_young.RData")
load("~/pCloudDrive/FACE-IT_data/disko_bay/full_product_disko.RData")
load("~/pCloudDrive/FACE-IT_data/nuup_kangerlua/full_product_nuup.RData")
load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/full_product_por.RData")

# Model data
model_por <- load_model("porsangerfjorden_rcp")

# NOAA OISST extractions
load("~/pCloudDrive/FACE-IT_data/porsangerfjorden/sst_por.RData")
load("data/sst_trom.RData")


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
# system.time(
# por_dec_trend <- plyr::ddply(por_temp_annual, c("lon", "lat", "depth"), dec_trend_calc, .parallel = T)
# ) # 463 seconds on 12 cores
# save(por_dec_trend, file = "data/analyses/por_dec_trend.RData")
load("data/analyses/por_dec_trend.RData")

# Clip coastline polygons for faster plotting
coastline_full_df_por <- coastline_full_df %>% 
  filter(x >= bbox_por[1]-10, x <= bbox_por[2]+10,
         y >= bbox_por[3]-10, y <= bbox_por[4]+10)

# Plot the results
por_insitu_trend <- por_dec_trend %>% 
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
ggsave("figures/por_dec_trends.png", por_insitu_trend, height = 10, width = 8)

# Plot SST grid around Porsangerfjorden
sst_grid_por <- plot_sst_grid(sst_por)
ggsave("figures/sst_grid_por.png", sst_grid_por, width = 8, height = 6.2)

# Model temperature projections
sst_model_por <- plot_sst_model(model_por)
ggsave("figures/sst_model_por.png", sst_model_por, width = 7.5, height = 9)


# Tromsø ------------------------------------------------------------------

# bbox
bbox_trom <- c(17.6, 20.9, 69.2, 70.3)

# Plot SST grid around Tromso
sst_grid_trom <- plot_sst_grid(sst_trom)
ggsave("figures/sst_grid_trom.png", sst_grid_trom, width = 10, height = 4.6)


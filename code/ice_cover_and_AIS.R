# code# code/ice_cover_and_AIS.R
# This script contains the code used to prep and process ice cover and ship AIS (presence) data
# Currently this is only done for Isfjorden, but could be expanded in the future


# Setup -------------------------------------------------------------------

# Libraries used
library(tidyverse)
library(doParallel); registerDoParallel(cores = 12)

# Ice data
load("~/pCloudDrive/FACE-IT_data/isfjorden/ice_4km_is.RData")
ice_4km_is <- ice_4km_is %>% 
  filter(date >= as.Date("2006-01-01"), date <= as.Date("2021-12-31"))
load("~/pCloudDrive/FACE-IT_data/isfjorden/ice_1km_is.RData")
ice_1km_is <- ice_1km_is %>% 
  filter(date >= as.Date("2015-01-01"), date <= as.Date("2021-12-31"))

# Ice cover colours
ice_cover_colours <- c(
  "ocean" = "navy",
  "land" = "slategrey",
  "sea ice" = "mintcream",
  "coast" = "dodgerblue"
)

# Visualise raw data
ice_4km_is %>% 
  mutate(sea_ice_extent = factor(sea_ice_extent, labels = c("ocean", "land", "sea ice", "coast"))) %>% 
  filter(date == "2017-08-01") %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_point(aes(colour = sea_ice_extent), size = 5, shape = 15) +
  scale_colour_manual("Colours", values = ice_cover_colours)

# Ice prop function
ice_cover_prop <- function(ice_df){
  
  # Get open water pixel count
  water_pixels <- ice_df %>% filter(date == "2017-08-01", sea_ice_extent == 1) %>% nrow()
  
  # Find proportion per month per year that has ice
  ice_prop <- ice_df %>% 
    filter(sea_ice_extent == 3) %>% 
    group_by(date) %>% 
    summarise(count = n(),
              prop = count/water_pixels, .groups = "drop") %>% 
    complete(date = seq.Date(min(date), max(date), by = "day")) %>% 
    replace(is.na(.), 0) %>% 
    mutate(date = lubridate::round_date(date, "month"),
           year = lubridate::year(date),
           month = lubridate::month(date)) %>% 
    group_by(year, month, date) %>% 
    summarise(mean_prop = round(mean(prop, na.rm = T), 2), .groups = "drop")
}

# Annual trend in values
## NB: This assumes the dataframe is temporally complete
## It also assumes the column is called 'val'
trend_calc <- function(df){
  
  # Annual trends
  trend_res <- broom::tidy(lm(val ~ year, df)) %>% 
    slice(2) %>% 
    mutate(trend = round(estimate, 3),
           p.value = round(p.value, 4)) %>% 
    dplyr::select(trend, p.value)
  
  # Total means
  sum_stats <- df %>% 
    summarise(mean_val = round(mean(val, na.rm = T), 2),
              sd_val = round(sd(val, na.rm = T), 3), .groups = "drop")
  
  # Combine and exit
  res <- cbind(trend_res, sum_stats)
  rm(df, trend_res, sum_stats); gc()
  return(res)
}

# Ship AIS data
load("~/pCloudDrive/FACE-IT_data/isfjorden/AIS/is_AIS_raw.RData")


# Process ice data --------------------------------------------------------

# Find proportion per month per year that has ice
ice_4km_is_prop <- ice_cover_prop(ice_4km_is)
ice_1km_is_prop <- ice_cover_prop(ice_1km_is)

# Plot the monthly proportions over time
ggplot(data = ice_4km_is_prop, aes(x = date, y = mean_prop, fill = month)) + 
  geom_col() + scale_fill_continuous(type = "viridis")
ggplot(data = ice_4km_is_prop, aes(x = year, y = mean_prop, colour = month)) + 
  geom_point() + geom_smooth(method = "lm", se = F, aes(group = month)) +
  scale_colour_continuous(type = "viridis")
ggplot(data = ice_1km_is_prop, aes(x = date, y = mean_prop, fill = month)) + 
  geom_col() + scale_fill_continuous(type = "viridis")
ggplot(data = ice_1km_is_prop, aes(x = year, y = mean_prop, colour = month)) + 
  geom_point() + geom_smooth(method = "lm", se = F, aes(group = month)) +
  scale_colour_continuous(type = "viridis")

# Compare 4 km and 1 km results
ice_1km_4km_is_prop <- left_join(ice_1km_is_prop, ice_4km_is_prop, by = c("year", "month", "date")) %>% 
  dplyr::rename(mean_prop_1km = mean_prop.x, mean_prop_4km = mean_prop.y) %>% 
  mutate(diff = mean_prop_1km- mean_prop_4km)
mean(ice_1km_4km_is_prop$diff)
range(ice_1km_4km_is_prop$diff)

# Calculate annual monthly trends
ice_4km_is_trend <- plyr::ddply(dplyr::rename(ice_4km_is_prop, val = mean_prop), c("month"), trend_calc, .parallel = T) %>% 
  mutate(resolution = "4 km")
ice_1km_is_trend <- plyr::ddply(dplyr::rename(ice_1km_is_prop, val = mean_prop), c("month"), trend_calc, .parallel = T) %>% 
  mutate(resolution = "1 km")
ice_1km_4km_is_trend <- rbind(ice_1km_is_trend, ice_4km_is_trend)

# Create figure for further use
## boxplot
ice_box <- ggplot(data = ice_4km_is_prop, aes(x = as.factor(month), y = mean_prop, fill = month)) + 
  geom_boxplot(aes(group = month)) + 
  scale_fill_continuous(type = "viridis") +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1),
                     labels = c("0%", "25%", "50%", "75%", "100%"),
                     limits = c(-0.02, 1), expand = c(0, 0)) +
  labs(x = "Month", y = "Ice cover (%)", fill = "Month") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_blank(),
                     axis.ticks.x = element_blank())
ice_box

## Scatterplot
ice_scatter <- ggplot(data = ice_4km_is_prop, aes(x = year, y = mean_prop, colour = month)) + 
  geom_point() + geom_smooth(method = "lm", se = F, aes(group = month)) +
  scale_colour_continuous(type = "viridis", breaks = c(1:12), labels = c(1:12)) +
  # scale_fill_viridis_d() +
  guides(colour = guide_legend(override.aes = c(shape = 12, size = 5), nrow = 1)) +
  # scale_x_continuous() +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1),
                     labels = c("0%", "25%", "50%", "75%", "100%"),
                     limits = c(-0.02, 1), expand = c(0, 0)) + 
  labs(x = "Year", y = NULL, colour = "Month") +
  theme_bw() + theme(legend.position = "bottom")
ice_scatter

## Combine
ice_plot <- ggpubr::ggarrange(ice_box, ice_scatter, ncol = 2, align = "hv", 
                              legend = "bottom", legend.grob = ggpubr::get_legend(ice_scatter))
ice_plot


# Process AIS data --------------------------------------------------------

# Get daily count of unique ships in the fjord

# Get daily count of pixels for unique ships

# Get daily count of time in fjord for unique ships

# Get daily distance of unique ships

# Plot the monthly values for ships in the fjord

# Calculate annual and monthly trends


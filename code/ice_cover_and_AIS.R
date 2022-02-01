# code# code/ice_cover_and_AIS.R
# This script contains the code used to prep and process ice cover and ship AIS (presence) data
# Currently this is only done for Isfjorden, but could be expanded in the future


# Setup -------------------------------------------------------------------

# Libraries used
library(tidyverse)

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

# Calculate annual and monthly trends


# Process AIS data --------------------------------------------------------

# Get daily count of unique ships in the fjord

# Get daily count of pixels for unique ships

# Get daily count of time in fjord for unique ships

# Get daily distance of unique ships

# Plot the monthly values for ships in the fjord

# Calculate annual and monthly trends


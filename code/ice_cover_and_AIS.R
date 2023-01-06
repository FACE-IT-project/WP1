# code# code/ice_cover_and_AIS.R
# This script contains the code used to prep and process ice cover and ship AIS (presence) data
# Currently this is only done for Isfjorden, but could be expanded in the future


# Setup -------------------------------------------------------------------

# Libraries used
library(tidyverse)
library(RCurl) # For web scraping etc.
library(XML) # Same same
library(doParallel); registerDoParallel(cores = 12)

# Isfjorden bbox
bbox_is <- c(12.97, 17.50, 77.95, 78.90)

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

# globalfishingwatch API key
load("metadata/globalfishingwatch_API_key.RData")

# Ship AIS data
## sog: speed over ground,
## cog: course over ground
if(!exists("is_AIS_raw")) load("~/pCloudDrive/FACE-IT_data/isfjorden/AIS/is_AIS_raw.RData")
is_AIS_raw <- is_AIS_raw %>% 
  mutate(year = lubridate::year(date_time_utc),
         month = lubridate::month(date_time_utc)) %>% 
  dplyr::select(-nav_status, -message_nr); gc()

# Convenience wrapper for parsing ship AIS data
# df <- is_AIS_unique[1,]
gfw_query <- function(df){
  
  # Create query
  query <- paste0("curl --location --request GET 'https://gateway.api.globalfishingwatch.org/v2/vessels/search?query=",
                  df$mmsi,
                  "&datasets=public-global-support-vessels:latest,public-global-carrier-vessels:latest,public-global-fishing-vessels:latest&limit=10&offset=0' -H 'Authorization: Bearer ",
                  globalfishingwatch_API_key,"'")
  
  # Get data
  dl_error <- NULL
  suppressWarnings(
    res <- tryCatch(system(query, intern = T), error = function(query) {dl_error <<- "Cannot access database"})
  )
  
  # Extract data from multiple lists as necessary
  if(!is.null(dl_error)){
    res <- data.frame(mmsi = df$mmsi)
  } else {
    res <- jsonlite::fromJSON(res)[["entries"]]
  }
  if(length(res) == 0) res <- data.frame(mmsi = df$mmsi)
  return(res)
  # rm(df, query, res, dl_error)
}
#


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
mean(abs(ice_1km_4km_is_prop$diff))
range(ice_1km_4km_is_prop$diff)

# Calculate annual monthly trends
ice_4km_is_trend <- plyr::ddply(dplyr::rename(ice_4km_is_prop, val = mean_prop), c("month"), trend_calc, .parallel = T) %>% 
  mutate(dataset = "ice 4 km")
ice_1km_is_trend <- plyr::ddply(dplyr::rename(ice_1km_is_prop, val = mean_prop), c("month"), trend_calc, .parallel = T) %>% 
  mutate(dataset = "ice 1 km")
ice_1km_4km_is_trend <- rbind(ice_1km_is_trend, ice_4km_is_trend)

# Create figure for further use
## boxplot
ice_box <- ggplot(data = ice_4km_is_prop, aes(x = as.factor(month), y = mean_prop, fill = month)) + 
  geom_boxplot(aes(group = month)) + 
  scale_fill_continuous(type = "viridis") +
  scale_y_continuous(breaks = c(0.25, 0.50, 0.75),
                     labels = c("25%", "50%", "75%"),
                     limits = c(-0.02, 1.02), expand = c(0, 0)) +
  labs(x = "Month", y = "Ice cover (%)", fill = "Month") +
  theme_bw() + theme(legend.position = "none")
ice_box

## Scatterplot
ice_scatter <- ggplot(data = ice_4km_is_prop, aes(x = year, y = mean_prop, colour = month)) + 
  geom_point() + geom_smooth(method = "lm", se = F, aes(group = month)) +
  scale_colour_continuous(type = "viridis", breaks = c(1:12), labels = c(1:12)) +
  scale_y_continuous(breaks = c(0.25, 0.50, 0.75),
                     labels = c("25%", "50%", "75%"),
                     limits = c(-0.02, 1.02), expand = c(0, 0)) + 
  labs(x = "Year", y = NULL, colour = "Month") +
  theme_bw() + theme(legend.position = "none")
ice_scatter

## Combine
ice_plot <- ggpubr::ggarrange(ice_box, ice_scatter, ncol = 2, align = "hv", labels = c("A)", "B)"))
ice_plot
ggsave("figures/ice_cover_is.png", ice_plot, width = 12, height = 5)


# Process AIS data --------------------------------------------------------

# Create coarser resolution for easier use
is_AIS_coarse <- is_AIS_raw %>% 
  mutate(lon = round(lon, 2),
         lat = round(lat, 2)) %>% 
  group_by(mmsi, lon, lat, year, month) %>% 
  summarise(ship_count = n(), .groups = "drop")
rm(is_AIS_raw) # When working on laptop
gc()

# Unique mmsi values
is_AIS_mmsi <- is_AIS_coarse %>% 
  dplyr::select(mmsi) %>% 
  distinct() %>% 
  filter(mmsi != 0) %>% # These values must be wrong
  mutate(row_idx = 1:n())

# Query AIS database by mmsi to get ship info
# NB: Only run this once. Takes a long time and uses monitored API bandwidth.
# is_gfw_database <- plyr::ddply(is_AIS_mmsi, c("row_idx"), gfw_query)
# save(is_gfw_database, file = "metadata/is_gfw_database.RData")
load("metadata/is_gfw_database.RData")

# Query shipais.uk
test1 <- RCurl::getURL("http://shipais.uk/search.html?q=227802680")
test2 <- readHTMLTable(test1, skip.rows = 1:6)
test3 <- htmlParse(test1, asText = T)
test5 <- XML::htmlParse(RCurl::getURLContent("https://www.marinetraffic.com/en/data/?asset_type=vessels&columns=flag,shipname,photo,recognized_next_port,reported_eta,reported_destination,current_port,imo,mmsi,ship_type,show_on_live_map,time_of_latest_position,lon_of_latest_position,notes&quicksearch|begins|quicksearch=259650000")) # Obscured behind javascript
test6 <- XML::htmlParse(RCurl::getURL("https://www.myshiptracking.com/vessels?side=false&name=259650000")) # Works
test7 <- XML::htmlParse(RCurl::getURL("https://www.vesselfinder.com/vessels?name=227802680")) # Forbidden
test8 <- XML::htmlParse(RCurl::getURL("https://www.vesselfinder.com/vessels/details/9210622")) # Forbidden
test9 <- XML::htmlParse(RCurl::getURL("https://www.vesseltracker.com/en/vessels.html?term=259650000")) # Works
test10 <- XML::htmlParse(RCurl::getURL("https://www.marinevesseltraffic.com/vessels?vessel=259650000&flag=&page=1&sort=none&direction=none")) # Works poorly
# test2 <- data.frame(files = readHTMLTable(OISST_url_get, skip.rows = 1:2)[[1]]$Name)

# Scrape info from myshiptracking.com
url_ship <- "https://www.myshiptracking.com/vessels?side=false&name=259650000"
ship_info <- test6 <- XML::htmlParse(RCurl::getURL(), asText = T)
flat_html <- readLines(con = url_ship)
ship_table <- readHTMLTable(flat_html)$`table-filter`
ship_read <- readLines(url_ship, encoding = "UTF-8")
parsed_ship <- htmlParse(ship_read, encoding = "UTF-8")
# "\t\t\t\t\t\t\t\t\t\t\t\t\t\t<td"

# Combine database queries
is_AIS_database <- bind_rows(is_gfw_database) %>% 
  dplyr::select(mmsi, vesselType) %>% 
  mutate(mmsi = as.numeric(mmsi)) %>% 
  distinct()

# Merge with mmsi database to get ship types
is_AIS_data <- is_AIS_coarse %>% 
  left_join(is_AIS_database, by = "mmsi") %>% 
  mutate(vesselType = case_when(is.na(vesselType) ~ "Unknown", TRUE ~ vesselType))

# Get monthly count of unique ships in the fjord
is_AIS_unique_monthly <- is_AIS_data %>% 
  dplyr::select(year, month, mmsi) %>% 
  distinct() %>% 
  group_by(year, month) %>% 
  summarise(ship_count = n(), .groups = "drop") #%>%
  # NB:Time series is already complete so this is unnecessary
  # mutate(date = as.Date(paste0(year,"-",month,"-01"))) #%>% 
  # complete(date = seq.Date(min(date), max(date), by = "month")) %>% 
  # replace(is.na(.), 0)

# Get monthly count of unique ships in the fjord
is_AIS_vessel_monthly <- is_AIS_data %>% 
  dplyr::select(year, month, vesselType) %>% 
  distinct() %>% 
  group_by(year, month) %>% 
  summarise(type_count = n(), .groups = "drop")

# Get monthly count of positions recorded for unique ships
is_AIS_unique_position <- is_AIS_data %>% 
  dplyr::select(year, month, lon, lat, mmsi) %>% 
  distinct() %>% 
  group_by(year, month) %>% 
  summarise(position_count = n(), .groups = "drop")

# Get monthly count of positions recorded for unique ships
is_AIS_vessel_position <- is_AIS_data %>% 
  dplyr::select(year, month, lon, lat, vesselType) %>% 
  distinct() %>% 
  group_by(year, month) %>% 
  summarise(vessel_position_count = n(), .groups = "drop")

# Get daily count of time in fjord for unique ships

# Get daily distance of unique ships

# Get trends in ship count
# NB: Don't run on laptop...
is_AIS_unique_trend <- plyr::ddply(dplyr::rename(is_AIS_unique_monthly, val = ship_count), c("month"), trend_calc, .parallel = T) %>% 
  mutate(dataset = "ship AIS")
ice_AIS_trend <- rbind(ice_4km_is_trend, is_AIS_unique_trend)
write_csv(ice_AIS_trend, "data/analyses/is_ice_AIS_trend.csv")

# Create ship count figure
## boxplot
ship_box <- ggplot(data = is_AIS_unique_monthly, aes(x = as.factor(month), y = ship_count, fill = month)) + 
  geom_boxplot(aes(group = month)) + 
  scale_fill_continuous(type = "viridis") +
  scale_y_continuous(limits = c(-10, 310), breaks = c(100, 200), expand = c(0, 0)) +
  labs(x = "Month", y = "Unique ship count", fill = "Month") +
  theme_bw() + theme(legend.position = "none")
ship_box

## Scatterplot
ship_scatter <- ggplot(data = is_AIS_unique_monthly, aes(x = year, y = ship_count, colour = month)) + 
  geom_point() + geom_smooth(method = "lm", se = F, aes(group = month)) +
  scale_colour_continuous(type = "viridis", breaks = c(1:12), labels = c(1:12)) +
  scale_x_continuous(breaks = c(2012, 2015, 2018)) +
  scale_y_continuous(limits = c(-10, 310), breaks = c(100, 200), expand = c(0, 0)) +
  labs(x = "Year", y = NULL, colour = "Month") +
  theme_bw() + theme(legend.position = "none")
ship_scatter

## Combine
ship_plot <- ggpubr::ggarrange(ship_box, ship_scatter, ncol = 2, align = "hv", labels = c("A)", "B)"))
ship_plot
ggsave("figures/ship_count_is.png", ship_plot, width = 12, height = 5)

# Create ship map figure
is_AIS_vessel_year_sum <- is_AIS_data %>% 
  group_by(vesselType, year, lon, lat, ship_count) %>% 
  summarise(vessel_type_sum = sum(ship_count), .groups = "drop")
map_vessel_year <- ggplot(data = is_AIS_vessel_year_sum, aes(x = lon, y = lat)) +
  borders() + geom_tile(aes(fill = log10(vessel_type_sum))) +
  coord_quickmap(xlim = bbox_is[1:2], ylim = bbox_is[3:4]) +
  facet_grid(year~vesselType) +
  theme(legend.position = "bottom")
map_vessel_year
ggsave("figures/vessel_sum_map_is.png", map_vessel_year, width = 7, height = 18)


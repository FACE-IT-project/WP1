# code# code/ice_cover_and_AIS.R
# This script contains the code used to prep and process ice cover and ship AIS (presence) data
# Currently this is only done for Isfjorden, but could be expanded in the future


# Setup -------------------------------------------------------------------

# Libraries used
library(tidyverse)
library(arrow)
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
# df <- is_AIS_mmsi[1,]
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

# Query www.myshiptracking.com database
# df <- is_AIS_mmsi[235,]
# df <- data.frame(mmsi = 123456789)
# df <- data.frame(mmsi = 248368000)
mst_query <- function(df){
  
  # Create query
  query <- paste0("https://www.myshiptracking.com/vessels?side=false&name=",df$mmsi)
  
  # Get data
  dl_error <- NULL
  suppressWarnings(
    res <- tryCatch(readHTMLTable(readLines(con = query))$`table-filter`, 
                    error = function(query) {dl_error <<- "Cannot access database"})
  )
  
  # Extract data from multiple lists as necessary
  if(!is.null(dl_error)){
    res <- data.frame(MMSI = df$mmsi, Type = "Can't access")
  } else {
    if(is.null(res)){
      res <- data.frame(MMSI = df$mmsi, Type = "No data")
    } else {
      res <- res[1:7]
    }
  }
  return(res)
  # rm(df, query, res, dl_error)
}

# Another query for www.myshiptracking.com database
# df <- sval_ship_unique[235,]
# df <- data.frame(mmsi = 123456789)
# df <- data.frame(mmsi = 248368000)
mst_info_query <- function(df){
  
  # Create query
  query <- paste0("https://www.myshiptracking.com/vessels/",df$mmsi)
  
  # Get data
  dl_error <- NULL
  suppressWarnings(
    res <- tryCatch(readHTMLTable(readLines(con = query))[[1]], 
                    error = function(query) {dl_error <<- "Cannot access database"})
  )
  
  # Extract data from multiple lists as necessary
  if(!is.null(dl_error)){
    res_wide <- data.frame(MMSI = df$mmsi, Type = "Can't access")
  } else {
    if(is.null(res)){
      res_wide <- data.frame(MMSI = df$mmsi, Type = "No data")
    } else {
      res_wide <- pivot_wider(res, names_from = V1, values_from = V2) |> 
        cbind(df)
    }
  }
  return(res_wide)
  # rm(df, query, res, res_wide, dl_error)
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


# Process Is AIS data -----------------------------------------------------

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

# Scrape info from myshiptracking.com
# NB: Only run this once.
# is_mst_database <- plyr::ddply(is_AIS_mmsi, c("row_idx"), mst_query)
# save(is_mst_database, file = "metadata/is_mst_database.RData")
load("metadata/is_mst_database.RData")

# Combine database queries
is_AIS_database <- left_join(is_mst_database, is_gfw_database, by  = c("MMSI" = "mmsi")) %>% 
  mutate(Type = case_when(Type == "No data" & !is.na(vesselType) ~ vesselType, TRUE ~ Type),
         Vessel = case_when(is.na(Vessel) & !is.na(shipname) ~ shipname, TRUE ~ Vessel)) %>% 
  dplyr::select(MMSI, Vessel, Type) %>% 
  mutate(MMSI = as.numeric(MMSI)) %>% 
  distinct() %>% 
                                # Unknown or otherwise missing vessel types
  mutate(Type_group = case_when(Type %in% c("No data", "Other Type", "Not available", "Reserved", 
                                            "---", "Reserved for future use") ~ "Unknown",
                                # Passenger vessels
                                Type %in% c("Passengers Ship", "Passenger", "High speed craft",
                                            "Ro-Ro/Passenger Ship", "Passenger/Cargo Ship") ~ "Passenger",
                                # Pleasure vessels
                                Type %in% c("Sailing", "Sailing Vessel") ~ "Pleasure",#  "Sailing",
                                Type %in% c("Pleasure Craft", "Yacht", "Wing in ground",
                                            "Wing in ground B") ~ "Pleasure",
                                # Fishing vessels
                                Type %in% c("Fishing", "Trawler", "Factory Trawler", "Fishing Vessel",
                                            "Fish Carrier", "Sealer", "Fish Factory") ~ "Fishing",
                                # Cargo vessels
                                Type %in% c("General Cargo", "Bulk Carrier", "Cargo",
                                            "Container Ship", "Reefer", "Ore Carrier",
                                            "Vehicles Carrier", "Wood Chips Carrier", "Bulker",
                                            "Cargo/Container Ship", "Cement Carrier", "Chemical Tanker",
                                            "Ro-Ro Cargo") ~ "Cargo",
                                # Fossil fuel activities
                                Type %in% c("Oil/Chemical Tanker", "Tanker", "Oil Products Tanker", 
                                            "Crude Oil Tanker", "Lng Tanker", "Lpg Tanker", 
                                            "Tanker B", "Tanker D") ~ "Cargo", #"Fossil",
                                # Research activities
                                Type %in% c("Research/Survey Vessel", "Fishery Research Vessel",
                                            "Diving ops", "Diving Support Vessel") ~ "Gov", #"Research",
                                # Port related
                                Type %in% c("Port Tender", "Tug", "Anchor Handling Vessel",
                                            "Pilot Vessel", "Spare - Local Vessel",
                                            "Standby Safety Vessel", "Anti-pollution equipment",
                                            "Towing") ~ "Gov", #"Port",
                                # Other governmental/law enforcement etc.
                                Type %in% c("Search and Rescue vessel", "Military ops",
                                            "Patrol Vessel", "Icebreaker", "Buoy-laying Vessel",
                                            "Cable Layer", "Dredging or underwater ops", "Crane Ship",
                                            "Grab Hopper Dredger", "Landing Craft", "Naval Patrol Vessel",
                                            "Offshore Supply Ship", "Salvage/Rescue Vessel", 
                                            "Search and Rescue Aircraft", "Training Ship",
                                            "Waste Disposal Vessel", "Law Enforcement") ~ "Gov",
                                TRUE ~ as.character(NA)))

# Table of grouped ship types
table_type_group <- is_AIS_database %>%
  group_by(Type_group, Type) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  arrange(Type_group, -count)
write_csv(table_type_group, "metadata/table_ship_type_group.csv")

# Merge with mmsi database to get ship types
is_AIS_data <- is_AIS_coarse %>% 
  left_join(is_AIS_database, by = c("mmsi" = "MMSI")) %>% 
  mutate(Type_group = case_when(is.na(Type_group) ~ "Unknown", TRUE ~ Type_group)) %>% 
  filter(between(lon, bbox_is[1], bbox_is[2]), between(lat, bbox_is[3], bbox_is[4]))

# Get monthly count of unique ships in the fjord
is_AIS_unique_monthly <- is_AIS_data %>% 
  dplyr::select(year, month, mmsi) %>% 
  distinct() %>% 
  group_by(year, month) %>% 
  summarise(unique_count_monthly = n(), .groups = "drop") #%>%
  # NB:Time series is already complete so this is unnecessary
  # mutate(date = as.Date(paste0(year,"-",month,"-01"))) #%>% 
  # complete(date = seq.Date(min(date), max(date), by = "month")) %>% 
  # replace(is.na(.), 0)

# Get monthly count of unique ships per type in the fjord
is_AIS_type_monthly <- is_AIS_data %>% 
  dplyr::select(year, month, Type_group, mmsi) %>% 
  distinct() %>% 
  group_by(year, month, Type_group) %>% 
  summarise(type_group_count_monthly = n(), .groups = "drop")

# Get monthly count of positions recorded for unique ships
is_AIS_unique_position <- is_AIS_data %>% 
  dplyr::select(year, month, lon, lat, mmsi) %>% 
  distinct() %>% 
  group_by(year, month) %>% 
  summarise(unique_position_count = n(), .groups = "drop")

# Get monthly count of positions recorded for unique ships by type
is_AIS_type_position <- is_AIS_data %>% 
  dplyr::select(year, month, lon, lat, Type_group, mmsi) %>% 
  distinct() %>% 
  group_by(year, month, Type_group) %>% 
  summarise(type_group_position_count = n(), .groups = "drop")

# Get daily count of time in fjord for unique ships

# Get daily distance of unique ships

# Get trends in ship count
# NB: Don't run on laptop...
# is_AIS_unique_trend <- plyr::ddply(dplyr::rename(is_AIS_unique_monthly, val = ship_count), c("month"), trend_calc, .parallel = T) %>% 
#   mutate(dataset = "ship AIS")
# ice_AIS_trend <- rbind(ice_4km_is_trend, is_AIS_unique_trend)
# write_csv(ice_AIS_trend, "data/analyses/is_ice_AIS_trend.csv")

# Create unique ship count figure
## boxplot
ship_unique_box <- ggplot(data = is_AIS_unique_monthly, 
                          aes(x = as.factor(month), y = unique_count_monthly, fill = month)) + 
  geom_boxplot(aes(group = month)) + 
  scale_fill_continuous(type = "viridis") +
  # scale_y_continuous(limits = c(-10, 310), breaks = c(100, 200), expand = c(0, 0)) +
  labs(x = "Month", y = "Unique ship count", fill = "Month",
       title = "Ship count by MMSI in Isfjorden per month from 2011 - 2019",
       subtitle = "Range in ship counts per month") +
  theme_bw() + theme(legend.position = "none")
ship_unique_box

## Scatterplot
ship_unique_scatter <- ggplot(data = is_AIS_unique_monthly, 
                              aes(x = year, y = unique_count_monthly, colour = month)) + 
  geom_point() + geom_smooth(method = "lm", se = F, aes(group = month)) +
  scale_colour_continuous(type = "viridis", breaks = c(1:12), labels = c(1:12)) +
  scale_x_continuous(breaks = c(2012, 2015, 2018)) +
  # scale_y_continuous(limits = c(-10, 310), breaks = c(100, 200), expand = c(0, 0)) +
  labs(x = "Year", y = NULL, colour = "Month", 
       subtitle = "Trend in ship counts per month") +
  theme_bw() + theme(legend.position = "none")
ship_unique_scatter

## Combine
ship_unique_plot <- ggpubr::ggarrange(ship_unique_box, ship_unique_scatter, ncol = 2, align = "hv")#, labels = c("A)", "B)"))
ship_unique_plot
ggsave("figures/ship_count_unique_is.png", ship_unique_plot, width = 12, height = 5)

# Create ship type count figure
## boxplot
ship_type_box <- ggplot(data = is_AIS_type_monthly,
                        aes(x = as.factor(month), y = type_group_count_monthly, fill = month)) + 
  geom_boxplot(aes(group = month)) + 
  scale_fill_continuous(type = "viridis") +
  # scale_y_continuous(limits = c(-10, 310), breaks = c(100, 200), expand = c(0, 0)) +
  facet_wrap(~Type_group, ncol = 1) +
  labs(x = "Month", y = "Unique ship count", fill = "Month",
       title = "Ship count by type in Isfjorden per month from 2011 - 2019",
       subtitle = "Range in ship counts per month") +
  theme_bw() + theme(legend.position = "none")
ship_type_box

## Scatterplot
ship_type_scatter <- ggplot(data = is_AIS_type_monthly, 
                            aes(x = year, y = type_group_count_monthly, colour = month)) + 
  geom_point() + geom_smooth(method = "lm", se = F, aes(group = month)) +
  scale_colour_continuous(type = "viridis", breaks = c(1:12), labels = c(1:12)) +
  scale_x_continuous(breaks = c(2012, 2015, 2018)) +
  # scale_y_continuous(limits = c(-10, 310), breaks = c(100, 200), expand = c(0, 0)) +
  facet_wrap(~Type_group, ncol = 1) +
  labs(x = "Year", y = NULL, colour = "Month", 
       subtitle = "Trend in ship counts per month") +
  theme_bw() + theme(legend.position = "none")
ship_type_scatter

## Combine
ship_type_plot <- ggpubr::ggarrange(ship_type_box, ship_type_scatter, ncol = 2, align = "hv")#, labels = c("A)", "B)"))
ship_type_plot
ggsave("figures/ship_count_type_is.png", ship_type_plot, width = 16, height = 10)

# Create ship map figure
is_AIS_type_year_sum <- is_AIS_data %>% 
  group_by(Type_group, year, lon, lat, ship_count) %>% 
  summarise(type_sum = sum(ship_count), .groups = "drop")
map_vessel_year <- ggplot(data = is_AIS_type_year_sum, aes(x = lon, y = lat)) +
  borders() + geom_tile(aes(fill = log10(type_sum))) +
  scale_fill_viridis_c(option = "A") +
  coord_quickmap(xlim = bbox_is[1:2], ylim = bbox_is[3:4]) +
  labs(x = NULL, y = NULL, fill = "Annual count [log10(n)]",
       title = "Heatmap of recorded ship type positions by ~0.01° from 2011 - 2019",
       subtitle = "NB: Values shown are log10 transformed") +
  facet_grid(Type_group ~ year) +
  theme(legend.position = "bottom")
map_vessel_year
ggsave("figures/ship_sum_type_map_is.png", map_vessel_year, width = 18, height = 12)


# Process Sval AIS data ---------------------------------------------------

# Function for loading Sval AIS files
load_sval_AIS <- function(file_name){
  suppressMessages(
    df <- read_delim_arrow(file_name, delim = ";") |> 
      dplyr::select(mmsi, imo_num_ais, name_ais, type, shiptype, length, draught, date_time_utc,
                    lon, lat, dist_prevpoint, sec_prevpoint, sog, cog, true_heading) |> 
      mutate(mmsi = as.numeric(mmsi))
  )
  # if(nrow(df) > 0){
  #   df <- df |>
  #     mutate(dist_prevpoint = case_when(dist_prevpoint == -99 ~ 0, TRUE ~ dist_prevpoint),
  #            sec_prevpoint = case_when(sec_prevpoint == -99 ~ 0, TRUE ~ sec_prevpoint))
  # }
  return(df)
  # rm(df, file_name); gc()
}

# Find .csv files
sval_AIS_dir <- dir("~/pCloudDrive/FACE-IT_data/svalbard/AIS", full.names = T, pattern = ".csv")
sval_AIS_info <- as.data.frame(file.info(sval_AIS_dir)) |> dplyr::filter(size > 0)
sval_AIS_files <- row.names(sval_AIS_info)

# Load data
system.time(
  sval_AIS_1 <- map_dfr(sval_AIS_files[1:999], load_sval_AIS)
) # 768 seconds for 999 files
save(sval_AIS_1, file = "~/pCloudDrive/FACE-IT_data/svalbard/AIS/sval_AIS_1.RData"); rm(sval_AIS_1); gc()
system.time(
  sval_AIS_2 <- map_dfr(sval_AIS_files[1000:1999], load_sval_AIS)
) # 993 seconds for 999 files
save(sval_AIS_2, file = "~/pCloudDrive/FACE-IT_data/svalbard/AIS/sval_AIS_2.RData"); rm(sval_AIS_2); gc()
system.time(
  sval_AIS_3 <- map_dfr(sval_AIS_files[2000:2999], load_sval_AIS)
) # 2582 seconds for 999 files
save(sval_AIS_3, file = "~/pCloudDrive/FACE-IT_data/svalbard/AIS/sval_AIS_3.RData"); rm(sval_AIS_3); gc()
system.time(
  sval_AIS_4 <- map_dfr(sval_AIS_files[3000:4075], load_sval_AIS)
) # 2361 seconds for 1075 files
save(sval_AIS_4, file = "~/pCloudDrive/FACE-IT_data/svalbard/AIS/sval_AIS_4.RData"); rm(sval_AIS_4); gc()

# Load all and combine
load("~/pCloudDrive/FACE-IT_data/svalbard/AIS/sval_AIS_1.RData")
load("~/pCloudDrive/FACE-IT_data/svalbard/AIS/sval_AIS_2.RData")
load("~/pCloudDrive/FACE-IT_data/svalbard/AIS/sval_AIS_3.RData")
load("~/pCloudDrive/FACE-IT_data/svalbard/AIS/sval_AIS_4.RData")
sval_AIS <- rbind(sval_AIS_1, sval_AIS_2, sval_AIS_3, sval_AIS_4)
rm(sval_AIS_1, sval_AIS_2, sval_AIS_3, sval_AIS_4); gc()
system.time(save(sval_AIS, file = "~/pCloudDrive/FACE-IT_data/svalbard/AIS/sval_AIS.RData")) # 140 seconds, 678.1 MB
system.time(load("~/pCloudDrive/FACE-IT_data/svalbard/AIS/sval_AIS.RData")) # 27 seconds
system.time(write_csv_arrow(sval_AIS, "~/pCloudDrive/FACE-IT_data/svalbard/AIS/sval_AIS.csv")) # 140 seconds, 6.1 GB
system.time(sval_AIS <- read_csv_arrow("~/pCloudDrive/FACE-IT_data/svalbard/AIS/sval_AIS.csv")) # xxx seconds


# Unique MMSI -------------------------------------------------------------

# Load data
# system.time(sval_AIS <- data.table::fread("~/pCloudDrive/FACE-IT_data/svalbard/AIS/sval_AIS.csv")) # 157 seconds

# Get unique values
# sval_ship_unique <- sval_AIS |>
#   dplyr::select(mmsi:draught) |> distinct()
# data.table::fwrite(sval_ship_unique, file = "~/pCloudDrive/FACE-IT_data/svalbard/AIS/ship_unique.csv")
sval_ship_unique <- read_csv("~/pCloudDrive/FACE-IT_data/svalbard/AIS/ship_unique.csv")

# Get just unique AIS values
# sval_AIS_unique <- unique(sval_AIS$mmsi)
# data.table::fwrite(x = data.frame(mmsi = sval_AIS_unique), file = "~/pCloudDrive/FACE-IT_data/svalbard/AIS/mmsi_unique.csv")
sval_AIS_unique <- read_csv("~/pCloudDrive/FACE-IT_data/svalbard/AIS/mmsi_unique.csv")

# Load grouped ship types
table_type_group <- read_csv("metadata/table_ship_type_group.csv")

# Get unique cruise ship MMSI
unique(sval_AIS$shiptype)
sval_AIS_cruise <- sval_AIS |> 
  filter(shiptype %in% c("Passenger/Cruise", "Passenger", "Passengers Ship", "Passenger/Cargo Ship")) |> 
  dplyr::select(mmsi, imo_num_ais, shiptype, name_ais) |> 
  distinct()
data.table::fwrite(sval_AIS_cruise, file = "~/pCloudDrive/FACE-IT_data/svalbard/AIS/mmsi_cruise.csv")

# Scrape more info from myshiptracking.com
# NB: Only run this once.
sval_mst_info <- plyr::ddply(sval_ship_unique, c("mmsi"), mst_info_query, .parallel = T)
save(sval_mst_info, file = "metadata/sval_mst_info.RData")
load("metadata/sval_mst_info.RData")

# Clean up a bit to send to Morten Simonsen
sval_info_clean <- sval_mst_info |> 
  left_join(table_type_group[,1:2], by = c("shiptype" = "Type")) |> 
  dplyr::select(MMSI, IMO, imo_num_ais, shiptype, Type_group, `Call Sign`, 
                name_ais, Flag, Size, length, draught, GT, DWT) |> 
  dplyr::rename(IMO_AIS = imo_num_ais, Type = shiptype, Call_sign = `Call Sign`, 
                Name = name_ais, Length = length, Draught = draught) |> 
  filter(!is.na(Call_sign)) |> 
  mutate(GT  = as.numeric(str_remove(str_remove(GT, " Tons"), ",")),
         DWT  = as.numeric(str_remove(str_remove(DWT, " Tons"), ",")))
write_csv(sval_info_clean, file = "~/pCloudDrive/FACE-IT_data/svalbard/AIS/sval_info_clean.csv")


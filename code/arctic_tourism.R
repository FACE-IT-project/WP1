# code/arctic_tourism.R
# Analyses of ship AIS in combination with the sort of biology interesting to tourists


# Setup -------------------------------------------------------------------

# Libraries used
source("code/functions.R")


# Data --------------------------------------------------------------------

# Hi-res map data
if(!exists("coastline_full_df")) load("metadata/coastline_full_df.RData")

# Cut to Svalbard
sval_coast_df <- coastline_full_df |> 
  dplyr::rename(lon = x, lat = y) |> 
  filter(lon >= bbox_sval[1], lon <= bbox_sval[2],
         lat >= bbox_sval[3], lat <= bbox_sval[4])

# NB: This file is created from a massive number of daily files from the Norwegian coast guard
# The code for the creation is in 'code/ice_cover_AIS.R' subsection 'Process Sval AIS data'
# NB: This dataset is massive, be cautious
system.time(load("~/pCloudDrive/FACE-IT_data/svalbard/AIS/sval_AIS.RData")) # 33 seconds

# Nesting sites
# NB: No coordinates were provided for "Løvenøyane and Breøyane" (two Kongsfjorden nesting sites)
## So I manually took a point between them via Google earth
sval_sbird_sites <- readxl::read_xls("~/pCloudDrive/restricted_data/arctic_tourism/Seabird monitoring data.xls", sheet = "Colony positions")
sval_sbird_count <- readxl::read_xls("~/pCloudDrive/restricted_data/arctic_tourism/Seabird monitoring data.xls", sheet = "Data") |> 
  left_join(sval_sbird_sites, by = "Colony") |> 
  mutate(Unit = case_when(Unit == "Number of individuals" ~ "individuals",
                          Unit == "Apparently Occupied Nests (<=> breeding pairs)" ~ "nests"))

# Ship landings
sval_landings_2025 <- readxl::read_xlsx("~/pCloudDrive/restricted_data/arctic_tourism/Ilandstigningsdata 1996-2023 i gult fra 2025.xlsx", sheet = "2025")
sval_landings <- readxl::read_xlsx("~/pCloudDrive/restricted_data/arctic_tourism/Ilandstigningsdata 1996-2023 i gult fra 2025.xlsx", sheet = "Data") |> 
  pivot_longer(cols = `1996`:`2023`, names_to = "year", values_to = "landings") |> 
  left_join(sval_landings_2025, by = join_by(Stedsnavn, Overordnet_sted, lon, lat)) |> 
  # NB: Replacing missing data with 0 count for landings, this may be incorrect to do
  mutate(landings = case_when(is.na(landings) ~ 0, TRUE ~ landings),
         `2025` = case_when(is.na(`2025`) ~ FALSE, TRUE ~ `2025`)) 
sval_landings_sites <- sval_landings |> 
  dplyr::select(-year, -landings) |> distinct()


# Visualise Sval AIS ------------------------------------------------------

# The cleaned up mmsi list from Morten
sval_info_Morten <- read_csv("~/pCloudDrive/FACE-IT_data/svalbard/AIS/sval_info_Morten.csv")

# Create coarser resolution for easier use
sval_AIS_coarse <- sval_AIS %>% 
  mutate(lon = round(lon, 2),
         lat = round(lat, 2),
         year = lubridate::year(date_time_utc),
         month = lubridate::month(date_time_utc)) %>% 
  group_by(mmsi, length, draught, lon, lat, year, month) %>% 
  summarise(ship_count = n(), .groups = "drop")
# rm(sval_AIS); gc() # When working on laptop

# Filter by Morten's list
sval_AIS_cruise <- right_join(sval_AIS_coarse, sval_info_Morten, by = "mmsi")

# Get monthly count of unique ships
sval_AIS_unique_monthly <- sval_AIS_cruise %>% 
  dplyr::select(lon, lat, year, month, mmsi) %>% 
  distinct() %>% 
  group_by(lon, lat, year, month) %>% 
  summarise(unique_count_monthly = n(), .groups = "drop") #%>%
# NB:Time series is already complete so this is unnecessary
# mutate(date = as.Date(paste0(year,"-",month,"-01"))) #%>% 
# complete(date = seq.Date(min(date), max(date), by = "month")) %>% 
# replace(is.na(.), 0)

# Get monthly count of unique ships per type
sval_AIS_type_monthly <- sval_AIS_cruise %>% 
  dplyr::select(lon, lat, year, month, Type, mmsi) %>% 
  distinct() %>% 
  group_by(lon, lat, year, month, Type) %>% 
  summarise(type_count_monthly = n(), .groups = "drop")

# Get spring season count of unique ships per type per year
sval_AIS_type_spring <- sval_AIS_type_monthly %>% 
  filter(month %in% 3:5) |> 
  group_by(lon, lat, year, Type) %>% 
  summarise(type_count_spring = sum(type_count_monthly), .groups = "drop")

# Get yearly count of unique ships per type
sval_AIS_type_yearly <- sval_AIS_type_monthly %>% 
  group_by(lon, lat, year, Type) %>% 
  summarise(type_count_yearly = sum(type_count_monthly), .groups = "drop")

# Heat maps for Svalbard for the different ship categories per year
map_vessel_year <- ggplot(data = sval_AIS_type_yearly, aes(x = lon, y = lat)) +
  borders() + geom_tile(aes(fill = log10(type_count_yearly))) +
  scale_fill_viridis_c(option = "A") +
  coord_quickmap(xlim = bbox_sval[1:2], ylim = bbox_sval[3:4]) +
  labs(x = NULL, y = NULL, fill = "Annual count\n[log10(n)]",
       title = "Heatmap of unique ship AIS tracks per type by ~0.01° pixels from 2011 - 2022",
       subtitle = "NB: Values shown are log10 transformed and only one unique ship is counted per pixel per month") +
  facet_grid(Type ~ year) +
  theme(legend.position = "bottom")
# map_vessel_year
ggsave("figures/ship_sum_type_map_sval.png", map_vessel_year, width = 20, height = 7.5)

# Heat maps for Svalbard for the spring season (March-May) per year
map_vessel_spring <- ggplot(data = sval_AIS_type_spring, aes(x = lon, y = lat)) +
  borders() + geom_tile(aes(fill = log10(type_count_spring))) +
  scale_fill_viridis_c(option = "A") +
  coord_quickmap(xlim = bbox_sval[1:2], ylim = bbox_sval[3:4]) +
  labs(x = NULL, y = NULL, fill = "Spring count\n[log10(n)]",
       title = "Heatmap of unique ship AIS tracks per type by ~0.01° pixels for spring (March-May) from 2011 - 2022",
       subtitle = "NB: Values shown are log10 transformed and only one unique ship is counted per pixel per month") +
  facet_grid(Type ~ year) +
  theme(legend.position = "bottom")
# map_vessel_spring
ggsave("figures/ship_spring_type_map_sval.png", map_vessel_spring, width = 20, height = 7.5)

# Sval map with 2022 cruise ship traffic, landing sites and sea bird nesting sites
map_vessel_sbird_2022 <- ggplot(data = filter(sval_AIS_type_yearly, year == 2022), aes(x = lon, y = lat)) +
  borders(fill = "grey70") + geom_tile(aes(fill = log10(type_count_yearly))) +
  geom_point(data = filter(sval_landings, year == 2022), colour = "darkred", 
             aes(size = landings, shape = `2025`)) +
  geom_point(data = sval_sbird_sites, colour = "darkblue") +
  ggrepel::geom_label_repel(data = sval_sbird_sites, aes(label = Colony), 
                            max.overlaps = 20, colour = "darkblue", alpha = 0.6) +
  scale_fill_viridis_c(option = "A") +
  coord_quickmap(xlim = c(9, 31), ylim = c(76.5, 81), expand = FALSE) +
  labs(x = NULL, y = NULL, fill = "Annual count\n[log10(n)]",
       title = "Heatmap of 2022 ship tracks with landing sites and sea bird nesting sites",
       subtitle = "NB: Values shown are log10 transformed and only one unique ship is counted per pixel per month") +
  theme(panel.border = element_rect(colour = "black", fill = NA))
# map_vessel_sbird_2022
ggsave("figures/ship_sbird_map_sval.png", map_vessel_sbird_2022, width = 9, height = 8)

# TODO: Sval map with walrus and tourism landing sites
# TODO: Good source of data for this: https://mosj.no/en/indikator/fauna/marine-fauna/

# Calculate trends in sbird ind and nests
sval_sbird_trend <- sval_sbird_count |> 
  group_by(Species, Colony, lon, lat, Unit) |> nest() |> 
  mutate(ts_out = purrr::map(data, ~ts(.x$Number, start = 1988, end = 2023, frequency = 1))) |>
  mutate(lm = purrr::map(data, ~lm(Number ~ year, .x)),
         lm_tidy = purrr::map(lm, ~broom::tidy(.x))) |> 
  dplyr::select(-lm) |> unnest(lm_tidy) |> ungroup() |> filter(term == "year") |>
  dplyr::select(Species, Colony, lon, lat, Unit, term, estimate, p.value) |> 
  dplyr::rename(slope = estimate) |> 
  mutate(slope = round(slope, 2), p.value = round(p.value, 4),
         p.sig = case_when(p.value <= 0.05 ~ TRUE, TRUE ~ FALSE))

# Zoom in on nesting sites and show difference by season, e.g. hatching time
# Show the time spent of ships close to nesting sites by counting each point as a 5 minute time point
# These can be summed up to imply tourism presence
# Colour code sites to show increase or decrease in nesting pairs


# t-test or ANOVA to show relationship between ship time and proximity to nesting sites
## Or perhaps a linear model with y = ship time and x = distance from nest
## This may not be a linear fit, depends on the methodology of comparison

# An idea is to show development since 2011 on the sites adjacent to sea bird nesting sites that we have data for.


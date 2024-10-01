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
if(!exists("sval_AIS")) system.time(load("~/pCloudDrive/FACE-IT_data/svalbard/AIS/sval_AIS.RData")) # 33 seconds

# The cleaned up mmsi list from Morten
sval_info_Morten <- read_csv("~/pCloudDrive/FACE-IT_data/svalbard/AIS/sval_info_Morten.csv")

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
         year = as.numeric(year),
         `2025` = case_when(is.na(`2025`) ~ FALSE, TRUE ~ `2025`)) 
sval_landings_sites <- sval_landings |> 
  dplyr::select(-year, -landings) |> distinct()


# Prep AIS ----------------------------------------------------------------

# Create coarser resolution for easier use
sval_AIS_coarse <- sval_AIS |> 
  mutate(lon = round(lon, 2),
         lat = round(lat, 2),
         year = lubridate::year(date_time_utc),
         month = lubridate::month(date_time_utc)) |> 
  summarise(ship_count = n(), .by = c("lon", "lat", "year", "month", "mmsi"))
# rm(sval_AIS); gc() # When working on laptop

# Get the distances travelled per ship
sval_AIS_dist <- sval_AIS |> 
  # NB: Filtering out suspicious values, the 95 percentile - 421
  filter(dist_prevpoint != -99, dist_prevpoint <= 500) |> 
  mutate(year = lubridate::year(date_time_utc),
         month = lubridate::month(date_time_utc)) |> 
  summarise(dist = sum(dist_prevpoint), .by = c("year", "month", "mmsi"))

# Filter by Morten's list
sval_AIS_cruise <- right_join(sval_AIS_coarse, sval_info_Morten, by = "mmsi")
sval_AIS_dist_cruise <- right_join(sval_AIS_dist, sval_info_Morten, by = "mmsi")

# Get monthly count of unique ships
sval_AIS_unique_monthly <- sval_AIS_cruise |> 
  dplyr::select(lon, lat, year, month, mmsi) |>  
  distinct() |> 
  group_by(lon, lat, year, month) |>  
  summarise(unique_count_monthly = n(), .groups = "drop") # |> 
# NB:Time series is already complete so this is unnecessary
# mutate(date = as.Date(paste0(year,"-",month,"-01"))) # |> 
# complete(date = seq.Date(min(date), max(date), by = "month")) |> 
# replace(is.na(.), 0)

# Get monthly count of ships per type
sval_AIS_type_monthly <- sval_AIS_cruise |> 
  summarise(type_count_monthly = sum(ship_count), .by = c("lon", "lat", "year", "month", "Type"))

# Get spring season count of unique ships per type per year
sval_AIS_type_spring <- sval_AIS_type_monthly |> 
  filter(month %in% 3:5) |>
  summarise(type_count_spring = sum(type_count_monthly), .by = c("lon", "lat", "year", "Type"))

# Get yearly count of unique ships per type
sval_AIS_type_yearly <- sval_AIS_cruise |> 
  summarise(type_count_yearly = sum(ship_count), .by = c("lon", "lat", "year", "Type"))

# Get overall yearly counts
sval_AIS_yearly <- sval_AIS_cruise |> 
  summarise(count_yearly = sum(ship_count),
            minutes_per_pixel = sum(ship_count)*5, .by = c("lon", "lat", "year")) |> 
  mutate(hours_per_pixel = as.integer(round(minutes_per_pixel/60)))


# Analyses ----------------------------------------------------------------

# Calculate trends in sbird ind and nests from 2011-2022
sval_sbird_trend <- sval_sbird_count |> 
  # NB: Lacking ship traffic for 2023, so excluding that year of bird data from linear models
  filter(year >= 2011, year <= 2022) |> 
  group_by(Species, Colony, lon, lat, Unit) |> nest() |> 
  mutate(ts_out = purrr::map(data, ~ts(.x$Number, start = 2011, end = 2022, frequency = 1))) |>
  mutate(lm = purrr::map(data, ~lm(Number ~ year, .x)),
         lm_tidy = purrr::map(lm, ~broom::tidy(.x))) |> 
  dplyr::select(-lm) |> unnest(lm_tidy) |> ungroup() |> filter(term == "year") |>
  dplyr::select(Species, Colony, lon, lat, Unit, term, estimate, p.value) |> 
  dplyr::rename(slope = estimate) |> 
  mutate(slope = round(slope, 2), p.value = round(p.value, 4),
         p.sig = case_when(p.value <= 0.05 ~ TRUE, TRUE ~ FALSE))
write_csv(sval_sbird_trend, "~/pCloudDrive/restricted_data/arctic_tourism/sval_sbird_trend.csv")

# Calculate trends in landings from 2011-2022
sval_landings_trend <- sval_landings |> 
  # NB: Lacking ship traffic for 2023, so excluding that year of bird data from linear models
  filter(year >= 2011, year <= 2022) |> 
  group_by(Stedsnavn, Overordnet_sted, lon, lat, `2025`) |> nest() |> 
  mutate(ts_out = purrr::map(data, ~ts(.x$landings, start = 2011, end = 2022, frequency = 1))) |>
  mutate(lm = purrr::map(data, ~lm(landings ~ year, .x)),
         lm_tidy = purrr::map(lm, ~broom::tidy(.x))) |> 
  dplyr::select(-lm) |> unnest(lm_tidy) |> ungroup() |> filter(term == "year") |>
  dplyr::select(Stedsnavn, Overordnet_sted, `2025`, lon, lat, term, estimate, p.value) |> 
  dplyr::rename(slope = estimate) |> 
  mutate(slope = round(slope, 2), p.value = round(p.value, 4),
         p.sig = case_when(p.value <= 0.05 ~ TRUE, TRUE ~ FALSE))
write_csv(sval_landings_trend, "~/pCloudDrive/restricted_data/arctic_tourism/sval_landings_trend.csv")

# Cut landings values for easier plotting
sval_landings <- sval_landings |> 
  mutate(landings_cut = cut(landings, c(-Inf, 0, 10, 100, 500, 1000, 10000, Inf), 
                            labels = c("0", "1-10", "11-100", "101-500", "501-1,000", "1,001-10,000", "10,000+")))


# Visualise Sval AIS ------------------------------------------------------

# Prep count of unique ships per year by type
sval_type_unique_annual <- sval_AIS_cruise |> 
  dplyr::select(year, Type, mmsi) |>  
  distinct() |> 
  summarise(unique_type = n(), .by = c("year", "Type")) |> 
  complete(year = 2011:2022, Type = c("Cruise", "Daytrip", "Expedition"), fill = list(unique_type = 0))
sval_AIS_dist_cruise_prep <- sval_AIS_dist_cruise |> 
  summarise(dist_yearly = sum(dist), .by = c("year", "Type")) |>
  complete(year = 2011:2022, Type = c("Cruise", "Daytrip", "Expedition"), fill = list(dist_yearly = 0)) |> 
  mutate(km_yearly  = dist_yearly/1000,
         Nm_yearly = km_yearly*0.5399568) |> 
  left_join(sval_type_unique_annual, by = c("year", "Type")) |> 
  mutate(Type = factor(Type, levels = c("Cruise", "Daytrip", "Expedition")))

# Calculate growth from 2018 tp 20222
sum(filter(sval_AIS_dist_cruise_prep, year == 2022)$Nm_yearly)/sum(filter(sval_AIS_dist_cruise_prep, year == 2018)$Nm_yearly)
sum(filter(sval_AIS_dist_cruise_prep, year == 2022)$unique_type)/sum(filter(sval_AIS_dist_cruise_prep, year == 2018)$unique_type)

# Barplot of overall ship traffic per year vy type
barplot_ship_type <- sval_AIS_dist_cruise_prep |> 
  ggplot(aes(x = year, y = Nm_yearly)) +
  geom_col(aes(fill = Type), colour = "black", position = "dodge") +
  geom_text(aes(label = unique_type, group = Type), colour = "black", 
            position = position_dodge(width = 1.0), vjust = -0.8, size = 2.5) +
  scale_fill_brewer(palette = "Set1", aesthetics = c("colour", "fill")) +
  scale_x_continuous(breaks = seq(2011, 2022, 2)) +
  scale_y_continuous(breaks = seq(0, 60000, 20000),
                     labels = seq(0, 60000, 20000)/1000,
                     limits = c(0, 80000), expand = c(0, 0)) +
  labs(x = "Year", y = "Distance travelled [1,000 Nm]", fill = "Ship type", 
       title = "Nautical miles travelled around Svalbard per ship type") +
  theme_bw() +
  theme(legend.position = "bottom")
# barplot_ship_type
ggsave("figures/ship_sum_type_barplot.png", barplot_ship_type, width = 6, height = 4, dpi = 600)
ggsave("~/pCloudDrive/restricted_data/arctic_tourism/fig_1.png", barplot_ship_type, width = 6, height = 4, dpi = 600)

# Heat maps for Svalbard for the different ship categories for the years of 2011, 2018, 2022
map_vessel_year <- sval_AIS_type_yearly |> 
  filter(year %in% c(2011, 2018, 2022)) |> 
  ggplot(aes(x = lon, y = lat)) +
  borders(colour = "black", fill = "grey70") + 
  geom_tile(aes(fill = log10(type_count_yearly))) +
  scale_fill_viridis_c(option = "A") +
  coord_quickmap(xlim = range(sval_AIS_type_yearly$lon), 
                 ylim = range(sval_AIS_type_yearly$lat), expand = FALSE) +
  labs(x = "Longitude [°E]", y = "Latitude [°N]", 
       fill = "Annual sum of\nship recordings\n[log10(n)]",
       title = "Heat map of AIS ship track records around Svalbard") +
  facet_grid(Type ~ year) +
  theme_bw() +
  theme(legend.position = "bottom")
# map_vessel_year
ggsave("figures/ship_sum_type_map_sval.png", map_vessel_year, width = 7.5, height = 7.5, dpi = 600)
ggsave("~/pCloudDrive/restricted_data/arctic_tourism/fig_2.png", map_vessel_year, width = 7.5, height = 7.5, dpi = 600)

# Heat maps for Svalbard for the spring season (March-May) per year
map_vessel_spring <- sval_AIS_type_spring |> 
  filter(year %in% c(2011, 2018, 2022)) |> 
  ggplot(aes(x = lon, y = lat)) +
  borders(colour = "black", fill = "grey70") + 
  geom_tile(aes(fill = log10(type_count_spring))) +
  scale_fill_viridis_c(option = "A") +
  coord_quickmap(xlim = range(sval_AIS_type_yearly$lon), 
                 ylim = range(sval_AIS_type_yearly$lat), expand = FALSE) +
  labs(x = "Longitude [°E]", y = "Latitude [°N]", 
       fill = "Spring sum of\nship recordings\n[log10(n)]",
       title = "Heat map of AIS ship track records around Svalbard",
       subtitle = "Sum of vaues only for spring (March-May)") +
  facet_grid(Type ~ year) +
  theme_bw() +
  theme(legend.position = "bottom")
# map_vessel_spring
ggsave("figures/ship_spring_type_map_sval.png", map_vessel_spring, width = 7.5, height = 7.5, dpi = 600)
ggsave("~/pCloudDrive/restricted_data/arctic_tourism/ship_spring_type_map_sval.png", map_vessel_spring, width = 7.5, height = 7.5, dpi = 600)


# Visualise sbird colonies ------------------------------------------------

# Prep landings data
sval_landings_2022 <- sval_landings |> 
  filter(year == 2022, landings > 0) |> 
  droplevels()

# Sval map with 2022 cruise ship traffic, landing sites and sea bird nesting sites
map_vessel_sbird_2022 <- sval_AIS_yearly |> 
  filter(year == 2022) |> 
  ggplot(aes(x = lon, y = lat)) +
  # borders(fill = "grey70") + 
  geom_polygon(data = sval_coast_df, fill = "grey70", colour = "black",
               aes(x = lon, y = lat, group = polygon_id)) +
  geom_tile(aes(fill = log10(count_yearly))) +
  geom_point(data = sval_landings_2022, 
             # size = 2,
             colour = "black", fill = "darkred", shape = 21,
             aes(size = landings_cut)) +
             # aes(colour = landings_cut)) +
  geom_point(data = sval_sbird_sites, colour = "black", fill = "darkblue", shape = 22, size = 5) +
  ggrepel::geom_label_repel(data = sval_sbird_sites, aes(label = Colony), 
                            max.overlaps = 20, colour = "darkblue", alpha = 0.8) +
  scale_fill_viridis_c(option = "A") +
  scale_colour_viridis_d(option = "E") +
  scale_size_discrete(range = c(1, 6)) +
  # coord_quickmap(xlim = c(9, 31), ylim = c(76.5, 81), expand = FALSE) +
  coord_quickmap(xlim = range(sval_AIS_type_yearly$lon), 
                 ylim = range(sval_AIS_type_yearly$lat), expand = FALSE) +
  labs(x = "Longitude [°E]", y = "Latitude [°N]", 
       size = "Annual sum\nof landings\n[n humans]",
       fill = "Annual sum of\nship recordings\n[log10(n)]",
       title = "Heatmap of 2022 ship traffic with landings and sea bird nesting sites") +
  guides(colour = guide_legend(override.aes = list(size = 6))) +
  theme_bw() +
  theme(legend.position = "bottom")
  # theme(panel.border = element_rect(colour = "black", fill = NA))
# map_vessel_sbird_2022
ggsave("figures/ship_sbird_map_sval.png", map_vessel_sbird_2022, width = 9, height = 8, dpi = 600)
ggsave("~/pCloudDrive/restricted_data/arctic_tourism/fig_3.png", map_vessel_sbird_2022, width = 9, height = 8, dpi = 600)

# Generate ship traffic rasters per colony
# NB: 'ship_col_rast_plot()' requires dataframes loaded above in order to work correctly
doParallel::registerDoParallel(cores = 8)
plyr::l_ply(unique(sval_sbird_trend$Colony), ship_col_rast_plot, .parallel = TRUE)

# t-test or ANOVA to show relationship between ship time and proximity to nesting sites
## Or perhaps a linear model with y = ship time and x = distance from nest
## This may not be a linear fit, depends on the methodology of comparison

# An idea is to show development since 2011 on the sites adjacent to sea bird nesting sites that we have data for.


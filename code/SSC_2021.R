# code/SSC_2021.R
# This script contains the code used for the analyses and figures in the SSC 2021 poster

# Setup -------------------------------------------------------------------

# Libraries
source("code/functions.R")
library(lubridate)
library(heatwaveR)

# Quick filtering function
# Manual tweaks will still be required after running this
pg_quick_filter <- function(file_name, bbox){
  pg_dat <- data.table::fread(file_name)
  if("Longitude" %in% colnames(pg_dat)){
    pg_res <- pg_dat %>% 
      dplyr::rename(lon = Longitude, lat = Latitude) %>% 
      filter(lon >= bbox[1], lon <= bbox[2],
             lat >= bbox[3], lat <= bbox[4]) %>% 
      janitor::remove_empty("cols")
  } else{
    pg_res <- NULL
  }
  rm(pg_dat); gc()
  return(pg_res)
}

# Function that adapts the climatology period to the data
ts2clm_adapt <- function(df){
  # Get start/end dates
  min_date <- min(df$t)
  if(min_date < as.Date("1981-01-01")){
    min_date <- as.Date("1981-01-01")
  } else {
    min_date <- as.Date(paste0(year(min_date)+1,"-01-01"))
  }
  max_date <- max(df$t)
  if(year(max_date)-year(min_date) > 30){
    max_date <- as.Date(paste0(year(min_date)+30,"-12-31"))
  } else {
    max_date <- as.Date(paste0(year(max_date)-1,"-12-31"))
  }
  if(year(max_date)-year(min_date) < 3) return(NULL)
  res <- ts2clm(df, climatologyPeriod = c(min_date, max_date)) %>% 
    mutate(clim_start = min_date, clim_end = max_date)
  return(res)
}

# Specify how the correlations should be performed
cor_test <- function(df, col1, col2){
  df_sub <- drop_na(df[c(col1, col2)])
  if(nrow(df_sub) > 2) {
    cor_res <- cor.test(df_sub[[1]], df_sub[[2]])
    res <- data.frame(r = round(cor_res$estimate, 2), p = round(cor_res$p.value, 4), df = cor_res$parameter, n = nrow(df_sub))
  } else {
    res <- data.frame(r = NA, p = NA, df = NA, n = nrow(df_sub))
  }
  return(res)
}


# Data --------------------------------------------------------------------

# Find PG data within Svalbard bbox
# pg_EU_files <- dir("data/pg_data", pattern = "pg_EU", full.names = T)
# system.time(
#   pg_svalbard <- plyr::ldply(pg_EU_files, pg_quick_filter, bbox = bbox_sval)
# ) # 225 seconds
# length(unique(pg_svalbard$citation)) # 1740

# Kongsfjorden data
load("~/pCloudDrive/FACE-IT_data/kongsfjorden/full_product_kong.RData")


# Bounding polygons -------------------------------------------------------

## NB: Areas adapted from Hop et al. 2002
# Note that the high-res coastline is probably from the 80's or 90's
# so it isn't a perfect match to the shape of the present day fjord
# This is particularly pronounced for the inner fjord and Blomstrand island

# Subset high-res coastline
coastline_kong <- coastline_full_df %>% 
  filter(x >= bbox_kong[1], x <= bbox_kong[2],
         y >= bbox_kong[3], y <= bbox_kong[4]) %>% 
  dplyr::select(x, y) %>% 
  dplyr::rename(lon = x, lat = y)

# Manually find points in the dataframe to cut up into polygons
ggplot(coastline_kong, aes(x = lon, y = lat)) + geom_point() +
  geom_point(data = coastline_kong[1,], colour = "red", size = 3)

# Subset fjord regions
kong_inner <- coastline_kong[270,] %>% 
  rbind(data.frame(lon = c(12.36, 12.65, 12.65), lat = c(78.86, 78.86, 79.01958))) %>% 
  rbind(coastline_kong[c(560:570, 536, 420),]) %>% 
  rbind(data.frame(lon = 12.36003, lat = 78.945)) %>% mutate(region = "inner")
kong_trans <- coastline_kong[c(157:270),] %>% 
  rbind(data.frame(lon = 12.36003, lat = 78.945)) %>% 
  rbind(coastline_kong[c(420, 536, 570:589, 500:470),]) %>% mutate(region = "transition")
kong_middle <- coastline_kong[c(76:157, 470:500, 589:666),] %>% mutate(region = "middle")
kong_outer <- coastline_kong[c(76, 666),] %>% rbind(data.frame(lon = 11.178, lat = 79.115)) %>% mutate(region = "outer")
kong_shelf <- coastline_kong[1:76,] %>% 
  rbind(data.frame(lon = c(11.178, 11.178, 11, 11, 11.72653), lat = c(79.115, 79.2, 79.2, 78.85, 78.85))) %>%  mutate(region = "shelf")
kong_regions <- rbind(kong_inner, kong_trans, kong_middle, kong_outer, kong_shelf) %>% 
  mutate(region = factor(region, levels = c("inner", "transition", "middle", "outer", "shelf")))

# Check the created dataframes
ggplot(coastline_kong, aes(x = lon, y = lat)) + geom_point() +
  # geom_point(data = kong_outer, colour = "red")
  geom_polygon(data = kong_regions, aes(group = region, fill = region))


# Analyses ----------------------------------------------------------------

# Find data inside regions
full_region_kong <- plyr::ldply(unique(kong_regions$region), points_in_region, .parallel = F, 
                                bbox_df = kong_regions, data_df = full_product_kong)
full_product_kong_unique <- full_product_kong %>% 
  dplyr::select(lon, lat) %>% distinct()

# Quicker plotting
coastline_kong_expand <- coastline_full_df %>% 
  filter(x >= bbox_kong[1]-1, x <= bbox_kong[2]+1, y >= bbox_kong[3]-1, y <= bbox_kong[4]+1)

# Check coord assignments
ggplot(coastline_kong_expand, aes(x = lon, y = lat)) + 
  geom_polygon(aes(x = x, y = y, group = polygon_id), colour = "grey20") +
  geom_polygon(data = kong_regions, aes(group = region, fill = region), alpha = 0.2) +
  geom_point(data = full_product_kong_unique) +
  geom_point(data = full_region_kong, aes(colour = region)) +
  coord_quickmap(expand = F,
                 xlim = c(bbox_kong[1]-0.3, bbox_kong[2]+0.3), 
                 ylim = c(bbox_kong[3]-0.05, bbox_kong[4]+0.05))

# Group data according to bounding boxes
full_product_kong_regions <- full_product_kong %>% 
  left_join(full_region_kong, by = c("lon", "lat")) %>% 
  filter(!is.na(region))
unique(full_product_kong_regions$var_name)
rm(full_product_kong); gc()

# Create mean time series per region for select variables
# NB: The var_cat variables are created here with units that match choices made later on in the script
choice_vars_kong <- full_product_kong_regions %>% 
  filter(depth >= 0, !is.na(date)) %>% 
  mutate(var_cat = case_when(grepl("°C|temp", var_name, ignore.case = T) ~ "temp",
                             grepl("O2|oxy", var_name, ignore.case = T) ~ "O2",
                             grepl("pCO2", var_name, ignore.case = T) ~ "pCO2",
                             grepl("ChlA|fluo|chloro", var_name, ignore.case = T) ~ "ChlA",
                             grepl("PAR", var_name, ignore.case = T) ~ "PAR",
                             grepl("ph", var_name, ignore.case = T) ~ "pH")) %>% 
  filter(!grepl("Chlamydomonas|Tpot|phosphate|NO2|Amph|Apher|Cten|Dimo|Euph|Megan|Poly|
                |Scaph|Scyph|Siph|Typh|Crypt|Onca|Para|Algir|Phae|Chlorophyc|Gymno|Dino|
                |Penta|Licmo|Cilio|Pachy|Dicty|Mering|Oxyr", var_name, ignore.case = T),
         !is.na(var_cat)) %>% 
  mutate(yearmon = lubridate::round_date(date, "month"),
         # month = lubridate::month(date),
         depth = round(depth, -1),
         depth_cat = case_when(depth > 0 & depth <= 10 ~ "0 - 10 m",
                               depth > 10 & depth <= 50 ~ "10 - 50 m",
                               depth > 50 & depth <= 200 ~ "50 - 200 m",
                               depth > 200 & depth <= 1000 ~ "200 - 1000 m",
                               depth > 1000 & depth <= 2000 ~ "1000 - 2000 m",
                               depth > 2000 ~ "2000+ m",
                               TRUE ~ as.character(NA)),
         depth_cat = factor(depth_cat, levels = c("0 - 10 m", "10 - 50 m", "50 - 200 m", 
                                                  "200 - 1000 m", "1000 - 2000 m", "2000+ m"))) %>% 
  filter(!is.na(depth_cat))

# Check variable names
unique(choice_vars_kong$var_name)
table(dplyr::select(choice_vars_kong, var_cat, var_name))

# Create monthly averages
choice_vars_kong_monthly <- choice_vars_kong %>% 
  # mutate(var_name = case_when(var_cat == "temp" ~ "temp [°C]", TRUE ~ var_name)) %>% 
  filter(!var_name %in% c("O2 [µmol/l]", "pH", "fluo_V [volt]", "par_V [volt]", "fluo [microgram/l]")) %>% 
  # filter(depth_cat == "0 - 10 m") %>% 
  filter(date >= "2009-01-01") %>% 
  group_by(region, depth_cat, var_cat, yearmon) %>% #, var_name) %>% 
  summarise(value = mean(value, na.rn = T),
            sd = sd(value, na.rm = T), .groups = "drop") %>% 
  complete(nesting(region, depth_cat, var_cat), yearmon = seq(min(yearmon), max(yearmon), by = "month"))

# Plot data
ggplot(choice_vars_kong_monthly) +
  geom_point(aes(x = yearmon, y = value, colour = depth_cat), size = 0.5) +
  geom_line(aes(x = yearmon, y = value, colour = depth_cat)) +
  facet_grid(var_cat ~ region, scales = "free")

# Relationship between daily data
choice_vars_kong_wide <- choice_vars_kong %>% 
  # mutate(var_name = case_when(var_cat == "temp" ~ "temp [°C]", TRUE ~ var_name)) %>% 
  filter(!var_name %in% c("O2 [µmol/l]", "pH", "fluo_V [volt]", "par_V [volt]", "fluo [microgram/l]")) %>% 
  # filter(depth_cat == "0 - 10 m") %>% 
  filter(date >= "2009-01-01") %>% 
  group_by(region, depth_cat, var_cat, date) %>% #, var_name) %>% 
  summarise(value = mean(value, na.rn = T), .groups = "drop") %>% 
  pivot_wider(id_cols = c("region", "depth_cat", "date"), values_from = value, names_from = var_cat)

# Grouped correlations
choice_vars_kong_cor <- choice_vars_kong_wide %>% 
  group_by(region, depth_cat) %>% 
  nest() %>% 
  mutate(temp_ChlA = purrr::map(data, cor_test, col1 = "temp", col2 = "ChlA"),
         temp_O2 = purrr::map(data, cor_test, col1 = "temp", col2 = "O2"),
         temp_PAR = purrr::map(data, cor_test, col1 = "temp", col2 = "PAR"),
         ChlA_O2 = purrr::map(data, cor_test, col1 = "ChlA", col2 = "O2"),
         ChlA_PAR = purrr::map(data, cor_test, col1 = "ChlA", col2 = "PAR"),
         O2_PAR = purrr::map(data, cor_test, col1 = "O2", col2 = "PAR"))# %>% 
choice_vars_kong_cor_unnest <- choice_vars_kong_cor %>% 
  unnest(cols = c(temp_ChlA, temp_O2, temp_PAR, ChlA_O2, ChlA_PAR, O2_PAR), names_sep = "-") %>% 
  dplyr::select(-data) %>% 
  pivot_longer(cols = `temp_ChlA-r`:`O2_PAR-n`) %>% 
  separate(name, into = c("test", "stat"), sep = "-") %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  drop_na()

# MHW analysis
# Make calculations
MHW_res <- choice_vars_kong %>%
  filter(var_cat == "temp") %>% 
  filter(depth_cat != "1000 - 2000 m") %>% 
  dplyr::rename(t = date, temp = value) %>% 
  group_by(region, depth_cat, var_cat, t) %>%
  summarise(temp = mean(temp, na.rn = T),
            count = n(), .groups = "drop") %>% 
  group_by(region, depth_cat) %>%
  nest() %>% 
  mutate(clim = purrr::map(data, ts2clm_adapt),
         event = purrr::map(clim, detect_event), 
         cat = purrr::map(event, category, climatology = T, season = "peak")) %>%
  select(-data, -clim)

# Climatology results
MHW_clim <- MHW_res %>% 
  dplyr::select(region, depth_cat, event) %>% 
  unnest(cols = event) %>% 
  filter(row_number() %% 2 == 1) %>% 
  unnest(event)

# Event results
MHW_event <- MHW_res %>% 
  dplyr::select(region, depth_cat, event) %>% 
  unnest(cols = event) %>% 
  filter(row_number() %% 2 == 0) %>% 
  unnest(event)


# Figures -----------------------------------------------------------------

## Figure 1: Map and time series of Kongsfjorden
# a) map
fig1a <- ggplot(coastline_kong_expand, aes(x = lon, y = lat)) + 
  geom_polygon(aes(x = x, y = y, group = polygon_id), fill = "grey30", colour = "black") +
  geom_polygon(data = kong_regions, aes(group = region, fill = region), alpha = 0.2) +
  geom_point(data = full_product_kong_unique) +
  geom_point(data = full_region_kong, aes(colour = region)) +
  scale_fill_brewer(palette = "Set1", aesthetics = c("colour", "fill")) +
  coord_quickmap(expand = F,
                 xlim = c(bbox_kong[1]-0.3, bbox_kong[2]+0.3), 
                 ylim = c(bbox_kong[3]-0.05, bbox_kong[4]+0.05)) +
  labs(x = NULL, y = NULL, fill = "Region", colour = "Region") +
  theme(panel.border = element_rect(colour = "black", fill = NA))
  # theme(legend.position = "bottom")
fig1a

# b) time series
fig1b <- ggplot(choice_vars_kong_monthly) +
  geom_point(aes(x = yearmon, y = value, colour = depth_cat), size = 0.5) +
  geom_line(aes(x = yearmon, y = value, colour = depth_cat)) +
  facet_grid(var_cat ~ region, scales = "free", switch = "y") +
  scale_colour_brewer(palette = "Accent") +
  labs(x = "Date", y = NULL, colour = "Depth") +
  theme(legend.position = "bottom",
        strip.placement = "outside",
        strip.background.y = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))
fig1b

# Combine and save
fig1 <- ggpubr::ggarrange(fig1a, fig1b, ncol = 1, heights = c(1, 0.8), labels = c("A)", "B)"))
fig1
ggsave("poster/fig1.png", fig1, width = 10, height = 12)

## Figure 2: Facetted scatterplot of relation of some variables with temperature
# Use different oolours and or shapes for depths
# Perhaps a facet grids to also show fjord position
choice_vars_kong_long_temp <- choice_vars_kong_wide %>% 
  pivot_longer(cols = c("ChlA", "O2", "PAR")) %>% 
  drop_na() %>% 
  mutate(name = case_when(name == "PAR" ~ "PAR (μmol/m^2/s)",
                          name == "O2" ~ "O2 (% sat)",
                          name == "ChlA" ~ "ChlA (μg/L)"))
fig2 <- ggplot(choice_vars_kong_long_temp, aes(x = temp, y = value)) +
  geom_smooth(method = "lm", se = F, aes(colour = depth_cat)) +
  geom_point(aes(colour = depth_cat)) +
  facet_grid(name ~ region, scales = "free_y", switch = "y") +
  scale_colour_brewer(palette = "Accent") +
  labs(y = NULL, x = "Temperature (°C)", colour = "Depth") +
  theme(legend.position = "bottom",
        strip.placement = "outside",
        strip.background.y = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))
fig2
ggsave("poster/fig2.png", fig2, width = 10, height = 5)

# code/SSC_2021.R
# This script contains the code used for the analyses and figures in the SSC 2021 poster

# Setup -------------------------------------------------------------------

# Libraries
source("code/functions.R")
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
kong_regions <- rbind(kong_inner, kong_trans, kong_middle, kong_outer, kong_shelf)

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
  left_join(full_region_kong, by = c("lon", "lat"))
unique(full_product_kong_regions$var_name)

# Create mean time series per region for select variables
choice_vars_kong <- full_product_kong_regions %>% 
  filter(depth >= 0) %>% 
  mutate(yearmon = lubridate::round_date(date, "month"),
         month = lubridate::month(date),
         depth = round(depth, -1),
         depth_cat = case_when(depth > 0 & depth <= 10 ~ "0 - 10 m",
                               depth > 10 & depth <= 50 ~ "10 - 50 m",
                               depth > 50 & depth <= 200 ~ "50 - 200 m",
                               depth > 200 & depth <= 1000 ~ "200 - 1000 m",
                               depth > 1000 & depth <= 2000 ~ "1000 - 2000 m",
                               depth > 2000 ~ "2000+ m",
                               TRUE ~ as.character(NA)),
         depth_cat = factor(depth_cat, levels = c("0 - 10 m", "10 - 50 m", "50 - 200 m", 
                                                  "200 - 1000 m", "1000 - 2000 m", "2000+ m")),
         var_cat = case_when(grepl("°C|temp", var_name, ignore.case = T) ~ "temp",
                             grepl("O2|oxy", var_name, ignore.case = T) ~ "02",
                             grepl("pCO2", var_name, ignore.case = T) ~ "pCO2",
                             grepl("ph", var_name, ignore.case = T) ~ "pH",
                             grepl("ChlA|fluo", var_name, ignore.case = T) ~ "ChlA")) %>% 
  filter(!is.na(depth_cat), !is.na(var_cat), !is.na(month),
         !grepl("Chlamydomonas|Tpot", var_name, ignore.case = T))

# Check variable names
unique(choice_vars_kong$var_name)

# Create monthly averages
choice_vars_kong_monthly <- choice_vars_kong %>% 
  group_by()

# Figures -----------------------------------------------------------------

## Figure 1: Map and time series of Kongsfjorden

## Figure 2: Bubble plot showing relationships of variables


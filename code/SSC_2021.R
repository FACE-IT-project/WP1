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

# Create mean time series per region for select variables
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

df <- choice_vars_kong_wide %>% 
  filter(depth_cat == "50 - 200 m.", region == "transition")
col1 <- "temp"; col2 <- "ChlA"

cor_test <- function(df, col1, col2){
  df_sub <- drop_na(df[c(col1, col2)])
  if(nrow(df_sub) > 0) {
    cor_res <- cor.test(df_sub[[1]], df_sub[[2]])
    res <- data.frame(r = cor_res$estimate, p = cor_res$p.value, df = cor_res$parameter, n = nrow(df_sub))
  } else {
    res <- data.frame(r = NA, p = NA, df = NA, n = 0)
  }
  return(res)
}

# Grouped correlations
choice_vars_kong_cor <- choice_vars_kong_wide %>% 
  group_by(region, depth_cat) %>% 
  nest() %>% 
  mutate(temp_ChlA = purrr::map(data, cor_test, col1 = "temp", col2 = "ChlA")) #%>% 
  # unnest(temp_ChlA)
  # summarise(temp_ChlA = cor_test(df = ., col1 = "temp", col2 = "ChlA"), .groups = "drop")
choice_vars_kong_n <- choice_vars_kong_wide %>% 
  group_by(region, depth_cat) %>% 
  filter(complete.cases(.)) %>% 
  summarise(n())

# Correlelograms
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- abs(cor(x, y)) # Remove abs function if desired
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
}
pairs(choice_vars_kong_wide[c("ChlA", "O2", "temp", "PAR")],
      upper.panel = panel.cor,
      lower.panel = panel.smooth)

library(PerformanceAnalytics)

chart.Correlation(choice_vars_kong_wide[c("ChlA", "O2", "temp", "PAR")], histogram = TRUE, method = "pearson")

library(psych)

corPlot(choice_vars_kong_wide[c("ChlA", "O2", "temp", "PAR")], cex = 1.2)

library(corrgram)

corrgram(choice_vars_kong_wide[c("ChlA", "O2", "temp", "PAR")],
         order = TRUE,              # If TRUE, PCA-based re-ordering
         upper.panel = panel.pie,   # Panel function above diagonal
         lower.panel = panel.shade, # Panel function below diagonal
         text.panel = panel.txt,    # Panel function of the diagonal
         main = "Correlogram")      # Main title

library(corrplot)

corrplot(cor(choice_vars_kong_wide[c("ChlA", "O2", "temp", "PAR")]),        # Correlation matrix
         method = "shade", # Correlation plot method
         type = "full",    # Correlation plot style (also "upper" and "lower")
         diag = TRUE,      # If TRUE (default), adds the diagonal
         tl.col = "black", # Labels color
         bg = "white",     # Background color
         title = "",       # Main title
         col = NULL)       # Color palette

# Pie charts
corrplot(cor(choice_vars_kong_wide[c("ChlA", "O2", "temp", "PAR")]), 
         method = "pie",
         title = "method = 'pie'",
         tl.pos = "n", mar = c(2, 1, 3, 1)) 

corrplot.mixed(cor(choice_vars_kong_wide[c("ChlA", "O2", "temp", "PAR")], use = "everything"),
               lower = "number", 
               upper = "circle",
               tl.col = "black")

# MHW analysis
# Make calculations
MHW_res <- choice_vars_kong %>%
  filter(var_cat == "temp") %>% 
  filter(depth_cat != "1000 - 2000 m") %>% 
  dplyr::rename(t = date, temp = value) %>% 
  group_by(region, depth_cat, var_cat, t) %>%
  summarise(temp = mean(temp, na.rn = T), .groups = "drop") %>% 
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
fig1a <- ggplot(coastline_kong, aes(x = lon, y = lat)) + geom_point() +
  geom_polygon(data = kong_regions, aes(group = region, fill = region))
fig1a

# b) time series
fig1b <- ggplot(choice_vars_kong_monthly) +
  geom_line(aes(x = yearmon, y = value, colour = depth_cat)) +
  facet_grid(var_cat ~ region, scales = "free")
fig1b

# Combine and save
fig1 <- ggpubr::ggarrange(fig1a, fig1b, ncol = 1)
fig1

## Figure 2: Bubble plot showing relationships of variables

